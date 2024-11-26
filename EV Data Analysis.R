# installing the required packages
install.packages(c("tidyverse","plotly","ggthemes", "bslib"), repos = "https:\\cran.rstudio.com")
library(tidyverse)
library(plotly)
library(ggthemes)
library(bslib)

# Loading the data
ev <- read.csv("F:\\R practice\\R Projects\\Project 5\\EV Project\\Electric_Vehicle_Population_Data.csv")

# checking head and str
str(ev)
head(ev)

# Checking for null values
any(is.na(ev))

# Count of NA's with column names
null <- colSums(is.na(ev))

null[null > 0]

# imputing NA's with 0
ev$Postal.Code[is.na(ev$Postal.Code)] <- 0
ev$Legislative.District[is.na(ev$Legislative.District)] <- 0
ev$X2020.Census.Tract[is.na(ev$X2020.Census.Tract)] <- 0

View(ev)

# Analysis

# 1. CAFV Eligibility and Trends:
# - What percentage of the registered vehicles are considered Clean Alternative Fuel Vehicle 
#  (CAFV) eligible?
# - Are there any noticeable trends or changes in CAFV eligibility among different vehicle 
# types or over various model years?

# For better readability the values of 'Electric.Vehicle.Type' column is changed to 
# 'BEV' for Battery Electric Vehicle 
# & PHEV for Plug-in Hybrid Electric Vehicles 
ev_clean <- ev %>% 
  mutate(Electric.Vehicle.Type = ifelse(Electric.Vehicle.Type == "Battery Electric Vehicle (BEV)", "BEV", "PHEV"))

# Changing column name Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility to CAFV_Eligibility
ev_clean <- ev_clean %>% 
  mutate(CAFV_Eligibility = Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)

ev_clean <- ev_clean %>% 
  select(- Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)

# Clean Alternative Fuel Vehicle percentage
CAFV_perc <- ev_clean %>% 
  group_by(CAFV_Eligibility) %>% 
  summarise(count = n()) %>% 
  mutate(Perc = (count / sum(count))* 100) # CAFV 40%

# Looking for trends in Vehicle Type and Model Year

# Columns to look for:
        # CAFV eligibility
        # Electric.Vehicle.Type
        # Model.Year

ggplot(ev_clean, aes(x = Electric.Vehicle.Type)) +
  geom_bar(aes(fill = factor(Model.Year))) +
  labs(title = "Distribution of Vehicle Type vs Model Year" ,
       x = "Model Type", 
       subtitle = "Highest number of vehicles are manufactured on 2023",
       fill = "Model Year") 
       ggplotly()


# 23329 BEV's manufactured on 2022
# 4351 PHEV's manufactured on 2022
# 38744 BEV's manufactured on 2023
# 6500 PHEV's manufactured on 2023
# 608 BEV's manufactured on 2024
# 1106 PHEV's manufactured on 2024

# from the above figure we can assess the following:
  # 1. Most number of vehicles made are BEV's
  # 2. The number of proposed PHEV vehicle numbers is higher than that of BEV
  # 3. There is a steady increase in the number of BEV's
  # 4. There isn't a steady increase in the number of PHEV's
  # 5. For BEV's & PHEV's most vehicles manufactured on 2023

# Looking for distinct values in CAFV_Eligibility column
table(ev_clean$CAFV_Eligibility)

# For better readability changing the values of CAFV_Eligibility to 
# Eligible, Not Eligible and Unknown 
ev_clean$CAFV_Eligibility[ev_clean$CAFV_Eligibility == "Clean Alternative Fuel Vehicle Eligible"] <- "Eligible"
ev_clean$CAFV_Eligibility[ev_clean$CAFV_Eligibility == "Eligibility unknown as battery range has not been researched"] <- "Unknown"
ev_clean$CAFV_Eligibility[ev_clean$CAFV_Eligibility == "Not eligible due to low battery range"] <- "Not Eligible"     

#setting a common theme for all ggplot visuals
theme_set(theme_few())

# Checking the Distribution of Vehicle Type with CAFV Eligibility
ggplot(ev_clean, aes(x = CAFV_Eligibility)) +
  geom_bar(aes(fill = Electric.Vehicle.Type)) +
  labs(title = "Distribution of Vehicle Type vs CAFV Eligibility",
       x = "CAFV Eligibility", fill = "Vehicle Type") +
  theme_few()

# Most Vehicles eligibility type is 'Unknown'

# Battery Electric Vehicles (BEV) are the most manufactured vehicle type


# Subsetting 'Model Year' is necessary due to lower data counts before 2011
year <- c(2011:2024)

ev_year_adjusted<- ev_clean %>% 
  subset(Model.Year %in% year) 

# Finding out distribution of 'Make' with 'Model Year'
ggplot(ev_year_adjusted, aes(x = factor(Model.Year))) +
  geom_bar(aes(fill = Make)) +
  theme_few() +
  coord_flip() +
  labs(title = "Model Year VS Make", 
       x = "Count",
       y = "Model Year")
  ggplotly()

# Most registered vehicles 'Make' is 'Tesla'.

# 2. Electric Range Distribution:
#  - What is the distribution of electric ranges among BEVs and PHEVs in the dataset?
#  - Are there specific clusters or ranges where most vehicles fall, and how do these 
#     ranges impact eligibility and adoption?

  
# Filtering out PHEVs with an electric range of 0, as they operate on both electric charge and gasoline.
ev_range <- ev_clean %>% 
  filter(Electric.Range > 0) %>% 
  subset(Model.Year %in% year)

# boxplot to understand the distribution
ggplot(ev_range, aes(x = Electric.Vehicle.Type, y = Electric.Range)) + 
  geom_boxplot(color = "blue", fill = "grey",alpha = 0.7) +
  labs(title = "Distribution of Vehicle Type VS Electric Range",
       x = "Vehicle Type") + theme_igray()
ggplotly()

# for BEV's 50% of electric range lies between 125 to 238 miles
# for PHEV's 50% of the range lies between 21 to 38 miles

# Electric range variation across different model years
ggplot(ev_range, aes(x = Electric.Vehicle.Type, y = Electric.Range)) +
  geom_boxplot(aes(fill = Electric.Vehicle.Type)) +
  facet_wrap(~Model.Year) +
  guides(fill = FALSE) +
  labs(title = "Electric Range over the years in BEV's and PHEV's",
       x = "Vehicle Type", 
       y = "Electric Range") 
ggplotly()


# Over the years there is steady increase in the electric range for BEV's
# however there is decline in this trend after 2020
# Electric Range in PHEV's doesn't seem to have much of an increase over the years

# 3. Base MSRP Analysis:
#   - What is the range and distribution of Base Manufacturer's Suggested Retail Price
#     (MSRP) across different vehicle types and models?
#   - Are there correlations between MSRP and CAFV eligibility or electric range?

# I opted to filter out the numerous zero values to obtain a clearer depiction.
msrp <- ev_clean %>% 
  filter(Base.MSRP > 0)

ggplot(msrp, aes(x = Electric.Vehicle.Type, y = Base.MSRP )) +
 geom_boxplot() # noticed an outlier

# finding outlier
sorted_msrp <- msrp %>% 
  arrange(-Base.MSRP)

# normally the Base.MSRP of PHEV vehicles are lower than BEV's, maximum base MSRP of 
# BEV in this dataset is 110950, so decided to filter out those values of PHEV's above 110950 
msrp_at <- msrp %>% 
  filter(!(Base.MSRP > 110950))

# Distribution of Base MSRP with Vehicle Type
ggplot(msrp_at, aes(x = Electric.Vehicle.Type, 
                    y = Base.MSRP )) +
  geom_boxplot(color = "#046576", 
               fill = "#45C8C4", 
               alpha = 0.3) +
              theme_igray() +
              labs(x = "Vehicle Type", 
              y = "Base MSRP", 
              title = "Distribution of Base MSRP with Vehicle Type")
  ggplotly()

# 50% of BEVs have a base MSRP between 33950 and 69900
# 50% of PHEVs have a base MSRP between 39995 and 54950

# Vehicle Model VS Base MSRP
ggplot(msrp_at, aes(x = Base.MSRP, y = Model)) +
  geom_col(aes(fill = Electric.Vehicle.Type)) +
  labs(title = "Vehicle Model VS Base MSRP", 
       x = "Base MSRP", 
       fill = "Vehicle Type",
       subtitle =  expression(bold("'Model S' is having the highest Base MSRP and the least being 'Wheego'")))+
  scale_fill_manual(values = c("#EA5046", "#4696EA")) +
  theme_hc() +
  theme(plot.subtitle = element_text(color = "#666260",size = 9),
        plot.title = element_text(color = "#5046EA"))
  
# Base MSRP of Model S is the highest and weego the lowest

# finding correlation between MSRP and electric range
correlation1 <- cor(msrp_at$Base.MSRP, msrp_at$Electric.Range) # 0.5982857

# Plotting Correlation between Base MSRP and Electric Range
ggplot(msrp_at, aes(x = Electric.Range, 
                    y = Base.MSRP))+
  geom_point( aes(color = Electric.Vehicle.Type, 
                  shape = Electric.Vehicle.Type), 
                  size = 3) +
  geom_smooth(method = lm, se = F, color = "#478E6B") +
  scale_color_manual(values = c("#A40F03", "#2A3277"), 
                     name = "Vehicle Type")+
  labs(title = "Correlation between Base MSRP and Electric Range", 
       x = "Electric Range", y = "Base MSRP", 
       shape = "Vehicle Type") +
  theme_stata()

# there is a moderate correlation between Base MSRP and Electric Range

# 4. Geographical Insights:
#  - How are these electric vehicles distributed geographically across counties and 
#    cities within Washington State?


# there are a lot of state names and county names, so it is difficult to plot labels in the chart
# so decided to filter them out based on finding the top 20
# Majority of the sales are in WA state, so not going to check for any state wise trends

county_filt <- ev_clean %>% 
  group_by(County) %>% 
  summarise(Vehicle_Count = n())

View(county_filt)

state_filt <- ev_clean %>% 
  group_by(State) %>% 
  summarise(Vehicle_Count = n())

View(state_filt)

# Finding the top 20 Counties 
county_filt_desc <- county_filt %>% 
  arrange(desc(Vehicle_Count))

top_20_county <- head(county_filt_desc,20)
View(top_20_county)

print(top_20_county)

# Top 20 Counties with most number of Vehicle Registrations
ggplot(top_20_county, aes(x = County, 
                          y = Vehicle_Count)) +
  geom_bar(stat = "identity", 
           fill = "#825555") +
  coord_flip()+
  labs(title = expression(bold("Top 20 Counties with most number of Vehicle Registrations")),
      subtitle = expression(italic("King County leads with 83,413 registrations"))) +
  theme(plot.subtitle = element_text(color = "#666260",size = 9),
        plot.title = element_text(color = "#632B02", size = 11))


# Most number of registrations are on King county with 83,413 registrations 
# Snohomish being second with 18,544

# 5. Utility Provider Preferences:
#  - Is there a preference for a particular electric utility among the registered 
#    electric vehicles?

eu_count <- ev_clean %>% 
  group_by(Electric.Utility) %>% 
  summarise(Count = n())

eu <- eu_count %>% 
  arrange(-Count) 

# Top 3 Electric Utilities
top_3_eu <- head(eu,3)

print(top_3_eu)

# PUGET SOUND ENERGY INC||CITY OF TACOMA - (WA) : 58884
# PUGET SOUND ENERGY INC : 31869
# CITY OF SEATTLE - (WA)|CITY OF TACOMA - (WA): 28634


  