### Master
rm(list = ls())

## Enter user_name, Period, Media group, & Channel------------------------------
user_name <- Sys.getenv("Username")

# setwd
setwd(paste0("C:/Users/", Sys.getenv("username"),"/OneWorkplace/Decisions Science - Documents/1. Business Growth/Clients/Compare the market/Geo-Test Nov 22"))

## Load Libraries
library(readxl)
library(readr)
library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(ggplot2)
library(GeoLift)
library(visdat)
library(writexl)

## Import Files

result_1 <- read_xlsx("Raw Data/Copy of DI-14468  Re-run DI-14391 but for quotes instead of sales (1).xlsx", sheet = "Result 1")


## Clean Files

# result_1 by region/district/sector [PRE-TREATMENT DATA]
result_1_region_clean <- result_1 %>% 
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d"))%>%
  select(date, region, quotes)

result_1_district_clean <- result_1 %>% 
  mutate(date = as.POSIXct (date, format = "%Y-%m-%d"))%>%
  mutate(district = substr(postcode,1,3))%>%
  select(date, district, quotes)

result_1_sector_clean <- result_1 %>% 
  mutate(date = as.POSIXct (date, format = "%Y-%m-%d"))%>%
  select(date, sector = postcode, quotes)


## TEST ON THE FIRST LETTERS OF THE POSTCODE  

postcode_letters <- result_1_district_clean %>%
  mutate(district = sub("^([[:alpha:]]*).*", "\\1", district)) %>%
  group_by(date,district) %>%
  summarise(quotes = sum(quotes))


##Reading in to GeoLift Package
geotest_r1_region <- GeoDataRead(data = postcode_letters,
                                 date_id = "date",
                                 location_id = "district",
                                 Y_id = "quotes",
                                 X = c(),
                                 format = "yyyy-mm-dd",
                                 summary = TRUE)

GeoPlot(geotest_r1_region,
        Y_id = "Y",
        time_id = "time",
        location_id = "location")


##GeoLift Power Analysis
MarketSelections <- GeoLiftMarketSelection(data = geotest_r1_region,
                                           treatment_periods = 28, #first number is number of periods to include, second number is a 28 day test 
                                           N = c(20,25,30,35,40,45,50), #list of number of test markets to calculate power for. The example explores text market sizes of 2,3, and 4 markets, with the remaining markets being in the donor pool
                                           Y_id = "Y", #the dependent variable
                                           location_id = "location", #the location variable
                                           time_id = "time", #the time variable, test it at different increments 
                                           effect_size = seq(0,0.25,0.05), #metric which describes the size of the overlap
                                           lookback_window = 1, #A number indicating how far back in time the simulations for the power analysis should go. For instance, a value equal to 5 would simulate power for the last five possible tests
                                           holdout = c(0.5, 1), #holdout is the share of conversions from markets that will not see the ad.  1st number is the smallest desirable holdout 2nd is the largest
                                           cpic = 2.04, #cost per incremental conversion. On average, how much do we typically need to spend to bring incremental conversions 
                                           budget = 20000, # maximum budget available
                                           alpha = 0.1, #standard error
                                           Correlations = TRUE, # Boolean, ???
                                           fixed_effects = TRUE, # Boolean, fixed effects represent non-random components in the data. 
                                           side_of_test = "one_sided" #one or 2sided test
                                           )


plot(MarketSelections, market_ID = 1, print_summary = FALSE)


## REVERT BACK TO POSTCODE LEVEL

best_regions <- c(trimws(toupper(str_split_fixed(MarketSelections$BestMarkets[1,2],",",20))))


FINAL_REGIONS <- read.csv("postcodes.csv") %>%
  mutate(pcl = toupper(sub("^([[:alpha:]]*).*", "\\1", postcode))) %>%
  filter( pcl %in% best_regions) %>%   
  select(postcode)

#### GETTING THE RIGHT POSTCODE FORMAT 

test <- FINAL_REGIONS %>% mutate(postcode = sub("^([[:alpha:]]*).*", "\\1", postcode)) %>%
  select(postcode) %>%
  unique()


right_format <- read.csv("Raw Data/postcodes.csv", header = F ) %>%
  separate(V1, into = c("prefix", "suffix"), sep = " ") %>%
  mutate(suffix = gsub("([0-9]+).*$", "\\1", suffix)) %>%
  unite(col = "postcode", c("prefix", "suffix"), sep = " ") %>%
  distinct(postcode) %>%
  mutate(pcl = toupper(sub("^([[:alpha:]]*).*", "\\1", postcode))) %>%
  semi_join(test, by = c("pcl"="postcode")) 


final_df <- right_format %>% select(postcode)

# Write out the csv of postcodes to run a test campaign for
#write.csv(final_df, "Out Data/PROPER POSTCODES (real).csv")


##############################################
#####        POST TREATMENT          ########
#############################################

# COMBINE ALL DATA [PRE + TREAT + POST]

treat_postcode_letters <- read.csv("Raw Data/Post Test Data/DI-15040   Rerun DI-14468 For latest information (Quotes only) 1-17May.csv") %>% 
  mutate(date = as.POSIXct (date, format = "%Y-%m-%d"), district = substr(postcode,1,3)) %>%
  select(date, district, quotes) %>% 
  bind_rows(read.csv("Raw Data/Post Test Data/DI-15040   Rerun DI-14468 For latest information (Quotes only).csv") %>%
              mutate(date = ymd(dmy(date)), district = substr(postcode,1,3)) %>%
              select(date, district, quotes),
            read_xlsx("Raw Data/Post Test Data/DI-15079 Home Quotes 18th May - 24th May.xlsx") %>%
              mutate(date = ymd(date), district = substr(postcode,1,3)) %>%
              select(date, district, quotes)) %>%
  mutate(district = sub("^([[:alpha:]]*).*", "\\1", district)) %>%
  group_by(date,district) %>%
  summarise(quotes = sum(quotes)) %>%
  bind_rows(postcode_letters) %>%
  mutate(district = tolower(district)) %>%
  group_by(date,district) %>%
  summarise(quotes = sum(quotes))

check <- treat_postcode_letters[,1] %>% distinct 

# FIND THE START AND END TIMES OF THE TREATMENT

START_TIME <- which(check$date == "2023-04-17")

END_TIME <- which(check$date == "2023-05-16")


##Reading in to GeoLift Package
geotest_treat <- GeoDataRead(data = treat_postcode_letters,
                                 date_id = "date",
                                 location_id = "district",
                                 Y_id = "quotes",
                                 X = c(),
                                 format = "yyyy-mm-dd",
                                 summary = TRUE)

GeoPlot(geotest_treat,
        Y_id = "Y",
        time_id = "time",
        location_id = "location")



GeoTest <- GeoLift(Y_id = "Y",
                   data = geotest_treat,
                   locations = c(tolower(best_regions)),
                   treatment_start_time = START_TIME,
                   treatment_end_time = END_TIME)

GeoTest


treat_summary <- as.data.frame(unlist(summary(GeoTest))) %>%
  rownames_to_column() %>%
  filter(!str_detect(rowname,paste(c("ATT.Estimate", "ATT.upper_bound", "ATT.lower_bound","ATT.Time","ATT.p_val","weights.location","weights.weight"), collapse = '|')))
names(treat_summary) <- c("Metric", "Values")

write_xlsx(treat_summary, "Out Data/Treatment Summary.xlsx")
