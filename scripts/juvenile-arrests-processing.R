library(dplyr)
library(datapkg)
library(tidyr)

##################################################################
#
# Processing Script for Juvenile Arrests
# Created by Jenna Daly
# On 08/09/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_data <- dir(path_to_raw, recursive=T, pattern = "Crime") 

#Create new population data set for all years
source('./scripts/getPopulation.R')

juv_arrests <- data.frame(stringsAsFactors=F)
for (i in 1:length(raw_data)) {
  current_file <- read.csv(paste0(path_to_raw, "/", raw_data[i]), stringsAsFactors = F, header=T, check.names=F)
  names(current_file)[1] <- "Crime"
  #Remove rows without Ages
  current_file <- current_file[grepl("[0-9]", current_file$Crime),]
  first_col <- current_file[2]
  last_col <- ncol(current_file)
  current_file_long <- gather(current_file, Indicator, Value, 2:last_col, factor_key=TRUE)
  #Assign Age column
  current_file_long$Age <- gsub("([a-zA-Z ]+)(<?[0-9+-]+$)", "\\2", current_file_long$Crime)
  #Isolate juvenile ages
  current_file_long <- current_file_long[current_file_long$Age %in% c("<10","10-12","13-14","15","16","17"),]
  #Remove Ages from Crime column
  current_file_long$Crime <- gsub("[^a-zA-Z]", "", current_file_long$Crime)
  #Relabel Crime column
  current_file_long$Crime[current_file_long$Crime == "AgAsslt"] <- "Aggravated Assault"
  current_file_long$Crime[current_file_long$Crime == "AllOthr"] <- "Other"
  current_file_long$Crime[current_file_long$Crime == "Burgl"] <- "Burglary"
  current_file_long$Crime[current_file_long$Crime == "Disord"] <- "Disorderly Conduct"
  current_file_long$Crime[current_file_long$Crime == "DrugTot"] <- "Drugs"
  current_file_long$Crime[current_file_long$Crime == "Embezzl"] <- "Embezzlement"
  current_file_long$Crime[current_file_long$Crime == "Family"] <- "Offense Against Family"
  current_file_long$Crime[current_file_long$Crime == "Gamble"] <- "Gambling"
  current_file_long$Crime[current_file_long$Crime == "Liquor"] <- "Liquor Law Violation"
  current_file_long$Crime[current_file_long$Crime == "MVTheft"] <- "Motor Vehicle Theft"
  current_file_long$Crime[current_file_long$Crime == "NgMansl"] <- "Negligent Manslaughter"
  current_file_long$Crime[current_file_long$Crime == "Prostit"] <- "Prostitution"
  current_file_long$Crime[current_file_long$Crime == "SexOff"] <- "Sexual Offense"
  current_file_long$Crime[current_file_long$Crime == "SmAsslt"] <- "Other Assault (Simple)"
  current_file_long$Crime[current_file_long$Crime == "StlProp"] <- "Stolen Property"
  current_file_long$Crime[current_file_long$Crime == "Vagrncy"] <- "Vagrancy"
  current_file_long$Crime[current_file_long$Crime == "Vandal"] <- "Vandalism"
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(raw_data[i])), "")), 1, 4))
  current_file_long$Year <- get_year
  juv_arrests <- rbind(juv_arrests, current_file_long)
}

juv_arrests$Indicator <- as.character(juv_arrests$Indicator)

test <- juv_arrests

#Removing values for county (where name is both a county and a town)
years <- c("2010", "2011", "2012", "2013") #Years where indicators are counties not towns
indicators <- c("Hartford", "Windham", "Tolland", "New London", "New Haven", "Litchfield", "Fairfield")
test <- test[!(test$Year %in% years & test$Indicator %in% indicators),]

test$Indicator[test$Indicator == "CT"] <- "Connecticut"
test$Indicator <- gsub(" CSP", "", test$Indicator)
test$Indicator <- gsub(" PD", "", test$Indicator)

#Merge Groton names and Putnam names
test$Indicator[which(grepl("Groton", test$Indicator))] <- "Groton"
test$Indicator[which(grepl("Putnam", test$Indicator))] <- "Putnam"

#Merge in FIPS (to remove non-towns)
names(test)[names(test) == "Indicator"] <- "Town"

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

test_fips <- merge(test, fips, by = "Town", all.y=T)

test_fips <- test_fips %>% 
  group_by(Year, Age, Indicator, Crime) %>% 
  mutate(Value = sum(Value))

test_fips <- unique(test_fips)

# relabel age ranges, and aggregate them as needed
test_fips$`Age Range` <- NA
test_fips$`Age Range`[test_fips$`Age` == "<10"] <- "0 to 9 years"
test_fips$`Age Range`[test_fips$`Age` == "10-12"] <- "10 to 14 years"
test_fips$`Age Range`[test_fips$`Age` == "13-14"] <- "10 to 14 years"
test_fips$`Age Range`[test_fips$`Age` == "15"] <- "15 to 17 years"
test_fips$`Age Range`[test_fips$`Age` == "16"] <- "15 to 17 years"
test_fips$`Age Range`[test_fips$`Age` == "17"] <- "15 to 17 years"

test_fips$Age <- NULL

test_fips <- test_fips %>% 
  group_by(Year, `Age Range`, Town, Crime) %>% 
  mutate(Value = sum(Value))

test_fips <- unique(test_fips)

CT_2015 <- test_fips[test_fips$Year == "2015",]

CT_2015 <- CT_2015 %>% 
  group_by(Crime, `Age Range`) %>% 
  summarise(Value = sum(Value))

CT_2015$Town <- "Connecticut"
CT_2015$FIPS <- "09"
CT_2015$Year <- 2015

test_fips <- rbind(test_fips, CT_2015)

#####################################################################################
test_totals <- test_fips

#Create Total value for all crimes
total.crime <- test_totals %>% 
  select(Crime, Town, FIPS, Value, Year, `Age Range`) %>% 
  group_by(Town, `Age Range`, Year) %>% 
  mutate(Value = sum(`Value`))

total.crime$Crime <- "Total"
total.crime <- unique(total.crime)

test_totals <- rbind(test_totals, total.crime)

#Create Total value for all ages
total.age <- test_totals %>% 
  select(Crime, Town, FIPS, Value, Year, `Age Range`) %>% 
  group_by(Town, Crime, Year) %>% 
  mutate(Value = sum(`Value`)) 

total.age$`Age Range` <- "Total"
total.age <- unique(total.age)

test_totals <- rbind(test_totals, total.age)

##################################################################################
## read population data for denominators in rate calculations
pops <- read.csv(paste0(path_to_raw, "/", "populations.csv"), stringsAsFactors = F, header=T, check.names=F)

# Helper function for MOE
calcMOE <- function(x, y, moex, moey) {
  moex2 <- moex^2
  moey2 <- moey^2
  d <- x/y
  d2 <- d^2
  
  radicand <- ifelse(
    moex2 < (d2 * moey2),
    moex2 + (d2 * moey2),
    moex2 - (d2 * moey2)
  )
  
  return(sqrt(radicand)/y)
}

pops$FIPS <- gsub("^", "0", pops$FIPS)

percents <- merge(test_totals, pops, by = c("Year", "Age Range", "FIPS"))

percents <- percents %>% 
  mutate(Pop = (Pop/1e5), 
         MOE = (MOE/1e5))

# calculate rates with population denominators,
# keep MOES, calculating appropriately
percents <- percents %>% 
  mutate(`Juvenile Arrests` = round((Value / Pop), 2),
         `Margins of Error` = round((calcMOE(Value, Pop, 0, MOE)), 2),
         `Measure Type` = "Rate (per 100,000)")

nulls <- c("Value", "Pop", "MOE")
percents[nulls] <- NULL

# melt percents
percents <- melt(
  percents,
  id.vars = c("Town", "FIPS", "Year", "Crime", "Age Range", "Measure Type"),
  variable.name = "Variable",
  variable.factor = F,
  value.name = "Value",
  value.factor = F
)

percents$Variable <- as.character(percents$Variable)

## FINAL STEPS
# add extra data
test_totals$`Measure Type` <- "Number"
test_totals$Variable <- "Juvenile Arrests"

percents <- as.data.frame(percents)
test_totals <- as.data.frame(test_totals)

# combine number and rate measures into one dataset
juv_arrests_total <- rbind(test_totals, percents)

#Assign factors for sorting
juv_arrests_total$`Age Range` <- factor(juv_arrests_total$`Age Range`, levels = c("Total", "0 to 9 years", "10 to 14 years", "15 to 17 years"))

juv_arrests_total$Crime <- factor(juv_arrests_total$Crime, levels = c("Total", "Aggravated Assault", "Arson", "Burglary", "Curfew", 
                                                                      "Disorderly Conduct", "Drugs", "DUI", "Embezzlement", "Forgery", 
                                                                      "Fraud", "Gambling", "Larceny", "Liquor Law Violation", 
                                                                      "Motor Vehicle Theft", "Murder", "Negligent Manslaughter", 
                                                                      "Offense Against Family", "Other Assault (Simple)", "Other", 
                                                                      "Prostitution", "Rape", "Robbery", "Runaway", "Sexual Offense", 
                                                                      "Stolen Property", "Vagrancy", "Vandalism", "Weapons"))

# Order and sort columns
juv_arrests_total <- juv_arrests_total %>% 
  select(Town, FIPS, Year, `Age Range`, Crime, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Age Range`, Crime, `Measure Type`)

# Write to File
write.table(
  juv_arrests_total,
  file.path(getwd(), "data", "juvenile-arrests_2015.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)

#######################################################################################################

