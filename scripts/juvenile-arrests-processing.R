library(dplyr)
library(datapkg)

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

for (i in 1:length(raw_data)) {
  current_file <- read.csv(paste0(path_to_raw, "/", raw_data[1]), stringsAsFactors = F, header=T, check.names=F)
  #Remove rows without Ages
  current_file <- current_file[grepl("[0-9]", current_file$Description),]
  #Assign Crime column
  current_file$Crime <- gsub(" .*$", "", current_file$Description)
  #Assign Age column
  current_file$Age <- gsub("([a-zA-Z ]+)(<?[0-9+-]+$)", "\\2", current_file$Description)
  #Isolate juvenile ages
  current_file <- current_file[current_file$Age %in% c("<10","10-12","13-14","15","16","17"),]
  #Remove Ages from Description column
  current_file$Description <- gsub("[^a-zA-Z]", "", current_file$Description)
  
  #Relabel description column
  # Relabel some descriptions
  raw[
    ,
    Crime := switch(
      Description,
      `AgAsslt` = "Aggravated Assault",
      `AllOthr` = "Other",
      `Burgl` = "Burglary",
      `Disord` = "Disorderly Conduct",
      `DrugTot` = "Drugs",
      `Embezzl` = "Embezzlement",
      `Family` = "Offense Against Family",
      `Gamble` = "Gambling",
      `Liquor` = "Liquor Law Violation",
      `MVTheft` = "Motor Vehicle Theft",
      `NgMansl` = "Negligent Manslaughter",
      `Prostit` = "Prostitution",
      `SexOff` = "Sexual Offense",
      `SmAsslt` = "Other Assault (Simple)",
      `StlProp` = "Stolen Property",
      `Vagrncy` = "Vagrancy",
      `Vandal` = "Vandalism",
      Description
    ),
    by = Description
    ]
  
  
  
  
  
  
  
  
}

test <- current_file[current_file[,]]

write.table(
  current_file,
  file.path(getwd(), "raw", "current_file.csv"),
  sep = ",",
  row.names = F
)


