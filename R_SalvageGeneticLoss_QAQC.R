library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(readxl)

##################### Load Salvage Count Data from Vanessa Gusman

salvage_data <- read.csv(file = file.path("data_input/ChinookLoss_20221215.csv"))
salvage_data$SampleDateTime<-as.POSIXct(paste(salvage_data$Date, salvage_data$Time), format="%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles")

#Keep data with non-existent date and time
salvage_data_wrong_time <- salvage_data %>% filter(is.na(SampleDateTime))

#Change date to the proper format
salvage_data$SampleDate<-as.Date(salvage_data$Date, format = "%m/%d/%Y")
#Change time to the proper format
salvage_data<-salvage_data %>% mutate(SampleTime=as.POSIXct(paste("2022-10-01", Time), format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")) %>%
  mutate(SampleTime=format(SampleTime, "%H:%M:%S"))

########## Prep salvage data
str(salvage_data)
#Rename columns to make it easier to work in R and divide Loss + Expanded Salvage by nfish
salvage_data_adjusted<- salvage_data %>% 
  dplyr::mutate(Facility =case_when(Facility==1 ~ "SWP",
                                    Facility==2 ~ "CVP")) %>%
  
  #Remove erroneous sample date and time, ignore for now
  #filter(!is.na(SampleDateTime)) %>%
  #Remove single sample with NA count 
  filter(!is.na(Count)) %>%
  #Remove adclip fish
  filter(AdClip == FALSE) %>%
  #Ensure that data for salvage and loss are for a single fish
  mutate(Salvage=Salvage/Count, Encounter=Encounter/LengthFrequency, Entrain=Entrain/LengthFrequency, Release=Release/LengthFrequency, Loss=Loss/LengthFrequency)

#Multiply rows by nfish
salvage_data_adjusted<- setDT(expandRows(salvage_data_adjusted, "LengthFrequency")) 

salvage_data_adjusted<- salvage_data_adjusted%>%
  # build grouping by combination of variables
  dplyr::group_by(SampleDateTime, Race, FL, Facility) %>%
  # add row number which works per group due to prior grouping
  dplyr::mutate(duplicateID = dplyr::row_number()) %>%
  # ungroup to prevent unexpected behaviour down stream
  dplyr::ungroup() 

#### Read Kevin Reece's Jan 13, 2023 data

genetic_data <-read_excel(file.path("data_input/WY1996-2022 Genetically Confirmed WR_CVP and SWP salvage and loss for OMR analysis_20230113Final.xlsx"), sheet = "WY1996-2022 JUVWR Genetic_ID", range = cell_cols("A:K")) %>% 
  rename(SampleDate='Sample Date',SampleTime='Sample Time',FL=Forklength,Facility=Loc,Salvage_KevinReece=Salvage,Loss_KevinReece=Loss,LAD_KevinReece='Model Race',GeneticAssignment=Assignment) %>%
  mutate(SampleTime=format(SampleTime, "%H:%M:%S")) %>% mutate(SampleDate=as.Date(SampleDate)) %>%
  mutate(SampleDateTime=as.POSIXct(paste(SampleDate, SampleTime), format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")) %>% select(-Catch)


genetic_data_expanded<- genetic_data %>%
  #adjust time zone
  mutate(SampleDateTime=force_tz(SampleDateTime, tz="America/Los_Angeles")) %>%
  mutate(WaterYear=as.numeric(WaterYear)) %>%
  # build grouping by combination of variables
  dplyr::group_by(SampleDateTime, Facility, FL) %>%
  # add row number which works per group due to prior grouping
  dplyr::mutate(duplicateID = dplyr::row_number()) %>%
  # ungroup to prevent unexpected behaviour down stream
  dplyr::ungroup()

##################### Combine the salvage and genetic data sets
combined_data<-left_join(salvage_data_adjusted,genetic_data_expanded)

# All data are matched up to the salvage database
unpaired_genetic_data<-full_join(salvage_data_adjusted,genetic_data_expanded) %>% filter(is.na(Salvage))
count(unpaired_genetic_data)/count(genetic_data)
#Remove NA columns
#unpaired_genetic_data <- unpaired_genetic_data %>% select_if(~ !any(is.na(.)))

#Print out csv
#write.csv(unpaired_genetic_data,file=file.path("output/Unpaired_Genetic_data_2023-01-13_fromJanuaryKevinReeceData.csv"),row.names = F)


###Double check that numbers are correct for matched data
paired_genetic_data <- combined_data %>% filter(!is.na(ArchiveID)) %>%
  #specify 2 decimals for loss
  mutate(Loss=as.numeric(format(round(Loss, 2)))) %>%
  mutate(Loss_KevinReece=as.numeric(format(round(Loss_KevinReece, 2)))) %>%
  #Check that loss calculations match up for both genetic and salvage data sets
  mutate(QAQC_Loss_Difference=abs(Loss-Loss_KevinReece))
  

#Percent correct
count(paired_genetic_data %>% filter(abs(QAQC_Loss_Difference)<0.05))/count(paired_genetic_data)*100

#Total difference in loss calculation
sum(paired_genetic_data$QAQC_Loss_Difference)
#Print out csv
#write.csv(paired_genetic_data,file=file.path("output/Paired_Genetic_Data_Loss_Comparison_2023-01-13.csv"),row.names = F)

#See RMD file for final analysis