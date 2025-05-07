setwd("~/GitHub/2023_OMR_PA_Salmon_Genetics")

library(tidyverse)
library(lubridate)

data_WR<-read_csv("output/Paired_Genetic_Data_Loss_Comparison_2023-01-17.csv")

str(data_WR)

data_WR <- data_WR %>% mutate(september_year = ifelse(month(SampleDate)>8,year(SampleDate),year(SampleDate)-1)) %>%
  mutate(september_first_last_year = as.Date(paste(september_year, "09", "01", sep = "-"))) %>%
  mutate(days_since_september_first=as.numeric(SampleDate - september_first_last_year))

data_WR_sum<- data.frame(quantile = scales::percent(c(0, 0.05, 0.1, 0.9, 0.95, 1.0)),
            date = quantile(data_WR$days_since_september_first, c(0, 0.05, 0.1, 0.9, 0.95, 1.0), type=1)) 

hist(data_WR$days_since_september_first)

new_data <- data.frame(
  quantile = c("mean", "median"),
  date = c(round(mean(data_WR$days_since_september_first)), median(data_WR$days_since_september_first)),
  stringsAsFactors = FALSE
)


# Add new rows using rbind
data_WR_sum_new <- rbind(data_WR_sum, new_data)

# Change to date

# Define September 1st of the previous year
september_first_2022 <- as.Date(paste("2022", "09", "01", sep = "-"))


# Convert the days back to month and day
data_WR_sum_new$new_date <- september_first_2022 + data_WR_sum_new$date

data_WR_sum_new<- data_WR_sum_new %>%
  arrange(date)

# Write csv
write.csv(data_WR_sum_new,file.path("output","date_distribution_geneticWR_salvage.csv"),row.names=F)

# Do year by year according to Josh
data_WR_year <- data_WR %>% group_by(september_year) %>% summarise(min_date=min(SampleDate),median_date=median(SampleDate),max_date=max(SampleDate)) %>%
  mutate(water_year=september_year+1) %>% select(-september_year)

# Write csv
write.csv(data_WR_year,file.path("output","date_distribution_geneticWR_salvage_by_year.csv"),row.names=F)
