#install.packages("janitor")
#install.packages("dplyer")
#install.packages("MASS")
#install.packages("reshape2")
#install.packages("reshape")
#install.packages("naniar")
#install.packages("tibble)

library(MASS)
library(reshape2)
library(reshape)
library(janitor)
library(dplyr)
library(naniar)
library(tibble)

#create new df for cleaned data
#df_cleaned <- datafest_information_collection_sorted
df_cleaned <- read.csv("data/datafest_information_collection_sorted.csv", na.strings=c("", "NA", "N/A", "None", "(will need to dig up the ranking/% info)" ,"I don't know", "(we haven't requested titles for presentations)", "Not sure - SMU hosted but I was not on faculty then", "Did not gather this information"))
#format column names
df_cleaned <- clean_names(df_cleaned)
#colnames(df_cleaned) <- colnames(datafest_information_cleaned)
#delete first two rows w unnecessary info
df_cleaned <- df_cleaned[-c(1,2), ]

#delete unnecessary columns
df_cleaned <- subset(df_cleaned, select = -c(start_date,end_date,status,ip_address,progress,duration_in_seconds,finished,recipient_last_name,recipient_first_name,recipient_email,external_reference,user_language,distribution_channel,x1_4_22,x1_1_topics,x1_1_parent_topics,response_id) )

#delete pre 2017 info
df_cleaned <- subset(df_cleaned, select = -c(q51_id,q51_name,q51_size,q51_type,q51_url,q52,x1_4_29))

#delete files
df_cleaned <- subset(df_cleaned, select = -c(x2_7_id,x2_7_name,x2_7_type,x2_7_url,x2_7_size))
df_cleaned <- subset(df_cleaned, select = -c(x3_7_id,x3_7_name,x3_7_type,x3_7_url,x3_7_size))
df_cleaned <- subset(df_cleaned, select = -c(x4_7_id,x4_7_name,x4_7_type,x4_7_url,x4_7_size))
df_cleaned <- subset(df_cleaned, select = -c(x5_7_id,x5_7_name,x5_7_type,x5_7_url,x5_7_size))
df_cleaned <- subset(df_cleaned, select = -c(x6_7_id,x6_7_name,x6_7_type,x6_7_url,x6_7_size))
df_cleaned <- subset(df_cleaned, select = -c(x7_7_id,x7_7_name,x7_7_type,x7_7_url,x7_7_size))

#rename columns to be consistent w datafest df
df_cleaned <- df_cleaned %>% dplyr::rename(
  timestamp = recorded_date,
  host = x1_1,
  address = x1_2,
  url = x1_3,
  lat = location_latitude,
  lon = location_longitude,
  other_inst_2017 = x2_1,
  num_part_2017 = x2_2,
  major_dist_2017 = x2_3,
  winners_insight_2017 = x2_4,
  winners_visualization_2017 = x2_5,
  winners_external_2017 = x2_6,
  other_inst_2018 = x3_1,
  num_part_2018 = x3_2,
  major_dist_2018 = x3_3,
  winners_insight_2018 = x3_4,
  winners_visualization_2018 = x3_5,
  winners_external_2018 = x3_6,
  other_inst_2019 = x4_1,
  num_part_2019 = x4_2,
  major_dist_2019 = x4_3,
  winners_insight_2019 = x4_4,
  winners_visualization_2019 = x4_5,
  winners_external_2019 = x4_6,
  other_inst_2020 = x5_1,
  num_part_2020 = x5_2,
  major_dist_2020 = x5_3,
  winners_insight_2020 = x5_4,
  winners_visualization_2020 = x5_5,
  winners_external_2020 = x5_6,
  other_inst_2021 = x6_1,
  num_part_2021 = x6_2,
  major_dist_2021 = x6_3,
  winners_insight_2021 = x6_4,
  winners_visualization_2021 = x6_5,
  winners_external_2021 = x6_6,
  other_inst_2022 = x7_1,
  num_part_2022 = x7_2,
  major_dist_2022 = x7_3,
  winners_insight_2022 = x7_4,
  winners_visualization_2022 = x7_5,
  winners_external_2022 = x7_6)


#format NA and N/A strings
#na_strings <- c("NA", "N/A", "None", "(will need to dig up the ranking/% info)" ,"I don't know", "(we haven't requested titles for presentations)", "Not sure - SMU hosted but I was not on faculty then", "Did not gather this information")

df_cleaned <- df_cleaned %>%  replace_with_na_all(condition = ~.x %in% na_strings)

#add missing columns
df_cleaned$city <- NA
df_cleaned$state <- NA
df_cleaned$country <- NA
df_cleaned$df <- NA
df_cleaned$year <- NA

#change order of columns
df_cleaned <- df_cleaned[, c(
  "timestamp",
  "host",
  "city",
  "state",
  "country",
  "url",
  "address",
  "lon",
  "lat",
  "year",
  "df",
  "num_part_2017",
  "other_inst_2017",
  "major_dist_2017",
  "winners_insight_2017",
  "winners_visualization_2017",
  "winners_external_2017",
  "num_part_2018",
  "other_inst_2018",
  "major_dist_2018",
  "winners_insight_2018",
  "winners_visualization_2018",
  "winners_external_2018",
  "num_part_2019",
  "other_inst_2019",
  "major_dist_2019",
  "winners_insight_2019",
  "winners_visualization_2019",
  "winners_external_2019",
  "num_part_2020",
  "other_inst_2020",
  "major_dist_2020",
  "winners_insight_2020",
  "winners_visualization_2020",
  "winners_external_2020",
  "num_part_2021",
  "other_inst_2021",
  "major_dist_2021",
  "winners_insight_2021",
  "winners_visualization_2021",
  "winners_external_2021",
  "num_part_2022",
  "other_inst_2022",
  "major_dist_2022",
  "winners_insight_2022",
  "winners_visualization_2022",
  "winners_external_2022")]

#loop through the existing years and create new dataframes for each year
#append these dataframes to new dataframe
year_max <- 2022
year_min <- 2017

#create data frame
df_new <- data.frame(matrix(ncol = 17, nrow = 0))

#provide column names
colnames(df_new) <- c(
  "timestamp",
  "host",
  "city",
  "state",
  "country",
  "url",
  "address",
  "lon",
  "lat",
  "year",
  "df",
  "num_part",
  "other_inst",
  "major_dist",
  "winners_insight",
  "winners_visualization",
  "winners_external")

for (i in year_min:year_max) {
  df_cur <- df_cleaned[c(
    "timestamp",
    "host",
    "city",
    "state",
    "country",
    "url",
    "address",
    "lon",
    "lat",
    "year",
    "df",
    paste0("num_part_",i),
    paste0("other_inst_",i),
    paste0("major_dist_",i),
    paste0("winners_insight_",i),
    paste0("winners_visualization_",i),
    paste0("winners_external_",i)
  )]

  df_cur$year <- i

  names(df_cur)[names(df_cur) == paste0("num_part_",i)] <- "num_part"
  names(df_cur)[names(df_cur) == paste0("other_inst_",i)] <- "other_inst"
  names(df_cur)[names(df_cur) == paste0("major_dist_",i)] <- "major_dist"
  names(df_cur)[names(df_cur) == paste0("winners_insight_",i)] <- "winners_insight"
  names(df_cur)[names(df_cur) == paste0("winners_visualization_",i)] <- "winners_visualization"
  names(df_cur)[names(df_cur) == paste0("winners_external_",i)] <- "winners_external"

  df_new <- rbind(df_new,df_cur)
}

#fill in city, state, country fields
#remove duplicate years

#look for duplicate universities in df_cleaned

#for duplicate rows, check each field and choose one row to add the info from the other row into
#if there is conflicting info: choose one?
#delete the other row

#cleaned version of new data
df_new

#create new df to store combined data
updated_datafest <- datafest

#add missing fields to old data
updated_datafest$major_dist <- NA
updated_datafest$winners_insight <- NA
updated_datafest$winners_visualization <- NA
updated_datafest$winners_external <- NA

#combine the old and new
updated_datafest <- rbind(updated_datafest, df_new)

updated_datafest