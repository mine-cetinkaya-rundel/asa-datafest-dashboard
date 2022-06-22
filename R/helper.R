# load packages -----------------------------------------------------
library(tidyverse)
library(leaflet)
library(shiny)
library(shinythemes)
library(here)
library(praise)
library(usethis)
library(wordcloud)
library(shinyWidgets)
library(shinydashboard)

# load data ---------------------------------------------------------

datafest <- read.csv("data/datafest.csv")
past_prompts <- read.csv("past_winners/past_prompts.csv")

updated_datafest <- read.csv("data/updated_datafest.csv")
datafest_titles <- read.csv("data/titles.csv")
major_df <- updated_datafest %>%
  dplyr::select(host,year,major_dist)

major_df <- na.omit(major_df)

# get data for universities page
universities_df <- datafest %>%
  dplyr::select(host, year, num_part)

#Map

# set map bounds ----------------------------------------------------
left <- floor(min(datafest$lon))
right <- ceiling(max(datafest$lon))
bottom <- floor(min(datafest$lat))
top <- ceiling(max(datafest$lat))


# set colors --------------------------------------------------------
href_color <- "#9966CC"
marker_color <- "darkseagreen"
part_color <- "#CC9966"

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
countries <- geojsonio::geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")
country <- countries[countries$name %in% datafest$country,]
recent <- datafest %>% 
  filter(year == max(year))
write.csv(recent, "data/recent.csv")
num_part <- datafest %>% 
  filter(!is.na(num_part)) %>% 
  group_by(year, state) %>% 
  summarise(num = sum(num_part))
max_part <- as.numeric(max(num_part$num))

country$density = NA
states <- rbind(states,country)

participants <- recent %>%
  mutate(state = case_when(country == "Germany"~ "Germany",
                           country == "Canada"~ "Canada",
                           state == "Minnessota"~ "Minnesota",
                           TRUE ~ state)) %>%
  dplyr::select(state, num_part) %>%
  dplyr::rename(name = state) 

states$num_par=0
for (i in 1:nrow(states)) {
  for (j in 1:nrow(participants)) {
    if (states$name[i] == participants$name[j]) {
      if (!is.na(participants$num_part[j])) {
        states$num_par[i] = states$num_par[i] + participants$num_part[j]
      }
    }
  }
}

bins <- c(0, 10, 20, 40, 60, 80, 100, 200, 300, 400, max_part)
pal <- colorBin("Blues", domain = states$num_par, bins = bins)

labels <- sprintf(
  "<strong>%s</strong>",
  states$name
) %>% lapply(htmltools::HTML)

host_text <- paste0(
  "<b><a href='", datafest$url, "' style='color:", href_color, "'>", datafest$host, "</a></b>"
)

other_inst_text <- paste0(
  ifelse(is.na(datafest$other_inst),
         "",
         paste0("<br>", "with participation from ", datafest$other_inst))
)

part_text <- paste0(
  "<font color=", part_color,">", datafest$num_part, " participants</font>"
)

popups <- paste0(
  host_text, other_inst_text, "<br>" , part_text
)

# calculate total participants for each year ------------------------
part_count <- datafest %>%
  group_by(year) %>%
  summarise(tot_part = sum(num_part, na.rm = TRUE))

#make main and max according to min and max of inputted college, change year scale?
# min_tot_part <- min(part_count$tot_part)
# max_tot_part <- max(part_count$tot_part)


# calculate total countries participating for each year ------------------------
df_yes <- datafest[datafest$df == "Yes", ]
country_count <- df_yes %>%
  group_by(year) %>%
  summarise(tot_country = n_distinct(country))

# calculate total hosts participating for each year ------------------------
df_yes <- datafest[datafest$df == "Yes", ]
host_count <- df_yes %>%
  group_by(year) %>%
  summarise(tot_host = n_distinct(host))

## calculate DataSource list for each year ----------------------
year <- c("2011","2012","2013","2014","2015","2016","2017")
#"2018","2019","2020","2021","2022")
source_data <- c("LAPD","Kiva.com","eHarmony","GridPoint","Edmunds.com","Ticketmaster", "Expedia")
#"Indeed","Candadian National Women's Rugby Team","Covid-19 (Virtual Data Challenge)","Rocky Mountain Posion and Drug Safety","Play2Prevent Lab")
datasource <- data.frame(year, source_data)

# ## Subset dataframe to Year Country, State, City, Majors, Participating institutions
# country_hosts_df <- subset(datafest,
#                            df =="Yes",
#                            select= c("year","host","country","state","city","other_inst"))
