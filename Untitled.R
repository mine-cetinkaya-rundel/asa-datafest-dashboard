update_titles <- read.csv("/Users/yangzhenyu/asa-datafest-dashboard/data/update_titles.csv")
update_titles$Slides <- paste0(update_titles$Slides, ".pdf")
update_titles$Awards <- gsub("([a-z])([A-Z])","\\1 \\2", update_titles$Awards)
update_titles$other_awards <- gsub("([a-z])([A-Z])","\\1 \\2", update_titles$other_awards)
names(update_titles) <- tools::toTitleCase(names(update_titles))
names(update_titles) <- gsub("_", " ", names(update_titles))
write.csv(update_titles, "/Users/yangzhenyu/asa-datafest-dashboard/data/update_titles.csv")

filenames <- list.files(path="/Users/yangzhenyu/asa-datafest-dashboard/past_winners")
df <- data.frame(
  file = c(filenames),
  stringsAsFactors = FALSE
)


a <- df %>% mutate(file = sub( ".*_", "", df$file))
a <- unique(a$file)

