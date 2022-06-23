update_titles <- updated_datafest %>% select(host, year, BestInsight, BestVisualization, BestUseOfExternalData)
update_titles <- update_titles %>% mutate(Awards = "", .before = host)
write.csv(update_titles, "/Users/yangzhenyu/asa-datafest-dashboard/data/update_titles.csv")

filenames <- list.files(path="/Users/yangzhenyu/asa-datafest-dashboard/past_winners")
df <- data.frame(
  file = c(filenames),
  stringsAsFactors = FALSE
)


a <- df %>% mutate(file = sub( ".*_", "", df$file))
a <- unique(a$file)

