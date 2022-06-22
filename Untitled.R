past_comp <- read.csv(past_winners/past_comp)
past_comp[1, c("goal")] <- "Goal: Make a data-based policy proposal to reduce crime
Data consisted of arrest records for every arrest in Los Angeles from 2005-2010, including time, location, and weapons involved."
past_comp$goal<-gsub("Goal: ","",as.character(past_comp$goal))
write.csv(past_comp, "past_winners/past_prompts.csv")
