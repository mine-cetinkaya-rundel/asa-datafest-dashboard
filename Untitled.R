updated_datafest <- read.csv("data/updated_datafest.csv")
updated_datafest <- updated_datafest %>% rename(BestVisualization = winners_visualization)
updated_datafest <- updated_datafest %>% mutate(JudgesChoice = "", .after = BestUseOfExternalData)
