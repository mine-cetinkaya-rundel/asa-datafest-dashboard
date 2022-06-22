past_comp <- data.frame (
  year = c(2011:2022),
  goal = c("Goal: Make a data-based policy proposal to reduce crime
            Data consisted of arrest records for every arrest in Los Angeles from 2005-2010, including time, location, and weapons involved.", 
           "Goal: Help understand what motivates people to lend money to developing-nation entrepreneurs and what factors are associated with paying these loans.
           Several data sets were provided, including characteristics of lenders and borrowers and loan pay-back data.
           ",
           "Goal: Help understand what qualities people look for in prospective dates.
           The DataFest students worked with a large sample of prospective matches. For each customer, data were provided on his or her preferences, as well as four matches, their preferences, and information about whether parties contacted one another.", 
           "Goal: Help understand how customers can best save money and energy.
Data consisted of a random sample of customers, with five-minute aggregates over a year of energy consumption that was then aggregated across important features of the commercial properties, as well as supporting climate and location data.",
           "Goal: Detect insights into the process of car shopping that can help make the process easier for customers.
Data consist of visitor 'pathways' through a website that helps customers configure car features and shop for cars. Five data files were linked by a customer key, and including data about the customer, about his or her visits to the webpage, and, when applicable, about the car purchased and the dealership where the car was purchased.",
           'Goal: How can site visits be converted to ticket sales, and how can TicketMaster identify "true fans" of an artist or band?
Data consisted of three sets. One included events from the last 12 months that tracked customer travel through the website. Another provided information about advertising campaigns on Google, and the third included data on the events themselves.',
           "Goal: How do visitors' searches relate to the choices of hotels booked or not booked? What role do external factors play in hotel choice?
Expedia provided DataFesters with data from search results from millions of visitors around the world who were interested in traveling to destinations all over the world. The data were in two files, one of which included data collected on search results from visitors' sessions, and another which contained detailed information about the destinations that visitors searched for.",
           "Goal: What advice would you give a new high school about what major to choose in college? How does Indeed's data compare to official government data on the labor market? Can it be used to provide good economic indicators?",
           "Goal: How do we quantify the role of fatigue and workload in a team’s performance in Rugby 7s? How reliable are the subjective wellness Fata? Should the quality of the opponent or the outcome of the game be considered when examining fatigue during a game? Can widely used measurements of training load and fatigue be improved? How reliable are GPS data in quantifying fatigue?",
           "Goal: Explore data to understand a society impact of the COVID-19 pandemic other than its direct health outcomes. What have been the effects on pollution levels, transportation levels, or working from home? Has there been a change in the number of posts on TikTok? What is the impact on online education? The focus is up to you!",
           "Goal: Use data from surveys conducted in the United States, Canada, Germany, and the United Kingdom to discover and identify patterns of drug use, with particular attention paid to identifying misuse. The analysis results could potentially be used to predict future drug misuse and to inform the development of a questionnaire physicians can use to predict drug misuse.",
           "Goal: Use player logs and survey data to analyze game-play patterns of Elm City Stories, an educational game designed to promote healthy behaviors among middle and high school students. The analysis results could help researchers understand how users navigate the game, allowing them to evaluate how much users' in-game decisions can be used to understand real-life behavior."
           ),
  stringsAsFactors = FALSE
)

write.csv(past_comp, "past_winners/past_prompts.csv")
