# Necassary Libraries
library(doBy)
library(nflreadr)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)

# Load player draft infomration from the past 25 years
draft <- load_draft_picks(
  seasons = 2000:2025,
  file_type = getOption("nflreadr.prefer", default = "rds")
)

#Clean out some of the data for, some colleges are too small or recognized or international players
#Dont get recognized the same. in 2025 some draft picsk appeared incorrectly attributed for some teams
#these are removed
draft$college[draft$college == "" | is.na(draft$college)] <- "Unknown"
draft <- draft %>% filter(pfr_player_name !="" )

#Help keep track of how many positions were drafted from each school. Helps ensure your analysis matches
# the totlas from the draft
poscollege <- draft %>% group_by(college, position) %>% 
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = position, values_from = count, values_fill = 0)

unique(draft$position)
#Want only WR right now for testing out a model

#Total WRs drafted by each 
wrdraft <- poscollege[,c(1,4)]
wrdraft <- wrdraft%>% filter(WR>=7)


#Only picks for WR, does not account for posistion changes
WRUNI <- wrdraft$college
WRpicks <- draft %>% filter(college %in% WRUNI, position == "WR")


#Couple of issues i noticed, some players change their name example, Steve Smith becomes 
#Steve Smith Sr after 2000, Chod Ochocinco was Chad Johnson prior to the 2008 season. 
#Luckily the NFL Draft database does have career yardarge for every player drafted
#Players such as Terrel Pryor, drafted as a QB but changed to WR were kept out of this. 
#Using weekly averages seems a little volatile, what i will be looking at instead is career avareges
#Also im going to filter out players based on a couple of things.
# 1. No games, if they did not play a game they are out. 2. I will set a threshold, for minimum games 16
# this is one full season, i also want seasons started minimum 1, 

minwrpicks <- subset(WRpicks, games>=16 & seasons_started > 0 & receptions > 10)

#filter for columns 1 season, 2 round, 3 pick, 4 team, 8 name, 10 pos, 13 college, 14 age, 15 to year
#16 all pro, 17 probowls, 18 season started, 31:33

minwrpicks <- minwrpicks[,c(1:4,8:10,13:18,31:33)]

# add careey ypc
minwrpicks$career_ypc <- minwrpicks$rec_yards / minwrpicks$receptions

#scoring for receivers Receptions * 3 + yards + tds * 2 = WRSCORE
minwrpicks$WRSCORE <- (minwrpicks$receptions * 2) + (minwrpicks$rec_yards * 3) + (
  minwrpicks$rec_tds) + (minwrpicks$allpro * 10)

# To create the Z score, we get the mean and standard deviation from our dataframe
# Z-score helps us find players who have truly elite careers in the 25 years of data
mean_score <- mean(minwrpicks$WRSCORE, na.rm = TRUE)
sd_score <- sd(minwrpicks$WRSCORE, na.rm = TRUE)

minwrpicks$ZScore <- (minwrpicks$WRSCORE - mean_score) / sd_score

#Find the people with careers greaten than 2
minwrpicks <- minwrpicks %>%
  mutate(outlier = ZScore > 2)


# Create a summary of all the colleges
wrsummary <- minwrpicks %>%
  group_by(college) %>%
  summarise(
    total_wrs = n(),
    avg_pick = mean(pick, na.rm = TRUE),
    first_rounds = sum(round==1, na.rm=TRUE),
    total_yards = sum(rec_yards, na.rm = TRUE),
    total_receptions = sum(receptions, na.rm = TRUE),
    total_tds = sum(rec_tds, na.rm = TRUE),
    first_team_allpro = sum(allpro, na.rm = TRUE),
    receiverscore = sum(WRSCORE, na.rm = TRUE),
    num_outliers = sum(outlier, na.rm = TRUE),
    avg_score = receiverscore / total_wrs,
    .groups = "drop"
  )
#Find the smallest quartile for receivers drafted, in this case for a college that has
#Smallest quintile is 2.25 so we only allow schools that have at least 3 receivers drafted

quantile(wrsummary$total_wrs, 0.25, na.rm = TRUE)
wrsummary <- subset(wrsummary, total_wrs > 2)

#LSU has the highest score but Cal has the highest per player score

LSWRs <- subset(minwrpicks, college=="LSU")
CALWRs <- subset(minwrpicks, college=="California")
BAMAWRs <- subset(minwrpicks, college=="Alabama")
MiamiWRs<- subset(minwrpicks, college=="Miami (FL)")


# Filter for only the 10 colleges that had the higest scores
topscorecolleges <- wrsummary %>% arrange(desc(receiverscore)) %>%
  slice_head(n=10)

# Plot for highest overall score
ggplot(topscorecolleges, aes(x = reorder(college, -receiverscore), y = receiverscore)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = receiverscore),size=4, vjust = -0.2) +
  theme_minimal() +
  labs(
    title = "Top 10 Colleges by Total WR Score",
    x = "College",
    y = "Receiver Score"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) +
  expand_limits(y = max(topscorecolleges$receiverscore) + 10) +
  theme(axis.title.y=element_blank(),legend.position="none", axis.text.y=element_blank()) +
  theme(panel.background = element_rect(fill='white', color="white"))

higehst_avg <- wrsummary %>% arrange(desc(avg_score)) %>%
  slice_head(n=10)

higehst_avg$avg_score <- round(higehst_avg$avg_score)

# Plot for highest average score for each reaciver
ggplot(higehst_avg, aes(x = reorder(college, -avg_score), y = avg_score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = avg_score),size=4, vjust = -0.2) +
  theme_minimal() +
  labs(
    title = "Top 10 Colleges by Highest Average WR Score",
    x = "College",
    y = "Receiver Score"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) +
  expand_limits(y = max(higehst_avg$avg_score) + 10) +
  theme(axis.title.y=element_blank(),legend.position="none", axis.text.y=element_blank())+
  theme(panel.background = element_rect(fill='white', color="white"))

# Average Tenure for Miami Receivers
sum(MiamiWRs$to - MiamiWRs$season) / nrow(MiamiWRs)




