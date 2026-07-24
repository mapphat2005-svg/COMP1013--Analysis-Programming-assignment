library(dplyr)
library(ggplot2)
#Exercise 1
#Load data
Tournaments <- read.csv("E:/A2 data/Tournaments.csv")
Teams <- read.csv("E:/A2 data/Teams.csv")
Stadiums <- read.csv("E:/A2 data/Stadiums.csv")
Matches <- read.csv("E:/A2 data/Matches.csv")
# Inspect the dimensions, variable names, and original data types.
str(Tournaments)
str(Teams)
str(Stadiums)
str(Matches)
# Display the first six observations from each dataset.
head(Tournaments)
head(Teams)
head(Stadiums)
head(Matches)
#Change '?' into 'NA'
Tournaments[Tournaments == "?"] <- NA
Teams[Teams == "?"] <- NA
Stadiums[Stadiums == "?"] <- NA
Matches[Matches == "?"] <- NA
#Convert variables into factors 
Matches$Result <-as.factor(Matches$Result)
Matches$Stage <- as.factor(Matches$Stage)
Stadiums$Country <- as.factor(Stadiums$Country)
Matches$ExtraTime <- as.factor(Matches$ExtraTime)
#Convert two penalty variables into numeric format
Matches$HomePenalty <- as.numeric(Matches$HomePenalty)
Matches$AwayPenalty <- as.numeric(Matches$AwayPenalty)
# Replace missing penalty values with zero.
Matches$HomePenalty[is.na(Matches$HomePenalty)] <- 0
Matches$AwayPenalty[is.na(Matches$AwayPenalty)] <- 0
## Inspect the cleaned data
str(Matches)
str(Stadiums)
# Present a summary of the cleaned Matches dataset.
summary(Matches)
# Identify matches that involved a penalty shootout
PenaltyMatches <- subset(
  Matches,
  PenaltyShootout == 1
)
# Inspect the filtered data.
head(PenaltyMatches)
nrow(PenaltyMatches)
# Visualise the distribution of HomePenalty
ggplot(
  data = PenaltyMatches,
  aes(x = factor(HomePenalty))
) +
  geom_bar(
    fill = "steelblue",
    colour = "black"
  ) +
  labs(
    title = "Distribution of Home Penalty Goals in Penalty Shootouts",
    x = "Number of home-team penalty goals",
    y = "Number of matches"
  ) +
  theme_minimal()
#Exercise 2: 
# Add TournamentName to the Matches dataset using TournamentID.
Matches2 <- merge(
  Matches,
  Tournaments[, c("TournamentID", "TournamentName")],
  by = "TournamentID",
  all.x = TRUE,
  sort = FALSE
)
# Present the first six rows of the combined dataset.
head(Matches2)
#Distribution of HomeTeamScore by Stage
ggplot(
  Matches2,
  aes(x = HomeTeamScore)
) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    fill = "steelblue",
    colour = "black"
  ) +
  scale_x_continuous(
    breaks = 0:max(Matches2$HomeTeamScore, na.rm = TRUE)
  ) +
  facet_wrap(
    ~ Stage,
    ncol = 2,
    scales = "free_y"
  ) +
  labs(
    title = "Distribution of Home-Team Scores by Competition Stage",
    x = "Home-team goals",
    y = "Number of matches"
  ) +
  theme_minimal()
## Distribution of HomeTeamScore by TournamentName
ggplot(
  Matches2,
  aes(x = HomeTeamScore)
) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    fill = "steelblue",
    colour = "black"
  ) +
  scale_x_continuous(
    breaks = 0:max(Matches2$HomeTeamScore, na.rm = TRUE)
  ) +
  facet_wrap(
    ~ TournamentName,
    ncol = 4,
    scales = "free_y"
  ) +
  labs(
    title = "Distribution of Home-Team Scores by FIFA World Cup",
    x = "Home-team goals",
    y = "Number of matches"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 7),
    axis.text.x = element_text(size = 7)
  )
## Create AwayTeamScore groups
# Divide AwayTeamScore into the three groups required by the task.
Matches2$AwayScoreGroup <- cut(
  Matches2$AwayTeamScore,
  breaks = c(-1, 1, 3, Inf),
  labels = c(
    "0-1 goals",
    "2-3 goals",
    "4 or more goals"
  ),
  include.lowest = TRUE
)
# Check the new variable.
str(Matches2$AwayScoreGroup)
## Distribution of HomeTeamScore by AwayTeamScore group
ggplot(
  Matches2,
  aes(x = HomeTeamScore)
) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    fill = "steelblue",
    colour = "black"
  ) +
  scale_x_continuous(
    breaks = 0:max(Matches2$HomeTeamScore, na.rm = TRUE)
  ) +
  facet_wrap(
    ~ AwayScoreGroup,
    nrow = 1,
    scales = "free_y"
  ) +
  labs(
    title = "Distribution of Home-Team Scores by Away-Team Score Group",
    x = "Home-team goals",
    y = "Number of matches"
  ) +
  theme_minimal()
#Question 3
##Filter out the matches decided by a penalty shootout
Q3penalty_matches <- subset(
  Matches, 
  PenaltyShootout == 1 
)
#Check the number of penalty-shootour matches
nrow(Q3penalty_matches)
#Check the accuracy of filtered data 
table(Q3penalty_matches$PenaltyShootout)
##Top 5 home-teams that have most frequently participated in these matches
# Count how many penalty shootouts each home team participated in
HomeTeamCounts <- as.data.frame(
  table(Q3penalty_matches$HomeTeamID)
)

# Rename the columns
names(HomeTeamCounts) <- c(
  "TeamID",
  "NumberOfMatches"
)

# Make sure TeamID has the same data type in both datasets
HomeTeamCounts$TeamID <- as.character(HomeTeamCounts$TeamID)
Teams$TeamID <- as.character(Teams$TeamID)

# Add TeamName and TeamCode using TeamID
HomeTeamResults <- merge(
  HomeTeamCounts,
  Teams,
  by = "TeamID"
)
# Sort from highest to lowest
HomeTeamResults <- HomeTeamResults[
  order(-HomeTeamResults$NumberOfMatches),
]
# Keep the required columns
HomeTeamResults <- HomeTeamResults[
  ,
  c("TeamName", "TeamCode", "NumberOfMatches")
]
# Select the top five teams
Top5HomeTeams <- head(HomeTeamResults, 5)

Top5HomeTeams

##Top 5 away-teams that have most frequently participated in these matches
AwayTeamCounts <- as.data.frame(
  table(Q3penalty_matches$AwayTeamID)
)

names(AwayTeamCounts) <- c(
  "TeamID",
  "NumberOfMatches"
)

AwayTeamCounts$TeamID <- as.character(AwayTeamCounts$TeamID)

AwayTeamResults <- merge(
  AwayTeamCounts,
  Teams,
  by = "TeamID"
)

AwayTeamResults <- AwayTeamResults[
  order(-AwayTeamResults$NumberOfMatches),
]

AwayTeamResults <- AwayTeamResults[
  ,
  c("TeamName", "TeamCode", "NumberOfMatches")
]
#Select the top five away-teams
Top5AwayTeams <- head(AwayTeamResults, 5)

Top5AwayTeams
#Question 4: 
## Factor 1: Competition Stage

### Create a table of outcomes by Stage
# Count each match outcome within every competition stage.

StageResultCount <- table(
  Matches$Stage,
  Matches$Result
)

# Convert the counts into row percentages.
# margin = 1 calculates percentages within each stage.

StageResultPercent <- round(
  prop.table(
    StageResultCount,
    margin = 1
  ) * 100,
  1
)

# Present the percentage table.

knitr::kable(
  StageResultPercent,
  caption = "Percentage of Match Outcomes by Competition Stage"
)

### Visualise Result by Stage
ggplot(
  Matches,
  aes(
    x = Stage,
    fill = Result
  )
) +
  geom_bar(
    position = "fill",
    colour = "black"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "Match Outcomes by Competition Stage",
    x = "Competition stage",
    y = "Percentage of matches",
    fill = "Match result"
  ) +
  theme_minimal()

## Factor 2: Extra Time
### Create a table of outcomes by ExtraTime
# Count each match outcome according to whether extra time occurred.

ExtraTimeResultCount <- table(
  Matches$ExtraTime,
  Matches$Result
)

# Convert the counts into percentages within each ExtraTime group.

ExtraTimeResultPercent <- round(
  prop.table(
    ExtraTimeResultCount,
    margin = 1
  ) * 100,
  1
)
# Present the percentage table.
knitr::kable(
  ExtraTimeResultPercent,
  caption = "Percentage of Match Outcomes by Extra-Time Status"
)

### Visualise Result by ExtraTime
ggplot(
  Matches,
  aes(
    x = ExtraTime,
    fill = Result
  )
) +
  geom_bar(
    position = "fill",
    colour = "black"
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "Match Outcomes by Extra-Time Status",
    x = "Extra time: 0 = No, 1 = Yes",
    y = "Percentage of matches",
    fill = "Match result"
  ) +
  theme_minimal()