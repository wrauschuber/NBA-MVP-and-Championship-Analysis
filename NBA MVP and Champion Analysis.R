#Data Wrangling Project - MVP & Champion data
#Weston Rauschuber and Colin Behr
rm(list=ls())

## INSTALL ALL PACKAGES ##
install.packages('readxl')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('tidyr')
install.packages('rvest')

library(readxl)
nba<- read_excel("DW NBA Project Data.xlsx")

# SCRAPE MVP DATA 
library(rvest)
url <- "https://www.basketball-reference.com/awards/mvp.html"
page <- read_html(url)
table_nodes <- page %>% html_nodes("table")
table_data <- table_nodes %>% html_table(fill = TRUE)
mvp_data <- table_data[[1]][, c(1, 3)]
colnames(mvp_data) <- c("season", "player_name")
mvp_data <- mvp_data[-1, ]

#merge 
nba <- merge(nba, mvp_data, by = "season", all.x = TRUE)

#1 and 0 MVP column in nba
nba$MVP <- ifelse(!is.na(nba$player_name.x) & nba$player_name.x == nba$player_name.y, 1, 0)

#drop & rename after merge 
nba$player_name.y<- NULL
colnames(nba)[colnames(nba) == "player_name.x"] <- "player_name"


################################################### QUESTION 1 (Important Statistics) ###########################################################
mvp_model <- glm(MVP ~ gp+pts+reb+ast+net_rating, data=nba, family= binomial)

summary(mvp_model)

# Intercept = -26.02 represents the log odds of winning MVP the mvp award when all independent variables are equal to 0
#Games Played: The exponential value is 1.133, which represents a 13.3% increase in the probability of winning MVP for every one unit increase in Games Played.

#Points: The exponential value is 1.405, which represents a 40.5% increase in the probability of winning MVP for every one unit increase in Points scored.

#Rebounds: The exponential value is 1.345, which represents a 34.5% increase in the probability of winning MVP for every one unit increase in Rebounds.

#Assists: The exponential value is 1.440, which represents a 44.0% increase in the probability of winning MVP for every one unit increase in Assists.

#Net_Rating: The exponential value is 1.086, which represents an 8.6% increase in the probability of winning MVP for every one unit increase in Net_Rating.


library(ggplot2)
data <- data.frame(Value = c(exp(0.12502), exp(0.34051), exp(0.29631), exp(0.36487), exp(0.08358)),
                   Label = c("Games Played", "Points", "Rebounds", "Assists", "Net_Rating"))
data <- data[order(data$Value, decreasing = TRUE), ]
# Create a bar chart using ggplot2
ggplot(data, aes(x = reorder(Label, -Value), y = Value))+
  geom_bar(stat = "identity", fill = "brown2", width = 0.5) +
  labs(title = "Statistic Importance for Winning MVP",
       x = "Statistic",
       y = "Exponential Value") +
  theme_minimal() 


########################################## QUESTION 2 (MVP College, Age, and Country Distribution) #################################################

#### COLLEGE BREAKDOWN ####
#filter out top 10 colleges

library(dplyr)
mvp_winners <- nba[nba$MVP == 1, ]
mvp_counts <- mvp_winners %>% group_by(college) %>% summarise(count = n())
mvp_counts <- mvp_counts %>% arrange(desc(count))
top_colleges <- head(mvp_counts, 10)

#plot college data
library(ggplot2)
ggplot(mvp_counts, aes(x = reorder(college, -count), y = count)) +
  geom_bar(stat = "identity", fill = "cyan3") +
  xlab("College") +
  ylab("Number of MVP winners") +
  ggtitle("Number of MVP winners by college")


#### MVP AGE HISTORY ####
#use mvp df
mvp_winners 

#get their age
mvp_age <- aggregate(age ~ season, data = mvp_winners, FUN = mean)
mvp_age$season <- as.character(mvp_age$season)

#line chart
ggplot(data = mvp_age, aes(x = season, y = age)) +
  geom_line(aes(group=1)) +
  geom_point(aes(group=1), color = 'blueviolet') +
  geom_path(aes(group=1), color = 'blueviolet') +
  labs(title = "History of MVP Winners' Age",
       x = "Season",
       y = "Age of MVP Winner") +
  theme_minimal() +
  scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE)]}) 


#### MVP COUNTRY ####
library(dplyr)
library(ggplot2)
library(tidyr)

# Count occurrences of each country in mvp_winners dataframe
country_count <- mvp_winners %>% 
  count(country)

# Use complete() to add missing countries with count of 0
country_count <- country_count %>% 
  complete(country, fill = list(n = 0))

# Create pie chart
ggplot(country_count, aes(x = "", y = n, fill = country)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  labs(title = "Distribution of MVP Winners by Country",
       fill = "Country") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))


################################################### QUESTION 3 (Champion Data) ##################################################################################

#SCRAPE CHAMPION DATA
library(rvest)
url <- "https://www.basketball-reference.com/playoffs/"
html <- read_html(url)

# Extract the table containing the NBA championship teams
nba_champions_table <- html %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

colnames(nba_champions_table) <- nba_champions_table[1,]

nba_champions_table <- nba_champions_table %>%
  slice(-c(1, 2))
nba_champions_table <- subset(nba_champions_table, select = -c(2, 4:10))
nba_champions_table <- nba_champions_table[nba_champions_table$Year >= 1996,]


#dictionary to match team name and abbreviation
team_lookup <- data.frame(
  team = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets",
           "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets",
           "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
           "LA Clippers", "Los Angeles Lakers", "Memphis Grizzlies", "Miami Heat",
           "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans",
           "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers",
           "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs",
           "Toronto Raptors", "Utah Jazz", "Washington Wizards"),
  
  abb = c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU",
          "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL",
          "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
)

#merge team and champion
nba_champions_table <- merge(nba_champions_table, team_lookup, by.x = "Champion", by.y = "team", all.x = TRUE)
nba_champions_table$Champion<-NULL

colnames(nba_champions_table)<- c("season","team_abbreviation")

#make season format match (i.e. 1996-97 not 1997)
nba_champions_table <- nba_champions_table %>%
  mutate(season = paste0(as.numeric(season) - 1, "-", substr(season, 3, 4)))

#reformat type to match nba frame
as.character(nba_champions_table$season)
str(nba_champions_table)

#Merge for champion column
nba <- merge(nba, nba_champions_table, by = "season", all.x = TRUE)
nba$Champion <- ifelse(!is.na(nba$team_abbreviation.x) & nba$team_abbreviation.x == nba$team_abbreviation.y, 1, 0)
nba$team_abbreviation.y<-NULL

colnames(nba)[colnames(nba) == "team_abbreviation.x"] <- "team_abbreviation"




nba_players_champions <- nba[nba$Champion == 1, c("player_name", "team_abbreviation","season", "player_height", "player_weight", "age")]

#### HEIGHT COMPARISON ####
avg_champion_team_height <- nba_players_champions %>%
  group_by(season, team_abbreviation) %>%
  summarise(average_height_cm = mean(player_height, na.rm = TRUE)) %>%
  mutate(average_height_ft = average_height_cm * 0.0328084) %>% #Convert cm to feet
  select(-average_height_cm)

ggplot(avg_champion_team_height, aes(x = season, y = average_height_ft, fill = team_abbreviation)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(average_height_ft, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, 
            size = 3) + 
  labs(title = "Average Height of Championship Teams by Season",
       x = "Season",
       y = "Average Height (ft)",
       fill = "Team") +
  theme_minimal() +
  coord_cartesian(ylim = c(6, NA)) +
  scale_x_discrete(breaks = function(x) x[c(TRUE, FALSE, FALSE)])

#### WEIGHT COMPARISON ####
avg_champion_team_weight <- nba_players_champions %>%
  group_by(season, team_abbreviation) %>%
  summarise(average_weight_kg = mean(player_weight, na.rm = TRUE)) %>%
  mutate(average_weight_lbs = average_weight_kg * 2.20462) %>%
  select(-average_weight_kg)

ggplot(avg_champion_team_weight, aes(x = season, y = average_weight_lbs, fill = team_abbreviation)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(average_weight_lbs, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, 
            size = 3) + 
  labs(title = "Average Weight of Championship Teams by Season",
       x = "Season",
       y = "Average Weight (lbs)",
       fill = "Team") +
  theme_minimal() +
  coord_cartesian(ylim = c(150, NA)) +
  scale_x_discrete(breaks = function(x) x[c(TRUE, FALSE, FALSE)])

#### AGE COMPARISON ####
avg_champion_team_age <- nba_players_champions %>%
  group_by(season, team_abbreviation) %>%
  summarise(average_age = mean(age, na.rm = TRUE)) 

ggplot(avg_champion_team_age, aes(x = season, y = average_age, fill = team_abbreviation)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(average_age,2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, 
            size = 3) + 
  labs(title = "Average Age of Championship Teams by Season",
       x = "Season",
       y = "Average Age (years)",
       fill = "Team") +
  theme_minimal() +
  coord_cartesian(ylim = c(18, NA)) +
  scale_x_discrete(breaks = function(x) x[c(TRUE, FALSE, FALSE)])




