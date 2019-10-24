## Basic Graphing and Data Wrangling ##

install.packages("tidyverse")
library(tidyverse)

?tidyverse
?ggplot

# Data from https://sportsdatabase.com/nba/query?output=default&sdql=after+all+star+break%2C+assists%2C+ats+margin%2C+ats+streak%2C+attendance%2C+biggest+lead%2C+blocks%2C+conference%2C+date%2C+day%2C+defensive+rebounds%2C+division%2C+dpa%2C+dps%2C+fast+break+points%2C+field+goals+attempted%2C+field+goals+made%2C+fouls%2C+free+throws+attempted%2C+free+throws+made%2C+game+number%2C+lead+changes%2C+line%2C+losses%2C+margin%2C+margin+after+the+first%2C+margin+after+the+third%2C+margin+at+the+half%2C+matchup+losses%2C+matchup+wins%2C+minutes%2C+month%2C+offensive+rebounds%2C+officials%2C+opponents%2C+ou+margin%2C+ou+streak%2C+overtime%2C+playoffs%2C+points%2C+points+in+the+paint%2C+position%2C+quarter+scores%2C+rebounds%2C+rest%2C+round%2C+season%2C+seed%2C+series+game%2C+series+games%2C+series+losses%2C+series+wins%2C+site%2C+site+streak%2C+start+time%2C+steals%2C+streak%2C+team%2C+o%3Ateam%2C+team+rebounds%2C+three+pointers+attempted%2C+three+pointers+made%2C+time+of+game%2C+time+zone%2C+times+tied%2C+total%2C+turnovers%2C+wins+%40+2018+%3E+season+%3E+2010&submit=++S+D+Q+L+%21++

# Load a cleaned data set
load("/Users/solsen/Documents/SDAC/NBA_Data.RData") #Data Loaded as NBA_data
# For Windows: "C\\Users\\..."

View(NBA_data)

# Simple Scatterplot Visualizations

ggplot(data = NBA_data) +
  geom_point(mapping = aes(x = Assists, y = Points))

ggplot(NBA_data) +
  geom_point(aes(x = Assists, y = Points, color = Margin > 0))


# Scatterplot of Blocks vs. Three Point Shots
Threes_V_Blocks <- NBA_data %>%
  select(Three_Pointers_Attempted, Blocks, Season, Team) %>%
  group_by(Season, Team) %>%
  summarise(
    Three_Pt_Atms = sum(Three_Pointers_Attempted, na.rm = T),
    Blocked_Shots = sum(Blocks, na.rm = T)
  )

Threes_V_Blocks

ggplot(Threes_V_Blocks) +
  geom_point(aes(x = Three_Pt_Atms, y = Blocked_Shots, color = Season)) +
  labs(title = "Number of Blocked Shots versus Number of Three Point Attempts",
       x = "Number of Three Point Shots Attempted",
       y = "Number of Blocked Shots")


# Number of Three Point Shots across Seasons

Three_point_Sum <- NBA_data %>%
  select(Three_Pointers_Attempted, Season, Team) %>%
  group_by(Season, Team) %>%
  summarise(
    Three_Pt_Atms = sum(Three_Pointers_Attempted, na.rm = T)
  )

Three_point_Sum

ggplot(Three_point_Sum) +
  geom_line(aes(x = Season, y = Three_Pt_Atms, color = Team)) +
  labs(title = "Total Number of Three Point Attempts per Season",
       y = "Total Number of Three Point Attempts") +
  scale_x_discrete(limits = Three_point_Sum$Season)


Three_point_Avg <- NBA_data %>%
  select(Three_Pointers_Attempted, Season, Team) %>%
  group_by(Season, Team) %>%
  summarise(
    Three_Pt_Atms = mean(Three_Pointers_Attempted, na.rm = T)
  )

Three_point_Avg

ggplot(Three_point_Avg) + 
  geom_line(aes(x = Season, y = Three_Pt_Atms, color = Team)) +
  labs(title = "Average Number of Three Point Attempts per Season",
       y = "Average Number of Three Point Attempts") +
  scale_x_discrete(limits = Three_point_Avg$Season)

# Number of Games Each Season

Length_Of_Season <- NBA_data %>%
  select(Season, Playoffs) %>%
  filter(Playoffs == 0) %>%
  group_by(Season) %>%
  summarise(
    Number_Of_Games = length(Season)
  ) %>%
  mutate(Time = "Regular_Season")

Length_Of_Season

Length_Of_Playoffs <- NBA_data %>%
  select(Season, Playoffs) %>%
  filter(Playoffs == 1) %>%
  group_by(Season) %>%
  summarise(
    Number_Of_Games = length(Season)
  ) %>%
  mutate(Time = "Playoffs")

Length_Of_Playoffs

Length_Of_DF <- full_join(Length_Of_Season, Length_Of_Playoffs) %>% data.frame()

Length_Of_DF

ggplot(Length_Of_DF) +
  geom_bar(aes(x = Season, y = Number_Of_Games, fill = Time), stat = "identity",
           position = "stack") +
  labs(title = "Number of Games Played Each Year", y = "Number of Games") +
  scale_x_discrete(limits = Length_Of_DF$Season)


## How to create NBA shot charts in R
## Tutorial from: 
## https://thedatagame.com.au/2015/09/27/how-to-create-nba-shot-charts-in-r/


#### Step 1: Get the Data  ####

install.packages("rjson")
library(rjson)

# Shot Data for Stephen Curry

# Steps from Online Tutorial that did NOT work
# playerID <- 201939
# shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2014-15&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2014-15&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
# 
# # import from JSON
# shotData <- fromJSON(file = shotURL2, method="C")

# Change this file path based on your own file location
shotData <- fromJSON(file = "/Users/solsen/Documents/SDAC/shotchartdetail.json")

# Clean up the Data

# unlist shot data, save into a data frame
shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))

# shot data headers
colnames(shotDataf) <- shotData$resultSets[[1]][[2]]

# covert x and y coordinates into numeric
shotDataf$LOC_X <- as.numeric(as.character(shotDataf$LOC_X))
shotDataf$LOC_Y <- as.numeric(as.character(shotDataf$LOC_Y))
shotDataf$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))

# have a look at the data
View(shotDataf)


#### Basic Chart ####

# simple plot using EVENT_TYPE to colour the dots
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
  geom_point(aes(colour = EVENT_TYPE))


#### Shot Charts ####

install.packages("grid")
library(grid)
install.packages("jpeg")
library(jpeg)
install.packages("RCurl")
library(RCurl)

# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# plot using NBA court background and colour by shot zone
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) +
  xlim(-250, 250) +
  ylim(-50, 420)


# plot using ggplot and NBA court background image
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) +
  xlim(250, -250) +
  ylim(-50, 420) +
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", unique(shotDataf$PLAYER_NAME), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, lineheight = 0.9, face = "bold"))


install.packages("gridExtra")
library(gridExtra)
install.packages("png")
library(png)

# scrape player photo and save as a raster object
playerimg.URL <- "https://stats.nba.com/media/players/132x132/201939.png"
playerImg <- rasterGrob(readPNG(getURLContent(playerimg.URL)), 
                        width=unit(0.15, "npc"), height=unit(0.15, "npc"))

# plot using ggplot and NBA court background
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point(aes(colour = EVENT_TYPE, alpha = 0.8), size = 3) +
  scale_color_manual(values = c("#008000", "#FF6347")) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", unique(shotDataf$PLAYER_NAME), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

# add player photo and footnote to the plot
pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "thedatagame.com.au", just = "centre", vjust = 50)



install.packages("hexbin")
library(hexbin)

# plot shots using ggplot, hex bins, NBA court backgroung image.
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -52, 418) +
  stat_binhex(bins = 25, colour = "gray", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", unique(shotDataf$PLAYER_NAME), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

# add player photo and footnote to the plot
pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "thedatagame.com.au", just = "centre", vjust = 50)




# exclude backcourt shots
shotDataS <- shotDataf[which(!shotDataf$SHOT_ZONE_BASIC=='Backcourt'), ]

# summarise shot data
library(plyr)
shotS <- ddply(shotDataS, .(SHOT_ZONE_BASIC), summarize, 
               SHOTS_ATTEMPTED = length(SHOT_MADE_FLAG),
               SHOTS_MADE = sum(as.numeric(as.character(SHOT_MADE_FLAG))),
               MLOC_X = mean(LOC_X),
               MLOC_Y = mean(LOC_Y))

# calculate shot zone accuracy and add zone accuracy labels
shotS$SHOT_ACCURACY <- (shotS$SHOTS_MADE / shotS$SHOTS_ATTEMPTED)
shotS$SHOT_ACCURACY_LAB <- paste(as.character(round(100 * shotS$SHOT_ACCURACY, 1)), "%", sep="")

# plot shot accuracy per zone
ggplot(shotS, aes(x=MLOC_X, y=MLOC_Y)) + 
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point(aes(colour = SHOT_ZONE_BASIC, size = SHOT_ACCURACY, alpha = 0.8), size = 8) +
  geom_text(aes(colour = SHOT_ZONE_BASIC, label = SHOT_ACCURACY_LAB), vjust = -1.2, size = 8) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  ggtitle(paste("Shot Accuracy\n", unique(shotDataf$PLAYER_NAME), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size = 12),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

# add player photo and footnote to the plot
pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "thedatagame.com.au", just = "centre", vjust = 50)



#### ---------------------------------------------------------------- ####

## SpatialBall R package examples from:
## https://cran.r-project.org/web/packages/SpatialBall/vignettes/Starting.html

install.packages("SpatialBall")
library(SpatialBall)

## Player Level Visualization

data("season2017")
ShotSeasonGraphPlayer(season2017, player = "Stephen Curry")

ShotSeasonGraphPlayer(season2017, player = "Stephen Curry", type = "PCT")


ShotSeasonGraphPlayer(season2017, player = "Kyle Singler")

PointShotSeasonGraphPlayer(season2017, player = "Kyle Singler")

PointShotSeasonGraphPlayer(season2017, player = "Kyle Singler", Type = "Made")

PointShotSeasonGraphPlayer(season2017, player = "Kyle Singler", Type = "Made", kernel = FALSE)

## Team Level Visualization

data("season2017")
OffShotSeasonGraphTeam(season2017, team = "GSW")

OffShotSeasonGraphTeam(season2017, team = "GSW", type = "PCT")

DefShotSeasonGraphTeam(season2017, team = "Sas", type = "PCT")


## League Level Visualization

data("season2017")
ShotSeasonGraph(season2017, quant = 0.4)


#### ---------------------------------------------------------------- ####

## BallR Shiny App from:
## https://toddwschneider.com/posts/ballr-interactive-nba-shot-charts-with-r-and-shiny/

rm(list=ls())

# Copy and Paste this code into the console

packages = c("shiny", "ggplot2", "hexbin", "dplyr", "httr", "jsonlite")
install.packages(packages, repos = "https://cran.rstudio.com/")
library(shiny)
runGitHub("ballr", "toddwschneider")
