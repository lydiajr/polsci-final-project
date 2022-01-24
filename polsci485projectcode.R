library(tidyverse)
library(eforensics)
library(lubridate)
library(stats)

data <- read_csv("Canada2011_4EFT.csv")

data <- data %>% mutate(turnout = NValid/NVoters, winnerprop = Votes/NValid, guel = if_else(distname == "Guelph", "Guelph", "Other"))
dat2 <- data %>% filter(guel == "Other")
guelph <- data %>% filter(guel == "Guelph")

#find the winner !
dee <- data %>% mutate(Liberal = Liberal/NValid, Conservative = Conservative/NValid, NDPNewDemocraticParty = NDPNewDemocraticParty/NValid) %>% select(pollnum, Conservative, NDPNewDemocraticParty, Liberal, winnerprop, turnout, NValid) %>% gather(key = "Party", value = "prop_votes", Liberal, Conservative, NDPNewDemocraticParty) %>% filter(prop_votes == winnerprop) 

t.test(dat2$turnout, guelph$turnout, alternative = "greater") 

ggplot(data, aes(x = guel, y = turnout)) + geom_boxplot() + ggtitle("Summary of Proportions of Voter Turnout per Polling Station") + labs(" ", "Proportion of Voter Turnout")

ggplot(guelph, aes(x=turnout)) + geom_histogram() + ggtitle("Distribution of Voter Turnout Proportions for Guelph Polling Stations")

question <- data %>% filter(distnum %in% c(10004, 47003))

ben <- question %>% select(pollnum, Conservative, NDPNewDemocraticParty, Liberal, winnerprop, turnout, NValid) %>% gather(key = "Party", value = "num_votes", Liberal, Conservative, NDPNewDemocraticParty)

ben2 <- data %>% select(-c(NValid, guel, winnerprop, NVoters, Votes))
ben2 <- ben2 %>% select(-turnout) %>% gather(key = "Party", value = "numVotes", -c(X, distnum, distname, pollnum, pollname, electors, X1))
