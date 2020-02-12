# NOTE: 
# The points in the CSV are based on various questions from various polls. 
# Using the website for comparison, we might have to average out some 
# data to get the 'result' we're looking for.

# CONCERN: 
# Did the website scale the points for us, or do we have to do it?

# Running Code:
# For RStudio, you have to select all the code and press run.

library(ggplot2)
library(dplyr)

all_data<-read.csv("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv", header=TRUE)
dem_data<-subset(all_data, party=="DEM")

# get Ipsos poll only, we can add more polls later
dem_data<-subset(dem_data, pollster_id=="744")

# get polls after Nov. 15, 2019
dem_data<-subset(dem_data, as.Date(as.character(start_date), "%m/%d/%y") > "2019-11-15")

# get first question of first poll per day
dem_data<-distinct(dem_data, start_date, candidate_name, .keep_all=TRUE)

ggplot(dem_data, aes(y = pct, x = start_date, group = interaction(candidate_name), color = candidate_name)) + 
  geom_line() + 
  labs(title = "Democratic Primary Polling", y = "Points", x = "Date", caption = "Data: www.fivethirtyeight.com")

ggsave("dem_polls.png", units="in", width=10, height=10, dpi=300)


