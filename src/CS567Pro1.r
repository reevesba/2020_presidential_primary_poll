library(ggplot2)
library(dplyr)

all_data<-read.csv("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv", header=TRUE)

# filter: only Ipsos polls after Nov. 15, 2019 for Democratic candidates
dem_data<-subset(all_data, party=="DEM"
                 & grepl("[ABC]", fte_grade)
                 & as.Date(as.character(end_date), "%m/%d/%y") > "2020-01-01"
                 
                 # I'd like to the mean(pct) < 15 for each candidate, by I can't figure out how
                 & pct > 15) 

# get first question of first poll per day
dem_data<-distinct(dem_data, end_date, candidate_name, .keep_all = TRUE)

ggplot(dem_data, aes(y = pct, x = end_date, group = interaction(candidate_name), 
                     color = candidate_name, linetype = candidate_name)) + 
  geom_line(size = 1) + 
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) +
  labs(title = "Democratic Primary Polling", y = "Percent", x = "Date", 
       caption = "Data: www.fivethirtyeight.com")

ggsave("dem_polls.pdf", units = "in", width = 20, height = 10, dpi = 600)
