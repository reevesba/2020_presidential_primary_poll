# Project One: Democratic Primaries/Caususes
# Authors: Brad Reeves, Paul Collet, Tin Nguyen, Cheng Su
# 02/20/2020

library(ggplot2)
library(scales)

# -------------------------------------------------------
# Polling                                               
# -------------------------------------------------------
fte_df<-read.csv("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv", header=TRUE)

# candidate names which is reusable
candidateNames<-list("Biden","Bloomberg","Buttigieg","Gabbard","Klobuchar","Sanders","Steyer","Warren")

dem_df<-subset(fte_df[grepl("A|B|C", fte_df$fte_grade),], answer %in% candidateNames)

dem_df$start_date<-as.Date(as.character(dem_df$start_date), "%m/%d/%y")

# aggregate points by candidate and date
dem_df<-setNames(aggregate(dem_df$pct, by=list(dem_df$candidate_name, dem_df$start_date), FUN=mean), c("candidate_name", "start_date", "pct"))

# caucuses/primaries
cau_df<-data.frame(date=as.Date(c("2020-02-03")), events=c("IA"))

# debates
deb_df<-data.frame(date=as.Date(c("2020-01-14", "2020-02-07")), debates=c(7:8))

colnames(dem_df)[which(names(dem_df)=="candidate_name")]<-"Candidate"

# constant graphing parameters
aesSet = aes(y=pct, x=start_date, group=Candidate, color=Candidate)
lines  = geom_line(size=1.25, alpha=0.6)
vlinesE = geom_vline(data=cau_df, aes(xintercept=date), color="black", linetype="longdash")
vlinesD = geom_vline(data=subset(deb_df, date > "2020-01-10" ), aes(xintercept=date), color="black", linetype="longdash")
labInfo = labs(title="Iowa Primary Polling 2020", y="Percentage Points", x="", caption="Data: www.fivethirtyeight.com")
scaleColor = scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff"))
scaleDate = scale_x_date(labels=date_format("%b"), date_breaks="1 month")

eventLines = annotate("label", label=cau_df$events, x=cau_df$date, y=48, fill="#F5D76E")
debateLines = annotate("label", label=subset(deb_df, date > "2020-01-10" )$debates, 
                       x=subset(deb_df, date > "2020-01-10")$date, y=48, fill="#F5D76E")

# first plots will be all data since June '19
temp_df<-subset(dem_df, start_date > "2020-01-10" & start_date < "2020-02-10")

ggplot(temp_df, aesSet) + lines + 
  vlinesD + debateLines + 
  vlinesE + eventLines + 
  labInfo + scaleColor + scaleDate
ggsave("Iowa_polls1.pdf", units="in", width=10, height=5, dpi=300)


