# Project One: Democratic Primaries/Caususes
# Authors: Brad Reeves, Paul Collet, Tin Nguyen, Cheng Su
# 02/20/2020

library(ggplot2)
library(scales)
library(stringr)
library(dplyr)

# -------------------------------------------------------
# Polling                                               
# -------------------------------------------------------
fte_df<-read.csv("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv", header=TRUE)

# candidate names which is reusable
candidateNames<-list("Biden","Bloomberg","Buttigieg","Gabbard","Klobuchar","Sanders","Steyer","Warren")

dem_df<-subset(fte_df[grepl("A|B|C", fte_df$fte_grade),], answer %in% candidateNames)

polls<-subset(fte_df[grepl("A|B|C", fte_df$fte_grade),])
polls<-distinct(polls, pollster, sponsors, fte_grade)
write.csv(polls, "../dat/polls.csv")

dem_df$start_date<-as.Date(as.character(dem_df$start_date), "%m/%d/%y")

# aggregate points by candidate and date
dem_df<-setNames(aggregate(dem_df$pct, by=list(dem_df$candidate_name, dem_df$start_date), FUN=mean), c("candidate_name", "start_date", "pct"))

# caucuses/primaries
cau_df<-data.frame(date=as.Date(c("2020-02-03", "2020-02-11", "2020-02-22", "2020-02-29", "2020-03-03")), events=c("IA", "NH", "NV", "SC", "ST"))

# debates
deb_df<-data.frame(date=as.Date(c("2020-01-14", "2020-02-07", "2020-02-19", "2020-02-26")), debates=c(7:10))

colnames(dem_df)[which(names(dem_df)=="candidate_name")]<-"Candidate"
caption<-str_replace_all(toString(c("Data: www.fivethirtyeight.com (as of ", format(Sys.Date(), format="%m/%d/%Y"), ")")), ",", "")

all_df<-subset(dem_df, start_date > "2020-02-01")

# all events
ggplot(all_df, aes(y=pct, x=start_date, group=Candidate, color=Candidate)) + 
  geom_line(size=1.25, alpha=0.6) + 
  geom_vline(data=cau_df, aes(xintercept=date), color="black", linetype="longdash") +
  annotate("label", label=cau_df$events, x=cau_df$date, y=48, fill="#F5D76E") +
  labs(title="Democratic Candidate Polling", y="Percentage Points", x="", caption=caption) +
  scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff")) #+ 

ggsave("../out/polls/dem_polls-events.pdf", units="in", width=10, height=5, dpi=300)

# all debates
ggplot(all_df, aes(y=pct, x=start_date, group=Candidate, color=Candidate)) + 
  geom_line(size=1.25, alpha=0.6) + 
  geom_vline(data=subset(deb_df, date > "2020-02-01"), aes(xintercept=date), color="black", linetype="longdash") +
  annotate("label", label=subset(deb_df, date > "2020-02-01")$debates, x=subset(deb_df, date > "2020-02-01")$date, y=48, fill="#F5D76E") +
  labs(title="Democratic Candidate Polling", y="Percentage Points", x="", caption=caption) +
  scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff")) #+ 

ggsave("../out/polls/dem_polls-debates.pdf", units="in", width=10, height=5, dpi=300)

# Iowa
temp_df<-subset(dem_df, start_date > "2020-01-10" & start_date < "2020-02-10")
cau_df<-data.frame(date=as.Date(c("2020-02-03")), events=c("IA"))
deb_df<-data.frame(date=as.Date(c("2020-01-14", "2020-02-07")), debates=c(7:8))

ggplot(temp_df, aes(y=pct, x=start_date, group=Candidate, color=Candidate)) + 
  geom_line(size=1.25, alpha=0.6) + 
  geom_vline(data=deb_df, aes(xintercept=date), color="black", linetype="longdash") +
  geom_vline(data=cau_df, aes(xintercept=date), color="black", linetype="longdash") +
  annotate("label", label=deb_df$debates, x=deb_df$date, y=48, fill="#F5D76E") +
  annotate("label", label=cau_df$events, x=cau_df$date, y=48, fill="#F5D76E") +
  labs(title="Polling During Iowa Caucus", y="Percentage Points", x="", caption=caption) +
  scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff")) #+ 

ggsave("../out/polls/Iowa.pdf", units="in", width=10, height=5, dpi=300)

# New Hampshire
temp_df<-subset(dem_df, start_date > "2020-02-01" & start_date < "2020-02-20")
cau_df<-data.frame(date=as.Date(c("2020-02-11")), events=c("NH"))
deb_df<-data.frame(date=as.Date(c("2020-02-07", "2020-02-19")), debates=c(8:9))

ggplot(temp_df, aes(y=pct, x=start_date, group=Candidate, color=Candidate)) + 
  geom_line(size=1.25, alpha=0.6) + 
  geom_vline(data=deb_df, aes(xintercept=date), color="black", linetype="longdash") +
  geom_vline(data=cau_df, aes(xintercept=date), color="black", linetype="longdash") +
  annotate("label", label=deb_df$debates, x=deb_df$date, y=48, fill="#F5D76E") +
  annotate("label", label=cau_df$events, x=cau_df$date, y=48, fill="#F5D76E") +
  labs(title="Polling During New Hampshire Primary", y="Percentage Points", x="", caption=caption) +
  scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff")) #+ 

ggsave("../out/polls/NewHampshire.pdf", units="in", width=10, height=5, dpi=300)

# Nevada
temp_df<-subset(dem_df, start_date > "2020-02-15" & start_date < "2020-02-29")
cau_df<-data.frame(date=as.Date(c("2020-02-22")), events=c("NV"))
deb_df<-data.frame(date=as.Date(c("2020-02-19", "2020-02-26")), debates=c(9:10))

ggplot(temp_df, aes(y=pct, x=start_date, group=Candidate, color=Candidate)) + 
  geom_line(size=1.25, alpha=0.6) + 
  geom_vline(data=deb_df, aes(xintercept=date), color="black", linetype="longdash") +
  geom_vline(data=cau_df, aes(xintercept=date), color="black", linetype="longdash") +
  annotate("label", label=deb_df$debates, x=deb_df$date, y=48, fill="#F5D76E") +
  annotate("label", label=cau_df$events, x=cau_df$date, y=48, fill="#F5D76E") +
  labs(title="Polling During Nevada Primary", y="Percentage Points", x="", caption=caption) +
  scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff")) #+ 

ggsave("../out/polls/Nevada.pdf", units="in", width=10, height=5, dpi=300)

# South Carolina & Super Tuesday
temp_df<-subset(dem_df, start_date > "2020-02-20")
cau_df<-data.frame(date=as.Date(c("2020-02-29", "2020-03-03")), events=c("NV", "ST"))
deb_df<-data.frame(date=as.Date(c("2020-02-26")), debates=c(10))

ggplot(temp_df, aes(y=pct, x=start_date, group=Candidate, color=Candidate)) + 
  geom_line(size=1.25, alpha=0.6) + 
  geom_vline(data=deb_df, aes(xintercept=date), color="black", linetype="longdash") +
  geom_vline(data=cau_df, aes(xintercept=date), color="black", linetype="longdash") +
  annotate("label", label=deb_df$debates, x=deb_df$date, y=48, fill="#F5D76E") +
  annotate("label", label=cau_df$events, x=cau_df$date, y=48, fill="#F5D76E") +
  labs(title="Polling During SC and ST Primaries", y="Percentage Points", x="", caption=caption) +
  scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff")) #+ 

ggsave("../out/polls/SC-ST.pdf", units="in", width=10, height=5, dpi=300)

# -------------------------------------------------------
# Fundraising                                              
# -------------------------------------------------------

# -------------------------------------------------------
# Advertising                                               
# -------------------------------------------------------

# -------------------------------------------------------
# Delegates                                              | 
# -------------------------------------------------------
del_df<-read.csv("../dat/delegates.csv", header=TRUE)

candidate<-colnames(del_df)[5:12]
delegates<-c(sum(del_df$Klobuchar), sum(del_df$Sanders), sum(del_df$Warren), sum(del_df$Biden), sum(del_df$Bloomberg), sum(del_df$Buttigieg), sum(del_df$Steyer), sum(del_df$Gabbard))
delegates[is.na(delegates)]<-0
del_df<-data.frame(candidate, delegates)

ggplot(del_df, aes(x=candidate, y=delegates)) + 
  geom_bar(stat="identity", color="red", fill="orange") +
  geom_text(aes(label=delegates), vjust=-0.5, color="red") +
  labs(title="Total Delegates Awarded", y="Delegates", x="Candidate", caption="Candidates need 1,991 delegates to secure their nomination on the first ballot at the Democratic National Convention")

ggsave("../out/dem_dels.pdf", units="in", width=10, height=5, dpi=300)
