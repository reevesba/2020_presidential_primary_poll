# Project One: Democratic Primaries/Caususes
# Authors: Brad Reeves, Paul Collet, Tin Nguyen, Cheng Su
# 02/20/2020

library(ggplot2)

# -------------------------------------------------------
# Polling                                               
# -------------------------------------------------------
fte_df<-read.csv("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv", header=TRUE)

dem_df<-subset(fte_df[grepl("A|B|C", fte_df$fte_grade),], candidate_id=="13257"|candidate_id=="13345"|candidate_id=="13258"|candidate_id=="13310"|
                                                           candidate_id=="13256"|candidate_id=="13289"|candidate_id=="13327"|candidate_id=="13343")

dem_df$start_date<-as.Date(as.character(dem_df$start_date), "%m/%d/%y")

# aggregate points by candidate and date
dem_df<-setNames(aggregate(dem_df$pct, by=list(dem_df$candidate_name, dem_df$start_date), FUN=mean), c("candidate_name", "start_date", "pct"))

# caucuses/primaries
cau_df<-data.frame(date=as.Date(c("2020-02-03", "2020-02-11", "2020-02-22")), events=c("IA", "NH", "NV"))

# debates
deb_df<-data.frame(date=as.Date(c("2019-06-26", "2019-07-30", "2019-09-12", "2019-10-15", "2019-11-20",
                                  "2019-12-19", "2020-01-14", "2020-02-07", "2020-02-19")), debates=c(1:9))

colnames(dem_df)[which(names(dem_df)=="candidate_name")]<-"Candidate"

# first plots will be all data since June '19
dem_df<-subset(dem_df, start_date > "2019-06-01")

ggplot(dem_df, aes(y=pct, x=start_date, group=Candidate, color=Candidate)) + 
  geom_line(size=1.25, alpha=0.6) + 
  geom_vline(data=cau_df, aes(xintercept=date), color="black", linetype="longdash") +
  annotate("label", label=cau_df$events, x=cau_df$date, y=48, fill="yellow") +
  labs(title="Democratic Candidate Polling 2019-2020", y="Percentage Points", x="", caption="Data: www.fivethirtyeight.com") +
  scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff")) +
  scale_x_date(labels=date_format("%b"), date_breaks="1 month")

ggsave("../out/dem_polls1a.pdf", units="in", width=10, height=5, dpi=300)

ggplot(dem_df, aes(y=pct, x=start_date, group=Candidate, color=Candidate)) + 
  geom_line(size=1.25, alpha=0.6) + 
  geom_vline(data=deb_df, aes(xintercept=date), color="black", linetype="longdash") +
  annotate("label", label=deb_df$debates, x=deb_df$date, y=48, fill="yellow") +
  labs(title="Democratic Candidate Polling 2019-2020", y="Percentage Points", x="", caption="Data: www.fivethirtyeight.com") +
  scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff")) +
  scale_x_date(labels=date_format("%b"), date_breaks="1 month")

ggsave("../out/dem_polls1b.pdf", units="in", width=10, height=5, dpi=300)

# second plots will be slices of the first plots
dem_df<-subset(dem_df, start_date > "2020-02-01")

ggplot(dem_df, aes(y=pct, x=start_date, group=Candidate, color=Candidate)) + 
  geom_line(size=1.25, alpha=0.6) + 
  geom_vline(data=cau_df, aes(xintercept=date), color="black", linetype="longdash") +
  annotate("label", label=cau_df$events, x=cau_df$date, y=48, fill="yellow") +
  labs(title="Democratic Candidate Polling", y="Percentage Points", x="", caption="Data: www.fivethirtyeight.com") +
  scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff")) #+ 

ggsave("../out/dem_polls2a.pdf", units="in", width=10, height=5, dpi=300)

ggplot(dem_df, aes(y=pct, x=start_date, group=Candidate, color=Candidate)) + 
  geom_line(size=1.25, alpha=0.6) + 
  geom_vline(data=subset(deb_df, date > "2020-02-01"), aes(xintercept=date), color="black", linetype="longdash") +
  annotate("label", label=subset(deb_df, date > "2020-02-01")$debates, x=subset(deb_df, date > "2020-02-01")$date, y=48, fill="yellow") +
  labs(title="Democratic Candidate Polling", y="Percentage Points", x="", caption="Data: www.fivethirtyeight.com") +
  scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff")) #+ 

ggsave("../out/dem_polls2b.pdf", units="in", width=10, height=5, dpi=300)

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

# -------------------------------------------------------
# Fundraising                                              
# -------------------------------------------------------

# -------------------------------------------------------
# Advertising                                               
# -------------------------------------------------------

