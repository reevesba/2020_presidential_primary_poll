library(ggplot2)
library(scales)

#---- polling ----#
fte_df<-read.csv("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv", header=TRUE)

dem_df<-subset(fte_df[grepl("A|B|C+", fte_df$fte_grade),], candidate_id=="13257"|candidate_id=="13345"|candidate_id=="13258"|candidate_id=="13310"|
                                                           candidate_id=="13256"|candidate_id=="13289"|candidate_id=="13327"|candidate_id=="13343")

dem_df$start_date<-as.Date(as.character(dem_df$start_date), "%m/%d/%y")
dem_df<-subset(dem_df, start_date > "2019-06-15")

dem_df<-setNames(aggregate(dem_df$pct, by=list(dem_df$candidate_name, dem_df$start_date), FUN=mean), c("candidate_name", "start_date", "pct"))

cau_df<-data.frame(date=as.Date(c("2020-02-03","2020-02-11")), events=c("IA", "NH"))

colnames(dem_df)[which(names(dem_df)=="candidate_name")]<-"Candidate"

ggplot(dem_df, aes(y=pct, x=start_date, group=Candidate, color=Candidate)) + 
  geom_line(size=2) + 
  geom_vline(data=cau_df, aes(xintercept=date), color="black", linetype="longdash") +
  annotate("label", label=d$events, x=d$date, y=48, fill="yellow") +
  labs(title="Democratic Primary Polling", y="Points", x="Date", caption="Data: www.fivethirtyeight.com") +
  scale_color_manual(values=c("#4C4CFF", "#e6194B", "#ffa500", "#00ff80", "#000000", "#ff00bf", "#008000", "#00ffff")) + 
  scale_x_date(labels=date_format("%B %y"), date_breaks="1 month") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave("../out/dem_polls.pdf", units="in", width=10, height=10, dpi=300)

#--- delegates ---#
del_df<-read.csv("../dat/delegates.csv", header=TRUE)

candidate<-c("Biden", "Buttigieg", "Klobuchar", "Sanders", "Warren")
delegates<-c(sum(del_df$Biden), sum(del_df$Buttigieg), sum(del_df$Klobuchar), sum(del_df$Sanders), sum(del_df$Warren))
del_df<-data.frame(candidate, delegates)

ggplot(del_df, aes(x=candidate, y=delegates)) + 
  geom_bar(stat="identity", color="red", fill="orange") +
  geom_text(aes(label=delegates), vjust=2) +
  labs(title="Total Delegates Awarded", y="Delegates", x="Candidate", caption="Candidates need 1,991 delegates to secure their nomination on the first ballot at the Democratic National Convention")

ggsave("../out/dem_dels.pdf", units="in", width=10, height=10, dpi=300)

