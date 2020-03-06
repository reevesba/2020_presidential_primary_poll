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
# NOTE
# this section will read data from AdsData.xlsx file which is located in the dat folder
# END NOTE
library(readxl)
library(ggplot2)
setwd("https://github.com/safrannn/2020_presidential_primary_poll/edit/master/src/dat")
filename = 'AdsData.xlsx'
dataset <- read_excel(filename,sheet = 1, range='A4:B11',col_names=c("x", "y"))
#plot <- ggplot(dataset,aes(x=x,y=y)) + geom_bar() 
x1 <- dataset[['x']]
y1 <- dataset[['y']]
xx <- barplot(y1,names.arg=x1,xaxt='n',ylim=c(0,350),ylab='Money Spent in Millions on Ads',width=0.4, 
        col="orange", main='Money Spent on Ads (in Millions) for Democratic Presidential Nomination'
        ,border='red')
# add text on top of bar
text(x=xx, y= y1,labels = y1, pos =3, cex=0.8, col = "red")
# add x axis title
axis(1, at= xx, labels = x1, tick = FALSE, las =2, line=-0.8, cex.axis=1)

###########################
# IOWA #
dataset_IOWA <- read_excel(filename, sheet = 1, range='D4:E11', col_names=c("x_iowa","y_iowa"))
x_iowa <- dataset_IOWA[['x_iowa']]
y_iowa <- dataset_IOWA[['y_iowa']]
print(x_iowa)
xx_iowa <- barplot(y_iowa,names.arg=x_iowa,xaxt='n',ylim=c(0,12.6),ylab='Money Spent in Millions on Ads',width=0.4, 
              col="orange", main='Money Spent on Ads (in Millions) for IOWA state'
              ,border='red')
# add text on top of bar
text(x=xx_iowa, y= y_iowa,labels = y_iowa, pos =3, cex=0.8, col = "red")
# add x axis title
axis(1, at= xx_iowa, labels = x_iowa, tick = FALSE, las =2, line=-0.8, cex.axis=1)

###########################
# New Hamshire #
dataset_new <- read_excel(filename, sheet = 1,range='D4:F11', col_names=c("x_new","skip","y_new"))
x_new <- dataset_new[['x_new']]
y_new <- dataset_new[['y_new']]
print(y_new)
xx_new <- barplot(y_new,names.arg=x_new,xaxt='n',ylim=c(0,3.5),ylab='Money Spent in Millions on Ads',width=0.4, 
                   col="orange", main='Money Spent on Ads (in Millions) for New Hampshire state'
                   ,border='red')
# add text on top of bar
text(x=xx_new, y= y_new,labels = y_new, pos =3, cex=0.8, col = "red")
# add x axis title
axis(1, at= xx_new, labels = x_new, tick = FALSE, las =2, line=-0.8, cex.axis=1)

###########################
# Nevada #
dataset_nevada <- read_excel(filename, sheet = 1,range='D4:G11', col_names=c("x_nevada","skip1","skip2","y_nevada"))
x_nevada
#x_nevada <- dataset_nevada[['x_nevada']]
y_nevada <- dataset_nevada[['y_nevada']]
xx_nevada <- barplot(y_nevada,names.arg=x_nevada,xaxt='n',ylim=c(0,12),ylab='Money Spent in Millions on Ads',width=0.4, 
                  col="orange", main='Money Spent on Ads (in Millions) for Nevada state'
                  ,border='red')
# add text on top of bar
text(x=xx_nevada, y= y_nevada,labels = y_nevada, pos =3, cex=0.8, col = "red")
# add x axis title
axis(1, at= xx_nevada, labels = x_nevada, tick = FALSE, las =2, line=-0.8, cex.axis=1)

###########################
# South Carolina #
dataset_sc <- read_excel(filename, sheet = 1,range='D4:H11', col_names=c("x_sc","test","skip2","skip3","y_sc"))
x_sc <- dataset_sc[['x_sc']]
y_sc <- dataset_sc[['y_sc']]
y_sc
xx_sc <- barplot(y_sc,names.arg=x_sc,xaxt='n',ylim=c(0,14),ylab='Money Spent in Millions on Ads',width=0.4, 
                     col="orange", main='Money Spent on Ads (in Millions) for South Carolina state'
                     ,border='red')
# add text on top of bar
text(x=xx_sc, y= y_sc,labels = y_sc, pos =3, cex=0.8, col = "red")
# add x axis title
axis(1, at= xx_sc, labels = x_sc, tick = FALSE, las =2, line=-0.8, cex.axis=1)

########################
## California #####
dataset_cali <- read_excel(filename, sheet = 1,range='i4:j8', col_names=c("x_cali","y_cali"))
x_cali <- dataset_cali[['x_cali']]
y_cali <- dataset_cali[['y_cali']]
xx_cali <- barplot(y_cali,names.arg=x_cali,xaxt='n',ylim=c(0,65),ylab='Money Spent in Millions on Ads',width=0.4, 
                     col="orange", main='Money Spent on Ads (in Millions) for California state'
                     ,border='red')
# add text on top of bar
text(x=xx_cali, y= y_cali,labels = y_cali, pos =3, cex=0.8, col = "red")
# add x axis title
axis(1, at= xx_cali, labels = x_cali, tick = FALSE, las =2, line=-0.8, cex.axis=1)



########################
## Texas #####
dataset_cali <- read_excel(filename, sheet = 1,range='i4:k8', col_names=c("x_cali","skip","y_cali"))
x_cali <- dataset_cali[['x_cali']]
y_cali <- dataset_cali[['y_cali']]
xx_cali <- barplot(y_cali,names.arg=x_cali,xaxt='n',ylim=c(0,1.5),ylab='Money Spent in Millions on Ads',width=0.2, 
                   col="orange", main='Money Spent on Ads (in Millions) for Texas state'
                   ,border='red')
# add text on top of bar
text(x=xx_cali, y= y_cali,labels = y_cali, pos =3, cex=0.8, col = "red")
# add x axis title
axis(1, at= xx_cali, labels = x_cali, tick = FALSE, las =2, line=-0.8, cex.axis=1)


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
