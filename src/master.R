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


# -------------------------------------------------------
# Funding                                                 
# -------------------------------------------------------

# ++++++++++++++++++++++++++++++++ read files +++++++++++++++++++++++++++++++++++
fund_biden = read.csv("./dat/funding/biden.csv", header = TRUE)
fund_bloomberg = read.csv("./dat/funding/bloomberg.csv", header = TRUE)
fund_buttigieg1 = read.csv("./dat/funding/buttigieg_9-30-2019.csv", header = TRUE)
fund_buttigieg2 = read.csv("./dat/funding/buttigieg_2-10-2020.csv", header = TRUE)
fund_buttigieg = rbind(fund_buttigieg1, fund_buttigieg2)
fund_gabbard =  read.csv("./dat/funding/gabbard.csv", header = TRUE)
fund_klobuchar =  read.csv("./dat/funding/klobuchar.csv", header = TRUE)
fund_sanders1 =  read.csv("./dat/funding/sanders_5-31-19.csv", header = TRUE)
fund_sanders2 =  read.csv("./dat/funding/sanders_8-30-19.csv", header = TRUE)
fund_sanders = rbind(fund_sanders1, fund_sanders2)
fund_warren =  read.csv("./dat/funding/warren.csv", header = TRUE)
poll_result = read.csv("./dat/funding/result.csv", header = TRUE)

# +++++++++++++++++++++++++++++ extract data ++++++++++++++++++++++++++++++++++++
fund_biden_use = fund_biden[,c("committee_name", "entity_type",
                               "contribution_receipt_amount",
                               "contributor_state")]

fund_bloomberg_use = fund_bloomberg[,c("committee_name", "entity_type",
                                       "contribution_receipt_amount",
                                       "contributor_state")]

fund_buttigieg_use = fund_buttigieg[,c("committee_name", "entity_type",
                                       "contribution_receipt_amount",
                                       "contributor_state")]

fund_gabbard_use = fund_gabbard[,c("committee_name", "entity_type",
                                   "contribution_receipt_amount",
                                   "contributor_state")]

fund_klobuchar_use = fund_klobuchar[,c("committee_name", "entity_type",
                                       "contribution_receipt_amount",
                                       "contributor_state")]

fund_sanders_use = fund_sanders[,c("committee_name", "entity_type",
                                   "contribution_receipt_amount",
                                   "contributor_state")]

fund_warren_use = fund_warren [,c("committee_name", "entity_type",
                                  "contribution_receipt_amount",
                                  "contributor_state")]

fund_all = bind_rows(fund_biden_use, 
                     fund_bloomberg_use,
                     fund_buttigieg_use,
                     fund_gabbard_use,
                     fund_klobuchar_use,
                     fund_sanders_use,
                     fund_warren_use)
# change names
fund_all$committee_name[fund_all$committee_name == "BIDEN FOR PRESIDENT"] = "Biden"
fund_all$committee_name[fund_all$committee_name == "MIKE BLOOMBERG 2020, INC."] = "Bloomberg"
fund_all$committee_name[fund_all$committee_name == "PETE FOR AMERICA, INC."] = "Buttigeig"
fund_all$committee_name[fund_all$committee_name == "AMY FOR AMERICA"] = "Klobuchar"
fund_all$committee_name[fund_all$committee_name == "BERNIE 2020"] = "Sanders"
fund_all$committee_name[fund_all$committee_name == "TULSI NOW"] = "Gabbard"
fund_all$committee_name[fund_all$committee_name == "WARREN FOR PRESIDENT, INC."] = "Warren"


# correlation between fund and polls
states = c("IA","NH","NV","SC","AL","AR","CA","CO","ME","MA","MN","NC","OK","TN","TX","UT","VT","VA")

# sander
sanders_state = fund_sanders_use[,c("contribution_receipt_amount","contributor_state")]
sanders_state_fund = aggregate(data = sanders_state[sanders_state$contributor_state %in% states,],
                               contribution_receipt_amount ~ contributor_state, 
                               FUN = sum)
sanders_state_poll_count = poll_result[poll_result$Candidate == "Sanders",]
sanders_state_poll_count = sanders_state_poll_count[,c("State","Count")]

sanders_amount = merge(x=sanders_state_fund, y=sanders_state_poll_count, 
                       by.x="contributor_state", by.y="State")
sanders_amount = sanders_amount[,c("contribution_receipt_amount","Count")]
candidate_name = rep("Sanders",length(sanders_amount))
sanders_amount_wn = cbind(sanders_amount, candidate_name)

# biden
biden_state = fund_biden_use[,c("contribution_receipt_amount","contributor_state")]
biden_state_fund = aggregate(data = biden_state[biden_state$contributor_state %in% states,],
                             contribution_receipt_amount ~ contributor_state, 
                             FUN = sum)
biden_state_poll_count = poll_result[poll_result$Candidate == "Biden",]
biden_state_poll_count = biden_state_poll_count[,c("State","Count")]
biden_amount = merge(x=biden_state_fund, y=biden_state_poll_count, 
                     by.x="contributor_state", by.y="State")
biden_amount = biden_amount[,c("contribution_receipt_amount","Count")]
candidate_name = rep("Biden",length(biden_amount))
biden_amount_wn = cbind(biden_amount, candidate_name)

# buttigieg
buttigieg_state = fund_buttigieg_use[,c("contribution_receipt_amount","contributor_state")]
buttigieg_state_fund = aggregate(data = buttigieg_state[buttigieg_state$contributor_state %in% states,],
                                 contribution_receipt_amount ~ contributor_state, 
                                 FUN = sum)
buttigieg_state_poll_count = poll_result[poll_result$Candidate == "Buttigieg",]
buttigieg_state_poll_count = buttigieg_state_poll_count[,c("State","Count")]
buttigieg_amount = merge(x=buttigieg_state_fund, y=buttigieg_state_poll_count, 
                         by.x="contributor_state", by.y="State")
buttigieg_amount = buttigieg_amount[,c("contribution_receipt_amount","Count")]
candidate_name = rep("Buttigieg",length(buttigieg_amount))
buttigieg_amount_wn = cbind(buttigieg_amount, candidate_name)

# warren
warren_state = fund_warren_use[,c("contribution_receipt_amount","contributor_state")]
warren_state_fund = aggregate(data = warren_state[warren_state$contributor_state %in% states,],
                              contribution_receipt_amount ~ contributor_state, 
                              FUN = sum)
warren_state_poll_count = poll_result[poll_result$Candidate == "Warren",]
warren_state_poll_count = warren_state_poll_count[,c("State","Count")]
warren_amount = merge(x=warren_state_fund, y=warren_state_poll_count, 
                      by.x="contributor_state", by.y="State")
warren_amount = warren_amount[,c("contribution_receipt_amount","Count")]
candidate_name = rep("Warren",length(warren_amount))
warren_amount_wn = cbind(warren_amount, candidate_name)

# klobuchar
klobuchar_state = fund_klobuchar_use[,c("contribution_receipt_amount","contributor_state")]
klobuchar_state_fund = aggregate(data = klobuchar_state[klobuchar_state$contributor_state %in% states,],
                                 contribution_receipt_amount ~ contributor_state, 
                                 FUN = sum)
klobuchar_state_poll_count = poll_result[poll_result$Candidate == "Klobuchar",]
klobuchar_state_poll_count = klobuchar_state_poll_count[,c("State","Count")]
klobuchar_amount = merge(x=klobuchar_state_fund, y=klobuchar_state_poll_count, 
                         by.x="contributor_state", by.y="State")
klobuchar_amount = klobuchar_amount[,c("contribution_receipt_amount","Count")]
candidate_name = rep("Klobuchar",length(klobuchar_amount))
klobuchar_amount_wn = cbind(klobuchar_amount, candidate_name)


amount_all = bind_rows(sanders_amount_wn,biden_amount_wn,buttigieg_amount_wn,
                       warren_amount_wn,klobuchar_amount_wn)
amount_all_plot = ggplot(amount_all, aes(x=contribution_receipt_amount, y=Count, 
                                         shape=candidate_name, color=candidate_name)) + 
  geom_point() + scale_y_sqrt() + scale_x_sqrt() + 
  labs(title = "Funding and poll count", 
       x = "funding($million)", y = "Poll Count")
amount_all_plot
ggsave("./out/fund/amount_all.jpg", amount_all_plot)

# curse fitting
biden_fit_log = lm(data = biden_amount,Count ~ log(contribution_receipt_amount))
summary(biden_fit_log)
biden_fit_plot_log = ggplot(data = biden_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ log(x)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Biden's Funding", 
       x = "", y = "Fund Raised ($million)")
biden_fit_plot_log
ggsave("./out/fund/biden_fit_log.jpg", biden_fit_plot_log)

biden_fit_poly = lm(data = biden_amount,Count ~ poly(contribution_receipt_amount,3))
summary(biden_fit_poly)
biden_fit_plot_poly = ggplot(data = biden_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Biden's Funding", 
       x = "", y = "Fund Raised ($million)")
biden_fit_plot_poly
ggsave("./out/fund/biden_fit_poly.jpg", biden_fit_plot_poly)


sanders_fit_log = lm(data = sanders_amount,Count ~ log(contribution_receipt_amount))
summary(sanders_fit_log)
sanders_fit_plot_log = ggplot(data = biden_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ log(x)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Sanders's Funding", 
       x = "", y = "Fund Raised ($million)")
sanders_fit_plot_log
ggsave("./out/fund/sanders_fit_log.jpg", sanders_fit_plot_log)

sanders_fit_poly = lm(data = sanders_amount,Count ~ poly(contribution_receipt_amount,3))
summary(sanders_fit_poly)
sanders_fit_plot_poly = ggplot(data = biden_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Sanders's Funding", 
       x = "", y = "Fund Raised ($million)")
sanders_fit_plot_poly
ggsave("./out/fund/sanders_fit_poly.jpg", sanders_fit_plot_poly)



warren_fit_log = lm(data = warren_amount,Count ~ log(contribution_receipt_amount))
summary(warren_fit_log)
warren_fit_plot_log = ggplot(data = warren_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ log(x)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Warren's Funding", 
       x = "", y = "Fund Raised ($million)")
warren_fit_plot_log
ggsave("./out/fund/warren_fit_log.jpg", warren_fit_plot_log)

warren_fit_poly = lm(data = warren_amount,Count ~ poly(contribution_receipt_amount,3))
summary(warren_fit_poly)
warren_fit_plot_poly = ggplot(data = warren_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Warren's Funding", 
       x = "", y = "Fund Raised ($million)")
warren_fit_plot_poly
ggsave("./out/fund/warren_fit_poly.jpg", warren_fit_plot_log)



klobuchar_fit_log = lm(data = klobuchar_amount,Count ~ log(contribution_receipt_amount))
summary(klobuchar_fit_log)
klobuchar_fit_plot_log = ggplot(data = klobuchar_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ log(x)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Klobuchar's Funding", 
       x = "", y = "Fund Raised ($million)")
klobuchar_fit_plot_log
ggsave("./out/fund/klobuchar_fit_log.jpg", klobuchar_fit_plot_log)

klobuchar_fit_poly = lm(data = klobuchar_amount,Count ~ poly(contribution_receipt_amount,3))
summary(klobuchar_fit_poly)
klobuchar_fit_plot_poly = ggplot(data = klobuchar_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Klobuchar's Funding", 
       x = "", y = "Fund Raised ($million)")
klobuchar_fit_plot_poly
ggsave("./out/fund/klobuchar_fit_poly.jpg", klobuchar_fit_plot_poly)



buttigieg_fit_log = lm(data = buttigieg_amount,Count ~ log(contribution_receipt_amount))
summary(buttigieg_fit_log)
buttigieg_fit_plot_log = ggplot(data = buttigieg_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ log(x)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Buttigieg's Funding", 
       x = "", y = "Fund Raised ($million)")
buttigieg_fit_plot_log
ggsave("./out/fund/buttigieg_fit_log.jpg", buttigieg_fit_plot_log)

buttigieg_fit_poly = lm(data = buttigieg_amount,Count ~ poly(contribution_receipt_amount,3))
summary(buttigieg_fit_poly)
buttigieg_fit_plot_poly = ggplot(data = buttigieg_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Buttigieg's Funding", 
       x = "", y = "Fund Raised ($million)")
buttigieg_fit_plot_poly
ggsave("./out/fund/buttigieg_fit_poly.jpg", buttigieg_fit_plot_poly)


# -------------------------------------------------------
# Funding curve fit                                             
# -------------------------------------------------------
states = c("IA","NH","NV","SC","AL","AR","CA","CO","ME","MA","MN","NC","OK","TN","TX","UT","VT","VA")

# sander
sanders_state = fund_sanders_use[,c("contribution_receipt_amount","contributor_state")]
sanders_state_fund = aggregate(data = sanders_state[sanders_state$contributor_state %in% states,],
                               contribution_receipt_amount ~ contributor_state, 
                               FUN = sum)
sanders_state_poll_count = poll_result[poll_result$Candidate == "Sanders",]
sanders_state_poll_count = sanders_state_poll_count[,c("State","Count")]

sanders_amount = merge(x=sanders_state_fund, y=sanders_state_poll_count, 
                       by.x="contributor_state", by.y="State")
sanders_amount = sanders_amount[,c("contribution_receipt_amount","Count")]
candidate_name = rep("Sanders",length(sanders_amount))
sanders_amount_wn = cbind(sanders_amount, candidate_name)

# biden
biden_state = fund_biden_use[,c("contribution_receipt_amount","contributor_state")]
biden_state_fund = aggregate(data = biden_state[biden_state$contributor_state %in% states,],
                             contribution_receipt_amount ~ contributor_state, 
                             FUN = sum)
biden_state_poll_count = poll_result[poll_result$Candidate == "Biden",]
biden_state_poll_count = biden_state_poll_count[,c("State","Count")]
biden_amount = merge(x=biden_state_fund, y=biden_state_poll_count, 
                     by.x="contributor_state", by.y="State")
biden_amount = biden_amount[,c("contribution_receipt_amount","Count")]
candidate_name = rep("Biden",length(biden_amount))
biden_amount_wn = cbind(biden_amount, candidate_name)

# buttigieg
buttigieg_state = fund_buttigieg_use[,c("contribution_receipt_amount","contributor_state")]
buttigieg_state_fund = aggregate(data = buttigieg_state[buttigieg_state$contributor_state %in% states,],
                                 contribution_receipt_amount ~ contributor_state, 
                                 FUN = sum)
buttigieg_state_poll_count = poll_result[poll_result$Candidate == "Buttigieg",]
buttigieg_state_poll_count = buttigieg_state_poll_count[,c("State","Count")]
buttigieg_amount = merge(x=buttigieg_state_fund, y=buttigieg_state_poll_count, 
                         by.x="contributor_state", by.y="State")
buttigieg_amount = buttigieg_amount[,c("contribution_receipt_amount","Count")]
candidate_name = rep("Buttigieg",length(buttigieg_amount))
buttigieg_amount_wn = cbind(buttigieg_amount, candidate_name)

# warren
warren_state = fund_warren_use[,c("contribution_receipt_amount","contributor_state")]
warren_state_fund = aggregate(data = warren_state[warren_state$contributor_state %in% states,],
                              contribution_receipt_amount ~ contributor_state, 
                              FUN = sum)
warren_state_poll_count = poll_result[poll_result$Candidate == "Warren",]
warren_state_poll_count = warren_state_poll_count[,c("State","Count")]
warren_amount = merge(x=warren_state_fund, y=warren_state_poll_count, 
                      by.x="contributor_state", by.y="State")
warren_amount = warren_amount[,c("contribution_receipt_amount","Count")]
candidate_name = rep("Warren",length(warren_amount))
warren_amount_wn = cbind(warren_amount, candidate_name)

# klobuchar
klobuchar_state = fund_klobuchar_use[,c("contribution_receipt_amount","contributor_state")]
klobuchar_state_fund = aggregate(data = klobuchar_state[klobuchar_state$contributor_state %in% states,],
                                 contribution_receipt_amount ~ contributor_state, 
                                 FUN = sum)
klobuchar_state_poll_count = poll_result[poll_result$Candidate == "Klobuchar",]
klobuchar_state_poll_count = klobuchar_state_poll_count[,c("State","Count")]
klobuchar_amount = merge(x=klobuchar_state_fund, y=klobuchar_state_poll_count, 
                         by.x="contributor_state", by.y="State")
klobuchar_amount = klobuchar_amount[,c("contribution_receipt_amount","Count")]
candidate_name = rep("Klobuchar",length(klobuchar_amount))
klobuchar_amount_wn = cbind(klobuchar_amount, candidate_name)

# all
amount_all = bind_rows(sanders_amount_wn,biden_amount_wn,buttigieg_amount_wn,
                       warren_amount_wn,klobuchar_amount_wn)
amount_all_plot = ggplot(amount_all, aes(x=contribution_receipt_amount, y=Count, 
                                         shape=candidate_name, color=candidate_name)) + 
  geom_point() + scale_y_sqrt() + scale_x_sqrt() + 
  labs(title = "Funding and poll count", 
       x = "funding($million)", y = "Poll Count")
amount_all_plot
ggsave("./out/fund/amount_all.jpg", amount_all_plot)

# curve fitting
# biden
biden_fit_log = lm(data = biden_amount,Count ~ log(contribution_receipt_amount))
summary(biden_fit_log)
biden_fit_plot_log = ggplot(data = biden_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ log(x)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Biden's Funding", 
       x = "", y = "Fund Raised ($million)")
biden_fit_plot_log
ggsave("./out/fund/biden_fit_log.jpg", biden_fit_plot_log)

biden_fit_poly = lm(data = biden_amount,Count ~ poly(contribution_receipt_amount,3))
summary(biden_fit_poly)
biden_fit_plot_poly = ggplot(data = biden_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Biden's Funding", 
       x = "", y = "Fund Raised ($million)")
biden_fit_plot_poly
ggsave("./out/fund/biden_fit_poly.jpg", biden_fit_plot_poly)

# sanders
sanders_fit_log = lm(data = sanders_amount,Count ~ log(contribution_receipt_amount))
summary(sanders_fit_log)
sanders_fit_plot_log = ggplot(data = biden_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ log(x)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Sanders's Funding", 
       x = "", y = "Fund Raised ($million)")
sanders_fit_plot_log
ggsave("./out/fund/sanders_fit_log.jpg", sanders_fit_plot_log)

sanders_fit_poly = lm(data = sanders_amount,Count ~ poly(contribution_receipt_amount,3))
summary(sanders_fit_poly)
sanders_fit_plot_poly = ggplot(data = biden_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Sanders's Funding", 
       x = "", y = "Fund Raised ($million)")
sanders_fit_plot_poly
ggsave("./out/fund/sanders_fit_poly.jpg", sanders_fit_plot_poly)

# warren
warren_fit_log = lm(data = warren_amount,Count ~ log(contribution_receipt_amount))
summary(warren_fit_log)
warren_fit_plot_log = ggplot(data = warren_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ log(x)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Warren's Funding", 
       x = "", y = "Fund Raised ($million)")
warren_fit_plot_log
ggsave("./out/fund/warren_fit_log.jpg", warren_fit_plot_log)

warren_fit_poly = lm(data = warren_amount,Count ~ poly(contribution_receipt_amount,3))
summary(warren_fit_poly)
warren_fit_plot_poly = ggplot(data = warren_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Warren's Funding", 
       x = "", y = "Fund Raised ($million)")
warren_fit_plot_poly
ggsave("./out/fund/warren_fit_poly.jpg", warren_fit_plot_log)

# klobuchar
klobuchar_fit_log = lm(data = klobuchar_amount,Count ~ log(contribution_receipt_amount))
summary(klobuchar_fit_log)
klobuchar_fit_plot_log = ggplot(data = klobuchar_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ log(x)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Klobuchar's Funding", 
       x = "", y = "Fund Raised ($million)")
klobuchar_fit_plot_log
ggsave("./out/fund/klobuchar_fit_log.jpg", klobuchar_fit_plot_log)

klobuchar_fit_poly = lm(data = klobuchar_amount,Count ~ poly(contribution_receipt_amount,3))
summary(klobuchar_fit_poly)
klobuchar_fit_plot_poly = ggplot(data = klobuchar_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Klobuchar's Funding", 
       x = "", y = "Fund Raised ($million)")
klobuchar_fit_plot_poly
ggsave("./out/fund/klobuchar_fit_poly.jpg", klobuchar_fit_plot_poly)

# buttigieg
buttigieg_fit_log = lm(data = buttigieg_amount,Count ~ log(contribution_receipt_amount))
summary(buttigieg_fit_log)
buttigieg_fit_plot_log = ggplot(data = buttigieg_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ log(x)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Buttigieg's Funding", 
       x = "", y = "Fund Raised ($million)")
buttigieg_fit_plot_log
ggsave("./out/fund/buttigieg_fit_log.jpg", buttigieg_fit_plot_log)

buttigieg_fit_poly = lm(data = buttigieg_amount,Count ~ poly(contribution_receipt_amount,3))
summary(buttigieg_fit_poly)
buttigieg_fit_plot_poly = ggplot(data = buttigieg_amount, aes(x = contribution_receipt_amount, y = Count)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3)) + 
  scale_y_sqrt() + scale_x_sqrt() +
  labs(title = "Buttigieg's Funding", 
       x = "", y = "Fund Raised ($million)")
buttigieg_fit_plot_poly
ggsave("./out/fund/buttigieg_fit_poly.jpg", buttigieg_fit_plot_poly)


