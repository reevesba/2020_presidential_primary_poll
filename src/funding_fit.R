setwd("~/projects/2020_presidential_primary_polling")

library(ggplot2)
library(dplyr)
library(plyr)

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
cor.test(anders_amount$contribution_receipt_amount, sanders_amount$Count, alternative = "less", method = "pearson")


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
