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


# candidate names
candidates = c("Biden", "Bloomberg", "Buttigieg", "Gabbard", "Klobuchar","Sanders", "Warren")


# +++++++++++++++++++++++++++++ plot by state +++++++++++++++++++++++++++++++++++
#can: "coral", ccm: "darkolivegreen1", com: "cornflowerblue",
# ind: "darkorange", org : "darkseagreen2", pac: "burlywood1", pty: "darkkhaki"
  
# national
national_data_committee = aggregate(data = fund_all,
                               contribution_receipt_amount ~ committee_name, 
                               FUN = sum)
national_plot_data = aggregate(data = fund_all,
                           contribution_receipt_amount ~ committee_name + entity_type, 
                           FUN = sum)
national_plot_data$contribution_receipt_amount = 
  round(as.numeric(national_plot_data$contribution_receipt_amount)/10000) / 100

national_plot = ggplot(national_plot_data, 
                   aes(x = committee_name, y = contribution_receipt_amount,
                       fill = entity_type)) +
  geom_bar(stat="identity", width = 0.7, color = "#D55E00") +
 # geom_text(position = "stack",aes(label = c("53.71","200.14","83.84","15.21","30.31",
 #                                            "36.55","42.86")), 
 #           color = "#D55E00", size = 3) +
  labs(title = "Candidate's Funding Total", 
       x = "", y = "Fund Raised ($million)") +
  scale_fill_manual(values = alpha(c("coral","darkolivegreen1","cornflowerblue",
                                     "darkorange","darkseagreen2","burlywood1",
                                     "darkkhaki")))+
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank(),
        legend.position=c(.95, .8),
        legend.title=element_blank())
national_plot
ggsave("./out/fund/fund_national.jpg", national_plot)

# iowa
iowa_data = subset(fund_all, contributor_state == "IA")
iowa_plot_data = aggregate(data = iowa_data,
                            contribution_receipt_amount ~ committee_name + entity_type, 
                            FUN = sum)
iowa_plot_data$contribution_receipt_amount = 
  round(as.numeric(iowa_plot_data$contribution_receipt_amount)/10000) / 100

iowa_plot = ggplot(iowa_plot_data, 
                    aes(x = committee_name, y = contribution_receipt_amount,
                        fill = entity_type)) +
  geom_bar(stat="identity", width = 0.7, color = "#D55E00") +
  geom_text(aes(label = contribution_receipt_amount), color = "#D55E00", size = 5, vjust = -0.2) +
  labs(title = "Candidate's Funding from Iowa", 
       x = "", y = "Fund Raised ($million)") +
  scale_fill_manual(values = alpha(c("cornflowerblue","darkorange","darkseagreen2")))+
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank(),
        legend.position=c(.95, .95),
        legend.title=element_blank())
iowa_plot
ggsave("./out/fund/fund_iowa.jpg", iowa_plot)

# new hampshire
nh_data = subset(fund_all, contributor_state == "NH")
nh_plot_data = aggregate(data = nh_data,
                            contribution_receipt_amount ~ committee_name + entity_type, 
                            FUN = sum)
nh_plot_data$contribution_receipt_amount = 
  round(as.numeric(nh_plot_data$contribution_receipt_amount)/10000) / 100

nh_plot = ggplot(nh_plot_data, 
                    aes(x = committee_name, y = contribution_receipt_amount,
                        fill = entity_type)) +
  geom_bar(stat="identity", width = 0.7, color = "#D55E00") +
  geom_text(aes(label = contribution_receipt_amount), color = "#D55E00", size = 5, vjust = -0.2) +
  labs(title = "Candidate's Funding from New Hampshire", 
       x = "", y = "Fund Raised ($million)") +
  scale_fill_manual(values = alpha(c("cornflowerblue","darkorange","darkseagreen2","darkkhaki")))+
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank(),
        legend.position=c(.95, .95),
        legend.title=element_blank())
nh_plot
ggsave("./out/fund/fund_newHampshire.jpg", nh_plot)


# nevada
nevada_data = subset(fund_all, contributor_state == "NV")
nevada_plot_data = aggregate(data = nevada_data,
                            contribution_receipt_amount ~ committee_name + entity_type, 
                            FUN = sum)
nevada_plot_data$contribution_receipt_amount = 
  round(as.numeric(nevada_plot_data$contribution_receipt_amount)/10000) / 100

nevada_plot = ggplot(nevada_plot_data, 
                    aes(x = committee_name, y = contribution_receipt_amount,
                        fill = entity_type)) +
  geom_bar(stat="identity", width = 0.7, color = "#D55E00") +
  geom_text(aes(label = contribution_receipt_amount), color = "#D55E00", size = 5, vjust = -0.2) +
  labs(title = "Candidate's Funding from Nevada", 
       x = "", y = "Fund Raised ($million)") +
  scale_fill_manual(values = alpha(c("cornflowerblue","darkorange","darkseagreen2","darkkhaki")))+
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank(),
        legend.position=c(.95, .95),
        legend.title=element_blank())
nevada_plot
ggsave("./out/fund/fund_nevada.jpg", nevada_plot)

# sc
sc_data = subset(fund_all, contributor_state == "SC")
sc_plot_data = aggregate(data = sc_data,
                         contribution_receipt_amount ~ committee_name + entity_type, 
                         FUN = sum)
sc_plot_data$contribution_receipt_amount = 
  round(as.numeric(sc_plot_data$contribution_receipt_amount)/10000) / 100

sc_plot = ggplot(sc_plot_data, 
                 aes(x = committee_name, y = contribution_receipt_amount,
                     fill = entity_type)) +
  geom_bar(stat="identity", width = 0.7, color = "#D55E00") +
  geom_text(aes(label = contribution_receipt_amount), color = "#D55E00", size = 5, vjust = -0.2) +
  labs(title = "Candidate's Funding from South Carolina", 
       x = "", y = "Fund Raised ($million)") +
  scale_fill_manual(values = alpha(c("darkorange","darkseagreen2")))+
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank(),
        legend.position=c(.95, .95),
        legend.title=element_blank())
sc_plot
ggsave("./out/fund/fund_southCarolina.jpg", sc_plot)

# texas
texas_data = subset(fund_all, contributor_state == "TX")
texas_plot_data = aggregate(data = texas_data,
                                contribution_receipt_amount ~ committee_name + entity_type, 
                                FUN = sum)
texas_plot_data$contribution_receipt_amount = 
        round(as.numeric(texas_plot_data$contribution_receipt_amount)/10000) / 100
       
texas_plot = ggplot(texas_plot_data, 
                    aes(x = committee_name, y = contribution_receipt_amount,
                        fill = entity_type)) +
  geom_bar(stat="identity", width = 0.7, color = "#D55E00") +
  geom_text(aes(label = contribution_receipt_amount), color = "#D55E00", size = 5, vjust = -0.2) +
  labs(title = "Candidate's Funding from Texas", 
       x = "", y = "Fund Raised ($million)") +
  scale_fill_manual(values = alpha(c("darkolivegreen1", "cornflowerblue","darkorange","darkkhaki")))+
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank(),
        legend.position=c(.95, .95),
        legend.title=element_blank())
texas_plot
ggsave("./out/fund/fund_texas.jpg", texas_plot)

# ca
ca_data = subset(fund_all, contributor_state == "CA")
ca_plot_data = aggregate(data = ca_data,
                            contribution_receipt_amount ~ committee_name + entity_type, 
                            FUN = sum)
ca_plot_data$contribution_receipt_amount = 
  round(as.numeric(ca_plot_data$contribution_receipt_amount)/10000) / 100

ca_plot = ggplot(ca_plot_data, 
                    aes(x = committee_name, y = contribution_receipt_amount,
                        fill = entity_type)) +
  geom_bar(stat="identity", width = 0.7, color = "#D55E00") +
  geom_text(aes(label = contribution_receipt_amount), color = "#D55E00", size = 5, vjust = -0.2) +
  labs(title = "Candidate's Funding from California", 
       x = "", y = "Fund Raised ($million)") +
  scale_fill_manual(values = alpha(c("cornflowerblue","darkorange","darkseagreen2","burlywood1")))+
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank(),
        legend.position=c(.95, .95),
        legend.title=element_blank())
ca_plot
ggsave("./out/fund/fund_california.jpg", ca_plot)

# super tuesday
st_data = subset(fund_all, contributor_state %in% c("AL","AR","CA","CO","ME","MA","MN","NC","OK","TN","TX","VA","VT","UA"))
st_plot_data = aggregate(data = st_data,
                            contribution_receipt_amount ~ committee_name + entity_type, 
                            FUN = sum)
st_plot_data_sum = aggregate(data = st_data,
                         contribution_receipt_amount ~ committee_name, 
                         FUN = sum)
st_plot_data$contribution_receipt_amount = 
  round(as.numeric(st_plot_data$contribution_receipt_amount)/10000) / 100

st_plot = ggplot(st_plot_data, 
                    aes(x = committee_name, y = contribution_receipt_amount,
                        fill = entity_type)) +
  geom_bar(stat="identity", width = 0.7, color = "#D55E00") +
  labs(title = "Candidate's Funding from Super Tuesday", 
       x = "", y = "Fund Raised ($million)") +
  scale_fill_manual(values = alpha(c("coral", "darkolivegreen1", "cornflowerblue",
                                     "darkorange","darkseagreen2","burlywood1")))+
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
st_plot
ggsave("./out/fund/fund_superTuesday.jpg", st_plot)




