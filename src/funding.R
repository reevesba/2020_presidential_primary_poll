library(ggplot2)

# read files
fund_biden = read.csv("./dat/funding/biden.csv", header = TRUE)
fund_bloomberg = read.csv("./dat/funding/bloomberg.csv", header = TRUE)
fund_buttigieg1 = read.csv("./dat/funding/buttigieg_9-30-2019.csv", header = TRUE)
fund_buttigieg2 = read.csv("./dat/funding/buttigieg_2-10-2020.csv", header = TRUE)
fund_gabbard =  read.csv("./dat/funding/gabbard.csv", header = TRUE)
fund_klobuchar =  read.csv("./dat/funding/klobuchar.csv", header = TRUE)
fund_sanders1 =  read.csv("./dat/funding/sanders_5-31-19.csv", header = TRUE)
fund_sanders2 =  read.csv("./dat/funding/sanders_8-30-19.csv", header = TRUE)
fund_warren =  read.csv("./dat/funding/warren.csv", header = TRUE)


fund_buttigieg1_sum = aggregate(data = fund_buttigieg1,
                                contribution_receipt_amount ~ contributor_state, 
                                FUN = sum)
fund_buttigieg2_sum = aggregate(data = fund_buttigieg2,
                                contribution_receipt_amount ~ contributor_state, 
                                FUN = sum)

fund_bloomberg_sum = aggregate(data = fund_bloomberg,
                               contribution_receipt_amount ~ contributor_state, 
                               FUN = sum)

fund_gabbard_sum = aggregate(data = fund_gabbard,
                             contribution_receipt_amount ~ contributor_state, 
                             FUN = sum)

fund_klobuchar_sum = aggregate(data = fund_klobuchar,
                               contribution_receipt_amount ~ contributor_state, 
                               FUN = sum)

fund_sanders1_sum = aggregate(data = fund_sanders1,
                              contribution_receipt_amount ~ contributor_state, 
                              FUN = sum)
fund_sanders2_sum = aggregate(data = fund_sanders2,
                              contribution_receipt_amount ~ contributor_state, 
                              FUN = sum)

fund_warren_sum = aggregate(data = fund_warren,
                            contribution_receipt_amount ~ contributor_state, 
                            FUN = sum)


# candidate names
candidates = c("Biden", "Bloomberg", "Buttigieg", "Gabbard", "Klobuchar","Sanders", "Warren")

# fund from Nevada
nevada_fund = c(0.834, 0, 0.179, 0.043, 0.038,0.109, 0.090)
nevada_list = data.frame(candidate = candidates, fund = nevada_fund)
nevada_plot = ggplot(data = nevada_list, aes(x = candidate, y = fund)) +
  ylim(0, 1) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 6, vjust = -0.4) +
  labs(title = "Candidate's Funding from Nevada", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
  panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                colour = "darkgrey"),
  panel.grid.major.x = element_blank())
nevada_plot
ggsave("./out/fund/fund_nevada.eps", nevada_plot)
ggsave("./out/fund/fund_nevada.jpg", nevada_plot)
nevada_plot1 = ggplot(data = nevada_list, aes(x = candidate, y = fund)) +
  ylim(0, 11) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 8, vjust = -0.4) +
  labs(title = "Candidate's Funding from Nevada", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 14), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
nevada_plot1
ggsave("./out/fund/fund_nevada1.eps", nevada_plot1)
ggsave("./out/fund/fund_nevada1.jpg", nevada_plot1)

# fund from Virginia
virginia_fund = c(0.388, 0.001, 1.724, 0.072, 0.337, 0.286, 0.388) 
virginia_list = data.frame(candidate = candidates, fund = virginia_fund)
virginia_plot = ggplot(data = virginia_list, aes(x = candidate, y = fund)) +
  ylim(0, 2.0) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 6, vjust = -0.4) +
  labs(title = "Candidate's Funding from Virginia", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
  panel.background = element_rect(fill = 'white',color='darkgrey'),
  panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                  colour = "darkgrey"),
  panel.grid.major.x = element_blank())
virginia_plot
ggsave("./out/fund/fund_virginia.eps", virginia_plot)
ggsave("./out/fund/fund_virginia.jpg", virginia_plot)
virginia_plot1 = ggplot(data = virginia_list, aes(x = candidate, y = fund)) +
  ylim(0, 11) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 8, vjust = -0.4) +
  labs(title = "Candidate's Funding from Virginia", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 14), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
virginia_plot1
ggsave("./out/fund/fund_virginia1.eps", virginia_plot1)
ggsave("./out/fund/fund_virginia1.jpg", virginia_plot1)

# fund from Vermont
vermont_fund = c(0.113, 0, 0.091, 0.017, 0.035, 10.326, 0.084) 
vermont_list = data.frame(candidate = candidates, fund = vermont_fund)
vermont_plot = ggplot(data = vermont_list, aes(x = candidate, y = fund)) +
  ylim(0, 11) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 6, vjust = -0.4) +
  labs(title = "Candidate's Funding from Vermont", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
  panel.background = element_rect(fill = 'white',color='darkgrey'),
  panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                  colour = "darkgrey"),
  panel.grid.major.x = element_blank())
vermont_plot
ggsave("./out/fund/fund_vermont.eps", vermont_plot)
ggsave("./out/fund/fund_vermont.jpg", vermont_plot)
vermont_plot1 = ggplot(data = vermont_list, aes(x = candidate, y = fund)) +
  ylim(0, 11) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 8, vjust = -0.4) +
  labs(title = "Candidate's Funding from Vermont", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 14), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
vermont_plot1
ggsave("./out/fund/fund_vermont1.eps", vermont_plot1)
ggsave("./out/fund/fund_vermont1.jpg", vermont_plot1)

# fund from South Carolina
scarolina_fund = c(0.419, 0, 0.302, 0.002, 0.063, 0.078, 0.058) 
scarolina_list = data.frame(candidate = candidates, fund = scarolina_fund)
scarolina_plot = ggplot(data = scarolina_list, aes(x = candidate, y = fund)) +
  ylim(0, 0.44) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 6, vjust = -0.4) +
  labs(title = "Candidate's Funding from Sourth Carolina", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size=14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
scarolina_plot
ggsave("./out/fund/fund_scarolina.eps", scarolina_plot)
ggsave("./out/fund/fund_scarolina.jpg", scarolina_plot)
scarolina_plot1 = ggplot(data = scarolina_list, aes(x = candidate, y = fund)) +
  ylim(0, 11) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 8, vjust = -0.4) +
  labs(title = "Candidate's Funding from Sourth Carolina", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 14), axis.title=element_text(size=14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
scarolina_plot1
ggsave("./out/fund/fund_scarolina1.eps", scarolina_plot1)
ggsave("./out/fund/fund_scarolina1.jpg", scarolina_plot1)

# fund from Texas
texas_fund = c(1.827, 0.001, 1.853, 0.256, 0.487, 0.544, 0.663) 
texas_list = data.frame(candidate = candidates, fund = texas_fund)
texas_plot = ggplot(data = texas_list, aes(x = candidate, y = fund)) +
  ylim(0, 2.0) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 6, vjust = -0.4) +
  labs(title = "Candidate's Funding from Texas", 
       x = "", y = "Fund Raised ($million)") +
   theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
         panel.background = element_rect(fill = 'white',color='darkgrey'),
         panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                         colour = "darkgrey"),
         panel.grid.major.x = element_blank())
texas_plot
ggsave("./out/fund/fund_texas.eps", texas_plot)
ggsave("./out/fund/fund_texas.jpg", texas_plot)
texas_plot1 = ggplot(data = texas_list, aes(x = candidate, y = fund)) +
  ylim(0, 11) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 8, vjust = -0.4) +
  labs(title = "Candidate's Funding from Texas", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 14), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
texas_plot1
ggsave("./out/fund/fund_texas1.eps", texas_plot1)
ggsave("./out/fund/fund_texas1.jpg", texas_plot1)

# fund from Georgia
georgia_fund = c(0.670, 0.001, 0.921, 0.055, 0.260, 0.199, 0.212) 
georgia_list = data.frame(candidate = candidates, fund = georgia_fund)
georgia_plot = ggplot(data = georgia_list, aes(x = candidate, y = fund)) +
  ylim(0, 1.0) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 6, vjust = -0.4) +
  labs(title = "Candidate's Funding from Georgia", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
georgia_plot
ggsave("./out/fund/fund_georgia.eps", georgia_plot)
ggsave("./out/fund/fund_georgia.jpg", georgia_plot)
georgia_plot1 = ggplot(data = georgia_list, aes(x = candidate, y = fund)) +
  ylim(0, 11) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 8, vjust = -0.4) +
  labs(title = "Candidate's Funding from Georgia", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 14), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
georgia_plot1
ggsave("./out/fund/fund_georgia1.eps", georgia_plot1)
ggsave("./out/fund/fund_georgia1.jpg", georgia_plot1)

# fund from Florida
florida_fund = c(3.538, 0.001, 2.362, 0.191, 0.615, 0.485, 0.475) 
florida_list = data.frame(candidate = candidates, fund = florida_fund)
florida_plot = ggplot(data = florida_list, aes(x = candidate, y = fund)) +
  ylim(0, 4.0) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 6, vjust = -0.4) +
  labs(title = "Candidate's Funding from Florida", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
florida_plot
ggsave("./out/fund/fund_florida.eps", florida_plot)
ggsave("./out/fund/fund_florida.jpg", florida_plot)
florida_plot1 = ggplot(data = florida_list, aes(x = candidate, y = fund)) +
  ylim(0, 11) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 8, vjust = -0.4) +
  labs(title = "Candidate's Funding from Florida", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 14), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
florida_plot
ggsave("./out/fund/fund_florida.eps", florida_plot)
ggsave("./out/fund/fund_florida.jpg", florida_plot)


# fund from Iowa
iowa_fund = c(0.149, 0, 0.247, 0.022, 0.090, 0.097, 0.064) 
iowa_list = data.frame(candidate = candidates, fund = iowa_fund)
iowa_plot = ggplot(data = iowa_list, aes(x = candidate, y = iowa_fund)) +
  ylim(0, 0.27) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 6, vjust = -0.4) +
  labs(title = "Candidate's Funding from Iowa", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
iowa_plot
ggsave("./out/fund/fund_iowa.eps", iowa_plot)
ggsave("./out/fund/fund_iowa.jpg", iowa_plot)

iowa_plot1 = ggplot(data = iowa_list, aes(x = candidate, y = iowa_fund)) +
  ylim(0, 11) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 8, vjust = -0.4) +
  labs(title = "Candidate's Funding from Iowa", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 14), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
iowa_plot1
ggsave("./out/fund/fund_iowa1.eps", iowa_plot1)
ggsave("./out/fund/fund_iowa1.jpg", iowa_plot1)



# fund from New Hampshire
newHampshire_fund = c(0.184, 0, 0.324, 0.046, 0.107, 0.111, 0.106) 
newHampshire_list = data.frame(candidate = candidates, fund = newHampshire_fund)
newHampshire_plot = ggplot(data = newHampshire_list, aes(x = candidate, y = fund)) +
  ylim(0, 0.35) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 6, vjust = -0.4) +
  labs(title = "Candidate's Funding from New Hampshire", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size=14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
newHampshire_plot
ggsave("./out/fund/fund_newHampshire.eps", newHampshire_plot)
ggsave("./out/fund/fund_newHampshire.jpg", newHampshire_plot)
newHampshire_plot1 = ggplot(data = newHampshire_list, aes(x = candidate, y = fund)) +
  ylim(0, 11) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 8, vjust = -0.4) +
  labs(title = "Candidate's Funding from New Hampshire", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 14), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
newHampshire_plot1
ggsave("./out/fund/fund_newHampshire1.eps", newHampshire_plot1)
ggsave("./out/fund/fund_newHampshire1.jpg", newHampshire_plot1)


# fund from Alabama
alabama_fund = c(0.164, 0, 0.200, 0.013, 0.030, 0.047, 0.047) 
alabama_list = data.frame(candidate = candidates, fund = alabama_fund)
alabama_plot = ggplot(data = alabama_list, aes(x = candidate, y = fund)) +
  ylim(0, 0.25) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 6, vjust = -0.4) +
  labs(title = "Candidate's Funding from Alabama", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size=14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
alabama_plot
ggsave("./out/fund/fund_alabama.eps", alabama_plot)
ggsave("./out/fund/fund_alabama.jpg", alabama_plot)
alabama_plot1 = ggplot(data = alabama_list, aes(x = candidate, y = fund)) +
  ylim(0, 11) +
  geom_bar(stat="identity", width = 0.7, fill = "orange1",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 8, vjust = -0.4) +
  labs(title = "Candidate's Funding from Alabama", 
       x = "", y = "Fund Raised ($million)") +
  theme(axis.text=element_text(size = 14), axis.title=element_text(size = 14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
alabama_plot1
ggsave("./out/fund/fund_alabama1.eps", alabama_plot1)
ggsave("./out/fund/fund_alabama1.jpg", alabama_plot1)








