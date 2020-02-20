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

# sum up the fund by state
fund_biden_sum = aggregate(data = fund_biden,
                           contribution_receipt_amount ~ contributor_state, 
                           FUN = sum)

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
nevada_fund = c(832.7, 0, 178.9, 42.6, 38.1,109.3, 90.1) 
nevada_list = data.frame(candidate = candidates, fund = nevada_fund)
nevada_plot = ggplot(data = nevada_list, aes(x = candidate, y = fund)) +
  ylim(0, 3600) +
  geom_bar(stat="identity", width = 0.6, fill = "#E69F00",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 3, vjust = -0.4) +
  labs(title = "Candidate's Funding from Nevada", 
       x = "", y = "Fund Raised ($1000)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
  panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                colour = "darkgrey"),
  panel.grid.major.x = element_blank())
nevada_plot
ggsave("./out/fund/fund_nevada.eps", nevada_plot)
ggsave("./out/fund/fund_nevada.jpg", nevada_plot)

# fund from Virginia
virginia_fund = c(387.5, 1.0, 1724.4, 71.8, 337.0,286.1, 387.5) 
virginia_list = data.frame(candidate = candidates, fund = virginia_fund)
virginia_plot = ggplot(data = virginia_list, aes(x = candidate, y = fund)) +
  ylim(0, 3600) +
  geom_bar(stat="identity", width = 0.6, fill = "#E69F00",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 3, vjust = -0.4) +
  labs(title = "Candidate's Funding from Virginia", 
       x = "", y = "Fund Raised ($1000)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
  panel.background = element_rect(fill = 'white',color='darkgrey'),
  panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                  colour = "darkgrey"),
  panel.grid.major.x = element_blank())
virginia_plot
ggsave("./out/fund/fund_virginia.eps", virginia_plot)
ggsave("./out/fund/fund_virginia.jpg", virginia_plot)

# fund from Vermont
vermont_fund = c(113.4, 0, 91.0, 16.8, 34.7, 10325.8, 83.9) 
vermont_list = data.frame(candidate = candidates, fund = vermont_fund)
vermont_plot = ggplot(data = vermont_list, aes(x = candidate, y = fund)) +
  ylim(0, 11000) +
  geom_bar(stat="identity", width = 0.6, fill = "#E69F00",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 3, vjust = -0.4) +
  labs(title = "Candidate's Funding from Vermont", 
       x = "", y = "Fund Raised ($1000)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
  panel.background = element_rect(fill = 'white',color='darkgrey'),
  panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                  colour = "darkgrey"),
  panel.grid.major.x = element_blank())
vermont_plot
ggsave("./out/fund/fund_vermont.eps", vermont_plot)
ggsave("./out/fund/fund_vermont.jpg", vermont_plot)

# fund from South Carolina
scarolina_fund = c(418.8, 0, 301.8, 1.5, 63.1, 77.8, 57.7) 
scarolina_list = data.frame(candidate = candidates, fund = scarolina_fund)
scarolina_plot = ggplot(data = scarolina_list, aes(x = candidate, y = fund)) +
  ylim(0, 3600) +
  geom_bar(stat="identity", width = 0.6, fill = "#E69F00",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 3, vjust = -0.4) +
  labs(title = "Candidate's Funding from Sourth Carolina", 
       x = "", y = "Fund Raised ($1000)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
scarolina_plot
ggsave("./out/fund/fund_scarolina.eps", scarolina_plot)
ggsave("./out/fund/fund_scarolina.jpg", scarolina_plot)

# fund from Texas
texas_fund = c(1826.8, 1.3, 1853.4, 255.9, 487.3, 544.2, 662.6) 
texas_list = data.frame(candidate = candidates, fund = texas_fund)
texas_plot = ggplot(data = texas_list, aes(x = candidate, y = fund)) +
  ylim(0, 3600) +
  geom_bar(stat="identity", width = 0.6, fill = "#E69F00",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 3, vjust = -0.4) +
  labs(title = "Candidate's Funding from Texas", 
       x = "", y = "Fund Raised ($1000)") +
   theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
         panel.background = element_rect(fill = 'white',color='darkgrey'),
         panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                         colour = "darkgrey"),
         panel.grid.major.x = element_blank())
texas_plot
ggsave("./out/fund/fund_texas.eps", texas_plot)
ggsave("./out/fund/fund_texas.jpg", texas_plot)

# fund from Georgia
georgia_fund = c(670.3, 0.6, 920.6, 55.4, 259.8, 198.7, 211.6) 
georgia_list = data.frame(candidate = candidates, fund = georgia_fund)
georgia_plot = ggplot(data = georgia_list, aes(x = candidate, y = fund)) +
  ylim(0, 3600) +
  geom_bar(stat="identity", width = 0.6, fill = "#E69F00",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", size = 3, vjust = -0.4) +
  labs(title = "Candidate's Funding from Georgia", 
       x = "", y = "Fund Raised ($1000)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
georgia_plot
ggsave("./out/fund/fund_georgia.eps", georgia_plot)
ggsave("./out/fund/fund_georgia.jpg", georgia_plot)

# fund from Florida
florida_fund = c(3537.9, 1.1, 2361.6, 190.7, 615.0, 484.5, 474.8) 
florida_list = data.frame(candidate = candidates, fund = florida_fund)
florida_plot = ggplot(data = florida_list, aes(x = candidate, y = fund)) +
  ylim(0, 3600) +
  geom_bar(stat="identity", width = 0.6, fill = "#E69F00",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", vjust = -0.4) +
  labs(title = "Candidate's Funding from Florida", 
       x = "", y = "Fund Raised ($1000)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
florida_plot
ggsave("./out/fund/fund_florida.eps", florida_plot)
ggsave("./out/fund/fund_florida.jpg", florida_plot)


# fund from Iowa
iowa_fund = c(148.9, 0, 246.9, 22.1, 89.9, 96.8, 63.9) 
iowa_list = data.frame(candidate = candidates, fund = iowa_fund)
iowa_plot = ggplot(data = iowa_list, aes(x = candidate, y = fund)) +
  ylim(0, 3600) +
  geom_bar(stat="identity", width = 0.6, fill = "#E69F00",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", vjust = -0.4) +
  labs(title = "Candidate's Funding from Iowa", 
       x = "", y = "Fund Raised ($1000)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
iowa_plot
ggsave("./out/fund/fund_iowa.eps", iowa_plot)
ggsave("./out/fund/fund_iowa.jpg", iowa_plot)

# fund from New Hampshire
newHampshire_fund = c(183.5, 0, 323.6, 45.8, 106.9, 110.7, 106.0) 
newHampshire_list = data.frame(candidate = candidates, fund = newHampshirefund)
newHampshire_plot = ggplot(data = iowa_list, aes(x = candidate, y = fund)) +
  ylim(0, 3600) +
  geom_bar(stat="identity", width = 0.6, fill = "#E69F00",color = "#D55E00") +
  geom_text(aes(label = fund), color = "#D55E00", vjust = -0.4) +
  labs(title = "Candidate's Funding from New Hampshire", 
       x = "", y = "Fund Raised ($1000)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        panel.background = element_rect(fill = 'white',color='darkgrey'),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "darkgrey"),
        panel.grid.major.x = element_blank())
newHampshire_plot
ggsave("./out/fund/fund_newHampshire.eps", newHampshire_plot)
ggsave("./out/fund/fund_newHampshire.jpg", newHampshire_plot)












