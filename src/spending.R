library(readxl)
library(ggplot2)
setwd("C:\\ComputerScience\\Statistic-R\\Project1")
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
setwd("C:\\ComputerScience\\Statistic-R\\Project1")
filename = 'AdsData.xlsx'
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
setwd("C:\\ComputerScience\\Statistic-R\\Project1")
filename = 'AdsData.xlsx'
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
setwd("C:\\ComputerScience\\Statistic-R\\Project1")
filename = 'AdsData.xlsx'
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
setwd("C:\\ComputerScience\\Statistic-R\\Project1")
filename = 'AdsData.xlsx'
dataset_sc <- read_excel(filename, sheet = 1,range='D4:H11', col_names=c("x_sc","test","skip2","skip3","y_sc"))
x_sc
y_sc
x_sc <- dataset_sc[['x_sc']]
y_sc <- dataset_sc[['y_sc']]
xx_sc <- barplot(y_sc,names.arg=x_sc,xaxt='n',ylim=c(0,12),ylab='Money Spent in Millions on Ads',width=0.4, 
                     col="orange", main='Money Spent on Ads (in Millions) for South Carolina state'
                     ,border='red')
# add text on top of bar
text(x=xx_sc, y= y_sc,labels = y_sc, pos =3, cex=0.8, col = "red")
# add x axis title
axis(1, at= xx_sc, labels = x_sc, tick = FALSE, las =2, line=-0.8, cex.axis=1)



########################
## California #####
setwd("C:\\ComputerScience\\Statistic-R\\Project1")
filename = 'AdsData.xlsx'
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
setwd("C:\\ComputerScience\\Statistic-R\\Project1")
filename = 'AdsData.xlsx'
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

