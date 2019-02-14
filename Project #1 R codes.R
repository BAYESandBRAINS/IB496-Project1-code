#Read in data
data = read.csv("/users/djcarly/desktop/IB496data.csv", header = T)

#Make litter a factor variable
data$LITTER = as.factor(data$LITTER)

#Remove subject 301b from the data set. This was predetermined before publication
finaldata = subset(data, SUB!= "301b")

#Subset male and female datasets, this makes graphing easier
fdata = subset(data, SEX == "FEMALE")
mdata = subset(data, SEX == "MALE")

##Run female ANOVAs
faov = aov(fdata$TOTAL ~ fdata$AGE + fdata$LITTER)
summary(faov)

#Run male ANOVAS
maov = aov(mdata$TOTAL ~ mdata$AGE + mdata$LITTER)
summary(maov)


##BEGIN PROJECT CODE
#load the library
library (ggplot2)
library(plyr)

##Set mytheme for graphs
mytheme = theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
                panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
                panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
                panel.border=element_blank(), #gets rid of square going around the entire graph
                axis.line = element_line(colour = 'black', size = 0.5),#sets the axis line size
                axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
                axis.title.x = element_text(family="Arial", face="bold", size=14, color="black"), #size of x-axis title
                axis.title.y = element_text(family="Arial", face="bold", size=14, color="black"), #size of y-axis title
                axis.text.x = element_text(family="Arial", size=12, color="black"), #size of x-axis text
                axis.text.y = element_text(family="Arial", size=12, color="black"))#size of y-axis text




##ALL ON ONE AXIS - The scatterplot
ggplot(finaldata, aes(x=AGE, y= TOTAL, color= SEX)) + 
  geom_jitter(position=position_dodge(9))+
  stat_summary(fun.y="mean", geom='point', shape=95, size=12, position=position_dodge(9)) + 
  scale_color_manual(values=c("deeppink2", "dodgerblue3")) +
  scale_y_continuous(expand=c(0,0), limits =c(0, 90000000))+
  scale_x_continuous(limits=c(20,95), breaks = c(25, 35, 45, 60, 90))+ 
  ylab("Total Synaptophysin")+
  xlab("Age")+
  mytheme


##ALL ON ONE AXIS p2 The Boxplot edition
ggplot(finaldata, aes(x=P_AGE, y= TOTAL, fill = SEX)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("deeppink2", "dodgerblue3")) +
  scale_y_continuous(expand=c(0,0), limits =c(0, 90000000))+  
  ylab("Total Synaptophysin")+
  xlab("Age")+
  mytheme


##SPLITTING INTO TWO PLOTS AND STACKING
#Save female and male graphs separately
Females = ggplot(fdata, aes(x=AGE, y= TOTAL)) + 
  geom_point(color = "deeppink2", size=3) +
  stat_summary(fun.y="mean", geom='point', shape=95, size=15) + 
  scale_color_manual(values=c("deeppink2")) +
  scale_y_continuous(expand=c(0,0), limits =c(0, 90000000))+
  scale_x_continuous(limits=c(20,90.2), breaks = c(25, 35, 45, 60, 90))+  
  ylab("Total Synaptophysin")+
  xlab("Age (days)")+
  ggtitle("Females")+
  mytheme
  
Males = ggplot(mdata, aes(x=AGE, y= TOTAL)) + 
  geom_point(color = "dodgerblue3", size=3) +
  stat_summary(fun.y="mean", geom='point', shape=95, size=15) + 
  scale_color_manual(values=c("deeppink2")) +
  scale_y_continuous(expand=c(0,0), limits =c(0, 90000000))+  
  scale_x_continuous(limits=c(20,90.2), breaks = c(25, 35, 45, 60, 90))+  
  ylab("Total Synaptophysin")+
  xlab("Age (days)")+
  ggtitle("Males")+
  mytheme

##TWO PLOTS AS BOX PLOTS
#As above, write male and female plots separately
Females2 = ggplot(fdata, aes(x=P_AGE, y= TOTAL)) + 
  geom_boxplot(fill = "deeppink2")+
  scale_y_continuous(expand=c(0,0), limits =c(0, 90000000))+
  ylab("Total Synaptophysin")+
  xlab("Age")+
  ggtitle("Females")+
  mytheme

Males2 = ggplot(mdata, aes(x=P_AGE, y= TOTAL)) + 
  geom_boxplot(fill = "dodgerblue2")+
  scale_y_continuous(expand=c(0,0), limits =c(0, 90000000))+  #set the limits on the y-axis and tell it what to do
  ylab("Total Synaptophysin")+
  xlab("Age")+
  ggtitle("Males")+
  mytheme
 
#Load the library
library(gridExtra)

#Place the scatterplots and bar graphs side by side. Admire how pretty!
Final2 = grid.arrange(ncol = 2, nrow = 1, Females, Males)
Final4 = grid.arrange(ncol = 2, nrow = 1, Females2, Males2)
