library(network)
library(ergm)
library(sna)
library(latticeExtra)
library(plyr)
library(GGally) #for ggnet
library(ggplot2)
library(cowplot)
theme_set(theme_gray())
library(RColorBrewer)

#Set working directory
setwd("C:/Users/Markus/OneDrive/2_Desmarais/Paper")
#setwd("/home/markus/onedrive/2_Desmarais/Paper/")

#load("Network Objects/twitter_house.rdata")
load("Network Objects/twitter_house_r.rdata")
load("Network Objects/twitter_senate.rdata")

#set color palette to be used for parties
party2_palette <- c("D" = "steelblue", "R" = "tomato" ,"I" = "slateblue2" ,"User" = "gray14")

##Senate
Senate_Graph <- ggnet2(twitter_senate, 
       node.size = 6, 
       edge.size = 0.5, 
       edge.color = "grey", 
       size = "degree", 
       size.min = 1, 
       color = "party2", 
       palette = party2_palette,
       legend.position = "none")

ggsave(filename="Figures/Senate_Graph.pdf", plot=Senate_Graph, 
       height=9, width=12)

##House
House_Graph <- ggnet2(twitter_house_r, 
       node.size = 6, 
       edge.size = 0.5, 
       edge.color = "grey", 
       size = "degree", 
       size.min = 1, 
       color = "party2", 
       palette = party2_palette,
       legend.position = "none")

ggsave(filename="Figures/House_Graph.pdf", plot=House_Graph, 
       height=9, width=12)

##House (full sample); not actually used in the paper
#House_Graph <- ggnet2(twitter_house, 
#       node.size = 6, 
#       edge.size = 0.5, 
#       edge.color = "grey", 
#       size = "degree", 
#       size.min = 1, 
#       color = "party2", 
#       palette = party2_palette,
#       legend.position = "none")
#
#ggsave(filename="Figures/House_Graph.pdf", plot=House_Graph, 
#       height=9, width=12)


##Degree Distributions

#House
#get the degrees of users and representatives
deg_house_users <- data.frame(deg_users=degree(twitter_house_r,gmode="graph")[1:1606])
deg_house_reps <- data.frame(deg_reps=degree(twitter_house_r,gmode="graph")[1607:1764])

#plot them with ggplot
g1 <- ggplot(deg_house_reps, aes(x = deg_reps)) + geom_histogram(binwidth = 5) + labs(x="Degree", y="Count", title="House Representatives")
g2 <- ggplot(deg_house_users, aes(x = deg_users)) + geom_histogram(binwidth = 1) + labs(x="Degree", y="Count", title="House Users")

#Senate
#get the degrees of users and representatives
deg_senate_users <- data.frame(deg_users=degree(twitter_senate,gmode="graph")[1:2506])
deg_senate_reps <- data.frame(deg_reps=degree(twitter_senate,gmode="graph")[2507:2578])

#plot them with ggplot
g3 <- ggplot(deg_senate_reps, aes(x = deg_reps)) + geom_histogram(binwidth = 5) + labs(x="Degree", y="Count", title="Senate Representatives")
g4 <- ggplot(deg_senate_users, aes(x = deg_users)) + geom_histogram(binwidth = 1) + labs(x="Degree", y="Count", title="Senate Users")

#plot everything together with cowplot
degree <- plot_grid(g1,g2,g3,g4)

#...and save
ggsave(filename="Figures/degree.pdf", plot=degree, 
       height=9, width=12)