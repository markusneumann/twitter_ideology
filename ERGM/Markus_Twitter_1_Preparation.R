#This rscript takes Barbera's files...
#...and turns them into the network used in the other scripts

library(network)
library(ergm)
library(sna)
library(latticeExtra)
library(plyr)

#Set seed
set.seed(1)
  
#Set working directory
setwd("C:/Users/Markus/OneDrive/2_Desmarais/Paper")
#setwd("/home/markus/onedrive/2_Desmarais/Paper/")

#Load data
load("Data/twitter_environment.RData")



#create users attribute data frame
twitter_attributes <- merge(users, contr, by = "uid")
twitter_attributes <- subset(twitter_attributes, is.na(cfscore)==F) #remove people who have no cfscore
twitter_attributes <- subset(twitter_attributes, amount>=0) #remove people who have donate less than 0 dollars

#create elites attribute data frame
elites <- subset(elites, is.na(dw.nom.1)==F)
elites <- subset(elites, title%in%c("House","Senate")) #only congress

#only include users who are still in the user attributes list
#same for elites
twitter_users <- twitter_users[rownames(twitter_users)%in%twitter_attributes$uid,]
twitter_users <- twitter_users[,colnames(twitter_users)%in%elites$screen_name]

#cut down the attribute lists to the same size and combine them
twitter_attributes <- twitter_attributes[!duplicated(twitter_attributes$uid),]
elites <- elites[elites$screen_name%in%colnames(twitter_users),]
elites <- elites[!duplicated(elites$screen_name),]

twitter_attributes <- rbind.fill(twitter_attributes,elites)
twitter_attributes$uid[is.na(twitter_attributes$uid)==T] <- twitter_attributes$screen_name[is.na(twitter_attributes$uid)==T]

##sort
twitter_users <- twitter_users[order(rownames(twitter_users)),]
twitter_users <- twitter_users[,order(colnames(twitter_users))]

#Create the network object from adjacency matrix and attribute list
twitter <- network(twitter_users, vertex.attrnames = c(rownames(twitter_users),colnames(twitter_users)), directed = F, bipartite = T, matrix.type = "incidence")
twitter



##add/change attributes

#ideology
twitter_attributes$ideology <- NA
twitter_attributes$ideology[is.na(twitter_attributes$dw.nom.1)==F] <- twitter_attributes$dw.nom.1[is.na(twitter_attributes$dw.nom.1)==F]*50+50
twitter_attributes$ideology[is.na(twitter_attributes$cfscore)==F] <- twitter_attributes$cfscore[is.na(twitter_attributes$cfscore)==F]*10+50

#extremism
twitter_attributes$extremism <- abs(twitter_attributes$ideology-50)

#set user ttle in addition to house and senate
twitter_attributes$title[is.na(twitter_attributes$title)==T] <- "User"

#add representative district
representatives <- read.csv("Data/representatives.csv", header = F)
representatives <- subset(representatives, V1%in%c(112,111))
representatives <- representatives[,c(2,4)]
names(representatives) <- c("rep_ICPSRcode","rep_district")
representatives <- representatives[!duplicated(representatives$rep_ICPSRcode),]

twitter_attributes <- merge(twitter_attributes,representatives, by.x="icpsr.id", by.y="rep_ICPSRcode", all.x=T)
#twitter_attributes <- merge(twitter_attributes,representatives, by.x="icpsr.id", by.y="rep_ICPSRcode", all.x=T)
twitter_attributes <- twitter_attributes[!duplicated(twitter_attributes$uid),]
twitter_attributes$rep_district[twitter_attributes$uid=="repsandylevin"] <- 12
twitter_attributes$rep_district[twitter_attributes$title=="Senate"] <- NA
rm(representatives)

#add zip codes
load("Data/zip.rdata")
zip <- zip[!duplicated(zip$V1),]
twitter_attributes <- merge(twitter_attributes,zip, by.x="uzip", by.y="V1", all.x=T)

#date
twitter_attributes$created_at <- sub("\\d\\d:\\d\\d:\\d\\d\\s\\+\\d\\d\\d\\d\\s","",twitter_attributes$created_at)
twitter_attributes$created_at <- as.Date(twitter_attributes$created_at, "%a %b %d %Y")
twitter_attributes$created_at <- difftime(as.Date("2012-11-06"), twitter_attributes$created_at, units = c("days"))

#region
twitter_attributes$state[twitter_attributes$title=="User"] <- state.abb[match(twitter_attributes$state[twitter_attributes$title=="User"],state.name)]
twitter_attributes$region <- state.region[match(twitter_attributes$state,state.abb)]

#district
twitter_attributes$V3 <- F
twitter_attributes$V3[grepl("\\d\\,\\d", twitter_attributes$V2)] <- T
twitter_attributes$V2[twitter_attributes$V3==T] <- NA

twitter_attributes$st_distr <- NA
twitter_attributes$st_distr <- paste(twitter_attributes$state,twitter_attributes$V2,sep = "-")
twitter_attributes$st_distr[twitter_attributes$V3==T] <- NA

twitter_attributes$st_distr[twitter_attributes$title!="User"] <- paste(twitter_attributes$state[twitter_attributes$title!="User"],twitter_attributes$rep_district[twitter_attributes$title!="User"],sep = "-")
twitter_attributes$st_distr[twitter_attributes$title=="Senate"] <- NA
twitter_attributes$V3[is.na(twitter_attributes$st_distr)==T] <- T

#contributions
twitter_attributes$contr_dem <- 0
twitter_attributes$contr_rep <- 0

twitter_attributes$contr_dem[twitter_attributes$amount_dem!=0] <- 1
twitter_attributes$contr_rep[twitter_attributes$amount_rep!=0] <- 1

#re-sort attributes to be in line with vertice names
twitter_attributes$users <- 0
twitter_attributes$users[twitter_attributes$title=="User"] <- 1
twitter_attributes_users <- subset(twitter_attributes, users==1)
twitter_attributes_elites <- subset(twitter_attributes, users==0)
twitter_attributes_users <- twitter_attributes_users[order(twitter_attributes_users$uid),]
twitter_attributes_elites <- twitter_attributes_elites[order(twitter_attributes_elites$uid),]
twitter_attributes <- rbind(twitter_attributes_users,twitter_attributes_elites)

#party2 (including "user" category)
twitter_attributes$party2 <- twitter_attributes$party
twitter_attributes$party2[is.na(twitter_attributes$party2)==T] <- "User"

#chamber leadership
twitter_attributes$leadership <- F
twitter_attributes$leadership[twitter_attributes$uid=="NancyPelosi"] <- T
twitter_attributes$leadership[twitter_attributes$uid=="WhipHoyer"] <- T
twitter_attributes$leadership[twitter_attributes$uid=="SpeakerBoehner"] <- T
twitter_attributes$leadership[twitter_attributes$uid=="GOPLeader"] <- T #Eric Cantor
twitter_attributes$leadership[twitter_attributes$uid=="GOPWhip"] <- T #Kevin McCarthy

twitter_attributes$leadership[twitter_attributes$uid=="SenJonKyl"] <- T #Senate GOP whip
twitter_attributes$leadership[twitter_attributes$uid=="SenatorDurbin"] <- T #Senate Dem whip
twitter_attributes$leadership[twitter_attributes$uid=="SenatorReid"] <- T
#Mitch McConnel doesn't have a Twitter account

#some values for gender are missing, fill them in
twitter_attributes$gender[is.na(twitter_attributes$gender)==T & twitter_attributes$title!="User"] <- "M"
twitter_attributes$gender[twitter_attributes$uid=="RepMaryFallin"] <- "F"
twitter_attributes$gender[twitter_attributes$uid=="stabenow"] <- "F"

#log donations
twitter_attributes$amount <- log(twitter_attributes$amount)

#twitter_attributes_users <- subset(twitter_attributes,party2=="User")
#save(twitter_attributes_users,file="twitter_attributes_users.rdata")


##network

#Set vertex attributes
set.vertex.attribute(twitter, names(twitter_attributes), twitter_attributes)

#Senate subgraph
twitter_senate <- get.inducedSubgraph(twitter,v=which(twitter%v%'title'!="House"))

#House subgraph
twitter_house <- get.inducedSubgraph(twitter,v=which(twitter%v%'title'!="Senate"))

#restricted House subgraph
twitter_house_r <- get.inducedSubgraph(twitter,v=which(twitter%v%'V3'!="TRUE"))


#save the network objects
save(twitter_house, file="Network Objects/twitter_house.rdata")
save(twitter_house_r, file="Network Objects/twitter_house_r.rdata")
save(twitter_senate, file="Network Objects/twitter_senate.rdata")
