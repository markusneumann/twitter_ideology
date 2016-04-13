library(network)
library(ergm)
library(sna)
library(latticeExtra)
library(plyr)

#Set seed
set.seed(1)
  
#Set working directory
setwd("C:/Users/Markus/OneDrive/2_Desmarais/Paper")

#Load data
load("twitter_environment.RData")

####

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
#network.vertex.names(twitter)


#add/change attributes

#ideology
twitter_attributes$ideology <- NA
twitter_attributes$ideology[is.na(twitter_attributes$dw.nom.1)==F] <- twitter_attributes$dw.nom.1[is.na(twitter_attributes$dw.nom.1)==F]*50+50
twitter_attributes$ideology[is.na(twitter_attributes$cfscore)==F] <- twitter_attributes$cfscore[is.na(twitter_attributes$cfscore)==F]*10+50

#extremism
twitter_attributes$extremism <- abs(twitter_attributes$ideology-50)

#set user ttle in addition to house and senate
twitter_attributes$title[is.na(twitter_attributes$title)==T] <- "User"

#add representative district
representatives <- read.csv("representatives.csv", header = F)
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
load("Zip_Codes/zip.rdata")
zip <- zip[!duplicated(zip$V1),]
twitter_attributes <- merge(twitter_attributes,zip, by.x="uzip", by.y="V1", all.x=T)

#date
twitter_attributes$created_at <- sub("\\d\\d:\\d\\d:\\d\\d\\s\\+\\d\\d\\d\\d\\s","",twitter_attributes$created_at)
twitter_attributes$created_at <- as.Date(twitter_attributes$created_at, "%a %b %d %Y")
twitter_attributes$created_at <- difftime(as.Date("2012-11-30"), twitter_attributes$created_at, units = c("days"))

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

##network

#Vertex attributes
#twitter_attributes <- twitter_attributes[]
set.vertex.attribute(twitter, names(twitter_attributes), twitter_attributes)

#Senate subgraph
twitter_senate <- get.inducedSubgraph(twitter,v=which(twitter%v%'title'!="House"))
length(which(has.edges(twitter_senate)==F)) #number of isolates
network.vertex.names(twitter_senate)[5]
get.vertex.attribute(twitter_senate, "uid")[5]

#House subgraph
twitter_house <- get.inducedSubgraph(twitter,v=which(twitter%v%'title'!="Senate"))
length(which(has.edges(twitter_house)==F)) #number of isolates
get.vertex.attribute(twitter_house, "uid")[5]

#restricted House subgraph
twitter_house_r <- get.inducedSubgraph(twitter,v=which(twitter%v%'V3'!="TRUE"))
length(which(has.edges(twitter_house_r)==F)) #number of isolates
network.vertex.names(twitter_house_r)[5]
get.vertex.attribute(twitter_house_r, "uid")[5]
get.vertex.attribute(twitter_house_r, "st_distr")[5]


#Plot the network -- not actually terribly useful, I'll revise these later
gplot(twitter_senate, gmode="graph", 
      displaylabels=F,
      displayisolates = F,
      label.bg="gray90", 
      label.cex=0.75, 
      label.pos=5,
      label.col=rgb(64,64,64, maxColorValue=255),
      vertex.col=c("blue","red")[1+(get.vertex.attribute(twitter_senate, "party")=="R")],
      vertex.lty=0,
      edge.col=rgb(204,204,204, maxColorValue=255),
      edge.lwd=0.25)

gplot(twitter_house, gmode="graph", 
      displaylabels=F,
      displayisolates = F,
      label.bg="gray90", 
      label.cex=0.75, 
      label.pos=5,
      label.col=rgb(64,64,64, maxColorValue=255),
      vertex.col=c("blue","red")[1+(get.vertex.attribute(twitter_house, "party")=="R")],
      vertex.lty=0,
      edge.col=rgb(204,204,204, maxColorValue=255),
      edge.lwd=0.25)

gplot(twitter_house_r, gmode="graph", 
      displaylabels=F, 
      displayisolates = F,
      label.bg="gray90", 
      label.cex=0.75, 
      label.pos=5,
      label.col=rgb(64,64,64, maxColorValue=255),
      vertex.col=c("blue","red")[1+(get.vertex.attribute(twitter_house_r, "party")=="R")],
      vertex.lty=0,
      edge.col=rgb(204,204,204, maxColorValue=255),
      edge.lwd=0.25)

#fit ergm -- Senate
senate.fit <- ergm(twitter_senate ~ 
                     edges + 
                     absdiff("ideology") + 
                     isolates + 
                     b1cov("amount") +
                     nodematch("state") + 
                     nodematch("region") +
                     nodefactor("region") +
                     b2factor("gender") +
                     b2factor("party") +
                     b1cov("extremism") +
                     b2cov("created_at"), 
                   control = control.ergm(MCMC.samplesize=2048, 
                                          MCMC.interval=1024))
summary(senate.fit)
par(mfrow=c(3,1)); plot(gof(senate.fit))
mcmc.diagnostics(senate.fit, vars.per.page=5)

#fit ergm -- House
house.fit <- ergm(twitter_house ~ 
                    edges + 
                    absdiff("ideology") + 
                    isolates + 
                    b1cov("amount") +
                    nodematch("state") + 
                    nodematch("region") +
                    nodefactor("region") +
                    b2factor("gender") +
                    b2factor("party") +
                    b1cov("extremism") +
                    b2cov("created_at"), 
                  control = control.ergm(MCMC.samplesize=2048, 
                                         MCMC.interval=1024))
summary(house.fit)
par(mfrow=c(3,1)); plot(gof(house.fit))
mcmc.diagnostics(house.fit, vars.per.page=5)

#fit ergm -- House restricted
house_r.fit <- ergm(twitter_house_r ~ 
                    edges + 
                    absdiff("ideology") + 
                    nodematch("st_distr") + 
                    isolates + 
                    b1cov("amount") +
                    nodematch("state") + 
                    nodematch("region") +
                    nodefactor("region") +
                    b2factor("gender") +
                    b2factor("party") +
                    b1cov("extremism") +
                    b2cov("created_at"), 
                    control = control.ergm(MCMC.samplesize=2048, 
                                           MCMC.interval=1024))
summary(house_r.fit)
par(mfrow=c(3,1)); plot(gof(house_r.fit))
mcmc.diagnostics(house_r.fit, vars.per.page=5)

#create latex regression output
library(stargazer)
stargazer(house_r.fit,house.fit,senate.fit,
          dep.var.labels.include = F,
          dep.var.caption = "",
          column.labels = c("House","House (Full sample)","Senate"),
          model.numbers = F,
          covariate.labels = c("Edges", "absdiff Ideology", "nodematch District",
                               "Isolates", "nodecov Donations Total", "nodematch State",
                               "nodematch Region", "nodefactor South", "nodefactor North Central",
                               "nodefactor West", "b2factor Gender (Male)", "b2factor Gender (NA)",
                               "b2factor Party (Ind.)", "b2factor Party (Rep.)",
                               "nodecov Extremism", "b2cov Account Duration"),
          omit.stat = c("AIC","BIC"))
