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
load("Network Objects/twitter_house.rdata")
load("Network Objects/twitter_house_r.rdata")
load("Network Objects/twitter_senate.rdata")

#fit ergm -- Senate
senate.fit <- ergm(twitter_senate ~ 
                     edges + 
                     absdiff("ideology") + 
                     isolates + 
                     b1concurrent +
                     b1cov("amount") +
                     nodematch("state") + 
                     nodematch("region") +
                     nodefactor("region") +
                     b2factor("gender") +
                     b2factor("party") +
                     b2factor("leadership") +
                     b1cov("extremism") +
                     b2cov("created_at"), 
                   control = control.ergm(MCMC.samplesize=4096, 
                                          MCMC.interval=2048))
summary(senate.fit)
par(mfrow=c(3,1)); plot(gof(senate.fit))
mcmc.diagnostics(senate.fit, vars.per.page=5)

#fit ergm -- House
house.fit <- ergm(twitter_house ~ 
                    edges + 
                    absdiff("ideology") + 
                    isolates + 
                    b1concurrent +
                    b1cov("amount") +
                    nodematch("state") + 
                    nodematch("region") +
                    nodefactor("region") +
                    b2factor("gender") +
                    b2factor("party") +
                    b2factor("leadership") +
                    b1cov("extremism") +
                    b2cov("created_at"), 
                  control = control.ergm(MCMC.samplesize=4096, 
                                         MCMC.interval=2048))
summary(house.fit)
par(mfrow=c(3,1)); plot(gof(house.fit))
mcmc.diagnostics(house.fit, vars.per.page=5)

#fit ergm -- House restricted
house_r.fit <- ergm(twitter_house_r ~ 
                    edges + 
                    absdiff("ideology") + 
                    nodematch("st_distr") + 
                    isolates + 
                    b1concurrent +
                    b1cov("amount") +
                    nodematch("state") + 
                    nodematch("region") +
                    nodefactor("region") +
                    b2factor("gender") +
                    b2factor("party") +
                    b2factor("leadership") +
                    b1cov("extremism") +
                    b2cov("created_at"), 
                    control = control.ergm(MCMC.samplesize=4096, 
                                           MCMC.interval=2048))
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
                               "Isolates", "b1concurrent","nodecov log Donations", "nodematch State",
                               "nodematch Region", "nodefactor South", "nodefactor North Central",
                               "nodefactor West", "b2factor Gender (Male)",
                               "b2factor Party (Ind.)", "b2factor Party (Rep.)", "b2factor Leadership",
                               "nodecov Extremism", "b2cov Account Duration"),
          omit.stat = c("AIC","BIC"))
