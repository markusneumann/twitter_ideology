library(ggplot2)
library(network)
library(sna)
library(ergm)
library(stargazer)

#setwd("/home/markus/onedrive/2_Desmarais/Paper/")
setwd("C:/Users/Markus/OneDrive/2_Desmarais/Paper/")

#load 2506*230 matrix
load("twitter_users_restricted.rdata")


#Step 1: projection of the observed bipartite network
tw <- twitter_users%*%t(twitter_users)

#Step 2: generating a random bipartite network using the SDSM
g <- network(twitter_users, directed = F, bipartite = T, matrix.type = "incidence")

#number of agents and artifacts
l.mode1 <- dim(twitter_users)[1]
l.mode2 <- dim(twitter_users)[2]

#dependent variable
y <- as.vector(twitter_users)

#X1
X1 <- rep(degree(g,gmode="graph")[1:l.mode1],l.mode2)

#X2
X2 <- rep(degree(g,gmode="graph")[(l.mode1+1):(l.mode1+l.mode2)],each=l.mode1)

#X3
X3 <- X1*X2


#bivariate response model
m.logit <- glm(y~X1+X2+X3,family=binomial(link="logit"))

#rename fitted values
probs <- m.logit$fitted.values

#make a vector to store the backbone edgelist in
result <- as.vector(tw)
result[] <- 0

#WARNING: The next step can take several hours
#run 10000 rounds of bernoulli trials...
for(i in 1:10000){
  probs.b <- probs
  #...where one round corresponds to about 600000 trials
  for(i in 1:length(probs)){
    probs.b[i] <- rbinom(1,1,probs[i])
  }
  
  #put the results back into matrix form
  probs.b.m <- matrix(probs.b, nrow = l.mode1, ncol = l.mode2)
  
  #Step 3: project the random bipartite network
  ran_proj <- probs.b.m%*%t(probs.b.m)
  
  #back to vector form
  ran_proj_v <- as.vector(ran_proj)
  tw_v <- as.vector(tw)
  
  #the results vector saves every instance in which the simulated projection edge has a 
  #higher weight than the corresponding edge from the projection of the actual network
  result[ran_proj_v>tw_v] <- result[ran_proj_v>tw_v]+1
  
}
#save(result,file="result10000sims.rdata") #save the results, if desired
#load("result10000sims.rdata") #... or re-load them

#create backbone edgelist
result_v <- result
result_v[] <- 0
result_v[result<.5] <- 1 #0.5 = 99.99% significance level for 10000 sims

#convert edgelist back to matrix
result_m <- matrix(result_v, nrow = l.mode1, ncol = l.mode1)

#re-introduce the vertex names
rownames(result_m) <- rownames(twitter_users)
colnames(result_m) <- rownames(twitter_users)

#convert backbone matrix to network
g_result <- network(result_m, vertex.attrnames = rownames(twitter_users), directed = F, bipartite = F, matrix.type = "adjacency")

#load attribute list
load("twitter_attributes_users.rdata")
twitter_attributes_users <- twitter_attributes_users[twitter_attributes_users$uid%in%rownames(twitter_users),]

#set edge attributes
set.vertex.attribute(g_result, names(twitter_attributes_users), twitter_attributes_users)

#apply color to nodes
library(colorRamps)
grad <- blue2red(nrow(twitter_attributes_users))
vcol <- grad[rank(twitter_attributes_users$ideology)]

#plot backbone network
gplot(g_result, gmode="graph", 
      displaylabels=F, 
      displayisolates = F,
      vertex.lty=0,
      vertex.col=vcol,
      edge.lwd=0.25)

#export as pdf manually, height=9, width=12
#or as png, 1200*900


#ERGM
backbone.fit <- ergm(g_result ~ 
                       edges + 
                       absdiff("ideology") +
                       nodematch("state") + 
                       nodematch("region") +
                       nodefactor("region") +
                       isolates +
                       gwesp(0,fixed=T),
                    control = control.ergm(main.method="Stochastic-Approximation")) #MCMCMLE doesn't work
#save(backbone.fit,file="backbone.fit.rdata") #save the results, if desired

summary(backbone.fit)
mcmc.diagnostics(backbone.fit)
par(mfrow=c(3,1)); plot(gof(backbone.fit)) #GOF plot
stargazer(backbone.fit)
