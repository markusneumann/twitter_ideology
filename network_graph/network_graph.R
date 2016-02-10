setwd("E:/Data/Barbera_Replication/output")

library(network)
library(sna)
library(igraph)
library(plotrix)

load("adj-matrix-US.rdata") #it may be called adjacency matrix, but it's actually an incidence matrix

set.seed(123)
m1 <- as.matrix(y)
m1 <- m1[,order(colSums(m1),decreasing = T)] #sort by follower count
m1 <- m1[,1:15] #keep only the top 15 political actors
m1 <- m1[rowSums(m1)!=0,] #remove users who don't follow these 15

m2 <- m1[sample(1:nrow(m1),750),] #randomly sample 750 users
g <- graph_from_incidence_matrix(m2) #create igraph object from incidence matrix

twitterLayout = layout_with_fr(g, dim = 2, niter = 5000, weights = E(g)$weight)

V(g)$label <- 1:765
V(g)$label[751:765] <- V(g)$name[751:765] #label users by their number

load("users-data-US.rdata") #contains ideology scores of users
users$colors <- color.scale(users$theta,c(0,1,1),0,c(1,1,0)) #color according to ideology (red conservative, blue liberal, orange VIPs)
users <- subset(users, select=c(uid,colors))

colorlist <- as.data.frame(V(g)$name) #create a data frame for colors, to be jused later
names(colorlist) <- "users"
colorlist <- merge(colorlist,users,by.x="users",by.y="uid",sort=F,all.x=T)
colorlist$colors[751:765] <- "orange"

plot(g,
  layout = twitterLayout,
  vertex.size = ifelse(degree(g) > 100, 40, 20),# vertices with degree>100 are larger
  rescale = F,
  xlim = range(twitterLayout[, 1]),
  ylim = range(twitterLayout[, 2]),
  vertex.label = ifelse(degree(g) > 100, V(g)$label, NA),
  vertex.label.dist = 2,
  vertex.label.color = "black",	
  vertex.label.cex = .5,
  vertex.color = colorlist$colors #color vertices according to colorlist defined above
)
