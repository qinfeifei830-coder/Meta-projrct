library(gemtc)
library(rjags)
library(dmetar)
data(TherapyFormatsGeMTC)

head(TherapyFormatsGeMTC$data)
TherapyFormatsGeMTC$treat.codes
network <- mtc.network(data.re  = TherapyFormatsGeMTC$data,
                       treatments = TherapyFormatsGeMTC$treat.codes)
summary(network)
plot(network, 
     use.description = TRUE) # Use full treatment names
library(igraph)
set.seed(12345) # set seed for reproducibility

plot(network, 
     use.description = TRUE,            # Use full treatment names
     vertex.color = "white",            # node color
     vertex.label.color = "gray10",     # treatment label color
     vertex.shape = "sphere",           # shape of the node
     vertex.label.family = "Helvetica", # label font
     vertex.size = 20,                  # size of the node
     vertex.label.dist = 2,             # distance label-node center
     vertex.label.cex = 1.5,            # node label size
     edge.curved = 0.2,                 # edge curvature
     layout = layout.fruchterman.reingold)
# We give our compiled model the name `model`.
model <- mtc.model(network,
                   likelihood = "normal",
                   link = "identity",
                   linearModel = "random",
                   n.chain = 4)
mcmc1 <- mtc.run(model, n.adapt = 50, n.iter = 1000, thin = 10)
mcmc2 <- mtc.run(model, n.adapt = 5000, n.iter = 1e5, thin = 10)
plot(mcmc1)
plot(mcmc2)

gelman.plot(mcmc1)
gelman.plot(mcmc2)
gelman.diag(mcmc1)$mpsrf
gelman.diag(mcmc2)$mpsrf


nodesplit <- mtc.nodesplit(network, 
                           linearModel = "random", 
                           likelihood = "normal",
                           link = "identity",
                           n.adapt = 500, 
                           n.iter = 1e3, 
                           thin = 10)
summary(nodesplit)

plot(summary(nodesplit)) 
rank <- rank.probability(mcmc2, preferredDirection = -1)
plot(rank, beside=TRUE)

forest(relative.effect(mcmc2, t1 = "cau"), 
       use.description = TRUE, # Use long treatment names
       xlim = c(-1.5, 0.5))


library(ggplot2)
library(dmetar)
rank.probability <- rank.probability(mcmc2)
sucra <- dmetar::sucra(rank.probability, lower.is.better = TRUE)

sucra
rank <- rank.probability(mcmc2, preferredDirection = -1)
plot(rank, beside=TRUE)

forest(relative.effect(mcmc2, t1 = "cau"), 
       use.description = TRUE, # Use long treatment names
       xlim = c(-1.5, 0.5))
library(dmetar)
rank.probability <- rank.probability(mcmc2)
sucra <- dmetar::sucra(rank.probability,lower.is.better = TRUE)

sucra
plot(sucra)
results <- relative.effect.table(mcmc2)
save(results, file = "results.csv")

forest(relative.effect(mcmc2, t1 = "cau"), 
       use.description = TRUE, # Use long treatment names
       xlim = c(-1.5, 0.5))

library(dmetar)
rank.probability <- rank.probability(mcmc2)
sucra <- dmetar::sucra(rank.probability, lower.is.better = TRUE)

sucra
plot(sucra)
results <- relative.effect.table(mcmc2)
save(results, file = "results.csv")

TherapyFormatsGeMTC$study.info
network.mr <- mtc.network(data.re = TherapyFormatsGeMTC$data,
                          studies = TherapyFormatsGeMTC$study.info,
                          treatments = TherapyFormatsGeMTC$treat.codes)

regressor <- list(coefficient = "shared",
                  variable = "rob",
                  control = "cau")

model.mr <- mtc.model(network.mr,
                      likelihood = "normal",
                      link = "identity",
                      type = "regression",
                      regressor = regressor)

mcmc3 <- mtc.run(model.mr,
                 n.adapt = 5000,
                 n.iter = 1e5,
                 thin = 10)
summary(mcmc3)
forest(relative.effect(mcmc3, t1 = "cau", covariate = 1),
       use.description = TRUE, xlim = c(-1.5, 1))
title("High Risk of Bias")




