library(metafor)
# Load data set from 'dmetar'
library(dmetar)
library(ggplot2)
data("Chernobyl")
head(Chernobyl)
full.model <- rma.mv(yi = z, 
                     V = var.z, 
                     slab = author,
                     data = Chernobyl,
                     random = ~ 1 | author/es.id, 
                     test = "t", 
                     method = "REML")
summary(full.model)
library(esc)
convert_z2r(0.52)
i2 <- var.comp(full.model)
summary(i2)
plot(i2)
l3.removed <- rma.mv(yi = z, 
                     V = var.z, 
                     slab = author,
                     data = Chernobyl,
                     random = ~ 1 | author/es.id, 
                     test = "t", 
                     method = "REML",
                     sigma2 =  c(0, NA))

summary(l3.removed)


mod.model <- rma.mv(yi = z, V = var.z, 
                    slab = author, data = Chernobyl,
                    random = ~ 1 | author/es.id, 
                    test = "t", method = "REML",
                    mods = ~ radiation)

summary(mod.model)

library(clubSandwich)
# constant sampling correlation assumption
rho <- 0.6
# constant sampling correlation working model
V <- with(Chernobyl, 
          impute_covariance_matrix(vi = var.z,
                                   cluster = author,
                                   r = rho))
che.model <- rma.mv(z ~ 1 + radiation,
                    V = V,
                    random = ~ 1 | author/es.id,
                    data = Chernobyl,
                    sparse = TRUE)
conf_int(che.model, 
         vcov = "CR2")
coef_test(che.model, 
          vcov = "CR2")

library(clubSandwich)
# constant sampling correlation assumption
rho <- 0.6
# constant sampling correlation working model
V <- with(Chernobyl, 
          impute_covariance_matrix(vi = var.z,
                                   cluster = author,
                                   r = rho))
che.model <- rma.mv(z ~ 1 + radiation,
                    V = V,
                    random = ~ 1 | author/es.id,
                    data = Chernobyl,
                    sparse = TRUE)
conf_int(che.model, 
         vcov = "CR2")
coef_test(che.model, 
          vcov = "CR2")
install.packages("robumeta")

# Make sure {wildmeta} and {tidyverse} is loaded
library(wildmeta)
library(tidyverse)

# Add year as extra variable
Chernobyl$year <- str_extract(Chernobyl$author, 
                              "[0-9]{4}") %>% as.numeric()
# Make sure {wildmeta} and {tidyverse} is loaded
library(wildmeta)
library(tidyverse)

# Add year as extra variable
Chernobyl$year <- str_extract(Chernobyl$author, 
                              "[0-9]{4}") %>% as.numeric()
che.model.bs <- rma.mv(z ~ 0 + radiation + scale(year),
                       V = V,
                       random = ~ 1 | author/es.id,
                       data = Chernobyl,
                       sparse = TRUE)
rad.constraints <- constrain_equal(constraints = 1:3,
                                   coefs = coef(che.model.bs))
rad.constraints
cw.boot <- Wald_test_cwb(full_model = che.model.bs,
                         constraints = rad.constraints,
                         adjust = "CR2",
                         R = 2000)
cw.boot
plot(cw.boot, 
     fill = "lightblue", 
     alpha = 0.5)

