## ----setup, include=FALSE------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----packages, warning = FALSE, message = FALSE--------------------
# load the packages for graphing and data wrangling
library(ggplot2)
library(PASWR2)
library(car) # install the car package first, if not already installed
library(dplyr) 
library(lattice)
library(boot)
library(MASS)


## ------------------------------------------------------------------
ggplot(data = VIT2005, aes(x = totalprice)) +  geom_density(fill = "green") +  theme_bw()

MD <- median(VIT2005$totalprice)
 
iqr <- IQR(VIT2005$totalprice)
 
c(MD, iqr)


## ------------------------------------------------------------------
# e.g. matrix can be produced with
scatterplotMatrix( ~ totalprice + area + age, data = VIT2005)

scatterplotMatrix( ~ totalprice + floor+ rooms, data = VIT2005)

scatterplotMatrix( ~ totalprice + toilets + garage, data = VIT2005)

scatterplotMatrix( ~ totalprice + elevator + storage, data = VIT2005)



## ------------------------------------------------------------------
NUM <- c("area", "age", "floor", "rooms", "toilets", "garage","elevator", "storage")
COR <- cor(VIT2005[, "totalprice"], VIT2005[, NUM])
COR


## ------------------------------------------------------------------
# use a model where totalprice is regressed on all other variables
model.be <- lm(totalprice ~ ., data = VIT2005)

# 
drop1(model.be, test = "F")


## ------------------------------------------------------------------
model.be <- update(model.be, .~. - age)
drop1(model.be, test = "F")




## ------------------------------------------------------------------
model.be <- update(model.be, .~. - floor)
drop1(model.be, test = "F")



## ------------------------------------------------------------------

model.be <- update(model.be, .~. - conservation)
drop1(model.be, test = "F")


## ------------------------------------------------------------------
model.be <- update(model.be, .~. - rooms)
drop1(model.be, test = "F")


## ------------------------------------------------------------------
model.be <- update(model.be, .~. - storage)
drop1(model.be, test = "F")


## ------------------------------------------------------------------
formula(model.be)

modelA <- lm(formula(model.be), data = VIT2005)



## ------------------------------------------------------------------
modelAg <- glm(formula(model.be), data = VIT2005)

# # Fro cv.glm - the default is to set K equal to the number of observations in data which gives the usual leave-one-out cross-validation.
# 
modelAg <- glm(formula(model.be), data = VIT2005)
library(boot)
cv.error <- cv.glm(data = VIT2005, glmfit = modelAg)
cv.error$delta[1]

set.seed(5)
cv.error5 <- cv.glm(data = VIT2005, glmfit = modelAg, K=5)
cv.error5$delta[1]


## ------------------------------------------------------------------
modelAg <- glm(formula(model.be), data = VIT2005)



set.seed(5) # use for replication purposes
cv.error5 <- cv.glm(data = VIT2005, glmfit = modelAg, K = 5)
CV5a <- cv.error5$delta[1]
CV5a



## ------------------------------------------------------------------

mgof <- function(model = model, data = DF, ...){
  R2a <- summary(model)$adj.r.squared
  R2 <- summary(model)$r.squared
  aic <- AIC(model)
  bic <- AIC(model, k = log(nrow(data)))
  se <- summary(model)$sigma
  form <- formula(model)
  ANS <- c(R2 = R2, R2.adj = R2a, AIC = aic, BIC = bic, SE = se)
  ANS
}

MGOF <- mgof(model = modelA, data = VIT2005)
MGOF


## ------------------------------------------------------------------
residualPlot(modelA, main = "Model A")


## ------------------------------------------------------------------
boxCox(modelA, lambda = seq(-0.5, 0.5, length = 200))


## ------------------------------------------------------------------
# transform the response variable using natural log transformation

VITNEW <- VIT2005 %>% mutate(logtotalprice = log(totalprice))

# build the fitted full model
model.be <- lm(logtotalprice ~ ., data = VITNEW[ ,-1]) # exclude the totalprice variable since it is no longer response variable
drop1(model.be, test = "F")



## ------------------------------------------------------------------
model.be <- update(model.be, .~. - age)
drop1(model.be, test = "F")


## ------------------------------------------------------------------
model.be <- update(model.be, .~. - conservation)
drop1(model.be, test = "F")


## ------------------------------------------------------------------
model.be <- update(model.be, .~. - floor)
drop1(model.be, test = "F")


## ------------------------------------------------------------------
model.be <- update(model.be, .~. - rooms)
drop1(model.be, test = "F")


## ------------------------------------------------------------------
model.be <- update(model.be, .~. - streetcategory)
drop1(model.be, test = "F")


## ------------------------------------------------------------------
formula(model.be)

modelE <- lm(formula(model.be), data = VITNEW[ ,-1])

