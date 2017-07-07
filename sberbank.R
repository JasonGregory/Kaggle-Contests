# Set options and load libraries

options(max.print=999999)
options(scipen=999)
# library(dataFun)
library(vtreat)
library(tidyverse)
library(forcats)

# Pull in data
iData <- read_csv("train.csv")
iData %>%
  select(price_doc, ends_with("all")) %>%
  gather(category, population,c(`0_6_male`:`0_13_female`,male_f:ekder_female)) %>%
  count(category)

iData %>%
  select(full_all:`0_13_female`)



select(age_cat, population)



select(`0_6_male`) %>%
  separate(`0_6_male`, c("age_cat", "Sex"))

select(`0_6_male`:`0_13_female`) %>%
  separate(`0_6_male`:`0_13_female`, c("age_cat", "Sex"), sep = 5)

gather(`0_6_male`:``)


select(full_all, male_f, female_f, young_all, young_male, young_female, work_all, work_male) %>%
  gather("Category", "Population", c("male_f","female_f"))

spec(iData)


#Pulling in data
train_data <- read.csv("train.csv")
#macro_data <- read.csv("macro.csv")

#head(train_data)
#prepIt(train_data)

doSplit <- function(dta, split){
  set.seed(123)
  trn <- runif(nrow(dta)) < split
  train <<- dta[trn==TRUE,]
  test <<- dta[trn==FALSE,]
}

###doing vtreat method
doSplit(train_data,.75)
outcome <- "price_doc"
vars = setdiff(colnames(train), c(p.unique(train),outcome))
#method 1
treatplan1 = designTreatmentsN(train,vars,outcome)
sframe1 <- treatplan1$scoreFrame
sframe1$sig <- round(sframe1$sig,4)
aVars <- sframe1$varName[!(sframe1$code %in% c("catP", "catD"))]
sframe1[sframe1$varName %in% aVars,
        c("origName","varName", "sig", "code")] %>%
  dplyr::filter(sig>.001) %>%
  arrange(sig)

train1.treat = prepare(treatplan1, train)
fmla1 = paste(outcome, " ~ ", paste(aVars, collapse=" + "))
model2 = lm(fmla1, data=train1.treat)
summary(model2)

#method 2
treatplan2 = mkCrossFrameNExperiment(train,vars,outcome)
sframe2 <- treatplan2$treatments$scoreFrame
sframe2$sig <- round(sframe2$sig,4)
aVars2 <- sframe2$varName[!(sframe1$code %in% c("catP", "catD"))]
sframe2[sframe2$varName %in% aVars2,
        c("origName","varName", "sig", "code")] %>%
  dplyr::filter(sig>.001) %>%
  arrange(sig)

train2.treat = prepare(treatplan2, train)
fmla1 = paste(outcome, " ~ ", paste(aVars, collapse=" + "))
model2 = lm(fmla1, data=train1.treat)
summary(model2)

#sframe1$varName[sframe1$sig<1/nrow(sframe1)] 


#trying out the vtreat method
outcome <- "price_doc"
vars = setdiff(colnames(train_data), c(p.unique(train_data),outcome))
treatmentsN = designTreatmentsN(train_data,vars,outcome)
treatplan <- treatmentsN
sframe <- treatplan$scoreFrame
sframe$varName[sframe$sig<1/nrow(sframe)] #limiting to 1/n of the variables basic way to do it.
vars <- sframe$varName[!(sframe$code %in% c("catP", "catD"))]
plotTest <- sframe[sframe$varName %in% vars,
                   c("origName","varName", "sig", "extraModelDegrees")]
plotTest <- sframe[sframe$varName %in% vars,
                   c("origName","varName", "sig", "code")]

plotTest$sig  <- round(plotTest$sig,4)
1/length(colnames(train_data))

plotTest2 %>% 
  dplyr::filter(sig>.1) %>%
  arrange(sig)


#iteration 2
treatmentsN2 = mkCrossFrameNExperiment(train_data,vars,outcome)
sframe2 <- treatmentsN2$treatments$scoreFrame
sframe2$varName[sframe2$sig<1/nrow(sframe)]
vars <- sframe2$varName[!(sframe2$code %in% c("catP", "catD"))]
plotTest2 <- sframe2[sframe2$varName %in% vars,
                     c("origName","varName", "sig", "extraModelDegrees")]
plotTest2$sig  <- round(plotTest2$sig,4)
plotTest2 %>% 
  arrange(sig)


str(plotTest)

plotTest$sig


# Build a map from vtreat names back to reasonable display names
vmap <-as.list(treatmentsN$scoreFrame$origName)
names(vmap) <-treatmentsN$scoreFrame$varName
print(vmap['timestamp_catP'])



train.treat = prepare(treatmentsN, train_data) #set pruneLevel=Null to include all variables

fmla = paste(outcome, " ~ ", paste(vars, collapse=" + "))
model2 = lm(fmla, data=train.treat)

anova(model2)
?anova
?vtreat
?xgboost


?lm

head(treatmentsN)

print(treatmentsN)

?designTreatmentsN

p.factor(train_data)
p.unique(train_data)

############Other miscalaneous stuff

#gain chart plot & split stuff
doSplit <- function(dta, split){
  #  set.seed(652)
  trn <- runif(nrow(dta)) < split
  train <<- dta[trn==TRUE,]
  test <<- dta[trn==FALSE,]
}

doGain <- function(scores){
  perc <- seq(0, 1, .05)
  plot(perc, perc, type="n"
       , xlab="cum % autos"
       , ylab="cum % pure premium")
  abline(h=perc, col='lightgrey')
  abline(v=perc, col='lightgrey')
  abline(0,1, col="navy", lwd=3)
  titl("Gains Charts")
  subt("Pure Premium Prediction")
  
  J <- length(scores)
  col <- rainbow(J)
  for (i in 1:J){
    yhat <- scores[i]
    breaks <- quantile(test[,yhat], p=seq(0,1,.05))
    quantile <- cut(test[,yhat], breaks, include.lowest=TRUE)
    gain <- tapply(test$PP, quantile, sum)
    gain <- gain[20:1]
    gain <- cumsum(gain) / sum(gain)
    gain <- c(0, gain)
    lines(perc, gain, type="o", lwd=3, col=col[i])
  }
  legend("bottomright", scores, col=col, lwd=3, bg="white", inset=.01)
}

par(mfrow=c(1,1))


doSplit(dat2, .5)

m1 <- glm(PP~factor(Kilometres)+factor(Zone),data=train,family=quasipoisson(link="log"),weights=Exposure) 
m2 <- glm(PP_pos~factor(Kilometres)+factor(Zone),data=train,family=Gamma(link="log"),weights=Exposure) 
m3 <- glm(PP~factor(Kilometres)+factor(Zone),data=train,family=gaussian(link="identity"),weights=Exposure,start=c(m1$coefficients))
m4 <- glm(PP_pos~factor(Kilometres)+factor(Zone),data=train,family=inverse.gaussian(link="identity"),weights=Exposure,start=c(t2glm1$coefficients))
m5 <- glm(PP_pos~factor(Kilometres)+factor(Zone),data=train,family=tweedie(var.power=2.1, link.power=0),weights=Exposure,start=c(t2glm1$coefficients))


test$yhat.m1 <- predict(m1, test, type="response")
test$yhat.m2 <- predict(m2, test, type="response")
test$yhat.m3 <- predict(m3, test, type="response")
test$yhat.m4 <- predict(m4, test, type="response")
test$yhat.m5 <- predict(m5, test, type="response")

#yhats <- c("yhat.m1", "yhat.m2", "yhat.m3", "yhat.m4", "yhat.m5")
yhats <- c("yhat.m1", "yhat.m2", "yhat.m5")

doGain(yhats)

#Look into average absolute error (need to add in k-fold rather than simple split)
head(test)
mean(abs(test$PP-test$yhat.m1))
mean(abs(test$PP-test$yhat.m2))
mean(abs(test$PP-test$yhat.m3))
mean(abs(test$PP-test$yhat.m4))
mean(abs(test$PP-test$yhat.m5))


##############K-fold 
nFolds <- 5
folds <- rep_len(1:nFolds, nrow(dat4))
folds <- sample(folds)
m1error <- NULL
m2error <- NULL

for(k in 1:nFolds) {
  fold <- which(folds == k)
  train <- dat4[-fold,]
  test <- dat4[fold,]
  
  t3m2 <- glm(PP~factor(Kilometres)+factor(Zone)+factor(Bonus)*factor(Make)+factor(Bonus)*factor(Kilometres),data=train,family=quasipoisson(link="log"),weights=Exposure)
  t3m1 <- glm(PP_pos~factor(Kilometres)+factor(Zone)+factor(Bonus)*factor(Make)+factor(Bonus)*factor(Kilometres),data=train,family=Gamma(link="log"),weights=Exposure,start=c(t3m2$coefficients))
  
  test$yhat.m1 <- predict(t3m1, test, type="response")
  test$yhat.m2 <- predict(t3m2, test, type="response")
  
  m1error <- c(m1error, mean(abs(test$PP-test$yhat.m1)))
  m2error <- c(m2error, mean(abs(test$PP-test$yhat.m2)))
}

mean(m1error)
mean(m2error)


#######GLM Items
dat2 <- data.frame("PP" = dat$PP,
                   "Kilometres" = dat$Kilometres,
                   "Zone" = dat$Zone,
                   "Exposure" = dat$Insured
)
dat2$PP_pos <- dat2$PP
dat2$PP_pos[dat2$PP_pos==0] <- .001

t2glm1 <- glm(PP~factor(Kilometres)+factor(Zone),data=dat2,family=quasipoisson(link="log"),weights=Exposure) 
t2glm1_dev <- residuals(t2glm1, type=c("deviance"))
t2glm1_pred <- predict(t2glm1)


plot(t2glm1_pred, t2glm1_dev, xlab=c("predicted value"), ylab=("deviance residual"), main=c("Plot of deviance residuals vs. predicted values"))
qqnorm(t2glm1_dev)


###################bootstrap model errors
iterations <- 200

corr1 = rep(NA,iterations)
corr2 = rep(NA,iterations)
corr3 = rep(NA,iterations)
corr4 = rep(NA,iterations)
coef1 = rep(NA,iterations)
coef2 = rep(NA,iterations)
coef3 = rep(NA,iterations)
coef4 = rep(NA,iterations)

for(k in 1:iterations) {
  
  bagg <- sample(1:nrow(data.prf), replace = TRUE)
  training <- data.prf[bagg,]
  testing <- data.prf[-bagg,]
  
  train.sparse <- sparse.model.matrix(PRF_CNT~., data=training)
  test.sparse <- sparse.model.matrix(PRF_CNT~., data=testing)
  
  #Standard model with no feature selection    
  #correlation
  lm1.fit <- lm(PRF_CNT ~ ., data = training)
  corr1[k] = sqrt((sum((testing$PRF_CNT-predict(lm1.fit,testing, type="response"))^2))/nrow(testing))
  
  #coefficient
  lm1.coef <- tidy(lm1.fit)  
  coef1[k] = paste(lm1.coef$term[grep("Procedure", lm1.coef$term)], collapse = ", ")
  coef1[k] = gsub("Procedure", "", coef1[k])
  
  #Step-wise model    
  #correlation
  lm2.fit <- lm(PRF_CNT ~ ., data = training)
  lm2.fit <- step(lm2.fit)
  corr2[k] = sqrt((sum((testing$PRF_CNT-predict(lm2.fit,testing, type="response"))^2))/nrow(testing))
  
  #coefficient
  lm2.coef <- tidy(lm2.fit)  
  coef2[k] = paste(lm2.coef$term[grep("Procedure", lm2.coef$term)], collapse = ", ")
  coef2[k] = gsub("Procedure", "", coef2[k])
  
  #LASSO Model
  alpha = 1
  #correlation    
  lm3.fit <- glmnet(train.sparse, training$PRF_CNT, alpha=alpha) #model the data
  cv <- cv.glmnet(train.sparse, training$PRF_CNT)
  corr3[k] = sqrt((sum((testing$PRF_CNT-predict(lm3.fit, newx = test.sparse, type = "response", s=cv$lambda.min))^2))/nrow(testing))
  
  #coefficient
  lm3.coef <- tidy(predict(lm3.fit, newx = test.sparse, type = "coefficient", s=cv$lambda.min))
  coef3[k] = paste(lm3.coef$row[grep("Procedure", lm3.coef$row)], collapse = ", ")
  coef3[k] = gsub("Procedure", "", coef3[k])  
  
  #Ridge Model
  alpha = 0
  #correlation    
  lm4.fit <- glmnet(train.sparse, training$PRF_CNT, alpha=alpha) #model the data
  cv <- cv.glmnet(train.sparse, training$PRF_CNT)
  corr4[k] = sqrt((sum((testing$PRF_CNT-predict(lm4.fit, newx = test.sparse, type = "response", s=cv$lambda.min))^2))/nrow(testing))
  
  #coefficient
  lm4.coef <- tidy(predict(lm4.fit, newx = test.sparse, type = "coefficient", s=cv$lambda.min))
  coef4[k] = paste(lm4.coef$row[grep("Procedure", lm4.coef$row)], collapse = ", ")
  coef4[k] = gsub("Procedure", "", coef4[k])    
  
}

corr.tbl <- data.frame("corr1"= corr1, "corr2"= corr2, "corr3"=corr3, "corr4"=corr4)
coef.tbl <- data.frame("coef1"=(str_count(coef1, ",")+1), "coef2"=(str_count(coef2, ",")+1), "coef3"=(str_count(coef3, ",")+1), "coef4"=(str_count(coef4, ",")+1))


if(exists("errors")) {
  errors <- rbind(errors,corr.tbl)  
  coefficients <- rbind(coefficients,coef.tbl)
} else {
  errors = corr.tbl
  coefficients = coef.tbl
}

#plot graphs
require(ggplot2)

#Normal Regression
ggplot(data=errors, aes(errors$corr1)) +
  geom_histogram(#aes(y = ..ncount..),
    #             breaks=seq(0,1000000, by = 1000),
    binwidth=1000,
    col="grey" #,
    #    fill="grey"
  ) +
  #  geom_density(aes(y=..scaled..), col="grey") +  
  xlim(c(5000,40000)) +
  labs(x="Mean Squared Error", y="Count")

#######Other stuff 

#Merge Data
training <- merge(x = train_data, y = macro_data, by = "timestamp", all.x = TRUE)
#Output Data for Future Reference
write.csv(training,"train_merged.csv")

prepIt(train_data, output_file = "DataPrep.html")
prepIt(macro_data, output_file = "MacroDataPrep.html")
prepIt(training, output_file = "FullPrep.html")
