library(foreign)
profile2013 <- read.spss(file.choose(),to.data.frame = T)
core <- grepl('^c',names(profile2013), ignore.case = T)
core[1] <- T
profile2013_core <- profile2013[,core]

#recode population variable
library(car)
Ye <- NA
Ye$population <- recode(profile2013_core$c0population,
                        "NA = NA;                       
                        1:24999 = '<25000';
                        25000:49999 = '25000-49999';
                        50000:99999 = '50000-99999';
                        100000:499999 = '100000-499999';
                        else = '500000+'")
Ye[1] <- NULL

#recode governance type
Ye$governance_type <- profile2013_core$c0govcat

#recode budget cut; this step makes total non-missing to 1886
Ye$budget <- recode(profile2013_core$c10q301,"c('less than')='with budget cuts';c('greater than','same')='without budget cuts';else=NA")

#recode BOH with authority related variable
#BOH_o to BOH_8 represent Local BOH characteristics in the table
Ye$BOH_0 <- profile2013_core$c2q301
Ye$BOH_1 <- profile2013_core$c2q6a
Ye$BOH_2 <- profile2013_core$c2q7a
Ye$BOH_3 <- profile2013_core$c2q8a
Ye$BOH_4 <- profile2013_core$c2q9a
Ye$BOH_5 <- profile2013_core$c2q10a
Ye$BOH_6 <- profile2013_core$c2q11a
Ye$BOH_7 <- profile2013_core$c2q14a
Ye$BOH_8 <- profile2013_core$c2q15a
Ye$BOH_9 <- ifelse((profile2013_core$c6q75a=='checked'|
                      profile2013_core$c6q76a=='checked'|
                      profile2013_core$c6q77a=='checked'|
                      profile2013_core$c6q78a=='checked'|
                      profile2013_core$c6q79a=='checked')==T,'checked','unchecked')

#recode total expenditures
Ye$total_exp <- recode(profile2013_core$c3q15, 
                       "NA=NA;
                       1:499999='<$500000';
                       500000:999999='$500000-$999999';
                       1000000:4999999='$1000000-$4999999';
                       5000000:9999999='$5000000-$9999999';
                       else = '$10000000+'")

#recode expenditures per capita
Ye$per_capita_exp <- profile2013_core$c3q15/profile2013_core$c0population
Ye$per_capita_exp_cat[Ye$per_capita_exp<20] <- "<$20"
Ye$per_capita_exp_cat[Ye$per_capita_exp<=34.99&Ye$per_capita_exp>=20] <- "$20-$34.99"
Ye$per_capita_exp_cat[Ye$per_capita_exp<=44.99&Ye$per_capita_exp>=35] <- "$35-$44.99"
Ye$per_capita_exp_cat[Ye$per_capita_exp<=54.99&Ye$per_capita_exp>=45] <- "$45-$54.99"
Ye$per_capita_exp_cat[Ye$per_capita_exp>=55] <- '$55+'

#recode mean% revenues 
Ye$localrev <- profile2013_core$c3q17p/profile2013_core$c3q16 * 100
Ye$Medicaidrev<-profile2013_core$c3q17r/profile2013_core$c3q16 * 100
Ye$Federalrev<-profile2013_core$c3q17qe/profile2013_core$c3q16 * 100

#FTE (full-time equivalents workforce at the LHD)
Ye$FTE <- profile2013_core$c5q37

#Total Revenues
#Ye$total_rev <- profile2013_core$c3q16

#highest degree of top execitive
Ye$degree[(profile2013_core$c4q31a=='checked'|profile2013_core$c4q31b=='checked'|
             profile2013_core$c4q31c=='checked'|profile2013_core$c4q32a=='checked'|
             profile2013_core$c4q32b=='checked'|profile2013_core$c4q32c=='checked'|
             profile2013_core$c4q32d=='checked')==TRUE] <- 'Under Masters Degree'

Ye$degree[(profile2013_core$c4q33e=='checked'|profile2013_core$c4q33f=='checked'|
             profile2013_core$c4q33a=='checked'|profile2013_core$c4q33b=='checked'|
             profile2013_core$c4q33c=='checked'|profile2013_core$c4q33d=='checked')==TRUE] <- 'Masters Degree'

Ye$degree[(profile2013_core$c4q34a=='checked'|profile2013_core$c4q34b=='checked'|
             profile2013_core$c4q34c=='checked'|profile2013_core$c4q34d=='checked'|
             profile2013_core$c4q34e=='checked'|profile2013_core$c4q34f=='checked'|
             profile2013_core$c4q34g=='checked'|profile2013_core$c4q34h=='checked'|
             profile2013_core$c4q34i=='checked')==TRUE] <- 'Doctoral Degree'

#recode weight variable
Ye$weight01 <- profile2013_core$c0coreweight_s
Ye$weight02 <- profile2013_core$c0coreweight_p

#set as data frame
Ye <- as.data.frame(Ye)

#means compared to paper
library(questionr)
Ye_1 <- subset(Ye,is.na(Ye$budget)==F)
wtd.mean(Ye_1$localrev[Ye_1$budget=="with budget cuts"],
         weights=Ye_1$weight01[Ye_1$budget=="with budget cuts"],
         normwt="ignored",
         na.rm=T)

wtd.mean(Ye_1$localrev[Ye_1$budget=="without budget cuts"], 
         weights=Ye_1$weight01[Ye_1$budget=="without budget cuts"],
         normwt="ignored",
         na.rm=T)

wtd.mean(Ye_1$Medicaidrev[Ye_1$budget=="with budget cuts"],
         weights=Ye_1$weight01[Ye_1$budget=="with budget cuts"],
         normwt="ignored",
         na.rm=T)

wtd.mean(Ye_1$Medicaidrev[Ye_1$budget=="without budget cuts"], 
         weights=Ye_1$weight01[Ye_1$budget=="without budget cuts"],
         normwt="ignored",
         na.rm=T)

wtd.mean(Ye_1$Federalrev[Ye_1$budget=="with budget cuts"],
         weights=Ye_1$weight01[Ye_1$budget=="with budget cuts"],
         normwt="ignored",
         na.rm=T)

wtd.mean(Ye_1$Federalrev[Ye_1$budget=="without budget cuts"], 
         weights=Ye_1$weight01[Ye_1$budget=="without budget cuts"],
         normwt="ignored",
         na.rm=T)


#subset data to exclude NAs, n=1874#
Ye_1<-subset(Ye, 
             !(is.na(Ye$budget)|
                 is.na(Ye$governance_type)|
                 is.na(Ye$BOH_0)))

#relevel budget#
Ye_1$budget <- relevel(Ye_1$budget, ref="without budget cuts")
#relevel remaining variables
Ye_1$governance_type <- factor(Ye_1$governance_type, levels = c("state","local"," shared"))
Ye_1$population <- factor(Ye_1$population, levels = c("<25000","25000-49999","50000-99999","100000-499999","500000+"))
Ye_1$population <- relevel(Ye_1$population, ref="<25000")
Ye_1$BOH_0 <- factor(Ye_1$BOH_0, levels = c("no","yes"))
Ye_1$BOH_1 <- factor(Ye_1$BOH_1, levels = c("unchecked","checked"))
Ye_1$BOH_2 <- factor(Ye_1$BOH_2, levels = c("unchecked","checked"))
Ye_1$BOH_3 <- factor(Ye_1$BOH_3, levels = c("unchecked","checked"))
Ye_1$BOH_4 <- factor(Ye_1$BOH_4, levels = c("unchecked","checked"))
Ye_1$BOH_5 <- factor(Ye_1$BOH_5, levels = c("unchecked","checked"))
Ye_1$BOH_6 <- factor(Ye_1$BOH_6, levels = c("unchecked","checked"))
Ye_1$BOH_7 <- factor(Ye_1$BOH_7, levels = c("unchecked","checked"))
Ye_1$BOH_8 <- factor(Ye_1$BOH_8, levels = c("unchecked","checked"))
Ye_1$BOH_9 <- factor(Ye_1$BOH_9, levels = c("unchecked","checked"))
Ye_1$total_exp <- factor(Ye_1$total_exp, levels = c("<$500000","$500000-$999999","$1000000-$4999999","$5000000-$9999999","$10000000+"))
Ye_1$per_capita_exp_cat <- factor(Ye_1$per_capita_exp_cat, levels = c("<$20","$20-$34.99","$35-$44.99","$45-$54.99","$55+"))

Ye_1$BOH_1[is.na(Ye_1$BOH_1)] <- "unchecked"
Ye_1$BOH_2[is.na(Ye_1$BOH_2)] <- "unchecked"
Ye_1$BOH_3[is.na(Ye_1$BOH_3)] <- "unchecked"
Ye_1$BOH_4[is.na(Ye_1$BOH_4)] <- "unchecked"
Ye_1$BOH_5[is.na(Ye_1$BOH_5)] <- "unchecked"
Ye_1$BOH_6[is.na(Ye_1$BOH_6)] <- "unchecked"
Ye_1$BOH_7[is.na(Ye_1$BOH_7)] <- "unchecked"
Ye_1$BOH_8[is.na(Ye_1$BOH_8)] <- "unchecked"
Ye_1$BOH_9[is.na(Ye_1$BOH_9)] <- "unchecked"

#logistic regression
library(stats)
model1 <- glm(budget ~ population + governance_type + BOH_0+ BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_8 + BOH_9,
              data=Ye_1, weights = weight01,
              family="binomial",
              na.action="na.omit")

summary(model1)
exp(cbind(OR=coef(model1), confint(model1)))

#relevel total exp, per capita exp#
Ye_1$total_exp <- relevel(Ye_1$total_exp, ref="<$500000")
Ye_1$per_capita_exp_cat <- relevel(Ye_1$per_capita_exp_cat, ref="<$20")

#model 2 logistic regression
model2 <- glm(budget ~ population + governance_type  + BOH_0+ BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_8 + BOH_9 +
                total_exp + per_capita_exp_cat + localrev + Medicaidrev + Federalrev,
              data = Ye_1, weights = weight01,
              family = "binomial",
              na.action="na.omit")

summary(model2)
exp(cbind(OR=coef(model2), confint(model2)))

##MULTIPLE IMPUTATION##

#look at missing data 
library(mice)
md.pattern(Ye_1) #1164 with no missing data

#impute missing data using MICE
library(mice)
mi_Ye <- mice(Ye_1, m=5)
summary(imputed_Data)

#build predictive model

modelfit1a <- with(mi_Ye, glm(budget ~ population + governance_type +  BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_9, 
                              family="binomial",
                              weights=Ye_1$weight01,
                              na.action="na.omit"))
print(pool(modelfit1a))
summary(pool(modelfit1a)) #no missing data, so no change in results 

modelfit2a <- with(mi_Ye, glm(budget ~ population + governance_type +  BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_9
                            + per_capita_exp_cat + localrev + Medicaidrev + Federalrev, 
                            family="binomial",
                            weights=Ye_1$weight01,
                            na.action="na.omit"))
summary(pool(modelfit2a))


##CHECKING ASSUMPTIONS OF MODEL##
##multicolinearity (want sqrt of VIF<2)
sqrt(vif(model1)) #BOH_0 and BOH_8 >2 
sqrt(vif(model2))  #population, BOH_0, BOH_8, total exp >2

#remove BOH_0, BOH_8 and total_exp from model 1 and model 2
model1a <- glm(budget ~ population + governance_type + BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_9,
               data=Ye_1, family="binomial", 
               weights=Ye_1$weight01,
               na.action="na.omit")
sqrt(vif(model1a)) #all variables<2
summary(model1a)
exp(cbind(OR=coef(model1a), confint(model1a)))

model2a <- glm(budget ~ population + governance_type + BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_9 + 
                 per_capita_exp_cat + localrev + Medicaidrev + Federalrev,
               data = Ye_1, weights = weight01,
               family = "binomial",
               na.action="na.omit")
summary(model2a)
exp(cbind(OR=coef(model2a), confint(model2a)))

sqrt(vif(model2a)) #all variables <2

##model fit/precent predicted
library(descr)
x <- predict(model1a, newdata=Ye_1, type="response")
Ye_1$pred1a <- factor(1*(x > .5), labels=c("No", "Yes"))
CrossTable(Ye_1$budget,Ye_1$pred1a, expected=F, prop.chisq=F, sresid=F, prop.r=T, prop.c=F, prop.t=F)

y <- predict(model2a, newdata=Ye_1, type="response")
Ye_1$pred2a <- factor(1*(x > .5), labels=c("No", "Yes"))
CrossTable(Ye_1$budget,Ye_1$pred2a, expected=F, prop.chisq=F, sresid=F, prop.r=T, prop.c=F, prop.t=F)


##likelihood ratio test
#model 1a
model1aChi<-model1a$null.deviance - model1a$deviance
chidf1a <-model1a$df.null - model1a$df.residual
chisq.prob1a <- 1 - pchisq(model1aChi, chidf1a)
model1aChi
chidf1a
chisq.prob1a  #better than baseline

#model 2a

model2aChi<-model2a$null.deviance - model2a$deviance
chidf2a <-model2a$df.null - model2a$df.residual
chisq.prob2a <- 1 - pchisq(model2aChi, chidf2a)
model2aChi
chidf2a
chisq.prob2a  #better than baseline

#compare model 1a and 2a

modelLRChi<-model1a$null.deviance - model2a$deviance
LRdf <-model1a$df.null - model2a$df.residual
chisq.prob <- 1 - pchisq(modelLRChi, LRdf)
modelLRChi
LRdf
chisq.prob #model 2a better than 1a

##linearity for continuous predictors - start with localrev
Ye_1$localrevlogint<-log(Ye_1$localrev)*Ye_1$localrev

model2a1 <- glm(budget ~ population + governance_type + 
                  BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_9 +
                  per_capita_exp_cat + localrev + Medicaidrev + Federalrev +
                  localrevlogint,
                data=Ye_1, family="binomial", 
                weights=Ye_1$weight01,
                na.action="na.omit")
summary(model2a1) #assumption not met

Ye_1$Medicaidrevlogint<-log(Ye_1$Medicaidrev)*Ye_1$Medicaidrev

model2a2 <- glm(budget ~ population + governance_type + 
                  BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_9 +
                  per_capita_exp_cat + localrev + Medicaidrev + Federalrev +
                  Medicaidrevlogint,
                data=Ye_1, family="binomial", 
                weights=Ye_1$weight01,
                na.action="na.omit")
summary(model2a2) #assumption met

Ye_1$Federalrevlogint<-log(Ye_1$Federalrev)*Ye_1$Federalrev

model2a3 <- glm(budget ~ population + governance_type + 
                  BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_9 +
                  per_capita_exp_cat + localrev + Medicaidrev + Federalrev +
                  Federalrevlogint,
                data=Ye_1, family="binomial", 
                weights=Ye_1$weight01,
                na.action="na.omit")
summary(model2a3) #assumption not met

#durbin-watson for correlated residuals
#include identifier variable
Ye$lhdID<-profile2013_core$nacchoid
Ye<-subset(Ye, 
             !(is.na(Ye$budget)|
                 is.na(Ye$governance_type)|
                 is.na(Ye$BOH_0)))
Ye_1$lhdID<-Ye$lhdID
Ye_1$standardres1a<-rstandard(model1a)
plot(Ye_1$lhdID, Ye_1$standardres1a)  #assumption met
