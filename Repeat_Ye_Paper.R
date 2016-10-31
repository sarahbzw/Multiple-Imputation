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
Ye$localrev <- profile2013_core$c3q17p/profile2013_core$c3q16*100
Ye$Medicaidrev<-profile2013_core$c3q17r/profile2013_core$c3q16*100
Ye$Federalrev<-profile2013_core$c3q17qe/profile2013_core$c3q16*100

#recode weight variable
Ye$weight01 <- profile2013_core$c0coreweight_s
Ye$weight02 <- profile2013_core$c0coreweight_p

#set as data frame
Ye <- as.data.frame(Ye)

#subset non-missing in weight
Ye_1 <- subset(Ye,is.na(Ye$weight01)==F)
Ye_2 <- subset(Ye,is.na(Ye$weight02)==F)

#weight
library(survey)
Ye_1 <- svydesign(ids = ~1, data = Ye_1, weights = Ye_1$weight01)
Ye_2 <- svydesign(ids = ~1, data = Ye_2, weights = Ye_2$weight02)

#percentage compared to paper 
prop.table(svytable(~population+budget, design=Ye_1),2)
prop.table(svytable(~population+budget, design=Ye_2),2)
prop.table(svytable(~governance_type+budget, design=Ye_1),2)
prop.table(svytable(~governance_type+budget, design=Ye_2),2)
prop.table(svytable(~total_exp+budget, design=Ye_1),2)
prop.table(svytable(~per_capita_exp_cat+budget, design=Ye_1),2)
#prop.table(svytable(~per_capita_exp+budget, design=Ye_1),2)
#for BOH, NA is included in denominator when calculating percentage
prop.table(svytable(~addNA(BOH_0)+addNA(budget),  exclude=NULL, na.action=na.pass ,design=Ye_1),2)
prop.table(svytable(~addNA(BOH_1)+addNA(budget),  exclude=NULL, na.action=na.pass ,design=Ye_1),2)
prop.table(svytable(~addNA(BOH_2)+addNA(budget),  exclude=NULL, na.action=na.pass ,design=Ye_1),2)
prop.table(svytable(~addNA(BOH_9)+addNA(budget),  exclude=NULL, na.action=na.pass ,design=Ye_1),2)
prop.table(svytable(~addNA(governance_type)+addNA(budget),  exclude=NULL, na.action=na.pass ,design=Ye_1),2)
#?
prop.table(svytable(~addNA(total_exp)+addNA(budget),  exclude=NULL, na.action=na.pass ,design=Ye_1),2)
# this is close to the table in paper
# prop.table(table(Ye_1$BOH_1, Ye_1$budget, useNA = c("always")),2)

#means compared to paper
library(questionr)
Ye_1 <- subset(Ye,is.na(Ye$budget)==F)
wtd.mean(Ye_1$localrev[Ye_1$budget=="with budget cuts"],
         weights=Ye_1$weights01,
         normwt="ignored",
         na.rm=T)

wtd.mean(Ye_1$localrev[Ye_1$budget=="without budget cuts"], 
          weights=Ye_1$weights01,
          normwt="ignored",
          na.rm=T)

wtd.mean(Ye_1$Medicaidrev[Ye_1$budget=="with budget cuts"],
         weights=Ye_1$weights01,
         normwt="ignored",
         na.rm=T)

wtd.mean(Ye_1$Medicaidrev[Ye_1$budget=="without budget cuts"], 
         weights=Ye_1$weights01,
         normwt="ignored",
         na.rm=T)

wtd.mean(Ye_1$Federalrev[Ye_1$budget=="with budget cuts"],
         weights=Ye_1$weights01,
         normwt="ignored",
         na.rm=T)

wtd.mean(Ye_1$Federalrev[Ye_1$budget=="without budget cuts"], 
         weights=Ye_1$weights01,
         normwt="ignored",
         na.rm=T)

##TABLE 4 - Model 1##

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
model1 <- glm(budget ~ population + governance_type  + BOH_0+ BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_8 + BOH_9,
              data=Ye_1, family="binomial", 
              weights=Ye_1$weight01,
              na.action="na.omit")

summary(model1)
exp(cbind(OR=coef(model1), confint(model1)))

#relevel total exp, per capita exp#
Ye_1$total_exp <- relevel(Ye_1$total_exp, ref="<$500000")
Ye_1$per_capita_exp_cat <- relevel(Ye_1$per_capita_exp_cat, ref="<$20")

#model 2 logistic regression
model2 <- glm(budget ~ population + governance_type  + BOH_0+ BOH_1 + BOH_2 + BOH_3 + BOH_4 + BOH_5 + BOH_6 + BOH_7 + BOH_8 + BOH_9 +
              total_exp + per_capita_exp_cat + localrev + Medicaidrev + Federalrev,
              data = Ye_1, weights = weight02,
              family = "binomial",
              na.action="na.omit")

summary(model2)
exp(cbind(OR=coef(model2), confint(model2)))

#Multiple Imputation
#auxillary variables (from Dr. Harris) = staff size (FTE), education of exec, rural/urban/etc, revenue
