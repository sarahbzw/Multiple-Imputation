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

