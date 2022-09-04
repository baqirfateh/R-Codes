#############################################
# Author: Baqir Fateh
# Project: Data Science Practicum 
# #########################################
rm(list=ls())
library(haven)
library(foreign)
library(tidyverse)
library(ggplot2)
library(data.table)
library(readr)
library(survey)
library(srvyr)
library(factoextra)
library(glmnet)

## Downloading data from the website 
temp<-tempfile()
download.file("https://meps.ahrq.gov/mepsweb/data_files/pufs/h216/h216dta.zip",temp)
meps_raw<-read_dta(unzip(temp,"h216.dta"))
rm(temp)
## putting the variables to lower case 
names(meps_raw)<-tolower(names(meps_raw))

#Removing some of survey administration and other variables
meps_reduced0 <- meps_raw%>%select(-c(duid:pid,panel:begrfm31,endrfm31:rurslt53,dobmm,dobyy,vsaqw19f:varpsu,hieuidx,spouid31:dapid53x,
                                      hibpdx:foodvl19, wagimp19:othimp19, insurc19:morecovr,verflg31:verflg19,
                                      trich31x:tricr53x, triat31x:triat53x, mcaid31:mcaid53,mcaid31x:mcaid53x,
                                      mcare31:mcaid53,mcare31:mcare53, mcare31x:mcare53x,mcdat31x:mcdat53x,govta31:govta53,
                                      govaat31:govaat53,govtb31:govtb53,govbat31:govbat53,govtc31:govtc53,
                                      govcat31:govcat53,vaprog31:vaprog53,vaprat31:vaprat53,ihs31:ihsat53,
                                      pridk31:pridk53,prieu31:prieu53,pring31:prstx53,priv31:priv53,
                                      privat31:priv53,pub31x:pubat53x,pubat31x:pubat53x,ins31x:ins53x,
                                      insat31x:insat53x,pmedpy31:pmedpy53, dentin31:dentin53,pmedin31:pmedup53))

dim(meps_reduced0)
##### Converting the categorical variables to binary ones
var_list <- meps_reduced0%>%select(sex,trija19x:unins19,tricr19x:dntins19)
indictor_func <- function(x){
  ifelse(x==1,1,0)
}
indicator_vars <- data.frame(sapply(var_list,indictor_func))

# Combining the indicator and number numerical variables
meps_reduced <- meps_reduced0%>%select(-c(trija19x:unins19,tricr19x:dntins19))%>%
  cbind(indicator_vars)%>%relocate(c(trija19x:unins19,tricr19x:dntins19),.before = tottch19)

## top-coding the total individual income

meps_reduced <- meps_reduced%>%mutate(inc_top_coded=case_when(ttlp19x>=100000~99999,
                                                                   ttlp19x<100000~ttlp19x))%>%
  relocate(inc_top_coded,.after = ttlp19x)%>%select(-ttlp19x)


##Top coding income and health expenditure 
sum(meps_indicator$ttlp19x>=100000)/nrow(meps_reduced)
meps_indicator <- meps_indicator%>%mutate(inc_top_coded=case_when(ttlp19x>=100000~99999,
                                                                  ttlp19x<100000~ttlp19x))%>%
  relocate(inc_top_coded,.after = ttlp19x)
#proportion of health expenditure greater than 100K
sum(meps_indicator$totexp19>=100000)/nrow(meps_reduced)
#proportion of health expenditure equal to zero 
sum(meps_indicator$totexp19==0)/nrow(meps_reduced)

meps_indicator <- meps_indicator%>%mutate(totexp19_top_coded= case_when(totexp19>=100000~99999,
                                                                       totexp19<100000~totexp19))%>%
  relocate(totexp19_top_coded, .after = totexp19)



## Checking for -8, -7 and -1 in the health status variables 
health_var <- meps_reduced%>%select(rthlth31:mnhlth53)%>%as.list()
health_status <- data.frame(inapplicable=sapply(health_var,function(x){sum(x==-1)}),
                           refused=sapply(health_var,function(x){sum(x==-7)}),
                           dk=sapply(health_var,function(x){sum(x==-8)}))%>%
  rownames_to_column(var = "variable")
health_status



## Checking age
meps_reduced%>%summarise(avg=mean(agelast),
                         med=median(agelast),
                         min=min(agelast),
                         max=max(agelast))

#creating age group
meps_reduced <- meps_reduced%>%mutate(age_group=case_when(agelast<=18~1,
                                                            agelast>18&agelast<35~2,
                                                            agelast>=35&agelast<65~3,
                                                            agelast>=35&agelast<65~3,
                                                            agelast>=65~4))%>%
  relocate(age_group,.after = agelast)#juxtaposing all the age variables 

##distribution by age group weighted and unweighted 

meps_reduced%>%group_by(age_group)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%select(age_group,mean)
meps_reduced%>%as_survey(weights=c(perwt19f))%>%group_by(age_group)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))%>%select(age_group,mean_wt)

# comment: the sample is balanced by age distribution


#creating indicator variable for sex
meps_reduced <- meps_reduced%>%mutate(sex=ifelse(sex==1,1,0))

#checking sex composition
meps_reduced%>%count(sex)%>%mutate(mean=n/sum(n))
meps_reduced%>%as_survey(weights=c(perwt19f))%>%group_by(sex)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))
# comment: male and female and female representation is balanced

#Race distribution, weighted and unweighted
meps_reduced%>%group_by(racev1x)%>%summarise(n=n())%>%
mutate(mean=n/sum(n))

meps_reduced%>%as_survey(weights=c(perwt19f))%>%group_by(racev1x)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))


## Health status
#creating dummy variable 
meps_reduced <-
  meps_reduced%>%mutate(hlth_r31_excel=ifelse(rthlth31==1,1,0),
                                    hlth_r31_vgood=ifelse(rthlth31==2,1,0),
                                    hlth_r31_good=ifelse(rthlth31==3,1,0),
                                    hlth_r31_fair=ifelse(rthlth31==4,1,0),
                                    hlth_r31_poor=ifelse(rthlth31==5,1,0))

#grouping health status into good and not good 

meps_reduced <- meps_reduced%>%mutate(hlth_st_bin=case_when(rthlth31%in%c(1,2,3)~1,
                                                          rthlth31%in%c(-8,-7,-1,4,5)~0))

## income
#individual income
meps_reduced%>%summarise(avg=mean(ttlp19x),
                         min=min(ttlp19x),
                         max=max(ttlp19x))

meps_reduced%>%summarise(avg=mean(faminc19),
                         min=min(faminc19),
                         max=max(faminc19))
#distribution by income group
meps_reduced%>%group_by(povcat19)%>%
  summarise(n=n())%>%mutate(mean=n/sum(n))
meps_reduced%>%as_survey(weights=c(perwt19f))%>%group_by(povcat19)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))
# comment: sample is balanced by income group

#share if income holders above 100k
sum(meps_reduced$ttlp19x>=100000)/nrow(meps_reduced)

# top coding income equal or above 100K as 99999
meps_reduced<-meps_reduced%>%mutate(inc_top_coded=case_when(ttlp19x>=100000~99999,
                                                            ttlp19x<100000~ttlp19x))%>%
  relocate(inc_top_coded,.after = ttlp19x)
sum(meps_reduced$inc_top_coded>99999)/nrow(meps_reduced)


##################### Visualization #########################

#age distribution 
meps_reduced%>%ggplot(aes(agelast))+
  geom_histogram(binwidth = 1,position = "identity",alpha=0.5,col="yellow")+
  xlab("Age")+ylab("Count")+xlim(0,85)+
  geom_density(aes(y=1.1*..count..),col="blue")+
  ggtitle("Distribution of Age in the Last Round of the Survey")+
  theme_bw()


##Health status
meps_reduced%>%filter(rthlth31%in%c(1:5))%>%
  ggplot(aes(rthlth31))+
  geom_histogram(binwidth = 1,position = "identity",alpha=0.5,col="green")+
  xlab("Health Status")+ylab("Counts")+
  ggtitle("Self-reported health status")+
  theme_bw()

#Health status and sex
meps_reduced%>%filter(rthlth31%in%c(1:5))%>%
  mutate(sex=ifelse(sex==1,"Male","Female"))%>%
  ggplot(aes(rthlth31))+
  geom_histogram(binwidth =1,position = "identity",alpha=0.5,col="blue")+
  xlab("")+ylab("Count")+
  ggtitle("Self-reported health status by sex")+
  facet_wrap(~sex,ncol = 2)+
  theme_bw()


meps_reduced%>%group_by(age_group)%>%summarise(mean=mean(totexp19),
                                               median=median(totexp19),
                                               min=min(totexp19),
                                               max=max(totexp19))

### Distribution of health expenditure 

meps_reduced%>%ggplot(aes(log(totexp19,base = 3)))+
  geom_histogram(binwidth = 1,position = "identity",alpha=0.5,col="green")+
  xlab("Log of Total Health Expenses")+ylab("Counts")+
  geom_density(aes(y=1.1*..count..),col="red")+
  ggtitle("Total Health Expenses")+
  theme_bw()

### Distribution of health expenditure by age group
meps_reduced%>%filter(totexp19>0)%>%
  ggplot(aes(agelast,log(totexp19),col=age_group))+geom_point()+
  stat_smooth(method = "lm",col="yellow")+stat_smooth(se=FALSE,col="red")+
  xlab("Age")+ylab("Log of Total Health Expenditure")+xlim(0,85)+
  scale_fill_continuous(name="Age Group",labels = c("65-85","35-64","19-34","0-18"))+
  theme_bw()


meps_reduced%>%filter(rthlth31%in%c(1:5))%>%
  mutate(rthlth31=case_when(rthlth31==1~'Excellent',rthlth31==2~'Very Good',
                            rthlth31==3~'Good', rthlth31==4~'Fair',
                            rthlth31==5~'Poor'))%>%
  ggplot(aes(reorder(rthlth31,log(totexp19), FUN=median),log(totexp19),fill=rthlth31))+
  geom_boxplot()+xlab("Self-reported Health Status")+
  ylab("Log of Total Health Expenditure")+
  scale_fill_discrete(name="Health Status")+
  theme(legend.position = "none")
  
### Income distribution 

meps_reduced%>%filter(ttlp19x>0)%>% 
  ggplot(aes(log(ttlp19x,base = 3)))+
  geom_histogram(binwidth = 1,position = "identity",alpha=0.5,col="pink")+
  xlab("Log of Total Health Expenses")+ylab("Counts")+
  geom_density(aes(y=1.1*..count..),col="blue")+
  ggtitle("Total Health Expenses")+
  #facet_wrap(~begrfy31,ncol=TRUE)+
  theme_bw()

# Health expenses and income category 
meps_reduced%>%
  mutate(povcat19=case_when(povcat19==1~'Poor/Negative',povcat19==2~'Never Poor',
                            povcat19==3~'Low Income', povcat19==4~'Middle Income',
                            povcat19==5~'High Income'))%>%
  ggplot(aes(reorder(povcat19,log(totexp19), FUN=median),log(totexp19),fill=povcat19))+
  geom_boxplot()+xlab("Income Category")+
  ylab("Log of Total Health Expenditure")+
  scale_fill_discrete(name="Health Status")+
  theme(legend.position = "none")

hlth_exp_income<- meps_reduced%>%
  mutate(povcat19=case_when(povcat19==1~'Poor/Negative',povcat19==2~'Never Poor',
                            povcat19==3~'Low Income', povcat19==4~'Middle Income',
                            povcat19==5~'High Income'))%>%
  filter(ttlp19x>0)%>%mutate(health_exp_income=totexp19/ttlp19x)
p <- hlth_exp_income%>%group_by(povcat19)%>%summarise(med=median(health_exp_income))

p%>%ggplot(aes(reorder(povcat19,med),med, fill=povcat19))+geom_col()+
  ggtitle("Health Expenses as a Portion of Total Income")+
  xlab("Income Category")+ylab("Median")+
  geom_text(aes(label = round(med,2)), vjust = -0.2)+
  theme(legend.position = "none")



### Data preparation for the shrinkage model 
#normalizing the numerical variables

x_num <- meps_reduced%>%filter(rthlth31%in%c(1:5))%>%
  select(age31x,marry19x,inc_top_coded,wagep19x,povlev19,
         tottch19:tototh19)
x_scaled <- scale(x_num)
ind_var <- meps_reduced%>%filter(rthlth31%in%c(1:5))%>%select(c(sex,trija19x:unins19,tricr19x:dntins19))
x_var <-as.matrix(cbind(x_scaled,ind_var))
y_var <- meps_reduced%>%filter(rthlth31%in%c(1:5))%>%select(rthlth31)%>%scale()


########### Models and evaluation
## top-coding the total individual income

meps_reduced <- meps_reduced%>%mutate(inc_top_coded=case_when(ttlp19x>=100000~99999,
                                                                      ttlp19x<100000~ttlp19x))%>%
  relocate(inc_top_coded,.after = ttlp19x)%>%select(-ttlp19x)


####################################################################
#                                                                  #
# Model Selection and evaluation                                   #
#                                                                  #
####################################################################

## Data preparation for the shrinkage model 
#normalizing the numerical variables

x_num <- meps_reduced%>%filter(rthlth31%in%c(1:5))%>%
  select(age31x,marry19x,inc_top_coded,wagep19x,povlev19,
         tottch19:tototh19)
x_scaled <- scale(x_num)
ind_var <- meps_reduced%>%filter(rthlth31%in%c(1:5))%>%select(c(sex,trija19x:unins19,tricr19x:dntins19))
x_var <-as.matrix(cbind(x_scaled,ind_var))

y_var <- meps_reduced%>%filter(rthlth31%in%c(1:5))%>%select(rthlth31)%>%scale()

############### Lasso #############################


lasso_eq <- glmnet(x_var, y_var,family = "gaussian",
                   intercept = F, alpha = 1)

## Graphing the regularized parameters. 
matplot(log(lasso_eq$lambda), t(lasso_eq$beta),
        type="l", main="Lasso", lwd=2,xlab = "log(lambda)", ylab = "Coefficients" )


##### Using cross validation using cv.glmnet

## 
# Running cross-validation k-10
  lasso_cv <- cv.glmnet(x_var, y_var,  family="gaussian",
                        intercept = F, alpha=1,nfolds = 10)
  plot(lasso_cv) 
  # The value of minimum lambda that minimizes MSE
  print(paste(lasso_cv$lambda.min,
              log(lasso_cv$lambda.1se)))



### cross-validated lambda 

lambda_cv <- lasso_cv$lambda.min


### Fitting final model 

lasso_eq <- glmnet(x_var, y_var,family = "gaussian",
                   intercept = F, alpha = 1, lambda = lambda_cv)

coef(lasso_eq)
y_hat_lasso <- predict(lasso_eq, x_var)
ssr_lasso <- t(y_var - y_hat_lasso) %*% (y_var - y_hat_lasso)
rsq_lasso <- cor(y_var, y_hat_lasso)^2
rsq_lasso

##################### Ridge ##############################

ridge_eq <- glmnet(x_var, y_var,family = "gaussian",
                   intercept = F, alpha = 0)

## Graphing the regularized parameters. 
matplot(log(ridge_eq$lambda), t(ridge_eq$beta),
        type="l", main="Ridge", lwd=2)


##### Using cross validation using cv.glmnet


# Running cross-validation k-10

ridge_cv <- cv.glmnet(x_var, y_var,  family="gaussian",
                      intercept = F, alpha=0,nfolds = 10)
plot(ridge_cv) 

## The value of minimum lambda that minimizes MSE
print(paste(ridge_cv$lambda.min,
            log(ridge_cv$lambda.1se)))



### cross-validated lambda 

lambda_cv_ridge <- ridge_cv$lambda.min


### Fitting final model 

ridge_eq <- glmnet(x_var, y_var,family = "gaussian",
                   intercept = F, alpha = 0, lambda = lambda_cv_ridge)
coef(ridge_eq)

y_hat_ridge <- predict(ridge_eq, x_var)
ssr_ridge <- t(y_var - y_hat_ridge) %*% (y_var - y_hat_ridge)
rsq_ridge <- cor(y_var, y_hat_ridge)^2
rsq_ridge


### Model comparison 

rsq <- cbind("R-squared" = c(rsq_lasso,rsq_ridge))

rownames(rsq) <- c("lasso-cv","rsq_ridge")
print(rsq)

############## PCA ####################

## Preparing data set 

pca_data <- meps_reduced%>%filter(rthlth31%in%c(1:5))%>%
  select(age31x,marry19x,rthlth31,inc_top_coded,wagep19x,povlev19,
                                       tottch19:tototh19,trija19x:unins19,tricr19x:dntins19)

## Data partition 

#removing the columns that sum to zero
pca_data <- pca_data[,colSums(pca_data)!=0]
sum(colSums(pca_data)==0)

## Data partition
set.seed(123)
ind <- sample(2,nrow(pca_data), replace = TRUE, prob = c(0.6, 0.4))
train_set <- pca_data[ind==1,]
test_set <- pca_data[ind==2,]

## Running PCA
pro.out <- prcomp(train_set,scale. = TRUE, center = TRUE)

pro.out$rotation[1:10,1:5]

pr.var<-pro.out$sdev^2

pve<-pr.var/sum(pr.var)

sum(pve[1:50])

plot(cumsum(pve[1:50]), xlab = "Principal Components",
     ylab = "Variance Explained",
     ylim = c(0, 1), type = "l", col="blue",lty=2,lwd=2)

library(factoextra)
vars <- get_pca_var(pro.out)
head(vars$contrib[1:15,1:50])
training <- predict(pro.out,train_set)
training <- data.frame(training,health_status = train_set$rthlth31)%>%
  mutate(health_status1=case_when(health_status==1~"Excellent",health_status==2~"Very Good",
                                  health_status==3~"Good",health_status==4~"Fair",
                                  health_status==5~"Poor"))

testing <- predict(pro.out,test_set)
testing <- data.frame(testing,health_status = test_set$rthlth31)%>%
  mutate(health_status1=case_when(health_status==1~"Excellent",health_status==2~"Very Good",
                                  health_status==3~"Good",health_status==4~"Fair",
                                  health_status==5~"Poor"))

## Running multinomial logistic 

library(nnet)
my_model <- multinom(health_status ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+
                       PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+
                       PC22+PC23+PC24+PC25+PC26+PC27+PC28+PC29+PC30+
                       PC31+PC32+PC33+PC34+PC35+PC36+PC37+PC38+PC39+
                       PC40+PC41+PC42+PC43+PC44+PC45+PC46+PC47+PC48+PC49+PC50, data = training)


## Confusion Matrix - Training Data 
pred_train <- predict(my_model,training)

result <- table(pred_train,training$health_status)

sum(diag(result))/sum(result)

## Confusion Matrix - Test Data

pred_test <- predict(my_model,testing)
tab <- table(pred_test,testing$health_status)

sum(diag(tab))/sum(tab)
## This model is not performing well on this data set 

## Ordered Logit 
library(MASS)
training$health_status1 <- factor(training$health_status1,levels = c("Excellent",
                                                                     "Very Good","Good",
                                                                     "Fair", "Poor",
                                                                     ordered = TRUE))
logit_mdl <- polr(health_status1~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+
                    PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+
                    PC22+PC23+PC24+PC25+PC26+PC27+PC28+PC29+PC30+
                    PC31+PC32+PC33+PC34+PC35+PC36+PC37+PC38+PC39+
                    PC40+PC41+PC42+PC43+PC44+PC45+PC46+PC47,
                  data = training, Hess = TRUE)
summary(logit_mdl)


## Confusion matrix

testing$health_status1 <- factor(testing$health_status1,levels = c("Excellent",
                                                                     "Very Good","Good",
                                                                     "Fair", "Poor", ordered = TRUE))
pred_ord <- predict(logit_mdl,testing)

tab_ord <- table(testing$health_status1,pred_ord)
tab_ord
sum(diag(tab_ord))/sum(tab_ord)

#### END ####################

