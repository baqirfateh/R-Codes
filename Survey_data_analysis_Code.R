################################################################################
# Author: Baqir Fateh                                                          #   
# Project: Survey data Analysis                                                #              
# Date: June 2021                                                              #
################################################################################
library(readxl)
library(tidyverse)
library(ggplot2)
library(haven)
library(stringr)
library(survey)
library(srvyr)
library(foreign)



getwd()

## Second Round Data 

data_r2_weight<-read_dta("R2_Data_w.dta")
dim(data_r2_weight)
n_distinct(data_r2_weight$userid)
data_r2_weight_sliced<-data_r2_weight%>%select(userid,geo3:weight_2)

## Round two data - final 

data_r2_updated<-read_dta("R2_data_labelled.dta")
dim(data_r2_updated)
n_distinct(data_r2_updated$userid)
##Adding the relevant columns of weighted data to the updated unweighted data
data_r2<-data_r2_weight_sliced%>%left_join(data_r2_updated,by="userid")
dim(data_r2)
n_distinct(data_r2$userid)
##first round data
data_r1<-read_dta("R2.dta")
dim(data_r1)
n_distinct(data_r1$userid)


# Extracting user IDs that are in the second round data but don't match the first round 
#no_match<-data_r2%>%anti_join(data_r1,by="userid")
#n_distinct(no_match$userid)

#### Generating new variables 
#locality 
data_r2<-data_r2%>%mutate(locality=ifelse(urban==1,"urban","rural"))

#Calculating labor participation                                                       
data_r2<-data_r2%>%mutate(labor_participation=case_when(L03==1~'worked in the past 7 days',
                                                        L04==1~'temporary absence',
                                                        L07==1&L04==0~'looking for job',
                                                        L07==0~'not looking for job'))
labor_participation_test<-data_r2%>%count(labor_participation)%>%mutate(mean=n/sum(n))
labor_participation_test_wet<-data_r2%>%as_survey(weights=c(hhw))%>%group_by(labor_participation)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))


#Calculating labor composition 

data_r2<-data_r2%>%mutate(labor_composition=case_when(L16%in%c(1,3)&L03==1|L04==1&L12==1~'Wage Worker-Public',
                                                      L16%in%c(2,4:6)&L03==1|L04==1&L12==1~'Wage Worker-Private',
                                                      L15==2~'Self-employed Worker',
                                                      L12==2&L04==1~"Self-employed Worker",
                                                      L07==1~'Unemployed',
                                                      L07==0~'Not in labor force'))
                                                     
labor_composition_test<-data_r2%>%group_by(labor_composition)%>%summarise(n=n())%>%mutate(mean=n/sum(n))
labor_composition_test_wt<-data_r2%>%as_survey(weights=c(hhw))%>%group_by(labor_composition)%>%
  summarise(n=survey_total())%>%mutate(mean_test=n/sum(n))

##Merging Round 1 and Round 2 data sets
data_r1$userid<-str_replace(data_r1$userid,pattern = "-",replacement = "")
data_r2$userid<-str_replace(data_r2$userid,pattern = "-",replacement = "")
data_merged<-data_r2%>%inner_join(data_r1,by="userid")


##Decompose unemployed 
unemployed<-data_merged%>%filter(labor_composition=="Unemployed")%>%
  select(L04,L05,L07,L11,L10_Job_Code:L12,L14:L16,v4a_30, v4a_04,v4a_03a,v4a_05,v4a_02job_code_label:v4c_01isco_label)
unemployed$v4a_04<-as.character(unemployed$v4a_04)
unemployed%>%count(v4a_30)
unemployed%>%count(v4a_04)
unemployed%>%count(v4a_05)
unemployed%>%count(L12)
data_r2%>%count(L12)
unemployed%>%filter(L12%in%c(1,2)&v4a_04!="NA")%>%count(L12)
unknown<-unemp_total-(unemp_wage_worker+unemp_self_employed)

### Demography 

#Gender 
data_r2%>%count(male)%>%mutate(mean=n/sum(n))

gender_wt<-data_r2%>%
  as_survey(weights=c(weight_2))%>%group_by(O05)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))
gender_hhw<-data_r2%>%
  as_survey(weights=c(hhw))%>%group_by(O05)%>%
  summarise(n=survey_total())%>%mutate(mean_hhw=n/sum(n))
gender<-data_r2%>%group_by(O05)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq), O05=case_when(O05==1~'Male',O05==2~'Female'))
#Age
age<-data_r2%>%group_by(O06)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),O06=case_when(O06==2~'15-30',O06==3~'Over 30'))
age_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(O06)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),O06=case_when(O06==2~'15-30',O06==3~'Over 30'))

#Education 
educ<-data_r2%>%group_by(O07)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),O07=case_when(O07==9~'No formal education ',
                                     O07==1~'6th grade (primary)',
                                     O07==2~'9th grade (elementary)',
                                     O07==3~'12th grade (high school)',
                                     O07==4~'14th grade (community college)',
                                     O07==5~'Bachelor',
                                     O07==6~'Master',
                                     O07==7~'Ph.D.',
                                     O07==8~'Religious education'))%>%arrange(desc(mean))
educ_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(O07)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq),O07=case_when(O07==9~'No formal education ',
                                           O07==1~'6th grade (primary)',
                                           O07==2~'9th grade (elementary)',
                                           O07==3~'12th grade (high school)',
                                           O07==4~'14th grade (community college)',
                                           O07==5~'Bachelor',
                                           O07==6~'Master',
                                           O07==7~'Ph.D.',
                                           O07==8~'Religious education'))

##Movement 
#province
province<-data_r2%>%group_by(O08)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq))
province_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(O08)%>%
  summarise(freq=survey_total())%>%mutate(mean_wt=freq/sum(freq))
#District 
district<-data_r2%>%filter(O10%in%c(0,1))%>%group_by(O10)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq))
district_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(O10%in%c(0,1))%>%
  group_by(O10)%>%summarise(freq=survey_total())%>%mutate(mean_wt=freq/sum(freq))
#Reasons for moving 
reason_moving<-data_r2%>%filter(as.character(O12)!="NA")%>%group_by(O12)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq))
reason_moving_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(as.character(O12)!="NA")%>%
  group_by(O12)%>%summarise(freq=survey_total())%>%mutate(mean_wt=freq/sum(freq))

###Labor Market

#Sources of Family Income
income_source<-function(x){
  mean(x==1,na.rm=TRUE)
}
income_var<-data_r2%>%select(L02_1:L02_16)%>%as.list()
family_income<-data.frame(mean=sapply(income_var, income_source))%>%
  rownames_to_column(var="variable")

income_source_wt<-function(x){
  weighted.mean(x==1,na.rm=TRUE, data_r2$weight_2)
}
income_var_wt<-data_r2%>%select(L02_1:L02_16)%>%as.list()
family_income_wt<-data.frame(mean_wt=sapply(income_var_wt, income_source_wt))%>%
rownames_to_column(var="variable_wt")
family_income_combined<-cbind(family_income,family_income_wt)

#Worked in the past seven days 
worked<-data_r2%>%group_by(L03)%>%summarise(n=n())%>%mutate(mean=n/sum(n))

worked_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(L03)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))
#Temporarily absent from work
temp_absence<-data_r2%>%filter(L04%in%c(0,1))%>%group_by(L04)%>%summarise(n=n())%>%mutate(mean=n/sum(n))
mean(data_r2$L04==1,na.rm=TRUE)
temp_absence_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(L04%in%c(0,1))%>%
  group_by(L04)%>%summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

#Worked in the past 12 Months 
worked_12months<-data_r2%>%filter(L05%in%c(0,1))%>%group_by(L05)%>%summarise(n=n())%>%mutate(mean=n/sum(n))
mean(data_r2$L05==1,na.rm=TRUE)
worked_12months_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(L05%in%c(0,1))%>%
  group_by(L05)%>%summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

#last payment 
last_payment<-data_r2%>%filter(L06!="NA")%>%group_by(L06)%>%summarise(freq=n())%>%mutate(mean=freq/sum(freq))%>%arrange(desc(mean))
last_payment_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(L06!="NA")%>%
  group_by(L06)%>%summarise(freq=survey_total())%>%mutate(mean_wt=freq/sum(freq))%>%arrange(desc(mean_wt))

#Currently looking for Job
searching_job<-data_r2%>%filter(L07%in%c(0,1))%>%group_by(L07)%>%summarise(n=n())%>%mutate(mean=n/sum(n))
mean(data_r2$L07==1,na.rm = TRUE)
searching_job_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(L07%in%c(0,1))%>%
  group_by(L07)%>%summarise(n=n())%>%mutate(mean_wt=n/sum(n))
#Why no looking for job
not_looking_for_job<-data_r2%>%filter(L08!="")%>%group_by(L08)%>%
  summarise(n=n())%>%mutate(mean=n/sum(n))
not_looking_for_job_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(L08!="")%>%group_by(L08)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

#Why not working currently 
not_working<-data_r2%>%filter(as.character(L09)!="")%>%group_by(L09)%>%
  summarise(freq=n())%>%mutate(mean=freq/sum(freq))
not_working_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(as.character(L09)!="")%>%group_by(L09)%>%
  summarise(freq=survey_total())%>%mutate(mean_wt=freq/sum(freq))

##Previous Job

#Job sector Past

job_sector_past<-data_r2%>%filter(L11%in%c(1,2,3))%>%group_by(L11)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),L11=case_when(L11==1~'Agriculture',L11==2~'Manufacturing',L11==3~'Services'))

job_sector_past_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(L11%in%c(1,2,3))%>%
  group_by(L11)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq),L11=case_when(L11==1~'Agriculture',L11==2~'Manufacturing',L11==3~'Services'))

#Job Category Past
job_categ_past<-data_r2%>%filter(L12%in%c(1,2))%>%group_by(L12)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),L12=case_when(L12==1~'wage worker',L12==2~'self-employed'))

job_categ_past_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(L12%in%c(1,2))%>%
  group_by(L12)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq),L12=case_when(L12==1~'wage worker',L12==2~'self-employed'))

##Current Job

#Job sector-current

job_sector_pres<-data_r2%>%filter(L14%in%c(1,2,3))%>%group_by(L14)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),L14=case_when(L14==1~'Agriculture',L14==2~'Manufacturing',L14==3~'Services'))
job_sector_pres_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(L14%in%c(1,2,3))%>%group_by(L14)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq),L14=case_when(L14==1~'Agriculture',L14==2~'Manufacturing',L14==3~'Services'))

#Job Category-current
job_categ_pres<-data_r2%>%filter(L15%in%c(1,2))%>%group_by(L15)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),L15=case_when(L15==1~'wage worker',L15==2~'self-employed'))

job_categ_pres_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(L15%in%c(1,2))%>%group_by(L15)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq),L15=case_when(L15==1~'wage worker',L15==2~'self-employed'))

## Wage worker

wage_worker<-data_r2%>%filter(L16%in%c(1,2,3,4,5,6))%>%group_by(L16)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),L16=case_when(L16==1~'Public org',
                                           L16==2~'Private firm',
                                           L16==3~'NGO',
                                           L16==4~'Individual emloyer',
                                           L16==5~'Household',
                                           L16==6~'No specific employer'))

wage_worker_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(L16%in%c(1,2,3,4,5,6))%>%group_by(L16)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq),L16=case_when(L16==1~'Public org',
                                           L16==2~'Private firm',
                                           L16==3~'NGO',
                                           L16==4~'Individual emloyer',
                                           L16==5~'Household',
                                           L16==6~'No specific employer'))

## Self-employed 

staff_size<-data_r2%>%summarise(mean=mean(as.numeric(L18,na.rm=TRUE)))
  
#Business revenue/earning 
var_list<-data_r2%>%select(L19a:L19d)%>%as.list()
func1<-function(x){
  mean(x==1,na.rm=TRUE)
}

increase<-sapply(var_list,func1)
func2<-function(x){
  mean(x==2,na.rm=TRUE)
}
decrease<-sapply(var_list,func2)

func3<-function(x){
  mean(x==3,na.rm=TRUE)
}


same<-sapply(var_list,func3)

self_employed_combined<-data.frame(rbind(increase,decrease,same))%>%
rownames_to_column(var="variable")

var_list_wt<-data_r2%>%select(L19a:L19d)%>%as.list()
func1_wt<-function(x){
  weighted.mean(x==1,na.rm=TRUE,data_r2$weight_2)
}

increase_wt<-sapply(var_list_wt,func1_wt)
func2_wt<-function(x){
  weighted.mean(x==2,na.rm=TRUE,data_r2$weight_2)
}
decrease_wt<-sapply(var_list_wt,func2_wt)

func3_wt<-function(x){
  weighted.mean(x==3,na.rm=TRUE,data_r2$weight_2)
}


same_wt<-sapply(var_list_wt,func3_wt)

self_employed_combined_wt<-data.frame(rbind(increase_wt,decrease_wt,same_wt))%>%
  rownames_to_column(var="variable")


##Labor Force Participation 
labor_participation<-data_r2%>%count(labor_participation)%>%mutate(mean=n/sum(n))
labor_participation_wt<-data_r2%>%as_survey(weights=c(hhw))%>%group_by(labor_participation)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))
#Labor force composition 
labor_composition<-data_r2%>%count(labor_composition)%>%mutate(mean=n/sum(n))
labor_composition_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(labor_composition)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))
labor_composition_hhw<-data_r2%>%as_survey(weights=c(hhw))%>%group_by(labor_composition)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

### Food safety  
#R2
food_safety_var<-data_r2%>%select(S01a:S02c)%>%as.list()

food_safety_func<-function(x){
  mean(x==1,na.rm=TRUE)
}
food_safety<-data.frame(mean=sapply(food_safety_var,food_safety_func))%>%
rownames_to_column(var="variable")

food_safety_var_wt<-data_r2%>%select(S01a:S02c)%>%as.list()

food_safety_func_wt<-function(x){
  weighted.mean(x==1,na.rm=TRUE, data_r2$weight_2)
}
food_safety_wt<-data.frame(mean_wt=sapply(food_safety_var_wt,food_safety_func_wt))%>%
  rownames_to_column(var="variable_wt")


#R1
food_safety_var_r1<-data_r1%>%select(v6_01c:v6_01d)%>%as.list()
food_safety_func_r1<-function(x){
  weighted.mean(x==1,na.rm=TRUE,data_r1$weight)
}
food_safety_r1_wt<-data.frame(mean=sapply(food_safety_var_r1,food_safety_func_r1)*mean(data_r1$v6_01b==1,na.rm = TRUE))%>%
  rownames_to_column(var = "variable")

food_safety_func_r1_cell<-function(x){
  weighted.mean(x==1,na.rm=TRUE,data_r1$weightcell)
}
food_safety_r1_wtcell<-data.frame(mean=sapply(food_safety_var_r1,food_safety_func_r1)*mean(data_r1$v6_01b==1,na.rm = TRUE))%>%
  rownames_to_column(var = "variable")

##disagregated by locality and labor composition
#Reduced consumption of preferred food 
reduced_cons_of_preferred_food<-data_r2%>%group_by(locality,labor_composition)%>%count(S01a)%>%
  mutate(mean=n/sum(n),S01a=ifelse(S01a==1,"Yes","No"))
#total and locality 
reduced_cons_of_preferred_food_total_loc_wt<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(locality,S01a)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),S01a=ifelse(S01a==1,"Yes","No"))
#total and labor composition 
reduced_cons_of_preferred_food_total_lc_wt<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(labor_composition,S01a)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),S01a=ifelse(S01a==1,"Yes","No"))
#total, labor composition and locality 
reduced_cons_of_preferred_food_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%
  group_by(locality,labor_composition,S01a)%>%summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),
                                                                                   S01a=ifelse(S01a==1,"Yes","No"))

#Borrowed food
borrowed_food_locality<-data_r2%>%group_by(locality)%>%count(S01b)%>%
  mutate(mean=n/sum(n))

#total and locality 
borrowed_food_total_locality_wt<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(locality,S01b)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),S01b=ifelse(S01b==1,"Yes","No"))
#total and labor composition 
borrowed_food_total_lc_wt<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(labor_composition,S01b)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),S01b=ifelse(S01b==1,"Yes","No"))
#total, labor composition and locality 
borrowed_food_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%
  group_by(locality,labor_composition,S01b)%>%summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),
                                                                                   S01b=ifelse(S01b==1,"Yes","No"))
#Eat smaller meal
eat_smaller_meal_locality<-data_r2%>%group_by(locality)%>%count(S01c)%>%
  mutate(mean=n/sum(n))

#total and locality 
eat_smaller_meal_total_locality_wt<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(locality,S01c)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),S01c=ifelse(S01c==1,"Yes","No"))
#total and labor composition 
eat_smaller_meal_total_lc_wt<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(labor_composition,S01c)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),S01c=ifelse(S01c==1,"Yes","No"))
#total, labor composition and locality 
eat_maller_meal_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%
  group_by(locality,labor_composition,S01c)%>%summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),
                                                                                   S01c=ifelse(S01c==1,"Yes","No"))

#Skipped meal 
skipped_meal_locality<-data_r2%>%group_by(locality)%>%count(S01d)%>%
  mutate(mean=n/sum(n))

#total and locality 
skipped_meal_total_locality_wt<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(locality,S01d)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),S01d=ifelse(S01d==1,"Yes","No"))
#total and labor composition 
skipped_meal_total_lc_wt<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(labor_composition,S01d)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),S01d=ifelse(S01d==1,"Yes","No"))
#total, labor composition and locality 
skipped_meal_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%
  group_by(locality,labor_composition,S01d)%>%summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),
                                                                                   S01d=ifelse(S01d==1,"Yes","No"))

#emergency expenses
emerg_exps_var<-data_r2%>%select(S03_1:S03_9999)%>%as.list()
emerg_exps_func<-function(y){
  mean(y==1,na.rm=TRUE)
}
emerg_exps<-data.frame(mean=sapply(emerg_exps_var,emerg_exps_func))%>%
rownames_to_column(var = "variable")

emerg_exps_var_wt<-data_r2%>%select(S03_1:S03_9999)%>%as.list()
emerg_exps_func_wt<-function(y){
  weighted.mean(y==1,na.rm=TRUE, data_r2$weight_2)
}
emerg_exps_wt<-data.frame(mean_wt=sapply(emerg_exps_var_wt,emerg_exps_func_wt))%>%
  rownames_to_column(var = "variable_wt")
#Current earning 
earning<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(labor_composition,S03_1)%>%
  summarise(n=survey_total())%>%mutate(mean_earning=n/sum(n),S03_1=ifelse(S03_1==1,"Yes","No"))

#by labor composition _ own saving 
own_saving<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(labor_composition,S03_2)%>%
  summarise(n=survey_total())%>%mutate(mean_saving=n/sum(n),S03_2=ifelse(S03_2==1,"Yes","No"))
#by labor composition _ relative with out interest
relative<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(labor_composition,S03_4)%>%
  summarise(n=survey_total())%>%mutate(mean_relative=n/sum(n),S03_4=ifelse(S03_4==1,"Yes","No"))
#by labor composition _ borrow
borrow<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(labor_composition,S03_5)%>%
  summarise(n=survey_total())%>%mutate(mean_borrow=n/sum(n),S03_5=ifelse(S03_5==1,"Yes","No"))
#money_lender
borrow_ml<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(labor_composition,S03_7)%>%
  summarise(n=survey_total())%>%mutate(mean_borrow=n/sum(n),S03_7=ifelse(S03_7==1,"Yes","No"))
#by labor composition _ no coping 
no_coping_strategy<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(labor_composition,S03_14)%>%
  summarise(n=survey_total())%>%mutate(mean_no_coping=n/sum(n),S03_14=ifelse(S03_14==1,"Yes","No"))
#by labor composition _ other
other<-data_r2%>%as_survey(weight=c(weight_2))%>%group_by(labor_composition,S03_7777)%>%
  summarise(n=survey_total())%>%mutate(mean_other=n/sum(n),S03_7777=ifelse(S03_7777==1,"Yes","No"))



#assistance
receiving_assistance<-data_r2%>%filter(S04%in%c(0,1))%>%group_by(S04)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),S04=ifelse(S04==1,"Yes","No"))
receiving_assistance_wt<-data_r2%>%as_survey(weight=c(weight_2))%>%filter(S04%in%c(0,1))%>%group_by(S04)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq),S04=ifelse(S04==1,"Yes","No"))
#locality 
receiving_assist_locality_wt<-data_r2%>%as_survey(weight=c(weight_2))%>%filter(S04%in%c(0,1))%>%group_by(locality,S04)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq),S04=ifelse(S04==1,"Yes","No"))

#Type of assistance received 
assist_var<-data_r2%>%select(S05_1:S05_9999)%>%as.list()
assisst_func<-function(x){
  mean(x==1,na.rm=TRUE)
}
assistance_received<-data.frame(mean=sapply(assist_var,assisst_func))%>%
rownames_to_column(var = "variable")

assist_var_wt<-data_r2%>%select(S05_1:S05_9999)%>%as.list()
assisst_func_wt<-function(x){
  weighted.mean(x==1,na.rm=TRUE, data_r2$weight_2)
}
assistance_received_wt<-data.frame(mean_wt=sapply(assist_var_wt,assisst_func_wt)*0.09)%>%
  rownames_to_column(var = "variable_wt")
#Cash Support 
cash_support_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(S05_1)%>%
  summarise(n=survey_total())%>%mutate(mean_cash_wt=n/sum(n))
  
cash_support_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(locality,S05_1)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

#food bank
food_bank_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(S05_3)%>%
  summarise(n=survey_total())%>%mutate(mean_cash_wt=n/sum(n))

food_bank_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(locality,S05_3)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

#support Assistance 

food_assist_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(S05_4)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

food_assist_locality_wt<-data_r2%>%as_survey(weights=c(hhw))%>%group_by(locality,S05_4)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

food_assist_locality<-data_r2%>%group_by(locality,S05_4)%>%
  summarise(n=n())%>%mutate(mean=n/sum(n))

# Assistance preferred 
assist_needed_var<-data_r2%>%select(S06_1:S06_9999)%>%as.list()
assist_needed_func<-function(x){
  mean(x==1, na.rm=TRUE)
}
assistance_preferred<-data.frame(mean=sapply(assist_needed_var,assist_needed_func))%>%
rownames_to_column(var="variable")

assist_needed_var_wt<-data_r2%>%select(S06_1:S06_9999)%>%as.list()
assist_needed_func_wt<-function(x){
  weighted.mean(x==1, na.rm=TRUE,data_r2$weight_2)
}
assistance_preferred_wt<-data.frame(mean_wt=sapply(assist_needed_var_wt,assist_needed_func_wt))%>%
  rownames_to_column(var="variable")
#Cash transferred 
cash_trans_preferred_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(S06_1)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

cash_trans_preferred_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(locality,S06_1)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))
#Work Job program 
work_program_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(S06_2)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

work_program_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(locality,S06_2)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))
#small business support
business_support_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(locality,S06_3)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))


#Food distribution 
food_dist_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(S06_4)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

food_dist_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(locality,S06_4)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))


#Distribution of Medicien
med_dist_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(locality,S06_5)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

#Health check-up
health_check_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(locality,S06_10)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

#Rent relief 
rent_relief_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(locality,S06_11)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

# familiarity with mobile money
familiarity<-data_r2%>%filter(S07%in%c(0,1))%>%count(S07)%>%mutate(mean=n/sum(n))
familiarity_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(S07%in%c(0,1))%>%
  group_by(S07)%>%summarise(n=survey_total())%>%mutate(meat_wt=n/sum(n))
used<-data_r2%>%filter(S07a!="")%>%count(S07a)%>%mutate(percent=n/sum(n))

used_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(S07a!="")%>%group_by(S07a)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))
mobile_money_combined<-cbind(familiarity,familiarity_wt, used,used_wt)

#Use mobile money
mobile_money_var<-data_r2%>%select(S07b_1:S07b_9999)%>%as.list()
mobile_money_func<-function(x){
  mean(x==1,na.rm=TRUE)
}
use_mobile_money<-data.frame(mean=sapply(mobile_money_var,mobile_money_func))%>%
rownames_to_column(var = "variable")

mobile_money_var_wt<-data_r2%>%select(S07b_1:S07b_9999)%>%as.list()
mobile_money_func_wt<-function(x){
  weighted.mean(x==1,na.rm=TRUE,data_r2$weight_2)
}
use_mobile_money_wt<-data.frame(mean_wt=sapply(mobile_money_var_wt,mobile_money_func_wt))%>%
  rownames_to_column(var = "variable_wt")

# Don't use mobile money 
no_mobile_money_var<-data_r2%>%select(S07c_1:S07c_9999)%>%as.list()
no_mobile_money_func<-function(x){
  mean(x==1,na.rm=TRUE)
}
no_mobile_money<-data.frame(mean=sapply(no_mobile_money_var,no_mobile_money_func))%>%
rownames_to_column(var = "variable")

no_mobile_money_var_wt<-data_r2%>%select(S07c_1:S07c_9999)%>%as.list()
no_mobile_money_func_wt<-function(x){
  weighted.mean(x==1,na.rm=TRUE, data_r2$weight_2)
}
no_mobile_money_wt<-data.frame(mean_wt=sapply(no_mobile_money_var_wt,no_mobile_money_func_wt))%>%
  rownames_to_column(var = "variable")


### Health

services_needed<-data_r2%>%count(H01)%>%mutate(percent=n/sum(n),H01=ifelse(H01==1,"Yes","No"))
services_needed_wt<-data_r2%>%
  as_survey(weights=c(weight_2))%>%group_by(H01)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),H01=ifelse(H01==1,"Yes","No"))
#locality
services_needed_locality_wt<-data_r2%>%
  as_survey(weights=c(weight_2))%>%group_by(locality, H01)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),H01=ifelse(H01==1,"Yes","No"))
#Labor composition 
services_needed_lc_wt<-data_r2%>%
  as_survey(weights=c(weight_2))%>%group_by(labor_composition, H01)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),H01=ifelse(H01==1,"Yes","No"))
#Labor composition and locality 
services_needed_lc_local_wt<-data_r2%>%
  as_survey(weights=c(weight_2))%>%group_by(labor_composition, locality, H01)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),H01=ifelse(H01==1,"Yes","No"))

#Reasons demanding health services 
reason_var<-data_r2%>%select(H02_1:H02_9999)%>%as.list()
reason_func<-function(x){
  mean(x==1, na.rm=TRUE)
}
reason<-data.frame(mean=sapply(reason_var,reason_func))%>%
rownames_to_column(var = "variable")

## Number of Reason 
reason_count<-data_r2%>%filter(Health_Followup_Questions_count%in%c(1:3))%>%
  group_by(Health_Followup_Questions_count)%>%
  summarise(freq=n())%>%mutate(percent=freq/sum(freq))

#first reason
reason1<-data_r2%>%filter(i_1%in%seq(1:10))%>%group_by(i_1)%>%summarise(freq=n())%>%
  mutate(percent=freq/sum(freq))
reason1_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(i_1%in%seq(1:10))%>%
  group_by(i_1)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq))


# Access

access1<-data_r2%>%filter(H03_1%in%c(0,1))%>%group_by(H03_1)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq))
access1_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(H03_1%in%c(0,1))%>%
  group_by(H03_1)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq))
#Locality
access1_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(H03_1%in%c(0,1))%>%
  group_by(locality, H03_1)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq))

#Labor Composition 
access1_labor_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(H03_1%in%c(0,1))%>%
  group_by(labor_composition, H03_1)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq))

#Locality and labor composition 
access1_locality_labor_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(H03_1%in%c(0,1))%>%
  group_by(locality,labor_composition, H03_1)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq))


# Reason 1 unable to access to medical services
reason1_no_access<-data_r2%>%filter(H04_1%in%seq(1:7777))%>%group_by(H04_1)%>%
  summarise(freq=n())%>%mutate(mean=freq/sum(freq))
reason1_no_access_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(H04_1%in%seq(1:7777))%>%
  group_by(H04_1)%>%summarise(freq=survey_total())%>%mutate(mean_wt=freq/sum(freq))

# service providers
service_provider1<-data_r2%>%filter(as.character(H05_1)!="NA")%>%group_by(H05_1)%>%
  summarise(freq=n())%>%mutate(mean=freq/sum(freq))

service_provider1_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(as.character(H05_1)!="NA")%>%
  group_by(H05_1)%>%summarise(freq=survey_total())%>%mutate(mean_wt=freq/sum(freq))

# Treatment cost1
treatment_cost<-data_r2%>%filter(as.character(H06_1)!="NA")%>%group_by(H06_1)%>%
  summarise(freq=n())%>%mutate(mean=freq/sum(freq))
treatment_cost_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(as.character(H06_1)!="NA")%>%
  group_by(H06_1)%>%summarise(freq=survey_total())%>%mutate(mean_wt=freq/sum(freq))

### Education 
# children with school going age

kids_5_18<-data_r2%>%filter(E01%in%c(0,1))%>%group_by(E01)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq))
going_to_school<-data_r2%>%filter(E02%in%c(0,1))%>%group_by(E02)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq))
school<-cbind(kids_5_18,going_to_school)

kids_5_18_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E01%in%c(0,1))%>%group_by(E01)%>%
  summarise(freq=survey_total())%>%mutate(mean_wt=freq/sum(freq))
going_to_school_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E02%in%c(0,1))%>%
  group_by(E02)%>%summarise(freq=survey_total())%>%mutate(mean_wt=freq/sum(freq))
school_wt<-cbind(kids_5_18_wt,going_to_school_wt)

#school reopening
school_reopen<-data_r2%>%filter(E03%in%c(0,1,2))%>%group_by(E03)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),E03=case_when(E03==0~'No, schools are fully closed',
                                            E03==1~'Yes, schools have reopened fully',
                                            E03==2~'Yes, schools have reopened, but not fully'))
school_reopen_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E03%in%c(0,1,2))%>%group_by(E03)%>%
  summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq),E03=case_when(E03==0~'No, schools are fully closed',
                                           E03==1~'Yes, schools have reopened fully',
                                           E03==2~'Yes, schools have reopened, but not fully'))
# Kids going back to school 
kids_going_to_school<-data_r2%>%filter(E04%in%c(0,1,2))%>%group_by(E04)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),E04=case_when(E04==1~'Yes, all',E04==2~'Yes, some of them',E04==0~'No'))

kids_going_to_school_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E04%in%c(0,1,2))%>%
  group_by(E04)%>%summarise(freq=survey_total())%>%
  mutate(mean_wt=freq/sum(freq),E04=case_when(E04==1~'Yes, all',E04==2~'Yes, some of them',E04==0~'No'))

all_kids_going_to_school<-data_r2%>%filter(E05%in%c(0,1,2))%>%group_by(E05)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),E05=case_when(E05==1~'Yes, all',E05==2~'No, some', E05==0~'No'))

all_kids_going_to_school_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E05%in%c(0,1,2))%>%
  group_by(E05)%>%summarise(freq=n())%>%
  mutate(mean=freq/sum(freq),E05=case_when(E05==1~'Yes, all',E05==2~'No, some', E05==0~'No'))

school_going<-cbind(school_reopen,kids_going_to_school,all_kids_going_to_school)
school_going_wt<-cbind(school_reopen_wt,kids_going_to_school_wt,all_kids_going_to_school_wt)

# Reason not going to school
not_going_to_school<-data_r2%>%filter(as.character(E06)!="NA")%>%group_by(E06)%>%
  summarise(freq=n())%>%mutate(percent=freq/sum(freq))


#Learning activities 
engaged_in_learning<-data_r2%>%filter(E07%in%c(0,1))%>%group_by(E07)%>%summarise(freq=n())%>%
  mutate(percent=freq/sum(freq))


learning_activity_var<-data_r2%>%select(E08_1:E08_9999)%>%as.list()
learning_activity_func<-function(x){
  mean(x==1,na.rm=TRUE)
}
learning_activity<-data.frame(mean=sapply(learning_activity_var,learning_activity_func))%>%
rownames_to_column(var = "variable")
## Disagregated by Urban-rural and region  
school_kids_locality<-data_r2%>%group_by(locality)%>%count(E01)%>%
  mutate(mean=n/sum(n))
school_kids_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(locality,E01)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))
school_kids_wt_labor_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(labor_composition,E01)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

school_kids_labor_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(locality,labor_composition,E01)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

school_kids_region_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%group_by(Region,E01)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))


## School closure 
#locality
going_to_school_locality<-data_r2%>%filter(E02%in%c(0,1))%>%group_by(locality)%>%
  count(E02)%>%mutate(mean=n/sum(n),E02=case_when(E02==1~'Yes',E02==0~'No'))

going_to_school_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E02%in%c(0,1))%>%group_by(locality,E02)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),E02=case_when(E02==1~'Yes',E02==0~'No'))
#labor
going_to_school_labor_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E02%in%c(0,1))%>%group_by(labor_composition,E02)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),E02=case_when(E02==1~'Yes',E02==0~'No'))
#labor and locality 
going_to_school_locality_labor_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E02%in%c(0,1))%>%group_by(locality,labor_composition,E02)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),E02=case_when(E02==1~'Yes',E02==0~'No'))

#Region
going_to_school_region<-data_r2%>%filter(E02%in%c(0,1))%>%group_by(Region)%>%
  count(E02)%>%mutate(mean=n/sum(n),E02=case_when(E02==1~'Yes',E02==0~'No'))

going_to_school_region_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E02%in%c(0,1))%>%group_by(Region,E02)%>%
  summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n),E02=case_when(E02==1~'Yes',E02==0~'No'))

## School reopening 
#locality 
school_reopening_locality<-data_r2%>%filter(E03%in%c(0,1,2))%>%group_by(locality)%>%count(E03)%>%
  mutate(mean=n/sum(n),E03=case_when(E03==0~'No, schools are fully closed',
                                      E03==1~'Yes, schools have reopened fully',
                                      E03==2~'Yes, schools have reopened, but not fully'))
school_reopening_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E03%in%c(0,1,2))%>%
  group_by(locality,E03)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),E03=case_when(E03==0~'No, schools are fully closed',
                                     E03==1~'Yes, schools have reopened fully',
                                     E03==2~'Yes, schools have reopened, but not fully'))

#Region
school_reopening_region<-data_r2%>%filter(E03%in%c(0,1,2))%>%group_by(Region)%>%count(E03)%>%
  mutate(mean=n/sum(n),E03=case_when(E03==0~'No, schools are fully closed',
                                      E03==1~'Yes, schools have reopened fully',
                                      E03==2~'Yes, schools have reopened, but not fully'))

school_reopening_region_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E03%in%c(0,1,2))%>%
  group_by(Region,E03)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),E03=case_when(E03==0~'No, schools are fully closed',
                                      E03==1~'Yes, schools have reopened fully',
                                      E03==2~'Yes, schools have reopened, but not fully'))

## Going back to school 
#locality 
going_back_to_school_locality<-data_r2%>%filter(E04%in%c(0,1,2))%>%group_by(locality)%>%count(E04)%>%
  mutate(mean=n/sum(n),E04=case_when(E04==1~'Yes, all',E04==2~'Yes, some of them',E04==0~'No'))

going_back_to_school_locality_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E04%in%c(0,1,2))%>%
  group_by(locality,E04)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),E04=case_when(E04==1~'Yes, all',E04==2~'Yes, some of them',E04==0~'No'))
write.csv(going_back_to_school_locality_wt,"C:/Users/Documents/Survey/Analysis/back_to_school_region.csv")

#labor 
going_back_to_school_labor_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E04%in%c(0,1,2))%>%
  group_by(labor_composition,E04)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),E04=case_when(E04==1~'Yes, all',E04==2~'Yes, some of them',E04==0~'No'))

#Region
going_back_to_school_region<-data_r2%>%filter(E04%in%c(0,1,2))%>%group_by(Region)%>%count(E04)%>%
  mutate(mean=n/sum(n),E04=case_when(E04==1~'Yes, all',E04==2~'Yes, some of them',E04==0~'No'))

going_back_to_school_region_wt<-data_r2%>%as_survey(weights=c(weight_2))%>%filter(E04%in%c(0,1,2))%>%
  group_by(Region,E04)%>%summarise(n=survey_total())%>%
  mutate(mean_wt=n/sum(n),E04=case_when(E04==1~'Yes, all',E04==2~'Yes, some of them',E04==0~'No'))

#### Analysis on the combined data data set 
#unweighted 
assistance_received_r1<-data_merged%>%count(v6_06)%>%mutate(mean=n/sum(n))
assistance_received_r2<-data_merged%>%count(S04)%>%mutate(mean=n/sum(n))
#Weighted 
assistance_received_r1_wt<-data_merged%>%as_survey(weights=c(weight))%>%
  group_by(v6_06)%>%summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))
assistance_received_r2_wt<-data_merged%>%as_survey(weights=c(weight_2))%>%
  group_by(S04)%>%summarise(n=survey_total())%>%mutate(mean_wt=n/sum(n))

### Exporting Table
output_func1<-function(x){
  write.table(data.frame(x),'C:/Users/result/tables/tables_r2.csv', row.names = FALSE, append = TRUE,sep=",")
}
vars_list1<-list(gender,gender_wt,gender_hhw, age, age_wt, educ, educ_wt, province,province_wt, district,district_wt, reason_moving,family_income_combined, worked,
                worked_wt,temp_absence,temp_absence_wt)
lapply(vars_list1,output_func1)
### Checking the association 
#Association between employment type and food security 
GTest(data_r2$labor_composition,data_r2$S01a)
GTest(data_r2$labor_composition,data_r2$S01b)
GTest(data_r2$labor_composition,data_r2$S01c)
GTest(data_r2$labor_composition,data_r2$S01d)
#Association between employment type and demand for medical attention 

chisq.test(data_r2$labor_composition,data_r2$H01)
#Association between employment type and access to health services 
chisq.test(data_r2$labor_composition,data_r2$H03_1)
#Association between food security and need for medical attention 
GTest(data_r2$H01,data_r2$S01a)
GTest(data_r2$H01,data_r2$S01b)
GTest(data_r2$H01,data_r2$S01c)
GTest(data_r2$H01,data_r2$S01d)

#Association between food security and access to medical attention 
GTest(data_r2$H03_1,data_r2$S01a)
GTest(data_r2$H03_1,data_r2$S01b)
GTest(data_r2$H03_1,data_r2$S01c)
GTest(data_r2$H03_1,data_r2$S01d)
#Association between locality and the need for and access to medical attention 
chisq.test(data_r2$locality, data_r2$H01)
chisq.test(data_r2$locality, data_r2$H03_1) 

#Education 
## Association between locality and school closure, re-opening and kids going back to school
chisq.test(data_r2$locality, data_r2$E01)
chisq.test(data_r2$locality, data_r2$E02)
GTest(data_r2$locality, data_r2$E03)
GTest(data_r2$locality, data_r2$E04)
## Association between region and school closure, re-opening and kids going back to school
chisq.test(data_r2$Region, data_r2$E01)
chisq.test(data_r2$Region, data_r2$E02)
GTest(data_r2$Region, data_r2$E03)
GTest(data_r2$Region, data_r2$E04)
## Association between employment type and school closure, re-opening and kids going back to school
chisq.test(data_r2$labor_composition, data_r2$E01)
chisq.test(data_r2$labor_composition, data_r2$E02)
GTest(data_r2$labor_composition, data_r2$E03)
GTest(data_r2$labor_composition, data_r2$E04)


