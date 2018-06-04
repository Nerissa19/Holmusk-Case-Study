setwd("C:/Users/Baihui/Desktop/Holmusk case")
library(gdata)
library(zoo)
library(survival)
library(survminer)

# First we need to clean and tidy the data
# read the csv to dataframe file and join the data together for analysis further
dmgrph <- read.csv('demographics.csv', header=TRUE)
bill_amount <- read.csv('bill_amount.csv',header=TRUE)
bill_id <-read.csv('bill_id.csv',header=TRUE)
clinical_data <- read.csv('clinical_data.csv',header=TRUE)
bill_amount$bill_id = as.character(bill_amount$bill_id)
bill_id$bill_id <- as.character(bill_id$bill_id)
bill_id$patient_id <- as.character(bill_id$patient_id)
attach(clinical_data)
bill_id$date_of_admission <- as.character(bill_id$date_of_admission)
clinical_data$id <- as.character(clinical_data$id)
clinical_data$date_of_admission <- as.character(clinical_data$date_of_admission)
clinical_data$date_of_discharge <- as.character(clinical_data$date_of_discharge)
clinical_data$medical_history_1 <- as.character(clinical_data$medical_history_1)
clinical_data$medical_history_2<-as.character(medical_history_2)
clinical_data$medical_history_3<-as.character(medical_history_3)
clinical_data$medical_history_4<-as.character(medical_history_4)
clinical_data$medical_history_5<-as.character(medical_history_5)
clinical_data$medical_history_6<-as.character(medical_history_6)
clinical_data$medical_history_7<-as.character(medical_history_7)
detach(clinical_data)
attach(dmgrph)
dmgrph$patient_id <- as.character(patient_id)
dmgrph$gender <- as.character(gender)
dmgrph$race <- as.character(race)
dmgrph$resident_status <- as.character((resident_status))
dmgrph$date_of_birth <- as.character(date_of_birth)
detach(dmgrph)
Holmusk <- matrix(rep('',13600*30),13600,32)
Holmusk <- as.data.frame(Holmusk)
names(Holmusk) <- c('bill_id','patient_id','bill_amount',"date_of_admission",'date_of_discharge'
                    ,'medical_history_1','medical_history_2','medical_history_3','medical_history_4',
                    'medical_history_5','medical_history_6','medical_history_7','preop_medication_1',
                    'preop_medication_2','preop_medication_3','preop_medication_4','preop_medication_5'
                    ,'preop_medication_6','symptom_1','symptom_2','symptom_3','symptom_4','symptom_5',
                    'lab_result_1','lab_result_2','lab_result_3','weight','height','gender','race',
                    'resident_status','date_of_birth')
Holmusk$bill_id <- bill_id$bill_id
Holmusk$patient_id <- bill_id$patient_id
Holmusk$date_of_admission <- bill_id$date_of_admission
for(i in 1:nrow(Holmusk)) {
  for(j in 1:nrow(bill_amount)){
     if(Holmusk$bill_id[i]==bill_amount$bill_id[j]) {
       Holmusk$bill_amount[i] <- bill_amount$amount[j] 
     }
  }
}
attach(Holmusk)

Holmusk$date_of_discharge <- as.character(Holmusk$date_of_discharge)
Holmusk$medical_history_1 <- as.character(Holmusk$medical_history_1)
Holmusk$medical_history_2<-as.character(medical_history_2)
Holmusk$medical_history_3<-as.character(medical_history_3)
Holmusk$medical_history_4<-as.character(medical_history_4)
Holmusk$medical_history_5<-as.character(medical_history_5)
Holmusk$medical_history_6<-as.character(medical_history_6)
Holmusk$medical_history_7<-as.character(medical_history_7)
Holmusk$preop_medication_1<-as.integer(preop_medication_1)
Holmusk$preop_medication_2<-as.integer(preop_medication_2)
Holmusk$preop_medication_3<-as.integer(preop_medication_3)
Holmusk$preop_medication_4<-as.integer(preop_medication_4)
Holmusk$preop_medication_5<-as.integer(preop_medication_5)
Holmusk$preop_medication_6<-as.integer(preop_medication_6)
Holmusk$symptom_1<-as.integer(symptom_1)
Holmusk$symptom_2<-as.integer(symptom_2)
Holmusk$symptom_3<-as.integer(symptom_3)
Holmusk$symptom_4<-as.integer(symptom_4)
Holmusk$symptom_5<-as.integer(symptom_5)
Holmusk$lab_result_1<-as.numeric(lab_result_1)
Holmusk$lab_result_2<-as.numeric(lab_result_2)
Holmusk$lab_result_3<-as.numeric(lab_result_3)
Holmusk$weight<-as.numeric(weight)
Holmusk$height<-as.numeric(height)
Holmusk$gender<-as.character(gender)
Holmusk$race<-as.character(race)
Holmusk$resident_status<-as.character(resident_status)
Holmusk$date_of_birth<-as.character(date_of_birth)
detach(Holmusk)

clinical_data=clinical_data[complete.cases(clinical_data), ]
attach(clinical_data)
for(i in 1:nrow(Holmusk)) {
    j = 1
    while((id[j]!=Holmusk$patient_id[i]&date_of_admission[j]!=Holmusk$date_of_admission[i])&j<=nrow(clinical_data)) {
      j = j+1
    }
    if(j <= nrow(clinical_data)){
    Holmusk$date_of_discharge[i]=date_of_discharge[j]
    Holmusk$medical_history_1[i]=medical_history_1[j]
    Holmusk$medical_history_2[i]=medical_history_2[j]
    Holmusk$medical_history_3[i]=medical_history_3[j]
    Holmusk$medical_history_4[i]=medical_history_4[j]
    Holmusk$medical_history_5[i]=medical_history_5[j]
    Holmusk$medical_history_6[i]=medical_history_6[j]
    Holmusk$medical_history_7[i]=medical_history_7[j]
    Holmusk$preop_medication_1[i]=clinical_data$preop_medication_1[j]
    Holmusk$preop_medication_2[i]=clinical_data$preop_medication_2[j]
    Holmusk$preop_medication_3[i]=clinical_data$preop_medication_3[j]
    Holmusk$preop_medication_4[i]=clinical_data$preop_medication_4[j]
    Holmusk$preop_medication_5[i]=clinical_data$preop_medication_5[j]
    Holmusk$preop_medication_6[i]=clinical_data$preop_medication_6[j]
    Holmusk$symptom_1[i]=clinical_data$symptom_1[j]
    Holmusk$symptom_2[i]=clinical_data$symptom_2[j]
    Holmusk$symptom_3[i]=clinical_data$symptom_3[j]
    Holmusk$symptom_4[i]=clinical_data$symptom_4[j]
    Holmusk$symptom_5[i]=clinical_data$symptom_5[j]
    Holmusk$lab_result_1[i]=clinical_data$lab_result_1[j]
    Holmusk$lab_result_2[i]=clinical_data$lab_result_2[j]
    Holmusk$lab_result_3[i]=clinical_data$lab_result_3[j]
    Holmusk$weight[i]=clinical_data$weight[j]
    Holmusk$height[i]=clinical_data$height[j]

    }
    else {
      Holmusk$date_of_discharge[i]=NA
      Holmusk$medical_history_1[i]=NA
      Holmusk$medical_history_2[i]=NA
      Holmusk$medical_history_3[i]=NA
      Holmusk$medical_history_4[i]=NA
      Holmusk$medical_history_5[i]=NA
      Holmusk$medical_history_6[i]=NA
      Holmusk$medical_history_7[i]=NA
      Holmusk$preop_medication_1[i]=NA
      Holmusk$preop_medication_2[i]=NA
      Holmusk$preop_medication_3[i]=NA
      Holmusk$preop_medication_4[i]=NA
      Holmusk$preop_medication_5[i]=NA
      Holmusk$preop_medication_6[i]=NA
      Holmusk$symptom_1[i]=NA
      Holmusk$symptom_2[i]=NA
      Holmusk$symptom_3[i]=NA
      Holmusk$symptom_4[i]=NA
      Holmusk$symptom_5[i]=NA
      Holmusk$lab_result_1[i]=NA
      Holmusk$lab_result_2[i]=NA
      Holmusk$lab_result_3[i]=NA
      Holmusk$weight[i]=NA
      Holmusk$height[i]=NA
    }
}
detach(clinical_data)
for(i in 1:nrow(Holmusk)) {
  j = 1
  while((dmgrph$patient_id[j]!=Holmusk$patient_id[i])&j<=nrow(dmgrph)) {
    j = j+1
  }
  if(j <= nrow(dmgrph)){
      Holmusk$gender[i]=dmgrph$gender[j]
      Holmusk$race[i]=dmgrph$race[j]
      Holmusk$resident_status[i]=dmgrph$resident_status[j]
      Holmusk$date_of_birth[i]=dmgrph$date_of_birth[j]
  }
  else {
    Holmusk$gender[i]=NA
    Holmusk$race[i]=NA
    Holmusk$resident_status[i]=NA
    Holmusk$date_of_birth[i]=NA
  }
  }
}
Holmusk=Holmusk[complete.cases(Holmusk), ]
# now all the original has been joined together with no missing values
# to analyze the driver of care cost, we calculate the age of patient on the date of admission
# also we calculate the duration of the care
# for the medical history, some entries are 'yes/no' and some are '1/0', to be consistent, we use '1/0' for all
# race and resident_status also have some typo and we need to correct them.

for(i in 1:nrow(Holmusk)) {
  if(Holmusk$medical_history_1[i]=='Yes'){Holmusk$medical_history_1[i]<-'1'} else if(Holmusk$medical_history_1[i]=='No') {Holmusk$medical_history_1[i]<-'0'}
  if(Holmusk$medical_history_2[i]=='Yes'){Holmusk$medical_history_2[i]<-'1'} else if(Holmusk$medical_history_2[i]=='No') {Holmusk$medical_history_2[i]<-'0'}
  if(Holmusk$medical_history_3[i]=='Yes'){Holmusk$medical_history_3[i]<-'1'} else if(Holmusk$medical_history_3[i]=='No') {Holmusk$medical_history_3[i]<-'0'}
  if(Holmusk$medical_history_4[i]=='Yes'){Holmusk$medical_history_4[i]<-'1'} else if(Holmusk$medical_history_4[i]=='No') {Holmusk$medical_history_4[i]<-'0'}
  if(Holmusk$medical_history_5[i]=='Yes'){Holmusk$medical_history_5[i]<-'1'} else if(Holmusk$medical_history_5[i]=='No') {Holmusk$medical_history_5[i]<-'0'}
  if(Holmusk$medical_history_6[i]=='Yes'){Holmusk$medical_history_6[i]<-'1'} else if(Holmusk$medical_history_6[i]=='No') {Holmusk$medical_history_6[i]<-'0'}
  if(Holmusk$medical_history_7[i]=='Yes'){Holmusk$medical_history_7[i]<-'1'} else if(Holmusk$medical_history_7[i]=='No') {Holmusk$medical_history_7[i]<-'0'}
  if(Holmusk$gender[i]=='f') {Holmusk$gender[i]<-'Female'}
  else if(Holmusk$gender[i]=='m') {Holmusk$gender[i]<-'Male'}
}
for(i in 1:nrow(Holmusk)) {
  if(Holmusk$race[i]=='chinese') {Holmusk$race[i]='Chinese'}
  if(Holmusk$race[i]=='india') {Holmusk$race[i]='Indian'}
  if(Holmusk$race[i]=='indian') {Holmusk$race[i]='Indian'}
  if(Holmusk$race[i]=='India') {Holmusk$race[i]='Indian'}
  if(Holmusk$race[i]=='malay') {Holmusk$race[i]='Malay'}
  if(Holmusk$race[i]=='others') {Holmusk$race[i]='Others'}
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:nrow(Holmusk)) {
  Holmusk$Age[i]=as.numeric(substrRight(Holmusk$date_of_admission[i],4))-as.numeric(substrRight(Holmusk$date_of_birth[i],4))
}
for(i in 1:nrow(Holmusk)) {
  Holmusk$Duration[i]=as.numeric(difftime(as.Date(Holmusk$date_of_discharge[i]),as.Date(Holmusk$date_of_admission[i],'%d/%m/%Y'),units = 'days'))
}

# As we have duration and age, date of birth, admission, and discharge are not important so we delete these columns
# In case, we make a copy of Holmusk
Final_Data <- Holmusk
Final_Data$date_of_admission<-NULL
Final_Data$date_of_birth<-NULL
Final_Data$date_of_discharge<-NULL
remove('bill_amount','bill_id','clinical_data','dmgrph')
# Now we have all the information that we need in this analysis
# The variables are composed of categorical and numerical varialbes, we use t test, anova analysis and linear regression model to find the drivers of cost

attach(Final_Data)
# We first analyzed categorical variable, they are medical history, preop medication,
# symptom, gender, race, resident status and age group (we group age into 5 groups)
agegpname <- c('below 30','31-40','41-50','51-60','61-70','above 70')
agegp <- rep(0,nrow(Final_Data))
for(i in 1:nrow(Final_Data)){
  if(Age[i]<31){agegp[i]=agegpname[1]}
  else if(Age[i]<41){agegp[i]=agegpname[2]}
  else if(Age[i]<51){agegp[i]=agegpname[3]}
  else if(Age[i]<61){agegp[i]=agegpname[4]}
  else if(Age[i]<71){agegp[i]=agegpname[5]}
  else {agegp[i]=agegpname[6]}
}
Final_Data$Age=agegp
Final_Data$preop_medication_1<-as.character(preop_medication_1)
Final_Data$preop_medication_2<-as.character(preop_medication_2)
Final_Data$preop_medication_3<-as.character(preop_medication_3)
Final_Data$preop_medication_4<-as.character(preop_medication_4)
Final_Data$preop_medication_5<-as.character(preop_medication_5)
Final_Data$preop_medication_6<-as.character(preop_medication_6)
Final_Data$symptom_1<-as.character(symptom_1)
Final_Data$symptom_2<-as.character(symptom_2)
Final_Data$symptom_3<-as.character(symptom_3)
Final_Data$symptom_4<-as.character(symptom_4)
Final_Data$symptom_5<-as.character(symptom_5)

CostMale <- bill_amount[gender=='Male']
CostFemale <- bill_amount[gender=='Female']
var.test(CostMale,CostFemale)
# reject H0, variances are different
t.test(CostMale,CostFemale,var.equal = FALSE)
# P-value > 0.01, do not reject the null hypothesis, indicating there is no significant difference
# between male and female in respect to bill_amount

MH1Y<-bill_amount[medical_history_1=='1']
MH2Y<-bill_amount[medical_history_2=='1']
MH3Y<-bill_amount[medical_history_3=='1']
MH4Y<-bill_amount[medical_history_4=='1']
MH5Y<-bill_amount[medical_history_5=='1']
MH6Y<-bill_amount[medical_history_6=='1']
MH7Y<-bill_amount[medical_history_7=='1']
MH1N<-bill_amount[medical_history_1=='0']
MH2N<-bill_amount[medical_history_2=='0']
MH3N<-bill_amount[medical_history_3=='0']
MH4N<-bill_amount[medical_history_4=='0']
MH5N<-bill_amount[medical_history_5=='0']
MH6N<-bill_amount[medical_history_6=='0']
MH7N<-bill_amount[medical_history_7=='0']
var.test(MH1Y,MH1N) # no difference in variance
var.test(MH2Y,MH2N) # difference
var.test(MH3Y,MH3N) # no difference
var.test(MH4Y,MH4N) # no difference
var.test(MH5Y,MH5N) # no difference
var.test(MH6Y,MH6N) # no difference
var.test(MH7Y,MH7N) # no difference
t.test(MH1Y,MH1N) # significant
t.test(MH2Y,MH2N,var.equal = FALSE) # not significant
t.test(MH3Y,MH3N) # not significant
t.test(MH4Y,MH4N) # not significant
t.test(MH5Y,MH5N) # not significant
t.test(MH6Y,MH6N) # not significant
t.test(MH7Y,MH7N) # not significant

pre1Y = bill_amount[preop_medication_1=='1']
pre2Y = bill_amount[preop_medication_2=='1']
pre3Y = bill_amount[preop_medication_3=='1']
pre4Y = bill_amount[preop_medication_4=='1']
pre5Y = bill_amount[preop_medication_5=='1']
pre6Y = bill_amount[preop_medication_6=='1']
pre1N = bill_amount[preop_medication_1=='0']
pre2N = bill_amount[preop_medication_2=='0']
pre3N = bill_amount[preop_medication_3=='0']
pre4N = bill_amount[preop_medication_4=='0']
pre5N = bill_amount[preop_medication_5=='0']
pre6N = bill_amount[preop_medication_6=='0']

var.test(pre1Y,pre1N) # no difference
var.test(pre2Y,pre2N) # no difference
var.test(pre3Y,pre3N) # no difference
var.test(pre4Y,pre4N) # no difference
var.test(pre5Y,pre5N) # difference
var.test(pre6Y,pre6N) # no difference
t.test(pre1Y,pre1N) # not significant
t.test(pre2Y,pre2N) # not significant
t.test(pre3Y,pre3N) # not significant
t.test(pre4Y,pre4N) # not significnat
t.test(pre5Y,pre5N,var.equal = FALSE) # not significant
t.test(pre6Y,pre6N) # not significant

symp1Y = bill_amount[symptom_1=='1']
symp2Y = bill_amount[symptom_2=='1']
symp3Y = bill_amount[symptom_3=='1']
symp4Y = bill_amount[symptom_4=='1']
symp5Y = bill_amount[symptom_5=='1']
symp1N = bill_amount[symptom_1=='0']
symp2N = bill_amount[symptom_2=='0']
symp3N = bill_amount[symptom_3=='0']
symp4N = bill_amount[symptom_4=='0']
symp5N = bill_amount[symptom_5=='0']

var.test(symp1Y,symp1N) # different
var.test(symp2Y,symp2N) #different
var.test(symp3Y,symp3N) # different
var.test(symp4Y,symp4N) # different
var.test(symp5Y,symp5N) # different
t.test(symp1Y,symp1N,var.equal = FALSE) # significant
t.test(symp2Y,symp2N,var.equal = FALSE) # significant
t.test(symp3Y,symp3N,var.equal = FALSE) # significant
t.test(symp4Y,symp4N,var.equal = FALSE) # significant
t.test(symp5Y,symp5N,var.equal = FALSE) # significant

summary(aov(bill_amount~race)) # significant
summary(aov(bill_amount~resident_status)) # significant
summary(aov(bill_amount~Age)) # significant

model = lm(bill_amount~lab_result_1+lab_result_2+lab_result_3+weight+height+Duration)
summary(model) # only weight matthers
model1 = lm(bill_amount~lab_result_1+lab_result_2+lab_result_3)
summary(model1) # not significant
model2 = lm(bill_amount~weight)
summary(model2) # significant
model3 = lm(bill_amount~height)
summary(model3) # not siginificant
model4 = lm(bill_amount~Duration)
summary(model4) # not significant

# In conclusion, symptom, race, resident status, age and weight have relationships with bill amount.


detach(Final_Data)
