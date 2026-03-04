#852 HW 6
library(readxl)
library(survMisc)
library(survival)
library(survminer)
library(tidyverse)
patients <- read_excel("~/Downloads/Spring 26/BS852 (Biostatistical Methods for Observational Studies)/852 Class 6/patients.xlsx")
attach(patients)
#test
tab <- table(treatment)
tab
tapply(patients$diseasefreetime, patients$treatment, sum)
tabR <- table(recurrence, treatment)
pct_tabR <- round(prop.table(tabR, 2), 3)
pct_tabR


#un-adjusted KM curve
km.out.unadjust <- survfit(Surv(diseasefreetime, recurrence) ~ treatment)
print(km.out.unadjust)
summary(km.out.unadjust)
ggsurvplot(km.out.unadjust, 
           data = patients, 
           pval = T, 
           pval.method = T, 
           xlab = "Time (Months)", 
           ylab = "Disease-Free Survival", 
           title = "Kaplan-Meier Curve By Treatment Status", 
           legend.labs = c("Standard Treatment", "New Treatment"))

#Log Rank Test
survdiff(Surv(diseasefreetime, recurrence) ~ treatment) # p = 0.03 * 

#Proportional Hazard Ratio for disease free survival with treatment as predictor 
#Cox Proportional Hazards Regression Model 
cox.unadjust.out <- coxph(Surv(diseasefreetime, recurrence) ~ treatment)
summary(cox.unadjust.out) #HR = 0.6447 (0.4322, 0.9615), p = 0.0314

#Adjusted Analysis
#multivariable proportional hazards regression
#time - diseasfree time, outcome-reccurence, covariate - age at diagosis, sex, risk, treatment

#relevel risk status
patients$riskstatus.f <- relevel(factor(riskstatus, 
                                        levels=c(1, 2, 3), 
                                        labels = c("high", "moderate", "low")), 
                                 ref = "low")

attach(patients)
#create model 
cox.out.ad <- coxph(Surv(diseasefreetime, recurrence) ~ 
                      age_dx + 
                      sexmale + 
                      riskstatus.f + 
                      treatment)
summary(cox.out.ad) #output

