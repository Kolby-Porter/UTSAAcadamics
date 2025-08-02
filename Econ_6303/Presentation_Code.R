#########################
####Presentation Code####
#########################

library(janitor)
library(tidyverse)
library(patchwork)
library(MatchIt)
library(foreign)
library(lmtest)
library(sandwich)
library(readr)
library(optmatch)
library(Matching)
library(rgenoud)
library(cobalt)
library(stargazer)
library(margins)

set.seed(1234)

Data <- read.csv("C:/Users/Kolby/OneDrive/Documents/School Stuff/ECO 6303/Presentation/Data/dec23pub.csv")
Backup <- Data

#################
##Working Model##
#################

# HESP1        <- Food Stamp participation (Treatment variable) 1 = Yes, 0 = No
# HRFS12M1     <- Food security from the last 12 months (Outcome variable) (Binary) 1 = Food Insecure, 0 = Food Secure
# HEFAMINC     <- Family Income Levels
# GEDIV        <- Geographic Region Indicator
# PRNMCHLD     <- Number of Own Children in Household
# PEMARITL     <- Marital Status
# PEEDUCA      <- Education level
# PTDTRACE     <- Race (Integrated Hispanic)
# PENATVTY     <- Country of Origin
# PEMLR        <- Labor Force Participation


####Cleaning Data####
Data1 <- subset(Data, select = c("HRFS12MD", "HRFS12M1", "HESP1", "HEFAMINC", "GEDIV", "PRNMCHLD", "PEMARITL", "PEEDUCA", "PTDTRACE", "PRDTHSP", "PENATVTY", "PEMLR"))
Data1$Hispanic <- ifelse(Data1$PRDTHSP == -1, "Non-Hispanic", 
                         ifelse(Data1$PRDTHSP %in% c(1, 2, 3), "Hispanic", "Other Hispanic"))
Data1$Race_Ethnicity <- ifelse(Data1$Hispanic == "Hispanic", "99", 
                               as.character(Data1$PTDTRACE))
Data1$Race_Ethnicity <- as.numeric(Data1$Race_Ethnicity)


#Removing any negative values across my subset
#-2 Don't Know
#-3 Refused
#-9 No Response
#Leaving HESP1 with
#1 Yes
#2 No

Data1 <- Data1 %>%
  filter(HRFS12M1 >= 0,
         HRFS12MD >= 0,
         HESP1 >= 0,
         HEFAMINC >= 0,
         GEDIV >= 0,
         PEMARITL >= 0,
         PEEDUCA >= 0,
         Race_Ethnicity >= 0,
         PENATVTY >= 0,
         PEMLR >= 0)

a<- ggplot(data = Data1, aes(x= HRFS12M1))+
  geom_density()
b<- ggplot(data = Data1, aes(x = HRFS12MD))+
  geom_density()
c<- ggplot(data = Data1, aes(x = HESP1))+
  geom_density()
d<- ggplot(data = Data1, aes(x = GEDIV))+
  geom_density()
e<- ggplot(data = Data1, aes(x = PEMARITL))+
  geom_density()
f<- ggplot(data = Data1, aes(x = PEEDUCA))+
  geom_density()
g<- ggplot(data = Data1, aes(x = PENATVTY))+
  geom_density()
#Factoring race and keeping top 4 groups and reallocating the remainder in "other"
Data1$PTDTRACE <- as.factor(Data1$Race_Ethnicity)
Data1$PTDTRACE <- fct_lump(Data1$PTDTRACE, n = 4)
#01 White Only
#02 Black Only
#04 Asian Only
#99 Hispanic
#Other - Other
h<- ggplot(data = Data1, aes(x = PTDTRACE))+
  geom_density()
i <- ggplot(data = Data1, aes(x = PEMLR))+
  geom_density()

(a|b)/(c|d)/(e|f)/(g|h)

#Factoring race and keeping top 4 groups and reallocating the remainder in "other"
Data1$PTDTRACE <- as.factor(Data1$Race_Ethnicity)
Data1$PTDTRACE <- fct_lump(Data1$PTDTRACE, n = 4)
#01 White Only
#02 Black Only
#04 Asian Only
#99 Hispanic
#Other - Other

#Factoring the treatment variable HESP1
#In the past 12 months, since December of last year,
#did (you/anyone in this household) get
#SNAP/Supplemental Nutrition Assistance Program or
#food stamp benefits?
#1 Yes
#2 No <- changing to 0 for absence of treatment
Data1$HESP1 <- ifelse(Data1$HESP1 == 2, 0, 1)

Data1$PRNMCHLD <- ifelse(Data1$PRNMCHLD == -1, 0, Data1$PRNMCHLD)

#Coding HRFS12M1 to be a binary outcome of relative food security
Data1$HRFS12M1<- ifelse(Data1$HRFS12M1 > 1, 1, 0)
#HRFS12M1 Was
#1 Food Secure High or Marginal Food Security
#2 Low Food Security
#3 Very Low Food Security

#HRFS12M1 is now
#0 Food Secure High or Marginal Food Security
#1 Low/Very Low Food Security

#Factoring my multinomial food security outcome variable and releveling to reference the most food insecure.
#1 High Food Security
#2 Marginal Food Security
#3 Low Food Security
#4 Very Low Food Security
Data1$HRFS12MD <- as.factor(Data1$HRFS12MD)
Data1$HRFS12MD <- relevel(as.factor(Data1$HRFS12MD), ref = "4")

#Factoring nativity and releveling to reflect US born as the baseline, keeping top 4 groups and reallocating the remainder in "other"
Data1$PENATVTY <- as.factor(Data1$PENATVTY)
Data1$PENATVTY <- relevel(Data1$PENATVTY, ref = "57")
Data1$PENATVTY <- fct_lump(Data1$PENATVTY, n = 4)
Data1$PENATVTY <- ifelse(Data1$PENATVTY == "Other", 999, Data1$PENATVTY)
#057 UNITED STATES
#233 Philippines
#303 Mexico
#312 El Salvador
#Other - Other

#Factoring Family income levels
Data1$HEFAMINC <- as.factor(Data1$HEFAMINC)
Data1$HEFAMINC <- relevel(Data1$HEFAMINC, ref = "1")
#1 LESS THAN $5,000
#2 5,000 TO 7,499
#3 7,500 TO 9,999
#4 10,000 TO 12,499
#5 12,500 TO 14,999
#6 15,000 TO 19,999
#7 20,000 TO 24,999
#8 25,000 TO 29,999
#9 30,000 TO 34,999
#10 35,000 TO 39,999
#11 40,000 TO 49,999
#12 50,000 TO 59,999
#13 60,000 TO 74,999
#14 75,000 TO 99,999
#15 100,000 TO 149,999
#16 150,000 OR MORE

#Factoring geographic area of residence
Data1$GEDIV <- as.factor(Data1$GEDIV)
#1 NEW ENGLAND
#2 MIDDLE ATLANTIC
#3 EAST NORTH CENTRAL
#4 WEST NORTH CENTRAL
#5 SOUTH ATLANTIC
#6 EAST SOUTH CENTRAL
#7 WEST SOUTH CENTRAL
#8 MOUNTAIN
#9 PACIFIC

#Facoring marital status and releveling to reference married.
Data1$PEMARITL <- as.factor(Data1$PEMARITL)
Data1$PEMARITL <- relevel(Data1$PEMARITL, ref = "1")
#1 MARRIED - SPOUSE PRESENT
#2 MARRIED - SPOUSE ABSENT
#3 WIDOWED
#4 DIVORCED
#5 SEPARATED
#6 NEVER MARRIED

#Factoring education level, releveled to reference lowest level of education
Data1$PEEDUCA <- as.factor(Data1$PEEDUCA)
Data1$PEEDUCA <- relevel(Data1$PEEDUCA, ref = "31")
#31 LESS THAN 1ST GRADE
#32 1ST, 2ND, 3RD OR 4TH GRADE
#33 5TH OR 6TH GRADE
#34 7TH OR 8TH GRADE
#35 9TH GRADE
#36 10TH GRADE
#37 11TH GRADE
#38 12TH GRADE NO DIPLOMA
#39 HIGH SCHOOL GRAD-DIPLOMA OR EQUIV (GED)
#40 SOME COLLEGE BUT NO DEGREE
#41 ASSOCIATE DEGREE-OCCUPATIONAL/VOCATIONAL
#42 ASSOCIATE DEGREE-ACADEMIC PROGRAM
#43 BACHELOR'S DEGREE (EX: BA, AB, BS)
#44 MASTER'S DEGREE (EX: MA, MS, MEng, MEd, MSW)
#45 PROFESSIONAL SCHOOL DEG (EX: MD, DDS, DVM)
#46 DOCTORATE DEGREE (EX: PhD, EdD)

#Factoring Employment status and releveling to reference actively working. 
Data1$PEMLR <- as.factor(Data1$PEMLR)
Data1$PEMLR <- relevel(Data1$PEMLR, ref = "1")

Data2 <- Data1[, -c(10,13)]

####Analysis####

#Outcome
#HRFS12M1
#0 Food Secure High or Marginal Food Security
#1 Low/Very Low Food Security

#Treatment
#HESP1
#In the past 12 months, since December of last year,
#did (you/anyone in this household) get
#SNAP/Supplemental Nutrition Assistance Program or
#food stamp benefits?
#1 Yes
#0 No

mean(Data2$HRFS12M1[Data2$HESP1==1])
dif<-mean(Data2$HRFS12M1[Data2$HESP1==1])-mean(Data2$HRFS12M1[Data2$HESP1==0])
dif

###Linear Regression###

lm_dif<-lm(HRFS12M1 ~ HESP1, Data2)
summary(lm_dif)
coeftest(lm_dif, vcovHC)

pols <-lm(HRFS12M1 ~HESP1 + HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, data = Data2)
summary(pols)
coeftest(pols, vcovHC)

aa<-c()
for(i in 1:100){
  Data2_boot<-Data2[sample(1:nrow(Data2),nrow(Data2),replace=T),]
  
  treat<-subset(Data2_boot, HESP1==1)
  untreat<-subset(Data2_boot, HESP1==0)
  
  sep_treat<-lm(HRFS12M1 ~ HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, data = treat)
  summary(sep_treat)
  
  sep_untreat<-lm(HRFS12M1 ~ HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, data = untreat)
  summary(sep_untreat)
  
  regressor <- model.matrix(~ HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, data = Data2)
 
  pred_treat <- regressor %*% coef(sep_treat)
  pred_untreat <- regressor %*% coef(sep_untreat)
  
  # Average Treatment Effect (ATE)
  a <- mean(pred_treat - pred_untreat)
  aa<-rbind(aa,a)
}

mean(aa)
sd(aa)

### Probit model ###
probit_model <- glm(HRFS12M1 ~ HESP1 + HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + 
                      PEEDUCA + PTDTRACE + PENATVTY + PEMLR, 
                    family = binomial(link = "probit"), data = Data2)
summary(probit_model)
L<-logLik(probit_model)

prob_mar <- margins(probit_model)

probit_model1 <-glm(HRFS12M1~1, data=Data2, family=binomial(link="probit"))

pseudo_r2<- 1- as.numeric(logLik(probit_model))/as.numeric(logLik(probit_model1))
pseudo_r2


logit<-glm(HRFS12M1 ~HESP1 + HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, data = Data2,
           family=binomial(link="logit") )
summary(logit)

logit_model1 <-glm(HRFS12M1~1, data=Data2, family=binomial(link="logit"))
log_mar <- margins(logit)
summary(log_mar)

pseudo_r2<- 1- as.numeric(logLik(logit))/as.numeric(logLik(logit_model1))
pseudo_r2

ate_psw2 <-mean((Data2$HESP1 - probit_model$fitted.values)*Data2$HRFS12M1 / (probit_model$fitted.values*(1-probit_model$fitted.values)))

ate_psw<-mean((Data2$HESP1 - logit$fitted.values)*Data2$HRFS12M1 / (logit$fitted.values*(1-logit$fitted.values)))
att_psw<-mean((Data2$HESP1 - logit$fitted.values)*Data2$HRFS12M1 / ((sum(Data2$HESP1==1)/nrow(Data2))*(1-logit$fitted.values)))

ate_psw

ci_lower0 <- mean((Data2$HESP1 - logit$fitted.values)*Data2$HRFS12M1 / (logit$fitted.values*(1-logit$fitted.values))) - 1.96 * sd((Data2$HESP1 - logit$fitted.values)*Data2$HRFS12M1 / (logit$fitted.values*(1-logit$fitted.values)))
ci_upper0 <- mean((Data2$HESP1 - logit$fitted.values)*Data2$HRFS12M1 / (logit$fitted.values*(1-logit$fitted.values))) + 1.96 * sd((Data2$HESP1 - logit$fitted.values)*Data2$HRFS12M1 / (logit$fitted.values*(1-logit$fitted.values)))

### Propensity Score Weighting ###
psw_boot<-c()
for(i in 1:100){
  
  Data2_boot<-Data2[sample(1:nrow(Data2),nrow(Data2),replace=T),]
  
  logit2 <-glm(HRFS12M1 ~HESP1 + HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, data = Data2_boot,
             family=binomial(link="logit") )
  logit2$fitted.values
  
  psw<-mean((Data2_boot$HESP1 - logit2$fitted.values)*Data2_boot$HRFS12M1 / (logit2$fitted.values*(1-logit2$fitted.values)))
  
  psw_boot<-rbind(psw_boot,psw)
}
mean(psw_boot)
ci_lower <- mean(psw_boot) - 1.96 * sd(psw_boot)
ci_upper <- mean(psw_boot) + 1.96 * sd(psw_boot)
c(ci_lower, ci_upper)

sd(psw_boot)

### Regression with p-score weighting ###

Data2$PEEDUCA <- ifelse(Data2$PEEDUCA == 46, 45, Data2$PEEDUCA)

ps<-logit$fitted.values
ps1<-1/ps
ps2<-1/(1-ps)

Data2$psweights<-1/ps
Data2$psweights[Data2$HESP1==0]<- ps2[Data2$HESP1==0]

treat<-subset(Data2, HESP1==1)
untreat<-subset(Data2, HESP1==0)

sep_treat_we <- lm(HRFS12M1 ~ HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, 
                   data = treat, weights = psweights)
  
sep_untreat_we <- lm(HRFS12M1 ~ HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, 
                     data = untreat, weights = psweights)
  
regressor2 <- model.matrix(~1, HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, 
                           data = Data2)

pred_treat2 <- regressor2 %*% coef(sep_treat_we)
pred_untreat2 <- regressor2 %*% coef(sep_untreat_we)
  
PSW_a <- mean(pred_treat2 - pred_untreat2)

ci_lower2 <- mean(pred_treat2 - pred_untreat2) - 1.96 * sd(pred_treat2 - pred_untreat2)
ci_upper2 <- mean(pred_treat2 - pred_untreat2) + 1.96 * sd(pred_treat2 - pred_untreat2)

###########################
#### Matching, Nearest ####
###########################

logit<-glm(HRFS12M1 ~HESP1 + HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, data = Data2,
           family=binomial(link="logit") )

hist(Data2$psweights[Data2$HESP1 == 0])
hist(Data2$psweights[Data2$HESP1 == 1])

match_res<-matchit(HESP1 ~ HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, Data2, method = "nearest", distance = "logit",
                   distance.options = list(), discard = "none", reestimate = F)
summary(match_res)

m.data1 <- match.data(match_res)

fit0 <- lm(HRFS12M1 ~ HESP1, data = m.data1)
summary(fit0)

fit1 <- lm(HRFS12M1~ HESP1 + HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, data = m.data1)
summary(fit1)



ps_model <- matchit(HESP1 ~ HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR,
                    method = "nearest", data = Data2)
summary(ps_model)

# Examine matched dataset
matched_data <- match.data(ps_model)

# Outcome analysis on matched data
model_ps <- glm(HRFS12M1 ~ HESP1 + HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, family = binomial(link = "logit"), data = matched_data)
summary(model_ps)

stratified_sample <- Data2 %>%
  group_by(HESP1, GEDIV) %>%
  sample_frac(0.5)

########################
#### Matching, Full ####
########################


#match_res_full <- matchit(HESP1 ~ HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, 
#                          data = stratified_sample, method = "full", distance = "logit")
#summary(match_res_full)

#m.data.full <- match.data(match_res_full)

#fit_full <- lm(HRFS12M1 ~ HESP1, data = m.data.full)
#summary(fit_full)

#fit_full2 <- lm(HRFS12M1~ HESP1 + HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, data = m.data.full)
#summary(fit_full2)

#######################
#### Matching, CEM ####
#######################

match_cem <- matchit(HESP1 ~ HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, 
                     data = Data2, method = "cem")
summary(match_cem)

m.data.cem <- match.data(match_cem)

fit_cem1 <- lm(HRFS12M1 ~ HESP1, data = m.data.cem)
summary(fit_cem1)

fit_cem2 <- lm(HRFS12M1~ HESP1 + HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, data = m.data.cem)
summary(fit_cem2)

ps_model_cem <- matchit(HESP1 ~ HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR,
                     method = "cem", data = Data2)
summary(ps_model_cem)

# Examine matched dataset
matched_data_cem <- match.data(ps_model_cem)

# Outcome analysis on matched data
model_cem <- glm(HRFS12M1 ~ HESP1 + HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, family = binomial(link = "logit"), data = matched_data_cem)
summary(model_cem)

###########################
#### Matching, Optimal ####
###########################


match_res_opt <- matchit(HESP1 ~ HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, 
                          data = stratified_sample, method = "optimal", distance = "logit")
summary(match_res_opt)

m.data.opt <- match.data(match_res_opt)

fit_opt <- lm(HRFS12M1 ~ HESP1, data = m.data.opt)
summary(fit_opt)

fit_opt2 <- lm(HRFS12M1~ HESP1 + HEFAMINC + GEDIV + PRNMCHLD + PEMARITL + PEEDUCA + PTDTRACE + PENATVTY + PEMLR, data = m.data.opt)
summary(fit_full2)

###############################################################################
############ GRAPHS FOR PRESESNTATION #########################################
###############################################################################
Data3 <- Data2

Data3$GEDIV <- factor(Data3$GEDIV,
                      levels = 1:9,  # Original numeric values
                      labels = c("New England", "Middle Atlantic", "East North Central", 
                                 "West North Central", "South Atlantic", 
                                 "East South Central", "West South Central", 
                                 "Mountain", "Pacific"))
Data3$PTDTRACE <- factor(Data3$PTDTRACE,
                         levels = c(1, 2, 4, 99, "Other"),
                         labels = c("White", "Black", "Asian", "Hispanic", "Other"))
Data3$HasChildren <- ifelse(Data3$PRNMCHLD == 0, "No Children", "Has Children")
table(Data3$HasChildren)

Data3$HEFAMINC <- factor(Data3$HEFAMINC,
                         levels = c(1:16),
                         labels = c("LESS THAN $5,000", "5,000 TO 7,499", "7,500 TO 9,999", "10,000 TO 12,499", "12,500 TO 14,999", "15,000 TO 19,999", "20,000 TO 24,999",
                                    "25,000 TO 29,999", "30,000 TO 34,999", "35,000 TO 39,999", "40,000 TO 49,999", "50,000 TO 59,999", "60,000 TO 74,999", "75,000 TO 99,999", "100,000 TO 149,999", "150,000 OR MORE"
                         ))



ggplot(
  data = Data3,
  aes(x=GEDIV, fill = GEDIV))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Geographic Region of Respondents", x = "Region", y = "Count")
ggplot(
  data = Data3, 
  aes(x = PTDTRACE, fill = PTDTRACE))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Race of Respondents", x = "Race", y = "Count")
ggplot(
  data = Data3, 
  aes(x = HasChildren, fill = HasChildren))+
  geom_bar()+
  theme_bw()+
  labs(title = "Children", x = "Race", y = "Count")
ggplot(
  data = Data2, 
  aes(x = HEFAMINC, fill = HEFAMINC))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Income Levels", x = "Income", y = "Count")
library(dplyr)

Data3$HEFAMINC <- as.numeric(as.character(Data2$HEFAMINC))


summary_table <- Data3 %>%
  group_by(HESP1, HasChildren) %>%
  summarize(
    Avg_Income = mean(HEFAMINC, na.rm = TRUE),
    Food_Insecurity_Rate = mean(HRFS12M1, na.rm = TRUE),
    Avg_Education = mean(PEEDUCA, na.rm = TRUE),
    Num_Observations = n())%>%
  mutate(Treatment_Group = ifelse(HESP1 == 1, "SNAP Participants", "Non-Participants"))%>%
  ungroup()
# Average income

summary_table$Avg_Income <- ifelse(summary_table$Avg_Income > 10, "35,000 TO 39,999", "25,000 TO 29,999" )
summary_table$Avg_Education <- ifelse(summary_table$Avg_Education > 10, "12TH GRADE NO DIPLOMA/DIPLOMA/GED", "SOME COLLEGE BUT NO DEGREE")

print(summary_table)

summary_table %>%
  kable(format = "html", digits = 2, align = "c",
        col.names = c("Group", "Food Insecurity Rate", "Average Income", "Average Education", "Observations", "Participation")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, font_size = 14)

ggplot(Data2, aes(x = psweights, fill = as.factor(HESP1))) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(title = "Propensity Score Distribution by Treatment", 
       x = "Propensity Score", y = "Density", fill = "Treatment")
psw_boot<- mean(psw_boot)

treatment_effects <- data.frame(
  Method = c("Mean Differencing", "Bootstrapped", "Regression Adjustment"),
  ATE = c(ate_psw, psw_boot, PSW_a),
  Lower_CI = c(ci_lower0, ci_lower, ci_lower2),
  Upper_CI = c(ci_upper0, ci_upper, ci_upper2)
)

ggplot(treatment_effects, aes(x = Method, y = ATE)) +
  geom_point(size = 4) +  # Add points for ATE estimates
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +  # Add error bars
  theme_minimal() +  # Use a clean theme
  labs(
    title = "Average Treatment Effects by Method",
    x = "Estimation Method",
    y = "Estimated ATE"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a reference line at 0
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
treatment_effects2 <- data.frame(
  Method = c("Mean Differencing","Regression Adjustment"),
  ATE = c(ate_psw, PSW_a),
  Lower_CI = c(ci_lower0, ci_lower2),
  Upper_CI = c(ci_upper0, ci_upper2)
)

ggplot(treatment_effects2, aes(x = Method, y = ATE)) +
  geom_point(size = 4) +  # Add points for ATE estimates
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +  # Add error bars
  theme_minimal() +  # Use a clean theme
  labs(
    title = "Average Treatment Effects by Method",
    x = "Estimation Method",
    y = "Estimated ATE"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a reference line at 0
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

