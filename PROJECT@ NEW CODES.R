nhanes<-read.csv("nhanesdata.csv", header=TRUE)
#Part 1c
age<-(nhanes$Age>=30 & nhanes$Age<=79)
analysis1<-nhanes[age,]
analysis1<-analysis1[!(analysis1$Cancer%in%c(1,7,9)),]
analysis1<-analysis1[!(analysis1$HTcm>190),]
analysis1 <- analysis1[((analysis1$Sex==1) &
                          (!(analysis1$WTkg>160 | analysis1$WTkg<50))) |((analysis1$Sex==2) & 
                          (!(analysis1$WTkg>150 | analysis1$WTkg<45))),]

#Part 1d Race
analysis1$Race2<-ifelse(analysis1$Race2==1,"MexAm",
                        ifelse(analysis1$Race2==2,"OthHis",
                               ifelse(analysis1$Race2==3,"NHWhite",
                                      ifelse(analysis1$Race2==4,"NHBlack",
                                             ifelse(analysis1$Race2==6,"NHAsian",
                                                    ifelse(analysis1$Race2==7,"Other","Unknown"))))))
#Part 1e Marital Status
analysis1$Martial<-ifelse(analysis1$Mstatus==1,"Married",
                          ifelse(analysis1$Mstatus==6,"Married",
                                 ifelse(analysis1$Mstatus==2,"Widowed",
                                        ifelse(analysis1$Mstatus==3,"Div/Sep",
                                               ifelse(analysis1$Mstatus==4,"Div/Sep",
                                                      ifelse(analysis1$Mstatus==5,"Single","Unknown"))))))
#part 1f BMi
analysis1$BMI<-analysis1$WTkg/((analysis1$HTcm/100)^2)
#part 1g
analysis1$CHF <- ifelse(analysis1$CHF==2,0,
                        ifelse(analysis1$CHF == 1,1, NA))
analysis1$MI<-ifelse(analysis1$MI==2,0,
                     ifelse(analysis1$MI==1,1,NA))
analysis1$Stroke<-ifelse(analysis1$Stroke==2,0,
                         ifelse(analysis1$Stroke==1,1,NA))
analysis1$COPD<-ifelse(analysis1$COPD==2,0,
                       ifelse(analysis1$COPD==1,1,NA))
analysis1$Cancer<-ifelse(analysis1$Cancer==2,0,
                         ifelse(analysis1$Cancer==1,1, NA))
analysis1$CHD<-ifelse(analysis1$CHD==2,0,
                      ifelse(analysis1$CHD==1,1,NA))
#1H Sex
analysis1$Sex <- ifelse(analysis1$Sex==1, 0, ifelse(analysis1$Sex==2, 1, NA))

#1I rcode
write.csv(analysis1,file = "analysis1.csv")
 
#1J
summary(analysis1$Age)
length(analysis1$Age)

summary(analysis1$Income)
length(analysis1$Income)

summary(analysis1$WTkg)
length(analysis1$WTkg)

summary(analysis1$HTcm)
length(analysis1$HTcm)

summary(analysis1$WAISTcm)
length(analysis1$WAISTcm)

summary(analysis1$HIPcm)
length(analysis1$HIPcm)

summary(analysis1$SBP)
length(analysis1$SBP)

summary(analysis1$DBP)
length(analysis1$DBP)

summary(analysis1$HGB)
length(analysis1$HGB)

summary(analysis1$HCT)
length(analysis1$HCT)

summary(analysis1$RBC)
length(analysis1$RBC)

summary(analysis1$MCV)
length(analysis1$MCV)

summary(analysis1$MCH)
length(analysis1$MCH)

summary(analysis1$WBC)
length(analysis1$WBC)

summary(analysis1$HbA1c)
length(analysis1$HbA1c)

summary(analysis1$HDL)
length(analysis1$HDL)

summary(analysis1$LDL)
length(analysis1$LDL)

summary(analysis1$TRIG)
length(analysis1$TRIG)

summary(analysis1$CRP)
length(analysis1$CRP)




# part 2: 
# part 2a (i)
qqnorm(analysis1$Income)
qqline(analysis1$Income)
cor.test(analysis1$Income,analysis1$HGB, method = "spearman")
cor.test(analysis1$Income,analysis1$HCT, method = "spearman")
cor.test(analysis1$Income,analysis1$MCV, method = "spearman")
cor.test(analysis1$Income,analysis1$CRP, method = "spearman")
cor.test(analysis1$Income,analysis1$HDL, method = "spearman")
cor.test(analysis1$Income,analysis1$LDL, method = "spearman")

#Part2(ii)
table(analysis1$Martial)
tapply(analysis1$CRP,analysis1$Martial,mean,na.rm=TRUE)
tapply(analysis1$CRP,analysis1$Martial,sd,na.rm=TRUE)
table(analysis1$Race2)
tapply(analysis1$CRP,analysis1$Race2,mean,na.rm=TRUE)
tapply(analysis1$CRP,analysis1$Race2,sd,na.rm=TRUE)


analysis1$Martial<-as.factor(analysis1$Martial)
analysis1$Martial<-relevel(factor(analysis1$Martial),ref = "Unknown")
Global_test1<-aov(analysis1$CRP~analysis1$Martial)
summary(Global_test1)
crp_martial<-glm(analysis1$CRP~analysis1$Martial)
summary(crp_martial)
TukeyHSD(Global_test1)

analysis1$Race2<-as.factor(analysis1$Race2)
analysis1$Race2<- relevel(factor(analysis1$Race2), ref="Other")
Global_test2<-aov(analysis1$CRP~analysis1$Race2)
summary(Global_test2)
crp_race2<-glm(analysis1$CRP~analysis1$Race2)
summary(crp_race2)

crp_income<-lm(analysis1$CRP~analysis1$Income)
summary(crp_income)


# 2ii b linear regression between income and crp

multicrp<-lm(analysis1$CRP~analysis1$Race2+analysis1$Martial+analysis1$Income+analysis1$Sex+analysis1$Age)
summary(multicrp)

# 2ii c 
par(mfrow=c(2,2))
plot(multicrp)
bartlett.test(analysis1$CRP~analysis1$Martial)

#part 2b
part2b<-lm(CRP~COPD+Stroke+Age+Sex,data=analysis1)
summary(part2b)

#Part 3iib 
library(tidyverse)
analysis2 <- analysis1[!analysis1$CHF==1,]
analysis2 <- analysis2[!analysis2$MI==1,]
analysis2 <- analysis2[!analysis2$Stroke==1,]
analysis2 <- analysis2[!analysis2$COPD==1,]
analysis2$Race2<-relevel(factor(analysis2$Race2),ref = "Other")
mortality<-glm(analysis2$Death~analysis2$HGB+analysis2$HbA1c+analysis2$LDL+analysis2$HDL+
                 analysis2$TRIG+analysis2$CRP+analysis2$BMI+analysis2$Age+analysis2$Income+
                 analysis2$Race2+analysis2$Sex,family = "binomial")
summary(mortality)Cm
exp(cbind(OR=coef(mortality),confint(mortality)))

#Part 3iii

analysis2$LDL_TRIG<-ifelse(is.na(analysis2$LDL)|(is.na(analysis2$TRIG)),0,1)

analysis2$Race2<-relevel(factor(analysis2$Race2),ref = "NHWhite")

LDL_TRIG_0 <- glm(analysis2$Death~analysis2$LDL_TRIG+analysis2$HGB+analysis2$HbA1c+analysis2$HDL+analysis2$CRP+analysis2$BMI+
                    analysis2$Age+analysis2$Income+analysis2$Race2+analysis2$Sex, family = "binomial", data=subset(analysis2,LDL_TRIG==0))
exp(cbind(OR=coef(LDL_TRIG_0),confint(LDL_TRIG_0)))

summary(LDL_TRIG_0)
LDL_TRIG_1 <- glm(analysis2$Death~analysis2$LDL_TRIG+analysis2$HGB+analysis2$HbA1c+analysis2$HDL+analysis2$CRP+analysis2$BMI+
                    analysis2$Age+analysis2$Income+analysis2$Race2+analysis2$Sex, family = "binomial", data=subset(analysis2, analysis2$LDL_TRIG==1))
summary(LDL_TRIG_1)
exp(cbind(OR=coef(LDL_TRIG_1),confint(LDL_TRIG_1)))


