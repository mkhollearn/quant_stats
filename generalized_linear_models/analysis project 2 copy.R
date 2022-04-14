data <- read.csv("~/Desktop/Accident2022new.csv", stringsAsFactors=TRUE)
View(data)

######### Q1: vehicle age as an indicator of accident fatality when controlling for the speed of the car ##################
levels(data$died)

model1 <- glm(died ~ vehicleAge + speedCat,data = data, family=binomial(logit))
summary(model1)

library(DescTools)#Psuedo R2 
PseudoR2(model1,which= NULL)

#break down the log odds for vehicle age
library(pastecs)
range(data$vehicleAge)
mean(as.numeric(data$vehcileAge)) # doesn't work at all
stat.desc(data$vehicleAge, basic = TRUE) # still gives me an NaN on my laptop but works on my desktop - no idea why. 

intercept_value1 = summary(model1)$coeff[1,1]
beta_value1 = summary(model1)$coeff[2,1]
mean_veh_age <- stat.desc(data$vehicleAge)['mean']
exp(intercept_value1 + beta_value1*mean_veh_age)#at the mean level of vehicle Age
exp(beta_value1)#one unit change in vehicle Age
exp(confint.default(model1)) #CI for 1 unit change

model1.2<- glm(died~vehicleAge, data = data, family=binomial(logit))
summary(model1.2)

########## Q2: seatbelt and frontal impact on fatality controlling for speed and vehicle age ####################################
model2 <- glm(died ~ frontal + seatbelt + speedCat + vehicleAge, data = data, family=binomial(logit))
summary(model2)

PseudoR2(model2,which= NULL)

levels(data$died)

#break down log odds for fontral
levels(data$frontal) # 0=frontal, 1=not frontal 
intercept_value2 = summary(model2)$coeff[1,1]
beta_value_frontal = summary(model2)$coeff[2,1]
exp(intercept_value2 + beta_value_frontal*0) #frontal
exp(intercept_value2 + beta_value_frontal*1) #not frontal
exp(beta_value_frontal)#going from frontal to not frontal - 1 unit change
exp(confint.default(model2)) #CI for 1 unit change

#break down log odds for seatbelt
levels(data$seatbelt) #0 = belted, 1 = none
intercept_value2 = summary(model2)$coeff[1,1]
beta_value_belt = summary(model2)$coeff[3,1]
exp(intercept_value2 + beta_value_belt*0) #odds when belted
exp(intercept_value2 + beta_value_belt*1) # odds when none
exp(beta_value_belt) #going from belted to no belt - 1 unit change
exp(confint.default(model2)) #CI for the 1 unit change


######### Q3: interaction bw frontal and seatbelt controlling for speed and vehicle age ###########
model3 <- glm(died ~ frontal * seatbelt + speedCat + vehicleAge, data = data, family=binomial(logit))
summary(model3)

PseudoR2(model3,which= NULL)

#visuaize interaction
install.packages('interactions')
library(interactions)
cat_plot(model3, pred = seatbelt, modx = frontal, x.label = 'Wearing a Seatbelt', y.label = 'Probability of Dying') # for categorical
#interact_plot(model3, pred = seatbelt, modx = frontal) # for continuous predictor

levels(data$frontal) # 0=frontal, 1=not frontal 
levels(data$seatbelt) #0 = belted, 1 = none

## SIMPLE SLOPES
#to simplify simple slope analyses for catergorical precidtors, take the 0 value for that predictor
#by taking the 0 value of X here makes X and the interaction values zero
#also makes the value of Z 0

#Frontal accidents, 0 = belted, 1 = not belted
#break down log odds
intercept_value3 = summary(model3)$coeff[1,1]
beta_value_frontalbelt = summary(model3)$coeff[9,1]
exp(intercept_value3 + beta_value_frontalbelt*0) #odds when belted
exp(intercept_value3 + beta_value_frontalbelt*1) # odds when none
exp(beta_value_frontalbelt) #going from belted to no belt - 1 unit change
exp(confint.default(model3)) #CI for the 1 unit change


#Non frontal accidents -> reverse coded baseline
levels(data$frontal)
frontal_r <- factor(data$frontal, levels = c('NonFrontalImpact', 'FrontalImpact'))
levels(frontal_r)

model3.2 <- glm(died ~ frontal_r * seatbelt + speedCat + vehicleAge, data = data, family=binomial(logit))
summary(model3.2)

#break down log odds for non-frontal accidents
intercept_value3.2 = summary(model3.2)$coeff[1,1]
beta_value_nonfrontalbelt = summary(model3.2)$coeff[9,1]
exp(intercept_value3.2 + beta_value_nonfrontalbelt*0) #odds when belted
exp(intercept_value3.2 + beta_value_nonfrontalbelt*1) # odds when none
exp(beta_value_nonfrontalbelt) #going from belted to no belt - 1 unit change
exp(confint.default(model3.2)) #CI for the 1 unit change

######### Q4: age of occupant on fatality controlling for speed, frontal, and seatbelt ##################
model4 <- glm(died ~ ageOFocc + seatbelt + speedCat + frontal, data = data, family=binomial(logit))
summary(model4)

PseudoR2(model4,which= NULL)

#break down log odds for age of occupant
mean(data$ageOFocc)
sd(data$ageOFocc)
range(data$ageOFocc)
intercept_value4 = summary(model4)$coeff[1,1]
beta_value_ageOFocc = summary(model4)$coeff[2,1]
exp(intercept_value4 + beta_value_ageOFocc*mean(data$ageOFocc)) #at the mean age of occupant
exp(beta_value_ageOFocc)# 1 unit change in age of occupant
exp(confint.default(model4)) #CI for 1 unit change
exp(beta_value_ageOFocc*10) # 10 unit change in age of occupant
exp(confint.default(model4))[2,]^10 #CI for 10 unit change
exp(confint.default(model4))^10 #CI sanity check

