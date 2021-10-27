rm(list=ls()) #ensure a clean R environment 
library(knitr)  #for referenced tables with kable()
library(xtable) #makes data frame for kable
library(printr) #automatically prints output nicely
library(effects)
library(car)
library(AER)
library(broom) #for tidy lm output and function glance()
library(stats)
library(lmtest)#for coeftest() and other test functions 
library(stargazer) #nice and informative tables
library(ggplot2)

pizza4 <- read.csv(file='C:/Users/benzi/OneDrive/Documents/SME/pizza4.csv')
head(pizza4)
summary(pizza4)

#plot 1
plot(pizza4$pizza,pizza4$income, 
     ylim=c(0, max(pizza4$pizza)),
     xlim=c(0, max(pizza4$income)),
     xlab="weekly income in $1000", 
     ylab="pizza expenditure in $", 
     type = "p")
#plot 2
plot(pizza4$pizza/10,pizza4$age, 
     ylim=c(0, max(pizza4$pizza/10)),
     xlim=c(0, max(pizza4$age)),
     xlab="age", 
     ylab="pizza expenditure in $", 
     type = "p")


mod1 <- lm(pizza~age+income, data=pizza4)
mod2 <- lm(pizza~age+income+age*income, data=pizza4)
mod3 <- lm(pizza~age+income+age*income+female, data=pizza4)

summary(mod1)
summary(mod1)$r.squared
AIC(mod1)
BIC(mod1)

summary(mod2)
summary(mod3)

#make table
smod1 <- data.frame(xtable(summary(mod1)))
kable(smod1, 
      caption="The basic multiple regression model", 
      col.names=c("coefficient", "Std. Error", "t-value", "p-value"),
      align="c", digits=3)


set.seed(0569)
id <- rnorm(40, 0,1)

pizza1 = cbind(pizza4, id)
head(pizza1)

mod4 <- lm(pizza~age+income+age*income+female+id, data=pizza1)
summary(mod4)

r1 <- as.numeric(glance(mod1))  #as.numeric extracts only the numbers
r2 <- as.numeric(glance(mod2))  #glance()contains both col and row
r3 <- as.numeric(glance(mod3))
r4 <- as.numeric(glance(mod4))

tab1 <- data.frame(rbind(r1, r2, r3, r4))[,c(1,2,8,9)] #rbind() several rows in a matrix
row.names(tab1) <- c("age, income",
                     "age, income, age*income",
                     "age, income, age*income, female",
                     "age, income, age*income,female, ID")
kable(tab1, 
      caption="Model comparison, 'pizza' ", digits=3, 
      col.names=c("Rsq","AdjRsq","AIC","BIC"))


# Hypothesis test
mod3 <- lm(pizza~ age + income + I(age*income) + female, data=pizza4)
Hnull <- c("age = 0", "I(age * income) = 0")  # specify H0
linearHypothesis(mod3,Hnull)

qf(0.95, 2, 35) #F-critical value


# Prediction
predpoint <- data.frame(age=22, income=25, female=0)
predpoint

p <- predict(mod3, newdata=predpoint, interval="prediction") #level=95% by default
p


  