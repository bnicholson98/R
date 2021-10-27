rm(list=ls()) #ensure a clean R environment 

library(knitr)  #for referenced tables with kable()
library(xtable) #makes data frame for kable
library(printr) #automatically prints output nicely
library(effects)#Graphical and tabular effect displays
library(car) #Companion to Applied Regression
library(AER) #Applied Econometrics with R
library(broom) #for tidy lm output and function glance()
library(stats) #R stat functions
library(lmtest)#for coeftest() and other test functions 
library(stargazer) #nice and informative tables
library(ggplot2)


cps4 <- read.csv(file=file.path("C:/Users/benzi/OneDrive/Documents/SME/","cps4_small.csv"))
head(cps4)

# 1
mod1 <- lm(log(wage)~educ+exper+I(exper^2)+I(exper*educ), data=cps4)
summary(mod1)

cov1 <- hccm(mod1, type="hc1") #package 'car', "hc1"white correction
mod1.HC1 <- coeftest(mod1, vcov.=cov1)  #the same as summary()
mod1.HC1   #heteroskedasticity-consistent se 

# 2
mod2 <- lm(log(wage)~educ+exper+I(exper^2)+I(exper*educ)+married, data=cps4)
summary(mod2)

0.0402895/0.0337911
qt(0.99,994)

# 3
plot(cps4$married, resid(mod1),
     xlab = "Married",
     ylab = "Residuals",
     type = "p")

# 4
m = cps4[which(cps4$married==1),]
u = cps4[which(cps4$married==0),]
alpha <- 0.01

mar1 = lm(log(wage)~educ+exper+I(exper^2)+I(exper*educ), data=m)
mar0 = lm(log(wage)~educ+exper+I(exper^2)+I(exper*educ), data=u)
summary(mar1)
summary(mar0)

df1 <- mar1$df.residual #Numerator degrees of freedom
df0 <- mar0$df.residual #Denominatot df

sig1squared <- glance(mar1)$sigma^2  
sig0squared <- glance(mar0)$sigma^2  

fstat <- sig1squared/sig0squared  # GQ based on F
fstat

Flc <- qf(alpha/2, df1, df0) #Left (lower) critical F
Fuc <- qf(1-alpha/2, df1, df0) #Right (upper) critical F
Flc
Fuc

# 5
sigM <- summary(mar1)$sigma
sigU <- summary(mar0)$sigma

cps4$wght <- rep(0, nrow(cps4))  #create an empty matrix
# Create a vector of weights
for (i in 1:1000) 
{
   if (cps4$married[i]== 0){cps4$wght[i] <- 1/sigU}
   else{cps4$wght[i] <- 1/sigM}
}

mod1.fgls <- lm(log(wage)~educ+exper+I(exper^2)+I(exper*educ), weights = wght, data=cps4)
summary(mod1.fgls)


# 6
vcov(mod1)
vcov(mod1.fgls)
qt(0.95,994)


# 7
plot(cps4$educ, resid(mod1),
     xlab = "EDUC",
     ylab = "Residuals",
     type = "p")

plot(cps4$exper, resid(mod1),
     xlab = "EXPER",
     ylab = "Residuals",
     type = "p")


# 8
alpha <- 0.05      #significance level

ressq <- resid(mod1)^2
modres <- lm(ressq ~ educ + exper+married, data=cps4)
N <- nobs(modres)
gmodres <- glance(modres) #glance() see above

Rsq <- gmodres$r.squared
Rsq
S <- gmodres$df         #Number of Betas in model
chisq <- N*Rsq
chisq
chisqcr <- qchisq(1-alpha, S) #chi-square
chisqcr


# 9 
sigmahatsq <- fitted(modres)
sigmahat <- sqrt(sigmahatsq)
sigmahat


# 10
cps4$wght2 <- sigmahat

mod1.gls <- lm(log(wage)~educ+exper+I(exper^2)+I(exper*educ), weights = wght2, data=cps4)
summary(mod1.gls)


# 11
vcov(mod1)
vcov(mod1.gls)
qt(0.95,994)


# 13
qt(.95,994)*summary(mod1.gls)$sigma/sqrt(nobs(mod1.gls))

qf(.975, 146, 146)
qf(0.95, 3, 993)

