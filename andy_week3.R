library(knitr)
library(xtable)
library(printr)
library(effects)
library(car)
library(AER)
library(broom)
library(tseries)

andy <-read.csv(file='C:/Users/benzi/OneDrive/Documents/SME/andy.csv') 
head(andy)
s <- summary(andy)

mod1 <- lm(formula=sales~price, data=andy)
summary(mod1)

mod2 <- lm(formula=sales~price+advert, data=andy)
summary(mod2)

mod3 <- lm(formula=sales~price+advert+I(advert^2), data=andy)
summary(mod3)

mod4 <- lm(formula=sales~price+advert+I(advert^2)+I(price*advert), data=andy)
summary(mod4)
