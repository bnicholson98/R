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

utown <- read.csv(file='C:/Users/benzi/OneDrive/Documents/SME/utown.csv')
head(utown)

# 1
# -1
utown$utown <- as.factor(utown$utown)
utown$pool <- as.factor(utown$pool)
utown$fplace <- as.factor(utown$fplace)
kable(summary.data.frame(utown), caption="Summary for 'utown' dataset")

# -2
mod1 <- lm(price~utown+sqft+sqft*utown+age+pool+fplace, data=utown)
kable(tidy(mod1), caption="The 'house prices' model")

# -3
bsqft <- 1000*coef(mod1)[["sqft"]] #not near uni
bsqft1 <- 1000*(coef(mod1)[["sqft"]]+coef(mod1)[["utown1:sqft"]]) #near uni


# 2
cps4 <- read.csv(file='C:/Users/benzi/OneDrive/Documents/SME/cps4_small.csv')
head(cps4)
# -1
mod2 <- lm(wage~educ+black+female+black*female, data=cps4)
delta1 <- coef(mod2)[["black"]]
delta2 <- coef(mod2)[["female"]]
gamma <- coef(mod2)[["black:female"]]

blfm <- delta1+delta2+gamma #change of E(wage) from being black &female
blfm

kable(tidy(mod2), caption="A wage-discrimination model")

# -2
hyp <- c("black=0", "female=0", "black:female=0")
tab <- tidy(linearHypothesis(mod2,hyp))
kable(tab,caption="Testing joint hypothesis for the 'wage' equation")

# -3
dnosouth <- cps4[which(cps4$south==0),]#no south
dsouth <- cps4[which(cps4$south==1),] #south

mod3ns <- lm(wage~educ+black*female, data=dnosouth)
mod3s <- lm(wage~educ+black*female, data=dsouth)

mod4 <- lm(wage~educ+black*female+south/(educ+black*female), data=cps4)


stargazer(mod4, mod3ns, mod3s, header=FALSE, 
          type="text",
          title="Model comparison, 'wage' equation",
          keep.stat="n",digits=2, single.row=TRUE,
          intercept.bottom=FALSE)
#Chow-test
kable(anova(mod2, mod4), caption="Chow test for the south in'wage'equation")


# 3
mod5 <- lm(log(wage)~educ+female, data=cps4)
summary(mod5)

approx <- 100*coef(mod5)[['female']]
approx
actual <- 100*(exp(coef(mod5)[['female']])-1)
actual


# 4
T <- 1000
set.seed(105)

id <- rnorm(T, mean=0, sd=1)
id

utown1 = cbind(utown, id)
head(utown1)
summary(utown1)

mod6 <- lm(price~utown+sqft+sqft*utown+age+pool+fplace+id, data=utown1)
summary(mod6)
summary(mod1)
