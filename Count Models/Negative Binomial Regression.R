
######################################################################
###########	Negative Binomial Regression model	##############
######################################################################


###EX1

require(foreign)
require(ggplot2)
require(MASS)
dat <- read.dta("http://www.ats.ucla.edu/stat/stata/dae/nb_data.dta")
dat <- within(dat, {
    prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
    id <- factor(id)
})

summary(dat)

###Possion
summary(m1 <- glm.nb(daysabs ~ math + prog, data = dat))

###Prediction
newdata1 <- data.frame(math = mean(dat$math), prog = factor(1:3, levels = 1:3, 
    labels = levels(dat$prog)))
newdata1$phat <- predict(m1, newdata1, type = "response")
newdata1

