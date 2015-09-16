
##############################################################
###########	Truncated Regression		##############
##############################################################

###	EX1

###	REQUIRE PACKAGE
require(foreign)
require(ggplot2)
require(truncreg)
require(boot)


###	READ DATA
dat <- read.dta("http://www.ats.ucla.edu/stat/data/truncreg.dta")
summary(dat)


###	Fitting truncated regression
m <- truncreg(achiv ~ langscore + prog, data = dat, point = 40, direction = "left")
summary(m)

###	Testing on langscore
m2 <- update(m, . ~ . - prog)
pchisq(-2 * (logLik(m2) - logLik(m)), df = 2, lower.tail = FALSE)
