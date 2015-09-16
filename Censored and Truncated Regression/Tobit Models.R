

##############################################################
###########	Tobit model			##############
##############################################################

###	(Truncated) Tobit Model 
###	Truncation arises if one only observes data on {(yi,xi)} if yi*=xi*b+ui>0
###	P(y1<=y|x1=x)=P(y1*<=y|x1=x,y1*>0)=P(0<y1*<=y1|x1=x)/P(yi*>0|x1=x)
###	(lhs)={Phi(y-x*b/sigma)-Phi(-x*b/sigma)}/Phi(x*b/sigma)


###	EX1
dat <- read.csv("http://www.ats.ucla.edu/stat/data/tobit.csv")
summary(dat)


###	Fitting model
summary(m <- vglm(apt ~ read + math + prog, tobit(Upper = 800), data = dat))


### TEST
m2 <- vglm(apt ~ read + math, tobit(Upper = 800), data = dat)
(p <- pchisq(2 * (logLik(m) - logLik(m2)), df = 2, lower.tail = FALSE))
