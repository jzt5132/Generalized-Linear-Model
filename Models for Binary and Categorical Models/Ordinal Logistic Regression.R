

######################################################################
###########	Ordinal Logistic Regression model	##############
######################################################################

###	Ordered Response
###	yi=sum_j{1(yi*>=roj)}
###	where yi*=xi*b+ui, i=1,...,n


###	EX1

dat <- read.dta("http://www.ats.ucla.edu/stat/data/ologit.dta")
m <- polr(apply ~ pared + public + gpa, data = dat, Hess = TRUE)
summary(m)
