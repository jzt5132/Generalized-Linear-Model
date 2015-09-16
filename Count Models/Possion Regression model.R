

##############################################################
###########	Possion Regression model	##############
##############################################################



###	EX1
require(ggplot2)
require(sandwich)
require(msm)
p <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
    prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
    id <- factor(id)
})
summary(p)




summary(m1 <- glm(num_awards ~ prog + math, family = "poisson", data = p))
cov.m1 <- vcovHC(m1, type = "HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate = coef(m1), `Robust SE` = std.err, `Pr(>|z|)` = 2 * 
    pnorm(abs(coef(m1)/std.err), lower.tail = FALSE), LL = coef(m1) - 1.96 * 
    std.err, UL = coef(m1) + 1.96 * std.err)

r.est