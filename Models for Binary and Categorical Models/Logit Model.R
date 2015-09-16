


######################################################
###########	Logit model		##############
######################################################


###	Logit Model
###	yi*=xi*b+ui, where yi* is the latent variable
###	define P(y1=1|x1=x)=P(y1*>=0|x1=x)=P(u1>=-x1*b)=1-F(-x*b), for any x
###	where F is the distribution of ui.
###	In Probit model, ui has a logistic distribution


> data("SwissLabor")
> swiss_logit <- glm(participation ~ . + I(age^2), data = SwissLabor, family = binomial(link = "logit"))
> summary(swiss_logit)

