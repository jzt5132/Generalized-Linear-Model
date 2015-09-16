
######################################################
###########	Probit model		##############
######################################################


###	Probit Model
###	yi*=xi*b+ui, where yi* is the latent variable
###	define P(y1=1|x1=x)=P(y1*>=0|x1=x)=P(u1>=-x1*b)=1-F(-x*b), for any x
###	where F is the distribution of ui.
###	In Probit model, ui has a normal distribution, therefore P(y1=1|x1=x)=Phi(x*b)


> data("SwissLabor")
> swiss_probit <- glm(participation ~ . + I(age^2), data = SwissLabor, family = binomial(link = "probit"))
> summary(swiss_probit)





