
################################################################
###########	Multinomial Logit model	################
################################################################

###	Multinomial Logit 
###	yij*=xij*b+uij, i=1,...,n, j=0,...J
###	P(y1=0|X1=X)=P(max{y1j*}<=y10*|X1=X)
###	using IIA assumption

###	EX1
data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, shape="wide", varying=2:9, choice="mode")

###	EX2

data("Train", package="mlogit")
Tr <- mlogit.data(Train, shape = 'wide', choice="choice",varying=4:11, sep="", alt.levels=c(1, 2), id = "id")