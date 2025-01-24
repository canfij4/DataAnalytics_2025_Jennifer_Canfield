#Exercise 1
EPI_data <- read.csv("C:/Users/Jennifer Canfield/Desktop/Data Analytics/epi2024results06022024.csv")
View(EPI_data)
attach(EPI_data)
EPI.new
NAs <- is.na(EPI.new)
EPI.new.noNAs <- EPI.new[!NAs]
summary(EPI.new) #stats
fivenum(EPI.new,na.rm=TRUE)
stem(EPI.new)
hist(EPI.new)
hist(EPI.new,seq(20.,80.,1.0),prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)
boxplot(EPI.new, APO.new)
hist(EPI.new, seq(20.,80.,1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE, bw=1.))
rug(EPI.new)
hist(EPI.new, seq(20.,80.,1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE, bw="SJ"))
rug(EPI.new)
x <- seq(20,80,1)
q <- dnorm(x, mean=42, sd=5, log=FALSE)
lines(x,q)
lines(x,.4*q)
q <- dnorm(x,mean=65, sd=5, log=FALSE)
lines(x, .12*q)

#Exercise 2: Fitting a distribution beyond histograms
#cumulative density function?
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)
#Quantile-Quantile? 
qqnorm(EPI.new); qqline(EPI.new)
#Make a Q-Q plot against the generating distribution by: 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

#Exercise 2a-do the same exploration and fitting for another 2 variables in EPI
#First variable - ECO.ew
ECO.new
NAs <- is.na(ECO.new)
ECO.new.noNAs <- ECO.new[!NAs]
summary(ECO.new) #stats
fivenum(ECO.new,na.rm=TRUE)
stem(ECO.new)
hist(ECO.new)
hist(ECO.new,seq(20.,90.,1.0),prob=TRUE)
lines(density(ECO.new,na.rm=TRUE,bw=1.))
rug(ECO.new)
boxplot(ECO.new, APO.new)
hist(ECO.new, seq(20.,90.,1.0), prob=TRUE)
lines(density(ECO.new,na.rm=TRUE, bw=1.))
rug(ECO.new)
hist(ECO.new, seq(20.,90.,1.0), prob=TRUE)
lines(density(ECO.new,na.rm=TRUE, bw="SJ"))
rug(ECO.new)
x <- seq(20,90,1)
q <- dnorm(x, mean=45, sd=5, log=FALSE)
lines(x,q)
lines(x,.4*q)
q <- dnorm(x,mean=65, sd=5, log=FALSE)
lines(x, .12*q)

#Exercise 2a: Second Variable - PAR.new
PAR.new
NAs <- is.na(PAR.new)
PAR.new.noNAs <- PAR.new[!NAs]
summary(PAR.new) #stats
fivenum(PAR.new,na.rm=TRUE)
stem(PAR.new)
hist(PAR.new)
hist(PAR.new,seq(0.0,110.,1.0),prob=TRUE)
lines(density(PAR.new,na.rm=TRUE,bw=1.))
rug(PAR.new)
boxplot(PAR.new, APO.new)
hist(PAR.new, seq(0.0,110.,1.0), prob=TRUE)
lines(density(PAR.new,na.rm=TRUE, bw=1.))
rug(PAR.new)
hist(PAR.new, seq(0.0,110.,1.0), prob=TRUE)
lines(density(PAR.new,na.rm=TRUE, bw="SJ"))
rug(PAR.new)
x <- seq(0.0,110,1)
q <- dnorm(x, mean=45, sd=5, log=FALSE)
lines(x,q)
lines(x,.4*q)
q <- dnorm(x,mean=65, sd=5, log=FALSE)
lines(x, .12*q)
