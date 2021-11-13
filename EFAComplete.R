##import the file
str(responses)
NumData <- responses[,c(10:18)]
NumData <- as.data.frame(lapply(NumData, as.numeric))
NumData$NewVaccinesRiskierthanOlder <- abs(NumData$NewVaccinesRiskierthanOlder -6)
master <- NumData

##accuracy
summary(master)
str(master)
#recode

##outliers
cutoff = qchisq(1-.001, ncol(NumData))
mahal = mahalanobis(NumData,
                    colMeans(NumData),
                    cov(NumData))
cutoff ##cutoff score
ncol(NumData) ##df
summary(mahal < cutoff)

##exclude outliers
noout = subset(NumData, mahal < cutoff)

noout$NoVaccinesForUncommonDiseases <- NULL
##additivity
correl = cor(noout)
symnum(correl)
pairs.panels(noout)

##assumption set up
par(mfrow = c(1,2))
random = rchisq(nrow(noout), 5)
fake = lm(random~., data = noout)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)
par(mfrow = c(1,1))

##homogeneity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##running the efa analysis
library(psych)
library(GPArotation)

##correlation adequacy Bartlett's test
cortest.bartlett(correl, n = nrow(noout))

##sampling adequacy KMO test
KMO(correl)

##how many factors?
nofactors = fa.parallel(noout, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion


##simple structure with a three factor model
round1 = fa(noout, nfactors=3, rotate = "varimax", fm = "ml")
round1
print(factanal(noout, 2, rotation = "varimax"), cutoff = 0.3)
##get cfi
finalmodel = fa(noout[ ,], nfactors=3, rotate = "varimax", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))
fa.diagram(finalmodel, digits = 2, main = "Factor Diagram", cut= 0.3, simple = F,errors = T)
##reliability
factor1 = c(1:4,7)
factor2 = c(2,4,6)
factor3 = c(5,8)
factor4 = c(5,8)
psych::alpha(noout[ , factor1])
# noout$NewVaccinesRiskierthanOlder <- abs(noout$NewVaccinesRiskierthanOlder -6)
psych::alpha(noout[ , factor2], check.keys = T)
psych::alpha(noout[ , factor3], check.keys = T)

##create new factor scores
noout$f1 = apply(noout[ , factor1], 1, mean) ##creates average scores
noout$f2 = apply(noout[ ,factor2], 1, mean) ##creates average scores
noout$f3 = apply(noout[ , factor3], 1, mean) ##creates average scores
noout$f4 = apply(noout[ , factor4], 1, mean) ##creates average scores


summary(noout)
sd(noout$f1)
sd(noout$f2)
sd(noout$f3)
