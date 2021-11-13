head(r)
View(r)
str(r)
scoring <- r[,c(1,7:15)]
str(scoring)
scoring$scores <- rowSums(scoring[,2:10])
View(scoring)
barplot(table(scoring$scores))

qqnorm(table(scoring$scores))
qqline(table(scoring$scores))

scoring$scores


medscores <- num.med[,6:14]
medscores$scores <- rowSums(medscores)

nonmedscores <- num.nonmed[,6:14]
nonmedscores$scores <- rowSums(nonmedscores)

barplot(table(nonmedscores$scores))
barplot(table(medscores_remove_outliers$scores))
summary(nonmedscores$scores)
summary(medscores$scores)

qqnorm(log(medscores$scores))
qqline(log(medscores$scores), col = 'red')

qqnorm(medscores_remove_outliers$scores)
qqline(medscores_remove_outliers$scores, col = 'red')


boxplot(medscores_remove_outliers$scores)
boxplot(nonmedscores_remove_outliers$scores)
removeOutliers <- function(x){
  qm <- subset(x, x[,1]=='Medical')
  qm <- qm[,1:11]
  qnm <- subset(x, x[,1]=='NonMedical')
  qnm <- qnm[,1:11]
  
  Q <- quantile(qm[,11], probs=c(.25, .75), na.rm = FALSE)
  iqr <- IQR(qm[,11])
  up <-  Q[2]+1.5*iqr # Upper Range  
  low<- Q[1]-1.5*iqr # Lower Range
  qm <- subset(qm, qm[,11] > (Q[1] - 1.5*iqr) & qm[,11] < (Q[2]+1.5*iqr))
  
  Q <- quantile(qnm[,11], probs=c(.25, .75), na.rm = FALSE)
  iqr <- IQR(qnm[,11])
  up <-  Q[2]+1.5*iqr # Upper Range  
  low<- Q[1]-1.5*iqr # Lower Range
  qnm <- subset(qnm, qnm[,11] > (Q[1] - 1.5*iqr) & qnm[,11] < (Q[2]+1.5*iqr))
  
  x <- rbind(qm, qnm)
  
}
Q <- quantile(nonmedscores$scores, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(nonmedscores$scores)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
nonmedscores_remove_outliers <- subset(nonmedscores, nonmedscores$scores > (Q[1] - 1.5*iqr) & nonmedscores$scores < (Q[2]+1.5*iqr))


ggbetweenstats(scoring, Curriculum, scores, outlier.tagging = TRUE)



a$VaccinesImpForHeath <- abs(a$VaccinesImpForHeath-6)
a$VaccinesAreEffective <- abs(a$VaccinesAreEffective-6)
a$VaccinesImpForComm <- abs(a$VaccinesImpForComm-6)
a$NewVaccinesRiskierthanOlder <- abs(a$NewVaccinesRiskierthanOlder-6)
a$InformationReliable <- abs(a$InformationReliable-6)
a$VaccinesProtect <- abs(a$VaccinesProtect-6)
a$AllVaccinesAreGood <- abs(a$AllVaccinesAreGood-6)






medscores_remove_outliers$Curriculum <- rep('Medical', 211)
nonmedscores_remove_outliers$Curriculum <- rep('NonMedical', 173)
scoring <- rbind(medscores_remove_outliers, nonmedscores_remove_outliers)
##########CENTRAL LIMIT THEOREM
for(i in 1:1000)
{
  sample <- sample(a$Score, 20)
  mean <- append(mean,mean(sample))
}

hist(mean)
boxplot(mean) 

Q <- quantile(mean, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(mean)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
mean <- subset(mean, mean > (Q[1] - 1.5*iqr) & mean < (Q[2]+1.5*iqr))

par(mfrow = c(1,2))
qqnorm(mean)
qqline(mean, col = 'red')



med <- subset(a, a$Curriculum == 'Medical')
nonmed <- subset(a, Curriculum == 'NonMedical')
mean = 0
for(i in 1:1000)
{
  sample <- sample(med$Score, 20)
  mean <- append(mean, mean(sample))
}

hist(mean)
qqnorm(mean)
qqline(mean, col = 'red')
mean(mean)
mean(med$Score)

mean = 0
for(i in 1:1000)
{
  sample <- sample(nonmed$Score, 20)
  mean <- append(mean, mean(sample))
}

hist(mean)
qqnorm(mean)
qqline(mean, col = 'red')
mean(mean)
mean(nonmed$Score)

t.test()