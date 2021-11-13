motivationInc <- multistring_data$MotivationIncIf

levels(as.factor(motivationInc))

motiv <- as.factor(c(
  'I saw that my friends and family didnâ???Tt have negative side effects from the vaccination',
  'I was convinced that the vaccine had been rigorously tested',
  'A trusted health care worker told me to get vaccinated',
  'I saw that enough people who got the vaccine didnâ???Tt get sick with COVID-19',
  'Someone I knew was hospitalized/died from COVID-19',
  'I was convinced that getting vaccinated helped protect vulnerable members of my community',
  'Religious leaders in my community said I should get vaccinated',
  'I received a financial incentive',
  'Getting vaccinated was required by my government',
  'Getting vaccinated was a requirement for my university (or job)',
  'None of these apply'))

a <- as.data.frame(t(data.frame(rep(1,11))))
colnames(a) <- motiv
a <- a[-1,]
goodmatrix <- a
motiv <- a
View(a)

for(j in 1:length(multistring_data$MotivationIncIf)){
  vector <- unlist(strsplit(multistring_data$MotivationIncIf[j],', '))
  a<-matrix(,nrow = length(vector),ncol = 11)
  a <- na.omit(a)
  for(i in 1:length(vector)){
    a <- rbind(a,as.numeric(vector[i]==names(goodmatrix)))
  }
  a <- colSums(a)
  motiv <- rbind(motiv,a)
  names(motiv) <- names(goodmatrix)
}
View(motiv)

write.csv(statements,"C:/Users/sheharyar.akhtar/Desktop/MotivationIncreaseSparseMatrix.csv", row.names = F)


