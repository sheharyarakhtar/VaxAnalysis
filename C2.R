test3 <- r[,c(1,7:15)]
View(test3)

test3 <- as.data.frame(lapply(test3, as.factor))
levels(test3$Curriculum) <- c('Med','NonMed')

x <-chisq.test(table(test3$Curriculum, test3$VaccinesImpForHeath), simulate.p.value = T)
x <- rbind(x,chisq.test(table(test3$Curriculum, test3$VaccinesAreEffective), simulate.p.value = T))
x <- rbind(x, chisq.test(table(test3$Curriculum, test3$VaccinesImpForComm), simulate.p.value = T))
x <- rbind(x, chisq.test(table(test3$Curriculum, test3$AllVaccinesAreGood), simulate.p.value = T))
x <- rbind(x, chisq.test(table(test3$Curriculum, test3$NewVaccinesRiskierthanOlder), simulate.p.value = T))
x <- rbind(x, chisq.test(table(test3$Curriculum, test3$InformationReliable), simulate.p.value = T))
x <- rbind(x, chisq.test(table(test3$Curriculum, test3$VaccinesProtect), simulate.p.value = T))
x <- rbind(x, chisq.test(table(test3$Curriculum, test3$ConcernedofSideEffects), simulate.p.value = T))
x <- rbind(x, chisq.test(table(test3$Curriculum, test3$NoVaccinesForUncommonDiseases), simulate.p.value = T))

par(mfrow = c(1,2))
barplot(table(num.med$VaccinesProtect))
barplot(table(num.nonmed$VaccinesProtect))

boxplot(num.nonmed$VaccinesImpForComm)
barplot(table(num.nonmed$VaccinesImpForComm))

barplot(table(num.med$VaccinesProtect))
barplot(table(num.nonmed$VaccinesProtect))
barplot(table(r$VaccinesProtect))
barplot(table(r$VaccinesProtect))
barplot(table(r$VaccinesProtect))
barplot(table(r$VaccinesProtect))
barplot(table(r$VaccinesProtect))



chisqtest$parameter <- c(rep(4,9))

ggplot(data = r, aes(x = VaccinesImpForHeath)) +
  geom_bar();
