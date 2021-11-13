responses <- read.csv('responses.csv', stringsAsFactors = T)
str(responses)
View(responses)
responses$Timestamp <- NULL
responses$If.you.re.a.Non.Medical.Student..specify.your.major.or.discipline <- NULL
table(as.factor(responses$My.motivation.to.get.vaccinated.will.increase.IF..you.may.select.more.than.one.statement..))


names(responses) <- c('Participating','Gender','Age','Curriculum','Approval for Vaccine','RecoveredNeedVaccines','SOPsAfterCovid',
                      'VaccinesCanGiveCovid','TransmittingAfterVaccine','VaccinesImpForHeath','VaccinesAreEffective',
                      'VaccinesImpForComm','AllVaccinesAreGood','NewVaccinesRiskierthanOlder','InformationReliable','VaccinesProtect',
                      'ConcernedofSideEffects','NoVaccinesForUncommonDiseases','Statements','MotivationIncIf')

responses$VaccinesImpForHeath <- as.factor(responses$VaccinesImpForHeath)
responses$VaccinesAreEffective <- as.factor(responses$VaccinesAreEffective)
responses$VaccinesImpForComm <- as.factor(responses$VaccinesImpForComm)
responses$AllVaccinesAreGood <- as.factor(responses$AllVaccinesAreGood)
responses$NewVaccinesRiskierthanOlder <- as.factor(responses$NewVaccinesRiskierthanOlder)
responses$InformationReliable <- as.factor(responses$InformationReliable)
responses$VaccinesProtect <- as.factor(responses$VaccinesProtect)
responses$ConcernedofSideEffects <- as.factor(responses$ConcernedofSideEffects)
responses$NoVaccinesForUncommonDiseases <- as.factor(responses$NoVaccinesForUncommonDiseases)


write.csv(responses,"C:/Users/sheharyar.akhtar/Desktop/CleanVax", row.names = F)



nonmed <- subset(responses, responses$Curriculum == "Non-Medical")
str(nonmed)
med <- subset(responses, responses$Curriculum == 'Medical')
table(med$Curriculum)

med$Curriculum <- NULL
nonmed$Curriculum <- NULL
prop.table(table(med$Gender))
prop.table(table(nonmed$Gender))

med$Statements <- NULL
med$MotivationIncIf <- NULL
nonmed$Statements <- NULL
nonmed$MotivationIncIf <- NULL

str(med)
par(mfrow=c(2,1))
plot(nonmed$VaccinesAreEffective, ylab = 'Percentage', xlab = 'Belief of Effectiveness of Vaccines', main = 'Non Medical')
plot(med$VaccinesAreEffective, ylab = 'Percentage', xlab = 'Belief of Effectiveness of Vaccines', main = 'Medical')
par(mfrow=c(1,1))

summary(as.numeric(med))
summary(as.numeric(nonmed$VaccinesAreEffective))

plot(nonmed$VaccinesAreEffective, ylab = 'Percentage', xlab = 'Belief of Effectiveness of Vaccines', main = 'Non Medical')


summary(as.data.frame(lapply(med, as.numeric)))

num.med <- as.data.frame(lapply(med, as.numeric))
str(num.med)

num.med$Age<- as.factor(num.med$Age)
num.med$Gender<- as.factor(num.med$Gender)
num.med$Participating<- as.factor(num.med$Participating)
num.med$RecoveredNeedVaccines<- as.factor(num.med$Approval.for.Vaccine)
num.med$SOPsAfterCovid<- as.factor(num.med$SOPsAfterCovid)
num.med$VaccinesCanGiveCovid<- as.factor(num.med$VaccinesCanGiveCovid)
num.med$TransmittingAfterVaccine<- as.factor(num.med$TransmittingAfterVaccine)

summary(num.med)


num.nonmed <- as.data.frame(lapply(nonmed, as.numeric))
str(num.nonmed)

num.nonmed$Approval.for.Vaccine<- as.factor(num.nonmed$Approval.for.Vaccine)
num.nonmed$Age<- as.factor(num.nonmed$Age)
num.nonmed$Gender<- as.factor(num.nonmed$Gender)
num.nonmed$Participating<- as.factor(num.nonmed$Participating)
num.nonmed$RecoveredNeedVaccines<- as.factor(num.nonmed$Approval.for.Vaccine)
num.nonmed$SOPsAfterCovid<- as.factor(num.nonmed$SOPsAfterCovid)
num.nonmed$VaccinesCanGiveCovid<- as.factor(num.nonmed$VaccinesCanGiveCovid)
num.nonmed$TransmittingAfterVaccine<- as.factor(num.nonmed$TransmittingAfterVaccine)

summary(num.med)

pairs.panels(r[,], pch = 19)

r$Curriculum <- as.factor(r$Curriculum)
r$Approval.for.Vaccine<- as.factor(r$Approval.for.Vaccine)
r$Age<- as.factor(r$Age)
r$Gender<- as.factor(r$Gender)
r$Participating<- as.factor(r$Participating)
r$RecoveredNeedVaccines<- as.factor(r$Approval.for.Vaccine)
r$SOPsAfterCovid<- as.factor(r$SOPsAfterCovid)
r$VaccinesCanGiveCovid<- as.factor(r$VaccinesCanGiveCovid)
r$TransmittingAfterVaccine<- as.factor(r$TransmittingAfterVaccine)


num.med <- subset(r, r$Curriculum == "Medical")
num.nonmed <- subset(r,r$Curriculum=='NonMedical')

pairs.panels(r[,c('Curriculum','VaccinesImpForHeath',
                  'VaccinesAreEffective','InformationReliable',
                  'NewVaccinesRiskierthanOlder')])

summary(num.med)
summary(num.nonmed)

#------------------
boxplot(r$VaccinesImpForComm ~ r$Curriculum)
boxplot(r$VaccinesImpForHeath ~ r$Curriculum)
boxplot(r$VaccinesProtect ~ r$Curriculum)

plot(r$NewVaccinesRiskierthanOlder ~ r$Curriculum)
plot(r$InformationReliable ~ r$Curriculum)

r %>% 
  select(VaccinesProtect, Curriculum) %>% 
  filter(Curriculum == 'Medical')

#-------------ttest
t.test(data = r, AllVaccinesAreGood~ Curriculum)


library(ggplot2)
ggplot(r, aes(x = Curriculum, y = VaccinesImpForComm,
              col = VaccinesAreEffective,
              size = InformationReliable)) +
  geom_point(alpha = 0.2)+
  geom_boxplot()



ggplot(r, aes(x = Curriculum, y = VaccinesAreEffective, 
               size = VaccinesProtect, col= VaccinesImpForHeath)) + 
  geom_boxplot(alpha = 1)


corelations <- cor(as.data.frame(lapply(r[,c(a,'Curriculum')],as.numeric)))
View(corelations)
pairs.panels(r[,c(a,'Curriculum')])
r
str(r)




chisq.test(table(test2$Curriculum, test2$RecoveredNeedVaccines))
chisq.test(table(responses$Curriculum, responses$RecoveredNeedVaccines)es1[,1:9])




