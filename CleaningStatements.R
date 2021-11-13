multistring_data <- as.data.frame(responses$Curriculum)
multistring_data <- cbind(multistring_data, responses$Statements)
multistring_data <- cbind(multistring_data, responses$MotivationIncIf)


names(multistring_data) <- c('Curriculum','Statements','MotivationIncIf')
names(multistring_data)
vector <- multistring_data$Statements

head(sapply(multistring_data$MotivationIncIf, function(x){
  strsplit(x, ',')
}))

statements <- data.frame(row.names = f)
names(statements) <- f

for(i in 1:length(multistring_data$Statements)){
  vector <- multistring_data$Statements[i]
  vector <- unlist(strsplit(vector,", "))
  for(j in 1:length(vector)){
    # for(k in 1:13){
    #     if(vector[j]== names(statements)[k]){
    #       statements[i,k] <- 1
    #     } else{
    #     statements[i,k] <- 0
    #   }
    a <- rbind(a,as.numeric(vector[i]==names(statements)))
    a <- colSums(a)
  }
  statements <- rbind(statements, a)
  }


f <- as.factor(c(levels(as.factor(statements$V1)),"I am afraid the vaccine may cause autism",
                 "I am against vaccination in general"))
f

a<-matrix(,nrow = length(vector),ncol = 13)
a <- na.omit(a)
statements <- as.data.frame(goodmatrix)
for(j in 1:415){
    vector <- unlist(strsplit(multistring_data$Statements[j],', '))
    a<-matrix(,nrow = length(vector),ncol = 13)
    a <- na.omit(a)
        for(i in 1:length(vector)){
           a <- rbind(a,as.numeric(vector[i]==names(statements)))
        }
    a <- colSums(a)
    statements <- rbind(statements,a)
    names(statements) <- names(as.data.frame(goodmatrix))
}


names(statements) <- names(as.data.frame(goodmatrix))


statements <- cbind(multistring_data$Curriculum, statements)
View(statements)

write.csv(statements,"C:/Users/sheharyar.akhtar/Desktop/StatementsSparseMatrix.csv", row.names = F)


