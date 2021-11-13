library(tidyverse)
library(rstatix)
library(ggpubr)
 r %>%  sample_n(6)
a<-r
a.long <- a %>% 
  pivot_longer(-Curriculum, names_to = 'Variables', values_to = 'Values')

##Grouping data by variables and comparing curriculum

stat.test <- a.long %>% 
  group_by(Variables) %>% 
  chisq_test(Values ~ Curriculum) %>% 
  adjust_pvalue(method = 'fdr') %>% 
  add_significance()
stat.test
stat.test <- subset(stat.test, stat.test$p.adj.signif != ns)

# Create the plot
myplot <- ggboxplot(
  a.long, x = "Curriculum", y = "Values",
  fill = "Curriculum", palette = "npg", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~Variables)
# Add statistical test p-values
stat.test <- stat.test %>% add_xy_position(x = "Curriculum")
myplot + stat_pvalue_manual(stat.test, label = "p.adj.signif")

a<- stat.test$Variables

# Group the data by variables and do a graph for each variable
graphs <- a.long %>%
  group_by(Variables) %>%
  doo(
    ~ggboxplot(
      data =., x = "Curriculum", y = "Values",
      fill = "Curriculum", palette = "npg", legend = "none",
      ggtheme = theme_pubr()
    ), 
    result = "plots"
  )
graphs

Variables <- graphs$Variables
for(i in 1:length(Variables)){
  graph.i <- graphs$plots[[i]] + 
    labs(title = Variables[i]) +
    stat_pvalue_manual(stat.test[i, ], label = "p.adj.signif")
  print(graph.i)
}

write.csv(statix,"C:/Users/sheharyar.akhtar/Desktop/StatsForVax", row.names = T)

x <- a %>% 
  summarise_each(funs(chisq.test(., a$Curriculum)$p.value), -one_of('Curriculum'))
