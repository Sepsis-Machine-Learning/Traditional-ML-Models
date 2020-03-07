setwd("Downloads/Data_science")
roc = read.csv("ROC.score.csv")

ggplot(roc, aes(x=Model.Name, y=ROC.AUC)) + geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
