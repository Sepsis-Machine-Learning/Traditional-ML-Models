Alkalinephos.sepsis <- read.csv("Alkalinephos.csv.sepsis.csv", header = TRUE)
Alkalinephos.no <- read.csv("Alkalinephos.csv.no.csv", header = TRUE)
ggplot(Alkalinephos.sepsis, aes(Time, Alkalinephos, color = SepsisLabel)) + geom_point() + geom_line(aes(group = Patient)) + labs(x = NULL, y = NULL) + scale_x_continuous(breaks = c(-350:0), labels = c(-350:0)) + 
  coord_cartesian(xlim = c(-30, 0), ylim = c(0, 1)) + geom_smooth(color = "red") + 
  labs(x = "Time", y = "Alkalinephos.sepsis", title = "Alkalinephos patient trend")

ggplot(Alkalinephos.no, aes(Time, Alkalinephos, color = SepsisLabel)) + geom_point() + geom_line(aes(group = Patient)) + labs(x = NULL, y = NULL) + scale_x_continuous(breaks = c(-350:0), labels = c(-350:0)) + 
  coord_cartesian(xlim = c(-30, 0), ylim = c(50, 80)) + geom_smooth(color = "red") + 
  labs(x = "Time", y = "Alkalinephos.no", title = "Alkalinephos patient trend")

Alkalinephos.sepsis$sign = 1
Alkalinephos.no$sign = 0
dataset = rbind(Alkalinephos.no, Alkalinephos.sepsis)
dataset$sign = factor(dataset$sign)
ggplot(dataset, aes(sign, Alkalinephos)) +geom_boxplot() + labs(title = "Alkalinephos no vs sepsis"
) + coord_cartesian(ylim = c(50, 130))

ggplot(Alkalinephos.no, aes(Alkalinephos)) +geom_bar()
