library(ggplot2)
library(dplyr)
library(plyr)
homeworkData = read.csv(file.choose(), header = T)

# 3
dCount = count(homeworkData, "Status")
dCountTotal = dCount[2]/sum(dCount[2])
pie1 = ggplot(dCount, aes(x=factor(1), y=dCountTotal))+
  geom_bar(width=1, stat='identity', color='white')+coord_polar(theta='y', start=0)+
  geom_text(aes(label=paste0(round(dCountTotal), "%")), position=position_stack(vjust=.5))
pie1
  
# 4
lifeScatter = ggplot(subset(homeworkData, Country == 'United States of America'), 
           aes(x = Year, y = Life.expectancy))+geom_point()
lifeScatter

# 5
homework5 = subset(homeworkData, Hepatitis.B != "NA", select = Hepatitis.B)
stem = stem(as.numeric(unlist(homework5)), scale = 1, width = 100, atom = 1e-08)

# 6
homework6 = subset(homeworkData, Year == '2015', select = Adult.Mortality)
boxplot(homework6, ylab = "Adult Mortality in 2015")
