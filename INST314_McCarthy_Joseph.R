homeworkData = read.csv(file.choose(), header=T)

attach(homeworkData)
class(DISTANCE)
Distance = as.factor(DISTANCE)
median(DISTANCE)
class(FARE)
Fare = as.factor(FARE)

# q1
southwest1 = subset(homeworkData, SW == 'Yes', select = FARE)
Southwest1 = as.numeric(unlist(southwest1))
mean(Southwest1)

# q2
southwest2 = subset(homeworkData, SW == 'No', select = FARE)
Southwest2 = as.numeric(unlist(southwest2))
mean(Southwest2)

# q3
Southwest3 = mean(Southwest2) - mean(Southwest1)
Southwest3

# q4
southwest4 = subset(homeworkData, SW == 'Yes')
proportion4 = nrow(southwest4)/nrow(homeworkData)
proportion4

# q5
southwest5 = subset(homeworkData, SW == 'Yes' & DISTANCE <= 500, select = DISTANCE)
southwest5_2 = subset(homeworkData, SW == 'No'& DISTANCE <= 500, select = DISTANCE)
proportion5 = nrow(southwest5)/nrow(southwest5_2)
proportion5
Southwest5_3 = as.numeric(unlist(southwest5))
mean(Southwest5_3)
median(Southwest5_3)
getMode = function(input){
  uniq = unique(input)
  #matching match(x,y) x = data, y = levels
  uniq[which.max(tabulate(match(input, uniq)))]
}
getMode(Southwest5_3)

# q6
southwest6 = subset(homeworkData, SW == 'Yes' & DISTANCE > 1000, select = DISTANCE)
southwest6_2 = subset(homeworkData, SW == 'No'& DISTANCE > 1000, select = DISTANCE)
proportion6 = nrow(southwest5)/nrow(southwest6_2)
proportion6
Southwest6_3 = as.numeric(unlist(southwest6))
mean(Southwest6_3)
median(Southwest6_3)
getMode = function(input){
  uniq = unique(input)
  uniq[which.max(tabulate(match(input, uniq)))]
}
getMode(Southwest6_3)

# q7  
southwest7 = subset(homeworkData, SW == 'Yes', select = DISTANCE)
southwest7_2 = as.numeric(unlist(southwest7))
range(southwest7)

library(ggplot2)
library(dplyr)
library(plyr)

# pie chart   
eCount = count(homeworkData, "E_CITY")
eCountTotal = eCount[2]/sum(eCount[2])
pie1 = ggplot(eCount, aes(x=factor(1), y=freq, fill=factor(E_CITY)))+
    geom_bar(width=1, stat='identity', color='white')+coord_polar(theta='y', start=0)+
    geom_text(aes(label=paste0(round(freq), "%")), position=position_stack(vjust=.5))
pie1
eCount2 = count(homeworkData, "S_CITY")
eCountTotal2 = eCount2[2]/sum(eCount2[2])
pie2 = ggplot(eCount2, aes(x=factor(1), y=freq, fill=factor(S_CITY)))+
    geom_bar(width=1, stat='identity', color='white')+coord_polar(theta='y', start=0)+
    geom_text(aes(label=paste0(round(freq), "%")), position=position_stack(vjust=.5))
pie2
  

# scatterplot
a = ggplot(subset(homeworkData, SW == 'Yes'), aes(x= DISTANCE, y = as.numeric(unlist(FARE))))+
  geom_point(colour = 'red', shape  = 2)
a
b = ggplot(subset(homeworkData, SW == 'No'), aes(x= DISTANCE, y = as.numeric(unlist(FARE))))+
  geom_point(colour = 'blue', shape  = 3)
b
c = ggplot(homeworkData, aes(x= DISTANCE, y = as.numeric(unlist(FARE)), colour = SW, shape = SW))+
  geom_point()
c

# box plots
boxFlights = ggplot(subset(homeworkData), aes(x = homeworkData$SW, y = DISTANCE))+geom_boxplot()
boxFlights

#bar graph
d = ggplot(homeworkData, aes(E_CITY, fill = E_CITY))+geom_bar()+coord_flip()
d
