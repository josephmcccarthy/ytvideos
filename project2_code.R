library(ggplot2)
library(dplyr)
library(plyr)
project = read.csv(file.choose(), header = T)

getMode = function(input){
  uniq = unique(input)
  uniq[which.max(tabulate(match(input, uniq)))]
}

#views
top15_v = subset(project, project$views > 149376127)
ggplot(top15_v, aes(x="views", y=top15_v$views))+geom_boxplot()+
  labs(title="boxplot for top 15 views" ,x="", y = "views")+theme_classic()
mean(project$views)
median(project$views)
getMode(project$views)
sd(project$views)
range(project$views)
  
#likes
top15_l = subset(project, project$likes > 4776680)
ggplot(top15_l, aes(x="likes", y=top15_l$likes))+geom_boxplot()+
  labs(title="boxplot for top 15 likes" ,x="", y = "likes")+theme_classic()
mean(project$likes)
median(project$likes)
getMode(project$likes)
sd(project$likes)
range(project$likes)
var(project$likes)
  
#dislikes
top15_d = subset(project, project$dislikes > 487820)
ggplot(top15_d, aes(x="dislikes", y=top15_d$dislikes))+geom_boxplot()+
  labs(title="boxplot for top 15 dislikes" ,x="", y = "dislikes")+theme_classic()
mean(project$dislikes)
median(project$dislikes)
getMode(project$dislikes)
range(project$dislikes)
sd(project$dislikes)
var(project$dislikes)

#comments_disabled
ggplot(project, aes(comments_disabled, fill = comments_disabled))+geom_bar()+coord_flip()
comments_subset = subset(project, comments_disabled == "True")
getMode(project$comments_disabled)
mean(comments_subset$views)
median(comments_subset$views)
getMode(comments_subset$views)
sd(comments_subset$views)
var(comments_subset$views)

#ratings_disabled
ggplot(project, aes(ratings_disabled, fill = ratings_disabled))+geom_bar()+coord_flip()
ratings_subset = subset(project, ratings_disabled == "True")
getMode(project$ratings_disabled)
mean(ratings_subset$views)
median(ratings_subset$views)
getMode(ratings_subset$views)
sd(ratings_subset$views)
var(ratings_subset$views)
