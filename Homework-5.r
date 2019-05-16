#load libraries and datasets
library('Sleuth3')
head(ex1320)
ex1320

#exploratory part
nrow(ex1320)
names(ex1320)

#1a,b,c,d,e,f all below
pfrow = c(2,1)

boxplot(Score~Sex,data=ex1320, main="Scores by Sex",
        xlab="Gender",col = c(663, 456), ylab="ACT math score") 

boxplot(Score~Background,data=ex1320, main="Scores by Background",
        xlab="Background", col = c(508,448,638), ylab="ACT math score") 

##?aggregate
twoway <- aggregate(Score ~ Sex + Background, data = ex1320, FUN = mean)
#table(twoway)


#table(aggregate(Score ~ Sex + Background, data = ex1320, FUN = mean))
#sumtable <- table(ex1320$Sex, ex1320$Background)
#sumtable
#addmargins(sumtable, c(1,2), c(mean,mean))
?addmargins

xtabs(Score ~ Sex + Background, twoway)
difference <-  xtabs(Score ~ Sex + Background, twoway)
addmargins(difference, 1, diff)

fit_add <- lm(Score ~ Sex + Background, data = ex1320)
summary(fit_add)
anova(fit_add)


fit_sat <- lm(Score ~ Sex + Background + Sex:Background, data = ex1320)
summary(fit_sat)
anova(fit_sat)
anova(fit_add, fit_sat)

library(ggplot2)
new_data <- expand.grid(Sex = unique(ex1320$Sex),Background = unique(ex1320$Background))
new_data$pred <- predict(fit_add, newdata =new_data)

round(xtabs(pred ~ Sex + Background, new_data), 2) # a table
qplot(reorder(Background, pred), pred, data = new_data,colour = Sex, geom = "line", group = Sex)

new_data$pred_sat <- predict(fit_sat, newdata = new_data)
round(xtabs(pred_sat ~ Sex + Background, new_data), 2)
qplot(reorder(Background, pred_sat), pred_sat, data = new_data,colour = Background, geom = "line", group = Sex)