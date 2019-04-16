library(Sleuth3)
library(ggplot2)
?case1002

fit_bats <- lm(log(Energy) ~ log(Mass) + Type, data = case1002)
summary(fit_bats)


View(case1002)

case1002$Type2 <- relevel(case1002$Type, ref = "non-echolocating bats")
fit_2 <- lm(log(Energy) ~ log(Mass) + Type2,data = case1002)
summary(fit_2)
confint(fit_2)

summary(fit_bats)
confint(fit_bats)

case1002$Type3 <- relevel(case1002$Type, ref = "non-echolocating birds")
fit_3 <- lm(log(Energy) ~ log(Mass) + Type3,data = case1002)
summary(fit_3)
confint(fit_3)

qplot('log(Mass)', .resid, data = fit_2)
qplot(Type2, .resid, data = fit_2)

qplot(sample =  .resid, data = fit_bats)