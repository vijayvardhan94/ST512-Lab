 
# The dataset ex0824 from Sleuth3 contains the age and respiratory rate for 618 children.

library(Sleuth3)
library(ggplot2)
head(ex0824)

# Fit a linear regression model of Rate on Age and examine the three diagnostic plots.

Data = ex0824
lin_mod = lm(Rate ~ Age, data = Data)
summary(lin_mod)

# Is there evidence any of the assumptions are violated?

Data$res1 = residuals(lin_mod)
Data$fit1 = predict(lin_mod)

# Diagnostic Plot 1: Residuals versus fitted values
plot(Data$fit1, Data$res1, xlab = "Fitted Values", ylab = "Residuals", cex = 0.8)
abline(h = 0)

# Or in ggplot2
qplot(fit1, res1, data = Data) + geom_hline(aes(yintercept = 0))

# Diagnostic Plot 2: Residuals versus age
plot(Data$Age, Data$res1, xlab = "Age", ylab = "Residuals", cex = 0.8)
abline(h = 0)

# Or in ggplot2
qplot(Age, res1, data = Data) + geom_hline(aes(yintercept = 0))

# Diagnostic Plot 3: Normal probability plot of residuals
qqnorm(Data$res1); qqline(Data$res1)

# Now try fitting a linear regression of log(Rate) on Age.

Data$log.rate = log(Data$Rate)
Data$Age2 = (Data$Age)^2 # Create quadratic term for practice
lin_mod2 = lm(log.rate ~ Age, data = Data)
summary(lin_mod2)

# Do the residual plots look better?

Data$res2 = residuals(lin_mod2)
Data$fit2 = predict(lin_mod2)

# Diagnostic Plot 1: Residuals versus fitted values
plot(Data$fit2, Data$res2, xlab = "Fitted Values", ylab = "Residuals", cex = 0.8)
abline(h = 0)

# Or in ggplot2
qplot(fit2, res2, data = Data) + geom_hline(aes(yintercept = 0))

# Diagnostic Plot 2: Residuals versus age
plot(Data$Age, Data$res2, xlab = "Age", ylab = "Residuals", cex = 0.8)
abline(h = 0)

# Or in ggplot2
qplot(Age, res2, data = Data) + geom_hline(aes(yintercept = 0))

# Diagnostic Plot 3: Normal probability plot of residuals
qqnorm(Data$res2); qqline(Data$res2)

# How do you interpret the slope parameter now the response is log transformed?
# (Read Section 8.4 in Sleuth)


                            