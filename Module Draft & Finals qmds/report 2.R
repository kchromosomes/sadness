#report 2
#loading packages needed for this exercise
library(ggplot2)
library(stats)
library(summarytools)
library(sjstats)
library(pwr)


#Setting factors
#categorical
housefinch$parktype <- as.factor(housefinch$parktype)
str(housefinch$parktype)

#continuous
housefinch$density <- as.numeric(housefinch$density)
housefinch$flocksize <- as.numeric(housefinch$flocksize)
str(housefinch$density)
str(housefinch$flocksize)

#Histogram for continuous factors
ggplot(housefinch, aes(x=housefinch$density)) + geom_histogram(binwidth = 7)

#normal probability plots per factor
qqnorm(housefinch$density) 
qqline(housefinch$density)

qqnorm(housefinch$flocksize)
qqline(housefinch$flocksize)

#descriptive stats for entire model
descr(housefinch, style = "rmarkdown" )

# table for exporting
desc <- descr(housefinch, style = "rmarkdown")
desc <- as.table(desc)
desc

#figure to show relationships
plot(housefinch$density, housefinch$flocksize, col = as.factor(housefinch$parktype), pch = 19, main = "Flock size & Density per Habitat Type", xlab = "density", ylab = "flock size")
legend("bottomright", legend = c("Rural", "Urban"), col = as.factor(housefinch$parktype), lty = 1)

#Assumptions check of current model

model2 <- lm(data = housefinch, flocksize ~ density)

summary(model2)
anova(model2, type = 3)
anova_stats(model2)


#To provide r for each factor
cor.test(housefinch$density, housefinch$flocksize)

#power tests
#Both Type 1 & 2 errors are 5%
pwr.r.test(n=6, sig.level = 0.05, power = 0.95)


#publication figure
ggplot(data = housefinch, aes(x = density, y = flocksize)) + geom_point() + geom_smooth(method = "loess", se = TRUE, level = 0.95) + xlab("Density") + ylab("Flock Size") + labs(title = "Finch Flock size & Density")


                   


