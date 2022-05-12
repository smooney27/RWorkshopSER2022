
# Slide 2: Review Challenge
# Load the USArrests dataset (built into R) and print out the list
# of states, sorted by murder arrest rate 

# Slide 3
dig <- read.csv("http://www.columbia.edu/~sjm2186/EPIC_R/dig.csv")
lm(HEARTRTE ~ TRTMT + SEX, data=dig)

# Slide 4
model <- lm(SYSBP ~ DIABP + SEX, data=dig)
str(model)
summary(model)
coef(model) 
confint(model)
anova(model)
residuals(model)
predict(model)

# Slide 5 
install.packages("car", dependencies = T)
library(car)
data(Duncan)
model<-lm(prestige~income, data=Duncan)
qqPlot(model)
infIndexPlot(model)	
infIndexPlot(model, "Cook")

# Slide 6
plot(Duncan$income, Duncan$prestige)
identify(Duncan$income, Duncan$prestige) 
Duncan[c(6, 16),]

abline(model)
plot(model)

# Slide 7
model <- glm(DEATH ~ DIABP + TRTMT, 
		data=dig, family=binomial)
model
summary(model)

# Slide 8
model <- glm(DEATH ~ DIABP + TRTMT, data=dig) # wrong, sort of
summary(model)

model <- glm(DEATH ~ DIABP + TRTMT, 
		data=dig, family=binomial)
summary(model)

# Slide 9: Challenge 
# Load the Duncan dataset (reminder: it's in the car package)
# Estimate the average increase in prestige associated with a one-unit increase in education


# Slide 10: Challenge 
# Using the Duncan dataset, find the occupation that has the largest 
# prestige bump not predicted by education  (i.e. has most positive residual)


# Slide 12
plot(Duncan$income)
plot(Duncan$type)

# Slide 13
hist(Duncan$income)
hist(Duncan$income, breaks=seq(0,100, 5))


# Slide 14
plot(Duncan$income, Duncan$prestige)
plot(Duncan$prestige, Duncan$income)
plot(y=Duncan$prestige, x=Duncan$income)
plot(Duncan$income, Duncan$prestige, type="l")

# Slide 15
title("Income and Prestige")
plot(Duncan$income, Duncan$prestige, main="Income and Prestige")

hist(Duncan$income)
hist(Duncan$income, main="")
title("I decide the title, yo!")

# Slide 16
plot(Duncan$income, Duncan$prestige)
plot(Duncan$income, Duncan$prestige, xlab="Income", ylab="Prestige", xlim=c(0,100))


# Slide 17
Duncan$high_education <- Duncan$education > 50
table(Duncan$high_education)
plot(Duncan$high_education, Duncan$education)

# Slide 18
Duncan$plot_color <- ifelse(Duncan$high_education, "red", "blue")
plot(Duncan$income, Duncan$prestige, col=Duncan$plot_color)


# Slide 19
plot(Duncan$type, Duncan$income)
class(Duncan$type)
plot(as.numeric(Duncan$type), Duncan$income)


# Slide 20: Challenge
# Use the Duncan dataset to make a box-and-whiskers plot 
# titled “Income by Job Type” wherein type is on the x 
# axis, income is on the y axis, and the y axis runs from 0 to 100


# Slide 22
library(ggplot2)
qplot(Duncan$income, Duncan$prestige)

# Slide 23
ggplot(Duncan) + aes(x=income, y=prestige) + geom_point()
ggplot(Duncan) + aes(x=income, y=prestige, color=education) + geom_point()


# Slide 24
p <- ggplot(Duncan)
p <- p + aes(x=income, y=prestige, color=high_education)
p + geom_point()

ggplot(Duncan) + aes(x=income, y=prestige, 
                     color=high_education) + geom_point()


# Slide 26: Challenge
# Use ggplot to make a scatterplot of prestige vs. income in the 
# Duncan dataset, making size proportional to education and color set by type
