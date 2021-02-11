library("ggplot2")

EPI_data <- read.csv("2010EPI_data.csv", skip = 1) # Get the col names

View(EPI_data)

attach(EPI_data)
fix(EPI_data)

EPI <- EPI_data$EPI

plot(ecdf(EPI), dp.point=FALSE, verticals=TRUE)

x <-seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t dsn")
qqline(x)

# Intercomparing <----------

DALY <- EPI_data$AIR_H
tf_a <- is.na(AIR_H) # Drop Null
E_a <- DALY[!tf_a]

plot(ecdf(E_a), do.points=FALSE, verticals=TRUE)

WATER_H = EPI_data$WATER_H
tf_h <- is.na(WATER_H)
E_h <- WATER_H[!tf_h]

plot(ecdf(E_h), do.points=FALSE, verticals=TRUE)

boxplot(E_a, E_h)

qqplot(E_a, E_h)

# Trying multivariate.csv
multivariate <- read.csv("multivariate.csv")
head(multivariate)
attach(multivariate)
mm <- lm(Homeowners~Immigrant)
summary(mm)$coef
