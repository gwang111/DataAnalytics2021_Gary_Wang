library("readxl")

#Exercise 1
EPI_data <- read.csv("2010EPI_data.csv", skip = 1) # Get the col names

View(EPI_data)

attach(EPI_data)
fix(EPI_data)

EPI <- EPI_data$EPI

tf <- is.na(EPI) # Drop Null
E <- EPI[!tf]

# Exercise 1: exploring the distribution <=======
summary(EPI)
fivenum(EPI, na.rm=TRUE)
#Plots
stem(EPI)
hist(EPI)
hist(EPI, seq(30.,95.,1.0), probability = TRUE)
lines(density(EPI, na.rm=TRUE, bw=1.)) #lines(density(EPI, na.rm=TRUE, bw="SJ"))
rug(EPI)

# Exercise 1 fitting a distribution beyond histograms <=======

# Cumulative density
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)

# Quantile-Quantile
par(pty="s")
qqnorm(EPI); qqline(EPI)

#Q-Q against the generating distribution
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# Exercise 1: fitting a distribution using DALY & WATER_H
DALY <- EPI_data$DALY
tf_d <- is.na(DALY) # Drop Null
E_d <- DALY[!tf_d]

plot(ecdf(E_d), do.points=FALSE, verticals=TRUE)

WATER_H = EPI_data$WATER_H
tf_h <- is.na(WATER_H)
E_h <- WATER_H[!tf_h]

plot(ecdf(E_h), do.points=FALSE, verticals=TRUE)

#Comparing distributions
boxplot(EPI, DALY)
qqplot(EPI, DALY)

# Tried to find 2016 csv but cannot find the file that matches 2010
EPI_2016 <- read_excel("2016EPI_Raw_Data.xls") # Not the correct data set
View(EPI_2016)

#Distributions
help("distributions")

# Exercise 2: filtering (populations) <=======
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)

#Plotting
plot(ecdf(Eland), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(Eland); qqline(Eland)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn for Eland")
qqline(x)

boxplot(Eland, E_d)
qqplot(Eland, E_d)

boxplot(Eland, E_h)
qqplot(Eland, E_h)

# Check for desert
EPIDesert <- EPI[Desert]
View(EPIDesert)

# Testing on EPI_South_Asia
EPISouthAsia <- EPI[EPI_regions %in% c("South Asia")] # More suited if multiple filters
EPISouthAsia

# Exercise 2 Looking through GRUMP <=======
GRUMP_data <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")

attach(GRUMP_data)
fix(GRUMP_data)

GrumpUnits <- GRUMP_data$NumUnits

tf <- is.na(GrumpUnits)
GUnits <- GrumpUnits[!tf]

plot(GUnits)
hist(GUnits)

par(pty="s")
qqnorm(GUnits); qqline(GUnits)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn for GUnits")
qqline(x)

GFilter <- GrumpCont[ContinentName %in% c("Asia")]
GFilter

plot(GFilter)
hist(GFilter)

# Water Treatment !Ran out of Time!


