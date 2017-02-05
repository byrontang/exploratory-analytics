setwd("C:/Users/Byron/Documents/R projects/Project")
data1 <- read.csv("model_file.csv")

## Vehicle Market Overview
library(ggplot2)
attach(data1)
veh.class_by_year <- table(Veh.Class, year)
ggplot(data1,aes(x = year,fill = Veh.Class),geom="text") +
  geom_bar(position = "fill" ) +
  scale_fill_brewer(palette="YlGnBu")

## Graphs for interval variables (MPG)
highway <- data.frame(data1$Hwy.MPG.mean, 'Highway MPG')
city <- data.frame(data1$City.MPG.mean, 'City MPG')
combined <- data.frame(data1$Cmb.MPG.mean, 'Combined MPG')
name <- c('MPG', 'Type')
colnames(highway) <- name
colnames(city) <- name
colnames(combined) <- name
mpg3 <- rbind(highway,city,combined)
  # density plot
ggplot(mpg3, aes(mpg3$MPG, fill = mpg3$Type)) + 
  geom_density(alpha = 0.35) +
  labs(title="City, Highway, and Combined MPG\n", x="\nMPG", y="Type \n") + 
  theme(text = element_text(size=15)) + 
  scale_fill_brewer(palette="Set3", guide = guide_legend(title = NULL))
  # histogram
ggplot(mpg3, aes(x = mpg3$MPG, fill = mpg3$Type)) + 
  geom_histogram(alpha=0.35, position="identity",bins = 20)

## Quickly review MPG vs. other indexes
boxplot(Cmb.MPG.mean~year)
boxplot(Cmb.MPG.mean~Veh.Class)
boxplot(Cmb.MPG.mean~Transmission_number)
boxplot(Cmb.MPG.mean~Transmission_type)
boxplot(Cmb.MPG.mean~Fuel)

## Vehicle vs. Drivetrain Type
ggplot(data1, aes(x = year, fill = Drive)) + 
  geom_bar(alpha=0.6, position="fill",  stat = "count" ) + 
  labs(title="Drivetrain Type by Year", x="Drivetrain Type", y="Count") + 
  theme(text = element_text(size=14))
ggplot(data1, aes(x = Veh.Class, fill = Drive)) + 
  geom_bar(alpha=0.6, position="stack",  stat = "count" ) + 
  labs(title="Vehicle and Drivetrain Type", x="Drivetrain Type", y="Count") +
  theme(text = element_text(size=14))
data1_SUV <- data1[data1$Veh.Class == 'SUV', ]
ggplot(data1_SUV, aes(x = year, fill = Drive)) + 
  geom_bar(alpha=0.6, position="fill",  stat = "count" ) + 
  labs(title="SUV Drivetrain Type by Year", x="Drivetrain Type", y="Count") + 
  theme(text = element_text(size=14))

## Checking normality of MPG
  # Add normal curve
h <- hist(Cmb.MPG.mean)
x <- data1$Cmb.MPG.mean
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")
  # probability plot
qqnorm(x)
qqline(x, col = "blue")
  # Goodness of fit test of H0: normal
shapiro.test(Cmb.MPG.mean)

## Chi-square test for vehicle class vs. year
chisq.test(year, Veh.Class)
round((chisq.test(year, Veh.Class)$residual),2)

## ANOVA & Tukey test of performance by year
data1$year <- factor(data1$year)
performance_year <- aov(data1$Cmb.MPG.mean~data1$year)
summary(performance_year)
TukeyHSD(performance_year, conf.level = 0.95)

## ANOVA & Tukey test of performance by class
data1$Veh.Class <- factor(data1$Veh.Class)
performance_class <- aov(data1$Cmb.MPG.mean~data1$Veh.Class)
summary(performance_class)
TukeyHSD(performance_class, conf.level = 0.95)

## ANOVA & Tukey test of performance by transmission type
data1$Transmission_type <- factor(data1$Transmission_type)
performance_trans_type <- aov(data1$Cmb.MPG.mean~data1$Transmission_type)
summary(performance_trans_type)
TukeyHSD(performance_trans_type, conf.level = 0.95)

## ANOVA & Tukey test of performance by number of transmission
data1$Transmission_number <- factor(data1$Transmission_number)
performance_trans <- aov(data1$Cmb.MPG.mean~data1$Transmission_number)
summary(performance_trans)
TukeyHSD(performance_trans, conf.level = 0.95)

## ANOVA & Tukey test of performance by fuel type
data1$Fuel <- factor(data1$Fuel)
performance_fuel <- aov(data1$Cmb.MPG.mean~data1$Fuel)
summary(performance_fuel)
TukeyHSD(performance_fuel, conf.level = 0.95)

## ANOVA & Tukey test of performance by SmartWay
data1$SmartWay <- factor(data1$SmartWay)
performance_SmartWay <- aov(data1$Cmb.MPG.mean~data1$SmartWay)
summary(performance_SmartWay)
TukeyHSD(performance_SmartWay, conf.level = 0.95)

## ANOVA & Tukey test of performance by cylinder
data1$Cyl <- factor(data1$Cyl)
performance_cyl <- aov(data1$Cmb.MPG.mean~data1$Cyl)
summary(performance_cyl)
TukeyHSD(performance_cyl, conf.level = 0.95)

## Linear regression to test performance on Displ
linefit_d = lm(data1$Cmb.MPG.mean ~ data1$Displ)
summary(linefit_d)

## Visualize linear regression of performance vs. Displ
dat <- data.frame(Displacement = data1$Displ, MPG = data1$Cmb.MPG.mean)
ggplot(dat, aes(x=Displacement, y=MPG)) + geom_point(shape=1) + geom_smooth(method=lm)

## Performance vs. SmartWay
qplot(Cmb.MPG.mean, data=data1, fill = SmartWay, bins = 30)