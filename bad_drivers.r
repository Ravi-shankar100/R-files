data <- read.csv("bad_drivers.csv")
data
str(data)      #The structure of the data
data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles
data[3:4]
length(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)
pi = cbind(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding,data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired)
pi
greater = subset(data , Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding <
                 Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired)
greater
length(greater $Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding)

sort(data[2])

#Mean
m1 = mean(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)
m2 = mean(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding)
m3 = mean(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired)
m4 = mean(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted)
m5 = mean(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents)
m6 = mean(data$Car.Insurance.Premiums....)
m7 = mean(data$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....)

means = c(m1,m2,m3,m4,m5,m6,m7)
print(means)
  
#Variance
v1 = var(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)
v2 = var(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding)
v3 = var(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired)
v4 = var(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted)
v5 = var(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents)
v6 = var(data$Car.Insurance.Premiums....)
v7 = var(data$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....)

vars = c(v1,v2,v3,v4,v5,v6,v7)
print(vars)

#Median
median1 = median(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)
median2 = median(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding)
median3 = median(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired)
median4 = median(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted)
median5 = median(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents)
median6 = median(data$Car.Insurance.Premiums....)
median7 = median(data$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....)

medians = c(median1,median2,median3,median4,median5,median6,median7)
print(medians)

#Quantile
quantile1 = quantile(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)
quantile2 = quantile(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding)
quantile3 = quantile(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired)
quantile4 = quantile(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted)
quantile5 = quantile(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents)
quantile6 = quantile(data$Car.Insurance.Premiums....)
quantile7 = quantile(data$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....)

quantiles = c(quantile1,quantile2,quantile3,quantile4,quantile5,quantile6,quantile7)
print(quantiles)

#SD
sd1 = sd(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)
sd2 = sd(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding)
sd3 = sd(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired)
sd4 = sd(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted)
sd5 = sd(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents)
sd6 = sd(data$Car.Insurance.Premiums....)
sd7 = sd(data$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....)

sds = c(sd1,sd2,sd3,sd4,sd5,sd6,sd7)
print(sds)

#Summary
summary(data)

#Box Plot
boxplot(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles,xlab="billion miles")
boxplot(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding,xlab="Speeding")
boxplot(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired,xlab="Alcohol")
boxplot(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted,xlab = "Not Distracted")
boxplot(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents,xlab="Previous accidents")
boxplot(data$Car.Insurance.Premiums....,xlab="Car insurance premiums")
boxplot(data$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....,xlab="Loss for insurance company")

#Histogram
hist(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles,prob=TRUE,main="Number of drivers in collision per billion miles")
lines(density(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles,col= "green"))

hist(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding,prob=TRUE,main="percentage of drivers in collision of speeding")
lines(density(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding),col="green")

hist(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired,prob=TRUE,main="percentage of drivers in collision of alcohol impaired")
lines(density(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired),col="green")

hist(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted,prob=TRUE,main="percentage of drivers in collision who were not distracted")
lines(density(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted),col="green")

hist(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents,prob=TRUE,main="percentage of drivers in collison who were not involved in any previous accidents")
lines(density(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents),col="green")

hist(data$Car.Insurance.Premiums....,prob=TRUE,main="car insurance premiums")
lines(density(data$Car.Insurance.Premiums....),col="green")

hist(data$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....,prob=TRUE,main="losses incurred by insurance companies for insured drivers")
lines(density(data$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....),col="green")


#Bar Plot
barplot(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles,main="Number.of.drivers.involved.in.fatal.collisions.per.billion.miles",xlab = "number")
barplot(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding,main="Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding",xlab = "percentage")
barplot(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired,main="Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired",xlab = "percentage")
barplot(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted,main="Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted",xlab = "percentage")
barplot(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents,main="Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents",xlab = "percentage")
barplot(data$Car.Insurance.Premiums....,main="Car.Insurance.Premiums....",xlab = "insurance")
barplot(data$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....,main = "Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....",xlab = "Losses incurred by insurance")

#Pie Chart
get_table <- table(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)
get_table
pie(get_table)

get_table <- table(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding)
get_table
pie(get_table)

get_table <- table(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired)
get_table
pie(get_table)

get_table <- table(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted)
get_table
pie(get_table)

get_table <- table(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents)
get_table
pie(get_table)

get_table <- table(data$Car.Insurance.Premiums....)
get_table
pie(get_table)

get_table <- table(data$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....)
get_table
pie(get_table)


#Column comparisons along with the countries
subdata = data[c(1,2,3)]
greater = subset(subdata,Number.of.drivers.involved.in.fatal.collisions.per.billion.miles >
                   Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding)
greater

subdata = data[c(1,3,4)]
greater = subset(subdata,Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding >
                   Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired)
greater

subdata = data[c(1,4,5)]
greater = subset(subdata,Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired >
                   Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted)
greater

subdata = data[c(1,5,6)]
greater = subset(subdata,Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted >
                   Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents)
greater


#Joining Density Curves

plot(density(data$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles))
lines(density(data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding))


