data(trees)
?trees

str(trees)

par(mfrow=c(1,2))

scatter.smooth(x=trees$Girth,
               y=trees$Volume,
               main="Volume ~ Girth")
scatter.smooth(x=trees$Height,y=trees$Volume,main="Volume ~ Height")


#density plots
install.packages("e1071")
library(e1071)

p=boxplot(trees$Girth)
density(trees$Girth)
plot(density(trees$Girth), main="Density Plot: Girth", ylab="Frequency",sub=paste("skewness:",round(e1071::skewness(trees$Girth),2)))
polygon(density(trees$Girth), col="red")

plot(density(trees$Height), main="Density Plot: Height", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(trees$Height), 2)))  # density plot for 'Height'
polygon(density(trees$Height), col="blue")


install.packages("GGally")
library(GGally)
ggpairs(data=trees, columns=1:3, title="trees data")

m1=lm(Volume ~ Girth, data = trees)
m1

summary(m1)
AIC(m1)
BIC(m1)

m2=lm(Volume ~ Height, data = trees)
summary(m2)
AIC(m2)
BIC(m2)
