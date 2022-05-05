library(mclust);library(caret); library(ggplot2); library(gridExtra)
filepath <- "http://archive.ics.uci.edu/ml/machine-learning-databases/
breast-cancer-wisconsin/wdbc.data"


my_data <- read.csv(filepath, header = FALSE)


X <- my_data[,c(4, 26, 27)]


colnames(X) <- c('texture.mean', 'area.extreme', 'smoothness.extreme')


Class <- my_data[,2]

par(mfrow = c(1,3))

g1 <- ggplot(X, aes(x=Class, y=texture.mean, fill=Class)) +
  geom_boxplot() + ggtitle("Texture Mean")
g2 <- ggplot(X, aes(x=Class, y=area.extreme, fill=Class)) +
  geom_boxplot() + ggtitle("Area Extreme")
g3 <- ggplot(X, aes(x=Class, y=smoothness.extreme, fill=Class)) +
  geom_boxplot() + ggtitle("Smoothness Extreme")
grid.arrange(g1, g2, g3, nrow = 1)

set.seed(123)

intrain <- createDataPartition(Class, p = 2/3, list = FALSE)
X.train <- X[intrain, ]
Class.train <- Class[intrain]

pander(table(Class.train))

X.test <- X[-intrain, ]
Class.test <- Class[-intrain]

pander(table(Class.test))

mod1 <- MclustDA(X.train, Class.train, modelType = 'EDDA')

summary(mod1, newdata = X.test, newclass = Class.test)


cv <- cvMclustDA(mod1) 
unlist(cv[c('error', 'se')])

par(mfrow = c(2,2))
plot(mod1, what = 'scatterplot', dimens = c(1,2)) 
plot(mod1, what = 'scatterplot', dimens = c(2,3)) 
plot(mod1, what = 'scatterplot', dimens = c(3,1))

mod2 <- MclustDA(X.train, Class.train)
summary(mod2, newdata = X.test, newclass = Class.test)

cv <- cvMclustDA(mod2) 
unlist(cv[c('error', 'se')])

par(mfrow = c(1,4))
plot(mod2, what = 'scatterplot', dimens = c(1,2), main = '(a)') 
plot(mod2, what = 'scatterplot', dimens = c(2,3), main = '(b)') 
plot(mod2, what = 'scatterplot', dimens = c(3,1), main = '(c)')

drmod2 <- MclustDR(mod2)
summary(drmod2)
par(mfrow = c(1,1))
plot(drmod2, what = 'boundaries', ngrid = 200, main = '(d)')

mod3 <- MclustDA(X.train, Class.train, G = 2, modelNames = 'EEE')
summary(mod3, newdata = X.test, newclass = Class.test)
par(mfrow = c(1,3))
plot(mod3, what = 'scatterplot', dimens = c(1,2)) 
plot(mod3, what = 'scatterplot', dimens = c(2,3)) 
plot(mod3, what = 'scatterplot', dimens = c(3,1))


