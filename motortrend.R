par(mfrow = c(1, 2))
g1 <- qplot(mpg, am, data = mtcars, main = "Mileage by transmission", xlab = "Miles per Gallon",
            ylab = "0 = automatic, 1 = manual")
g2 <- qplot(mpg, am, data = mtcars, colour = gear)
g3 <- qplot(mpg, am, data = mtcars, size = gear)
g3bis <- qplot(am, mpg, data = mtcars, size = cyl, xlab = "Miles per Gallon",
               ylab="0 = automatic, 1 = manual", main = "Cylinders effect on mpg")
g3bis2 <- qplot(am, mpg, data = mtcars, size = carb, main = "Carburetors effect on mpg",
                xlab = "Miles per Gallon", ylab="0 = automatic, 1 = manual")
g3bis3 <- qplot(am, mpg, data = mtcars, size = wt, main = "Weight effect on mpg",
                xlab = "Miles per Gallon", ylab="0 = automatic, 1 = manual")

g4 <- qplot(factor(am), mpg, data=mtcars, geom=c("boxplot", "jitter"),
            fill = am, main="Mileage by Transmission",
            xlab="0 = automatic, 1 = manual", ylab="Miles per Gallon")

grid.arrange(g1, g3, ncol = 2, nrow = 1)
grid.arrange(g1, g3, ncol = 1, nrow = 2)
grid.arrange(g3bis, g3bis2, g3bis3, ncol = 3, nrow = 1)

plot(factor(am), mpg, data = mtcars, geom = "boxplot")

g5 <- qplot(factor(am), mpg, data = mtcars, geom = "boxplot", fill = gear)
g6 <- qplot(mpg, am, data = mtcars, size = gear)
g7 <- qplot(mpg, am, data = mtcars, size = gear, geom = "smooth")
g8 <- qplot(mpg, am, data = mtcars, size = gear, geom = c("point", "smooth"))

#binomial regression
mdl <- glm(am ~ mpg, family = "binomial", mtcars)
summary(mdl)
par(mfrow = c(2, 2))
plot(mdl)
exp(mdl$coefficients)
exp(confint(mdl))

mdl2 <- glm(am ~ mpg + gear, family = "binomial", mtcars)
summary(mdl2)

par(mfrow = c(1, 2))
with(mtcars, plot(mpg, am, xlab = "Miles per Galon", ylab = "Type of Transmission", pch = 16))
curve(predict(mdl, data.frame(mpg=x), type = "resp"), add = TRUE, col = "red", lwd = 2)

with(mtcars, plot(gear, am, xlab = "Number of gears", ylab = "Type of Transmission", pch = 16))
curve(predict(mdl2, data.frame(mpg=x, gear=x), type = "resp"), add = TRUE, col = "blue", lwd = 2)

#linear regression
fit <- lm(mpg ~ am, mtcars)
fit2 <- lm(mpg ~ ., mtcars)
fit3 <- step(lm(mpg ~ ., mtcars), trace = 0)
par(mfrow = c(1, 3))
plot(fit, which = 1)
title(main = "Linear regression mpg vs am", cex = 0.75)
plot(fit2, which = 1)
title(main = "Linear regression mpg vs all", cex = 0.75)
plot(fit3, which = 1)
title(main = "Step model", cex = 0.75)
fit4 <- lm(mpg ~ am * factor(gear), mtcars)