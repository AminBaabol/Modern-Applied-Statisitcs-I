 
  ## Quantile Regression
  
library(gamlss.data)
library(lattice)
library("quantreg")
## Head Circumference for Age
#library(gamlss.data)
data(db)
head(db)
dim(db)
plot(db$head ~ db$age, xlab = "Age(months)", 
     ylab = "Head circumference",
     pch = 16, cex = 0.5, col = "gray")


## Dutch boys head circumference
layout(matrix(1:2, nrow = 1))
hist(db$age)
hist(db$head)


## Head Circumference for Age
#library(gamlss.data)
data(db)
head(db, n = 3)
plot(db$head ~ db$age, xlab = "Age(years)", ylab = "Head circumference
     (cm)",
     pch = 16, cex = 0.5, col = "gray")

## Dutch boys head circumference
db <- db[db$age>2,] # subset data by age>2
summary(db)

## Dutch boys head circumference
#add a cut variable in data to subset data

db$cut <- cut(db$age, breaks = c(2, 9, 23),
              labels = c("2-9 yrs", "9-23 yrs"))

#different scatterplot by age group

xyplot(head ~ age | cut, data = db, xlab = "Age (years)",
       ylab = "Head circumference (cm)",
       scales = list(x = list(relation = "free")),
       layout = c(2, 1), pch = 19,
       col = rgb(.1, .1, .1, .1))


## Dutch boys head circumference
#Simple linear regression model by age group
lm2.9 <- lm(head ~ age, data = db, subset = age < 9)
lm2.9$coef 

lm9.23 <- lm(head ~ age, data = db, subset = age > 9)
lm9.23$coef
#Equivalent to 
lm_mod <- lm(head ~ age:I(age < 9) + I(age < 9) - 1,
             data = db)
lm_mod$coef
## Dutch boys head circumference

## Dutch boys head circumference - rq
rq_med2.9 <- rq(head ~ age, data = db, tau = 0.5,
                subset = age < 9)
rq_med2.9$coef
rq_med9.23 <- rq(head ~ age, data = db, tau = 0.5,
                 subset = age > 9)
rq_med9.23$coef
#Calculate Confidence intervals 
cbind(coef(lm2.9)[1],confint(lm2.9, parm = "(Intercept)"))
cbind(coef(lm2.9)[2],confint(lm2.9, parm = "age"))
options(warn=-1)# turns off warning message
summary(rq_med2.9, se = "rank")$coef
options(warn=0)# turns on warning message



## Dutch boys head circumference - growth curve lm
tau <- c(.01, .1, .25, .5, .75, .9, .99)
gage <- c(2:9, 9:23)
i <- 1:8
idf <- data.frame(age = gage[i])
p <- predict(lm2.9, newdata = idf, level = 0.5,
             interval = "prediction") # level - coverage
colnames(p) <- c("0.5", "0.25", "0.75")
p
## Dutch boys head circumference
#Find 80\% and 98% prediction intervals 
p <- cbind(p, predict(lm2.9, newdata = idf, level = 0.8,
                      interval = "prediction")[,-1])
colnames(p)[4:5] <- c("0.1", "0.9")
p <- cbind(p, predict(lm2.9, newdata = idf, level = 0.98,
                      interval = "prediction")[,-1])
colnames(p)[6:7] <- c("0.01", "0.99")
p2.9 <- p[, c("0.01", "0.1", "0.25", "0.5",
              "0.75", "0.9", "0.99")]
head(p2.9)

## Dutch boys head circumference
#Repeate the same for older boys
idf <- data.frame(age = gage[-i])
p <- predict(lm9.23, newdata = idf, level = 0.5,
             interval = "prediction")
colnames(p) <- c("0.5", "0.25", "0.75")
p <- cbind(p, predict(lm9.23, newdata = idf, level = 0.8,
                      interval = "prediction")[,-1])
colnames(p)[4:5] <- c("0.1", "0.9")
p <- cbind(p, predict(lm9.23, newdata = idf, level = 0.98,
                      interval = "prediction")[,-1])
colnames(p)[6:7] <- c("0.01", "0.99")
p9.23 <- p[, c("0.01", "0.1", "0.25", "0.5",
               "0.75", "0.9", "0.99")]
p9.23
q2.23 <- rbind(p2.9, p9.23)
head(round(q2.23, 3), n = 14)

## Dutch boys head circumference - plot
pfun <- function(x, y, ...) {
  panel.xyplot(x = x, y = y, ...)
  if (max(x) <= 9) {
    apply(q2.23, 2, function(x)
      panel.lines(gage[i], x[i]))
  } else {
    apply(q2.23, 2, function(x)
      panel.lines(gage[-i], x[-i]))
  }
  panel.text(rep(max(db$age), length(tau)),
             q2.23[nrow(q2.23),], label = tau, cex = 0.9)
  panel.text(rep(min(db$age), length(tau)),
             q2.23[1,], label = tau, cex = 0.9)
}
xyplot(head ~ age | cut, data = db, xlab = "Age (years)",
       ylab = "Head circumference (cm)", pch = 19,
       scales = list(x = list(relation = "free")),
       layout = c(2, 1), col = rgb(.1, .1, .1, .1),
       panel = pfun)

## Dutch boys head circumference
pfun <- function(x, y, ...) {
  panel.xyplot(x = x, y = y, ...)
  if (max(x) <= 9) {
    apply(q2.23, 2, function(x)
      panel.lines(gage[i], x[i]))
  } else {
    apply(q2.23, 2, function(x)
      panel.lines(gage[-i], x[-i]))
  }
  panel.text(rep(max(db$age), length(tau)),
             q2.23[nrow(q2.23),], label = tau, cex = 0.9)
  panel.text(rep(min(db$age), length(tau)),
             q2.23[1,], label = tau, cex = 0.9)
}
xyplot(head ~ age | cut, data = db, xlab = "Age (years)",
       ylab = "Head circumference (cm)", pch = 19,
       scales = list(x = list(relation = "free")),
       layout = c(2, 1), col = rgb(.1, .1, .1, .1),
       panel = pfun)

## Dutch boys head circumference - growth curves rq
rq2.9 <- rq(head ~ age, data = db, tau = tau,
            subset = age < 9)
rq2.9$coef
rq9.23 <- rq(head ~ age, data = db, tau = tau,
             subset = age > 9)
rq9.23$coef

## Dutch boys head circumference- growth curves rq
p2.23 <- rbind(predict(rq2.9,
                       newdata = data.frame(age = gage[i])),
               predict(rq9.23,
                       newdata = data.frame(age = gage[-i])))
head(p2.23)

## Dutch boys head circumference - growth curves rq
pfun <- function(x, y, ...) {
  panel.xyplot(x = x, y = y, ...)
  if (max(x) <= 9) {
    apply(q2.23, 2, function(x)
      panel.lines(gage[i], x[i], lty = 2))
    apply(p2.23, 2, function(x)
      panel.lines(gage[i], x[i]))
  } else {
    apply(q2.23, 2, function(x)
      panel.lines(gage[-i], x[-i], lty = 2))
    apply(p2.23, 2, function(x)
      panel.lines(gage[-i], x[-i]))
  }
  panel.text(rep(max(db$age), length(tau)),
             p2.23[nrow(p2.23),], label = tau, cex = 0.9)
  panel.text(rep(min(db$age), length(tau)),
             p2.23[1,], label = tau, cex = 0.9)
}
xyplot(head ~ age | cut, data = db, xlab = "Age (years)",
       ylab = "Head circumference (cm)", pch = 19,
       scales = list(x = list(relation = "free")),
       layout = c(2, 1), col = rgb(.1, .1, .1, .1),
       panel = pfun)
## Dutch boys head circumference - non-linear qr
#Non-linear quantile regression (use *rqss* function)
rqssmod <- vector(mode = "list", length = length(tau))
db$lage <- with(db, age^(1/3))
for (i in 1:length(tau))
  rqssmod[[i]] <- rqss(head ~ qss(lage, lambda = 1),
                       data = db, tau = tau[i])

gage <- seq(from = min(db$age), to = max(db$age), length = 50)
p <- sapply(1:length(tau), function(i) { predict(rqssmod[[i]],
                                                 newdata = data.frame(lage = gage^(1/3)))
})


## Dutch boys head circumference
#Non-linear quantile regression

pfun <- function(x, y, ...) {
  panel.xyplot(x = x, y = y, ...)
  apply(p, 2, function(x) panel.lines(gage, x))
  panel.text(rep(max(db$age), length(tau)),
             p[nrow(p),], label = tau, cex = 0.9)
  panel.text(rep(min(db$age), length(tau)),
             p[1,], label = tau, cex = 0.9)
}
xyplot(head ~ age | cut, data = db, xlab = "Age (years)",
       ylab = "Head circumference (cm)", pch = 19,
       scales = list(x = list(relation = "free")),
       layout = c(2, 1), col = rgb(.1, .1, .1, .1),
       panel = pfun)



## Playing with the whole db data 
## use the tau values as given in the above plot
library(gamlss.data)
data(db)
db2 <- db
tau <- c(.03, .15, .5, .85, .97)

rqssmod <- vector(mode = "list", length = length(tau))
db2$lage <- with(db2, age^(1/3))
for (i in 1:length(tau))
  rqssmod[[i]] <- rqss(head ~ qss(lage, lambda = 1),
                       data = db2, tau = tau[i])

gage <- seq(from = min(db2$age), to = max(db2$age), length = 100)
p <- sapply(1:length(tau), function(i) { predict(rqssmod[[i]],
                                                 newdata = data.frame(lage = gage^(1/3)))
})


## Playing with the whole db data 
pfun <- function(x, y, ...) {
  panel.xyplot(x = x, y = y, ...)
  apply(p, 2, function(x) panel.lines(gage, x))
  panel.text(rep(max(db2$age), length(tau)),
             p[nrow(p),], label = tau, cex = 0.9)
  #panel.text(rep(min(db2$age), length(tau)),
  #p[1,], label = tau, cex = 0.9)
}
xyplot(head ~ age, data = db2, xlab = "Age (years)",
       ylab = "Head circumference (cm)", pch = 19,
       scales = list(x = list(relation = "free")),
       layout = c(1, 1), col = rgb(.1, .1, .1, .1),
       panel = pfun)

## Playing with the whole db data 
#Change lambda = 20 for smoothness
## use the tau values as given in the above plot
library(gamlss.data)
data(db)
db2 <- db
tau <- c(.03, .15, .5, .85, .97)

rqssmod <- vector(mode = "list", length = length(tau))
db2$lage <- with(db2, age^(1/3))
for (i in 1:length(tau))
  rqssmod[[i]] <- rqss(head ~ qss(lage, lambda = 20),
                       data = db2, tau = tau[i])

gage <- seq(from = min(db2$age), to = max(db2$age), length = 100)
p <- sapply(1:length(tau), function(i) { predict(rqssmod[[i]],
                                                 newdata = data.frame(lage = gage^(1/3)))
})

pfun <- function(x, y, ...) {
  panel.xyplot(x = x, y = y, ...)
  apply(p, 2, function(x) panel.lines(gage, x))
  panel.text(rep(max(db2$age), length(tau)),
             p[nrow(p),], label = tau, cex = 0.9)
  #panel.text(rep(min(db2$age), length(tau)),
  #p[1,], label = tau, cex = 0.9)
}
xyplot(head ~ age, data = db2, xlab = "Age (years)",
       ylab = "Head circumference (cm)", pch = 19,
       scales = list(x = list(relation = "free")),
       layout = c(1, 1), col = rgb(.1, .1, .1, .1),
       panel = pfun)

