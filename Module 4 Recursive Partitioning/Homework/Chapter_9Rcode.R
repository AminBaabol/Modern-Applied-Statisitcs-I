
#title: "Recursive Partitioning"

  
  ## Librarys 
library("vcd")
library("lattice")
library("randomForest")
library("party")
library("partykit")
library("mboost")
library("TH.data")
library("ipred")
library("rpart")


#Do not forget to install.packages("")

## Body Fat Data

data("bodyfat")
head(bodyfat)
## Body Fat Data 
#rpart() function
set.seed(seed = 929270)
library("rpart")
bodyfat_rpart <- rpart(
  DEXfat ~ age + waistcirc + hipcirc + 
    elbowbreadth + kneebreadth, data = bodyfat, 
  control = rpart.control(minsplit = 10))
## RP-bodyfat-plot
printcp(bodyfat_rpart)
plotcp(bodyfat_rpart)
print(bodyfat_rpart)
plot(bodyfat_rpart)
text(bodyfat_rpart)

a <- mean(bodyfat$DEXfat[bodyfat$waistcirc>=88.4])
b <- mean(bodyfat$DEXfat[bodyfat$waistcirc<88.4])
y.hat <- ifelse(bodyfat$waistcirc>=88.4, a, b)
sum((y.hat[bodyfat$waistcirc<88.4]-bodyfat$DEXfat[bodyfat$waistcirc<88.4])^2)

##  RP-bodyfat-plot
#Better plot using 'partykit'

library("partykit")
plot(as.party(bodyfat_rpart), 
     tp_args = list(id = FALSE))



##  RP-bodyfat-plot

library("partykit")
plot(as.party(bodyfat_rpart), 
     tp_args = list(id = FALSE))




## RP-bodyfat-cp
print(bodyfat_rpart$cptable)
opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
opt

## RP-bodyfat-prune
#Prune back the large tree
cp <- bodyfat_rpart$cptable[opt, "CP"]
bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
bodyfat_prune


## RP-bodyfat-pruneplot
plot(as.party(bodyfat_prune), tp_args = list(id = FALSE))

#Note that the inner nodes 3 and 6 have been removed. 


## RP-bodyfat-predict
DEXfat_pred <- predict(bodyfat_prune, newdata = bodyfat)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data = bodyfat, xlab = "Observed", 
     ylab = "Predicted", ylim = xlim, xlim = xlim)
abline(a = 0, b = 1)

## Calculate mean square error (MSE)

mean((bodyfat$DEXfat - DEXfat_pred)^2)


#Exercise

#-  change the seed number (slide 12) and build a tree (go through pruning as well). Calculate the MSE after predicting using the new tree built. Is it the same as above?

## Set Seed

set.seed(290875)


## RP- glaucoma data
dim((GlaucomaM))
head(GlaucomaM, n = 2)


## RP-glaucome-rpart

par(mfrow = c(8,8))
p <- dim(GlaucomaM)[2]-1
for(i in 1:p){
  par(mar = c(0.5,0.5,0.5,0.5))
  boxplot(GlaucomaM[,i]~GlaucomaM$Class, col = 2:3, names=c("",""), axes = FALSE)
  legend("top",c(colnames(GlaucomaM)[i]))
  box()
}



## RP-glaucoma-rpart

data("GlaucomaM", package = "TH.data")
#xval = 100 means 100 runs of 10-fold CV
glaucoma_rpart <- rpart(Class ~ ., data = GlaucomaM, 
                        control = rpart.control(xval = 100)) 
glaucoma_rpart$cptable
opt <- which.min(glaucoma_rpart$cptable[,"xerror"])
cp <- glaucoma_rpart$cptable[opt, "CP"]
glaucoma_prune <- prune(glaucoma_rpart, cp = cp)



## RP-glaucoma-plot

plot(as.party(glaucoma_prune), tp_args = list(id = FALSE))


## RP-glaucoma-cp
#Unstable result
nsplitopt <- vector(mode = "integer", length = 25)
for (i in 1:length(nsplitopt)) {
  cp <- rpart(Class ~ ., data = GlaucomaM)$cptable
  nsplitopt[i] <- cp[which.min(cp[,"xerror"]), "nsplit"]
}
table(nsplitopt)


## RP-glaucoma-bagg
#Bagging for GlaucomaM data
trees <- vector(mode = "list", length = 25)
n <- nrow(GlaucomaM)
bootsamples <- rmultinom(length(trees), n, rep(1, n)/n)
mod <- rpart(Class ~ ., data = GlaucomaM, 
             control = rpart.control(xval = 0))
for (i in 1:length(trees))
  trees[[i]] <- update(mod, weights = bootsamples[,i])


## RP-glacoma - bagg
#Closer look at the trees
table(sapply(trees, function(x) 
  as.character(x$frame$var[1])))

#Note the change in the choice of variables for the root node. 

## RP-glaucoma-splits
classprob <- matrix(0, nrow = n, ncol = length(trees))
for (i in 1:length(trees)) {
  classprob[,i] <- predict(trees[[i]], 
                           newdata = GlaucomaM)[,1]
  classprob[bootsamples[,i] > 0,i] <- NA
}



## RP-glaucoma-avg
#Take average to find final predictions
avg <- rowMeans(classprob, na.rm = TRUE)
predictions <- factor(ifelse(avg > 0.5, "glaucoma", 
                             "normal"))
predtab <- table(predictions, GlaucomaM$Class)
predtab

round(predtab[1,1] / colSums(predtab)[1] * 100)

round(predtab[2,2] / colSums(predtab)[2] * 100)

## RP-glaucoma-baggplot

library("lattice")
gdata <- data.frame(avg = rep(avg, 2), 
                    class = rep(as.numeric(GlaucomaM$Class), 2),
                    obs = c(GlaucomaM[["varg"]], GlaucomaM[["vari"]]),
                    var = factor(c(rep("varg", nrow(GlaucomaM)), 
                                   rep("vari", nrow(GlaucomaM)))))
panelf <- function(x, y) {
  panel.xyplot(x, y, pch = gdata$class, col = gdata$class)
  panel.abline(h = 0.5, lty = 2)
}

print(xyplot(avg ~ obs | var, data = gdata, 
             panel = panelf,
             scales = "free", xlab = "", 
             ylab = "Estimated Class Probability- Glaucoma"))


## Random Forest
#randomForest function in r
library("randomForest")
rf <- randomForest(Class ~ ., data = GlaucomaM)
table(predict(rf), GlaucomaM$Class)
#importance(rf) #importance of the covariates

