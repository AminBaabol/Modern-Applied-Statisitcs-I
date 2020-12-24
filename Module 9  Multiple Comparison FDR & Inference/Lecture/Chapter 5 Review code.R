
library(HSAUR3)
data(weightgain)
head(weightgain)
table(weightgain$source, weightgain$type)

## 
tapply(weightgain$weightgain,
       list(weightgain$source, weightgain$type), mean)
tapply(weightgain$weightgain,
       list(weightgain$source, weightgain$type), sd)

## 
plot.design(weightgain)


## 
wg_aov <- aov(weightgain ~ .^2-1 , data = weightgain)
summary(wg_aov)

##
interaction.plot(weightgain$type, weightgain$source,
                 + weightgain$weightgain)

## 
coef(wg_aov)



data(foster)
head(foster, n = 4)
table(foster$litgen, foster$motgen)


## 
plot.design(foster)


## 
summary(aov(weight ~ litgen * motgen, data = foster))

## 
foster_aov <- aov(weight ~ litgen * motgen, data = foster)
foster_hsd <- TukeyHSD(foster_aov, "motgen")
foster_hsd


## 
plot(foster_hsd)


