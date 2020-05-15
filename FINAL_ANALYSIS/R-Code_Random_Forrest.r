## Read Data
library(readr)
final_cleaned <- read_csv("C:/Users/15129/Desktop/mimic_j/Final Dataset/final_cleaned.csv")
data = final_cleaned

##Set Data as Binary Factors
data[data$DELIRIUM == 0,]$DELIRIUM <- "No"
data[data$DELIRIUM == 1,]$DELIRIUM <- "Yes"
data$DELIRIUM <- as.factor(data$DELIRIUM)

data[data$GENDER == 0,]$sex <- "M"
data[data$GENDER == 1,]$sex <- "F"
data$GENDER <- as.factor(data$GENDER)

data[data$ETHNICITY == 0,]$sex <- "W"
data[data$ETHNICITY == 1,]$sex <- "M"
data$ETHNICITY <- as.factor(data$ETHNICITY)

data$OVER_65 <- as.factor(data$OVER_65)
data$OVER_75 <- as.factor(data$OVER_75)
data$OVER_85 <- as.factor(data$OVER_85)
data$ANTIPSY <- as.factor(data$ANTIPSY)
data$BAR <- as.factor(data$BAR)
data$BENZ <- as.factor(data$BENZ)
data$DIUR <- as.factor(data$DIUR)
data$GENANES <- as.factor(data$GENANES)
data$VPRES <- as.factor(data$VPRES)
data$FLUID <- as.factor(data$FLUID)
data$SHOCK <- as.factor(data$SHOCK)
data$TBI <- as.factor(data$TBI)
data$TRAUMA <- as.factor(data$TRAUMA)
data$BLOODTRF <- as.factor(data$BLOODTRF)
data$DIAL <- as.factor(data$DIAL)
data$RESUS <- as.factor(data$RESUS)

## Check Structure
str(data)
## Set Random Seed if Needed
set.seed(568)
#Subset Data from non-needed variables
data = subset(data, select =-c(HADM_ID,CLASSIFIER_ICD9_CODE,AGE,OVER_65,OVER_85))

##Build Forest
install.packages("randomForest")

library(randomForest)
model <- randomForest(DELIRIUM ~ ., data = data, proximity=TRUE)
model
##Plor OOB Error
library(ggplot2)
library(cowplot)


oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "No", "Yes"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"No"], 
          model$err.rate[,"Yes"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))


##Try Bigger Forest
model2 <- randomForest(DELIRIUM ~ ., data = data, ntree=1000, proximity=TRUE)
model2

oob.error.data2 <- data.frame(
  Trees=rep(1:nrow(model2$err.rate), times=3),
  Type=rep(c("OOB", "No", "Yes"), each=nrow(model2$err.rate)),
  Error=c(model2$err.rate[,"OOB"], 
          model2$err.rate[,"No"], 
          model2$err.rate[,"Yes"]))
ggplot(data=oob.error.data2, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))


##Optomize -> # of variables tried at each split
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(DELIRIUM ~ ., data = data, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
### The default already has lowest OOB Error Rate

##Draw Multidimensional scaling
distance.matrix <- as.dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data$DELIRIUM)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")