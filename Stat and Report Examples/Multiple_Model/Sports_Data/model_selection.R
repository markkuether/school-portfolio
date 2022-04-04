#open data
options(warn=-1)
athletes <- read.csv("ais.csv")

##############################
## LOGISTIC REGRESSION PREP ##
##############################
library(pROC)
land_sustained <- c("t_400m","b_ball","tennis")
land_sprint <- c("netball", "t_sprnt", "gym", "field")
water_sustained <- c("swim","row")
water_sprint <- c("w_polo")

#Review size of each category
length(athletes$Sport[athletes$Sport %in% land_sustained])
length(athletes$Sport[athletes$Sport %in% land_sprint])
length(athletes$Sport[athletes$Sport %in% water_sustained])
length(athletes$Sport[athletes$Sport %in% water_sprint])

#65, 61, 59, 17
#Water_Sprint is under represented in this classification
#Rejoin based on Sustained vs Sprint efforts
sustained <- c(land_sustained,water_sustained)
sprint <- c(land_sprint,water_sprint)

#Update data set for categories and other factors.
athletes$Ex_type <- athletes$Sport
athletes$Ex_type[athletes$Ex_type %in% sustained] <- "sustained"
athletes$Ex_type[athletes$Ex_type %in% sprint] <- "sprint"
athletes$Sport <- NULL

athletes$Ex_type <- as.factor(athletes$Ex_type)
athletes$Sex <- as.factor(athletes$Sex)

#Check for colinearity
pairs(athletes[,-c(2,13)])


lr.ath <- athletes
#These show strong covariance. Do not include in lr models
lr.ath$LBM <- NULL
lr.ath$Wt <- NULL
lr.ath$SSF <- NULL
lr.ath$Hc <- NULL
lr.ath$Hg <- NULL

#Check for extreme skew
colnames <- colnames(lr.ath)
for(i in c(1,3,4,5,6,7)){
  hist(lr.ath[,i],main=colnames[i],xlab="Values",ylab="Frequency")
}

#Skew exists, but not extreme.

#Check to see if logit has linear relation with predictors
lr.model <- glm(Ex_type~.-Sex,data=lr.ath,family = binomial)
lr.probs <- predict(lr.model,type="response")
logits <- log(lr.probs/(1-lr.probs))


colnames <- names(lr.ath)
for(i in c(1,3,4,5,6,7)){
  plot(logits,lr.ath[,i],main=colnames[i],ylab="logit",xlab="values")
  smoothingSpline = smooth.spline(logits, lr.ath[,i], spar=1)
  lines(smoothingSpline, col="red",lwd=2)
}

# ?????
# Some do not appear to be linear.  Short on time for attempting to correct.

#Review Predictors in model
summary(lr.model)

#Least to most significant. Adjust models accordingly
#Ferr, Bfat, RCC,BMI, HT,WCC
lrModel1 <- (Ex_type~Bfat+Sex+Ht+RCC+WCC+Ferr+BMI)
lrModel2 <- (Ex_type~Bfat+Sex+Ht+RCC+WCC+BMI)
lrModel3 <- (Ex_type~Sex+Ht+RCC+WCC+BMI)
lrModel4 <- (Ex_type~Sex+Ht+WCC+BMI)
lrModel5 <- (Ex_type~Sex+Ht+WCC)
lrModel6 <- (Ex_type~Sex+WCC)
All_lrModels <- list(lrModel1,lrModel2,lrModel3,lrModel4,lrModel5,lrModel6)
nlr_Models <- length(All_lrModels)

#For threshold values (a.k.a. "cutoff")
thresholds <- seq(.01,.99,.01)


#####################
## MLDA / QDA prep ##
#####################

#Check for overlap for possible LDA/QDA
lq_data <- athletes
colnames = names(lq_data)
for(i in c(1,3,4,5,6,7,8,9,10,11,12)){
  boxplot(lq_data[,i]~lq_data[,13], main=colnames[i],xlab=colnames[i],ylab="values")
}

#Too much overlap.

##############
## KNN PREP ##
##############

#KNN not used
#method is not easily explained to shareholders.
#Method is also less powerful than others.

#########################################
## Tree Prep ##
#########################################
library(tree)
library(randomForest)

# Evaluate importance of each factor:
evalmodel <- randomForest(Ex_type~.,data=athletes, mtry=dim(athletes)[1]-1, importance=TRUE)
evalmodel
varImpPlot(evalmodel)
mtry_val <- round(sqrt(dim(athletes)[1]),0)
           
#Variables - *=High Correlation
#Bfat, Sex, HT, *Wt, *LBM, RCC, WCC, *HC, *HG, Ferr, BMI, SSF
treeModel1 <- (Ex_type~.)
treeModel2 <- (Ex_type~Bfat+Sex+Ht+Wt+RCC+WCC+Hg+Ferr+BMI+SSF)
treeModel3 <- (Ex_type~Bfat+Sex+Ht+LBM+RCC+WCC+Hc+Ferr+BMI+SSF)
treeModel4 <- (Ex_type~Bfat+Sex+Ht+RCC+WCC+Ferr+BMI)
treeModel5 <- (Ex_type~Sex+Ht+WCC+BMI)
all_treeModels <- list(treeModel1,treeModel2,treeModel3,treeModel4,treeModel5)
ntree_Models <- length(all_treeModels)

all_Models <- c(All_lrModels,all_treeModels)


############################
## Model and Formula Prep ##
############################

fulldata.in <- athletes
resp = 13
intcp = 1

set.seed(9, sample.kind = "Rounding")
###########################
## Full modeling process ##
###########################

n.in = dim(fulldata.in)[1]
x.in = model.matrix(Ex_type~.,data=fulldata.in)[,-intcp]
y.in = fulldata.in[,resp]

k.in = 10 #Number of CV folds to use
groups.in = c(rep(1:k.in,floor(n.in/k.in))); if(floor(n.in/k.in) != (n.in/k.in)) groups.in = c(groups.in, 1:(n.in%%k.in))
cvgroups.in = sample(groups.in,n.in) 

nmodels = nlr_Models + ntree_Models

#lr models first
tree_index = nlr_Models+1
all_CV.in = matrix(rep(-1,n.in*nmodels),ncol=nmodels)

for(i in 1:k.in){
  train.in <- (cvgroups.in != i)
  test.in <- (cvgroups.in == i)
  
  #Parse Logistic Regression Models
  for (m in 1:nlr_Models){
    lr.mod <- glm(formula = all_Models[[m]],data=athletes[train.in,],family="binomial")
    all_CV.in[test.in,m] <- predict(lr.mod,athletes[test.in,],type="response")
  }

  #Parse Random Forest Models
  for (m in tree_index:nmodels){
    tree.model <- randomForest(all_Models[[m]],data=athletes[train.in,], mtry=mtry_val, importance=TRUE)
    all_CV.in[test.in,m] <- predict(tree.model,athletes[test.in,],type="prob")[2]
  }  
}

################################
## Evaluate all model results ##
################################

#determine best model using ROC AUC
all_auc.in <- rep(-1,nmodels)
for (m in 1:nmodels){
  thisROC <- roc(response=athletes$Ex_type, predict=all_CV.in[,m])
  all_auc.in[m] <- auc(thisROC)
}
best_model <- all_Models[[which(all_auc.in==max(all_auc.in))]]
best_val <- which(all_auc.in==max(all_auc.in))

#Determine lowest error achievable with best model
#We adjust the threshold value (a.k.a. "cutoff") to identify this value.
error_lvl <- rep(-1,length(thresholds))


#Adding 4 dummy values to response to ensure table below is always 2X2.
actual_results <- factor(c(as.character(athletes$Ex_type),"sprint","sprint","sustained","sustained"))
count = 0
for(r in thresholds){
  count <- count + 1
  categories <- ifelse(all_CV.in[,best_val] < r,"sprint","sustained")
  
  #Ensure table is always 2X2 by adding dummy values
  #This ensures at least 1 error in both categories = +2 errors.
  categories <- c(categories,"sprint","sustained","sprint","sustained")
  tbl_results <- table(actual_results,categories)
  
  #Calculate error - subtract out dummy errors (2)
  #Subtract 4 from total for all dummy values.
  error_lvl[count] <- (tbl_results[1,2]+tbl_results[2,1]-2)/(sum(tbl_results)-4)
}

best_rate <- min(error_lvl)
best_cutoff <- which(error_lvl==best_rate)[1]/100

#######################
## Choose Best Model ##
#######################

#Choose best overall model
#Fit best model to whole data set
#Extract coef.
bestmodel.in <- best_model
if (best_val>= tree_index){ 
  bestfit <- tree(bestmodel.in,data=fulldata.in)
  bestcoef.in <- c(NA)
} else {
  bestfit <- glm(bestmodel.in,data=fulldata.in,family="binomial")
  bestcoef.in <- coef(bestfit)
}

#############################
## End of modeling process ##
#############################


selectmodelsummary = list(selectmodel = bestmodel.in, selectfit = bestfit, 
                          selectcoef = bestcoef.in)
selectmodelsummary  # in order to recall the final selected fit after any validation

