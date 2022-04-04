#Read in data file
options(warn=-1)
athletes <- read.csv("ais.csv")

#Break into desired categories
land_sustained <- c("t_400m","b_ball","tennis")
land_sprint <- c("netball", "t_sprnt", "gym", "field")
water_sustained <- c("swim","row")
water_sprint <- c("w_polo")

sustained <- c(land_sustained,water_sustained)
sprint <- c(land_sprint,water_sprint)

#Update data frame to reflect categorires
athletes$Ex_type <- athletes$Sport
athletes$Ex_type[athletes$Ex_type %in% sustained] <- "sustained"
athletes$Ex_type[athletes$Ex_type %in% sprint] <- "sprint"
athletes$Sport <- NULL

athletes$Ex_type <- as.factor(athletes$Ex_type)
athletes$Sex <- as.factor(athletes$Sex)

#Logistic Regression Models
lrModel1 <- (Ex_type~Bfat+Sex+Ht+RCC+WCC+Ferr+BMI)
lrModel2 <- (Ex_type~Bfat+Sex+Ht+RCC+WCC+BMI)
lrModel3 <- (Ex_type~Sex+Ht+RCC+WCC+BMI)
lrModel4 <- (Ex_type~Sex+Ht+WCC+BMI)
lrModel5 <- (Ex_type~Sex+Ht+WCC)
lrModel6 <- (Ex_type~Sex+WCC)
All_lrModels <- list(lrModel1,lrModel2,lrModel3,lrModel4,lrModel5,lrModel6)
nlr_Models <- length(All_lrModels)

lr_cutoffs <- seq(.01,.99,.01) #Set 100 cutoffs
resp = 13
intcp = 1

#Random Forest Tree Models
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


###################################################################
##### Double cross-validation for modeling-process assessment #####				 
###################################################################

##### model assessment OUTER shell #####

fulldata.out <- athletes
k.out = 10 
n.out = dim(fulldata.out)[1]
trcount = 0

# set up storage for predicted values from the double-cross-validation
allpredictedCV.out = rep(NA,n.out)

#set up storage to see what models are "best" on the inner loops
allbestmodels = rep(NA,k.out)

#Set up outer groups
groups.out = c(rep(1:k.out,floor(n.out/k.out))) 
if(floor(n.out/k.out) != (n.out/k.out)) groups.out = c(groups.out, 1:(n.out%%k.out))

set.seed(9, sample.kind = "Rounding")
cvgroups.out = sample(groups.out,n.out) 


#Loop through outer splits
for (j in 1:k.out)  {  #be careful not to re-use loop indices
  groupj.out = (cvgroups.out == j)
  traindata.out = athletes[!groupj.out,]
  validdata.out = athletes[groupj.out,]
  
  trainx.out = model.matrix(Ex_type~.,data=traindata.out)[,-(intcp)]
  trainy.out = traindata.out[,resp]
  validx.out = model.matrix(Ex_type~.,data=validdata.out)[,-(intcp)]
  validy.out = validdata.out[,resp]
  
  ### entire model-fitting process ###
  fulldata.in = traindata.out
  ###	:	:	:	:	:	:	:  ###
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
  
  ###   :	:	:	:	:	:	:  ###
  ### resulting in bestmodel.in ###
  
  allbestmodels[j] = bestmodel.in
  
  ###############################################
  ## Need to record results from inner section ##
  ###############################################
  ## INCOMPLETE ##
  ################
  
  ####################################################################
  ### 11/18/20 ###                                                   #
  # Evaluation - categorize output based on best threshold value(s). #
  # Use p(valid) = 1 - (sum(I(y != y*)))/(number of models)          #
  # or  p(valid) = (sum(I(Y = Y*)))/(number of models)               #
  ####################################################################
  
  if (is.na(bestcoef.in)) {  #this is a tree model

      } else {  # this is a lr model
    lr_predCV.out[groupj.out] = predict(bestfit,newx=validdata.out,s=bestlambdaLASSO)
  }
}

# for curiosity, we can see the models that were "best" on each of the inner splits
#allbestmodels

#assessment

############################################################################
#Figure out how to combine results from tree and lr to provide an R2 value.#
############################################################################

#y.out = fulldata.out$BodyFatSiri
#CV.out = sum((allpredictedCV.out-y.out)^2)/n.out; CV.out
#R2.out = 1-sum((allpredictedCV.out-y.out)^2)/sum((y.out-mean(y.out))^2); R2.out



##########################
# Questions - 