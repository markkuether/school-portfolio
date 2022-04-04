#DEPENDENCY - bank_loan_data_cleaning.R run - bank dataframe loaded.

#Libraries
library(nnet)
library(pROC)
library(NeuralNetTools)

#Because the categories are unevenly distributed (~4:1),
#We oversample the "Charged Off" status to help with modeling
set.seed(14, sample.kind = "Rounding")

charged_off <- which(bank$Loan.Status == "Charged Off")
fully_paid <- which(bank$Loan.Status == "Fully Paid")
fp_data <- sample(fully_paid,length(charged_off))

#Build training dataframe
sub_bank <- bank[charged_off,]
sub_bank <- rbind(sub_bank,bank[fp_data,])

################################
## Logistic Regression Models ##
################################

#Review broad model to determine significance of each predictor
lr.model <- glm(Loan.Status~.-Purpose1-Purpose2-Purpose3-Purpose4,data=sub_bank,family = binomial)
summary(lr.model)

#Least to most significant. Adjust models accordingly
#Keep categorical vars per var_analysis.
#log.Annual.Income, Current.Loan.Amount, log.Monthly.Debt, Term, Credit.Score, delinquent_time, Purpose 
lrModel1 <- (Loan.Status~Current.Loan.Amount+log.Annual.Income+log.Monthly.Debt+
               log.Maximum.Open.Credit+log.Current.Credit.Balance+
               Term+Home.Ownership+Years.in.job+Purpose+Number.of.Open.Accounts+
               Tax.Liens)

lrModel2 <- (Loan.Status~Current.Loan.Amount+log.Annual.Income+log.Monthly.Debt+
               log.Maximum.Open.Credit+log.Current.Credit.Balance+               
               Term+Home.Ownership+Years.in.job+Purpose+Number.of.Open.Accounts)

lrModel3 <- (Loan.Status~Current.Loan.Amount+log.Annual.Income+log.Monthly.Debt+
               log.Maximum.Open.Credit+log.Current.Credit.Balance+               
               Term+Home.Ownership+Years.in.job+Purpose)

lrModel4 <- (Loan.Status~Current.Loan.Amount+log.Annual.Income+log.Monthly.Debt+
               log.Maximum.Open.Credit+log.Current.Credit.Balance+               
               Term+Home.Ownership+Years.in.job+Purpose)

All_lrModels <- list(lrModel1,lrModel2,lrModel3,lrModel4)
nlr_Models <- length(All_lrModels)

#################
## nnet Models ##
#################

#Size and Rate discovered from bank_loan_train_nnet.R
nnsize = 7
nndecayrate = 1.75

#Copy data to bank_nn so we can standardize the variables for neural net.
#We use the full data set for the final analysis
bank_nn <- bank

#Standardize numeric variables for bank_nn
bank_nn$Current.Loan.Amount <- scale(bank_nn$Current.Loan.Amount)
bank_nn$Credit.Score <- scale(bank_nn$Credit.Score)
bank_nn$log.Annual.Income <- scale(bank_nn$log.Annual.Income)
bank_nn$log.Monthly.Debt <- scale(bank_nn$log.Monthly.Debt)
bank_nn$Number.of.Open.Accounts <- scale(bank_nn$Number.of.Open.Accounts)
bank_nn$Number.of.Credit.Problems <- scale(bank_nn$Number.of.Credit.Problems)
bank_nn$log.Current.Credit.Balance <- scale(bank_nn$log.Current.Credit.Balance)
bank_nn$log.Maximum.Open.Credit <- scale(bank_nn$log.Maximum.Open.Credit)
bank_nn$Tax.Liens <- scale(bank_nn$Tax.Liens)

#Create the subset of data using the standardized data
sub_bank_nn <- bank_nn[charged_off,]
sub_bank_nn <- rbind(sub_bank_nn,bank_nn[fp_data,])

nnModel1 <- (Loan.Status~.-Purpose1-Purpose2-Purpose3-Purpose4) #Original Purpose
nnModel2 <- (Loan.Status~.-Purpose-Purpose2-Purpose3-Purpose4)  #Purpose 1
nnModel3 <- (Loan.Status~.-Purpose-Purpose1-Purpose3-Purpose4)  #Purpose 2
nnModel4 <- (Loan.Status~.-Purpose-Purpose1-Purpose2-Purpose4)  #Purpose 3
nnModel5 <- (Loan.Status~.-Purpose-Purpose1-Purpose2-Purpose3)  #Purpose 4

All_nnModels <- list(nnModel1,nnModel2,nnModel3,nnModel4,nnModel5)
nnn_Models <- length(All_nnModels)

############################
## Model and Formula Prep ##
############################
all_Models <- c(All_lrModels,All_nnModels)

fulldata.in <- bank
fulldata_nn.in <- bank_nn
subdata.in <- sub_bank
subdata_nn.in <- sub_bank_nn
resp = 1
set.seed(14, sample.kind = "Rounding")

###########################
## Full modeling process ##
###########################

n.in = dim(subdata.in)[1]
y.in = subdata.in[,resp]

k.in = 10 #Number of CV folds to use
groups.in = c(rep(1:k.in,floor(n.in/k.in))); if(floor(n.in/k.in) != (n.in/k.in)) groups.in = c(groups.in, 1:(n.in%%k.in))
cvgroups.in = sample(groups.in,n.in) 

nmodels = nlr_Models + nnn_Models

#lr models first
nn_index = nlr_Models+1
all_CV.in = matrix(rep(-1,n.in*nmodels),ncol=nmodels)

for(i in 1:k.in){
  train.in <- (cvgroups.in != i)
  test.in <- (cvgroups.in == i)
  print(paste("Group ",i," of ",k.in))
  
  #Parse Logistic Regression Models
  print("  Log Reg Models")
  for (m in 1:nlr_Models){
    lr.mod <- glm(formula = all_Models[[m]],data=subdata.in[train.in,],family="binomial")
    all_CV.in[test.in,m] <- predict(lr.mod,subdata.in[test.in,],type="response")
  }
  
  #Parse nnet Models - size and decay determined using "bank_loan_train_nnet.r"
  print("  Neurlal Net Model")
  for (m in nn_index:nmodels){
    print(paste("    Model ",m+1-nn_index," of ",nnn_Models))
    nn.mod <- nnet(formula= all_Models[[m]],data=subdata_nn.in[train.in,],size=nnsize,
                   decay=nndecayrate,maxit=500,trace=FALSE)
    all_CV.in[test.in,m] <- predict(nn.mod,newdata=subdata_nn.in[test.in,])
  }  
}

################################
## Evaluate all model results ##
################################

#determine best model using ROC AUC
all_auc.in <- rep(-1,nmodels)
for (m in 1:nmodels){
  thisROC <- roc(response=y.in, predict=all_CV.in[,m])
  all_auc.in[m] <- auc(thisROC)
}
best_model <- all_Models[[which(all_auc.in==max(all_auc.in))]]
best_val <- which(all_auc.in==max(all_auc.in))

#Determine highest success achievable with best model
#We adjust the threshold value to identify this value.

thresholds <- seq(.01,.99,.01)
success_rate <- rep(-1,length(thresholds))

#Adding 4 dummy values to response to ensure table below is always 2X2.
lvl1 <- levels(y.in)[1]
lvl2 <- levels(y.in)[2]
actual_results <- factor(c(as.character(y.in),lvl1,lvl1,lvl2,lvl2))
count = 0
for(r in thresholds){
  count <- count + 1
  categories <- ifelse(all_CV.in[,best_val] < r,lvl1,lvl2)
  
  #Ensure table is always 2X2 by adding dummy values
  #This ensures at least 1 error in both categories = +2 errors.
  categories <- c(categories,lvl1,lvl2,lvl1,lvl2)
  tbl_results <- table(actual_results,categories)
  
  #Calculate error - subtract out dummy errors (2)
  #Subtract 4 from total for all dummy values.
  success_rate[count] <- (tbl_results[1,1]+tbl_results[2,2]-2)/(sum(tbl_results)-4)
}

best_rate <- max(success_rate)
best_cutoff <- which(success_rate==best_rate)[1]/100

#######################
## Choose Best Model ##
#######################

#Choose best overall model - Fit best model to whole data set
#Extract coef.
modeltype = ""
bestmodel.in <- best_model
if (best_val>= nn_index){ 
  bestfit <- nnet(bestmodel.in,data=fulldata.in,size=nnsize,decay=nndecayrate,maxit=500)
  bestcoef.in <- garson(bestfit,bar_plot=FALSE)
  modeltype = "Neural Networks"
} else {
  bestfit <- glm(bestmodel.in,data=fulldata_nn.in,family="binomial")
  bestcoef.in <- coef(bestfit)
  modeltype = "Logistic Regression"
}

#############################
## End of modeling process ##
#############################

selectmodelsummary = list(selectmodel = bestmodel.in, selectfit = bestfit, 
                          selectcoef = bestcoef.in)

print(paste("The best model uses",modeltype, "with the model"))
print(bestmodel.in)

if(modeltype == "Logistic Regression"){
  print(paste("using the coefficients",bestcoef.in))
} else {
  print(paste("using",nnsize,"hidden nodes and a decay rate of",nndecayrate))
  print("The loan default rate is affected by the following variables:")
  for(i in 1:dim(bestcoef.in)[1]){
    print(paste(row.names(bestcoef.in)[i],":",round(bestcoef.in[i,1]*100,1),"%"))
    }
}

