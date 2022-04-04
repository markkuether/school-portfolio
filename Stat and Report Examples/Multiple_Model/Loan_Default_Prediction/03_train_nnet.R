#DEPENDENCY - bank_loan_data_cleaning.R run - bank dataframe loaded.

#This uses a subset of the data to determine 
#the optimal setting of size and decay rate for a specific model
#This uses AUC to determine the best settings.
library(nnet)
library(pROC)

#Starting with cleaned bank data from cleaning script
#Teted with different seeds
set.seed(08,sample.kind = "Rounding")

#Because the ratio of Fully Paid to Charged off is fairly large (~4:1),
#We oversample "Charged Off" to train the model.
#We use 5000 data points to reduce training time.
charged_off <- which(bank$Loan.Status == "Charged Off")
fully_paid <- which(bank$Loan.Status == "Fully Paid")
co_sample <- sample(charged_off,2500)
fp_sample <- sample(fully_paid,2500)

#Build training dataframe
sub_bank <- bank[co_sample,]
sub_bank <- rbind(sub_bank,bank[fp_sample,])

#Test against this model  (Full Model)
nnet_model <- (Loan.Status~.-Purpose1-Purpose2-Purpose3-Purpose4)

#(Reduced Model)
#nnet_model <- (Loan.Status~log.Annual.Income+Current.Loan.Amount+log.Monthly.Debt+
#                 Term+Credit.Score+delinquent_time+Purpose)

#Set standardize numeric variables
sub_bank$Current.Loan.Amount <- scale(sub_bank$Current.Loan.Amount)
sub_bank$Credit.Score <- scale(sub_bank$Credit.Score)
sub_bank$log.Annual.Income <- scale(sub_bank$log.Annual.Income)
sub_bank$log.Monthly.Debt <- scale(sub_bank$log.Monthly.Debt)
sub_bank$Number.of.Open.Accounts <- scale(sub_bank$Number.of.Open.Accounts)
sub_bank$Number.of.Credit.Problems <- scale(sub_bank$Number.of.Credit.Problems)
sub_bank$log.Current.Credit.Balance <- scale(sub_bank$log.Current.Credit.Balance)
sub_bank$log.Maximum.Open.Credit <- scale(sub_bank$log.Maximum.Open.Credit)
sub_bank$Tax.Liens <- scale(sub_bank$Tax.Liens)


#Set size and decay rates to determine ideal settings 
#Tested with different sizes and decays - final test settings.
sv <- c(5,6,7,8)
sizes = length(sv)
dv <- c(.1,.5,1,1.5,2,2.5,3)
decayrates = length(dv)

#Use Cross Validation to determine best values
resp = 1 #position of response variable

fulldata.in <- sub_bank
n.in = dim(fulldata.in)[1]
y.in = fulldata.in[,resp]

k.in = 10 #Number of CV folds to use
groups.in = c(rep(1:k.in,floor(n.in/k.in))); if(floor(n.in/k.in) != (n.in/k.in)) groups.in = c(groups.in, 1:(n.in%%k.in))
cvgroups.in = sample(groups.in,n.in) 
all_CV.in = array(rep(-1,n.in*decayrates*sizes),dim=c(n.in,decayrates,sizes))
auc_val <- matrix(rep(-1,sizes*decayrates),ncol=sizes)

si = 0
for(sz in sv){
  si = si + 1
  r = 0
  for(d in dv){
    r=r+1
    print(paste("Size: ",sz," Decay: ",d))
    for(i in 1:k.in){
      print(paste("    Group ",i," of ",k.in))
      train.in <- (cvgroups.in != i)
      test.in <- (cvgroups.in == i)
      bankfit <- nnet(formula=nnet_model,data=fulldata.in[train.in,],size=sz,decay=d,maxit=500,trace=FALSE)
      all_CV.in[test.in,r,si] <- predict(bankfit,newdata=fulldata.in[test.in,])            
    }
    thisROC <- roc(response=y.in, predict=all_CV.in[,r,si])
    auc_val[r,si] <- auc(thisROC)
    
  }
}

xaxis <- "Decay Rate\n"
for(i in 1:decayrates){
  xaxis <- paste(xaxis,i,"=",dv[i],"  |  ")
}
for(sz in 1:sizes){
  maintitle = paste("Trial# - AUC for nnet of size ",sv[sz])
  plot(auc_val[,sz],ylab="AUC",
       xlab=xaxis,
       main=maintitle)
  
}
#HIGHEST AUC OBSERVATIONS
#TRIAL 1: full model -> (size 1 decay .1+) (size 6 decay 1+)
#TRIAL 2: reduced model - same data ->  (size 4+ decay .5+)
#TRIAL 3: reduced model - reduced data (size 4+ decay .5+)
#TRIAL 4: full model - different data - higher decay (size 5, decay 1.5+)(7,8, decay 1.5+)

#For full model, use 7 nodes, decayRate = 1.75
