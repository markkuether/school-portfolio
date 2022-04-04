#DEPENDENCY - bank_loan_data_cleaning.R run - bank dataframe loaded.
#DEPENDENCY - Model Selection run - neural net model selected.

#Model chosen was a neural net model.
library(NeuralNetTools)

#sort bestcoef.in 
rownames <- row.names(bestcoef.in)
orderedcoef <- bestcoef.in[order(-bestcoef.in$rel_imp),]
orderednames <- rep("",length(rownames))

for(i in 1:length(orderedcoef)){
  for(j in 1:length(orderedcoef)){
    if(bestcoef.in$rel_imp[j]==orderedcoef[i]){
      orderednames[i] <- rownames[j]
      if(orderednames[i]== "Years.in.job7-9 years"){
        orderednames[i] <- "Emp. 7-9 years"
      }
      if(orderednames[i]== "delinquent_time24-47"){
        orderednames[i] <- "Last delq. 24-47 mos"
      }
      if(orderednames[i]== "Years.in.job10+ years"){
        orderednames[i] <- "Emp. 10+ years"
      }
      
      if(orderednames[i]=="Home.OwnershipRent" ){
        orderednames[i] <- "Is a renter"
      }
      
      if(orderednames[i]=="Years.in.jobNot_Employed" ){
        orderednames[i] <- "Is not employed"
      }
      
      if(orderednames[i]=="Purpose3Major_Expenses" ){
        orderednames[i] <- "Loan for Maj. Exp."
      }
      if(orderednames[i]== "Home.OwnershipOwn Home"){
        orderednames[i] <- "Owns home"
      }
      
      if(orderednames[i]== "Purpose3Medium_Expenses"){
        orderednames[i] <- "Loan for Med. Exp."
      }
      
      if(orderednames[i]=="Purpose3Misc._Expenses" ){
        orderednames[i] <- "Loan for Misc. Exp."
      }
      
      if(orderednames[i]== "Purpose3Smaller_Expenses"){
        orderednames[i] <- "Loan for Small Exp."
      }
      
      if(orderednames[i]== "delinquent_time72+"){
        orderednames[i] <- "Last delq. 72+ mos"
      }
      
      if(orderednames[i]== "Years.in.job4-6 years"){
        orderednames[i] <- "Emp. 4-6 years"
      }
      
      if(orderednames[i]== "delinquent_time48-71"){
        orderednames[i] <- "Last delq. 48-71 mos"
      }
      
      if(orderednames[i]== "delinquent_timeNot Delinquent"){
        orderednames[i] <- "Is not delinquent"
      }
      
      if(orderednames[i]== "TermShort Term"){
        orderednames[i] <- "Is Short Term Loan"
      }
            break
    }
  }
}

names(orderedcoef) <- orderednames
orderedcoef

par(mar=c(3,7,1,1))
barplot(orderedcoef[c(1,2,3,4,5)],cex.names = .9,ylab="Percent Importance",
        ylim=c(0,.15),col="blue",main="Top Five Factors")

###################################

summary(bestfit)
plotnet(bestfit,cex=.3)
#lekprofile(bestfit) not running - no time to investigate why.

