#DEPENDENCY - bank_loan_data_cleaning.R run - bank dataframe loaded.

#Review proportion of variables
charged_off <- which(bank$Loan.Status == "Charged Off")
fully_paid <- which(bank$Loan.Status == "Fully Paid")

#Factor Distributions
bank_names <- names(bank)
char_var_names <- c("Term","Home.Ownership","Purpose",
                    "delinquent_time","Purpose1","Purpose2","Purpose3","Purpose4",
                    "Years.in.job")

char_cols <- which(names(bank) %in% char_var_names)
row_names <- c("Charged Off","Fully Paid")
for(item in char_cols){
  col_lvls <- levels(bank[,item])
  lvl_size <- length(col_lvls)
  
  lvl_matrix <- matrix(rep(0,lvl_size*2),ncol=lvl_size,
                       dimnames=list(row_names,col_lvls))
  for(lvl_num in 1:lvl_size){
    co <- length(which(bank[charged_off,item]==col_lvls[lvl_num]))
    fp <- length(which(bank[fully_paid,item]==col_lvls[lvl_num]))
    tot=co+fp
    lvl_matrix[1,lvl_num]=round((co/tot)*100,1)
    lvl_matrix[2,lvl_num]=round((fp/tot)*100,1)
  }
  barplot(lvl_matrix,main=paste("Distribution for",bank_names[item]),
          ylab="Percentage",col=c("red","green"),
          cex.names=.7,horiz = FALSE,las=1)
  legend("topright",
         c("Charged Off","Fully Paid"),
         fill = c("red","green")
  )
}

#Analyze Numeric Variables
num_var_names <- c("Current.Loan.Amount","Credit.Score","log.Annual.Income","log.Monthly.Debt",
                   "Number.of.Open.Accounts",
                   "Number.of.Credit.Problems","log.Current.Credit.Balance","log.Maximum.Open.Credit",
                   "Tax.Liens")

num_var_pos <- which(names(bank) %in% num_var_names)

for(item in num_var_pos){
  co <- bank[charged_off,item]
  fp <- bank[fully_paid,item]
  boxplot(co,fp,names=c("Charged Off","Fully Paid"),
                        main=bank_names[item])
}

#Numeric vars have heavy overlap
#categorical vars have some differentiation

#Cleanup
rm(list=ls(pattern="^c"))
rm(list=ls(pattern="^lvl_"))
rm(list=ls(pattern="^f"))
rm(list=ls(pattern="^num_"))
rm(bank_names)
rm(item)
rm(row_names)
rm(tot)


