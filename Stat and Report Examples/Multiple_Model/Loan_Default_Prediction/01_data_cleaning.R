#Load data from local directory
bank <- read.csv("credit_train.csv")

##################
### Clean Data ###
##################
#Data contains 514 empty fields at the end.  Remove empty fields.
#Reviewing in notepad+ shows these are completely empty
empties <- which(bank$Loan.ID == "")
bank <- bank[-empties,]

#cleanup
rm(empties)

# Remove Loand ID and Customer ID - these are not predictor variables.
bank$Loan.ID = NULL
bank$Customer.ID =  NULL

#Home Ownership "HaveMortgage" = "Home Mortgage"
bank$Home.Ownership[bank$Home.Ownership == "HaveMortgage"] <- "Home Mortgage"

#Months.since.last.delingquent contains a lot of NA's which appears to signify that the
#consumer is not delinquent.  These dominate this field, so I grouped the values together
#to even out the distribution as best as possible with logical divisions.
#del_count <- numeric()
delinquent_time <- rep("none",dim(bank)[1])
delinquent_time[bank$Months.since.last.delinquent >= 72 & !(is.na(bank$Months.since.last.delinquent))] <- "72+"
delinquent_time[bank$Months.since.last.delinquent >= 48 & bank$Months.since.last.delinquent < 72] <- "48-71"
delinquent_time[bank$Months.since.last.delinquent >= 24 & bank$Months.since.last.delinquent < 48] <- "24-47"
delinquent_time[bank$Months.since.last.delinquent >= 0 & bank$Months.since.last.delinquent < 24] <- "0-23"
delinquent_time[is.na(bank$Months.since.last.delinquent)] <- "Not Delinquent"
bank$delinquent_time <- delinquent_time
bank$Months.since.last.delinquent <- NULL

#clean up
rm(delinquent_time)

#Similar to above, Years.in.current.job contains both N/A and a dominant category of 10+ years.
#We re-classify these as well.
Years.in.job <- rep("none",dim(bank)[1])
yij_not_emp <- c("n/a")
yij_zero_to_three <- c("< 1 year","1 year","2 years","3 years")
yij_four_to_six <- c("4 years","5 years","6 years")
yij_seven_to_nine <- c("7 years","8 years","9 years")
yij_tenplus <- c("10+ years")

Years.in.job[bank$Years.in.current.job %in% yij_not_emp] <- "Not_Employed"
Years.in.job[bank$Years.in.current.job %in% yij_zero_to_three] <- "0-3 years"
Years.in.job[bank$Years.in.current.job %in% yij_four_to_six] <- "4-6 years"
Years.in.job[bank$Years.in.current.job %in% yij_seven_to_nine] <- "7-9 years"
Years.in.job[bank$Years.in.current.job %in% yij_tenplus] <- "10+ years"
bank$Years.in.job <- Years.in.job
bank$Years.in.current.job <- NULL

#cleanup
rm(Years.in.job)
rm(list=ls(pattern="^yij"))

### Post Analysis Note ###
#The model ran into an error missing "renewable energy"
#I suspect that means it was not sufficiently prevelant to appear consistently in CV.
#I'm wrapping that into "Home Improvements"
bank$Purpose[bank$Purpose=="renewable_energy"] <- "Home Improvements"

#If we group purposes to reduce complexity, will this help the models?
#Create mulitiple groupings - retain original category
#Group common Purposes - try four different groupings
purposes1 <- rep("none",dim(bank)[1])
purposes2 <- rep("none",dim(bank)[1])
purposes3 <- rep("none",dim(bank)[1])
purposes4 <- rep("none",dim(bank)[1])
p1_business <- c("Business Loan","small_business")
#p1_LifeInv <- c("Buy House","Educational Expenses","Home Improvements","renewable_energy","moving")
p1_LifeInv <- c("Buy House","Educational Expenses","Home Improvements","moving")
p1_LifeRsk <- c("Debt Consolidation","Medical Bills")
p1_LifePur <- c("major_purchase","Buy a Car","Take a Trip","vacation","wedding")
p1_other <- c("other","Other")

p2_buythings <- c("Buy a Car","Buy House","Take a Trip","vacation","major_purchase")
p2_lifeExp <- c("Debt Consolidation","Medical Bills","moving","Educational Expenses")
#p2_Domestic <- c("wedding","Home Improvements","renewable_energy")
p2_Domestic <- c("wedding","Home Improvements")
p2_business <- c("Business Loan","small_business")
p2_other <- c("other","Other")

p3_largest <- c("Buy House","Educational Expenses")
p3_major <- c("small_business","Business Loan","Home Improvements")
#p3_medium <- c("major_purchase","Medical Bills","Buy a Car","Take a Trip","vacation","wedding","renewable_energy")
p3_medium <- c("major_purchase","Medical Bills","Buy a Car","Take a Trip","vacation","wedding")
p3_smaller <- c("moving","Debt Consolidation")
p3_other <- c("other","Other")

p4_personal <- c("small_business","Educational Expenses","Buy a Car","major_purchase","Take a Trip")
#p4_family <- c("Buy House","Home Improvements","moving","vacation","renewable_energy","wedding")
p4_family <- c("Buy House","Home Improvements","moving","vacation","wedding")
p4_Business <- c("Business Loan")
p4_other <- c("Debt Consolidation","Medical Bills","other","Other")

purposes1[bank$Purpose %in% p1_business] <- "Business_Inv."
purposes1[bank$Purpose %in% p1_LifeInv] <- "Personal_Inv."
purposes1[bank$Purpose %in% p1_LifeRsk] <- "Personal_Rsk."
purposes1[bank$Purpose %in% p1_LifePur] <- "Personal_Purchase"
purposes1[bank$Purpose %in% p1_other] <- "Undefined"

purposes2[bank$Purpose %in% p2_business] <- "Business_Expenses"
purposes2[bank$Purpose %in% p2_Domestic] <- "Domestic_Expenses"
purposes2[bank$Purpose %in% p2_buythings] <- "Purchases"
purposes2[bank$Purpose %in% p2_lifeExp] <- "Life_Expenses"
purposes2[bank$Purpose %in% p2_other] <- "Not_Defined"

purposes3[bank$Purpose %in% p3_largest] <- "Largest_Expenses"
purposes3[bank$Purpose %in% p3_major] <- "Major_Expenses"
purposes3[bank$Purpose %in% p3_medium] <- "Medium_Expenses"
purposes3[bank$Purpose %in% p3_smaller] <- "Smaller_Expenses"
purposes3[bank$Purpose %in% p3_other] <- "Misc._Expenses"

purposes4[bank$Purpose %in% p4_Business] <- "Business_Related"
purposes4[bank$Purpose %in% p4_personal] <- "Personal_Use"
purposes4[bank$Purpose %in% p4_family] <- "Family_Related"
purposes4[bank$Purpose %in% p4_other] <- "Other_Use"

bank$Purpose1 <- purposes1
bank$Purpose2 <- purposes2
bank$Purpose3 <- purposes3
bank$Purpose4 <- purposes4

#Set all character based values to factor fields.
char_var_names <- c("Loan.Status","Term","Years.in.current.job","Home.Ownership","Purpose",
                    "delinquent_time","Purpose1","Purpose2","Purpose3","Purpose4",
                    "Years.in.job")

char_cols <- which(names(bank) %in% char_var_names)
for(column in char_cols){
  bank[,column] <- as.factor(bank[,column])
}

#Clean Up
rm(list = ls(pattern="^purposes"))
rm(list = ls(pattern="^p1_"))
rm(list = ls(pattern="^p2_"))
rm(list = ls(pattern="^p3_"))
rm(list = ls(pattern="^p4_"))
rm(char_cols)
rm(char_var_names)
rm(column)


#Clean up columns with invalid value.  We are removing these records.
#Current.Loan.Amount - 99999999
#Credit.Score - >900
pos_CLA <- which(bank$Current.Loan.Amount == 99999999)
bank <- bank[-pos_CLA,]
pos_CS <- which(bank$Credit.Score > 900 & !(is.na(bank$Credit.Score)))
bank <- bank[-pos_CS,]

#Analyze credit and income na's
pos_NACS <- which(is.na(bank$Credit.Score))
banknc <- bank[pos_NACS,]
bankc <- bank[-pos_NACS,]

#The records with no credit score match those with no income.
#However, those records reflect varied employment and open credit.
#We are removing these records as incomplete.
bank <- bank[-(pos_NACS),]

#Cleanup
rm(banknc)
rm(bankc)

#Analyze Bankruptcies NA records
pos_bnkrpt <- which(is.na(bank$Bankruptcies))
nabnkrpt <- bank[pos_bnkrpt,]

#There are 125 records which accounts for .1% of the data.
#These are all for short term loans, but do not appear to
#have any other pattern with other data (i.e. income, employment, etc)
#We are removing these as incomplete records.
#This also will remove the 5 missing tax lien records.
bank <- bank[-(pos_bnkrpt),]

#cleanup
rm(nabnkrpt)
rm(list=ls(pattern="^pos_"))

#Review the distribution of all numeric variables to determine if they have a very strong skew.
#transforming them can even them out across the data set.
num_var_names <- c("Current.Loan.Amount","Credit.Score","Annual.Income","Monthly.Debt",
                   "Years.of.Credit.History","Number.of.Open.Accounts",
                   "Number.of.Credit.Problems","Current.Credit.Balance","Maximum.Open.Credit",
                   "Bankruptcies","Tax.Liens")

num_var_pos <- which(names(bank) %in% num_var_names)

for(vals in num_var_pos){
  hist(bank[,vals],main=names(bank)[vals],xlab="Values")
}

#These may benefit from transformations
#Annual.Income, Current.Credit.Balance, Maximum.Open.Credit, Monthly.Dept
log.Annual.Income <- log(bank$Annual.Income+1)
log.Current.Credit.Balance <- log(bank$Current.Credit.Balance+1)
log.Maximum.Open.Credit <- log(bank$Maximum.Open.Credit+1)
log.Monthly.Debt <- log(bank$Monthly.Debt+1)

hist(log.Annual.Income)
hist(log.Current.Credit.Balance)
hist(log.Maximum.Open.Credit)
hist(log.Monthly.Debt)

#These appear to be better distributed across the data.
#Update these fields to use the log of their values.
bank$log.Annual.Income <- log.Annual.Income
bank$log.Current.Credit.Balance <- log.Current.Credit.Balance
bank$log.Maximum.Open.Credit <- log.Maximum.Open.Credit
bank$log.Monthly.Debt <- log.Monthly.Debt
bank$Annual.Income <- NULL
bank$Current.Credit.Balance <- NULL
bank$Maximum.Open.Credit <- NULL
bank$Monthly.Debt <- NULL

#Check for Correlation between variables.  
#Reducing this is ideal for both nnet and log.reg.
banknums <- matrix(,nrow=dim(bank)[1])
for(pos in num_var_pos){
  banknums <- cbind(banknums,bank[,pos])
}
banknums <- banknums[,-1]

dimnames(banknums) <- list(c(),num_var_names)
results <- round(cor(banknums),3)

#High correlation seen between 
#log.max.open.credit and bankruptcies.
#Years.credit.history and number.open.accounts
#Since Bankruptcies is mostly 0, we will remove that one.
#Number of open accounts represents a present state vs historic.
#We will remove years of credit history
bank$Bankruptcies <- NULL
bank$Years.of.Credit.History <- NULL

#Cleanup
rm(list=ls(pattern="^num_var"))
rm(list=ls(pattern="^log."))
rm(vals)
rm(pos)
rm(results)
rm(banknums)
summary(bank)
