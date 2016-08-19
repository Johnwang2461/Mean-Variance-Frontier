#Loan Club Data
library(plyr)
library(dplyr)
library(data.table)

#Lending Club How it Works:
#Lending Club is a an online credit marketplace, meaning that it facilitates the lending by
#investors to borrowers through its platform. Borrowers are qualified and given a particular grade
#and subgrade based off of a variety of factors such as income, open accounts, max credit limit,
#a ent choices with respect to overall grade fall under for each year's time period. The years
#of analysis will thus be from 2007-2012 with full finished terms by December 2012.

#Variables for calculating return
cols<- c("grade", "int_rate", "loan_status", "term", "funded_amnt", "total_pymnt", "issue_d")

data_2007_2011 <- fread("C:\\Users\\John\\Documents\\R\\ECON 410W\\Paper\\LoanStats3a.csv", select = cols)
data_2012_2013 <- fread("C:\\Users\\John\\Documents\\R\\ECON 410W\\Paper\\LoanStats3b.csv", select = cols)

#Methodology
#Years for data: I choose the year ranges of 2007-2011 and 2012-2013 from Lending Club's data because
#I wanted to be able to approximate the expected returns based off of the past. In order to do so,
#I needed loans that have reached their full maturity date of either 3 years or 5 years. These loans
#have either been fully paid off or charged off at some point by the end of the term, making them
#appropriate for a retrospective analysis of the expected returns. The data is analyzed until the 
#issued date of Dec 2012 because examining the investment's placement along the mean variance
#frontier for each year requires loans be brought to completion, and Dec. 2012's 3 year term will
#be brought to completion at the end of Dec. 2015, thus we are able to use the data for that.

#The variables chosen
#"grade" was chosen because it will provide the foundation for the aggregate of all loan datas 
#expected return for placement later along the mean variance frontier

#"subgrade" was chosen because even within grades there are varying return values because of
#the differences among the borrowers; however, the subgrades' interest rates will be incorporated
#into the grade's final expected return value.

#"int_rate" was chosen because it is the nominal value that represents the interest rate

#"term" was chosen in combination with "issue_d" and "loan_status" to indicate whether an
#investment is already completed by the final month of analysis: Dec. 2012

#"funded_amnt" and "total_pymnt" will be used to account for whether the payment paid off both 
#the principle and the interest. IF not, it will be used to assess the expected return for the 
#particular investment grade.

data_2007_2012_temp <- bind_rows(data_2012_2013, data_2007_2011)
nrow(data_2007_2012_temp[data_2007_2012_temp$grade == "G",]) #38661

#First take out the 5 year terms that will not be usable in the 2012-2013 data by using only
#the number, then converting the string into a numeric, and finally taking out any term length
#that is greater than 36.


data_2012_2013$term <-substring(data_2012_2013$term, 0, 3)
data_2012_2013$term <- as.numeric(data_2012_2013$term)
data_2012_2013 <- data_2012_2013[!data_2012_2013$term>36,]

#The last entry for Dec-2012 is in 99487. Every date before is guaranteed to be Jan-2013 or later
#The data was not perfectly arranged, so in order to clean the data, we use only the data from
#row 99487 or later. Then we remove any dates that are Jan-2013 that fall within the subsetted data
#This allows the dates of analysis be up to Dec. 2015, which corresponds to the final month and 
#year of analysis.

min(which(data_2012_2013$issue_d == "Dec-2012"))
data_2012_2013 <- data_2012_2013[99487:nrow(data_2012_2013), ]
data_2012_2013 <- data_2012_2013[!data_2012_2013$issue_d == "Jan-2013",]

#To keep the 2007-2011 data consistent, the terms will be converted into numerics as well. Also,
#the data needs to be removed of any 5 year terms in 2011 because the end loan date would be in
#2016, which does not correspond to the measurements of the 2012-2013 data using 2015 as the 
#final year for analysis.
data_2007_2011$term <-substring(data_2007_2011$term, 0, 3)
data_2007_2011$term <- as.numeric(data_2007_2011$term)

nrow(data_2007_2012[data_2007_2012$grade == "G",]) #38661

issued <- data.frame(unique(data_2007_2011$issue_d), stringsAsFactors = FALSE)

i <- 1
for(i in i:12 ) {
  data_2007_2011<-data_2007_2011[!(data_2007_2011$term == 60 & data_2007_2011$issue_d==issued[i,]), ]
}

#Combine the relevant years of data
data_2007_2012 <- bind_rows(data_2012_2013,data_2007_2011)

#We are going to substring the years from issued date for use in later calculations.
data_2007_2012$year <- substring(data_2007_2012$issue_d, 5,8)

#Use data 
unique(data_2007_2012$loan_status)
nrow(data_2007_2012[which(data_2007_2012$loan_status == "Late (31-120 days)"), ])
nrow(data_2007_2012[which(data_2007_2012$loan_status == "In Grace Period"), ])
nrow(data_2007_2012[which(data_2007_2012$loan_status == "Default"), ])

#For analysis purposes, the loan statuses of "Late (31-120 days)", "In Grace Period", and "Default"
#will be assumed to be equivalent to "Charged Off" because at the time of analysis, these loans are
#not fully paid off and should not be classified as such. In addition, these loan statuses are 
#late in some way, which will not yield the full payment plus interest; however, they can still be
#used for calculating expected returns.

data_2007_2012[which(data_2007_2012$loan_status == "Late (31-120 days)"), "loan_status"] <- "Charged Off"
data_2007_2012[which(data_2007_2012$loan_status == "In Grace Period"), "loan_status"] <- "Charged Off"
data_2007_2012[which(data_2007_2012$loan_status == "Default"), "loan_status"] <- "Charged Off"

nrow(data_2007_2012[which(data_2007_2012$loan_status == "Charged Off"), "loan_status"])

nrow(data_2007_2012[which(data_2007_2012$loan_status == "Charged Off"),])

#We must calculate actual interest rate that happens for each loan accounting for earlier payment
#as well as defaults:

#First we make the int rate and funded amnt, and total_pymnt into numerics
data_2007_2012$int_rate <-substring(data_2007_2012$int_rate, 0, 6)
data_2007_2012$int_rate<-as.numeric(data_2007_2012$int_rate)

data_2007_2012$funded_amnt <- as.numeric(data_2007_2012$funded_amnt)
data_2007_2012$total_pymnt <- as.numeric(data_2007_2012$total_pymnt)

#We now attempt to identify the actual interest rate that occurs for the lending club loans
#There are some interesting pecularities regarding the data: using the give interest rate is not
#feasiable because of the nature of how loans work. Some individuals pay their loans back early
#thus the lenders do not receive the full principal plus interest accrument from the loans
#on the other hand, to say that an individual received their original percentage when someone
#charges off their loan is also not fair because these lenders lost money on their investment
#which must be accounted for in the returns. 

#The formula to calculate total payment with a given interest rate is as follows:
#total payment = ((interest rate/100/12) * (funded amount) * term) / (1-(1+interest rate)^-term)
#interest rate is divided by 100 then 12 because we are given the percent interest rate and want
#to convert it to a decimal and then account for monthly compounding.
#Using the information of funded amount and term, the algorithm takes these pieces of information
#along with the interest rate for the respective loan as a guideline to perform total payment
#calculations. The result is subsequently compared to the actual total payment, and if the number
#is within +/- 10% of the actual total payment, then it is marked down as the actual interest rate
#otherwise the algorithm will continue subtracting 1% until a close approximate interest rate
#is reached. 

#Notes: There were a few interesting cases: The 0 total payment case and the higher interest rate
#case. In the 0 total payment case, an arbitrary hard limit of -250% because the lowest is -214.15
#otherwise. There are only a few 0 total payment cases, so the expected return won't be hugely
#affected. In the higher interest rate case, another algorithm was designed to keep increasing
#until actual interest rate was found.
data_2007_2012$actual_int_rate <- NA

j <- 1
for(j in j:nrow(data_2007_2012)) {
  if (data_2007_2012[j,"total_pymnt"]==0) {
    j= j+1
  }
  
  i <- data_2007_2012[j, "int_rate"]
  while (i>-1000) {
    if(i/1200*data_2007_2012[j,"funded_amnt"]*data_2007_2012[j,"term"]/(1-(1+i/1200)^(-data_2007_2012[j,"term"])) > data_2007_2012[j,"total_pymnt"]*.9 && i/1200*data_2007_2012[j,"funded_amnt"]*data_2007_2012[j,"term"]/(1-(1+i/1200)^(-data_2007_2012[j,"term"])) < data_2007_2012[j,"total_pymnt"]*1.1) {
      data_2007_2012[j, "actual_int_rate"] <- i
      break    
    }

     else {
       if(i/1200*data_2007_2012[j,"funded_amnt"]*data_2007_2012[j,"term"]/(1-(1+i/1200)^(-data_2007_2012[j,"term"])) > data_2007_2012[j,"total_pymnt"]*1.5) {
         i= i-1
       }
       else {
       i= i-0.1
       }
     }
   }
 }

for(j in j:nrow(data_2007_2012)) {
  if (data_2007_2012[j,"total_pymnt"]==0) {
    j= j+1
  }
  
  i <- data_2007_2012[j, "int_rate"]
  while (i>-1000) {
    if(i/1200*data_2007_2012[j,"funded_amnt"]*data_2007_2012[j,"term"]/(1-(1+i/1200)^(-data_2007_2012[j,"term"])) > data_2007_2012[j,"total_pymnt"]*.9 && i/1200*data_2007_2012[j,"funded_amnt"]*data_2007_2012[j,"term"]/(1-(1+i/1200)^(-data_2007_2012[j,"term"])) < data_2007_2012[j,"total_pymnt"]*1.1) {
      data_2007_2012[j, "actual_int_rate"] <- i
      break    
    }
    
    else {
        i= i-0.1
    }
  }
}

#higher interest case
length(which(is.na(data_2007_2012$actual_int_rate) == TRUE))
NA_int_rate<- which(is.na(data_2007_2012$actual_int_rate) == TRUE)

k<-1
for(k in k:length(NA_int_rate)) {
  j = NA_int_rate[k]
  if (data_2007_2012[j,"total_pymnt"]==0) {
    next
  }
  
  i <- data_2007_2012[j, "int_rate"]
  while (i>-1000) {
    if(i/1200*data_2007_2012[j,"funded_amnt"]*data_2007_2012[j,"term"]/(1-(1+i/1200)^(-data_2007_2012[j,"term"])) > data_2007_2012[j,"total_pymnt"]*.9 && i/1200*data_2007_2012[j,"funded_amnt"]*data_2007_2012[j,"term"]/(1-(1+i/1200)^(-data_2007_2012[j,"term"])) < data_2007_2012[j,"total_pymnt"]*1.1) {
      data_2007_2012[j, "actual_int_rate"] <- i
      break    
    }
    
    else {
      i= i+0.1
    }
  }
}

#0 total payment case
total_0<- which(data_2007_2012$total_pymnt == 0)
min(data_2007_2012$actual_int_rate, na.rm = TRUE)
data_2007_2012[which(data_2007_2012$total_pymnt == 0), "actual_int_rate"] <- -250

saved_actual_int_rate <- data.frame(data_2007_2012$actual_int_rate)
save(saved_actual_int_rate, file="saved_actual_int_rate.Rda")

load("~/saved_actual_int_rate.Rda")
data_2007_2012$actual_int_rate <- saved_actual_int_rate

#To calculate real returns, we must look at the inflation rate as well.
#The CPI data is for annual average for all of the months in that year, and the months of Jan.,
#Feb., and March are averaged and included for the year of 2016 to provide a rough estimate of
#the CPI for 2016. 
CPIAUCSL <- read.csv("~/R/ECON 410W/Paper/CPIAUCSL.csv")

#Inflation Calculation: (CPI2 - CPI1)/CPI1 * 100 = Inflation
CPIAUCSL$VALUE2 <- CPIAUCSL[2:(nrow(CPIAUCSL)+1), "VALUE"]
CPIAUCSL$Inflation <- ((CPIAUCSL$VALUE2 - CPIAUCSL$VALUE)/CPIAUCSL$VALUE *100)

#Average Inflation over the course of the loan. We are using the average inflation over the course
#of the loan because we are calculating the expected real returns.
data_2007_2012$Avg_Inflation <- NA
data_2007_2012[which(data_2007_2012$year == "2007" & data_2007_2012$term == 36), "Avg_Inflation"] = mean(CPIAUCSL[61:63, "Inflation"])
data_2007_2012[which(data_2007_2012$year == "2008" & data_2007_2012$term == 36), "Avg_Inflation"] = mean(CPIAUCSL[62:64, "Inflation"])
data_2007_2012[which(data_2007_2012$year == "2009" & data_2007_2012$term == 36), "Avg_Inflation"] = mean(CPIAUCSL[63:65, "Inflation"])
data_2007_2012[which(data_2007_2012$year == "2010" & data_2007_2012$term == 36), "Avg_Inflation"] = mean(CPIAUCSL[64:66, "Inflation"])
data_2007_2012[which(data_2007_2012$year == "2011" & data_2007_2012$term == 36), "Avg_Inflation"] = mean(CPIAUCSL[65:67, "Inflation"])
data_2007_2012[which(data_2007_2012$year == "2012" & data_2007_2012$term == 36), "Avg_Inflation"] = mean(CPIAUCSL[66:68, "Inflation"])
data_2007_2012[which(data_2007_2012$year == "2007" & data_2007_2012$term == 60), "Avg_Inflation"] = mean(CPIAUCSL[61:65, "Inflation"])
data_2007_2012[which(data_2007_2012$year == "2008" & data_2007_2012$term == 60), "Avg_Inflation"] = mean(CPIAUCSL[62:66, "Inflation"])
data_2007_2012[which(data_2007_2012$year == "2009" & data_2007_2012$term == 60), "Avg_Inflation"] = mean(CPIAUCSL[63:67, "Inflation"])
data_2007_2012[which(data_2007_2012$year == "2010" & data_2007_2012$term == 60), "Avg_Inflation"] = mean(CPIAUCSL[64:68, "Inflation"])

#We now subtract the actual interest rate calculated with the inflation to get the real returns 
#for the lifepsan of the loan.
data_2007_2012$real_returns<-data_2007_2012$actual_int_rate - data_2007_2012$Avg_Inflation

#Now we calculate the expected returns for each grade using the real return amounts


#Grade A:
sapply(data_2007_2012[which(data_2007_2012$grade=="A"), "actual_int_rate"], mean) #4.610442
mean(data_2007_2012[which(data_2007_2012$grade=="A"), "Avg_Inflation"]) #1.469532

sapply(data_2007_2012[which(data_2007_2012$grade=="A"), "real_returns"], mean) #3.14091
sapply(data_2007_2012[which(data_2007_2012$grade=="A"), "real_returns"], sd) #13.71869

sapply(data_2007_2012[which(data_2007_2012$grade =="G"), "total_pymnt"], mean)

#Grade B
sapply(data_2007_2012[which(data_2007_2012$grade=="B"), "actual_int_rate"], mean) #6.175417
mean(data_2007_2012[which(data_2007_2012$grade=="B"), "Avg_Inflation"]) #1.385317

sapply(data_2007_2012[which(data_2007_2012$grade=="B"), "real_returns"], mean) #4.7901
sapply(data_2007_2012[which(data_2007_2012$grade=="B"), "real_returns"], sd) #18.9229

#Grade C
sapply(data_2007_2012[which(data_2007_2012$grade=="C"), "actual_int_rate"], mean) #6.206902
mean(data_2007_2012[which(data_2007_2012$grade=="C"), "Avg_Inflation"]) #1.412324

sapply(data_2007_2012[which(data_2007_2012$grade=="C"), "real_returns"], mean) #4.794577
sapply(data_2007_2012[which(data_2007_2012$grade=="C"), "real_returns"], sd) #23.11102

#Grade D
sapply(data_2007_2012[which(data_2007_2012$grade=="D"), "actual_int_rate"], mean) #6.603112
mean(data_2007_2012[which(data_2007_2012$grade=="D"), "Avg_Inflation"]) #1.438259

sapply(data_2007_2012[which(data_2007_2012$grade=="D"), "real_returns"], mean) #5.164853
sapply(data_2007_2012[which(data_2007_2012$grade=="D"), "real_returns"], sd) #25.83946

#Grade E
sapply(data_2007_2012[which(data_2007_2012$grade=="E"), "actual_int_rate"], mean) #6.770709
mean(data_2007_2012[which(data_2007_2012$grade=="E"), "Avg_Inflation"]) #1.588469

sapply(data_2007_2012[which(data_2007_2012$grade=="E"), "real_returns"], mean) #5.1812241
sapply(data_2007_2012[which(data_2007_2012$grade=="E"), "real_returns"], sd) #26.20359

#Grade F
sapply(data_2007_2012[which(data_2007_2012$grade=="F"), "actual_int_rate"], mean) #6.631329
mean(data_2007_2012[which(data_2007_2012$grade=="F"), "Avg_Inflation"]) #1.699662

sapply(data_2007_2012[which(data_2007_2012$grade=="F"), "real_returns"], mean) #4.931667
sapply(data_2007_2012[which(data_2007_2012$grade=="F"), "real_returns"], sd) #26.14608

#Grade G
sapply(data_2007_2012[which(data_2007_2012$grade=="G"), "actual_int_rate"], mean) #5.66557
mean(data_2007_2012[which(data_2007_2012$grade=="G"), "Avg_Inflation"]) #1.742679

sapply(data_2007_2012[which(data_2007_2012$grade=="G"), "real_returns"], mean) #3.922891
sapply(data_2007_2012[which(data_2007_2012$grade=="G"), "real_returns"], sd) #28.43761


#Construction of the Mean-Variance Curve
# "In general, portfolios of two assets or portfolios fill out a hyperbolic curve through the
# two assets. The curve is sharper the less correlated are the two assets, because the portfolio
# variance benefits from increasing diversification. Portfolios of a risky asset and risk free
#rate give rise to straight lines in mean-standard deviation space." (Cochrane 80)

#To construct our Mean-Variance Curve, we are using our risk free rate of US 3 month T-bills
#averaged out for the entire year as well as S&P 500 for the entire year from 2007-2015. To get
#the lagged returns for S&P 500, the months of Jan., Feb., and March have been included to provide
#an estimate for the year of 2016 by averaging out the S&P 500 for those months.

Risk_Free_Rate <- read.csv("~/R/ECON 410W/Paper/DTB3.csv")
SP500 <- read.csv("~/R/ECON 410W/Paper/SP500.csv")

#Get Risk Free Rate's Real Returns
Risk_Free_Rate$Inflation <- CPIAUCSL[61:69, "Inflation"]
Risk_Free_Rate$Real_Return <- Risk_Free_Rate$VALUE - Risk_Free_Rate$Inflation

#Average Risk Free Rate Returns from 2007-2016
mean(Risk_Free_Rate$VALUE, na.rm = TRUE) #0.7
mean(Risk_Free_Rate$Inflation) #1.547815
mean(Risk_Free_Rate$Real_Return, na.rm = TRUE) # -0.8478145


#Expected value of stochastic discount factor: E(m)
EM <- 1/ RFR

#Get S&P500 Lagged Real Returns
SP500$VALUE2 <- SP500[2:(nrow(SP500)+1), "VALUE"]
SP500$Returns <- (SP500$VALUE2-SP500$VALUE)/SP500$VALUE
SP500$Inflation <- CPIAUCSL[61:70, "Inflation"]
SP500$Real_Returns <- SP500$Returns*100 - SP500$Inflation

#SP500 Expected Returns from 2007-2015
mean(SP500$Returns, na.rm=TRUE)*100
mean(SP500$Inflation, na.rm = TRUE)
SPR <- mean(SP500$Real_Returns, na.rm = TRUE) #2.72610354197717
SDSPR <- sd(SP500$Real_Returns, na.rm = TRUE) #15.6785142546016

#Calculating volatility of the stochastic discount factor
SDM <- ((SPR-RFR)*EM/(-1*SDSPR)) #0.268867829623294

#Equation for Expected Returns for a particular asset given the risk free rate and voltaility:
# -1(SDM/EM)(SDAsset) + Rf

min(data_2007_2012$int_rate)
max(data_2007_2012$int_rate)

#Charged Off Percentage
A_aggregate <- data_2007_2012[which(data_2007_2012$grade == "A"), ]
A_Charged_Off <- sum(A_aggregate$loan_status == "Charged Off")/nrow(A_aggregate)*100

B_aggregate <- data_2007_2012[which(data_2007_2012$grade == "B"), ]
B_Charged_Off <- sum(B_aggregate$loan_status == "Charged Off")/nrow(B_aggregate)*100

C_aggregate <- data_2007_2012[which(data_2007_2012$grade == "C"), ]
C_Charged_Off <- sum(C_aggregate$loan_status == "Charged Off")/nrow(C_aggregate)*100

D_aggregate <- data_2007_2012[which(data_2007_2012$grade == "D"), ]
D_Charged_Off <- sum(D_aggregate$loan_status == "Charged Off")/nrow(D_aggregate)*100

E_aggregate <- data_2007_2012[which(data_2007_2012$grade == "E"), ]
E_Charged_Off <- sum(E_aggregate$loan_status == "Charged Off")/nrow(E_aggregate)*100

F_aggregate <- data_2007_2012[which(data_2007_2012$grade == "F"), ]
F_Charged_Off <- sum(F_aggregate$loan_status == "Charged Off")/nrow(F_aggregate)*100

G_aggregate <- data_2007_2012[which(data_2007_2012$grade == "G"), ]
G_Charged_Off <- sum(G_aggregate$loan_status == "Charged Off")/nrow(G_aggregate)*100

Charged_Off_All <- data.frame(c("A", "B", "C","D","E","F","G"))
Charged_Off_All$Charged_Off_Percentage <- c(A_Charged_Off,B_Charged_Off,C_Charged_Off,D_Charged_Off,E_Charged_Off,F_Charged_Off,G_Charged_Off)

#Given Interest =/= Actual Interest Rate
gradeA <- data_2007_2012[which(data_2007_2012$grade == "A"),]
nrow(gradeA[which(gradeA$int_rate == gradeA$actual_int_rate),])/nrow(gradeA) #.9149688
mean(gradeA$int_rate) #7.466445

gradeB <- data_2007_2012[which(data_2007_2012$grade == "B"),]
nrow(gradeB[which(gradeB$int_rate == gradeB$actual_int_rate),])/nrow(gradeB) #.8045709
mean(gradeB$int_rate) #11.69818

gradeC <- data_2007_2012[which(data_2007_2012$grade == "C"),]
nrow(gradeC[which(gradeC$int_rate == gradeC$actual_int_rate),])/nrow(gradeC) #.7240383
mean(gradeC$int_rate) #14.54043

gradeD <- data_2007_2012[which(data_2007_2012$grade == "D"),]
nrow(gradeD[which(gradeD$int_rate == gradeD$actual_int_rate),])/nrow(gradeD) #.6745889
mean(gradeD$int_rate) #17.02485

gradeE <- data_2007_2012[which(data_2007_2012$grade == "E"),]
nrow(gradeE[which(gradeE$int_rate == gradeE$actual_int_rate),])/nrow(gradeE) #.6276304
mean(gradeD$int_rate) #17.02485

gradeF <- data_2007_2012[which(data_2007_2012$grade == "F"),]
nrow(gradeF[which(gradeF$int_rate == gradeF$actual_int_rate),])/nrow(gradeF) #.5664622
mean(gradeF$int_rate) #19.51211

gradeG <- data_2007_2012[which(data_2007_2012$grade == "G"),]
nrow(gradeG[which(gradeG$int_rate == gradeG$actual_int_rate),])/nrow(gradeG) #.4966443
mean(gradeG$int_rate) #21.14678

data_2007_2012

#Without 2007 & 2008 Data: Stock Market Crash

#Average Risk Free Rate Returns from 2007-2016
RFR<- mean(Risk_Free_Rate$Real_Return, na.rm = TRUE)

#Expected value of stochastic discount factor: E(m)
EM <- 1/ RFR

#Get S&P500 Lagged Real Returns
SP500_No_Crash<- SP500[3:10, ]
SP500_No_Crash$VALUE2 <- SP500_No_Crash[2:(nrow(SP500_No_Crash)+1), "VALUE"]
SP500_No_Crash$Returns <- (SP500_No_Crash$VALUE2-SP500_No_Crash$VALUE)/SP500_No_Crash$VALUE
SP500_No_Crash$Inflation <- CPIAUCSL[63:70, "Inflation"]
SP500_No_Crash$Real_Returns <- SP500_No_Crash$Returns*100 - SP500_No_Crash$Inflation

RFR_No_Crash <- Risk_Free_Rate[3:9,]
RFR_No_Crash$Inflation <- CPIAUCSL[63:69, "Inflation"]
RFR_No_Crash$Real_Returns <- RFR_No_Crash$VALUE-RFR_No_Crash$Inflation

mean(RFR_No_Crash$VALUE, na.rm = TRUE) #0.08142857
mean(RFR_No_Crash$Inflation, na.rm = TRUE) #1.490786
mean(RFR_No_Crash$Real_Returns, na.rm=TRUE) #-1.409357
sd(RFR_No_Crash$Real_Returns, na.rm=TRUE) #0.9861156

mean(SP500_No_Crash$Returns, na.rm = TRUE)*100 #11.16669
mean(SP500_No_Crash$Inflation, na.rm = TRUE) #1.490786
mean(SP500_No_Crash$Real_Returns, na.rm = TRUE) #9.675906
sd(SP500_No_Crash$Real_Returns, na.rm = TRUE) #8.610095

data_2009_2012 <- data_2007_2012[which(data_2007_2012$year >2008),]

nrow(data_2009_2012[data_2009_2012$int_rate == data_2009_2012$actual_int_rate,])/nrow(data_2009_2012) *100
#79.49826

sum(data_2009_2012$loan_status == "Charged Off")/nrow(data_2009_2012)*100 #12.871333

nrow(data_2007_2012[data_2007_2012$int_rate == data_2007_2012$actual_int_rate,])/nrow(data_2007_2012) *100
#79.4886
nrow(data_2007_2012[data_2007_2012$int_rate != data_2007_2012$actual_int_rate,])/nrow(data_2007_2012) *100
#20.5114

sum(data_2007_2012$loan_status == "Charged Off")/nrow(data_2007_2012)*100 #12.871333
#12.94886

#Grade A:
sapply(data_2009_2012[which(data_2009_2012$grade=="A"), "actual_int_rate"], mean) #4.576085
sapply(data_2009_2012[which(data_2009_2012$grade=="A"), "actual_int_rate"], sd) #13.78742
mean(data_2009_2012[which(data_2009_2012$grade=="A"), "Avg_Inflation"]) #1.468627

sapply(data_2009_2012[which(data_2009_2012$grade=="A"), "real_returns"], mean) #3.107459
sapply(data_2009_2012[which(data_2009_2012$grade=="A"), "real_returns"], sd) #13.78332

#Grade B
sapply(data_2009_2012[which(data_2009_2012$grade=="B"), "actual_int_rate"], mean) #6.231239
mean(data_2009_2012[which(data_2009_2012$grade=="B"), "Avg_Inflation"]) #1.382649

sapply(data_2009_2012[which(data_2009_2012$grade=="B"), "real_returns"], mean) #4.84859
sapply(data_2009_2012[which(data_2009_2012$grade=="B"), "real_returns"], sd) #18.90166

#Grade C
sapply(data_2009_2012[which(data_2009_2012$grade=="C"), "actual_int_rate"], mean) #6.293344
mean(data_2009_2012[which(data_2009_2012$grade=="C"), "Avg_Inflation"]) #1.408877

sapply(data_2009_2012[which(data_2009_2012$grade=="C"), "real_returns"], mean) #4.884467
sapply(data_2009_2012[which(data_2009_2012$grade=="C"), "real_returns"], sd) #23.13609

#Grade D
sapply(data_2009_2012[which(data_2009_2012$grade=="D"), "actual_int_rate"], mean) #6.826691
mean(data_2009_2012[which(data_2009_2012$grade=="D"), "Avg_Inflation"]) #1.435879

sapply(data_2009_2012[which(data_2009_2012$grade=="D"), "real_returns"], mean) #5.390812
sapply(data_2009_2012[which(data_2009_2012$grade=="D"), "real_returns"], sd) #25.67282

#Grade E
sapply(data_2009_2012[which(data_2009_2012$grade=="E"), "actual_int_rate"], mean) #6.822837
mean(data_2009_2012[which(data_2009_2012$grade=="E"), "Avg_Inflation"]) #1.591344

sapply(data_2009_2012[which(data_2009_2012$grade=="E"), "real_returns"], mean) #5.231494
sapply(data_2009_2012[which(data_2009_2012$grade=="E"), "real_returns"], sd) #26.37926

#Grade F
sapply(data_2009_2012[which(data_2009_2012$grade=="F"), "actual_int_rate"], mean) #7.455249
mean(data_2009_2012[which(data_2009_2012$grade=="F"), "Avg_Inflation"]) #1.709263

sapply(data_2009_2012[which(data_2009_2012$grade=="F"), "real_returns"], mean) #5.745987
sapply(data_2009_2012[which(data_2009_2012$grade=="F"), "real_returns"], sd) #25.13945

#Grade G
sapply(data_2009_2012[which(data_2009_2012$grade=="G"), "actual_int_rate"], mean) #6.204207
mean(data_2009_2012[which(data_2009_2012$grade=="G"), "Avg_Inflation"]) #1.749779

sapply(data_2009_2012[which(data_2009_2012$grade=="G"), "real_returns"], mean) #4.454428
sapply(data_2009_2012[which(data_2009_2012$grade=="G"), "real_returns"], sd) #27.26007

A_aggregate <- data_2009_2012[which(data_2009_2012$grade == "A"), ]
A_Charged_Off <- sum(A_aggregate$loan_status == "Charged Off")/nrow(A_aggregate)*100

B_aggregate <- data_2009_2012[which(data_2009_2012$grade == "B"), ]
B_Charged_Off <- sum(B_aggregate$loan_status == "Charged Off")/nrow(B_aggregate)*100

C_aggregate <- data_2009_2012[which(data_2009_2012$grade == "C"), ]
C_Charged_Off <- sum(C_aggregate$loan_status == "Charged Off")/nrow(C_aggregate)*100

D_aggregate <- data_2009_2012[which(data_2009_2012$grade == "D"), ]
D_Charged_Off <- sum(D_aggregate$loan_status == "Charged Off")/nrow(D_aggregate)*100

E_aggregate <- data_2009_2012[which(data_2009_2012$grade == "E"), ]
E_Charged_Off <- sum(E_aggregate$loan_status == "Charged Off")/nrow(E_aggregate)*100

F_aggregate <- data_2009_2012[which(data_2009_2012$grade == "F"), ]
F_Charged_Off <- sum(F_aggregate$loan_status == "Charged Off")/nrow(F_aggregate)*100

G_aggregate <- data_2009_2012[which(data_2009_2012$grade == "G"), ]
G_Charged_Off <- sum(G_aggregate$loan_status == "Charged Off")/nrow(G_aggregate)*100

Charged_Off_All <- data.frame(c("A", "B", "C","D","E","F","G"))
Charged_Off_All$Charged_Off_Percentage <- c(A_Charged_Off,B_Charged_Off,C_Charged_Off,D_Charged_Off,E_Charged_Off,F_Charged_Off,G_Charged_Off)

#Given Interest =/= Actual Interest Rate
gradeA <- data_2009_2012[which(data_2009_2012$grade == "A"),]
nrow(gradeA[which(gradeA$int_rate == gradeA$actual_int_rate),])/nrow(gradeA) #.9153168
mean(gradeA$int_rate) #7.452817

gradeB <- data_2009_2012[which(data_2009_2012$grade == "B"),]
nrow(gradeB[which(gradeB$int_rate == gradeB$actual_int_rate),])/nrow(gradeB) #.8044572
mean(gradeB$int_rate) #11.72858

gradeC <- data_2009_2012[which(data_2009_2012$grade == "C"),]
nrow(gradeC[which(gradeC$int_rate == gradeC$actual_int_rate),])/nrow(gradeC) #.7224795
mean(gradeC$int_rate) #14.63303

gradeD <- data_2009_2012[which(data_2009_2012$grade == "D"),]
nrow(gradeD[which(gradeD$int_rate == gradeD$actual_int_rate),])/nrow(gradeD) #.673512
mean(gradeD$int_rate) #17.13962

gradeE <- data_2009_2012[which(data_2009_2012$grade == "E"),]
nrow(gradeE[which(gradeE$int_rate == gradeE$actual_int_rate),])/nrow(gradeE) #.624702
mean(gradeE$int_rate) #18.52083

gradeF <- data_2009_2012[which(data_2009_2012$grade == "F"),]
nrow(gradeF[which(gradeF$int_rate == gradeF$actual_int_rate),])/nrow(gradeF) #.5683297
mean(gradeF$int_rate) #19.71165

gradeG <- data_2009_2012[which(data_2009_2012$grade == "G"),]
nrow(gradeG[which(gradeG$int_rate == gradeG$actual_int_rate),])/nrow(gradeG) #.4965517
mean(gradeG$int_rate) #21.22352

