library(dplyr)
library(tidyverse)
library(ggplot2)
library(psych)
library(ctv)



#rm(gapminder)

#CSUS2011 <- read.clipboard.tab()
#CSUS2011 <-food
GrowingMarket <- read.csv(file = "/home/o/ocampod/fall2017/Econ591/GrowingMarket.csv", header=TRUE, sep= ",")
View(GrowingMarket)
GrowingMarket[1,]
ggplot(data = GrowingMarket) +
  geom_bar(mapping = aes(x = Healthclubs.Gym, y = Membership), stat = "identity")
#ggsave("/home/o/ocampod/fall2017/cs390-ochampoo/Data-Analytics/lab7/quest5.png")
#grow Gym membership

GrowingMarket <- mutate( GrowingMarket, percentage = Membership/Population*100)

######## Growing


######## Start Up Cost #### 4 Month to setup cost 

startup <- read.csv(file = "/home/o/ocampod/fall2017/Econ591/4month.csv", header=TRUE, sep= ",")
starupcost4 <- mutate( startup, RandomCost = 10000)
starupcost4 <- mutate(starupcost4 , startupcost4 =CTO  + COO +  CMO  + CFO + Rent + RandomCost+SBA.Loan)
starupcost4
sum(starupcost4$startupcost4)

View(startupcost4)


#### according to my data we should start off with 201,772 in intial cost.
#### Rev is 90,000k we open up Jan   
### 201,772 +
################ 4 Month Startup Cost ###########



###### Calculating Total  monthly Cost ################

View(totalCost)
totalCost
totalCost <- read.csv(file = "/home/o/ocampod/fall2017/Econ591/monthlcost.csv", header=TRUE, sep= ",")
totalCost <- mutate( totalCost, totalCostPart = SBA.Loan + Mortgage +CFO +COO +CMO+CEO  +CTO+Metal.Hanger.100+ Perc.55.Gallons+ Gas+ Electric + Water+Wipes +bussinessInsurance +Cleaning.supplies +oil.Machines+ Wear.and.Tear+ Yoga.Instructor +Personal.Trainers +Saleperson +Market.Manger+ Receptionists +Dry.Clean.Staff)
totalCost
write.csv(totalCost,"/home/o/ocampod/fall2017/Econ591/data/totalCost.csv")
View(totalCost)
fixedCost
fixedCost <- read.csv(file = "/home/o/ocampod/fall2017/Econ591/data/fixedCost.csv", header=TRUE, sep= ",")
ggplot(data = fixedCost ) +
  geom_bar(mapping = aes( x = reorder(Total.Cost,Month), y = Month  ), stat = "identity")+
  coord_flip()
ggsave("/home/o/ocampod/fall2017/Econ591/data/fixCost.png")

select(fixedCost,Items,Cost)

sum(fixedCost$Cost)### First month Cost
View(fixedCost)
####### Calculating Machine Cost/intialCost ##########
totalMachineCost
totalMachineCost <- read.csv(file = "/home/o/ocampod/fall2017/Econ591/intialCost.csv", header=TRUE, sep= ",")
View(totalMachineCost)

sapply(totalMachineCost, class)

totalMachineCost <- mutate( totalMachineCost, totalCost = Cost * Qty )

totalMachineCost[1,]

View(totalMachineCost)

write.csv(totalMachineCost,"/home/o/ocampod/fall2017/Econ591/data/TotalCost.csv")

sum(starupcost4$startupcost4)
sum(totalMachineCost$totalCost)
View(starupcost4)
View(tota)
sum(starupcost4$startupcost4)
sum(starupcost4$startupcost4)
sum(totalMachineCost$totalCost)  + sum(starupcost4$startupcost4) +   sum(fixedCost$totaCost)  
sum(fixedCost$Cost)
#### Total Machine Cost and startup Cost 201,772


write.csv(intialCost,"/home/o/ocampod/fall2017/Econ591/data/TotalCost.csv")
View(intialCost)


### so my first month shoud be about 800,592 + 100369 with monthy cost of fixed month cost would be 900,961 to starup. 


#### This is total revenue 

totalRev <- read.csv(file = "/home/o/ocampod/fall2017/Econ591/data/Calculation.csv", header=TRUE, sep= ",")
View(totalRev)
totalRev

totalRev <- mutate( totalRev,  MonthlyMemebers=  Week * NumMembers )### number of new
Intial.Memember
totalRev <- mutate( totalRev, Intial.Memember =  MonthlyMemebers * 50)

totalRev <- mutate( totalRev, TotalrevDryCleaners =  Week * costumers * MoneySpentDry )

dryCleaningRev <- select(totalRev,Month,costumers,MoneySpentDry,TotalrevDryCleaners)
View(dryCleaningRev)#### image for drycleaning Rev
totalRev <- mutate( totalRev, Gym.Total.Rev =  Intial.Memember + IncomeByMonth )
gymrev <-  select(totalRev,Month,MonthlyMemebers,Intial.Memember,IncomeByMonth,Gym.Total.Rev)### gym revenue fixed

RealRev <- select(totalRev,Month,TotalrevDryCleaners,Gym.Total.Rev,Totalrev)
View(RealRev)
View(gymrev)

totalRev <- mutate( totalRev, Totalrev =  TotalrevDryCleaners+ IncomeByMonth + Intial.Memember)
View(totalRev)
totalRev[1,]
View(totalRev)
write.csv(totalRev,"/home/o/ocampod/fall2017/Econ591/data/Calculation.csv")
totalRev
ggplot(data = totalRev) +
  geom_point(mapping = aes(x = reorder(Month,Totalrev) , y = Totalrev))
ggsave("/home/o/ocampod/fall2017/Econ591/data/TotalRevenue.png")



########## Total rev ##########
totalRevenueCombine <- read.csv(file = "/home/o/ocampod/fall2017/Econ591/data/Year1Profit.csv", header=TRUE, sep= ",")
View(totalRevenueCombine)
totalRevenueCombine <- mutate( totalRevenueCombine, budget =  Real.Rev + SBA.Loan - MonthlyCost)
totalRevenueCombine <- mutate( totalRevenueCombine, Total.Gym.Rev =  Total.Gym.Rev + Member.Fee)
postive <- select(totalRevenueCombine,Month,Total.Gym.Rev,Total.Dry.Cleaner.Rev,Real.Rev,MonthlyCost,SBA.Loan,budget)
View(postive)
totalRevenueCombine
write.csv(totalRevenueCombine,"/home/o/ocampod/fall2017/Econ591/data/Year1Profit.csv")
sum(totalRevenueCombine$budget)

totalRevenueCombine <- read.csv(file = "/home/o/ocampod/fall2017/Econ591/TotalRev.csv", header=TRUE, sep= ",")
totalRevenueCombine
