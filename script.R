# start code
library(ipumsr)
#install.packages("plm")
library(plm)
library(lmtest)
library(multiwayvcov)
#install.packages("multiwayvcov")
setwd("/Users/zhenyanggong/Desktop/r_code")
# reading data from IPUMs extract
# Change these filepaths to the filepaths of your downloaded extract
cps_ddi_file <- "cps_00007.xml"
cps_data_file <- "cps_00007.dat"
cps_ddi <- read_ipums_ddi(cps_ddi_file) 
cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)
# ipums_val_labels(cps_ddi, YEAR)
# ipums_val_labels(cps_ddi, SERIAL)
# ipums_val_labels(cps_ddi, AGE)
# ipums_val_labels(cps_ddi, SEX)
# ipums_val_labels(cps_ddi, RACE)
# ipums_val_labels(cps_ddi, IND)
# ipums_val_labels(cps_ddi, EDUC)
# ipums_val_labels(cps_ddi, UNION)
# ipums_val_labels(cps_ddi, REGION)
# ipums_val_labels(cps_ddi, INCWAGE)
# ipums_val_labels(cps_ddi, INCWELFR)
# ipums_val_labels(cps_ddi, STATEFIP)
library(dplyr) 
# calculate union density in different sectors
total_worker <- cps_data%>%   
  filter(UNION!=0)%>%
  group_by(YEAR, STATEFIP)%>%
  summarise(TOTAL_WORKER=n())
union_member <- cps_data%>%  
  filter(UNION==2 | UNION==3)%>%
  group_by(YEAR, STATEFIP)%>%
  summarise(UNION_MEMBER=n())

public_data <- cps_data%>%   
filter(CLASSWKR==24 |  CLASSWKR==25  |  CLASSWKR==26  |  CLASSWKR==27  |  CLASSWKR==28)
private_data <- cps_data%>%   
  filter(CLASSWKR==21 |  CLASSWKR==22  |  CLASSWKR==23)
total_public_worker <- public_data%>%   
  filter(UNION!=0)%>%
  group_by(YEAR, STATEFIP)%>%
  summarise(TOTAL_PUBLIC_WORKER=n())
total_public_worker
public_union_member <- public_data%>%  
  filter(UNION==2 | UNION==3)%>%
  group_by(YEAR, STATEFIP)%>%
  summarise(PUBLIC_UNION_MEMBER=n())
public_union_member
total_private_worker <- private_data%>%   
  filter(UNION!=0)%>%
  group_by(YEAR, STATEFIP)%>%
  summarise(TOTAL_PRIVATE_WORKER=n())
private_union_member <- private_data%>%  
  filter(UNION==2 | UNION==3)%>%
  group_by(YEAR, STATEFIP)%>%
  summarise(PRIVATE_UNION_MEMBER=n())

union_labor <- merge(total_public_worker,public_union_member,by=c("YEAR","STATEFIP"))
union_labor <- merge(union_labor,total_private_worker,by=c("YEAR","STATEFIP"))
union_labor <- merge(union_labor,private_union_member,by=c("YEAR","STATEFIP"))

union_labor <- merge(union_labor,total_worker,by=c("YEAR","STATEFIP"))
union_labor <- merge(union_labor,union_member,by=c("YEAR","STATEFIP"))

union_labor
union_labor["PUBLIC_UNION_DENSITY"] <- NA
union_labor["PRIVATE_UNION_DENSITY"] <- NA
union_labor["UNION_DENSITY"] <- NA
union_labor$PUBLIC_UNION_DENSITY <- union_labor$PUBLIC_UNION_MEMBER / union_labor$TOTAL_PUBLIC_WORKER
union_labor$PRIVATE_UNION_DENSITY <- union_labor$PRIVATE_UNION_MEMBER / union_labor$TOTAL_PRIVATE_WORKER
union_labor$UNION_DENSITY <- union_labor$UNION_MEMBER / union_labor$TOTAL_WORKER
union_labor
density <- union_labor%>% 
  select(YEAR, STATEFIP, c(PUBLIC_UNION_DENSITY, PRIVATE_UNION_DENSITY,UNION_DENSITY))
density
density_year <- aggregate(density[, 3:5], list(density$YEAR), mean)
density_year
density_state <- aggregate(density[, 3:5], list(density$STATEFIP), mean)
density_state

density_year$PUBLIC_UNION_DENSITY
X <- c(2000:2018)
Y1 <- density_year$PUBLIC_UNION_DENSITY
Y2 <- density_year$PRIVATE_UNION_DENSITY
Y3 <- density_year$UNION_DENSITY
plot(X, Y1, xlab = "YEAR", ylab ="Average Union Density", ylim = c(0.05, 0.45), las=1, main = "Change of the Average Union Density in Different Sector between 2000 and 2018", type = "o", pch = 20, lwd = 1.5, col = "blue")
lines(X, Y2, type = "o", pch = 20, lwd = 1.5, col="red",)
lines(X, Y3, type = "o", pch = 20, lwd = 1.5,col="black")

X <- c(1:51)
Y1 <- density_state$UNION_DENSITY
plot(X, Y1, xlab = "State Fip Code", ylab ="Average Union Density", las=1, main = "The Average Union Density for 51 states btween 2000 and 2018",ylim = c(0.01, 0.40), pch = 20, lwd = 1.5)

# finish getting union density
# merge union density into cps
# !!!!!  total is public sector workers  !!! Select total population here
cps_data <- cps_data%>%   
  filter(CLASSWKR==24 |  CLASSWKR==25  |  CLASSWKR==26  |  CLASSWKR==27  |  CLASSWKR==28 | CLASSWKR==21 |  CLASSWKR==22  |  CLASSWKR==23)
 
total<-merge(x=cps_data,y=density,by=c("YEAR","STATEFIP"),all.x=TRUE)
# this density rounds to 2 decimals, maybe a problem!

total <- subset(total, YEAR >= 2001)
total <- subset(total, YEAR <= 2018)


# start running regression

# there are some bad data, first, the same individual would occur mutiple times
# second, final income also looks weried
total <- subset(total, EARNWEEK != 0)
total <- subset(total, EARNWEEK != 9999.99)

total <- subset(total, RACE != 999)
total <- subset(total, EDUC != 999 & EDUC != 000 & EDUC != 001)
total <- subset(total, SEX != 9)


# adjust income by CPI
cpi <- data.frame("YEAR" = 2001:2018, "CPI" = c(0.967, 0.941, 0.926, 0.905, 0.882, 0.853, 0.826, 0.804, 0.774, 0.777, 0.764, 0.741, 0.726, 0.715, 0.704, 0.703, 0.694, 0.679))
cpi
total <- merge(x=total,y=cpi,by="YEAR",all.x=TRUE)
total["EARNWEEK_AD"] <- NA
total$EARNWEEK_AD <- total$EARNWEEK * total$CPI
# Experience = Age - Education - 6   
# THIS NEED SOME ATTENTION!
# total["EXPERIENCE"] <- NA
# total$EXPERIENCE <- total$AGE - total$EDUC - 6
# think about union = 1 
total$UNION[total$UNION==1] <- 0
total$UNION[total$UNION>1] <- 1

# 21 22 23 is private sector
total$CLASSWKR[total$CLASSWKR==21 |  total$CLASSWKR==22  |  total$CLASSWKR==23] <- 0
total$CLASSWKR[total$CLASSWKR>1] <- 1

test <- select(total, "YEAR", "STATEFIP", "SERIAL", "SEX", "EDUC", "AGE", "RACE", "UNION","PUBLIC_UNION_DENSITY", "PRIVATE_UNION_DENSITY", "UNION_DENSITY", "CLASSWKR", "EARNWEEK_AD")
#which(! complete.cases(worker_total))
    


worker_total <- na.omit(test)
summary(worker_total)
union_worker_total <- subset(worker_total, UNION == 1)
nonunion_worker_total <- subset(worker_total, UNION == 0)


union_fit1 <- plm(formula=log(EARNWEEK_AD)~PUBLIC_UNION_DENSITY + PRIVATE_UNION_DENSITY + CLASSWKR + SEX + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data=union_worker_total, index=c("STATEFIP"), model="pooling") 
coeftest(union_fit1, vcov=vcovHC(union_fit1, type="sss", cluster="group")) 
union_fit1 <- lm(log(EARNWEEK_AD) ~ PUBLIC_UNION_DENSITY + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data = union_worker_total)
summary(union_fit1)

## IV
total_fit1 <- plm(formula=log(EARNWEEK_AD)~PUBLIC_UNION_DENSITY + PRIVATE_UNION_DENSITY + SEX + EDUC + AGE + RACE + (YEAR * UNION) + (STATEFIP * UNION), data=worker_total, index=c("STATEFIP"), model="pooling") 
coeftest(total_fit1, vcov=vcovHC(total_fit1, type="sss", cluster="group")) 
summary(total_fit1)

total_fit1 <- plm(formula=log(EARNWEEK_AD)~PRIVATE_UNION_DENSITY + SEX + EDUC + AGE + RACE + (YEAR * UNION) + (STATEFIP * UNION), data=worker_total, index=c("STATEFIP"), model="pooling") 
coeftest(total_fit1, vcov=vcovHC(total_fit1, type="sss", cluster="group")) 

nonunion_fit1 <- plm(formula=log(EARNWEEK_AD)~PUBLIC_UNION_DENSITY + PRIVATE_UNION_DENSITY  + PRIVATE_UNION_DENSITY*PUBLIC_UNION_DENSITY + CLASSWKR + SEX + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data=nonunion_worker_total, index=c("STATEFIP"), model="pooling") 
coeftest(nonunion_fit1, vcov=vcovHC(nonunion_fit1, type="sss", cluster="group")) 
nonunion_fit1 <- lm(log(EARNWEEK_AD) ~ PUBLIC_UNION_DENSITY + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data = nonunion_worker_total)
summary(nonunion_fit1)

total_fit1 <- plm(formula=log(EARNWEEK_AD)~PUBLIC_UNION_DENSITY + SEX + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data=worker_total, index=c("STATEFIP"), model="pooling") 
coeftest(total_fit1, vcov=vcovHC(total_fit1, type="sss", cluster="group")) 
total_fit1 <- lm(log(EARNWEEK_AD) ~ PUBLIC_UNION_DENSITY + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data = worker_total)
summary(total_fit1)


union_fit1 <- plm(formula=log(EARNWEEK_AD)~PRIVATE_UNION_DENSITY + SEX + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data=union_worker_total, index=c("STATEFIP"), model="pooling") 
coeftest(union_fit1, vcov=vcovHC(union_fit1, type="sss", cluster="group")) 
union_fit1 <- lm(log(EARNWEEK_AD) ~ PRIVATE_UNION_DENSITY + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data = union_worker_total)
summary(union_fit1)

nonunion_fit1 <- plm(formula=log(EARNWEEK_AD)~PRIVATE_UNION_DENSITY + SEX + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data=nonunion_worker_total, index=c("STATEFIP"), model="pooling") 
coeftest(nonunion_fit1, vcov=vcovHC(nonunion_fit1, type="sss", cluster="group")) 
nonunion_fit1 <- lm(log(EARNWEEK_AD) ~ PRIVATE_UNION_DENSITY + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data = nonunion_worker_total)
summary(nonunion_fit1)

total_fit1 <- plm(formula=log(EARNWEEK_AD)~PRIVATE_UNION_DENSITY + SEX + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data=worker_total, index=c("STATEFIP"), model="pooling") 
coeftest(total_fit1, vcov=vcovHC(total_fit1, type="sss", cluster="group")) 
total_fit1 <- lm(log(EARNWEEK_AD) ~ PRIVATE_UNION_DENSITY + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data = worker_total)
summary(total_fit1)

total_fit1 <- plm(formula=log(EARNWEEK_AD)~UNION_DENSITY + SEX + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data=worker_total, index=c("STATEFIP"), model="pooling") 
coeftest(total_fit1, vcov=vcovHC(total_fit1, type="sss", cluster="group")) 

## IV
total_fit1 <- plm(formula=log(EARNWEEK_AD)~UNION_DENSITY + SEX + EDUC + AGE + RACE + factor(YEAR * UION) + factor(STATEFIP * UION), data=worker_total, index=c("STATEFIP"), model="pooling") 
coeftest(total_fit1, vcov=vcovHC(total_fit1, type="sss", cluster="group")) 


total_fit1 <- plm(formula=log(EARNWEEK_AD)~PUBLIC_UNION_DENSITY + PUBLIC_UNION_DENSITY*PRIVATE_UNION_DENSITY + PRIVATE_UNION_DENSITY + SEX + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data=nonunion_worker_total, index=c("STATEFIP"), model="pooling") 
coeftest(total_fit1, vcov=vcovHC(total_fit1, type="sss", cluster="group")) 
total_fit2 <- lm(log(EARNWEEK_AD) ~ PUBLIC_UNION_DENSITY  + PUBLIC_UNION_DENSITY*PRIVATE_UNION_DENSITY + PRIVATE_UNION_DENSITY + EDUC + factor(YEAR) + factor(STATEFIP), data = worker_total)
summary(total_fit2)

#vcov_STATE <- cluster.vcov(pooled.ols, worker_total$index)
#coeftest(pooled.ols, vcov_STATE)
#table(index(union_worker_total), useNA = "ifany")

union_fit2 <- lm(log(EARNWEEK_AD) ~ PUBLIC_UNION_DENSITY + PUBLIC_UNION_DENSITY*PRIVATE_UNION_DENSITY + PRIVATE_UNION_DENSITY + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data = union_worker_total)
summary(union_fit2)

nonunion_fit2 <- lm(log(EARNWEEK_AD) ~ PUBLIC_UNION_DENSITY + PUBLIC_UNION_DENSITY*PRIVATE_UNION_DENSITY + PRIVATE_UNION_DENSITY + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data = nonunion_worker_total)
summary(nonunion_fit2)

total_fit2 <- lm(log(EARNWEEK_AD) ~ PUBLIC_UNION_DENSITY  + PUBLIC_UNION_DENSITY*PRIVATE_UNION_DENSITY + PRIVATE_UNION_DENSITY + EDUC + factor(YEAR) + factor(STATEFIP), data = worker_total)
summary(total_fit2)

union_fit3 <- lm(log(EARNWEEK_AD) ~ PRIVATE_UNION_DENSITY + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data = union_worker_total)
summary(union_fit3)

nonunion_fit3 <- lm(log(EARNWEEK_AD) ~ PRIVATE_UNION_DENSITY + EDUC + AGE + RACE + factor(YEAR) + factor(STATEFIP), data = nonunion_worker_total)
summary(nonunion_fit3)

total_fit3 <- lm(log(EARNWEEK_AD) ~ PRIVATE_UNION_DENSITY + EDUC + factor(YEAR) + factor(STATEFIP), data = worker_total)
summary(total_fit3)

# graph year avg
total_worker1 <- cps_data%>%   
  filter(UNION!=0)%>%
  group_by(YEAR)%>%
  summarise(TOTAL_WORKER=n())
union_member1 <- cps_data%>%  
  filter(UNION==2)%>%
  group_by(YEAR)%>%
  summarise(UNION_MEMBER=n())
union_labor1 <- merge(total_worker1,union_member1,by="YEAR")
union_labor1
union_labor1["UNION_DENSITY"] <- NA
union_labor1$UNION_DENSITY <- union_labor1$UNION_MEMBER / union_labor1$TOTAL_WORKER
union_labor1
density1 <- union_labor1%>% 
  select(YEAR, UNION_DENSITY)
density1
graph <- merge(x=density1,y=approval,by="YEAR",all.x=TRUE)
graph
g_range <- range(0, graph$UNION_DENSITY, graph$APPROVAL)
plot(graph$YEAR, graph$UNION_DENSITY, type="o", ylim=c(0.0,0.8), xlab="Year", ylab="Percentage")

lines(graph$YEAR, graph$APPROVAL, type="o", pch=22, lty=2, col="red")
legend(2011,0.4, c("Union Density","Union Approval"), cex=0.8, 
       col=c("black","red"), pch=21:22, lty=1:2);

