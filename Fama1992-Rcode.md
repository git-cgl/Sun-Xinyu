```
########################################################################################################
#-------------------------------------------------------------------------------------------------------
# Author    : Sun Xinyu
# Date      : 11/02/2019
# Filename  : Fama1992-Rcode.md
# Code      : Fama-French 1992 Replication 
# Sections  : 
#            (1) CRSP Data setup
#            (2) Table 1 pre-beta, post-beta, post-beta (ln(ME))
#            (3) 
#           (**) CRSP and Compustat Data Merge)
#           (**) Data Wrangling
#-------------------------------------------------------------------------------------------------------
########################################################################################################

rm(list=ls(all=TRUE))
library(readr)         # For reading the data
library(dplyr)         # Data wrangling


#-----------------------------------
# (1) CRSP Data setup
#-----------------------------------

library(readxl);library(dplyr);library(zoo);library(purrr);library(StatMeasures);
require(data.table); library(lubridate)
setwd("E:/博士资料库/博二上/实证资产定价/Fama 1992")
compustat = read_excel("Comp.xlsx") #Comp
crsp=read_excel("Crsp.xlsx") #Crsp
Price=read_excel("Price.xlsx") #Price
Rf=read_excel("bond.xlsx")
crsp01=crsp%>%left_join(Price) 

comp=compustat%>%rename(sic=`Standard Industry Classification Code`)%>%
  filter(sic>6999|sic<6000)%>%
  filter(`Stock Exchange Code`==11|`Stock Exchange Code`==12|`Stock Exchange Code`==14)%>%
 # rename(permno=`Historical CRSP PERMNO Link to COMPUSTAT Record`)%>%
  rename(share=`Common Shares Outstanding`)%>%
  rename(price12=`Price Close - Annual - Calendar`)%>%
  rename(date=`Data Date`)%>%
  rename(ticker=`Ticker Symbol`)%>%
  rename(exchange=`Stock Exchange Code`)%>%
  rename(BE=`Book Value Per Share`)

crsp1=crsp01%>%rename(price=`Price or Bid/Ask Average`)%>%
  rename(sic=`Standard Industrial Classification Code`)%>%
  rename(ticker=`Ticker Symbol`)%>%
  rename(exchange=`Exchange Code`)%>%
  rename(date=`Names Date`)%>%
  rename(cusip=`CUSIP Header`)%>%
  rename(share=`Shares Outstanding`)%>%
 # rename(vwrm=`Value-Weighted Return-incl. dividends`)%>%
#  rename(ewrm=`Equal-Weighted Return-incl. dividends`)%>%
  rename(permno=`PERMNO`)%>%
  rename(shrcd=`Share Code`)%>%
  filter(sic>6999|sic<6000)
  
crsp2=crsp1%>%  
  mutate(t=floor((date-700)/10000))%>%
  group_by(permno,t)%>%
  mutate(year=substr(date,1,4))%>%
  mutate(month=substr(date, 5, 6),
                   has_June=any(month=="06"))%>%
  #mutate(month=substr(date, 5, 6),has_Dec=any(month=="12"))%>%
  mutate(yearmonth=substr(date,1,6))%>%
  filter(has_June==TRUE)%>%
  #filter(has_Dec==TRUE)%>%
  filter(shrcd==10|shrcd==11|(is.na(shrcd)&!is.na(price)))%>%
  filter(exchange==1|exchange==2|exchange==3)%>%
  mutate(price1=abs(price))%>%
  filter(Returns!="C"&Returns!="B")

crsp2$Returns=as.numeric(crsp2$Returns) #as.integer
Rf=Rf%>%mutate(rf1=rf/100)%>%select(yearmonth,rf1)
crsp2$yearmonth=as.numeric(crsp2$yearmonth)
crsp2=crsp2%>%left_join(Rf)
crsp2$Returns1=crsp2$Returns[]-crsp2$rf1[] #calculate excess return

crsp2$ME=crsp2$price1*crsp2$share/1000 ##calculate ME and filter as available
filter(crsp2,ME>0)
  
crsp2$y=ifelse(crsp2$month=='06',crsp2$ME,NA) ## add fixed yearly ME for every stock 
crsp21=crsp2%>%group_by(permno)%>%arrange(permno,t)%>%mutate(y1=lag(y,12))
crsp3=crsp21%>%  
  group_by(permno,t)%>%
  mutate(MEt=sum(y1,na.rm = TRUE))%>%
  filter(MEt!=0)


crsp3$month=as.numeric(crsp3$month)
crsp3$year=as.numeric(crsp3$year)  
crsp4=crsp3%>% ## figure out cumulative 24-60 months(for each permno)
  group_by(permno)%>%
  mutate(REt=lag(Returns1,24))%>%
  filter(!is.na(REt))
  
NYSE=crsp4%>% ## NYSE size portfolio number
  filter(exchange==1)%>%
  filter(t>=1963)%>%  ##
  group_by(t)%>%
  mutate(portfo=decile(MEt))%>%
  group_by(t,portfo)%>%
  mutate(value=max(MEt))

breakpoint=NYSE%>%select(t,portfo,value)%>%unique()%>%
  arrange(t,portfo)# produce unique NYSE breakpoint value

bkpt2=crsp4%>%left_join(NYSE)%>%filter(t>=1963) ##

bkpt3=bkpt2%>%select(permno,t,portfo,MEt)%>%unique()

for(i in 1:length(bkpt3$permno)){
  if(is.na(bkpt3$portfo[i])==T){
    test=breakpoint$value[breakpoint$t==bkpt3$t[i]]
    k=1
    while(k<10 & bkpt3$MEt[i]>test[k]){
      k=k+1
    }
    bkpt3$portfo[i]=k
  }
}  ## merge other exchanges stock with NYSE portfo standard

test2=crsp4%>%
  select(t,permno,month,MEt)%>%
  filter(month==6)%>%
  group_by(t)%>%
  mutate(sum1=sum(MEt))%>%
  mutate(weight=MEt/sum1)%>%
  mutate(lagweight=lag(weight))

test3=test2%>%
  select(t,permno,MEt,weight,lagweight)
  
crsp5=crsp4%>%
  left_join(bkpt3)%>%
  left_join(test3)%>%
  group_by(t,month)%>%
  mutate(val_wetReturn=Returns*lagweight)%>%
  mutate(sum2=sum(val_wetReturn,na.rm = TRUE))

crsp5$sum2=crsp5$sum2[]-crsp5$rf1[] # value-weighted portfolio Rm's excess return

crsp6=crsp5%>%
  select(t,permno,portfo,month,sum2,Returns1)%>%
  group_by(permno)%>%
  arrange(desc(t),desc(month))%>%
  mutate(lagsum=lead(sum2)) #Find the "next" or "previous" values in a vector.

crsp7=crsp5%>%
  left_join(crsp6)

namelist=crsp7$permno%>%
  unique()

crsp8=crsp7%>%
  mutate(beta1=NA)

for(i in namelist){
  testpno=crsp6 %>% filter(permno==i)
  year2=testpno$t%>% unique()
  for(k in year2){
    testpno2=testpno%>%
    filter(t<k)
    if(length(testpno2$month)>=24){
      if(length(testpno2$month)>60){
        testpno2=testpno2%>%arrange(desc(t),desc(month))
        testpno2=testpno2[1:60,]
      }
      lm.pre=lm(testpno2$Returns1~testpno2$sum2+testpno2$lagsum)
      preranking=coefficients(lm.pre)
      beta=preranking[2]+preranking[3] 
      crsp8$beta1[crsp8$permno==i&crsp8$t==k]=beta #数字
    }
  }
}  

  crsp9=crsp8%>%
    left_join(crsp5)%>%
    filter(is.na(beta1)==F)
  
  crsp10=crsp9%>%
    filter(exchange==1)%>%
    filter(t>=1963)%>%
    group_by(portfo)%>%
    mutate(portfo2=decile(beta1))%>%
    group_by(portfo,portfo2)%>%
    mutate(value2=max(beta1)) #在NYSE里求出beta的分组和临界点（最大值）
 
   breakpoint2=crsp10%>%select(portfo,portfo2,value2)%>%unique()%>%
    arrange(portfo2)# produce unique NYSE breakpoint value
  
  bkpt4=crsp9%>%left_join(crsp10)%>%filter(t>=1963) 
  bkpt5=bkpt4%>%select(permno,exchange,portfo,portfo2,beta1,Returns,sum2,rf1,ME)%>%unique()
  
  for(i in 1:length(bkpt5$t)){
    if(is.na(bkpt5$portfo2[i])==T){
      test=breakpoint2$value2[breakpoint2$portfo==bkpt5$portfo[i]]
      k=1
      while(k<10 & bkpt5$beta1[i]>test[k]){
        k=k+1
      }
      bkpt5$portfo2[i]=k
    }
  }
  crsp11=bkpt5
 # bkpt6=bkpt4%>%full_join(bkpt55)#%>%filter(is.na(Returns)==F)
  # crsp11=crsp11%>%
  #  left_join(bkpt5)%>%select(t,permno,portfo,sum2,Returns,portfo2)%>%
  #  filter(is.na(portfo2)==F)


  #Panel A
  panelA=matrix(nrow = 11,ncol = 11)
  panelA[1,1]=100*mean(crsp11$Returns)
  for(i in 2:11){
    panelA[i,1]=100*mean(crsp11$Returns[crsp11$portfo==i-1])
    panelA[1,i]=100*mean(crsp11$Returns[crsp11$portfo2==i-1])
    for(j in 2:11){
      panelA[i,j]=100*mean(crsp11$Returns[crsp11$portfo==i-1&crsp11$portfo2==j-1])
    }
  }
  
  #Panel B
  panelB=matrix(nrow = 11,ncol = 11)
  crsp12=crsp11%>%group_by(t,month,portfo,portfo2)%>%mutate(portreturn=mean(Returns))
  for(i in 2:11){
    test=crsp12[crsp12$portfo==i-1,]%>%
      group_by(t,month)%>%
      mutate(portreturn2=mean(portreturn))%>%
      arrange(desc(t),desc(month))%>%
      mutate(portreturn21=portreturn2[]-rf1[])%>%
      select(portreturn21,sum2)%>%unique()
    {
      l=coefficients(lm(test$portreturn21~test$sum2+lag(test$sum2)))
      beta=l[2]+l[3]
      panelB[i,1]=beta
    }
    test=crsp12[crsp12$portfo2==i-1,]%>%
      group_by(t,month)%>%
      mutate(portreturn2=mean(portreturn))%>%
      arrange(desc(t),desc(month))%>%
      mutate(portreturn21=portreturn2[]-rf1[])%>%
      select(portreturn21,sum2)%>%unique()
    {
      l=coefficients(lm(test$portreturn21~test$sum2+lag(test$sum2)))
      beta=l[2]+l[3]
      panelB[1,i]=beta
    }
    for(j in 2:11){
      test=crsp12[crsp12$portfo==i-1&crsp12$portfo2==j-1,]%>%
      group_by(t,month,portfo,portfo2)%>%
      mutate(portreturn=mean(Returns))%>%
      arrange(desc(t),desc(month))%>%
      mutate(portreturn21=portreturn[]-rf1[])%>%
      select(portreturn21,sum2)%>%unique()
        {
          l=coefficients(lm(test$portreturn21~test$sum2+lag(test$sum2)))
          beta=l[2]+l[3]
          panelB[i,j]=beta
        }
    }
  }
  
  #Panel C
  crsp13=crsp11%>% mutate(lnME=log(ME))
  panelC=matrix(nrow = 11,ncol = 11)
  panelC[1,1]=mean(crsp13$lnME)
  for(i in 2:11){
    panelC[i,1]=mean(crsp13$lnME[crsp13$portfo==i-1])
    panelC[1,i]=mean(crsp13$lnME[crsp13$portfo2==i-1])
    for(j in 2:11){
      panelC[i,j]=mean(crsp13$lnME[crsp13$portfo==i-1&crsp13$portfo2==j-1])
    }
  }
  
  ##extract specific column or row,which should be supplemented with var[xxxx,]
  
#----------------------------------------
# (**) CRSP and Compustat Data Merge
#----------------------------------------
                 
                                            
setwd("/run/media/john/1TB/Projects/Fama-French Replicatoin/")

compustat <- read.csv("Compustat.csv", header = TRUE, stringsAsFactors = FALSE)
crsp <- read.csv("Crsp.csv", header = TRUE, stringsAsFactors = FALSE)

# Remove last cusip digit from compustat to merge with crsp

compustat$cusip <- substr(compustat$cusip, 1, nchar(compustat$cusip) - 1)

# Merge by cusip and date, NA -> 0, subset for years and save

data <- full_join(compustat, crsp, by=c("cusip" = "CUSIP", "datadate" = "date"))
data[is.na(data)] <- 0
data <- filter(data, datadate >= 19580731 & datadate <= 19890631)
write.csv(data, "compustat_crsp_merged_1958-1989.csv")

# Read in data from csv

data <- read.csv("compustat_crsp_merged_1958-1989.csv")

#----------------------------------------
# (**) Data Wrangling
#----------------------------------------
# Fix missing fyears
data$fyear <- substr(data$datadate, 1, 4)

# Fix data elements

data$cusip <- as.numeric(data$cusip)
data$fyear <- as.numeric(data$fyear)

# Only keep those stocks with returns at the end of June
data %>%
  group_by(cusip, fyear) %>%
  mutate(month = substr(datadate, 5, 6),
         has_June = any(month == "06")) -> data

data <- filter(data, has_June == TRUE)

# Only keep those stocks with returns at the end of December
data %>%
  group_by(cusip, fyear) %>%
  mutate(month = substr(datadate, 5, 6),
         has_Dec = any(month == "12")) -> data

data <- filter(data, has_Dec == TRUE)

# Calculate Book Equity (BE) : BE = CEQ + TXDB
data$be <- data$ceq + data$txdb

# Calculate Market Equity (ME) : ME = prcc_f*csho
data$me <- data$prcc_f*data$csho
data <- filter(data, me > 0)   # Ensure has me

# Calculate Book-to-Market (BE/ME) : be / me
data$beme <- data$be/data$me

# Calculate EP : EP = IB + TXDFED + TXDFO + TXDS -  DVP/PRCC_F
data$ep <- data$ib + data$txdfed + data$txdfo + data$txds - (data$dvp/data$prcc_f)


## Monthly returns for at least 24 of 60 months preceding July of year t

# Need to ungroup data to run this
data <- ungroup(data)

data <- mutate(data, month = as.numeric(substr(datadate, 5, 6))) %>%
  mutate(datadate = as.POSIXct(gsub("^(\\d{4})(\\d{2}).*$", "\\1-\\2-01", datadate),
                    format("%Y-%m-%d"), tz = "GMT")) %>%  
  arrange(cusip, datadate) %>%                        
  filter(between(datadate, 
         datadate[tail(which(month == 6, arr.ind = TRUE), n = 1)] - (60*60*24*30*60),
         datadate[tail(which(month == 6, arr.ind = TRUE), n = 1)] -(60*60*24*30*24))) %>%
  group_by(cusip) %>%
  mutate(check = abs(lead(month)-month) == 11|abs(lead(month)-month) == 1|abs(lead(month)-month) == 0) %>%
  filter(all(check == TRUE | check %in% NA)) 

# Write out sample dataset after all checks and ready for regressions (obs = 11,721)
write.csv(data, "92_data.csv")




##########################

```
