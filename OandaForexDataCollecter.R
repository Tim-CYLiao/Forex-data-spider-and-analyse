#以下為每次一開始一定要的執行項目
sysinformation<-Sys.getenv()
if(length(grep("Apple",names(Sys.getenv(names=TRUE))))==1)
   {
       commonpath<-("MacOS path")

   }else{
            commonpath<-("WindowsOS path")

        }
setwd(commonpath)
source("Oanda api original.R")
source("myandafunction.R",encoding="utf-8")

#downloader::source_url("http://bit.ly/GitHubROandaAPI",prompt=FALSE,quiet=TRUE)
#安裝所需的package,,"lubridate","reshape2"先暫時不安裝，可以執行# 
oldw <- getOption("warn")
options(warn = -1)

instalmypackage()

options(warn = oldw)

Account<-read.xlsx("Forexfactor.xlsx",sheet = 2, startRow = 1, colNames = TRUE,rowNames = TRUE)
       AccountType <- "practice"
       AccountID   <- as.character(Account["Test","AccountID"])
       Token       <- as.character(Account["Test","Token"])
       DayAlign<-17
       TimeAlign<-"UTC"
       #以下為呼叫外部參數
       Forexfactor<-read.xlsx("Forexfactor.xlsx",sheet = 1, startRow = 1, colNames = TRUE,rowNames = TRUE)
       Instrulist<-read.csv("Instrulist.csv")
       timerange<-c(5000,800,600,200,100,17,3)
       timerangename<-c("D","H4","H3","H1","M30","M5","M1")
       #設定M30,H1,D輸出參數
       File_type<-"New start"
       localtimezone<-"Asia/Taipei"
       originaltimezone<-"America/New_York"
       outputperiod<-c("M30","H1","D")
       alignfactor<-c(1800,3600,0)
       #設定執行週期
       firsttime<-TRUE
       collectperiod<-as.numeric(15)
       #設定存取list
       USDollarfacter<-as.numeric(Forexfactor["USDollarfacter",1:7])
       #EURUSD 1 EUR等於x元usd 若usd價值為y,EUR的價值將會是x*y(1),如果USDJPY  1USD=zJPY,1JPY=1/zUSD, USD 價值為y,JPY價值為y/z (-1)
       USDollarfacterway<-as.numeric(Forexfactor["USDollarfacterway",1:7])
       USDollarfactor2<-USDollarfactorcal(commonpath,Instrulist,USDollarfacter)
       Indexlist<-c("USD","EUR","JPY","GBP","CHF","AUD","NZD","CAD")
       runinstrument<-nrow(Instrulist)-0 

statusmonitor<-FALSE
getdata(commonpath,statusmonitor)
