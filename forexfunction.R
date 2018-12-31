#===================================================================================
#"dplyr"ˊ只能在分析使用，不然目前會干擾程式碼，解決方式是將load package順序排到第一順位，讓其他package複寫重複function
instalmypackage<-function()
   {
       Pkg <- c("dplyr","base","downloader","forecast","httr","jsonlite","moments",
       "PerformanceAnalytics","quantmod","RCurl","stats","scales","tseries",
       "TTR","TSA","xts","zoo","TFX","openxlsx","PerformanceAnalytics","lubridate","stringr","parallel")
       inst <- Pkg %in% installed.packages()
       if(length(Pkg[!inst]) > 0) install.packages(Pkg[!inst])
       instpackages <- lapply(Pkg, library, character.only=TRUE)
    
   }
#===================================================================================
#擷取貨幣的基本資料，還有要修正的地方
InstrumentsList <- function(AccountType,Token,AccountID)
{
    if(AccountType == "practice")
    {
      httpaccount <- "https://api-fxpractice.oanda.com"
    } else 
      if(AccountType == "live"){
        httpaccount <- "https://api-fxtrade.oanda.com"
      } else print("Account type error. Must be practice or live")
    
    auth       <- c(Authorization = paste("Bearer",Token,sep=" "))
    Queryhttp  <- paste(httpaccount,"/v1/instruments?accountId=",sep="")
    #fields<-paste("instrument","displayName","pip","maxTradeUnits","marginRate","halted","interestRate",sep="%2C")
    fields<-paste("instrument","displayName","pip","maxTradeUnits","marginRate",sep="%2C")
    QueryInst  <- paste(Queryhttp,AccountID,"&fields=",fields,sep="")

    QueryInst1 <- getURL(QueryInst,cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"),httpheader=auth)
    InstJson   <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
    FinalData  <- data.frame(InstJson)
    #colnames(FinalData) <- c("Instrument","DisplayName","PipSize","MaxTradeUnits")
    colnames(FinalData) <- c("instrument","displayName","pip","maxTradeUnits","marginRate")
    #interestRate
    #FinalData$MaxTradeUnits <- as.numeric(FinalData$MaxTradeUnits)

    return(FinalData)
}
  
#===================================================================================
#還需要再修改17-06-14-17:09，這是用來讀取檔案資料位置資料，並提供檔案路徑進行檔案操作
filefactor<-function(File_type,commonpath,Instrument,Granularity,Saved_data)
   {   
       Saved_data<-Saved_data
       #判斷目標資料夾的資料型態，不同型態的資料有不同的儲存位置
       if(File_type=="Index")
          {             
             filepath1<-paste(commonpath,"forexdata","Index",Instrument,Granularity,sep="/")
          }else
             {
                 filepath1<-paste(commonpath,"forexdata",Instrument,Granularity,sep="/")
             }

       #計算目標資料夾內存在的檔案，並記錄其名稱
       FileList<-grep(".csv",list.files(filepath1),value=T)
       #計算目標資料夾內存在的檔案數量
       filecount<-length(FileList)
       if(filecount==0)
          {                        
              #如果沒有，就只列出目標資料夾路徑
              FileListWithPath<-filepath1
          }else
             {
                 #如果目標資料夾內有檔案，列出個別檔案完整路徑
                 FileListWithPath<-paste(filepath1,FileList,sep="/")
             }

       #檢查是否需要輸出檔案名稱
       if(as.character(Saved_data)!="" | File_type=="Normal Save")
          {
              #如果有input第一筆資料的時間，會以此計算檔案名稱
              filemonth<-str_pad(month(as.Date(as.character(Saved_data))),2,"left",'0')
              filename<-paste(Instrument,Granularity,year(Saved_data),filemonth,sep="-")
              filepath<-paste(filepath1,"/",filename,".csv",sep="")
          }else
             {
                 filepath<-filepath1
             }
       #下面兩個 if function 還需要再修改17-06-14-17:09
       #temp save 目前幾乎沒有再使用，但時短時間內的紀錄可能還會用這種方式，等到要使用的時候再修改即可
       if(File_type=="Temp save")
          {
             temppath<-gsub("/文件/投資/Forex/Forex program",replacement="",commonpath)
             filepath1<-paste(temppath,"Tempfxdata",Granularity,Instrument,sep="/")
             filename<-paste(Instrument,Granularity,"Temp",sep="-")
             filepath<-paste(filepath1,"/",filename,".csv",sep="")
          }
       #以下的狀況較常用於更新檔案，但目前柿子看起來有點點怪
       if(File_type!="Index" & File_type!="Normal Save" & File_type!="Temp save")
          {
               filepath<-paste(filepath1,"/",last(grep(".csv",list.files(filepath1),value=T)),sep="")
          }
       Outputfactor<-list(FilePath=filepath,FileCount=filecount,FileList=FileList,FileListWithPath=FileListWithPath)
       return(Outputfactor)
   }
#===================================================================================
#用來計算indexfacter，用來normalized
USDollarfactorcal<-function(commonpath,Instrulist,USDollarfacter)
   {
       USDollarfactor2<-c(1:8)
       Granularity<-"D"
       Instrument<- Instrulist[1,"instrument"]
       File_type<-"Save"
       Saved_data<-""
       filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
       filecount<-1
       fxdata<-vector("list", 7) 
       fxindex<-vector("list", 8) 
        
       for(x in 1:7)
          {   
              Instrument<- Instrulist[x,"instrument"]
              filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)

              fxdata[[x]]<-select(read.csv(filefactorresult$FileListWithPath[filecount]),TimeStamp:Complete)
          }
       fxindex[[1]]<-fxdata[[1]]
       tempfxindex<-fxdata[[1]][,2:6]^USDollarfacter[1]
       for(x in 2:7)
          {
              tempfxindex[,1:4]<-fxdata[[x]][,2:5]^(USDollarfacter[x]*USDollarfacterway[x])*tempfxindex[,1:4]
              tempfxindex[,5]<-fxdata[[x]][,6]+tempfxindex[,5]

          }

       
       USDollarfactor2[1]<-100/tempfxindex[1,1]
       tempfxindex[,1:5]<-tempfxindex[,1:5]*USDollarfactor2[1]
       #重新校正 index參數，改成用2005-01-03開盤價為準 
       fxindex2<-vector("list", 8) 
       fxindex2[[1]]<-tempfxindex

       for(x in 2:8)
          {
              fxindex2[[x]]<-fxindex2[[1]]
              fxindex2[[x]][,1:5]<-fxdata[[x-1]][,2:6]^(-(USDollarfacterway[x-1]))*fxindex2[[1]][,1:5]
              USDollarfactor2[x]<-100/fxindex2[[x]][1,2]
          }

        return(USDollarfactor2) 

   }
   #===================================================================================

#將分鐘資料進行轉換成日線、分線等
outputtimeperioddata<-function(datatimeadjust,outputperiod)
   {
       #設定目標時區與參數
       localtimezone<-"Asia/Taipei"
       originaltimezone<-"America/New_York"
       alignfactor<-c(1800,3600,0)
       #按照時間先後順序重新排序
       datatimeadjust<-datatimeadjust[order(datatimeadjust$TimeStamp),]
       #擷取colnames
       colplace<-TSpCpfinder(datatimeadjust)
       datatimeadjustcolname<-colnames(datatimeadjust)[colplace[1]:colplace[2]]
       #設定資料的時區為GMT+8
       datatimeadjust[,"TimeStamp"]<-as.POSIXct(datatimeadjust[,"TimeStamp"], tz=localtimezone)
       #將資料時間調整為GMT-5,並將收盤時間17:00調整成凌晨
       datatimeadjust[,"TimeStamp"]<-as.POSIXct(format(datatimeadjust[,"TimeStamp"], tz=originaltimezone,usetz=TRUE))+hours(7) 
       #去除屬於週六、週日的Data
       datatimeadjust<-subset(datatimeadjust ,weekdays(as.Date(as.character(datatimeadjust[,"TimeStamp"])))!="週六" )
       datatimeadjust<-subset(datatimeadjust ,weekdays(as.Date(as.character(datatimeadjust[,"TimeStamp"])))!="週日")
       datatimeadjust<-subset(datatimeadjust ,grepl("-01-01",datatimeadjust[,"TimeStamp"])!=as.logical(TRUE))
       #將TimeStamp擷取出來，要align的話必須先將格式變成xts，要馬事先function化，不然就要一個一個做>>function 化
       times.init <-as.POSIXct(strptime(datatimeadjust[,"TimeStamp"], '%Y-%m-%d %H:%M:%S'), tz=originaltimezone,usetz=TRUE)
       datatimeadjust <-xts(select(datatimeadjust,Open:Volume),order.by=times.init)
       #按不同線圖執行擷取，分別為1分線、30分線、日線
                  
       if(outputperiod=="M30")
           {
               outputperiodfactor<-1
               datatimeadjust<-align.time(to.minutes30(datatimeadjust, indexAt='startof'),alignfactor[outputperiodfactor])
               index(datatimeadjust) <- index(datatimeadjust) -alignfactor[outputperiodfactor]
                            }
       if(outputperiod=="H1")
           {
               outputperiodfactor<-2
               datatimeadjust<-align.time(to.hourly(datatimeadjust, indexAt='startof'),alignfactor[outputperiodfactor])
               index(datatimeadjust) <- index(datatimeadjust) -alignfactor[outputperiodfactor]
           }
       if(outputperiod=="D")
           {
               outputperiodfactor<-3
               datatimeadjust<-align.time(to.daily(datatimeadjust, indexAt='startof'))
               datatimeadjust<-datatimeadjust[2:nrow(datatimeadjust),]
           }
       #periodsavelocation<-gsub("M1",replacement=outputperiod[outputperiodfactor] ,filefactorresult$FileListWithPath[filecount])
       datatimeadjust <-as.data.frame(datatimeadjust)
       timestamp<-t(t(rownames(datatimeadjust)))
       #rownames(outputdata1)<-seq(1:nrow(outputdata1))
       status_complete<-sprintf("TRUE",seq(1:nrow(datatimeadjust)))
       status_complete<-t(t(status_complete))
       datatimeadjust<-cbind(timestamp,datatimeadjust,status_complete)
       colnames(datatimeadjust)<-datatimeadjustcolname
       datatimeadjust$TimeStamp<-datatimeadjust$TimeStamp
       #將加上的7小時減回去，並依設定成GMT-5
       datatimeadjust$TimeStamp<-as.POSIXct(datatimeadjust$TimeStamp,tz=originaltimezone)-hours(7)
       #時區轉換成GMT+8
       datatimeadjust$TimeStamp<-as.POSIXct(format(datatimeadjust$TimeStamp, tz=localtimezone))
       rownames(datatimeadjust)<-seq(1:nrow(datatimeadjust))
       datatimeadjust$Complete<-as.character(datatimeadjust$Complete)
       if(as.Date(as.character(datatimeadjust[nrow(datatimeadjust),"TimeStamp"]))==Sys.Date())
           {
               datatimeadjust[nrow(datatimeadjust),"Complete"]<-as.character("FALSE")
           }
       return(datatimeadjust)
       rm(datatimeadjust,status_complete,timestamp)
   }                     
#===================================================================================


#HisPrices修正 以時間區間項oanda呼叫報價，最多5000筆 資料
HisPricestime<-function(AccountType,Granularity,DayAlign,TimeAlign,Token,Instrument,Start,End)
                        {
  if(AccountType == "practice")
  {
    httpaccount  <- "https://api-fxpractice.oanda.com"
  }else{
       if(AccountType == "live")
       {
        httpaccount  <- "https://api-fxtrade.oanda.com"
       }else{
              print("Account type error. Must be practice or live")
            }
      }
  #qcount  <- paste("count=",Count,sep="")

  qstart <- paste("start=",Start,sep="")
  qend   <- paste("end=",End,sep="")

  qcandleFormat  <- "candleFormat=midpoint"
  qgranularity   <- paste("granularity=",Granularity,sep="")
  qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep="")
  qalignmentTimezone <- paste("alignmentTimezone=",TimeAlign,sep="")

  auth           <- c(Authorization = paste("Bearer",Token,sep=" "))
  QueryHistPrec  <- paste(httpaccount,"/v1/candles?instrument=",sep="")
  QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep="")
  QueryHistPrec2 <- paste(QueryHistPrec1,qstart,qend,qcandleFormat,qgranularity, qdailyalignment,qalignmentTimezone,sep="&")
  InstHistP <- getURL(QueryHistPrec2,cainfo=system.file("CurlSSL","cacert.pem",
               package="RCurl"),httpheader=auth)
  InstHistPjson <- fromJSON(InstHistP, simplifyDataFrame = TRUE)
  Prices        <- data.frame(InstHistPjson[[3]])
  Prices$time <- paste(substr(Prices$time,1,10),substr(Prices$time,12,19), sep=" ")
  colnames(Prices) <- c("TimeStamp","Open","High","Low","Close","Volume","Complete")
  Prices$TimeStamp <- as.POSIXct(Prices$TimeStamp,tz="UTC")
  Prices$TimeStamp<-format(Prices$TimeStamp,tz="Asia/Taipei")
  return(Prices)
  rm(Prices)
}
#===================================================================================

##HisPrices修正 以抓取資料筆數向oanda呼叫報價，最多5000筆

HisPricescount<-function(AccountType,Granularity,DayAlign,TimeAlign,Token,Instrument,Count)
                        {
  if(AccountType == "practice")
  {
    httpaccount  <- "https://api-fxpractice.oanda.com"
  }else{
       if(AccountType == "live")
       {
        httpaccount  <- "https://api-fxtrade.oanda.com"
       }else{
              print("Account type error. Must be practice or live")
            }
      }
  qcount  <- paste("count=",Count,sep="")

  

  qcandleFormat  <- "candleFormat=midpoint"
  qgranularity   <- paste("granularity=",Granularity,sep="")
  qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep="")
  qalignmentTimezone <- paste("alignmentTimezone=",TimeAlign,sep="")

  auth           <- c(Authorization = paste("Bearer",Token,sep=" "))
  QueryHistPrec  <- paste(httpaccount,"/v1/candles?instrument=",sep="")
  QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep="")
  QueryHistPrec2 <- paste(QueryHistPrec1,qcount,qcandleFormat,qgranularity, qdailyalignment,qalignmentTimezone,sep="&")
  InstHistP <- getURL(QueryHistPrec2,cainfo=system.file("CurlSSL","cacert.pem",
               package="RCurl"),httpheader=auth)
  InstHistPjson <- fromJSON(InstHistP, simplifyDataFrame = TRUE)
  Prices        <- data.frame(InstHistPjson[[3]])
  Prices$time <- paste(substr(Prices$time,1,10),substr(Prices$time,12,19), sep=" ")
  colnames(Prices) <- c("TimeStamp","Open","High","Low","Close","Volume","Complete")
  Prices$TimeStamp <- as.POSIXct(Prices$TimeStamp,tz="UTC")
  Prices$TimeStamp<-format(Prices$TimeStamp,tz="Asia/Taipei")
  return(Prices)
  rm(Prices)
}
#===================================================================================
#擷取完資料後會依據不同狀況而使系統暫時休息，不需要一直擷取
systemstandby<-function(time,firsttime)
   {
       firsttime<-firsttime
       if(firsttime==FALSE)
          {
              if(time==minute(Sys.time()))
                 {
                     Sys.sleep(as.numeric(60-floor(second(Sys.time()))+1))
                 }
              if((weekdays(Sys.Date())=="週六" & hour(Sys.time())>=6) | weekdays(Sys.Date())=="週日")
                 {
                      timeleft<-24*60*60-floor((hour(Sys.time())*60+minute(Sys.time()))*60+second(Sys.time()))
                 }
              
              if(weekdays(Sys.Date())=="週一" & hour(Sys.time())<5)
                 {
                      timeleft<-5*60*60-floor((hour(Sys.time())*60+minute(Sys.time()))*60+second(Sys.time()))
                 }else
                    {
                        if(minute(Sys.time())< collectperiod)
                           {
                             timeleft<-(collectperiod-minute(Sys.time()))*60+second(Sys.time())
                           }else
                              {
                                  timeleft<-(collectperiod-(minute(Sys.time()) %% collectperiod))*60+second(Sys.time())
                              }  
                    }
              Sys.sleep(timeleft)

          }else
             {
                 time<-time
                 firsttime<-FALSE
                 #會多休息一次是因為避免頻繁擷取資料
                 Sys.sleep(0.1)
             }
          
       return(firsttime)
        
    }
#=================================================================================== 
#檢查日期是超過最後一天與跨週末，如果是會修正，目前用在休息的檢查上還有問題，需要修正
daycheck<-function(CheckDay)
    {
         weekday<-as.character(weekdays(as.Date(as.character(CheckDay))))
         Dayadjust<-CheckDay
         #如果開始日期為週六，要往前調start才會ok，如果是end，會少收一天，直接進入下個循環
         #以美國時間來看，開盤時間會是周日下午，結束時間會是週五下午五點
         if(weekday==as.character("周六"))
            {
                Dayadjust<-CheckDay+1
            }

         #如果天數超過最後一天，時間點會往後加上一天，確保會收到資料但不會溢位                  
         if(Dayadjust>=Sys.Date())
            {
               Dayadjust<-Sys.Date()+1
            }
         return(Dayadjust)
    }
#===================================================================================

TSpCpfinder<-function(table)
   {
       tablecolnames<-colnames(table)
       tableresult<-c(0,0)
       tableresult[1]<-which("TimeStamp"==tablecolnames)
       tableresult[2]<-which("Complete" ==tablecolnames)
       return(tableresult)
   }
#===================================================================================
###forbidden trade strategy###

#===================================================================================
#將輸入的資料按月分切割後輸出儲存 
 savedatabymonth<-function(Hpricetime1,File_type,commonpath,Instrument,Granularity,filecount)
    {
        Hpricetime1<-Hpricetime1
        firstmonthplace<-months(as.Date(first(Hpricetime1[,"TimeStamp"])))
        lastmonthplace <-months(as.Date( last(Hpricetime1[,"TimeStamp"])))
        #直接在這裡抓完所有資料，之後再進行計算並進行計算
        while(firstmonthplace!=lastmonthplace)
           {  
               Hpricetime3<-subset(Hpricetime1,months(as.Date(Hpricetime1[,"TimeStamp"]))==firstmonthplace)
               #輸出檔案路徑與路徑檔案數
               Saved_data<-Hpricetime3[1,"TimeStamp"]
               filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
               rownames(Hpricetime3)<-seq(1:nrow(Hpricetime3))
               write.csv(Hpricetime3,filefactorresult$FilePath)
               Hpricetime1<-subset(Hpricetime1,months(as.Date(Hpricetime1[,"TimeStamp"]))!=firstmonthplace)
               filecount<-filecount+1
               firstmonthplace<-months(as.Date(first(Hpricetime1[,"TimeStamp"])))
               lastmonthplace <-months(as.Date( last(Hpricetime1[,"TimeStamp"])))
               rm(Hpricetime3)
           }
        return(Hpricetime1)
        rownames(Hpricetime1)<-seq(1:nrow(Hpricetime1))
                  Saved_data<-Hpricetime1[1,"TimeStamp"]
                  filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
                  filecount<-filefactorresult$FileCount
                  write.csv(Hpricetime1,filefactorresult$FilePath)       
    }
      

#===================================================================================
#以表格形式呈現目前的資料擷取狀況
Statusplot<-function(x,Instrument,Granularity,filecount,Start,End,Status)
   {
        table<-c(as.character(Sys.time()),x,as.character(Instrument),Granularity,filecount,as.character(Start),as.character(End),Status)
        table<-t(t(table))
        rownames(table)<-c("Time","No.","Currency","Timerange","Filecount","Start","End","Status")
        textplot(table, cex=3,show.colnames=FALSE)
   } 
#===================================================================================
#按分鐘擷取資料
getdata<-function(commonpath,statusmonitor)
   {
       #以下為呼叫帳號資料
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

       repeat
          {

              #時間執行序，當時間間隔等於貨幣報價週期時，執行。
              
              time<-as.numeric(minute(Sys.time()))
        
              if(time%% collectperiod==0 | firsttime==TRUE)
                 {
           
                    M30Hpricetime<-vector("list",runinstrument)
                     H1Hpricetime<-vector("list",runinstrument)
                      DHpricetime<-vector("list",runinstrument)
                    #Hisminfxdatatemp<-vector("list",runinstrument)
                    fxdata<-vector("list",runinstrument)
                    fxdatacheck<-vector("list",runinstrument)
                    fxindex<-vector("list",runinstrument)
                    #a<-Sys.time()
                    for(x in 1:(runinstrument))
                       {
                            Instrument<-Instrulist[x,"instrument"]
                            Granularity <- "M1"
                            File_type<-"New start"
                            #輸出檔案路徑與路徑檔案數
                            Saved_data<-""
                            filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
                            filecount<-filefactorresult$FileCount
                            #判斷起始時間值
                            if(filecount==0)
                                {
                                     Start<- as.Date("2005-01-02")
                                     if(Granularity=="D")
                                        {
                                            Start<-as.Date("2002-05-06")
                                        }
                                     if(Instrument=="USD_CNH")
                                        {
                                            Start<-as.Date("2011-02-13")
                                        }   
                                }else
                                   {  
                                       
                                       Hpricetime1<-select(read.csv(filefactorresult$FilePath),TimeStamp:Complete)
                                       #iffirsttime
                                       Start <- as.Date(as.character(Hpricetime1[nrow(Hpricetime1),"TimeStamp"]))-1
                                       Count<-ceiling(as.numeric(difftime(Sys.time(), as.POSIXct(Hpricetime1[nrow(Hpricetime1),"TimeStamp"]),units = "mins")))+4
                                   }
                            End<-Sys.Date()-10
                            while(End<=Sys.Date())
                               {                           
                                   File_type<-"Normal Save"
                                   Start <- daycheck(Start)
                                   if(as.Date(Start)==Sys.Date() & as.character(weekdays(Sys.Date()))==as.character("週日"))
                                      {
                                          Start<-Sys.Date()-2
                                      }
                                   End   <- Start+3
                                   End   <- daycheck(End)
                                   #判斷抓檔的類型，資料結構可以用於後面的內容
                                   if(filecount==0)
                                      {
                                          Hpricetime1<-HisPricestime(AccountType,Granularity,DayAlign,TimeAlign,Token,Instrument,Start,End)
                                          filecount<-1
                                      }else
                                         {
                                             if(Count<5000)
                                                {
                                                    Hpricetime2<-HisPricescount(AccountType,Granularity,DayAlign,TimeAlign,Token,Instrument,Count)
                                                    End<-Sys.Date()+1
                                                }else
                                                   {
                                                        Hpricetime2<-HisPricestime(AccountType,Granularity,DayAlign,TimeAlign,Token,Instrument,Start,End)
                                                   }
                                             
                                             if(as.POSIXct(Hpricetime1[nrow(Hpricetime1),"TimeStamp"])>as.POSIXct(Hpricetime2[1,"TimeStamp"]))
                                                {
                                                    if(nrow(Hpricetime1)!=1)
                                                       {
                                                           Hpricetime1<-rbind(Hpricetime1[1:(nrow(Hpricetime1)-2),],subset(Hpricetime2,as.POSIXct(Hpricetime1[nrow(Hpricetime1)-1,"TimeStamp"])<=as.POSIXct(Hpricetime2[,"TimeStamp"])))

                                                       }else
                                                          {
                                                              Hpricetime1<-subset(Hpricetime2,as.POSIXct(Hpricetime1[nrow(Hpricetime1),"TimeStamp"])<=as.POSIXct(Hpricetime2[,"TimeStamp"]))
                                                          }
                                                }else
                                                   {
                                                       Hpricetime1<-rbind(Hpricetime1,Hpricetime2)
                                                   }
                                             rm(Hpricetime2)                          
                                         }
                                   #以樂為單位切割檔案並將之輸出\
                                   firstmonthplace<-months(as.Date(as.character(first(Hpricetime1[,"TimeStamp"]))))
                                   lastmonthplace <-months(as.Date(as.character( last(Hpricetime1[,"TimeStamp"]))))
                                   #直接在這裡抓完所有資料，之後再進行計算並進行計算
                                   while(firstmonthplace!=lastmonthplace)
                                      {
                                           Hpricetime3<-subset(Hpricetime1,months(as.Date(as.character(Hpricetime1[,"TimeStamp"])))==firstmonthplace)
                                           #輸出檔案路徑與路徑檔案數
                                           Saved_data<-Hpricetime3[1,"TimeStamp"]
                                           File_type<-"Normal Save"
                                           filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
                                           rownames(Hpricetime3)<-seq(1:nrow(Hpricetime3))
                                           write.csv(Hpricetime3,filefactorresult$FilePath)
                                           Hpricetime1<-subset(Hpricetime1,months(as.Date(as.character(Hpricetime1[,"TimeStamp"])))!=firstmonthplace)
                                           firstmonthplace<-months(as.Date(as.character(first(Hpricetime1[,"TimeStamp"]))))
                                           lastmonthplace <-months(as.Date(as.character( last(Hpricetime1[,"TimeStamp"]))))
                                           rm(Hpricetime3)
                                           Status<-"Processing"
                                           Statusplot(x,Instrument,Granularity,filecount,Start,End,Status)
                                      }
                                   Start<-End
                                   #Sys.sleep(0.1)
                                   if(weekdays(Sys.Date())=="週日"& Start==(Sys.Date()-1))
                                      {
                                          End<-Sys.Date()+2
                                      }
                               }
                            #輸出檔案路徑與路徑檔案數
                            rownames(Hpricetime1)<-seq(1:nrow(Hpricetime1))
                            Saved_data<-Hpricetime1[1,"TimeStamp"]
                            filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
                            write.csv(Hpricetime1,filefactorresult$FilePath)
                            #檢測檔案擷取狀況
                            if(statusmonitor==TRUE)
                              {
                                  graphics.off()
                                  Status<-"Finish"
                                  Statusplot(x,Instrument,Granularity,filecount,Start,End,Status)
                              }
                            
                            rm(Hpricetime1,filefactorresult,Start,End)
                       }
                    #Instrument<- t(t(as.character(Instrulist[,"instrument"])))
                    #apply(Instrument,1,getmindata,System,AccountType,DayAlign,TimeAlign,Token)
                    graphics.off()
                    for(x in 1:(runinstrument))
                       {
                           #輸入變數
                           Instrument<- Instrulist[x,"instrument"]
                           #輸出檔案路徑與路徑檔案數
                           File_type<-"New start"

                           Granularity <- "M30"
                           Saved_data<-""
                           outpathresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
                           outputfilecount<-outpathresult$FileCount
                           Granularity <- "M1"
                           filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
                           filecount<-filefactorresult$FileCount  
                           #讀入資料，並跟上個檔案合併(如果有的話) 
                           Hisminfxdataold<-select(read.csv(outpathresult$FileListWithPath[outputfilecount]),TimeStamp:Complete)

                           Hisminfxdataold[,"TimeStamp"]<-as.POSIXct(Hisminfxdataold[,"TimeStamp"],tz=localtimezone)

                           Hisminfxdata<-Hisminfxdataold[1:(nrow(Hisminfxdataold)-1),]
                           for(filecount in (outputfilecount):filefactorresult$FileCount)
                           #for(filecount in (filefactorresult$FileCount-1):filefactorresult$FileCount)
                              {
                                  Hisminfxdatainput<-select(read.csv(filefactorresult$FileListWithPath[filecount]),TimeStamp:Complete)
                                  Hisminfxdatainput<-subset(Hisminfxdatainput,as.POSIXct(Hisminfxdatainput[,"TimeStamp"],tz=localtimezone)>=(as.POSIXct(Hisminfxdataold[nrow(Hisminfxdataold),"TimeStamp"],tz=localtimezone)))
                                  Hisminfxdata<-rbind(Hisminfxdata,Hisminfxdatainput)
                              }                     
                   
                           outputperiod<-"M30"
                           fxdata[[x]]<-outputtimeperioddata(Hisminfxdata,outputperiod)
                           rm(filefactorresult,Hisminfxdataold,Hisminfxdatainput,Hisminfxdata)
                       }
                    timestamp<-as.matrix(as.character(fxdata[[1]][,"TimeStamp"]))
                    for(x in 2:(runinstrument))
                        {
                            timestamp2<-as.matrix(as.character(fxdata[[x]][,"TimeStamp"]))
                            timestamp<-timestamp
                            timestamp<-rbind(timestamp,timestamp2)
                        }                         
                    timestamp<-unique(timestamp)
                    timestamp<-as.data.frame(timestamp)
                    colnames(timestamp)<-"TimeStamp"
                    #填入缺值，遺漏職之前的close變成open
                    for(x in 1:(runinstrument))
                       {
                           fxdata[[x]][,"TimeStamp"]<-as.character(fxdata[[x]][,"TimeStamp"])
                           fxdatacheck[[x]]<-merge(timestamp, fxdata[[x]], by = "TimeStamp", all = T)
                           narow<-which(is.na(fxdatacheck[[x]]$High))
                           while(length(narow)!=0)
                              {
                                  trow1<-which(is.na(fxdatacheck[[x]]$High)==FALSE)
                                  diffnarow<-narow[1]-trow1
          
                                  if(narow[1]==1)
                                     {
                                         narowforback<-max(subset(diffnarow,diffnarow<0))
                                         fxdatacheck[[x]][narow[1],2:5]<-fxdatacheck[[x]][(narow[1]-narowforback),"Open"]
                                     }else
                                         {
                                             if(narow[1]==nrow(fxdatacheck[[x]]))
                                                {
                                                    narowforfront<-min(subset(diffnarow,diffnarow>0))
                                                    fxdatacheck[[x]][narow[1],2:5]<-fxdatacheck[[x]][(narow[1]-narowforfront),"Close"]

                                                }else
                                                   {
                                                       narowforfront<-min(subset(diffnarow,diffnarow>0))
                                                       narowforback<-max(subset(diffnarow,diffnarow<0))
                                                       fornaopen<-fxdatacheck[[x]][(narow[1]-narowforfront),"Close"]
                                                       if(abs(narowforback)>1)
                                                         {
                                                            fornaclose<-fornaopen
                                                         }else
                                                             {
                                                                 fornaclose<-fxdatacheck[[x]][(narow[1]-narowforback),"Open"]
                                                             }
                                                       fxdatacheck[[x]][narow[1],"Open"]<-fornaopen
                                                       fxdatacheck[[x]][narow[1],"High"]<-max(fornaopen,fornaclose)
                                                       fxdatacheck[[x]][narow[1],"Low"]<-min(fornaopen,fornaclose)
                                                       fxdatacheck[[x]][narow[1],"Close"]<-fornaclose
                                                   }
                                         }
                                  fxdatacheck[[x]][narow[1],"Volume"]<-0
                                  narow<-which(is.na(fxdatacheck[[x]]$High))
                              }
                           Instrument<- Instrulist[x,"instrument"]
                           File_type<-"Normal Save"
                           
                           #轉換資料為H1,D1
                           M30Hpricetime[[x]]<-fxdatacheck[[x]]
                           Granularity <- "M30"
                           data<-M30Hpricetime[[x]]
                           savedata(File_type,data,commonpath,Instrument,Granularity)
                           Hisminfxdata<-M30Hpricetime[[x]]
                           outputperiod<-"H1"
                           H1Hpricetime[[x]]<-outputtimeperioddata(Hisminfxdata,outputperiod)
                           Granularity <- "H1"
                           data<-H1Hpricetime[[x]]
                           #"H1,D"目前會出錯
                           savedata(File_type,data,commonpath,Instrument,Granularity)
                           Hisminfxdata<-H1Hpricetime[[x]]
                           outputperiod<-"D"
                           DHpricetime[[x]]<-outputtimeperioddata(Hisminfxdata,outputperiod)
                           Granularity <- "D"
                           data<-DHpricetime[[x]]
                           savedata(File_type,data,commonpath,Instrument,Granularity)
                       }
                       
                    rm(fxdata,fxdatacheck,Hisminfxdata)
                    for(y in 1:3)
                       {
                           Granularitylist<-c("M30","H1","D")
                           Granularity<-Granularitylist[y]
                           #這邊要加入比對
                           File_type<-"Index"
                           Saved_data<-""
                           Instrument<-Indexlist[1]
                           outpathresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
                           outputfilecount<-outpathresult$FileCount

                           File_type<-"New Start"
                           Instrument<- Instrulist[1,"instrument"]
                           filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
                           inputfilecount<-filefactorresult$FileCount
                           if(outputfilecount==0)
                              {
                                  outputfilecount<-1
                              }
                           for(filecount in outputfilecount: inputfilecount)
                              {
                                  fxindex<-vector("list", 8) 
                                  fxdata <-vector("list", 7)
                                  for(x in 1:7)
                                     {   
                                         File_type<-"New Start"
                                         Instrument<- Instrulist[x,"instrument"]
                                         filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
                                         fxdata[[x]]<-select(read.csv(filefactorresult$FileListWithPath[filecount]),TimeStamp:Complete)
                                         naplace<-which(is.na(fxdata[[x]][,7]))
                                         if(length(naplace)!=0)
                                            {
                                               for(n in length(naplace))
                                                  {
                                                      fxdata[[1]][naplace[n],7]<-fxdata[[x]][naplace[n],7]
                                                  }
                                            }   
                                     } 
                                  tempfxindex<-fxindex[[1]]<-fxdata[[1]]
                                  tempfxindex[,2:6]<-fxdata[[1]][,2:6]^USDollarfacter[1]
                                  for(x in 2:7)
                                     {
                                         tempfxindex[,2:5]<-fxdata[[x]][,2:5]^(USDollarfacter[x]*USDollarfacterway[x])*tempfxindex[,2:5]
                                         tempfxindex[,6]<-fxdata[[x]][,6]+tempfxindex[,6]                                
                                     }


                                  if(filecount==1)
                                     {
                                          USDollarfactor2<-c(1:8)
                                          fxindex2<-vector("list", 8)
                                          USDollarfactor2[1]<-100/tempfxindex[1,2]
                                          tempfxindex[,2:6]<-tempfxindex[,2:6]*USDollarfactor2[1]
                                          #重新校正 index參數，改成用2005-01-03開盤價為準 
                                          fxindex2<-fxindex
                                          fxindex2[[1]]<-tempfxindex

                                          for(x in 2:8)
                                             {
                                                 fxindex2[[x]]<-fxindex2[[1]]
                                                 fxindex2[[x]][,2:6]<-fxdata[[x-1]][,2:6]^(-(USDollarfacterway[x-1]))*fxindex2[[1]][,2:6]
                                                 USDollarfactor2[x]<-100/fxindex2[[x]][1,2]
                                             }
                                                  
                                     }else
                                        {
                                            tempfxindex[,2:5]<-tempfxindex[,2:5]*USDollarfactor2[1]
                                        }
                                  #重新校正 index參數，改成用2005-01-03開盤價為準，這邊好像有問題

                                  fxindex[[1]]<-tempfxindex
                                  for(x in 1:8)
                                     {
                                          if(x>=2)
                                             {
                                                  fxindex[[x]]<-fxindex[[1]]
                                                  fxindex[[x]][,2:6]<-round(fxdata[[x-1]][,2:6]^(-(USDollarfacterway[x-1]))*fxindex[[1]][,2:6]*USDollarfactor2[x],4)
                                                  
                                             }
                                          fxindex[[x]][,2:5]<-round(fxindex[[x]][,2:5],4)
                                          fxindex[[x]][,"Volume"]<-t(t(round(as.numeric(fxindex[[x]][,"Volume"]),0)))
                                     }
                                  
                                  File_type<-"Index"
                                  for(x in 1:8)
                                     {
                                         saveddata<-fxindex[[x]]
                                         rownames(saveddata)<-seq(1:nrow(saveddata))
                                         Saved_data<-as.character(saveddata[nrow(saveddata),"TimeStamp"])
                                         Instrument<-Indexlist[x]
                                         Indexfileresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
                                         write.csv(saveddata,Indexfileresult$FilePath)
                                     }
                              }  
                       }                      
                    rm(fxindex,saveddata,tempfxindex,M30Hpricetime,H1Hpricetime,DHpricetime)   
                 }
              #資料抓完，進行其他週期計算此時是在for迴圈內個別計算

              firsttime<-systemstandby(time,firsttime)
              rm(time)
         }                                         
   }




savedata<-function(File_type,data,commonpath,Instrument,Granularity)
   {
       
       Hpricetime1<-data
       firstmonthplace<-months(as.Date(as.character(first(Hpricetime1[,"TimeStamp"]))))
       lastmonthplace <-months(as.Date(as.character( last(Hpricetime1[,"TimeStamp"]))))
       #直接在這裡抓完所有資料，之後再進行計算並進行計算
       while(firstmonthplace!=lastmonthplace)
          {
               Hpricetime3<-subset(Hpricetime1,months(as.Date(as.character(Hpricetime1[,"TimeStamp"])))==firstmonthplace)
               #輸出檔案路徑與路徑檔案數
               Saved_data<-Hpricetime3[1,"TimeStamp"]
               filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
               rownames(Hpricetime3)<-seq(1:nrow(Hpricetime3))
               write.csv(Hpricetime3,filefactorresult$FilePath)
               Hpricetime1<-subset(Hpricetime1,months(as.Date(as.character(Hpricetime1[,"TimeStamp"])))!=firstmonthplace)
               firstmonthplace<-months(as.Date(as.character(first(Hpricetime1[,"TimeStamp"]))))
               lastmonthplace <-months(as.Date(as.character( last(Hpricetime1[,"TimeStamp"]))))
               rm(Hpricetime3)
          }
        rownames(Hpricetime1)<-seq(1:nrow(Hpricetime1))
        Saved_data<-Hpricetime1[1,"TimeStamp"]
        filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
        write.csv(Hpricetime1,filefactorresult$FilePath)
        rm(Hpricetime1)
   }



#===============

###forbidden trade strategy###



#計算統計到每個點的log值
profitvalue<-function(input)
   {
       input<-as.vector(input)
       output<-input
       #input<-log(input,base=10)
       for(x in 1:length(input))
          {
              #output[x]<-sum(input[1:x])
              output[x]<-sum(log(input[1:x],base=10))
          }

   }

