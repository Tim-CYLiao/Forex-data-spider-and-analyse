System<-"Mac"
if(System=="Mac")
   {
       commonpath<-("Path for MacOS")

   }else{
            commonpath<-("Path for Windows")

        }
setwd(commonpath)
source("Oanda api original.R")
source("forexfunction.R",encoding="utf-8")

#downloader::source_url("http://bit.ly/GitHubROandaAPI",prompt=FALSE,quiet=TRUE)
#安裝所需的package,,"lubridate","reshape2"先暫時不安裝，可以執行# 
instalmypackage()


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
timerange<-c(num_1100,800,num_100,200,100,17,3)
timerangename<-c("D","H4","H3","H1","M30","M5","M1")
#設定M30,H1,D輸出參數
File_type<-"New start"
localtimezone<-"Asia/Taipei"
originaltimezone<-"America/New_York"
outputperiod<-c("M30","H1","D")
alignfactor<-c(1800,3num_100,0)
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


#===================================================================================
a<-Sys.time()


#以下是呼叫oanda 所得到的報價
 currencysymbol<-Instrulist[,"instrument"]
 Currencycoltitle<-c("Bid","Ask","Spread","Unit")
 Currency<-mm_protect(nrow=length(currencysymbol),  ncol=length(Currencycoltitle))
 colnames(Currency)<-Currencycoltitle
 rownames(Currency)<-currencysymbol
 Instrumentforactual<-currencysymbol[1]
 for(x in 2:length(currencysymbol))
    {
        Instrumentforactual <- paste(Instrumentforactual,currencysymbol[x],sep="%2C")
    }
 Price <- ActualPrice (AccountType, Token , Instrumentforactual)
 Currency[,"Bid"]<-Price[,"Bid"]
 Currency[,"Ask"]<-Price[,"Ask"]
 Currency[,"Unit"]<-Instrulist[,"pip"]
 Currency[,"Spread"]<-(Currency[,"Ask"]-Currency[,"Bid"])/Currency[,"Unit"]

 Instrulist2<-t(t(as.character(Instrulist[,"instrument"])))
 colnames(Instrulist2)<-"instrument"
 oandaInstrulist<-InstrumentsList(AccountType,Token,AccountID)
 Instrulist2<-merge(Instrulist2,oandaInstrulist,by="instrument")
  #===================================================================================
indexcal<-TRUE
currencypositionorder<-1
indexrequireposition<-currencypositionorder+1
Granularity <- "M30"
operator_6method<-"mobile"
Saved_data<-""
analysetype<-"Test"
if(analysetype=="Train")
   {
       Direct<-TRUE
       if(Direct==TRUE)
          {
              tradedirection<-1
          }else
             {
                  tradedirection<--1
             }

       if( indexcal==TRUE)
          {
              #這邊要加入比對
              File_type<-"Index"
              #這裡記得改
              Instrument<-Indexlist[indexrequireposition]
          }else
             {
                 Instrument<-Instrulist[currencypositionorder,"instrument"]
                 File_type<-"New start" 
             }

             #輸出檔案路徑與路徑檔案數
             filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
             filecount<-filefactorresult$FileCount
             Hprice<-read.csv(filefactorresult$FileListWithPath[1])[,c("TimeStamp" ,"Open"  ,    "High" ,     "Low"      , "Close"    , "Volume"  ,  "Complete" )]
             filecount<-filefactorresult$FileCount
             for(x in 2:filecount)
                {
                    Hprice<-rbind(Hprice,read.csv(filefactorresult$FileListWithPath[x])[,c("TimeStamp" ,"Open"  ,    "High" ,     "Low"      , "Close"    , "Volume"  ,  "Complete" )])

                }      
                           
       #分離timestamp
       timestamp<-t(t(as.character(Hprice[,"TimeStamp"])))
       colnames(timestamp)<-"TimeStamp"
       #計算indicator
       b<-Sys.time()-a


       indicatorname<-c("indicator_1","indicator_2","indicator_3","indicator_4","indicator_5","indicator_6","indicator_7","indicator_8")
       indicatorresult<-mm_protect(ncol=length(indicatorname),nrow=nrow(Hprice))
       colnames(indicatorresult)<-indicatorname
       rownames(indicatorresult)<-seq(1:nrow(indicatorresult))
       indicatorresult<-cbind(timestamp,indicatorresult)
       indicatorresult<-as.data.frame(indicatorresult)
       type<-"Close"
       indicator_1_period<-c(num_1,num_2,num_3,num_4,num_5)
       for(x in 1:length(indicator_1_period))
          {
              indicatorresult[,(x+1)]<-t(t(indicator_function_1(Hprice[,type],indicator_1_period[x])))
          }
       indicatorresult$indicator_6<-t(t(indicator_function_2(Hprice[,type],num_6)))
       indicatorresult$indicator_7<-(indicatorresult[,2]-indicatorresult[,3])/indicatorresult[,2]*100
       indicatorresult$indicator_8<-t(t(indicator_function_3(indicatorresult$indicator_3,1)))



       #計算交易訊號(單一，沒有組合)
       deltavalueseq<-seq(from = 0 , to = num_11, by =0.5)
       #indicator_6_operator_2_colnames<-paste("indicator_6operator_1",deltavalueseq,sep="")
       signalcommonname<-"indicator_6_operator_2_"
       indicator_6_operator_2_colnames<-paste(signalcommonname,num_11-deltavalueseq,"-",num_11+deltavalueseq,sep="")
       signalname<-c("operation_1_operator_1",indicator_6_operator_2_colnames)
       signalresult<-mm_protect(ncol=length(signalname),nrow=nrow(Hprice))
       signalresult<-as.data.frame(signalresult)
       colnames(signalresult)<-signalname
       rownames(signalresult)<-seq(1:nrow(signalresult))
       signalresult<-cbind(timestamp,signalresult)
       signalresult<-as.data.frame(signalresult)

       #這裡計算指標目前是用來計算各種operator_10的組合
       ###forbidden trade strategy### 
        a<-Sys.time()
        for(indicatorcount in 1:length(deltavalueseq))
           {
               deltavalue<-deltavalueseq[indicatorcount]
               if(indicatorcount==1)
                  {
                       ###forbidden trade strategy###
                        
                  }else
                     {
                         ###forbidden trade strategy###
                     }
               
              
           }

        b<-Sys.time()-a   
        write.csv(signalresult,paste(Instrument," ",Granularity," ",signalcommonname," signal result.csv",sep=""))


       if(indexcal==TRUE)
          {

              Instrulist<-read.csv("Instrulist.csv")
              
              Instrument<-Instrulist[currencypositionorder,"instrument"]
              File_type<-"New start"
              #輸出檔案路徑與路徑檔案數
              filefactorresult<-filefactor(File_type,commonpath,Instrument,Granularity,Saved_data)
              filecount<-filefactorresult$FileCount
              Hprice<-read.csv(filefactorresult$FileListWithPath[1])[,c("TimeStamp" ,"Open"  ,    "High" ,     "Low"      , "Close"    , "Volume"  ,  "Complete" )]
              filecount<-filefactorresult$FileCount
              for(x in 2:filecount)
                 {
                     Hprice<-rbind(Hprice,read.csv(filefactorresult$FileListWithPath[x])[,c("TimeStamp" ,"Open"  ,    "High" ,     "Low"      , "Close"    , "Volume"  ,  "Complete" )])

                 }  
          }


       #計算operator_7，可以抽離
       ###forbidden trade strategy###
       operator_7<-cbind(timestamp,operator_7)
       operator_7<-as.data.frame(operator_7)
       for(x in 2:ncol(operator_7))
          {
             operator_7[,x]<-as.numeric(as.character(operator_7[,x]))
          }




       c<-Sys.time()-a

       ###forbidden trade strategy###

        Instrulist3<-t(t(as.character(Instrulist[,"instrument"])))
        colnames(Instrulist3)<-"instrument"
        Instrulist4<-cbind(seq(1:nrow(Instrulist3)),Instrulist3)
        oandaInstrulist<-InstrumentsList(AccountType,Token,AccountID)
        Instrulist2<-merge(Instrulist4,oandaInstrulist,all.x=TRUE,by.x="instrument")
        Instrulist2[,"V1"]<-as.numeric(Instrulist2[,"V1"])
        Instrulist4<-Instrulist2[order(Instrulist2[,"V1"]),]
        Instrulist2<-cbind(Instrulist3,Instrulist2[,c("displayName",    "pip", "maxTradeUnits","marginRate")])
       currencysymbol<-Instrulist2[,"instrument"]
       pip<-as.numeric(as.character(Instrulist2[1,"pip"]))
              
              signalresultsimplify<-signalresult2
              for(indicatorcount in 1:length(deltavalueseq))
                 {   
                     ###forbidden trade strategy###

                 }

              
              

              ###forbidden trade strategy###
   }else
      {
          ###forbidden trade strategy###
      }

       
       monthcount<-(year(as.Date(Hprice[nrow(Hprice),1]))-year(as.Date(Hprice[1,1])))*12-month(as.Date(Hprice[1,1]))+month(as.Date(Hprice[nrow(Hprice),1]))

tradecurrentvalue2<-vector("list",length=length(indicator_6_operator_2_colnames))

       for(indicatorcount in 1:length(indicator_6_operator_2_colnames))
          {   
              #原始交易發生時的價格

              signalresult<-t(t(signalresultsimplify[,indicator_6_operator_2_colnames[indicatorcount]]))
              tradeprice<-signalresult
              #目前交易是在開盤時交易，故trade 地點為"Open"，如果訊號是利用"Open"製作的，那可以不需要考慮延遲
              tradeprice[,1]<-Hprice[,"Open"]
              #將訊號加對值*價格，會留下訊號發生時的交易價格
              signalpricetemp<-t(t(abs(signalresult)*tradeprice))
              #原始交易訊號價格，去掉空值
              signalprice<-t(t(signalpricetemp[which(signalpricetemp!=0),]))
              #原始交易訊號發生位置
              signalposition<-t(t(which(signalpricetemp!=0)))
              if(nrow(signalposition)>1)
                 {
                     ###forbidden trade strategy###
                     overalloperator_12value<-operator_7[which(is.na(operator_7[,"operator_13"])==FALSE),"operator_13"]
                     if(operator_6method!="operator_6")
                        {
                            operator_62<-###forbidden trade strategy###
                        }else
                           {
                               operator_62<-operator_6
                           }
                     signaloperator_5[,2]<-operator_62
                     signaloperator_5<-apply(signaloperator_5,1,max)
                     #無條件進位到小數點一位
                     signaloperator_5 <-t(t(ceiling (signaloperator_5*10)/10))
                     ###forbidden trade strategy###
                     #製作出模板，給之後使用
                     signalpricetemplate<-signalpricetemp
                     nalength<-length(which(is.na(signalresult)))
                     signalpricetemplate[(nalength+1):nrow(signalpricetemplate),]<-0
                     seperatetradesignal<-list(bulltradesignal=signalpricetemplate,beartradesignal=signalpricetemplate)
                     for(tradetypecount in 1:2)
                        {
                            seperatetradesignal[[tradetypecount]]<-signalpricetemplate
                        }
                     operator_13_signaltemp2<-signalpricetemplate


                     #分成作多(1)，做空兩種(-1)，而1是代表買進，-1賣出，所以做多為先1後-1，做空為先-1後1
                     tradesignalfactor<-c(bulltradesignal=1,beartradesignal=-1)
                     tempoperator_13_<-signalpricetemplate
                     #先製作每一個 trade時，填入的價格
                     tradeopenprice<-tradeprice
                     tradecloseprice<-tradeprice
                     for(x in 1:(nrow(signalposition)-1))
                        {
                            ###forbidden trade strategy###

                            ###forbidden trade strategy###
                            #which內是vector的位置，是找出tempoperator_13_哪裡超過止損範圍
                            operator_13_tradepoint<-which(tempoperator_13_>=signaloperator_5[x,])
                            #如果有找到
                            if(length(operator_13_tradepoint)!=0)
                               {
                                   ###forbidden trade strategy###
                                   for(tradetypecount in 1:2)
                                      {
                                          ###forbidden trade strategy###
                                      }
                               }else
                                  {
                                      ###forbidden trade strategy###

                                  }
                      
                        }
                     operator_13_value<-signalpricetemplate
                     colnames(operator_13_value)<-"operator_13_value"
                     #填入成交價
                     operator_13_value[signalposition,]<-signaloperator_5


                     for(tradetypecount in 1:2)
                        {
                            #這裡是填入剛開始交易的交易訊號
                            ###forbidden trade strategy###
                            if(length(tradeposition)!=0)
                               {
                                   ###forbidden trade strategy###
                               }
                            
                            if(Direct==TRUE)
                               {
                                   
                                   profitrate<-tradesignalfactor[tradetypecount]*(tradecloseprice-tradeopenprice)*tradesizeratio+1
                               }else
                                  {
                                      profitrate<-tradesignalfactor[tradetypecount]*(tradecloseprice-tradeopenprice)*tradesizeratio/tradecloseprice+1
                                  }
         
                            seperatetradesignal[[tradetypecount]]<-cbind(seperatetradesignal[[tradetypecount]],operator_13_value,tradesizeratio,tradeopenprice,tradecloseprice,profitrate)
                            colnames(seperatetradesignal[[tradetypecount]])<-c(names(tradesignalfactor)[tradetypecount],"operator_13_value","tradesizeratio","tradeopenprice","tradecloseprice","profitrate")
                        } 

                         
                     tradecurrentvalue2[[indicatorcount]]<-rbind(seperatetradesignal[[1]][which(seperatetradesignal[[1]][,"profitrate"]!=1),],seperatetradesignal[[2]][which(seperatetradesignal[[2]][,"profitrate"]!=1),])

                 }else
                    {
                        tradecurrentvalue2[[indicatorcount]]<-mm_protect(0,ncol=10,nrow=10)
                    }

            }

       for(indicatorcount in 1:length(indicator_6_operator_2_colnames))
          {

              if(sum(tradecurrentvalue2[[indicatorcount]][1,]!=0))
                 {

                     tradecurrentvalue<-tradecurrentvalue2[[indicatorcount]]
                     #profit for each time
                     eachtimeprofit<-profitvalue(t(tradecurrentvalue[which(tradecurrentvalue[,1]!=0 &  tradecurrentvalue[,"profitrate"]!=1),"profitrate"]))
                     #Highest value
                     Highestvalue<-10^(max(eachtimeprofit))
                     #Lowest value
                     Lowestvalue<-10^(min(eachtimeprofit))
                     gainresult<-tradecurrentvalue[which(tradecurrentvalue[,"profitrate"]>1 & tradecurrentvalue[,1]!=0),"profitrate"]
                     
                     if(length(gainresult)!=0)
                        {
                            #Avg gain
                            Avggain<-median(gainresult)
                            #Max gain
                            Maxgain<-max(gainresult)
                            #Gain count
                            Gaincount<-length(gainresult)
                        }else
                           {
                               Avggain<-0
                               Maxgain<-0
                               Gaincount<-0
                           }
                   
                     #Highest asset rate
                     Highestassetsrate<-max(tradecurrentvalue[,"profitrate"])
                     

                     loseresult<-tradecurrentvalue[which(tradecurrentvalue[,"profitrate"]<1 & tradecurrentvalue[,1]!=0),"profitrate"]
                     
                     if(length(loseresult)!=0)
                        {
                            #Avg lose
                            Avglose<-median(loseresult)
                            #Max lose
                            Maxlose<-min(loseresult)
                            #Lose count
                            Losecount<-length(loseresult)
                        }else
                           {
                               Avglose<-0
                               Maxlose<-0
                               Losecount<-0
                           }
                    
                     #Lowest asset rate
                     Lowestassetsrate<-min(tradecurrentvalue[,"profitrate"])
                     
                     #Total profit
                     Totalprofit<-10^(last(eachtimeprofit))
                     #Worest lose
                     Worestlose<-10^(sum(log(tradecurrentvalue[which(tradecurrentvalue[,1]!=0 & tradecurrentvalue[,"profitrate"]<1),"profitrate"],base=10)))
                     #profitrate per month
                     profitratepermonth<-10^(log(Totalprofit,base=10)/monthcount)  
                     successfullrate<-Gaincount/(Gaincount+Losecount)

                     eachsr<-10^(log(Avggain^successfullrate,base=10)+log(Avglose^(1-successfullrate),base=10))
                     summary<-c(Highestvalue,Lowestvalue,Avggain,Maxgain,Highestassetsrate,Gaincount,Avglose,Maxlose,Lowestassetsrate,Losecount,Totalprofit,Worestlose,successfullrate,profitratepermonth,deltavalueseq[indicatorcount],eachsr) 
                 }else
                    {
                        summary<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,deltavalueseq[indicatorcount],0) 
                    }
              
              names(summary)<-c("Highest value","Lowest value","Avg gain","Max gain","Highest assets rate","Gain count","Avg lose","Max lose","Lowest assets rate","Lose count","Total profit","Worest lose","successfull rate","profitrate per month","deltavalueseq","eachsr")
              #summary
              if(indicatorcount==1)
                 {
                    summaryf<-summary
                 }else
                    {
                        summaryf<-rbind(summaryf,summary)
                    }
              
             
                  
                 # return(seperatetradesignal)
          }

          rownames(summaryf)<-indicator_6_operator_2_colnames[1:nrow(summaryf)]

   

   ratefactorposition<-which(summaryf[,"eachsr"]>1 &summaryf[,"profitrate per month"]>1)
   ratefactor<-summaryf[ratefactorposition,"successfull rate"]
   if(analysetype=="Train")
      {

          weight<-t(t(seq(1:nrow(summaryf))))
          weight[,1]<-0
          weight[ratefactorposition,1]<-t(t(ratefactor/sum(ratefactor)))

      }
   colnames(weight)<-"Weight"
   summaryf2<-cbind(summaryf,weight)
   combineresult<-summaryf2[1,]
   for(x in 1:(ncol(summaryf2)-2))
      {
          combineresult[x]<-sum(((summaryf2[,x]-1)*weight))+1
      }
   colposition<-c(6,10,12,13)
   for(x in 1:3)
      {
          eachcp<-colposition[x]
          combineresult[eachcp]<-sum(summaryf2[,eachcp]*weight)
      }
   combineresult["deltavalueseq"]<-"Total"
   combineresult["Weight"]<-1
   summaryf2<-rbind(summaryf2,combineresult)


   e<-Sys.time()-a
write.csv(summaryf2,paste(Instrument," ",Granularity," ",signalcommonname," result summary.csv",sep=""))

a
b
c
d
e


nrow(summaryf2)
