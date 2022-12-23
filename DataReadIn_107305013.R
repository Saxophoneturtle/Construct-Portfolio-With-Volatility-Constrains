#107305103企管四陳明鴻
rm(list = ls(all = T))
source("~/Downloads/functions.R")
library(quadprog)
data_ARVL<-read.table("~/Downloads/FinancialData_FinalProj_107305103/ARVL.csv", header = T, sep=",")
data_BMWYY<-read.table("~/Downloads/FinancialData_FinalProj_107305103/BMWYY.csv", header = T, sep=",")
data_BYDDY<-read.table("~/Downloads/FinancialData_FinalProj_107305103/BYDDY.csv", header = T, sep=",")
data_CENN<-read.table("~/Downloads/FinancialData_FinalProj_107305103/CENN.csv", header = T, sep=",")
data_DMLRY<-read.table("~/Downloads/FinancialData_FinalProj_107305103/DMLRY.csv", header = T, sep=",")
data_F<-read.table("~/Downloads/FinancialData_FinalProj_107305103/F.csv", header = T, sep=",")
data_GM<-read.table("~/Downloads/FinancialData_FinalProj_107305103/GM.csv", header = T, sep=",")
data_HMC<-read.table("~/Downloads/FinancialData_FinalProj_107305103/HMC.csv", header = T, sep=",")
data_RACE<-read.table("~/Downloads/FinancialData_FinalProj_107305103/RACE.csv", header = T, sep=",")
data_LCID_UnMod<-read.table("~/Downloads/FinancialData_FinalProj_107305103/LCID.csv", header = T, sep=",")
data_LI<-read.table("~/Downloads/FinancialData_FinalProj_107305103/LI.csv", header = T, sep=",")
data_MULN<-read.table("~/Downloads/FinancialData_FinalProj_107305103/MULN.csv", header = T, sep=",")
data_NIO<-read.table("~/Downloads/FinancialData_FinalProj_107305103/NIO.csv", header = T, sep=",")
data_NKLA<-read.table("~/Downloads/FinancialData_FinalProj_107305103/NKLA.csv", header = T, sep=",")
data_POAHY<-read.table("~/Downloads/FinancialData_FinalProj_107305103/POAHY.csv", header = T, sep=",")
data_FSR<-read.table("~/Downloads/FinancialData_FinalProj_107305103/FSR.csv", header = T, sep=",")
data_STLA<-read.table("~/Downloads/FinancialData_FinalProj_107305103/STLA.csv", header = T, sep=",")
data_TM<-read.table("~/Downloads/FinancialData_FinalProj_107305103/TM.csv", header = T, sep=",")
data_TSLA<-read.table("~/Downloads/FinancialData_FinalProj_107305103/TSLA.csv", header = T, sep=",")
data_TTM<-read.table("~/Downloads/FinancialData_FinalProj_107305103/TTM.csv", header = T, sep=",")
data_XPEV<-read.table("~/Downloads/FinancialData_FinalProj_107305103/XPEV.csv", header = T, sep=",")

data_ARVL[,1]<-as.Date(data_ARVL[,1])
data_BMWYY[,1]<-as.Date(data_BMWYY[,1])
data_BYDDY[,1]<-as.Date(data_BYDDY[,1])
data_CENN[,1]<-as.Date(data_CENN[,1])
data_DMLRY[,1]<-as.Date(data_DMLRY[,1])
data_F[,1]<-as.Date(data_F[,1])
data_GM[,1]<-as.Date(data_GM[,1])
data_HMC[,1]<-as.Date(data_HMC[,1])
data_RACE[,1]<-as.Date(data_RACE[,1])
data_LCID_UnMod[,1]<-as.Date(data_LCID_UnMod[,1])
data_LI[,1]<-as.Date(data_LI[,1])
data_MULN[,1]<-as.Date(data_MULN[,1])
data_NIO[,1]<-as.Date(data_NIO[,1])
data_NKLA[,1]<-as.Date(data_NKLA[,1])
data_POAHY[,1]<-as.Date(data_POAHY[,1])
data_FSR[,1]<-as.Date(data_FSR[,1])
data_STLA[,1]<-as.Date(data_STLA[,1])
data_TM[,1]<-as.Date(data_TM[,1])
data_TSLA[,1]<-as.Date(data_TSLA[,1])
data_TTM[,1]<-as.Date(data_TTM[,1])
data_XPEV[,1]<-as.Date(data_XPEV[,1])

data_ARVL<-data_ARVL[data_ARVL$Date>="2020-09-21",]
data_BMWYY<-data_BMWYY[data_BMWYY$Date>="2020-09-21",]
data_BYDDY<-data_BYDDY[data_BYDDY$Date>="2020-09-21",]
data_CENN<-data_CENN[data_CENN$Date>="2020-09-21",]
data_DMLRY<-data_DMLRY[data_DMLRY$Date>="2020-09-21",]
data_F<-data_F[data_F$Date>="2020-09-21",]
data_GM<-data_GM[data_GM$Date>="2020-09-21",]
data_HMC<-data_HMC[data_HMC$Date>="2020-09-21",]
data_RACE<-data_RACE[data_RACE$Date>="2020-09-21",]
data_LCID_UnMod<-data_LCID_UnMod[data_LCID_UnMod$Date>="2020-09-21",]
data_LI<-data_LI[data_LI$Date>="2020-09-21",]
data_MULN<-data_MULN[data_MULN$Date>="2020-09-21",]
data_NIO<-data_NIO[data_NIO$Date>="2020-09-21",]
data_NKLA<-data_NKLA[data_NKLA$Date>="2020-09-21",]
data_POAHY<-data_POAHY[data_POAHY$Date>="2020-09-21",]
data_FSR<-data_FSR[data_FSR$Date>="2020-09-21",]
data_STLA<-data_STLA[data_STLA$Date>="2020-09-21",]
data_TM<-data_TM[data_TM$Date>="2020-09-21",]
data_TSLA<-data_TSLA[data_TSLA$Date>="2020-09-21",]
data_TTM<-data_TTM[data_TTM$Date>="2020-09-21",]
data_XPEV<-data_XPEV[data_XPEV$Date>="2020-09-21",]

#data_ARVL
#data_BMWYY
#data_BYDDY
#data_CENN
#data_DMLRY
#data_F
#data_GM
#data_HMC
#data_RACE
#data_LCID_UnMod
#data_LI
#data_MULN
#data_NIO
#data_NKLA
#data_POAHY
#data_FSR
#data_STLA
#data_TM
#data_TSLA
#data_TTM
#data_XPEV

sum(is.na(data_ARVL[,6]))
sum(is.na(data_BMWYY[,6]))
sum(is.na(data_BYDDY[,6]))
sum(is.na(data_CENN[,6]))
sum(is.na(data_DMLRY[,6]))
sum(is.na(data_F[,6]))
sum(is.na(data_GM[,6]))
sum(is.na(data_HMC[,6]))
sum(is.na(data_RACE[,6]))
sum(is.na(data_LCID_UnMod[,6]))
sum(is.na(data_LI[,6]))
sum(is.na(data_MULN[,6]))
sum(is.na(data_NIO[,6]))
sum(is.na(data_NKLA[,6]))
sum(is.na(data_POAHY[,6]))
sum(is.na(data_FSR[,6]))
sum(is.na(data_STLA[,6]))
sum(is.na(data_TM[,6]))
sum(is.na(data_TSLA[,6]))
sum(is.na(data_TTM[,6]))
sum(is.na(data_XPEV[,6]))

data_ARVL$ret<-c(NA, round(retx(data_ARVL$Adj.Close),3))  
data_BMWYY$ret<-c(NA, round(retx(data_BMWYY$Adj.Close),3)) 
data_BYDDY$ret<-c(NA, round(retx(data_BYDDY$Adj.Close),3)) 
data_CENN$ret<-c(NA, round(retx(data_CENN$Adj.Close),3)) 
data_DMLRY$ret<-c(NA, round(retx(data_DMLRY$Adj.Close),3))
data_F$ret<-c(NA, round(retx(data_F$Adj.Close),3))  
data_GM$ret<-c(NA, round(retx(data_GM$Adj.Close),3)) 
data_HMC$ret<-c(NA, round(retx(data_HMC$Adj.Close),3)) 
data_RACE$ret<-c(NA, round(retx(data_RACE$Adj.Close),3)) 
data_LCID_UnMod$ret<-c(NA, round(retx(data_LCID_UnMod$Adj.Close),3))
data_LI$ret<-c(NA, round(retx(data_LI$Adj.Close),3))  
data_MULN$ret<-c(NA, round(retx(data_MULN$Adj.Close),3)) 
data_NIO$ret<-c(NA, round(retx(data_NIO$Adj.Close),3)) 
data_NKLA$ret<-c(NA, round(retx(data_NKLA$Adj.Close),3)) 
data_POAHY$ret<-c(NA, round(retx(data_POAHY$Adj.Close),3))
data_FSR$ret<-c(NA, round(retx(data_FSR$Adj.Close),3))  
data_STLA$ret<-c(NA, round(retx(data_STLA$Adj.Close),3)) 
data_TM$ret<-c(NA, round(retx(data_TM$Adj.Close),3))
data_TSLA$ret<-c(NA,round(retx(data_TSLA$Adj.Close),3)) 
data_TTM$ret<-c(NA, round(retx(data_TTM$Adj.Close),3))
data_XPEV$ret<-c(NA,round(retx(data_XPEV$Adj.Close),3))

data_ARVL$per.ret<-round(data_ARVL$ret*100,3)
data_BMWYY$per.ret<-round(data_BMWYY$ret*100,3)
data_BYDDY$per.ret<-round(data_BYDDY$ret*100,3)
data_CENN$per.ret<-round(data_CENN$ret*100,3)
data_DMLRY$per.ret<-round(data_DMLRY$ret*100,3)
data_F$per.ret<-round(data_F$ret*100,3)
data_GM$per.ret<-round(data_GM$ret*100,3)
data_HMC$per.ret<-round(data_HMC$ret*100,3)
data_RACE$per.ret<-round(data_RACE$ret*100,3)
data_LCID_UnMod$per.ret<-round(data_LCID_UnMod$ret*100,3)
data_LI$per.ret<-round(data_LI$ret*100,3)
data_MULN$per.ret<-round(data_MULN$ret*100,3)
data_NIO$per.ret<-round(data_NIO$ret*100,3)
data_NKLA$per.ret<-round(data_NKLA$ret*100,3)
data_POAHY$per.ret<-round(data_POAHY$ret*100,3)
data_FSR$per.ret<-round(data_FSR$ret*100,3)
data_STLA$per.ret<-round(data_STLA$ret*100,3)
data_TM$per.ret<-round(data_TM$ret*100,3)
data_TSLA$per.ret<-round(data_TSLA$ret*100,3)
data_TTM$per.ret<-round(data_TTM$ret*100,3)
data_XPEV$per.ret<-round(data_XPEV$ret*100,3)

data_ARVL$cum.ret<-c(NA,round(cumprod(1+data_ARVL$ret[2:nrow(data_ARVL)]),3))
data_BMWYY$cum.ret<-c(NA,round(cumprod(1+data_BMWYY$ret[2:nrow(data_BMWYY)]),3))
data_BYDDY$cum.ret<-c(NA,round(cumprod(1+data_BYDDY$ret[2:nrow(data_BYDDY)]),3))
data_CENN$cum.ret<-c(NA,round(cumprod(1+data_CENN$ret[2:nrow(data_CENN)]),3))
data_DMLRY$cum.ret<-c(NA,round(cumprod(1+data_DMLRY$ret[2:nrow(data_DMLRY)]),3))
data_F$cum.ret<-c(NA,round(cumprod(1+data_F$ret[2:nrow(data_F)]),3))
data_GM$cum.ret<-c(NA,round(cumprod(1+data_GM$ret[2:nrow(data_GM)]),3))
data_HMC$cum.ret<-c(NA,round(cumprod(1+data_HMC$ret[2:nrow(data_HMC)]),3))
data_RACE$cum.ret<-c(NA,round(cumprod(1+data_RACE$ret[2:nrow(data_RACE)]),3))
data_LCID_UnMod$cum.ret<-c(NA,round(cumprod(1+data_LCID_UnMod$ret[2:nrow(data_LCID_UnMod)]),3))
data_LI$cum.ret<-c(NA,round(cumprod(1+data_LI$ret[2:nrow(data_LI)]),3))
data_MULN$cum.ret<-c(NA,round(cumprod(1+data_MULN$ret[2:nrow(data_MULN)]),3))
data_NIO$cum.ret<-c(NA,round(cumprod(1+data_NIO$ret[2:nrow(data_NIO)]),3))
data_NKLA$cum.ret<-c(NA,round(cumprod(1+data_NKLA$ret[2:nrow(data_NKLA)]),3))
data_POAHY$cum.ret<-c(NA,round(cumprod(1+data_POAHY$ret[2:nrow(data_POAHY)]),3))
data_FSR$cum.ret<-c(NA,round(cumprod(1+data_FSR$ret[2:nrow(data_FSR)]),3))
data_STLA$cum.ret<-c(NA,round(cumprod(1+data_STLA$ret[2:nrow(data_STLA)]),3))
data_TM$cum.ret<-c(NA,round(cumprod(1+data_TM$ret[2:nrow(data_TM)]),3))
data_TSLA$cum.ret<-c(NA,round(cumprod(1+data_TSLA$ret[2:nrow(data_TSLA)]),3))
data_TTM$cum.ret<-c(NA,round(cumprod(1+data_TTM$ret[2:nrow(data_TTM)]),3))
data_XPEV$cum.ret<-c(NA,round(cumprod(1+data_XPEV$ret[2:nrow(data_XPEV)]),3))


