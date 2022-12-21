#107305103企管四陳明鴻
quartz(width = 8, height = 10)
par(mfrow = c(1,1))
plot(x = data_ARVL$Date, y = data_ARVL$Adj.Close, ylim = range(0,1300),
     main = "Time Series Plot of Assets' Prices",
     xlab = "Date", ylab = "Assets' Prices", 
     type="l", lwd =1, col="#FFCC00",
     cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8)

lines(x =data_ARVL$Date, y = data_BMWYY$Adj.Close,  col ="#0033FF" , lwd =1)
lines(x =data_ARVL$Date, y = data_BYDDY$Adj.Close, col = "#FF9900", lwd =1)
lines(x =data_ARVL$Date, y = data_CENN$Adj.Close,  col = "#FF3366", lwd =1)
lines(x =data_ARVL$Date, y = data_DMLRY$Adj.Close,  col = "#0099FF", lwd =1)
lines(x =data_ARVL$Date, y = data_F$Adj.Close, col = "#6699CC", lwd =1)
lines(x =data_ARVL$Date, y = data_GM$Adj.Close,  col = "#9999FF", lwd =1)
lines(x =data_ARVL$Date, y = data_HMC$Adj.Close,  col = "#CC66FF", lwd =1)
lines(x =data_ARVL$Date, y = data_RACE$Adj.Close, col = "#666699", lwd =1)
lines(x =data_ARVL$Date, y = data_LCID_UnMod$Adj.Close,  col = "#FFFF99", lwd =1)
lines(x =data_ARVL$Date, y = data_LI$Adj.Close,  col = "#FFFF00", lwd =1)
lines(x =data_ARVL$Date, y = data_MULN$Adj.Close, col = "#FF6666", lwd =1)
lines(x =data_ARVL$Date, y = data_NIO$Adj.Close,  col = "#FF0099", lwd =1)
lines(x =data_ARVL$Date, y = data_NKLA$Adj.Close,  col = "#CC0033", lwd =1)
lines(x =data_ARVL$Date, y = data_POAHY$Adj.Close, col = "#333399", lwd =1)
lines(x =data_ARVL$Date, y = data_FSR$Adj.Close,  col = "#990033", lwd =1)
lines(x =data_ARVL$Date, y = data_STLA$Adj.Close,  col = "#000033", lwd =1)
lines(x =data_ARVL$Date, y = data_TM$Adj.Close, col = "#33CC99", lwd =1)
lines(x =data_ARVL$Date, y = data_TSLA$Adj.Close,  col = "#CCCC00", lwd =1)
lines(x =data_ARVL$Date, y = data_TTM$Adj.Close,  col = "#CC9999", lwd =1)
lines(x =data_ARVL$Date, y = data_XPEV$Adj.Close, col = "#993300", lwd =1)


legend("topleft", legend = c("ARVL",
                             "BMWYY",
                             "BYDDY",
                             "CENN",
                             "DMLRY",
                             "F",
                             "GM",
                             "HMC",
                             "RACE",
                             "LCID_UnMod",
                             "LI",
                             "MULN",
                             "NIO",
                             "NKLA",
                             "POAHY",
                             "FSR",
                             "STLA",
                             "TM",
                             "TSLA",
                             "TTM",
                             "XPEV"),
       lty = 1, col = c("#FFCC00",
                        "#0033FF",
                        "#FF9900",
                        "#FF3366",
                        "#0099FF",
                        "#6699CC",
                        "#9999FF",
                        "#CC66FF",
                        "#666699",
                        "#FFFF99",
                        "#FFFF00",
                        "#FF6666",
                        "#FF0099",
                        "#CC0033",
                        "#333399",
                        "#990033",
                        "#000033",
                        "#33CC99",
                        "#CCCC00",
                        "#CC9999",
                        "#993300"), 
       lwd = 1,cex = 1)
#Individual Assets' return
par(mfrow = c(4,1))
plot(x = data_ARVL$Date,
     y = data_ARVL$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of ARVL Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_BMWYY$Date,
     y = data_BMWYY$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of BMWYY Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_BYDDY$Date,
     y = data_BYDDY$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of BYDYY Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_CENN$Date,
     y = data_CENN$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of CENN Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_DMLRY$Date,
     y = data_DMLRY$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of DMLRY Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)

plot(x = data_F$Date,
     y = data_F$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of Ford Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_FSR$Date,
     y = data_FSR$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of Fisker Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_GM$Date,
     y = data_GM$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of GM Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_HMC$Date,
     y = data_HMC$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of HMC Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_LCID_UnMod$Date,
     y = data_LCID_UnMod$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of LCID Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_LI$Date,
     y = data_LI$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of LI Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_MULN$Date,
     y = data_MULN$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of MULN Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_NIO$Date,
     y = data_NIO$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of NIO Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)

plot(x = data_NKLA$Date,
     y = data_NKLA$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of NKLA Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_POAHY$Date,
     y = data_POAHY$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of Porsche Holdings Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_RACE$Date,
     y = data_RACE$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of RACE(Ferrari) Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_STLA$Date,
     y = data_STLA$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of STLA Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_TM$Date,
     y = data_TM$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of Toyota Motor Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_TTM$Date,
     y = data_TTM$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of Tata Motor Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_TSLA$Date,
     y = data_TSLA$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of TSLA Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)
plot(x = data_XPEV$Date,
     y = data_XPEV$per.ret, type = "l", lwd =1.5,
     main = "Time series plots of XPEV Daily Return(percentage)",
     xlab = "Date", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=2
)
abline(h=0)

r_range<-range(cbind(data_ARVL[2:438,]$cum.ret,
                     data_BMWYY[2:438,]$cum.ret,
                     data_BYDDY[2:438,]$cum.ret,
                     data_CENN[2:438,]$cum.ret,
                     data_DMLRY[2:438,]$cum.ret,
                     data_F[2:438,]$cum.ret,
                     data_GM[2:438,]$cum.ret,
                     data_HMC[2:438,]$cum.ret,
                     data_RACE[2:438,]$cum.ret,
                     data_LCID_UnMod[2:438,]$cum.ret,
                     data_LI[2:438,]$cum.ret,
                     data_MULN[2:438,]$cum.ret,
                     data_NIO[2:438,]$cum.ret,
                     data_NKLA[2:438,]$cum.ret,
                     data_POAHY[2:438,]$cum.ret,
                     data_FSR[2:438,]$cum.ret,
                     data_STLA[2:438,]$cum.ret,
                     data_TM[2:438,]$cum.ret,
                     data_TSLA[2:438,]$cum.ret,
                     data_TTM[2:438,]$cum.ret,
                     data_XPEV[2:438,]$cum.ret))



par(mfrow = c(1,1))
plot(x = data_ARVL$Date, y = data_ARVL$cum.ret, ylim = r_range,
     main = "Time Series Plot of Assets' Cumulative Return",
     xlab = "Date", ylab = "Cumulative Return", 
     type="l", lwd =1, col=1,
     cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8)

lines(x =data_ARVL$Date, y = data_ARVL$cum.ret,  col ="#FFCC00", lwd = 1)
lines(x =data_ARVL$Date, y = data_BMWYY$cum.ret,  col ="#0033FF", lwd = 1)
lines(x =data_ARVL$Date, y = data_BYDDY$cum.ret, col ="#FF9900", lwd = 1)
lines(x =data_ARVL$Date, y = data_CENN$cum.ret,  col ="#FF3366", lwd = 1)
lines(x =data_ARVL$Date, y = data_DMLRY$cum.ret,  col ="#0099FF", lwd = 1)
lines(x =data_ARVL$Date, y = data_F$cum.ret, col ="#6699CC", lwd = 1)
lines(x =data_ARVL$Date, y = data_GM$cum.ret,  col ="#9999FF", lwd = 1)
lines(x =data_ARVL$Date, y = data_HMC$cum.ret,  col ="#CC66FF", lwd = 1)
lines(x =data_ARVL$Date, y = data_RACE$cum.ret, col = "#666699", lwd = 1)
lines(x =data_ARVL$Date, y = data_LCID_UnMod$cum.ret,  col = "#FFFF99", lwd = 1)
lines(x =data_ARVL$Date, y = data_LI$cum.ret,  col = "#FFFF00", lwd = 1)
lines(x =data_ARVL$Date, y = data_MULN$cum.ret, col = "#FF6666", lwd = 1)
lines(x =data_ARVL$Date, y = data_NIO$cum.ret,  col = "#FF0099", lwd = 1)
lines(x =data_ARVL$Date, y = data_NKLA$cum.ret,  col = "#CC0033", lwd = 1)
lines(x =data_ARVL$Date, y = data_POAHY$cum.ret, col = "#333399", lwd = 1)
lines(x =data_ARVL$Date, y = data_FSR$cum.ret,  col = "#990033", lwd = 1)
lines(x =data_ARVL$Date, y = data_STLA$cum.ret,  col = "#000033", lwd = 1)
lines(x =data_ARVL$Date, y = data_TM$cum.ret, col = "#33CC99", lwd = 1)
lines(x =data_ARVL$Date, y = data_TSLA$cum.ret,  col = "#CCCC00", lwd = 1)
lines(x =data_ARVL$Date, y = data_TTM$cum.ret,  col = "#CC9999", lwd = 1)
lines(x =data_ARVL$Date, y = data_XPEV$cum.ret, col = "#993300", lwd = 1)

abline(h = 1)
legend("topleft", legend = c("ARVL",
                             "BMWYY",
                             "BYDDY",
                             "CENN",
                             "DMLRY",
                             "F",
                             "GM",
                             "HMC",
                             "RACE",
                             "LCID_UnMod",
                             "LI",
                             "MULN",
                             "NIO",
                             "NKLA",
                             "POAHY",
                             "FSR",
                             "STLA",
                             "TM",
                             "TSLA",
                             "TTM",
                             "XPEV"),
       lty = 1, col = c("#FFCC00",
                        "#0033FF",
                        "#FF9900",
                        "#FF3366",
                        "#0099FF",
                        "#6699CC",
                        "#9999FF",
                        "#CC66FF",
                        "#666699",
                        "#FFFF99",
                        "#FFFF00",
                        "#FF6666",
                        "#FF0099",
                        "#CC0033",
                        "#333399",
                        "#990033",
                        "#000033",
                        "#33CC99",
                        "#CCCC00",
                        "#CC9999",
                        "#993300"), 
       lwd = 1,cex=0.75)
data_ARVLcum.ret<-cumprod(1+data_ARVL$ret[253:nrow(data_ARVL)])
data_BMWYYcum.ret<-cumprod(1+data_BMWYY$ret[253:nrow(data_BMWYY)])
data_BYDDYcum.ret<-cumprod(1+data_BYDDY$ret[253:nrow(data_BYDDY)])
data_CENNcum.ret<-cumprod(1+data_CENN$ret[253:nrow(data_CENN)])
data_DMLRYcum.ret<-cumprod(1+data_DMLRY$ret[253:nrow(data_DMLRY)])
data_Fcum.ret<-cumprod(1+data_F$ret[253:nrow(data_F)])
data_GMcum.ret<-cumprod(1+data_GM$ret[253:nrow(data_GM)])
data_HMCcum.ret<-cumprod(1+data_HMC$ret[253:nrow(data_HMC)])
data_RACEcum.ret<-cumprod(1+data_RACE$ret[253:nrow(data_RACE)])
data_LCID_UnModcum.ret<-cumprod(1+data_LCID_UnMod$ret[253:nrow(data_LCID_UnMod)])
data_LIcum.ret<-cumprod(1+data_LI$ret[253:nrow(data_LI)])
data_MULNcum.ret<-cumprod(1+data_MULN$ret[253:nrow(data_MULN)])
data_NIOcum.ret<-cumprod(1+data_NIO$ret[253:nrow(data_NIO)])
data_NKLAcum.ret<-cumprod(1+data_NKLA$ret[253:nrow(data_NKLA)])
data_POAHYcum.ret<-cumprod(1+data_POAHY$ret[253:nrow(data_POAHY)])
data_FSRcum.ret<-cumprod(1+data_FSR$ret[253:nrow(data_FSR)])
data_STLAcum.ret<-cumprod(1+data_STLA$ret[253:nrow(data_STLA)])
data_TMcum.ret<-cumprod(1+data_TM$ret[253:nrow(data_TM)])
data_TSLAcum.ret<-cumprod(1+data_TSLA$ret[253:nrow(data_TSLA)])
data_TTMcum.ret<-cumprod(1+data_TTM$ret[253:nrow(data_TTM)])
data_XPEVcum.ret<-cumprod(1+data_XPEV$ret[253:nrow(data_XPEV)])
par(mfrow = c(1,1))
plot(x = data_ARVL[253:437,]$Date, y = data_ARVLcum.ret[2:186], ylim = range(0,5),
     main = "Time Series Plot of Assets' Cumulative Return During Out of Sample Period",
     xlab = "Date", ylab = "Terminal Wealth(from $1)", 
     type="l", lwd =1, col="#FFCC00",
     cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8)


lines(x =data_ARVL[253:437,]$Date, y = data_BMWYYcum.ret[2:186],  col ="#0033FF",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_BYDDYcum.ret[2:186],  col ="#FF9900",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_CENNcum.ret[2:186],  col ="#FF3366",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_DMLRYcum.ret[2:186],  col ="#0099FF",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_Fcum.ret[2:186],  col ="#6699CC",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_GMcum.ret[2:186],  col ="#9999FF",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_HMCcum.ret[2:186],  col ="#CC66FF",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_RACEcum.ret[2:186],  col ="#666699",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_LCID_UnModcum.ret[2:186],  col = "#FFFF99",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_LIcum.ret[2:186],  col = "#FFFF00",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_MULNcum.ret[2:186],  col = "#FF6666",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_NIOcum.ret[2:186],  col = "#FF0099",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_NKLAcum.ret[2:186],  col = "#CC0033",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_POAHYcum.ret[2:186],  col = "#333399",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_FSRcum.ret[2:186],  col = "#990033",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_STLAcum.ret[2:186],  col = "#000033",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_TMcum.ret[2:186],  col = "#33CC99",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_TSLAcum.ret[2:186],  col = "#CCCC00",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_TTMcum.ret[2:186],  col = "#CC9999",lwd=1)
lines(x =data_ARVL[253:437,]$Date, y = data_XPEVcum.ret[2:186],  col = "#993300",lwd=1)
abline(h = 1)
legend("topleft", legend = c("ARVL",
                             "BMWYY",
                             "BYDDY",
                             "CENN",
                             "DMLRY",
                             "F",
                             "GM",
                             "HMC",
                             "RACE",
                             "LCID_UnMod",
                             "LI",
                             "MULN",
                             "NIO",
                             "NKLA",
                             "POAHY",
                             "FSR",
                             "STLA",
                             "TM",
                             "TSLA",
                             "TTM",
                             "XPEV"),
       lty = 1, col = c("#FFCC00",
                        "#0033FF",
                        "#FF9900",
                        "#FF3366",
                        "#0099FF",
                        "#6699CC",
                        "#9999FF",
                        "#CC66FF",
                        "#666699",
                        "#FFFF99",
                        "#FFFF00",
                        "#FF6666",
                        "#FF0099",
                        "#CC0033",
                        "#333399",
                        "#990033",
                        "#000033",
                        "#33CC99",
                        "#CCCC00",
                        "#CC9999",
                        "#993300"), 
       lwd = 1,cex=0.75)