#107305103企管四陳明鴻
quartz(width = 8, height = 10)
par(mfrow = c(3,1))
plot(x = result[(kx+1):nrow(result),]$Date,ylim = range(-15,15),
     y = por_fw_netrx, type = "l", lwd =1.5,
     main = "Fixed Weight Oos Return(percentage)",
     xlab = "Date", ylab = "return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col="#FFCC00"
)
abline(h=0)
plot(x = result[(kx+1):nrow(result),]$Date,ylim = range(-15,15),
     y = por_pw_netrx, type = "l", lwd =1.5,
     main = "Price Weight Oos Return(percentage)",
     xlab = "Date", ylab = "return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col="#009999"
)
abline(h=0)
plot(x = result[(kx+1):nrow(result),]$Date,ylim = range(-15,15),
     y = por_gmvp_netrx, type = "l", lwd =1.5,
     main = "GMVP Oos Return(percentage)",
     xlab = "Date", ylab = "return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=3
)
abline(h=0)
plot(x = result[(kx+1):nrow(result),]$Date,ylim = range(-15,15),
     y = por_nsgmvp_netrx, type = "l", lwd =1.5,
     main = "NSGMVP Oos Return(percentage)",
     xlab = "Date", ylab = "return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=6
)
abline(h=0)
plot(x = result[(kx+1):nrow(result),]$Date,ylim = range(-15,15),
     y = por_mvp_netrx, type = "l", lwd =1.5,
     main = "MVP Oos Return(percentage)",
     xlab = "Date", ylab = "return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=4
)
abline(h=0)
plot(x = result[(kx+1):nrow(result),]$Date,ylim = range(-15,15),
     y = por_rfmvp_netrx, type = "l", lwd =1.5,
     main = "RFMVP Oos Return(percentage)",
     xlab = "Date", ylab = "return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=5
)
abline(h=0)

abline(h=0)
plot(x = result[(kx+1):nrow(result),]$Date,ylim = range(-500,500),
     y = por_tan_netrx, type = "l", lwd =1.5,
     main = "Tangency Portfolio Oos Return(percentage)",
     xlab = "Date", ylab = "return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8, col=7
)
abline(h=0)
par(mfrow = c(1,1))
plot(x = result[(kx+1):nrow(result),]$Date, ylim = range(-1,2.5),
     y = cumr_mvp, type = "l", lwd =1.5,
     main = "Cumulative net return",
     xlab = "Date", ylab = "Cumulative return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8
)
lines(x = result[(kx+1):nrow(result),]$Date,
      y = cumr_nsgmvp, col=2)
lines(x = result[(kx+1):nrow(result),]$Date,
      y = cumr_fw, col=3)
lines(x = result[(kx+1):nrow(result),]$Date,
      y = cumr_pw, col=4)
lines(x = result[(kx+1):nrow(result),]$Date,
      y = cumr_gmvp, col=5)
lines(x = result[(kx+1):nrow(result),]$Date,
      y = cumr_rfmvp, col=6)
lines(x = result[(kx+1):nrow(result),]$Date,
      y = cumr_tan, col=7)

abline(h = 1)
legend("bottomleft",legend = c("MVP","NSGMVP","FW","PW","GMVP","RFMVP","TAN"),lty = 1, col = c(1,2,3,4,5,6,7))
## nsmvp
