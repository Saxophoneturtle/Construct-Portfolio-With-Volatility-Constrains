#107305103企管四陳明鴻

result<-data.frame(matrix(0,nrow(data_ARVL),22))
result[,1]<-data_ARVL$Date
result[,2:ncol(result)]<-cbind(data_ARVL$per.ret,
                               data_BMWYY$per.ret,
                               data_BYDDY$per.ret,
                               data_CENN$per.ret,
                               data_DMLRY$per.ret,
                               data_F$per.ret,
                               data_GM$per.ret,
                               data_HMC$per.ret,
                               data_RACE$per.ret,
                               data_LCID_UnMod$per.ret,
                               data_LI$per.ret,
                               data_MULN$per.ret,
                               data_NIO$per.ret,
                               data_NKLA$per.ret,
                               data_POAHY$per.ret,
                               data_FSR$per.ret,
                               data_STLA$per.ret,
                               data_TM$per.ret,
                               data_TSLA$per.ret,
                               data_TTM$per.ret,
                               data_XPEV$per.ret
)
colnames(result)<-c("Date",
                    "ARVL",
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
                    "XPEV")
result<-result[-1,]

##daily portfolio return
##note that it is needed to transform the data.frame to matrix

kx<-252                                   ##window length
hx<-nrow(result)-kx                     ##length of out-of-sample period

## portfolio weights, starting from period t-1
wx_gmvp_mat<-matrix(0, hx+1, ncol(result)-1)
wx_mvp_mat<-matrix(0, hx+1, ncol(result)-1)
wx_rfmvp_mat<-matrix(0, hx+1, ncol(result)-1)
wx_nsgmvp_mat<-matrix(0, hx+1, ncol(result)-1)
wx_fw_mat<-matrix(0, hx+1, ncol(result)-1)
wx_pw_mat<-matrix(0, hx+1, ncol(result)-1)
wx_tan_mat<-matrix(0, hx+1, ncol(result)-1)

## portfolio net return
por_gmvp_netrx<-numeric(hx)
por_mvp_netrx<-numeric(hx)
por_rfmvp_netrx<-numeric(hx)
por_nsgmvp_netrx<-numeric(hx)
por_fw_netrx<-numeric(hx)
por_pw_netrx<-numeric(hx)
por_tan_netrx<-numeric(hx)
#percentage portfolio return
per_por_mvp_netrx<-numeric(hx)
per_por_rfmvp_netrx<-numeric(hx)
per_por_gmvp_netrx<-numeric(hx)
per_por_nsgmvp_netrx<-numeric(hx)
per_por_fw_netrx<-numeric(hx)
per_por_pw_netrx<-numeric(hx)
per_por_tan_netrx<-numeric(hx)
## turn over rate
tor_gmvp<-numeric(hx)
tor_mvp<-numeric(hx)
tor_rfmvp<-numeric(hx)
tor_nsgmvp<-numeric(hx)
tor_fw<-numeric(hx)
tor_pw<-numeric(hx)
tor_tan<-numeric(hx)

## HHI
hhi_gmvp<-numeric(hx)
hhi_mvp<-numeric(hx)
hhi_rfmvp<-numeric(hx)
hhi_nsgmvp<-numeric(hx)
hhi_fw<-numeric(hx)
hhi_pw<-numeric(hx)
hhi_tan<-numeric(hx)
## SLR
slr_gmvp<-numeric(hx)
slr_mvp<-numeric(hx)
slr_rfmvp<-numeric(hx)
slr1_nsgmvp<-numeric(hx)
slr_fw<-numeric(hx)
slr_pw<-numeric(hx)
slr_tan<-numeric(hx)
## transaction cost
epx<-0.35/1000 ##transaction cost
rfx<-3/252
mu_targ<-0.07                                            
rf1<-3/252

for(i in 1:hx){
  
  datax<-result[i:(i+kx-1), 2:ncol(result)]             
   
  ##mvp
  wx_mvp<-as.vector(mvp_wx(datax, mu_targ = mu_targ))
  
  ##gmvp
  wx_gmvp<-gmvp_wx_quad(datax)$solution
  ##nsmvp
  wx_rfmvp<-rf_mvp_wx_quad(datax,mu_targ = mu_targ,rf = rfx)$solution
  ##nsgmvp
  wx_nsgmvp<-nsgmvp_wx_quad(datax)$solution                          
  wx_nsgmvp<-round(wx_nsgmvp,8)
  #Fixed Weight
  wx_fw<-c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)/(21)
  #Tangency Portfolio
  wx_tan<-as.vector(tan_wx(datax, rf = rf1))
  #Price Weighted
  wx_pw<-cbind(data_ARVL[kx+i-1,]$Adj.Close,
               data_BMWYY[kx+i-1,]$Adj.Close,
               data_BYDDY[kx+i-1,]$Adj.Close,
               data_CENN[kx+i-1,]$Adj.Close,
               data_DMLRY[kx+i-1,]$Adj.Close,
               data_F[kx+i-1,]$Adj.Close,
               data_GM[kx+i-1,]$Adj.Close,
               data_HMC[kx+i-1,]$Adj.Close,
               data_RACE[kx+i-1,]$Adj.Close,
               data_LCID_UnMod[kx+i-1,]$Adj.Close,
               data_LI[kx+i-1,]$Adj.Close,
               data_MULN[kx+i-1,]$Adj.Close,
               data_NIO[kx+i-1,]$Adj.Close,
               data_NKLA[kx+i-1,]$Adj.Close,
               data_POAHY[kx+i-1,]$Adj.Close,
               data_FSR[kx+i-1,]$Adj.Close,
               data_STLA[kx+i-1,]$Adj.Close,
               data_TM[kx+i-1,]$Adj.Close,
               data_TSLA[kx+i-1,]$Adj.Close,
               data_TTM[kx+i-1,]$Adj.Close,
               data_XPEV[kx+i-1,]$Adj.Close)/
              (data_ARVL[kx+i-1,]$Adj.Close+
               data_BMWYY[kx+i-1,]$Adj.Close+
               data_BYDDY[kx+i-1,]$Adj.Close+
               data_CENN[kx+i-1,]$Adj.Close+
               data_DMLRY[kx+i-1,]$Adj.Close+
               data_F[kx+i-1,]$Adj.Close+
               data_GM[kx+i-1,]$Adj.Close+
               data_HMC[kx+i-1,]$Adj.Close+
               data_RACE[kx+i-1,]$Adj.Close+
               data_LCID_UnMod[kx+i-1,]$Adj.Close+
               data_LI[kx+i-1,]$Adj.Close+
               data_MULN[kx+i-1,]$Adj.Close+
               data_NIO[kx+i-1,]$Adj.Close+
               data_NKLA[kx+i-1,]$Adj.Close+
               data_POAHY[kx+i-1,]$Adj.Close+
               data_FSR[kx+i-1,]$Adj.Close+
               data_STLA[kx+i-1,]$Adj.Close+
               data_TM[kx+i-1,]$Adj.Close+
               data_TSLA[kx+i-1,]$Adj.Close+
               data_TTM[kx+i-1,]$Adj.Close+
                 data_XPEV[kx+i-1,]$Adj.Close)
              
  
  ##note that wpx is from 2010-12-30
  ##use the weights generated from previous day's data 
  
  rx<-result[i+kx,2:ncol(result)]                      
  rx_lag<-datax[nrow(datax),] 
  
  ## individual assets' turnover over rate
  tor_ind<-wx_mvp-wx_mvp_mat[i,]*(1+rx_lag)/(1+sum(wx_mvp_mat[i,]*rx_lag))
  tor_rfmvp_ind<-wx_rfmvp-wx_rfmvp_mat[i,]*(1+rx_lag)/(1+sum(wx_rfmvp_mat[i,]*rx_lag))
  tor_gmvp_ind<-wx_gmvp-wx_gmvp_mat[i,]*(1+rx_lag)/(1+sum(wx_gmvp_mat[i,]*rx_lag))
  tor1_ind<-wx_nsgmvp-wx_nsgmvp_mat[i,]*(1+rx_lag)/(1+sum(wx_nsgmvp_mat[i,]*rx_lag))
  tor_fw_ind<-wx_fw-wx_fw_mat[i,]*(1+rx_lag)/(1+sum(wx_fw_mat[i,]*rx_lag))
  tor_pw_ind<-wx_pw-wx_pw_mat[i,]*(1+rx_lag)/(1+sum(wx_pw_mat[i,]*rx_lag))
  tor_tan_ind<-wx_tan-wx_tan_mat[i,]*(1+rx_lag)/(1+sum(wx_tan_mat[i,]*rx_lag))
  ## portfolio turn over rate
  tor_mvp[i]<-sum(abs(tor_ind))
  tor_rfmvp[i]<-sum(abs(tor_rfmvp_ind))
  tor_gmvp[i]<-sum(abs(tor_gmvp_ind))
  tor_nsgmvp[i]<-sum(abs(tor1_ind))
  tor_fw[i]<-sum(abs(tor_fw_ind))
  tor_pw[i]<-sum(abs(tor_pw_ind))
  tor_tan[i]<-sum(abs(tor_tan_ind))
  ## portfolio net return
  por_mvp_netrx[i]<-(1+sum(wx_mvp*rx))*(1-epx*tor_mvp[i])-1
  por_rfmvp_netrx[i]<-(1+sum(wx_rfmvp*rx))*(1-epx*tor_rfmvp[i])-1
  por_gmvp_netrx[i]<-(1+sum(wx_gmvp*rx))*(1-epx*tor_gmvp[i])-1
  por_nsgmvp_netrx[i]<-(1+sum(wx_nsgmvp*rx))*(1-epx*tor_nsgmvp[i])-1
  por_fw_netrx[i]<-(1+sum(wx_fw*rx))*(1-epx*tor_fw[i])-1
  por_pw_netrx[i]<-(1+sum(wx_pw*rx))*(1-epx*tor_pw[i])-1
  por_tan_netrx[i]<-(1+sum(wx_tan*rx))*(1-epx*tor_tan[i])-1
  #percentage portfolio return
  per_por_mvp_netrx[i]<-((1+sum(wx_mvp*rx))*(1-epx*tor_mvp[i])-1)*100
  per_por_rfmvp_netrx[i]<-((1+sum(wx_rfmvp*rx))*(1-epx*tor_rfmvp[i])-1)*100
  per_por_gmvp_netrx[i]<-((1+sum(wx_gmvp*rx))*(1-epx*tor_gmvp[i])-1)*100
  per_por_nsgmvp_netrx[i]<-((1+sum(wx_nsgmvp*rx))*(1-epx*tor_nsgmvp[i])-1)*100
  per_por_fw_netrx[i]<-((1+sum(wx_fw*rx))*(1-epx*tor_fw[i])-1)*100
  per_por_pw_netrx[i]<-((1+sum(wx_pw*rx))*(1-epx*tor_pw[i])-1)*100
  per_por_tan_netrx[i]<-((1+sum(wx_tan*rx))*(1-epx*tor_tan[i])-1)*100
  ## HHI
  hhi_mvp[i]<-sum(wx_mvp^2)/(sum(abs(wx_mvp))^2)
  hhi_rfmvp[i]<-sum(wx_rfmvp^2)/(sum(abs(wx_rfmvp))^2)
  hhi_gmvp[i]<-sum(wx_gmvp^2)/(sum(abs(wx_gmvp))^2)
  hhi_nsgmvp[i]<-sum(wx_nsgmvp^2)/(sum(abs(wx_nsgmvp))^2)
  hhi_fw[i]<-sum(wx_fw^2)/(sum(abs(wx_fw))^2)
  hhi_pw[i]<-sum(wx_pw^2)/(sum(abs(wx_pw))^2)
  hhi_tan[i]<-sum(wx_tan^2)/(sum(abs(wx_tan))^2)
  ## SLR
  slr_mvp[i]<-sum(abs(wx_mvp[wx_mvp<0]))/sum(abs(wx_mvp[wx_mvp>0]))
  slr_rfmvp[i]<-sum(abs(wx_rfmvp[wx_rfmvp<0]))/sum(abs(wx_rfmvp[wx_rfmvp>0]))
  slr_gmvp[i]<-sum(abs(wx_gmvp[wx_gmvp<0]))/sum(abs(wx_gmvp[wx_gmvp>0]))
  slr1_nsgmvp[i]<-sum(abs(wx_nsgmvp[wx_nsgmvp<0]))/sum(abs(wx_nsgmvp[wx_nsgmvp>0]))
  slr_fw[i]<-sum(abs(wx_fw[wx_fw<0]))/sum(abs(wx_fw[wx_fw>0]))
  slr_pw[i]<-sum(abs(wx_pw[wx_pw<0]))/sum(abs(wx_pw[wx_pw>0]))
  slr_tan[i]<-sum(abs(wx_tan[wx_tan<0]))/sum(abs(wx_tan[wx_tan>0]))
  
  ## store portfolio weight vector at this period
  wx_mvp_mat[i+1,]<-wx_mvp
  wx_rfmvp_mat[i+1,]<-wx_rfmvp
  wx_gmvp_mat[i+1,]<-wx_gmvp
  wx_nsgmvp_mat[i+1,]<-wx_mvp
  wx_fw_mat[i+1,]<-wx_fw
  wx_pw_mat[i+1,]<-wx_pw
  wx_tan_mat[i+1,]<-wx_tan
  print(i)
  
   
  
}  

cumr_nsgmvp<-cumprod(1+por_nsgmvp_netrx/100)    
cumr_fw<-cumprod(1+por_fw_netrx/100)
cumr_mvp<-cumprod(1+por_mvp_netrx/100)
cumr_rfmvp<-cumprod(1+por_rfmvp_netrx/100)
cumr_gmvp<-cumprod(1+por_gmvp_netrx/100)
cumr_pw<-cumprod(1+por_pw_netrx/100)  
cumr_tan<-cumprod(1+por_tan_netrx/100)


