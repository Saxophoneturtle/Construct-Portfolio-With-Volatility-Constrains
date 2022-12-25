## some summary statistics for net portfolio returns
por_mvp_ret_stat<-c(summary(por_mvp_netrx), sd(por_mvp_netrx))
por_rfmvp_ret_stat<-c(summary(por_rfmvp_netrx), sd(por_rfmvp_netrx))
por_gmvp_ret_stat<-c(summary(por_gmvp_netrx), sd(por_gmvp_netrx))
por_nsgmvp_ret_stat<-c(summary(por_nsgmvp_netrx), sd(por_nsgmvp_netrx))
por_fw_ret_stat<-c(summary(por_fw_netrx), sd(por_fw_netrx))
por_pw_ret_stat<-c(summary(por_pw_netrx), sd(por_pw_netrx))




por_mvp_SP<-(mean(por_mvp_netrx)-rfx)/(sd(por_mvp_netrx))*sqrt(252)
por_rfmvp_SP<-(mean(por_rfmvp_netrx)-rfx)/(sd(por_rfmvp_netrx))*sqrt(252)
por_gmvp_SP<-(mean(por_gmvp_netrx)-rfx)/(sd(por_gmvp_netrx))*sqrt(252)
por_nsgmvp_SP<-(mean(por_nsgmvp_netrx)-rfx)/(sd(por_nsgmvp_netrx))*sqrt(252)
por_fw_SP<-(mean(por_fw_netrx)-rfx)/(sd(por_fw_netrx))*sqrt(252)
por_pw_SP<-(mean(por_pw_netrx)-rfx)/(sd(por_pw_netrx))*sqrt(252)
por_tan_SP<-(mean(por_tan_netrx)-rfx)/(sd(por_tan_netrx))*sqrt(252)
por_sp<-data.frame(0,7)
por_sp<-data.frame(por_fw_SP,
          por_pw_SP,
          por_mvp_SP,
          por_rfmvp_SP,
          por_gmvp_SP,
          por_nsgmvp_SP,
          por_tan_SP)
colnames(por_sp)<-c("FW", "PW", "MVP", "RfMVP","GMVP", "NSGMVP", "TAN")
por_sp<-round(por_sp,3)
write.table(por_sp, "~/Downloads/FinancialData_FinalProj_107305103/por_sp.csv", sep = ",")
## tunrover rate


por_tor_st<-rbind(c(summary(tor_fw),sd(tor_fw)),
                  c(summary(tor_pw),sd(tor_pw)),
                  c(summary(tor_mvp),sd(tor_mvp)),
                  c(summary(tor_rfmvp),sd(tor_rfmvp)),
                  c(summary(tor_gmvp),sd(tor_gmvp)),
                  c(summary(tor_nsgmvp),sd(tor_nsgmvp)),
                  c(summary(tor_tan),sd(tor_tan))
)
rownames(por_tor_st)<-c("FW", "PW", "MVP", "RfMVP","GMVP", "NSGMVP", "TAN")
colnames(por_tor_st)[7]<-("Std.")
por_tor_st<-round(por_tor_st,3)
write.table(por_tor_st, "~/Downloads/FinancialData_FinalProj_107305103/por_tor_st.csv", sep = ",")
## HHI index
por_HHI_st<-rbind(c(summary(hhi_fw),sd(hhi_fw)),
                  c(summary(hhi_pw),sd(hhi_pw)),
                  c(summary(hhi_mvp),sd(hhi_mvp)),
                  c(summary(hhi_rfmvp),sd(hhi_rfmvp)),
                  c(summary(hhi_gmvp),sd(hhi_gmvp)),
                  c(summary(hhi_nsgmvp),sd(hhi_nsgmvp)),
                  c(summary(hhi_tan),sd(hhi_tan))
)
rownames(por_HHI_st)<-c("FW", "PW", "MVP", "RfMVP","GMVP", "NSGMVP", "TAN")
colnames(por_HHI_st)[7]<-("Std.")
por_HHI_st<-round(por_HHI_st,3)
write.table(por_HHI_st, "~/Downloads/FinancialData_FinalProj_107305103/por_HHI_st.csv", sep = ",")
## SLR

por_SLR_st<-rbind(c(summary(slr_fw),sd(slr_fw)),
                  c(summary(slr_pw),sd(slr_pw)),
                  c(summary(slr_mvp),sd(slr_mvp)),
                  c(summary(slr_rfmvp),sd(slr_rfmvp)),
                  c(summary(slr_gmvp),sd(slr_gmvp)),
                  c(summary(slr1_nsgmvp),sd(slr1_nsgmvp)),
                  c(summary(slr_tan),sd(slr_tan)))
rownames(por_SLR_st)<-c("FW", "PW", "MVP", "RfMVP","GMVP", "NSGMVP", "TAN")
colnames(por_SLR_st)[7]<-("Std.")
por_SLR_st<-round(por_SLR_st,3)
write.table(por_SLR_st, "~/Downloads/FinancialData_FinalProj_107305103/por_SLR_st.csv", sep = ",")
  

total_por_per_ret<-cbind(por_fw_netrx,
                         por_pw_netrx,
                         por_mvp_netrx,
                         por_rfmvp_netrx,
                         por_gmvp_netrx,
                         por_nsgmvp_netrx,
                         por_tan_netrx,
                         data_GM[254:438,]$per.ret,
                         data_F[254:438,]$per.ret,
                         data_TSLA[254:438,]$per.ret,
                         data_LCID_UnMod[254:438,]$per.ret,
                         data_NIO[254:438,]$per.ret
)
dim(total_por_per_ret)[1]
summary_por<-rbind(dim(total_por_per_ret)[1],
                   apply(total_por_per_ret, 2,summary),
                   apply(total_por_per_ret, 2,var)*252,
                   apply(total_por_per_ret, 2, sd)*sqrt(252),
                   apply(total_por_per_ret, 2, my_skewness)/sqrt(252),
                   apply(total_por_per_ret, 2, my_kurtosis)/252,
                   apply(total_por_per_ret, 2, my_acf1))
rownames(summary_por)[1]<-"No. of Obs."
rownames(summary_por)[8:nrow(summary_por)]<-c("Var.","Std.","SKewness.","Kurtosis.","ACF1.")
colnames(summary_por)<-c("FW", "PW", "MVP", "RfMVP","GMVP", "NSGMVP", "TAN","GM","Ford","TSLA","Lucid","NIO")
summary_por<-t(round(summary_por,3))
summary_por<-data.frame(summary_por)
summary_por<-round(summary_por,3)
summary_por
write.table(summary_por, "~/Downloads/FinancialData_FinalProj_107305103/summary_por.csv", sep = ",")
por_fw_risk<-cbind(VaR_normx(por_fw_netrx, 1, 0.05),
                   ES_normx(por_fw_netrx, 1, 0.05),
                   LPSDx(x = por_fw_netrx, rfx = rfx)
)
por_gmvp_risk<-cbind(VaR_normx(por_gmvp_netrx, 1, 0.05),
                     ES_normx(por_gmvp_netrx, 1, 0.05),
                     LPSDx(x = por_gmvp_netrx, rfx = rfx))
por_mvp_risk<-cbind(VaR_normx(por_mvp_netrx, 1, 0.05),
                    ES_normx(por_mvp_netrx, 1, 0.05),
                    LPSDx(x = por_mvp_netrx, rfx = rfx))
por_nsgmvp_risk<-cbind(VaR_normx(por_nsgmvp_netrx, 1, 0.05),
                       ES_normx(por_nsgmvp_netrx, 1, 0.05),
                       LPSDx(x = por_nsgmvp_netrx, rfx = rfx))
por_rfmvp_risk<-cbind(VaR_normx(por_rfmvp_netrx,1, 0.05),
                      ES_normx(por_rfmvp_netrx, 1, 0.05),
                      LPSDx(x = por_rfmvp_netrx, rfx = rfx))
por_pw_risk<-cbind(VaR_normx(por_pw_netrx, 1, 0.05),
                   ES_normx(por_pw_netrx, 1, 0.05),
                   LPSDx(x = por_pw_netrx, rfx = rfx))
por_tan_risk<-cbind(VaR_normx(por_tan_netrx, 1s, 0.05),
                    ES_normx(por_tan_netrx, 1, 0.05),
                    LPSDx(x = por_tan_netrx, rfx = rfx))
TSLA_risk<-cbind(VaR_normx(data_TSLA[254:438,]$per.ret, 1, 0.05),
                 ES_normx(data_TSLA[254:438,]$per.ret, 1, 0.05),
                 LPSDx(x = data_TSLA[254:438,]$per.ret, rfx = rfx))
NIO_risk<-cbind(VaR_normx(data_NIO[254:438,]$per.ret, 1, 0.05),
                ES_normx(data_NIO[254:438,]$per.ret, 1, 0.05),
                LPSDx(x = data_NIO[254:438,]$per.ret, rfx = rfx))
LCID_risk<-cbind(VaR_normx(data_LCID_UnMod[254:438,]$per.ret, 1, 0.05),
                 ES_normx(data_LCID_UnMod[254:438,]$per.ret, 1, 0.05),
                 LPSDx(x = data_LCID_UnMod[254:438,]$per.ret, rfx = rfx))
GM_risk<-cbind(VaR_normx(data_GM[254:438,]$per.ret, 1, 0.05),
               ES_normx(data_GM[254:438,]$per.ret, 1, 0.05),
               LPSDx(x = data_GM[254:438,]$per.ret, rfx = rfx))
Ford_risk<-cbind(VaR_normx(data_F[254:438,]$per.ret, 1, 0.05),
                 ES_normx(data_F[254:438,]$per.ret, 1, 0.05),
                 LPSDx(x = data_F[254:438,]$per.ret, rfx = rfx))
summary_por_risk<-rbind(por_fw_risk,
                        por_pw_risk,
                        por_mvp_risk,
                        por_rfmvp_risk,
                        por_gmvp_risk,
                        por_nsgmvp_risk,
                        por_tan_risk,
                        GM_risk,
                        Ford_risk,
                        TSLA_risk,
                        LCID_risk,
                        NIO_risk)

colnames(summary_por_risk)<-c("VaR(0.05)","ES(0.05)","LPSD")
rownames(summary_por_risk)<-c("FW", "PW", "MVP", "RfMVP","GMVP", "NSGMVP", "TAN","GM","Ford","TSLA","Lucid","NIO")
summary_por_risk<-round(summary_por_risk,3)
summary_por_risk
write.table(summary_por_risk, "~/Downloads/FinancialData_FinalProj_107305103/summary_por_risk.csv", sep = ",")
dim(data_TSLA[252:438,])[1]

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
BHCUMR<-cbind(data_ARVLcum.ret,
              data_BMWYYcum.ret,
              data_BYDDYcum.ret,
              data_CENNcum.ret,
              data_DMLRYcum.ret,
              data_Fcum.ret,
              data_GMcum.ret,
              data_HMCcum.ret,
              data_RACEcum.ret,
              data_LCID_UnModcum.ret,
              data_LIcum.ret,
              data_MULNcum.ret,
              data_NIOcum.ret,
              data_NKLAcum.ret,
              data_POAHYcum.ret,
              data_FSRcum.ret,
              data_STLAcum.ret,
              data_TMcum.ret,
              data_TSLAcum.ret,
              data_TTMcum.ret,
              data_XPEVcum.ret
)