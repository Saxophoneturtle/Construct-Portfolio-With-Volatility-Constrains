#107305103企管四陳明鴻
data_IndAsset<-data.frame(matrix(0,nrow(data_ARVL),22))
data_IndAsset[,1]<-data_ARVL$Date
data_IndAsset[,2:ncol(data_IndAsset)]<-cbind(data_ARVL$per.ret,
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

colnames(data_IndAsset)<-c("Date",
                           "ARVL",
                           "BMWYY",
                           "BYDDY",
                           "CENN",
                           "DMLRY",
                           "F",
                           "GM",
                           "HMC",
                           "RACE",
                           "LCID_UnMo",
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
data_IndAsset<-data_IndAsset[-1,]

summary_IndAsset<-rbind(dim(data_IndAsset)[1],
                        apply(data_IndAsset[,2:ncol(data_IndAsset)],2, summary),
                        apply(data_IndAsset[,2:ncol(data_IndAsset)],2, var)*252,
                        apply(data_IndAsset[,2:ncol(data_IndAsset)],2, sd)*sqrt(252),
                        apply(data_IndAsset[,2:ncol(data_IndAsset)],2, my_skewness)/sqrt(252),
                        apply(data_IndAsset[,2:ncol(data_IndAsset)],2, my_kurtosis)/252,
                        apply(data_IndAsset[,2:ncol(data_IndAsset)],2, my_acf1))
rownames(summary_IndAsset)[1]<-("Nums of Orbs.")
rownames(summary_IndAsset)[8:nrow(summary_IndAsset)]<-c("Var.","Std.","SKewness.","Kurtosis.","ACF1.")
summary_IndAsset<-t(round(summary_IndAsset,3))
summary_IndAsset<-data.frame(summary_IndAsset)
summary_IndAsset
write.table(summary_IndAsset, "~/Downloads/FinancialData_FinalProj_107305103/summary_IndAsset.csv", sep = ",")

data_IndAsset_OosPd<-data_IndAsset[253:437,]
summary_IndAsset_OosPd<-rbind(dim(data_IndAsset_OosPd)[1],
                        apply(data_IndAsset_OosPd[,2:ncol(data_IndAsset_OosPd)],2, summary),
                        apply(data_IndAsset_OosPd[,2:ncol(data_IndAsset_OosPd)],2, var)*252,
                        apply(data_IndAsset_OosPd[,2:ncol(data_IndAsset_OosPd)],2, sd)*sqrt(252),
                        apply(data_IndAsset_OosPd[,2:ncol(data_IndAsset_OosPd)],2, my_skewness)/sqrt(252),
                        apply(data_IndAsset_OosPd[,2:ncol(data_IndAsset_OosPd)],2, my_kurtosis)/252,
                        apply(data_IndAsset_OosPd[,2:ncol(data_IndAsset_OosPd)],2, my_acf1))
rownames(summary_IndAsset_OosPd)[1]<-("Nums of Orbs.")
rownames(summary_IndAsset_OosPd)[8:nrow(summary_IndAsset_OosPd)]<-c("Var.","Std.","SKewness.","Kurtosis.","ACF1.")
summary_IndAsset_OosPd<-t(round(summary_IndAsset_OosPd,3))
summary_IndAsset_OosPd<-data.frame(summary_IndAsset_OosPd)
summary_IndAsset_OosPd
write.table(summary_IndAsset_OosPd, "~/Downloads/FinancialData_FinalProj_107305103/summary_IndAsset_OosPd.csv", sep = ",")