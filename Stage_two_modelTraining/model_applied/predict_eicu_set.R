library(dplyr)
library(survival)
#library(Biobase)
library(randomForestSRC)
library(survivalROC)
library(survminer)
library(Hmisc)
library(xgboost)
library(Matrix)
library(glmnet)
library(survivalsvm)
library(Ckmeans.1d.dp)
library(rms)
library(pec)
library(ggplot2)
require(ggsci)
library("scales")
pal_nejm("default")(8)
show_col(pal_nejm("default")(8))

  ####Load 10Features data####
F10data <- read.csv("Stage2/all_model/apply/data/eicu_neumis_data_10f.csv", encoding="UTF-8")


t.F10data <- F10data$time
s.F10data <- F10data$status
num_feature <- dim(F10data)[2]
lostset <-as.data.frame(F10data[,c(2:num_feature)])


#XGBoost
F10data.xgb <- data.matrix(F10data) # 将自变量转化为矩阵
dF10data<-list(data=F10data.xgb[,c(2:(num_feature-2))],label=F10data.xgb[,(num_feature-1)]*(-(-1)^(as.numeric(F10data.xgb[,num_feature]))))	#time
DF10data<-xgb.DMatrix(dF10data$data,label=dF10data$label)

F10data_xgb_pred <-  predict(xgboost10Fmodel,DF10data)
F10data_xgb_result <- list()
cutoff = 14
if ( min(F10data$time) < cutoff )
{
  y <- survivalROC(Stime = t.F10data, status = s.F10data, marker = F10data_xgb_pred, predict.time = cutoff,lambda=0.01,method = "NNE")
  F10data_xgb_result$fp_1 = y$FP
  F10data_xgb_result$tp_1 = y$TP
  F10data_xgb_result$auc_1 = y$AUC
}else
{
  F10data_xgb_result$fp_1 = NA
  F10data_xgb_result$tp_1 = NA
  F10data_xgb_result$auc_1 = NA
}

cutoff=28
y <- survivalROC(Stime = t.F10data, status = s.F10data, marker = F10data_xgb_pred, predict.time = cutoff,lambda=0.01,method = "NNE")
F10data_xgb_result$fp_3 = y$FP
F10data_xgb_result$tp_3 = y$TP
F10data_xgb_result$auc_3 = y$AUC

##F10data
plot(F10data_xgb_result$fp_1, F10data_xgb_result$tp_1, ## x=FP,y=TP
     type="l",col="#BC3C29FF", ##线条设置
     xlim=c(0,1), ylim=c(0,1), 
     xlab=("1 - Specificity"), ##连接
     ylab="Sensitivity",
     main="F10-XGBoost eICU ROC")## \n换行符
abline(0,1,col="gray",lty=2)##线条颜色

#lines函数在原有基础上继续绘图 #E18727FF
#legend函数增加legend
lines(F10data_xgb_result$fp_3, F10data_xgb_result$tp_3, type="l",col="#0072B5FF",xlim=c(0,1), ylim=c(0,1))
legend(0.45,0.3,c(paste("AUC-XGBoost-14days =",round(F10data_xgb_result$auc_1,3)),
                  paste("AUC-XGBoost-28days =",round(F10data_xgb_result$auc_3,3)))
)

#RSF
F10data_rsf_pred <- predict(rsf10Fmodel,as.data.frame(F10data[,c(2:num_feature)]))
F10data_rsf_result <- list()
cutoff = 14
if ( min(F10data$time) < cutoff )
{
  y <- survivalROC(Stime = t.F10data, status = s.F10data, marker = F10data_rsf_pred$predicted, predict.time = cutoff,lambda=0.01,method = "NNE")
  F10data_rsf_result$fp_1 = y$FP
  F10data_rsf_result$tp_1 = y$TP
  F10data_rsf_result$auc_1 = y$AUC
}else
{
  F10data_rsf_result$fp_1 = NA
  F10data_rsf_result$tp_1 = NA
  F10data_rsf_result$auc_1 = NA
}

cutoff=28
y <- survivalROC(Stime = t.F10data, status = s.F10data, marker = F10data_rsf_pred$predicted, predict.time = cutoff,lambda=0.01,method = "NNE")
F10data_rsf_result$fp_3 = y$FP
F10data_rsf_result$tp_3 = y$TP
F10data_rsf_result$auc_3 = y$AUC

##F10data
plot(F10data_rsf_result$fp_1, F10data_rsf_result$tp_1, ## x=FP,y=TP
     type="l",col="#BC3C29FF", ##线条设置
     xlim=c(0,1), ylim=c(0,1), 
     xlab=("1 - Specificity"), ##连接
     ylab="Sensitivity",
     main="F10data_RSF eICU ROC")## \n换行符
abline(0,1,col="gray",lty=2)##线条颜色

#lines函数在原有基础上继续绘图 #E18727FF
#legend函数增加legend
lines(F10data_rsf_result$fp_3, F10data_rsf_result$tp_3, type="l",col="#0072B5FF",xlim=c(0,1), ylim=c(0,1))
legend(0.45,0.3,c(paste("AUC-RSF-14days =",round(F10data_rsf_result$auc_1,3)),
                  paste("AUC-RSF-28days =",round(F10data_rsf_result$auc_3,3)))
)



    ####load 52features data####
F52data <- read.csv("Stage2/all_model/apply/data/eicu_neumis_data_52f.csv", encoding="UTF-8")

  t.F52data <- F52data$time
  s.F52data <- F52data$status
  num_feature <- dim(F52data)[2]
  lostset <-as.data.frame(F52data[,c(2:num_feature)])
    
    
    #XGBoost
    F52data.xgb <- data.matrix(F52data) # 将自变量转化为矩阵
    dF52data<-list(data=F52data.xgb[,c(2:(num_feature-2))],label=F52data.xgb[,(num_feature-1)]*(-(-1)^(as.numeric(F52data.xgb[,num_feature]))))	#time
    DF52data<-xgb.DMatrix(dF52data$data,label=dF52data$label)
    
    F52data_xgb_pred <-  predict(xgboost52Fmodel,DF52data)
    F52data_xgb_result <- list()
    cutoff = 14
    if ( min(F52data$time) < cutoff )
    {
      y <- survivalROC(Stime = t.F52data, status = s.F52data, marker = F52data_xgb_pred, predict.time = cutoff,lambda=0.01,method = "NNE")
      F52data_xgb_result$fp_1 = y$FP
      F52data_xgb_result$tp_1 = y$TP
      F52data_xgb_result$auc_1 = y$AUC
    }else
    {
      F52data_xgb_result$fp_1 = NA
      F52data_xgb_result$tp_1 = NA
      F52data_xgb_result$auc_1 = NA
    }
    
    cutoff=28
    y <- survivalROC(Stime = t.F52data, status = s.F52data, marker = F52data_xgb_pred, predict.time = cutoff,lambda=0.01,method = "NNE")
    F52data_xgb_result$fp_3 = y$FP
    F52data_xgb_result$tp_3 = y$TP
    F52data_xgb_result$auc_3 = y$AUC
    
    ##F52data
    plot(F52data_xgb_result$fp_1, F52data_xgb_result$tp_1, ## x=FP,y=TP
         type="l",col="#BC3C29FF", ##线条设置
         xlim=c(0,1), ylim=c(0,1), 
         xlab=("1 - Specificity"), ##连接
         ylab="Sensitivity",
         main="F52data_XGBoost eICU ROC")## \n换行符
    abline(0,1,col="gray",lty=2)##线条颜色
    
    #lines函数在原有基础上继续绘图 #E18727FF
    #legend函数增加legend
    lines(F52data_xgb_result$fp_3, F52data_xgb_result$tp_3, type="l",col="#0072B5FF",xlim=c(0,1), ylim=c(0,1))
    legend(0.45,0.3,c(paste("AUC-XGBoost-14days =",round(F52data_xgb_result$auc_1,3)),
                      paste("AUC-XGBoost-28days =",round(F52data_xgb_result$auc_3,3)))
    )

#RSF
    F52data_rsf_pred <- predict(rsf52Fmodel,as.data.frame(F52data[,c(2:num_feature)]))
    F52data_rsf_result <- list()
    cutoff = 14
    if ( min(F52data$time) < cutoff )
    {
      y <- survivalROC(Stime = t.F52data, status = s.F52data, marker = F52data_rsf_pred$predicted, predict.time = cutoff,lambda=0.01,method = "NNE")
      F52data_rsf_result$fp_1 = y$FP
      F52data_rsf_result$tp_1 = y$TP
      F52data_rsf_result$auc_1 = y$AUC
    }else
    {
      F52data_rsf_result$fp_1 = NA
      F52data_rsf_result$tp_1 = NA
      F52data_rsf_result$auc_1 = NA
    }
    
    cutoff=28
    y <- survivalROC(Stime = t.F52data, status = s.F52data, marker = F52data_rsf_pred$predicted, predict.time = cutoff,lambda=0.01,method = "NNE")
    F52data_rsf_result$fp_3 = y$FP
    F52data_rsf_result$tp_3 = y$TP
    F52data_rsf_result$auc_3 = y$AUC
    
    ##F52data
    plot(F52data_rsf_result$fp_1, F52data_rsf_result$tp_1, ## x=FP,y=TP
         type="l",col="#BC3C29FF", ##线条设置
         xlim=c(0,1), ylim=c(0,1), 
         xlab=("1 - Specificity"), ##连接
         ylab="Sensitivity",
         main="F52data_RSF eICU ROC")## \n换行符
    abline(0,1,col="gray",lty=2)##线条颜色
    
    #lines函数在原有基础上继续绘图 #E18727FF
    #legend函数增加legend
    lines(F52data_rsf_result$fp_3, F52data_rsf_result$tp_3, type="l",col="#0072B5FF",xlim=c(0,1), ylim=c(0,1))
    legend(0.45,0.3,c(paste("AUC-RSF-14days =",round(F52data_rsf_result$auc_1,3)),
                      paste("AUC-RSF-28days =",round(F52data_rsf_result$auc_3,3)))
    )
    
        
  