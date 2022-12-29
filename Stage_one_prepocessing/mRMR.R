library('mRMRe')

#遍历90%范围参数数量
for(fc in 10:60){

  data <- read.csv("Stage2/data/neutropenia_sepsis_basic_detNA.csv", encoding="UTF-8")
  #data <- read.csv("Stage2/all_model/apply/data/eicu_neumis_data.csv", encoding="UTF-8")
  
  feature_num = ncol(data) - 2
  train_feature = data[,2:feature_num] 
  #id列
  train_id = data[1] 
  #结果列
  st<-ncol(data)-1
  train_st = data[,st:ncol(data)] 
  
  train_label = data[,ncol(data)]
  mrmr_feature<-train_feature
  mrmr_feature$y<-train_label
  #筛选的数据要加上Y值
  target_indices = which(names(mrmr_feature)=='y')
  #因为读取csv文件的时候，Y（label）值是整数，没有小数，所以就被转换成了integer
  #但是mRMR.data需要所有的数据都是numeric，所以要将里面不是numeric的转换成numeric
  for (m in which(sapply(mrmr_feature, class)!="numeric")){mrmr_feature[,m]=as.numeric(unlist(mrmr_feature[,m]))}
  #转化成mRMR.data的形式才能被使用
  Data <- mRMR.data(data = data.frame(mrmr_feature))
  #data就是数据，target_indices就是Y（label）值，也就是需要对比相关度的列
  #feature_count是需要筛选出来的特征个数
  mrmr=mRMR.ensemble(data = Data, target_indices = target_indices, 
                     feature_count = fc, solution_count = 1)
  #获取筛选出来的特征的列，包含在mrmr@filters中，mrmr@filters[原特征个数]这个list里
  index=mrmr@filters[[as.character(mrmr@target_indices)]]
  #获取训练集特征
  new_data <- nrow(data):ncol(index)
  dim(new_data) <- c(nrow(data),ncol(index))
  #new_data = data[,index]
  new_data = train_feature[,index]
  new_data_0 = cbind(train_id,new_data)
  new_data_1 = cbind(new_data_0,train_st)
  
  file_dir=paste0("Stage2/data/mRMR_features/mimic_neutropenia_fselected_",fc,".csv")
  write.csv (new_data_1,file_dir,row.names = F)
  #write.csv (new_data_0,"Stage2/all_model/apply/data/eicu_neumis_data_s10.csv",row.names = F)
  
}
