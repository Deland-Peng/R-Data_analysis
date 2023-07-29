#使用规则：
  #1.通过fold参数可以选择训练集与验证集的比例；例如：rep((1:5)为8-2开；rep((1:10)为9-1开。

cross_va_sample <- function(x,seed,fold,count){
  
  xalive <- which(x$status==0)
  xdead <- which(x$status==1)
  
  ####生存样本####
  
  x.alive <- x[xalive,]
  n.alive<-nrow(x.alive)	#个数346
  n.alive	 #346
  #等同k.alive <- rep((1:5), 225)[1:n.alive]
  k.alive <- rep((1:10), round(n.alive/fold)+1)[1:n.alive] #这个可以适应n.alive比1225多的情况

  k.alive	#1到5重复225次(最终得到1225个）,然后再从中获取n.alive个
  
  set.seed(seed)
  i.alive <- sample(k.alive, size=n.alive, replace = FALSE)	#*不放回的在随机在向量k中抽取n.alive个元素
  k.alive.test <-(1:n.alive)[i.alive==fold] #将随机分组中等于fold的数字所对应位置的数据取出
  k.alive.test			#（测试集有115个,训练集有115*2个）
  x.alive.train <- x.alive[(-k.alive.test),] #将测试取出后，剩下的4/5作为训练集
  x.alive.train
  dim(x.alive.train)                  #【提取数据】训练集 231*19757
  x.alive.test <- x.alive[(k.alive.test),]
  dim(x.alive.test)                   #验证集 115*19757
  
  #没的用代码？？？
  #a=dim(x)[2]#数据的列数
  #t.alive.train <- x.alive[(-k.alive.test),a-1]    #时间
  #t.alive.test <- x.alive[(k.alive.test),a-1]
  #s.alive.train <- x.alive[(-k.alive.test),a]   #状态
  #s.alive.test <- x.alive[(k.alive.test),a]
  
  ####死亡样本####
  
  x.dead <- x[xdead,]
  n.dead<-nrow(x.dead)	#列的个数346
  n.dead	 #346
  #k.dead <- rep((1:5), 125)[1:n.dead]
  k.dead <- rep((1:10), round(n.dead/fold)+1)[1:n.dead]
  k.dead	#1到3重复120次(最终得到564个）
  
  set.seed(seed)
  i.dead <- sample(k.dead, size=n.dead, replace = FALSE)	#不放回的在随机在向量k中抽取346个元素
  #i.dead
  k.dead.test <-(1:n.dead)[i.dead==fold]
  k.dead.test			#（测试集有115个,训练集有115*2个）
  x.dead.train <- x.dead[(-k.dead.test),]
  dim(x.dead.train)                  #训练集 87*15536
  x.dead.test <- x.dead[(k.dead.test),]
  dim(x.dead.test)                   #验证集 43*15536
  
  
  ##### 样本 <- 生存：死亡相等####
  
  x.train <- rbind(x.alive.train, x.dead.train)
  x.train <- as.data.frame(x.train)
  #增加1列，test=0
  t0<-rep(0,nrow(x.train))
  x.train$test<-t0

  
  x.test <- rbind(x.alive.test, x.dead.test)
  x.test <- as.data.frame(x.test)
  #增加1列，test=1
  t1<-rep(1,nrow(x.test))
  x.test$test<-t1

  ####合并####
  x.data<-rbind(x.train, x.test)
  
  write.table(x.data,file=sprintf("2_live/test/data/set2/dataset_%d.txt",count),row.names=TRUE,col.names=TRUE,append=FALSE,sep="\t")

}






