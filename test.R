test<- read.table("c:/users/ferrarisf50/desktop/test.csv",sep = ",",header=TRUE);
test<- test[,3:5]

test2<- read.table("c:/users/ferrarisf50/desktop/test2.csv",sep = ",",header=TRUE);
test2<- test2[,3:5]


test3<- read.table("c:/users/ferrarisf50/desktop/test3.csv",sep = ",",header=TRUE);
test3<- test3[,3:5]

test <- na.omit(rbind(test,test2,test3))



warrior<- read.table("c:/users/ferrarisf50/desktop/warrior.csv",sep = ",",header=TRUE);   ##读取武将编号文件

n_warrior <- 187
n_game <- 7384

count_m <- matrix(data=0,n_warrior,n_warrior)   ## 武将i对阵武将j先手胜场数
total_m <- matrix(data=0,n_warrior,n_warrior)    ## 武将i对阵武将j先手场数
tran1_m <- matrix(data=0,n_warrior,n_warrior)     ## 转移矩阵
empty_m <- matrix(data=0,n_warrior,n_warrior)      ##武将i与武将j是否已经交手



for (i in 1:n_game) {        ##读取对战记录
total_m[test[i,2],test[i,3]] <- total_m[test[i,2],test[i,3]]+1

if (test[i,1]==1) count_m[test[i,2],test[i,3]]<- count_m[test[i,2],test[i,3]]+1

}



for (i in 1:n_warrior)
{
  for (j in 1:n_warrior) 
  {
    if (i!=j) 
     { 
       if (total_m[i,j]!=0 && total_m[j,i]!=0) 
        
          tran1_m[i,j] <- 0.5*(1-(count_m[i,j]/total_m[i,j]))+   0.5*(count_m[j,i]/total_m[j,i])
 
       
       else if ( total_m[i,j]!=0 && total_m[j,i]==0)
          {
           tran1_m[i,j] <- 0.5*(1-(count_m[i,j]/total_m[i,j]))+ 0.5* ( 1-(count_m[i,j]/total_m[i,j])^2)
           tran1_m[j,i] <- 0.5*(1-sqrt(1-(count_m[i,j]/total_m[i,j])))+ 0.5* ( (count_m[i,j]/total_m[i,j])) 
          }
       else  empty_m[i,j] <- 1
           
     }
    else tran1_m[i,j] <-0 
  }
}


for (i in 1:n_warrior)
{
  for (j in 1:n_warrior)
  {   
           if (i!=j)
                   tran1_m[i,j] <- tran1_m[i,j]/(n_warrior-1-sum(empty_m[i,]))
  }
  tran1_m[i,i] <- 1-sum(tran1_m[i,])
}


library(markovchain)      ##调用markovchain package
colnames(tran1_m) <- warrior[,1]
rownames(tran1_m) <- warrior[,1]

chain_m<-new("markovchain",transitionMatrix=tran1_m)    ## 定义markov链

rank_v <- steadyStates(chain_m)          ## 求稳定状态概率


rr <-order(rank_v,decreasing = TRUE)   ##稳定概率最终排名
warrior[rr,]

ranking <- warrior[rr,]
 n <- ranking[,3]>=3.5
hp <- ranking[n,]
hp
write.table(hp,"c:/users/ferrarisf50/desktop/cup.csv",sep = ",")

nrow[hp]
warrior
###

##c1<- cbind(diag(tran1_m))   ##胜率排名
##aa=order(c1,decreasing = TRUE)
##warrior[aa,]

###







