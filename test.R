library(xlsx)
setwd("C:/Users/ferra/Desktop/My project/Sanguosha_ranking")

test1<- read.xlsx("test.xlsx",sheetIndex=2,header=TRUE,encoding="UTF-8");
test1<- test1[,3:5]

test2<- read.xlsx("test.xlsx",sheetIndex=3,header=TRUE,encoding="UTF-8");
test2<- test2[,3:5]


test3<- read.xlsx("test.xlsx",sheetIndex=4,header=TRUE,encoding="UTF-8");
test3<- test3[,3:5]

test4<- read.xlsx("test.xlsx",sheetIndex=5,header=TRUE,encoding="UTF-8");
test4<- test4[,3:5]

test5<- read.xlsx("test.xlsx",sheetIndex=6,header=TRUE,encoding="UTF-8");
test5<- test5[,3:5]

test6<- read.xlsx("test.xlsx",sheetIndex=7,header=TRUE,encoding="UTF-8");
test6<- test6[,3:5]

test7<- read.xlsx("test.xlsx",sheetIndex=8,header=TRUE,encoding="UTF-8");
test7<- test7[,3:5]



test <- na.omit(rbind(test1,test2,test3,test4,test5,test6,test7))



warrior<- read.xlsx("test.xlsx",sheetIndex=1,header=TRUE,encoding="UTF-8");   ##读取武将编号文件

n_warrior <- 224
n_game <- nrow(test)
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


rr <-order(rank_v[2,],decreasing = FALSE)   ##稳定概率最终排名
r1 <- rr[1:50]



cbind(c(1:nrow(warrior)),warrior[rr,])

ranking <- warrior[rr,]
# n <- ranking[,3]>=3.5
#hp <- ranking[n,]
#hp
#write.table(hp,"c:/users/ferra/desktop/cup.csv",sep = ",")

#nrow[hp]
#warrior
###

##c1<- cbind(diag(tran1_m))   ##胜率排名
##aa=order(c1,decreasing = TRUE)
##warrior[aa,]

###







