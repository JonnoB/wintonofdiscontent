

x <- sample(0:1, 1000, replace = T) %>% data.frame

classmat <- matrix(0, nrow = 1000, ncol = 3) %>% data.frame

classmat <- sapply(1:3, function(n){
      x<- ifelse(x[1]==1 & runif(1000) >0.2,1,0)
      #x <- ifelse(x[1]==0 & runif(1000) >0.8,0,1)
      x
     }
) %>% data.frame

confusionMatrix(classmat[,1], x)

cutoff <- sapply(1:3, function(n)
  ifelse(rowSums(classmat ==1) >n,1,0) 
)

AggAcc <- sapply(1:3, function(i) {
  confusionMatrix(cutoff[,i], x)$overall[1]
}
)