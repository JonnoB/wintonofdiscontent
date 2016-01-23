# Kaggle competition Winton dataset

# 
# 
# SETUP
# 
# 


packages <- c("dplyr", "ggplot2", "zoo","forecast", "TSclust", "TSdist", 
              "caret", "Amelia") #, "betareg", "mi", "Amelia") #If amelia is used Betadist and mi can be removed



sapply(packages, library, character.only = TRUE)
select <- dplyr::select #Clash between dplyr and MASS which is loaded by Caret
basewd <- "C:/Users/Jonno/Documents/Kaggle/Winton"
setwd(basewd)
data.raw<- read.csv("train.csv")
test <- read.csv("test_2.csv")
data <- data.raw
# 
# 
# clean
# 
# 

# The plan is to find highly correlated variables, 
# variables with near zero varience and variables with to many missing values to reduce the size
# of the data set
#Ther are some pretty extreme outliers as well these should be deleted and re imputed
# then impute missing values
# then create some regression models for the day ahead models
# then create some forecasts for the intra day models


sum(complete.cases(data)) #929 complete cases virtually non of the observations are complete, remedies will need to be taken

#data_small <- data %>% sample_frac(0.01)

nearZeroVar(test[2:28]); nearZeroVar(data[1:28]) #these are the same so can be removed from the datasets

# 
# 
# BASE
# 
#

#zero prediction plus1 plus2 returns
sum((data[,208:209])^2) / (nrow(data)*ncol(data[,208:209]))

#timeseries returns
sum((data[,148:207])^2) / (nrow(data)*ncol(data[,148:207]))

#all returns
sum((data[,148:209])^2) / (nrow(data)*ncol(data[,148:209]))




#there are a lot of missing data, if this can be reduced then the imputation can be improved
#If columns with more than 10% missing are removed and only the main features are considered 
#for bayesian imputation then the number of missing values can be greatly reduced

NASdata <- sapply( data[,1:28], function(n) sum(is.na(n)))
NAStest <- sapply( test[,1:28], function(n) sum(is.na(n)))

which(NAStest/nrow(test) >0.1) #returns index of major missing elements

sum(is.na(data)) #209467
sum(is.na(data[,2:28])) #97006
sum(is.na(data[,-c(2,3,5,11,21,29:211)])) #19529

sum(is.na(test)) #641703
sum(is.na(test[,2:28])) #292275
sum(is.na(test[,-c(2,3,5,11,21,29:211)])) #59992


#luckily the percentage of missing elements is similar on both data sets. removal of the other
#columns results in a 90% reduction in number of missing data to impute

data2 <- data[,-c(2,3,5,11,21,29:211, 17)] #removes near zero variance row 2
test2 <- test[,-c(2,3,5,11,21,29:211, 17)]
#checks for highly corellated columns and removes
cortest <-cor(data2, use = "pairwise.complete.obs")
hc = findCorrelation(cortest, cutoff=0.9)
data2 <- data[,-hc] #can I keep this column in?x
test2 <- test2[,-hc]

#this was so fast that I am suspicious it isn't working properly, it took about 10 minutes to make 5 imputations
imputs1 <- data2 %>% amelia(., idvars = "Id", m = 80)
imputs2 <- test2 %>% amelia(., idvars = "Id", m = 80)

setwd(file.path(basewd,"train_80_noIntra"))
write.amelia( imputs1, file.stem = "train_80_noIntra")

setwd(file.path(basewd,"test_80_noIntra"))
write.amelia( imputs2, file.stem = "trest_80_noIntra")
plot(imputs) #shows how well the fit was relative to the actual density, as can bee seen some of the fitted distribtions are very bad

# 
# Filling with the median is a rubbish imputation
# for (n in 1:ncol(data_small)) {
#   data_small[is.na(data_small[,n]),n] <- median(data_small[,n], na.rm = TRUE)
# }
# 
# 
# 
# for (n in 1:ncol(data)) {
#   data[is.na(data[,n]),n] <- median(data[,n], na.rm = TRUE)
# }


vis <- data_small[1,] %>% select(Ret_2:Ret_180) %>%t %>% data.frame(., minut = 1:179)
names(vis)[1] <- "returns"
ggplot(vis, aes(x=minut, y = returns)) + geom_line()
ggplot(vis, aes(x = returns)) + geom_density()

x <- data_small %>% select(Ret_2:Ret_180) %>% sapply(., mean,na.rm=TRUE)  %>% 
  data.frame(., minut = 1:179)
names(x)[1] <- "returns"
ggplot(x, aes(x=minut, y = returns)) + geom_line()
ggplot(x, aes(x = returns)) + geom_density()

# 
# 
# Modelling
# 
# 

#Create training and test sets
  set.seed(1902)
  traindatarows <- createDataPartition(data$Ret_PlusOne, p=0.8)
  testdata <- data[-traindatarows[[1]],]
  traindata <- data[traindatarows[[1]],]


data_small_Retplus1 <- traindata %>% select(Feature_1:Ret_120, Ret_PlusOne)
rm(data.raw); rm(traindata)

data_Retplus1 <- data %>% select(Feature_1:Ret_120, Ret_PlusOne)
data_Retplus2 <- data %>% select(Feature_1:Ret_120, Ret_PlusTwo)


train_control <- trainControl(method="cv", number=10)
 
#mod1 <- train(Ret_PlusOne ~., data_small_Retplus1, trControl = train_control, method = "lm")
#mod2 <- train(data_small_Retplus1[,1:146], data_small_Retplus1$Ret_PlusOne, 
#                  trControl = train_control, method = "rf")

mod3 <- train(Ret_PlusOne ~., data_Retplus1, trControl = train_control, method = "lm")
mod4 <- train(Ret_PlusTwo ~., data_Retplus2, trControl = train_control, method = "lm")

#mod4 <- train(data_Retplus1[,1:146], data_Retplus1$Ret_PlusOne, 
              trControl = train_control, method = "rf")

postResample(predict(mod2, data), data$Ret_PlusOne)

#data_Retplus2 <- data %>% select(Feature_1:Ret_120, Ret_PlusTwo)

mod6 <- train(data_Retplus1[,1:146], data$Ret_PlusTwo, 
              trControl = train_control, method = "rf")

# 
# 
# PARRALLELISING
# 
# 
z=vector('list',4)
z=1:4
system.time(lapply(z,function(x) Sys.sleep(1)))
cl<-makeCluster(2,type="SOCK")
  system.time(clusterApply(cl, z,function(x) Sys.sleep(1)))
  stopCluster(cl)