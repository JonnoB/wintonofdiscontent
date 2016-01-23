

for (n in 1:ncol(test)) {
  test[is.na(test[,n]),n] <- median(test[,n], na.rm = TRUE)
}

rmeds <- apply(test[,29:147], 1, median, na.rm = TRUE)
x <-matrix(rmeds,nrow=length(rmeds),ncol=60,byrow=FALSE) %>% data.frame 

test_plus1 <- predict(mod3, test)
test_plus2 <- predict(mod4, test)

x <- cbind(x, test_plus1, test_plus2)
x <- as.vector(t(x)) %>% data.frame

obs = 120000
vars = 62
ids <- paste(rep(1:obs, each = vars), "_",rep(1:vars, obs), sep="")

x2 <- cbind(ids, x) #%>% t %>% data.frame
names(x2) <- c("Id", "Predicted")
write.csv(x2, "sub_1.csv", row.names = FALSE)
