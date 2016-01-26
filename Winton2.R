


x <-matrix(0, nrow = nrow(data), ncol = 60) %>% data.frame 

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
