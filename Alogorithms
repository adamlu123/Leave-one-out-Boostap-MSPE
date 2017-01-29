###211 HW1

######Q4 
################################################################################ Part b Naive MSPE
set.seed(12345)
n <- 100
x <- sort( rnorm(n) )
epsilon <- rnorm(n,sd=6)
beta <- c( 0.2, -1.8, -1.4, 1.2, -0.5 )
X <- cbind( 1, poly(x, degree=4, raw=TRUE) )
y <- X %*% beta + epsilon

#############################################################################  Part c scatter plot 
plot(x,y, pch=16, main=" Scatterplot of Y vs. X " , xlab = "X", ylab = "Y", col="blue")
data.md <- lm(y~ poly(x, degree=4, raw=TRUE) , data= as.data.frame(X) )
abline(data.md)

##d
df <- data.frame(y=y, x1=x, x2=x^2)###This is the dataframe

fit0 <- lm( y ~ x1,data=df )
yhat0 <- fitted( fit0 )

fit1 <- lm( y ~ x1 + x2,data=df )
yhat1 <- fitted( fit1 )

M0 <- sum( (y-yhat0)^2  )/n
M1 <- sum( (y-yhat1)^2  )/n
n.M <- c(M0,M1)
print(c(M0,M1))

##########################################################################e new data and True MSPE
set.seed(12345)
n <- 500000 
x.simulated <- sort(rnorm(n))
X<- cbind( 1, poly(x.simulated, degree=4, raw=TRUE))
epsilon <- rnorm(n,sd=6)
beta <- c( 0.2, -1.8, -1.4, 1.2, -0.5 )
y <- X %*% beta + epsilon

pred.y0 <- predict(fit0,newdata = data.frame(y=y, x1=x.simulated))
pred.y1 <- predict(fit1,newdata = data.frame(x1=x.simulated,x2=x.simulated^2))

TM0 <- mean((pred.y0-y)^2); 
TM1 <- mean((pred.y1-y)^2); 
print(c(TM0, TM1))


##########################################################################f Booststrap MSPE

#####Reshow the original dataset
set.seed(12345)
n <- 100
x <- sort( rnorm(n) )
epsilon <- rnorm(n,sd=6)
beta <- c( 0.2, -1.8, -1.4, 1.2, -0.5 )
X <- cbind( 1, poly(x, degree=4, raw=TRUE) )
y <- X %*% beta + epsilon   


B.size <- 100 #observations in one Bsample
BM0 <- rep(0,2000)
BM1 <- rep(0,2000)
Bpred.y0 <- matrix(0,2000, 100)
Bpred.y1 <- matrix(0, 2000, 100)
##############################################################################b.MSPE
for(i in 1:2000){
  id <- sample(1:100, B.size, replace=TRUE)
  
  y.boot <- y[id] # the boot y 
  
  df <- data.frame(y=y.boot, x1=x[id], x2=x[id]^2) ###dataframe
  B.fit0 <- lm(y~x1, data=df)         ## fitted model 0 in each Boostrap sample
  B.fit1 <- lm(y~x1 + x2, data=df)
  #for (j in 1: 100){
    #Bpred.y0[j] <- predict(B.fit0, newdata=data.frame(y=y[j],x1=x[j]))
    #Bpred.y1[j] <- predict(B.fit1, newdata=data.frame(y=y[j],x1=x[j], x2=x[j]^2 ))
    Bpred.y0[i,] <- predict(B.fit0 , newdata=data.frame(y=y,x1=x))
    Bpred.y1[i,] <- predict( B.fit1 , newdata=data.frame(y=y,x1=x, x2=x^2 )) #}
                             #Bpred.y0 <- fitted(B.fit0)     # the new predicted boot y 
                             #Bpred.y1 <- fitted(B.fit1)
  
  BM0[i] <- mean( (Bpred.y0[i,] - y)^2) # Boot mspe in each sample
  BM1[i] <- mean( (Bpred.y1[i,] - y)^2  )
}

B.MSPE0 <- mean(BM0)
B.MSPE1 <- mean(BM1)
 
print( c(B.MSPE0,B.MSPE1) )
#####g  

t <- c(10, 20, 50, 100) ## vector of n 
t2 <- c(10, 20, 50, 100) ## vector of B
 
t3 <- matrix(0,4,4) ## placeholder

for(i in 1:4){
t3[i,] <- (1-(1- 1/t)^t)^t2[i]
}


#### an alternative loop
for(i in 1:4){
  
  
  for(j in 1:4){
    t3[i,j] <- (1-(1- 1/t[j])^t[j])^t2[i]
  }
};t3
########################################################################h LOOBS

set.seed(12345)
n <- 100
x <- sort( rnorm(n) )
epsilon <- rnorm(n,sd=6)
beta <- c( 0.2, -1.8, -1.4, 1.2, -0.5 )
X <- cbind( 1, poly(x, degree=4, raw=TRUE) )
y <- X %*% beta + epsilon  # data preparation
dfm <- data.frame(y=y, x1=x, x2=x^2)        # data preparation dfm= y, x1, x2
                  
B.num <- 1000 # numbers of Bootstrap samples
B.size <- 100 #observations in each sample
B1 <- matrix(0,B.size, B.num) #each entry is 0 o or the boostrap estimate model1
B2 <- matrix(0,B.size, B.num) #each entry is 0 o or the boostrap estimate model2

#B.id <- matrix(0,B.size, B.num) 
a <- seq(1:100) 

for (j in 1:B.num){ # number of boostrap samples B.num; row vector
  id <- sample(1:100, B.size, replace=TRUE)  # column vector, each sample is a column

  d <- setdiff(a, id) #elements that id do not contain, i.e. observations that are not in sample j
  
  fit.new1 <- lm(y~x1, data=dfm[id, ,] )
  fit.new2 <- lm(y~x1+x2, data=dfm[id, ,])
  for(i in 1:B.size){
    if(i %in% d){ # do the prediction for i in the difference set d, i.e. for i not in the j-th bootstrap sample
      B1[i,j] <- predict(fit.new1, newdata=data.frame(y=y[i],x1=x[i]))
      B2[i,j] <- predict(fit.new2, newdata=data.frame(y=y[i],x1=x[i], x2=x[i]^2 ))
      
    }
    
  }
  
}

q <- rep(0,B.size)  #B.size = 100
for(i in 1:B.size){
  p <- B1[i,]
  sq <- (y[i]-p[p!=0])^2
  q[i] <- mean(sq)  
}
LOOB.mspe1 <- mean(q);mean(q)          # model 1 result

q2 <- rep(0,B.size)
for(i in 1:B.size){
  p <- B2[i,]                   #using B2[i,] to compute model2's SE of observation i 
  sq <- (y[i]-p[p!=0])^2
  q2[i] <- mean(sq)             
}
LOOB.mspe2 <- mean(q2);mean(q2)         # model 2 result

l.M <- c(LOOB.mspe1, LOOB.mspe2)

en <- 0.368
Final <- en* n.M + (1- en) * l.M; Final

