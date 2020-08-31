# https://www.fharrell.com/post/covadj/
# http://thestatsgeek.com/2014/02/01/adjusting-for-baseline-covariates-in-randomized-controlled-trials/


# #The analogous mistake is to argue that in a clinical trial in which prognostic 
# covariates have been measured, 
# the covariates can be ignored because one has randomized.

#First, randomization does not guarantee balance.

# First, randomization does not guarantee balance. 
# Second, balanced covariates may not be ignored. Third, the possible 
# distribution of unmeasured covariates in a validly randomized trial does not 
# invalidate the probability statements about the effect of treatment.
rm(list=ls())
# i started this based on seen  but not made progress
set.seed(987897)
# patients  <- 1000
# covariates <- 1000
# mu1=runif(covariates,-100,100)
# sd1=runif(covariates, 1,100)
# rr <- replicate(covariates,
#                 rnorm(n=patients, mu1, sd1 ))
# head(apply(rr,1, mean))
# head(apply(rr,1, sd))
# 
# # create patients with binary covariates
# p=runif(covariates)
# rr1 <- replicate(covariates,
#                  rbinom(n=patients,prob=p,size=1))
# d <- cbind(rr, rr1)
# dim(d)
# d <- data.frame(d)
# d$trt <- sample(1:2,  patients , replace=TRUE)
# 
# d[1:10,990:1001]









####approach base on gelman's new book p159
   library(rstanarm)
   theta=.4  # treatment effect
   sigma=2  # residual error, noise
   pow =.8


   Po <- power.t.test( delta =theta, sd=sigma, sig.level=0.05,
                    power=pow, type="two.sample", alternative=c("two.sided"))


    N <-ceiling(Po$n)*2
  #  N <- 1000                                  # patients

    K <- 100                                   # variables
    X <- array(runif(N*K , -1,1), c(N,K))  
    z <- sample(c(0,1), N, replace=T)          # treatment indicator
    a <- 1                                     # intercept
    
    # making up beta coefficients
    #b=1:K 
    #b= rep(1,K)                               
    b <- round(sort(runif(K, 0,5)), digits=2)  # making up some beta coefficients
     
    y <- a+ X %*% b + theta*z + rnorm(N,0, sigma)
    fake <- data.frame(X=X, y=y, z=z)
    head(fake)

   # confidence interval
    zz <-   lapply(fake[1:K], function(x) 
                 t.test(x ~ fake$z, paired = FALSE, na.action = na.pass))

    zzz <-   lapply(zz, function(x) 
      x[4]
      )

   zzz<- as.data.frame(zzz)
   ci <- t(zzz)
   conf<- as.data.frame(ci)
   
   
   #  Mean diff
   mzz <-   lapply(zz, function(x) 
     (x[5])
   )
   mzz<- as.data.frame(mzz)
   ci <- t(mzz)
   r<- as.data.frame(ci)
   
   doff <- as.vector( r["mean in group 0"] - r["mean in group 1"] )
 

 
 # plot
 par(mar=c(3,3,3,3), mgp=c(1.5,.5,0), tck=-.01)
 plot(c(0, K+1), range(conf), bty="l", xlab="Covariates", 
      ylab="Estimate Mean difference", xaxs="i",  type="n", 
      main="'Imbalance' estimate mean difference of covariate distribution & 95% confidence interval ")
 #axis(2, seq(-5,5,1))
 # axis(1, seq(1,K,10))
 points(1:K, doff[,1], pch=20)
 abline(0, 0, col="gray")
 for (i in 1:K){
    if (prod(conf[i,c(1,2)]) < 0 ) {
    lines(c(i,i), conf[i,c(1,2)], lwd=.8, col='blue') 
   } else {
    lines(c(i,i), conf[i,c(1,2)], lwd=.8, col='red') 
   }
  }
 
 
 

 # analyzing when Xs are prognostic, related to outcome.
 ols2 <- lm(y~X+z,fake)
 ols1 <- lm(y~z,fake)
 summary(ols2)
 summary(ols1)
 
 # analyzing when X not related to outcome.
 
 y <- a +  theta*z + rnorm(N,0, sigma)
 fake2 <- data.frame(X=X, y=y, z=z)
 
 ols1 <- lm(y~z,fake2)
 ols2 <- lm(y~X+z,fake2)
 summary(ols2)
 summary(ols1)

 # xs related to outcome
 #     adjusting for xs...good
 #    not adjusting for xs...big SE!...not good!!!!!!!!!!!!!!!!!!!!

 # xs not related to outcome
 #    adjusting for xs good
 #   not adjusting for xs good
   
 
 
 
 library(Matrix)
 
 nobs <- N
 
 x <- Matrix(runif(K*K,-1,1), K)   # create a correlation matrix randomly
 
 A <- forceSymmetric(x)
 
 diag(A) <- 1
 
 #isSymmetric(A)
 
 M <- A
 
 #https://r.789695.n4.nabble.com/how-do-I-make-a-correlation-matrix-positive-definite-td3006440.html
 M <- nearPD(M, conv.tol = 1e-7)$mat # default
 # Cholesky decomposition
 L = chol(M)
 nvars = dim(L)[1]
 
 # R chol function produces an upper triangular version of L
 # so we have to transpose it.
 # Just to be sure we can have a look at t(L) and the
 # product of the Cholesky decomposition by itself
 
 t(L)
 
 t(L) %*% L
 
 # Random variables that follow an M correlation matrix
 r = t(L) %*% matrix(rnorm(nvars*nobs, 2,2), nrow=nvars, ncol=nobs)
 r = t(r)
 
 
 r <- as.matrix(r)
 rdata <- as.data.frame(r)
 rdata<- as.matrix(rdata)
 # rdata
 # x
 # splom(rdata)
 # cor(rdata)
 
 # #######################################
 # apply(rdata,2, sd)
 # apply(rdata,2, mean)
 # 
 
 
 
 y <- a+ rdata %*% b + theta*z + rnorm(N,0, sigma)
 fake4 <- data.frame(X=rdata, y=y, z=z)
 
 ols2 <- lm(y~X+z,fake4)
 ols1 <- lm(y~z,fake4)
 summary(ols2)
 summary(ols1)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ##bayesian analysis
 fit <- stan_glm(y~ X + z, data=fake)
 fit. <-posterior_interval(fit, prob = 0.95)
 
 
 fit0 <- stan_glm(y~ z, data=fake)
 fit0. <-posterior_interval(fit0, prob = 0.95)
 
 fit.
 fit0.
 
 
 
 x<- summary(ols1)
 stat1 <- t(cbind(c(x$coefficients["z",], sigma=x$sigma, r2= x$adj.r.squared)))
 

 
 ####################################frank harrell efficient way of cofing 
 
 ## https://discourse.datamethods.org/t/should-we-ignore-covariate-imbalance-and-stop-presenting-a-stratified-table-one-for-randomized-trials/547/3
 ## Simulate a difference in two means with data SD of 1.0
 n<-100 
 k<-100000
 sd =1

 z<- replicate(k,mean(rnorm(n/2,0,1))-mean(rnorm(n/2,0,1)))   # ok
 x <- rnorm(k, sd=sqrt(4 / n))   # much better!!
 
 hist(z, breaks=30)
 hist(x, breaks=30)
 sd(x);summary(x)
 sd(z);summary(z)
 
 
 
 ################################################
 
 
 
 
 ##https://stackoverflow.com/questions/14554558/simulate-a-linear-model-100-times

 # simfun <- function(a=3,B=0.5,C=-0.7,n=200,x1.sd=1,x2.sd=1,e.sd=1) {
 #    x1 <- rnorm(n, mean=0, sd=x1.sd)
 #    x2 <- rnorm(n, mean=0, sd=x2.sd) 
 #    e <-  rnorm(n, mean=0, sd=e.sd)
 #    y1 <- a+B*x1+C*x2+e 
 #    data.frame(x1,x2,y1)
 # }
 # 
 # 
 # 
 # statfun <- function(d) {
 #      
 #    zz <- lm(y1~x1+x2, data=d)
 #   
 #    f <-  summary(zz)
 #    
 #      cbind(
 #      f$coefficients [, 1],
 #      coef(f)[, "Std. Error"] 
 #      ) 
 #    
 # }
 # 
 # library(plyr)
 # raply(100,statfun(simfun()))
 # 
 
 
 
 #################################################
 #################################################run regression multiple times
 #################################################
 
 simfun <- function(N=100, K=10, a=1, sigma=1, theta=0.4) {
    
    X <- array(runif(N*K , -1,1), c(N,K))          # array of variables
    z <- sample(c(0,1), N, replace=T)              # treatment indicator
    b <- round(sort(runif(K, 0,5)), digits=2)      # making up some beta coefficients
    y <- a+ X %*% b + theta*z + rnorm(N,0, sigma)  # linear predictor
    #y2 <- a+  theta*z + rnorm(N,0, sigma)          # linear predictor
    #y <- a+    theta*z + rnorm(N,0, sigma)  # linear predictor
    data.frame(X=X, y=y, z=z)
    
 }
 
 #https://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame
 
 statfun <- function(d) {
    
    zz <- lm(y~., data=d)
    f <-  summary(zz)
    
    cbind(
       f$coefficients [, 1],
       coef(f)[, "Std. Error"]
    )
    
 }
 
 library(plyr)
 res <- raply(100,statfun(simfun())) # run the model many times
 res <- (res[,dim(res)[2],1:2])      # pull out mean of interest and se of interest
 apply(res,2,mean)
 
 
 
 
 
 
 
 
 
 sqrt(4/100)
 
 
 
 #################################################
 #################################################run regression multiple times
 #################################################
 
 simfun <- function(N=100, K=10, a=1, sigma=1, theta=0.4) {
   
   X <- array(runif(N*K , -1,1), c(N,K))          # array of variables
   z <- sample(c(0,1), N, replace=T)              # treatment indicator
   b <- round(sort(runif(K, 0,5)), digits=2)      # making up some beta coefficients
   y <- a+ X %*% b + theta*z + rnorm(N,0, sigma)  # linear predictor
   y2 <- a+  theta*z + rnorm(N,0, sigma)          # linear predictor
   #y <- a+    theta*z + rnorm(N,0, sigma)  # linear predictor
   data.frame(X=X, y=y, z=z, y2=y2)
   
 }
 
 #https://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame
 
 statfun <- function(d) {
   
   zz <- lm(y~.-y2, data=d)
   f <-  summary(zz)
   
   zz1 <- lm(y~z, data=d)
   f1 <-  summary(zz1)
   
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   zz2 <- lm(y2~.-y, data=d)    ## adjusting for  X which are not prognostic
   f2 <-  summary(zz2)
   
   zz3 <- lm(y2~z, data=d)      ## not adjusting for X which are not prognostic
   f3 <-  summary(zz3)
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   
   
   cbind(
     #f$coefficients [,1]["z"],
     coef(f)["z", "Estimate"],
     coef(f)["z", "Std. Error"],
     coef(f1)["z", "Estimate"],
     coef(f1)["z", "Std. Error"],
     
     coef(f2)["z", "Estimate"],
     coef(f2)["z", "Std. Error"],
     coef(f3)["z", "Estimate"],
     coef(f3)["z", "Std. Error"]
     
   )
   
 }
 
 library(plyr)
 res <- raply(100,statfun(simfun())) # run the model many times
 #res <- (res[,dim(res)[2],1:2])      # pull out mean of interest and se of interest
 apply(res,2,mean)
 
 
 
 
 
 
 
 
 
 