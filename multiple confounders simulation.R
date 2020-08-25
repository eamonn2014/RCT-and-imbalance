
# http://thestatsgeek.com/2014/02/01/adjusting-for-baseline-covariates-in-randomized-controlled-trials/

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
    fake

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
   
 
 ##bayesian analysis
 fit <- stan_glm(y~ X + z, data=fake)
 fit. <-posterior_interval(fit, prob = 0.95)
 
 
 fit0 <- stan_glm(y~ z, data=fake)
 fit0. <-posterior_interval(fit0, prob = 0.95)
 
 fit.
 fit0.
 
 
 
 
 
 
 