##############################################################################
# rcode to simulate covariate adjustment
# run the simfun3 function of interest
# change the model in statfun3 function and run the code below

  rm(list=ls()) 
  set.seed(333) # reproducible
  
  p0 <- function(x) {formatC(x, format="f", digits=0)}
  p1 <- function(x) {formatC(x, format="f", digits=1)}
  p2 <- function(x) {formatC(x, format="f", digits=2)}
  p3 <- function(x) {formatC(x, format="f", digits=3)}
  p4 <- function(x) {formatC(x, format="f", digits=4)}
  p5 <- function(x) {formatC(x, format="f", digits=5)}
  # checking simulations
  require(Matrix)
  
  ###############################################################################
  # parameters
  ###############################################################################
  
  simuls=1000
  theta1=0.223  # treatment effect
  sigma1=0.85   # residual error, noise
  pow =.9      # power
  Fact=1       # factor to adjust strength of covariates
  K1=3         # number of covariates
  covar=2      # the distribution of covariates
  alpha=0.05   # alpha level
  Kp=2         # number of prognostic covariates
  
  ## t-test power
  Po <- power.t.test( delta =theta1, sd=sigma1, sig.level=alpha,
                      power=pow, type="two.sample", alternative=c("two.sided"))
  
  N <-ceiling(Po$n)*2
  N1 <- N # total
  
  # create covariates betas
  b1 <- round(sort(runif(K1, -theta1*Fact,theta1*Fact)), digits=2) 
  
  RR=0.37 ## used to limit correlations between variable


  ###############################################################################
  # create regular prognostic data
  ###############################################################################
 

  simfun3 <- function(N=N1, K=K1, a=1, sigma=sigma1, theta=theta1, b=b1) {
    
    # we can select this, does not seem to have a big inpact
    if (covar==1) {  
      X <- array(runif(N*K , -1,1), c(N,K))     # initially covars were uniform dist
    } else {
      X <- array(rnorm(N*K, 0, 1), c(N,K))  
    }
    
    z <- sample(c(0,1), N, replace=T)                            # treatment indicators
    y <-  a+ X %*% b + theta*z + rnorm(N,0, sigma)              # linear predictor
    #y <- a+            theta*z + rnorm(N,0, sigma)              # linear predictor
    #y <- a+ X[,1:Kp] %*% b[1:Kp] + theta*z + rnorm(N,0, sigma) # linear predictor
    
    data.frame(X=X, y=y, z=z )
    
  }

  ###############################################################################
  # create imbalanced data
  ###############################################################################
 

  simfun3<- function(N=N1, K=K1, a=1, sigma=sigma1, theta=theta1, b=b1) {
    
    MM = N
    N2=MM/2
    N1=N2 
    
    if (covar==1) {  
      X1 <- array(runif(N1*K , -1,1), c(N1,K))  
      X2 <- array(runif(N2*K , -.8,1.2), c(N2,K))   ##imbalance compared to above
      XY <- X <- rbind(X1,X2)
    } else {
      X1 <- array(rnorm(N1*K, 0,  1), c(N1,K))  
      X2 <- array(rnorm(N2*K, .3, 1), c(N2,K))   ##imbalance compared to above
      XY <- X <- rbind(X1,X2)
    }
    
    
    z <- rep(0:1, c(N1,N2))  #assign 1 so we maintain shift in arms
    
    a <- 1                                     # intercept
    
    # b coefficient generated earlier 
#    y <- a+ X %*% b + theta*z + rnorm(MM,0, sigma)  # note I use M here
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # create y covariates not associated with y
    y <- a +  theta*z + rnorm(MM,0, sigma)    # note I use M here
    
    data.frame(X=XY, y=y, z=z)
    
  }
  
  ###############################################################################
  # create correlated data
  ###############################################################################
  
  simfun3<- function(N=N1, K=K1, a=1, sigma=sigma1, theta=theta1, b=b1) {
    
    x <- Matrix(runif(K*K,-RR,RR), K)   # create a correlation matrix randomly , wont allow very high correlations
    
    A <- forceSymmetric(x)
    
    diag(A) <- 1
    
    M <- A
    
    M <- nearPD(M, conv.tol = 1e-7)$mat # default
    # Cholesky decomposition
    L = chol(M)
    nvars = dim(L)[1]
    
    # Random variables that follow an M correlation matrix
    r = t(L) %*% matrix(rnorm(nvars*N, 0,1), nrow=nvars, ncol=N)  #2,2
    r = t(r)
    
    r <- as.matrix(r)#
    rdata <- as.data.frame(r)
    XX<- as.matrix(rdata)
    z <- sample(c(0,1), N, replace=T)                # treatment indicator
    y <- a+ XX %*% b + theta*z + rnorm(N,0, sigma)   # betas created earlier
    data.frame(X=XX, y=y, z=z)
    
  }
  ###############################################################################
  # Function to execute analysis, change model
  ##############################################################################

  statfun3 <- function(d) {
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     zz <- lm(y~z, data=d)    ## ignoring any covariate
    #zz <- lm(y~., data=d)  ## adjusting for all covariates
    
    f <-  summary(zz)
    pr <- predict(zz)
    
    cbind(
      
      coef(f)["z", "Estimate"],           #1
      coef(f)["z", "Std. Error"],         #2
      coef(f)["z", "Pr(>|t|)"]  < alpha,  #3  
      mean((d$y-predict(zz))^2),          #4
      mean(quantile( (d$y-pr)^2, .025)),  #5
      mean(quantile( (d$y-pr)^2, .975)),  #6
      f$sigma,                            #7
      f$adj.r.squared                     #8
       
    )
    
  }
  
  ###############################################################################
  # Execute
  ##############################################################################
 
 
  library(plyr)
  res <- raply(simuls, statfun3(simfun3())) # run the model many times
  # summarize
  result <- apply(res,2,mean)
  q1.result <- apply(res,2, quantile, probs=c(0.025), na.rm=TRUE)
  q2.result <- apply(res,2, quantile, probs=c(0.975), na.rm=TRUE)

  zz <- rbind(
    c(  p4(result[1])   ,p4(q1.result[1]),  p4(q2.result[1]), 
        p4(result[2] ) , p4(result[3] ) ,  p4(result[4] ) , 
        p4(result[5] ),  p4(result[6] ) ,  p4(result[7] )  , p4(result[8] ) )) 

  zz <- as.data.frame(zz)
  colnames(zz) <- c("Mean  ", "Lower 95%CI", "Upper 95%CI", "Stand.error", "Power","MSE", "MSE Low 95%CI", "MSE Upp 95%CI", "sigma","R2")
  zz <- data.frame(lapply(zz, function(x) as.numeric(as.character(x))))
  zz <- as.data.frame(zz)
  zz
 
  ###############################################################################
  # End
  ##############################################################################   
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #mean se power mse mselow msehigh sigma r2
   # summary(sc2)
   # 
   #   xx <- as.data.frame(rbind(apply(sc1,2,mean),apply(sc2,2,mean), apply(sc3,2,mean),
   #         apply(sc4,2,mean),apply(sc5,2,mean),apply(sc6,2,mean),
   #         apply(sc8,2,mean),apply(sc7,2,mean),apply(sc9,2,mean),
   #         apply(sc10,2,mean),apply(sc11,2,mean),apply(sc12,2,mean)))
   #         
   # 
   #   colnames(xx) <- c("mean","se","power","mse","mselow","msehigh","sigma","r2")
   #   rownames(xx) <- c(" adj. for true prognostic covariates", 
   #              " not adj. for true prognostic covariates" ,
   #              " adj. for covariates unrelated to outcome", 
   #              " not adj. for covariates unrelated to outcome",
   #              " adj. for mix of prognostic and unrelated to outcome", 
   #              " not adj. mix of prognostic and unrelated to outcome", 
   #              " adj. for correlated prognostic covariates", 
   #              " not adj. for correlated prognostic covariates",
   #              " adj. for imbalanced prognostic covariates", 
   #              " not adj. for imbalanced prognostic covariates", 
   #              " adj. for imbalanced covariates unrelated to outcome", 
   #              " not adj. imbalanced covariates unrelated to outcome"
   #   )
   # 
   #   xx <- xx[order(xx$mse),]
   #   
   #  xx
 
    #save.image(file = "qc_app_data.RData")
  
   #load("qc_app_data.RData")