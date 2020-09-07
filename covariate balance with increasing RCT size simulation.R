###################################################################
# simulate to show 5% of tie P<=0.05 when checking covariate balance
# # Myth 7. Large trials are more balanced than small ones
# COVARIATE IMBALANCE IN RANDOMIZED CLINICAL TRIALS 469 , see note 2
# However, it is not absolute imbalance which is important but 
# standardized imbalance and this is independent of sample
# size
###################################################################

# function to return how many p-values <0.05

imb <- function(N, K) {  # sample size and number of covariates

  X <- array(runif(N*K , -1,1), c(N,K))  
  z <- sample(c(0,1), N, replace=T)          # treatment indicator
  a <- 1                                     # intercept
  b <- round(sort(runif(K, 0,5)), digits=2)  # making up some beta coefficients
  
  y <- a+ X %*% b + theta*z + rnorm(N,0, sigma)
  fake <- data.frame(X=X, y=y, z=z)
  
  # ttest function
  zz <-   lapply(fake[1:K], function(x) 
    t.test(x ~ fake$z, paired = FALSE, na.action = na.pass)) 
  
  # pull out the pvalue
  zzz <-   lapply(zz, function(x) 
    x[3]
  )
  
  ret = sum(unlist(zzz) < 0.05 )          # record the count of the tosses
  return(ret)
  
} 

  imb(N=100,K=60)  # expect 3 on average

####################################################
# simulate change sample size and covriate

  set.seed(123)                 # for a reproducible result
  I<-  c( 100, 200, 300)        # total sample sizes 
  K <- c(5, 10, 20)             # no of covariates
  nrep <- 1000                  # simulations to run on each scenario

#set up empty array
  pwpr <- array(dim=c( length(I), nrep, length(K), 1 ),
                
                dimnames=list(sample.size=I , 
                              
                              simulation=seq(nrep), 
                              
                              cov.size=(K),
                              
                              Estimate=c("mean"))
  )

## run the function and populate array
  for (i in seq_along(K)) {  # covariate
    
      for (j in seq_along(I)) {  # sample size
  
             pwpr[j,,i,]  <- plyr::raply(nrep, imb(N= I[j], K=K[i]) 
    )
    }
  }  


#######################################################
#https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
  xx <- melt(pwpr)

# get total that differ significantly over all simulations
  zz <- tapply( xx$value, list(xx$sample.size, xx$cov.size), sum)

# divide by number of sims
  r <- as.matrix(zz/nrep) 

# now divide by number of covariates, ot givr the answer, imbalances as judged by p<0.05 for varying no
# of covariates and sample size.
  sweep(r, 2, K, "/")

#######################################################
  
  # simpler approach after seeing Frank Harrell code!! of course  5% are going to be >1.96

  # sd=1, n=50 per group
  
  covariates <- 10
  A <- rnorm(covariates, 0, sqrt(4/50))    # differences
  mean(abs(A) > (1.96*sqrt(4/50)))





  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



