

sim=1000
n =100


x <- replicate(sim, mean(rnorm(n/2, 0,1))-mean(rnorm(n/2, 0,1 )))
sd(x)
summary(x)
 
x2 <- rnorm(1000, sd=sqrt(4/ n))
sd(x2)
summary(x2)
 

##generalise

std <- 1

sim=1000
n =100
x <- replicate(sim, mean( rnorm(n/2, 0, std)) - mean(rnorm(n/2, 0, std )))
sd(x)
summary(x)

x2 <- rnorm(1000, sd= sqrt(  (std^2+std^2) / (n/2) )  )
sd(x2)
summary(x2)

 




###################################################################################################################################################
#https://discourse.datamethods.org/t/should-we-ignore-covariate-imbalance-and-stop-presenting-a-stratified-table-one-for-randomized-trials/547/3


n <- 100
k <- 10
nsim <- 1000
#set.seed(1)
ne <- jc <- integer(nsim)
nc <- 0
for(i in 1 : nsim) {
  ## Simulate a difference in two means with data SD of 1.0
  x <- rnorm(k, sd=sqrt(4 / n))
  ne[i] <- sum(x > 0.3)
  if(ne[i] == 1) {
    ## Simulate 200 additional covariates (never needed > 40)
    x  <- rnorm(200, s=sqrt(4 / n))
    ## Find first counterbalance not preceeded by another imbalance
    ## in the original direction
    j <- min(which(x < -0.3))
    if(j == 1 || max(x[1 : (j - 1)]) < 0.3) {
      nc <- nc + 1
      jc[nc] <- j
    }
  }
}
jc <- jc[1 : nc]
## Distribution of number of positively imbalanced covariates in first 20
table(ne)
ne
 

## Distribution of # additional covariates that had to be examined to
## find a counterbalance without being preceeded by another + imbalance
table(jc)
jc
 w <- c('Number of trials with exactly one + imbalance:', sum(ne == 1), '\n',
       'Number of such trials with a - imbalance in additional covariates not\n preceeded by another + imbalance:', nc, '\n',
       'Average number of additional variables examined to find this:',
       round(mean(jc)), '\n')
cat(w, sep='')
###################################################################################################################################################
