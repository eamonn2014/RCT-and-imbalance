
N=1000
K =100
x <- replicate(N, mean(rnorm(K/2)-mean(rnorm(K/2))))
sd(x)

sqrt(4/K)



####
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
