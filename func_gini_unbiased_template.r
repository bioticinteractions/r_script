######################################################################################################
## gini coefficient unbiased
######################################################################################################
# R Code to plot Lorenz curves and calculate the associated statistics with examples
# Hannah Buckley, Ecology Department, Lincoln University, New Zealand
# Hannah.Buckley@lincoln.ac.nz
# July, 2012
######################################################################################################
######################################################################################################
## This code writes the function to plot the Lorenz curves and calculate the following statistics
## Biased and unbiased forms of the Gini coefficient and the Lorenz asymmetry coeffcient as described
## by Damgaard, C. and Weiner, J. 2000. Describing inequality in plant size or fecundity. Ecology 81, 1139-1142.
## It requires a vector of abundances for one sample or a matrix (or data frame) of 
## abundances for a set of species from a range of samples (samples must be rows and 
## species are columns)
######################################################################################################

lorenz <- function(g.dat){   

if(class(g.dat)=="numeric") { g.dat <- t(matrix(g.dat)) }  # converts vectors into the right format for code

# Plot Lorenz curves
# y = cumulative percent abundance
# x = cumulative percent individuals
plot(0,0,ylim=c(0,100),xlim=c(0,100),type="n",xlab="Cumulative percent species",ylab="Cumulative percent abundance")     # plots an empty graph
    abline(1,1)
for (i in 1:length(g.dat[,1])) {
  x <- as.numeric(g.dat[i,])
  x <- x[x>0]                      # Cuts out species that were absent from the sample
  ox <- x[order(x,decreasing=T)]
  ab <- c(0,cumsum(ox/sum(ox)*100))
  sp <- numeric(length=length(ox))
  for (j in 1: length(ox)) {
    sp[j] <- (j/length(ox))*100
    }
  sp <- c(0,sp)
  lines(ab~sp,type="b")
  }

# Gini coefficient from equations in the Damgaard and Weiner (2000) paper
gini.coef.biased <- numeric(length = length(g.dat[,1])) # empty vector to put the coefficient in
gini.coef.unbiased <- gini.coef.biased                  # second empty vector for unbiased version
for (i in 1:length(g.dat[,1])) {                        # open loop to go through each OTU (species)
  x <- as.numeric(g.dat[i,])                            # put species data in a vector, x
  x <- x[x>0]                                           # remove all absent species
  ox <- x[order(x)]                                     # sort species by abundance value
  a <- numeric(length = length(ox))                     # a = total number of species
    for (j in 1:length(ox)) {                           # within each sample loop through each abundance value
      a[j] <- (2*j-length(ox)-1)*ox[j]                  # inside brackets of eqn 3 in Damgaard and Weiner 2000
      }
  gini.coef.biased[i] <- sum(a)/((length(ox)^2)*mean(ox))                   # biased
  gini.coef.unbiased[i] <- gini.coef.biased[i]*(length(ox)/(length(ox)-1))  # unbiased
    }
    
# Asymmetry coefficient
lac <- numeric(length = length(g.dat[,1]))
for (i in 1:length(g.dat[,1])) {
  y <- g.dat[i,]
  vec <- sort(y[y>0])
  n <- length(vec)
  mu <- mean(vec)
  red <- vec[vec<mean(vec)]                 # generate reduced vector of abundances less than the mean
  m <- length(red)                          # number of abundances less than the mean
  delta <- (mu - vec[m])/(vec[m+1]-vec[m])  # calculate delta from Damgaard and Weiner (2000)
  f <- (m + delta)/n                        # calculate F(mu)
  Lm <- sum(red)
  Ln <- sum(vec)
  L <- (Lm+delta*vec[m+1])/Ln               # calculate L(mu)
  lac[i] <- f+L
} # end of lac loop
results <- cbind(gini.coef.biased,gini.coef.unbiased,lac)
return(results)
} # end of function

##########################################################################
## Examples
##########################################################################
library(MASS) # required for the rnegbin function and the waders dataset

####### Example vector
vec1 <- rnegbin(20,mu=3,theta=0.5) # generate a vector of random abundances from a negative binomial distribution### DOESNT WORK!!
vec1 <- as.data.frame(rnegbin(20,mu=3,theta=0.5)) # generate a vector of random abundances from a negative binomial distribution
####### Example matrices
mat1 <- matrix(c(1,1,2,1,1,1,1,1,2,3,4,5),nrow=2,ncol=6,byrow=T) # test data
mat1 <- matrix(c(1,1,2,1,1,1,1,1,2,3,4,5),nrow=1,byrow=T) # test data
mat2 <- matrix(rep(c(0.678707,0.678707,2.06314,0.443171,197.766,0.121475,0.678707,0.000243745,0.626015,0.734366,129.633,70.9617,8.10685,0.059587,0.00652025,40.1964,0.0203569),2),nrow=2,ncol=34,byrow=T)