##############################################################
##gini unbiased ##############################################
##############################################################
#https://stat.ethz.ch/pipermail/r-help/2007-June/133875.html
#http://curtis.ml.cmu.edu/w/courses/index.php/Gini_coefficient
#https://github.com/jumanbar/Curso-R/blob/master/misc/gini-index.R

gini_unbiased <- function(x, unbiased = TRUE, na.rm = FALSE){ 
     if (!is.numeric(x)) { 
         warning("'x' is not numeric; returning NA") 
         return(as.numeric(NA)) 
     } 
     if (any(na.ind <- is.na(x))) { 
         if (!na.rm) 
             stop("'x' contain NAs") 
         else 
             x <- x[!na.ind] 
     } 
     n <- length(x) 
     mu <- mean(x) 
     N <- if (unbiased) n*(n - 1) else n*n 
     ox <- x[order(x)] 
     dd <- drop(crossprod(2 * 1:n - n - 1,  ox)) 
     dd / (mu * N) 
}

## example
# library(MASS)
# vec1 <- rnegbin(20,mu=3,theta=0.5) # generate a vector of random abundances from a negative binomial distribution
# gini_unbiased(x = vec1, unbiased = T)

