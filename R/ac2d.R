
# See 2dautocorr.qmd for creation and examples

# Make the 2d ac ----------------------------------------------------------


ac2d <- function(n_x, n_y, 
                 rho_y = 0, rho_x = 0, 
                 normVar = 1,
                 printStats = FALSE,
                 returnStats = FALSE) {
  # n_x = number of sites along the x-dimension
  # n_y = number of sites along the y-dimension
  # rho_y = desired autocorr in the x direction
  # rho_x = desired autocorr in the y direction
  # normVar = desired variance of the underlying normal distribution
  
  # The goal is a U matrix that is 2d AC, on the normal scale
  
  # make the U matrix as rnorms to initialise
  U <- matrix(rnorm(n_x*n_y)*sqrt(normVar), nrow = n_y)
  
  # Set up the errors for the y process alone
  # generate the errors - set the SD of these (hence the sqrt around the
  # variance)
  a <- rnorm(n_y) * sqrt((normVar*(1-rho_y^2)))
  
  # Make the y ac for the U matrix
  for (i in 1:(n_y-1)) {
    U[i+1, ] <- (rho_y * U[i, ]) + a[i]
  }
  
  # Set up for the x-autocorr, which needs to have errors autocorred in the y-dimension
  
  # first, generate a z error matrix- these are the errors for epsilon, which
  # are in turn the errors for U(t,x).
  # What should var(z) be theoretically?
  varZ <- normVar*(1-rho_y^2)*(1-rho_x^2)
  
  # Make z, adjusting its standard deviation
  # should have 'y' rows
  z <- matrix(rnorm(n_x*n_y), nrow = n_y) * 
    (sqrt(normVar * (1-rho_y^2) * (1-rho_x^2)))
  
  # now let's generate an epsilon matrix
  # These are the errors for x part of the 2d ac process. These errors are
  # themselves autocorrelated in the y dimension.
  vareps <- normVar * (1-rho_x^2)
  eps <- matrix(rnorm(n_x*n_y), nrow = n_y) * sqrt(vareps)
  
  # Now, generate the eps matrix y-autocorrelated (that is, going down rows within each column)
  # eps is already created, so just write into the rows
  for (i in 1:(n_y-1)) {
    eps[i+1, ] <- (rho_y * eps[i, ]) + z[i, ]
  }
  
  # Now, make the U matrix x-autocorrelated
  for (t in 1:(n_x-1)) {
    U[ ,t+1] <- (rho_x * U[ ,t]) + eps[ ,t]
    
  }
  
  # Check the stats if asked
  if (printStats | returnStats) {
    # calc stats in both dimensions
    acstats <- ac2dstats(U)
    
    if (printStats) {
      print(paste0('Mean of all points is ', round(mean(c(U)), 3)))
      print(paste0('Var of all points is ', round(var(c(U)), 3)))
      print(paste0('Mean y AC is ', round(mean(acstats$ac_y), 3)))
      print(paste0('Mean x AC is ', round(mean(acstats$ac_x), 3)))
    }
  }
  
  # usually don't want a list with the stats, and can always get later if needed, I suppose
  if (returnStats) {
    return(tibble::lst(U, acstats))
  } else {
    return(U)
  }
  
}


# Stats calculations ------------------------------------------------------

# 2d ac stats function, useful for calling elsewhere
ac2dstats <- function(acmatrix) {
  # Calculate the autocorrs in both dimensions
  
  # Conditionals on 0 variance are because ar throws an error if there's no variance. Could have set up a try, but this is clearer
  # Using 1 as the ac in that case because with no variance each value is the same as previous and so perfectly correlated. NA would be another option.
  
  # Get the ac in x-dimension: do this for each y (row)
  ac_x <- vector(mode = 'numeric', length = nrow(acmatrix)-1)
  for (i in 1:(nrow(acmatrix)-1)) {
    if (sd(acmatrix[i, ]) == 0) {
      ac_x <- 1
    } else {
      ac_x[i] <- acf(acmatrix[i, ], lag.max = 1, type = 'correlation', plot = FALSE, demean = TRUE)$acf[2]
    }
    
  } 
  
  # Get the ac acorss the stream: do this for each x (column)
  ac_y <- vector(mode = 'numeric', length = ncol(acmatrix)-1)
  for (i in 1:(ncol(acmatrix)-1)) {
    
    if (sd(acmatrix[,i]) == 0) {
      ac_y[i] <- 1
    } else {
      ac_y[i] <- acf(acmatrix[ ,i], lag.max = 1, type = 'correlation', plot = FALSE, demean = TRUE)$acf[2]
    }
    
  } 
  
  return(tibble::lst(ac_y, ac_x))
}
