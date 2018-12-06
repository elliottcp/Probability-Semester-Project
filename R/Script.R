Dr.Speegle.is.the.best <- function(pdf, n, lower_bound, upper_bound, C) {
  sample <- c()
  pdf <- function(x) {pdf}
  for(i in 1:n){
    success != TRUE
    while(success != TRUE)
      x <- runif(1,lower_bound, upper_bound)
      y <- runif(1,0,C)
      success <- y < pdf(x)
      sample <- if(success) {
      append(sample, x)
  print(sample)}
}

Dr.Speegle.is.the.best(2,10,0,1,5)

#While loops will require a for loop too, at least using Matthew Schuelke's method.
#Read Matt's code.  See how he structures the while loop.  I don't want to write a while loop that depends on the length of the sample vector.This is what's making the function take so long.
#
