x <- 2
test <- function(x,...) {
  x*...
}

test(2,3,2)


#joint_pdf can be a function with any x,y,z,...

joint_pdf_function <- function(...) {
  ...
#The above line might be x*2y*z.
}

listofdots <- list(...)

dimensions <- 4
samples_from_each_dimension <- function(dimensions) {
  for(i in c(1:dimensions)) {
    dimensions_sample <- runif(1,lower_bounds[i],upper_bounds[i])
  }
}

x <- 2
x<-2
test <- function(x,...){
  dots <-list(...)
  ndots <- length(dots) + 1
  i<-1
  while (i<ndots){
    x <- x * dots[i]
    i <- i + 1
  }
  return (x)
}

test(2,c(3))
test(2,3,4)
#Doesn't realize that we're trying to multiply by an integer.  Thinks that dots is not a number.  Can't multiply something that's not a number.

