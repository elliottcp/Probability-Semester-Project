THEFUNCTION <- function(pdf, n, lower_bound, upper_bound, C) {
  sample <- c()
  pdf_function <- function(x) {
    eval(parse(text = pdf))
  }
  while(length(sample) != n) {
    x <- runif(1,lower_bound, upper_bound)
    y <- runif(1,0,C)
    success <- y < pdf_function(x)
    sample <- if(success) {
      append(sample, x)}}
  print(sample)
}

THEFUNCTION(pdf = "x^2", n = 10.4, lower_bound = 0, upper_bound = 1, C = 1)

Error_Messages <- function(pdf, n, lower_bound, upper_bound, C) {
  if(n == "Dr. Speegle is the best!") stop("He's so cool!")
  n <- ceiling()
  integral_of_support <- integrate(pdf_function, lower_bound, upper_bound)
  if(integral_of_support !=1) stop("The integral of the support is not equal to 1.  Therefore, the pdf is not valid.")

  if((n >= 1) == FALSE) stop("n must be positive, like a good attitude.")
  if(is.numeric(lower_bound) == FALSE) stop("Watch out!  You must enter a numeric value for the lower_bound.")
  if(is.numeric(upper_bound) == FALSE) stop("Error: You must enter a numeric value for the upper_bound.")
  if(is.numeric(C) == FALSE) stop("Be careful.  You must enter a numeric value for C.  Higher values of C will slow the processing time.")
  if(lower_bound > upper_bound) stop("That's an error.  The lower_bound is greater than the upper_bound.")
}

library(FuzzyNumbers.Ext.2)

lower_bound <- 0
upper_bound <- 100
testing_function <- function(x) x^2
test_integral <- integrate(testing_function, lower_bound, upper_bound)

cumulative_test_function <- test()
is.increasing(test_integral, x.bound = c(lower_bound, upper_bound), step = 0.01)


#test... if(is.numeric(x) == FALSE) stop("Warning Message")
#error test for a custom pdf that's constant.  Run a different set of code then?
#make sure the pdf integrates to 1.
#browser function
