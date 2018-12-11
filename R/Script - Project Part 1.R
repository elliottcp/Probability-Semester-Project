#' THEFUNCTION1D
#'
#'@description This function produces a random sample for any given continuous, finite probability distribution.
#'
#'@param pdf A probability density function.  The user is responsible for ensuring it is valid.
#'@param n The desired number of samples from the given distribution.
#'@param lower_bound The lower limit of possible sample values.
#'@param upper_bound The upper limit of possible sample values.
#'@param C A real number greater than all possible values of the pdf.
#'@return Random sample in the form of a numeric vector.
#'
#'@export
#'
#'@examples
#'
#'pdf <- function(x) {dnorm(x,0,1)}
#'sample <- THEFUNCTION1D(pdf, n = 100000, lower_bound = -5, upper_bound = 5, C = 0.4)
#'
#'pdf <- function(x) {dunif(x,-10,10)}
#'sample <- THEFUNCTION1D(pdf, n = 200, lower_bound = -10, upper_bound = 10, C = 0.06)
#'
#'If the function produces an error, check for faulty parameters,
#'such as n less than 1, non-numeric lower_bound, non-numeric
#'upper_bound, non-numeric C, or lower_bound greater than the upper_bound.

THEFUNCTION1D <- function(n = 1, pdf, lower_bound = 0, upper_bound = 1, C = 1) {

  if(n == "Dr. Speegle is the best!") stop("You bet he is!")
  if((n >= 1) == FALSE) stop("n must be positive, like a good attitude.")
  if(is.numeric(lower_bound) == FALSE) stop("Watch out!  You must enter a numeric value for the lower_bound.")
  if(is.numeric(upper_bound) == FALSE) stop("Watch out! You must enter a numeric value for the upper_bound.")
  if(is.numeric(C) == FALSE) stop("Be careful.  You must enter a numeric value for C.  Greater values of C will slow the processing time.")
  if(lower_bound > upper_bound) stop("That's an error.  The lower_bound is greater than the upper_bound.")
  n <- ceiling(n)

  sample_function <- function(pdf, lower_bound, upper_bound, C) {
    sample <- c()
    while(length(sample) != 1) {
      x <- runif(1,lower_bound, upper_bound)
      y <- runif(1, 0, C)
      success <- y < pdf(x)
      if(success) {
        return(x)
        sample <- append(sample, x)
      }
    }
  }
  replicate(n, sample_function(pdf, lower_bound, upper_bound, C))
}
