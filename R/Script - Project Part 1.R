install.packages("devtools")
library(devtools)

THEFUNCTION1D <- function(pdf, n = 1, lower_bound = 0, upper_bound = 1, C = 1) {

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







?roxygenise

#To-do:
#make man pages and vignettes.


#Pre-office hours.  Looking to talk about:


#Will need the @export roxygenise function.
#@param, @examples
#Roxygen comments start with #'.  Each documentation block starts with some text which defines the title, the description, and the details.
#Tell prof. what my username and package is.

?install_github
