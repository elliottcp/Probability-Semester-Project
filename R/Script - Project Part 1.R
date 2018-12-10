install.packages("devtools")
library(devtools)

THEFUNCTION1D <- function(pdf, n, lower_bound, upper_bound, C) {

  if(n == "Dr. Speegle is the best!") stop("You bet he is!")
  if((n >= 1) == FALSE) stop("n must be positive, like a good attitude.")
  if(is.numeric(lower_bound) == FALSE) stop("Watch out!  You must enter a numeric value for the lower_bound.")
  if(is.numeric(upper_bound) == FALSE) stop("Watch out! You must enter a numeric value for the upper_bound.")
  if(is.numeric(C) == FALSE) stop("Be careful.  You must enter a numeric value for C.  Greater values of C will slow the processing time.")
  if(lower_bound > upper_bound) stop("That's an error.  The lower_bound is greater than the upper_bound.")

  n <- ceiling(n)
  pdf_function <- function(x) {
      eval(parse(text = pdf))
    }

  sample_function <- function(pdf_function, lower_bound, upper_bound, C) {
    sample <- c()
    while(length(sample) != 1) {
      x <- runif(1,lower_bound, upper_bound)
      y <- runif(1, 0, C)
      success <- y < pdf_function(x)
      if(success) {
        return(x)
        sample <- append(sample, x)
      }
    }
  }
  replicate(n, sample_function(pdf_function, lower_bound, upper_bound, C))
}




#Standard Normal distribution:
THEFUNCTION1D(pdf = "1/((2*pi)^0.5)*exp((-x^2)/2)", n = 10, lower_bound = -5, upper_bound = 5, C = 0.4)

#Uniform distribution(a=-10, b=10):
THEFUNCTION1D(pdf = "0.05", n = 20, lower_bound = -10, upper_bound = 10, C = 0.06)

#Exponential(lambda = 7):
THEFUNCTION1D(pdf = "(1/7)*exp((-1/7)*x)", n = 8, lower_bound = 0, upper_bound = 100, C = 50)


#Other examples:

THEFUNCTION1D(pdf = "1/((2*pi)^0.5)*exp((-x^2)/2)", n = 1, lower_bound = -4, upper_bound = 4, C = 0.4)
THEFUNCTION1D(pdf = "x^2", n = 100, lower_bound = -100, upper_bound = 100, C = 1)
THEFUNCTION1D(pdf = "0.1", n = 10, lower_bound = 0, upper_bound = 10, C = 0.2)



?roxygenise





#To-do:
#error test for a custom pdf that's constant.  Run a different set of code then?
#browser function
#make man pages and vignettes.

#Have the user create a function(x) from a gamma, beta, ... distribution.

#Pre-office hours.  Looking to talk about:
#1. allowing the user to enter non-character pdf functions
#-User must make a function(x) before using my package.

#2. I should treat vignettes like any R markdown file, right?
#-Yes.  The vignette will be available when the user enters ?package_name.

#3. If the vignettes are what appear when a user enters ?function, then what are the man pages?
#-The man pages appear with ?function.

#4. Should I try to make a function that produces samples from an n-dimensional joint pdf?
#-No.  I probably could use an ellipsis, but that's likely not the typical use of an ellipsis.

#Will need the @export roxygenise function.
#@param, @examples
#Roxygen comments start with #'.  Each documentation block starts with some text which defines the title, the description, and the details.
#Tell prof. what my username and package is.

?install_github
