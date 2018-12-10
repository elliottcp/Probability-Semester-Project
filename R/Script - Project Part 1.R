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
  if(is.character(pdf) == TRUE) {
    pdf_function <- function(x) {
      eval(parse(text = pdf))
    }
  }
  if(is.character(pdf) != TRUE) {
    pdf_function <- function(x, ...) {
      pdf(x, ...)
    }
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

#Gamma()
THEFUNCTION1D(pdf = dgamma(x,1,1), n = 8, lower_bound = 0, upper_bound = 1, C = 10)


pdf_function <- function(x,...) {
  dbeta(x, ...)
}

x <- c(1)
THEFUNCTION1D(pdf = dbeta(x,1,1), n = 8, lower_bound = 0, upper_bound = 1, C = 10)


test <- function(x) {x^2}

?dbeta

pdf_function(1)

THEFUNCTION1D(pdf = "1/((2*pi)^0.5)*exp((-x^2)/2)", n = 1, lower_bound = -4, upper_bound = 4, C = 0.4)
THEFUNCTION1D(pdf = "x^2", n = 100, lower_bound = -100, upper_bound = 100, C = 1)
THEFUNCTION1D(pdf = "0.1", n = 10, lower_bound = 0, upper_bound = 10, C = 0.2)

is.character("hey")
is.character(45)

#To-do:
#error test for a custom pdf that's constant.  Run a different set of code then?
#browser function
#make man pages and vignettes.
