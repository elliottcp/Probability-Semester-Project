THEFUNCTION <- function(pdf, n, lower_bound, upper_bound, C) {
  sample <- c()
  pdf_function <- function(pdf,x) {
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

THEFUNCTION(pdf = "x^2", n = 10, lower_bound = 0, upper_bound = 1, C = 1)


#test... if(is.numeric(x) == FALSE) stop("Warning Message")
#error test for a custom pdf that's constant.  Run a different set of code then?
#make sure the pdf integrates to 1.
#browser function
