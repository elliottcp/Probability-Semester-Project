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

THEFUNCTION("x^2",10,1,5,1)

test <- function(pdf,x) {
  pdf = "x^3"
  eval(parse(text = pdf))
  }

test(x = 2)

pdf_function <- function(pdf,x) {
    eval(parse(text = pdf))
    }

pdf_function(pdf = "x^2", x = 5)
