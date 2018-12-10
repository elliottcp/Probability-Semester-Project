THEFUNCTION2D <- function(pdf, n, lower_bound, upper_bound, C) {

  if(n == "Dr. Speegle is the best!") stop("You bet he is!")
  if((n >= 1) == FALSE) stop("n must be positive, like a good attitude.")
  if(is.numeric(lower_bound) == FALSE) stop("Watch out!  You must enter a numeric value for the lower_bound.")
  if(is.numeric(upper_bound) == FALSE) stop("Watch out! You must enter a numeric value for the upper_bound.")
  if(is.numeric(C) == FALSE) stop("Be careful.  You must enter a numeric value for C.  Higher values of C will slow the processing time.")
  if(lower_bound > upper_bound) stop("That's an error.  The lower_bound is greater than the upper_bound.")

  n <- ceiling(n)
  joint_pdf_function <- function(x,y) {
    eval(parse(text = pdf))}

  sample_function <- function(pdf_function, lower_bound, upper_bound, C) {
    sample <- c()
    while(length(sample) != 1) {
      x <- runif(1,lower_bound, upper_bound)
      y <- runif(1, B, C)
      z <- runif(1, 0, D)
      success <- y < pdf_function(x)
      if(success) {
        print(x,y)
        sample <- append(sample, x)
      }
    }
  }
  replicate(n, sample_function(pdf_function, lower_bound, upper_bound, C))
}
