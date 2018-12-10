THEFUNCTION2D <- function(joint_pdf, n, lower_bounds, upper_bounds, C) {

  if(n == "Dr. Speegle is the best!") stop("You bet he is!")
  if(is.numeric(lower_bounds) == FALSE) stop("The values within the lower_bounds vector must be numeric.")
  if(is.numeric(upper_bounds) == FALSE) stop("The values within the upper_bounds vector must be numeric.")
  if(is.vector(lower_bounds) == FALSE) stop("lower_bounds must be a vector.")
  if(is.vector(upper_bounds) ==FALSE) stop("upper_bounds must be a vector.")
  n <- ceiling(n)
  if((n >= 1) == FALSE) stop("n must be positive, like a good attitude.")
  if(is.numeric(C) == FALSE) stop("Be careful.  You must enter a numeric value for C.  Greater values of C will slow the processing time.")
  if(length(lower_bounds) != length(upper_bounds)) stop("dimension_lower_bounds must equal dimension_upper_bounds.")
  if(sum(lower_bounds <= upper_bounds) < length(upper_bounds)) stop("lower_bounds values must always be less than upper_bounds values.")

  dimensions <- length(lower_bounds)
  samples_from_each_dimension <- function(dimensions) {
    for(i in c(1:dimensions)) {
      dimensions_sample <- runif(1,lower_bounds[i],upper_bounds[i])
    }
  }
  joint_pdf_function <- function(...) {
    eval(parse(text = joint_pdf))
  }

  valid_sample_function <- function(joint_pdf_function, lower_bounds, upper_bounds, C, dimensions) {
    sample <- c()
    while(length(sample) != dimensions) {
      z <- runif(1, 0, C)
      success <- z < joint_pdf_function(...)
      if(success) {
        sample_table <- cbind()
        return(sample_table)
      }
    }
  }
  replicate(n, valid_sample_function(joint_pdf_function, lower_bounds, upper_bounds, C))
}

#The parameters of THEFUNCTION2D will allow for sampling from any 2D distribution defined on a rectangle.

joint_pdf_function <- function(joint_pdf, ...) {
  eval(parse(text = joint_pdf))
}

x <-

test <- function(x,...) {
  x*...
}

joint_pdf_function("x*y")
