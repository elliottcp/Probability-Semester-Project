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

  joint_pdf_function <- function(...) {
    eval(parse(text = joint_pdf))}

  sample_function <- function(joint_pdf_function, lower_bound_x, upper_bound_y, C) {
    sample <- c()
    while(length(sample) != 1) {
      uniform_sample_function <- function(dimensions) {
        for(i in c(1:dimensions)) {
          sample <- runif(1,x[i],y[i])
          print(sample)
        }
      }
      z <- runif(1, 0, C)
      success <- z < pdf_function(x,y)
      if(success) {
        sample_table <- cbind(x,y)
        return(sample_table)
      }
    }
  }
  replicate(n, sample_function(pdf_function, lower_bound, upper_bound, C))
}

#The parameters of THEFUNCTION2D will allow for sampling from any 2D distribution defined on a rectangle.


x <- c(0,1,45,3,4)
y <- c(1,2,3,4,5)

x<y
if((x<y) == FALSE) "x<y"

dimensions <- length(x)
print(dimensions)

uniform_sample_function <- function(dimensions) {
  for(i in c(1:dimensions)) {
    sample <- runif(1,x[i],y[i])
    print(sample)
  }
}

test_function <- function(x=c(n)){
  if(is.numeric(...) == FALSE) stop("It's not numeric.") else "Nice."
}
test_function(x=c(1,2,3,4,5,6))


is.numeric()
is.vector()

test <- function(n) {
  for(i in c(1:n)) {
    sample <- runif(1,x[i],y[i])
    print(sample)
  }
}

test(5)
