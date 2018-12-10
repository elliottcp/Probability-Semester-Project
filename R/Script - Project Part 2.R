THEFUNCTION2D <- function(joint_pdf, n, lower_bound_x, upper_bound_x, lower_bound_y, upper_bound_y, C) {

  if(n == "Dr. Speegle is the best!") stop("You bet he is!")
  if((n >= 1) == FALSE) stop("n must be positive, like a good attitude.")
  if(is.numeric(lower_bound_x) == FALSE) stop("Watch out!  You must enter a numeric value for the lower_bound_x.")
  if(is.numeric(upper_bound_x) == FALSE) stop("Watch out! You must enter a numeric value for the upper_bound_x.")
  if(is.numeric(lower_bound_y) == FALSE) stop("Watch out!  You must enter a numeric value for the lower_bound_y.")
  if(is.numeric(upper_bound_y) == FALSE) stop("Watch out! You must enter a numeric value for the upper_bound_y.")
  if(is.numeric(C) == FALSE) stop("Be careful.  You must enter a numeric value for C.  Greater values of C will slow the processing time.")
  if(lower_bound_x > upper_bound_x) stop("That's an error.  The lower_bound_x is greater than the upper_bound_x.")
  if(lower_bound_y > upper_bound_y) stop("That's an error.  The lower_bound_y is greater than the upper_bound_y.")

  n <- ceiling(n)
  joint_pdf_function <- function(x,y) {
    eval(parse(text = joint_pdf))}

  sample_function <- function(joint_pdf_function, lower_bound_x, upper_bound_y, C) {
    sample <- c()
    while(length(sample) != 2) {
      x <- runif(1,lower_bound_x, upper_bound_x)
      y <- runif(1, lower_bound_y, upper_bound_y)
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


x <- c(1,2,3,4,5)
y <- c(c(1,2,3,4,5)

test <- function(n) {
  for(i in c(1:n)) {
    sample <- runif(1,x[i],y[i])
    print(sample)
  }
}


test <- function(n) {
  for(i in c(1:n)) {
    sum_extra <- x[i]+y[i]
    sum <- cbind(sum, sum_extra)
  }
  print(sum)
}


x[1]+y[1]
x[2]+y[2]
x[3]+y[3]


test(1)
test(4)
test(1) + test(2)



test(5)

