test_for_valid_pdf <- function(pdf_function, lower_bound, upper_bound) {
  integral_of_support <- integrate(pdf_function, lower_bound, upper_bound)$value
  if(integral_of_support !=1) stop("The integral of the support is not equal to 1.  Therefore, the pdf is not valid.")
  pdf_testing_variable <- lower_bound
  step <- 0.001
  while(pdf_testing_variable < upper_bound) {
    if(pdf_function(pdf_testing_variable) < 0) stop("pdf not valid.  pdf is not positive over the support.")
    pdf_testing_variable <- pdf_testing_variable + step
  }
}

#These next few lines prove that I can't properly or reliably use the test_for_valid_pdf function
#to reject functions that are not valid pdf's.
pdf_function <- function(x) {exp(-x)}
integrate(pdf_function, 0, Inf)$value
test_for_valid_pdf(pdf_function, lower_bound = 0, upper_bound = Inf)
