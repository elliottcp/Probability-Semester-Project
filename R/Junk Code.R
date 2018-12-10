#While loops will require a for loop too, at least using Matthew Schuelke's method.
#Read Matt's code.  See how he structures the while loop.  I don't want to write a while loop that depends on the length of the sample vector.This is what's making the function take so long.
#


add <- function(a,b) {
  add <- sum(a,b)
  return(add)
}

sum(2,3)



is.numeric()
is.vector()

test <- function(n) {
  for(i in c(1:n)) {
    sample <- runif(1,x[i],y[i])
    print(sample)
  }
}

test(5)





test <- function(n) {
  for(i in c(1:n)) {
    sum_table <- cbind(x[i])
    print(sum_table)
  }
}


test(1)
test(4)
test(5)






test <- function(n) {
  for(i in c(1:n)) {
    sum <- sum(x[i], y[i])
    print(sum)
  }
}

test(4)



HelloWorld <- function(...) {
  arguments <- c(...)
  sum(arguments)
}

HelloWorld(1,2,3)

test <- function(...) {
  sample_max <- c(...)
  for(i in 1:n)
    sample_table <- cbind(sample_dimension_1, sample_dimension_2)
  return(sample_table)
}


THEFUNCTION <- function(pdf, n, lower_bound, upper_bound, C) {

  n <- ceiling(n)
  sample <- c()
  pdf_function <- function(x) {
    eval(parse(text = pdf))}

  sample_function <- function(pdf_function, sample, lower_bound, upper_bound) {
    while(length(sample) != 1) {
      x <- runif(1,lower_bound, upper_bound)
      y <- runif(1,0,C)
      success <- y < pdf_function(x)
      sample <- if(success) {
        append(sample, x)}
    }
  }

  if(n == "Dr. Speegle is the best!") stop("You bet he is!")
  if((n >= 1) == FALSE) stop("n must be positive, like a good attitude.")
  if(is.numeric(lower_bound) == FALSE) stop("Watch out!  You must enter a numeric value for the lower_bound.")
  if(is.numeric(upper_bound) == FALSE) stop("Watch out! You must enter a numeric value for the upper_bound.")
  if(is.numeric(C) == FALSE) stop("Be careful.  You must enter a numeric value for C.  Higher values of C will slow the processing time.")
  if(lower_bound > upper_bound) stop("That's an error.  The lower_bound is greater than the upper_bound.")
  )

replicate(n, sample_function)
}



integral_of_support <- integrate(pdf_function, lower_bound, upper_bound)$value
print(integral_of_support)
str(integral_of_support$value)
if(integral_of_support !=1) stop("The integral of the support is not equal to 1.  Therefore, the pdf is not valid.")


pdf_testing_variable <- -2
upper_bound <- 10

while(pdf_testing_variable < upper_bound) {
  if(pdf_function(pdf_testing_variable) < 0) stop("pdf not valid.  pdf is not positive over the support.")
  pdf_testing_variable <- pdf_testing_variable + step
}




pdf_function <-function(x) {x^3}
lower_bound <- 0
upper_bound <- 10
test <- -2
while(test < 5){
  if(pdf_function(test) < 0) stop("Can't get you off my mind.")
  test <- test + 0.1
}


pdf_function(test) < 0





Test_for_valid_pdf(pdf_function, 0, 3)

test <- !TRUE
if(test == TRUE) "Wow!" else "Heh"

print(testing_function)





Test_for_valid_pdf <- function(pdf, n, lower_bound, upper_bound, C) {
  integral_of_support <- integrate(pdf_function, lower_bound, upper_bound)
  if(integral_of_support !=1) stop("The integral of the support is not equal to 1.  Therefore, the pdf is not valid.")

  testing_function <- function(x) {x}
  integrating_variable <- lower_bound
  integral_step <- 0.001
  while(integrating_variable < upper_bound) {
    test_integral <- function(integrating_variable) {
      integrate(testing_function, lower_bound, integrating_variable)
    }
    next_test_integral <- function(integrating_variable) {
      integrate(testing_function, lower_bound, integrating_variable + integral_step)
    }
    if(test_integral > next_test_integral) break("pdf not valid.  pdf is not positive over the support.") else "pdf is valid."
  }
}





positive_pdf_test <- function() {
  lower_bound <- 0
  upper_bound <- 10
  integral_step <- 0.001
  testing_function <- function(x) {x}
  integrating_variable <- lower_bound
  while(integrating_variable + integral_step < upper_bound) {
    test_integral <- function(integrating_variable) {
      integrate(testing_function, lower_bound, integrating_variable)
    }
    next_test_integral <- function(integrating_variable) {
      integrate(testing_function, lower_bound, integrating_variable + integral_step)
    }
    if(test_integral(integrating_variable) > next_test_integral(integrating_variable)) break("pdf not valid.  pdf is not positive over the support.")
  }
  print("pdf is valid.")
}


test_integral <- function(integrating_variable) {
  integrate(testing_function, lower_bound, integrating_variable)
}
next_test_integral <- function(integrating_variable) {
  integrate(testing_function, lower_bound, integrating_variable + integral_step)
}

test_integral()






cumulative_numeric_integral <- c()
cumulative_numeric_integral <- append(cumulative_numeric_integral, test_integral(x))


test <- c(1:10)
is.increasing(test)

if(test_integral(x)-test)

  is.increasing(fun = test_integral, x.bound = c(lower_bound, upper_bound), step = 0.01)



pdf = "exp(-x)"
pdf_function <- function(x) {
  eval(parse(text = pdf))
}
pdf_function(2)
lower_bound <- 0
upper_bound <- 1
integrate(pdf_function, lower_bound, upper_bound)


testing <- function(pdf) {eval(parse(text(pdf = "x^2")))}
testing(2)
pdf_function(3)

integrate(pdf_function, 0, 10)

pdf_function(pdf = "x^2",x)


test <- function(x) {x^2}
integrate(test, 0, 100)
pdf <- function(x) {x^2}
integrate(pdf_function, 0, 100)

is.integer(11)
is.numeric(11)
is.wholenumber(55)

runif(1,2,1)



test <- function(pdf,x) {
  pdf = "x^3"
  eval(parse(text = pdf))
}

test(x = 2)

pdf_function <- function(pdf,x) {
  eval(parse(text = pdf))
}

pdf_function(pdf = "x^2", x = 5)



THEFUNCTION <- function(pdf, n, lower_bound, upper_bound, C) {
  sample <- c()
  pdf <- function(x) {pdf}
  while(length(sample) != n) {
    x <- runif(1,lower_bound, upper_bound)
    y <- runif(1,0,C)
    success <- y < expression(x)
    sample <- if(success) {
      append(sample, x)}}
  print(sample)
}

THEFUNCTION(x^2,10,0,1,5)


expression <- function(pdf,x) {
  pdf <- function(x) {
    eval(parse(text = "pdf"))
  }
}



THEFUNCTION <- function(pdf, n, lower_bound, upper_bound, C) {
  sample <- c()
  pdf <- function(x) {pdf}
  while(length(sample) != n) {
    x <- runif(1,lower_bound, upper_bound)
    y <- runif(1,0,C)
    success <- y < pdf(x)
    sample <- if(success) {
      append(sample, x)}}
  print(sample)
}



THEFUNCTION <- function(pdf, n, lower_bound, upper_bound, C) {
  sample <- c()
  pdf <- function(x) {pdf}
  success != TRUE
  while(success != TRUE)
    x <- runif(1,lower_bound, upper_bound)
  y <- runif(1,0,C)
  success <- y < pdf(x)
  sample <- if(success) {
    append(sample, x)}
  break
  print(sample)
}


The_Grand_Function <- function(pdf, n, lower_bound, upper_bound, C) {
  sample = c()
  pdf <- function(x) {pdf}
  while(length(sample) != n)
    x <- runif(1,lower_bound, upper_bound)
    y <- runif(1,0,C)
    sample <- if(y < pdf(x)) {
    append(sample, x)}
  print(sample)
}


The_Grand_Function <- function(pdf, n, lower_bound, upper_bound, C) {
  sample = c()
  pdf <- function(x) {pdf}
  while(length(sample) != n)
    x <- runif(1,lower_bound, upper_bound)
  y <- runif(1,0,C)
  sample <- if(y < pdf(x)) {
    append(sample, x)
    print(sample)}
}

The_Grand_Function(2,10,0,1,5)

#While loops will require a for loop too, at least using Matthew Schuelke's method.





The_Grand_Function <- function(pdf, n, lower_bound, upper_bound, C) {
  sample = c()
  pdf <- function(x) {pdf}
  while(length(sample) != n)
    sample <- if((y=runif(1,0,C)) <= ((x=runif(1,lower_bound, upper_bound)))
                 append(sample, x)
                 print(sample)
}


The_Grand_Function <- function(pdf, n, lower_bound, upper_bound, C) {
  sample = c()
  pdf <- function(x) {pdf}
  while(length(sample) != n)
    sample <- if((y=runif(1,0,C)) <= pdf((x=runif(1,lower_bound, upper_bound)))
      append(sample, x)
      print(sample)
      }

The_Grand_Function(x^2,10,0,1,1)

pdf <- function(x) {x*300}
y <- 2
y <= pdf(x = runif(1,0,1))

?numeric

pdf <- function(x) {x^2}
pdf(2)
y<=pdf(2)



n <- 6
C <- 1
lower_bound <- 10
upper_bound <- 22
sample <- c()
x <- runif(1, min = lower_bound, max = upper_bound)
y <- runif(1, min = 0, max = C)

while(length(sample) != n)
  sample <- {if(y <- runif(1, min = 0, max = C) <= pdf(x <- runif(1, min = lower_bound, max = upper_bound)) append(sample, x)}

test <- function(x,y,n) {
  sample = c()
  while(length(sample) != n) sample <- if(y<x) append(sample,x)
  print(sample)
}

test(2,0,10)

y <- 0
x <- 2
sample <- c()
while(length(sample) != n) if(y<x) append(sample,x)
sample


while(length(sample) != n) {sample <- if((y = runif(1, min = 0, max = 1))>0.5) append(sample,y)}
print(sample)

something <- c(1,2)
something <- append(something,789)
print(something)

x <- runif(1, min = lower_bound, max = upper_bound)
y <- runif(1, min = 0, max = C)
if(y <= pdf(x)) append(sample, x)
print(sample)
}

#The following code produces the following vector: c(1,2,3,4,5,1)
n <- 5
test <- c(3)
while(length(test)!= n) test <- if(1+1==2) append(test,test[length(test)]+1)
print(test)

length(test)
1+1==2

n <- 4
test_vector <- c()
success <- length(test_vector) == n


test <- function(x) {while(x!=3) return(x+1)}
test(0)

while(!success) {
  x <- runif(1, min = lower_bound, max = upper_bound)
  y <- runif(1, 0, C)
  if(y <= pdf(x)) return(x)
}

#Define the pdf function.
#Produce a vector with the while loop.

sample <- c()
while(!success) {
  x <- runif(1, min = lower_bound, max = upper_bound)
  y <- runif(1, min = 0, max = C)
  if(y <= pdf(x)) append(sample, x)
  print(sample)
}









for (i in 1:n) {
  while(!success)
    runif(1, min = lower_bound, max = upper_bound)
  runif(1, 0, C)

}


for (year in c(2010,2011,2012,2013,2014,2015)){
  print(paste("The year is", year))
}

runif(10,0,C)

?runif

?while

pdf = function(x) {3*(x^2)-2*(x^3)}

my_F <- function(x) {pdf}

my_inverse <- function(y, my_F) {
  x <- seq(0, 1, length.out = 500)
  ys <- my_F(x)
  x[min(which(ys > y))]
}

my_inverse(0.99999,my_F)
my_inverse(1,my_F)


sequence <- seq(0,0.999999999999, length.out = 250)
head(sequence)

x_values <- sapply(sequence , function(y) my_inverse(y , my_F))
y_values <- my_F(x_values)

hist(x_values)
plot(y_values~x_values)

