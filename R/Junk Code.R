#While loops will require a for loop too, at least using Matthew Schuelke's method.
#Read Matt's code.  See how he structures the while loop.  I don't want to write a while loop that depends on the length of the sample vector.This is what's making the function take so long.
#

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

