# Soju.JC, 07 May of 2020
########################################################################
# find the zeros of a*x^2 + b*x + c = 0|Needs to be call with source().
########################################################################

# clear the workspace
rm(list=ls())
# input 
a <- as.numeric(readline("a = "))
b <- as.numeric(readline("b = "))
c <- as.numeric(readline("c = "))
# calculate the discriminant
discrim <- b^2 - 4*a*c
# calculate the roots depending on the value of the discriminant
if (discrim > 0 & a != 0) {
  roots <- c( (-b + sqrt(b^2 - 4*a*c))/(2*a),
              (-b - sqrt(b^2 - 4*a*c))/(2*a))
  cat("Equation solution:", roots)
} else if(discrim == 0 & a != 0){
  roots <- -b/(2*a)
  cat("Both solutions of the equation are the same:", roots)
} else if(discrim < 0){ 
  roots <- c()
  cat(roots, "\n", "The domain is no more the real numbers")
} else if(a == 0 & b != 0){
  roots <- -c/b
  cat("Equation solution:", roots)
} else{
  #(a == 0 & b = 0)
  roots <- c
  cat("This is just a value, not an equation:", roots)
}


