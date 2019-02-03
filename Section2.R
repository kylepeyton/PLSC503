### Exercise 3

# Define the function for x > 0
f_x <- function(x){2*exp(-2*x)}

# Integrate over x > 0
integrate(f_x, lower = 0, upper = Inf)


# Sequence from -1.5 to 1.5 by 0.001 increments
x <- seq(-1.5, 1.5, by = 0.001)

# Plot the function
plot(x, f_x(x), type = "l")


### Exercise 4

f_xy <- function(x, y, p = 1/2){
  if(x == 1 & y == 0){
    out <- (1-p)/4
  } else if(x == 2 & y == 0){
    out <- ((1-p)^2)/4
  } else if(x == 3 & y == 0){
    out <- ((1-p)^3)/4
  } else if(x == 4 & y == 0){
    out <- ((1-p)^4)/4
  } else if(x == 1 & y == 1){
    out <- p/4
  } else if(x == 2 & y == 1){
    out <- (p*(1-p))/2
  } else if(x == 3 & y == 1){
    out <- (3*p*(1-p)^2)/4
  } else if (x == 4 &  y == 1){
    out <- p*(1-p)^3
  } else if (x == 2 & y == 2){
    out <- p^2/4
  } else if (x == 3 & y == 2){
    out <- (3*p^2*(1-p))/4
  } else if (x == 4 & y == 2){
    out <- (3*p^2*(1-p)^2)/2
  } else if (x == 3 &  y == 3){
    out <- p^3/4
  } else if (x == 4 & y == 3) {
    out <- p^3*(1-p)
  } else if (x == 4 & y == 4){
    out <- p^4/4
  }
  return(out)
}

f_xy(x = 3, y = 0, p = 0.25)



for(i in 1:4){
  print(f_xy(x = i, y = 0, p = 0.25))
}

f_xy(x = 1:4, y =  0, p = 0.25)

f_xy2 <- Vectorize(f_xy)
f_xy2(x = 1:4, y = 0, p = 0.25)


# Calculate probabilities for all event pairs (x,y) 
probs <- c(f_xy2(x = 1:4, y = 0, p = 0.25),
           f_xy2(x = 1:4, y = 1, p = 0.25),
           f_xy2(x = 2:4, y = 2, p = 0.25),
           f_xy2(x = 3:4, y = 3, p = 0.25),
           f_xy2(x = 4, y = 4, p = 0.25))
probs

# Confirm these sum to 1
sum(probs)
