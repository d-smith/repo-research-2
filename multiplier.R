multiplier <- function(exp) {
  # default to 1 for things that don't make sense, e.g. -,?,+, etc
  m <- 1
  if(exp == "0") {
    m <- 0
  } else if (exp == "2") {
    m <- 2
  } else if (exp == "3") {
    m <- 3
  } else if (exp == "4") {
    m <- 4
  } else if (exp == "5") {
    m <- 5
  } else if (exp == "6") {
    m <- 6
  } else if (exp == "7") {
    m <- 7
  } else if (exp == "8") {
    m <- 8
  } else if (exp == "B") {
    m <- 1000000000
  } else if(exp == "h" || exp == "H") {
    m <- 100
  } else if(exp == "K") {
    m <- 1000 
  } else if(exp == "m" || exp == "M") {
    m <- 1000000
  }
  
  m
  
}