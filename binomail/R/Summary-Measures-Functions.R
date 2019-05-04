aux_mean <- function(trials = 0, prob = 0){
  return(trials*prob)
}

aux_variance <- function(trials = 0, prob = 0) {
  return(trials*prob*(1-prob))
}
aux_mode <- function(trials = 0, prob = 0) {
  if ((((trials*prob) + prob)%%1) < 0.5 & (((trials*prob) + prob)%%1) > 0){
    return (round((trials*prob) + prob))
  }
  if ((((trials*prob) + prob)%%1) > 0.5){
    return (round((trials*prob) + prob) - 1)
  }
  if ((((trials*prob) + prob)%%1) == 0){
    x = ((trials*prob) + prob)
    y = ((trials*prob) + prob) - 1
    print(x)
    print(y)
  }
}
aux_skewness <- function(trials = 0, prob = 0){
  return((1-(2*prob))/sqrt(trials*prob*(1 - prob)))
}
aux_kurtosis <- function(trials = 0, prob = 0){
  return((1-6*prob*(1-prob))/(trials*prob*(1 - prob)))
}

