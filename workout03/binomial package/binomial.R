#title: Probability Checker Function
#description: this will tell if a given value is a proper probability value
#input: the input of this function will be a single "prob" / probability value
#output: the output of this function will either be TRUE or FALSE depending on if it is a valid value

check_prob <- function(prob = -1){
  if (prob > 1 | prob < 0){
    stop('invalid prob value')
  }
  return (TRUE)
}

#title: Trials Checker Function
#description: this will tell if a given value is a proper trial value
#input: the input of this function will be a single "trial" value
#output: the output of this function will either be TRUE or FALSE depending on if it is a valid value
check_trials <- function(trials = -1){
  if (trials < 0){
    stop('invalid trials value')
  }
  return (TRUE)
}


#title: Success Checker Function
#description: this will tell if a given value is a proper success value
#input: the input of this function will be a "success" value and a "trial" value
#output: the output of this function will either be TRUE or FALSE depending on if it is a valid value
check_success <- function(success = 0, trials = -1){
  if (success < 0){
    stop('invalid success value')
  }
  if (success > trials){
    stop('success cannot be greater than trials')
  }
  return (TRUE)
}

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
aux_mean(10,0.3)
aux_variance(10,0.3)
aux_mode(10,0.3)
aux_skewness(10,0.3)
aux_kurtosis(10,0.3)

#' @title: Bin Choose
#' @description: 
#' @param: 
#' @return: 
#' @export:
#' @examples: 
bin_choose <- function(n = 0, k = 0){
  if (k > n) {
    stop("k cannot be greater than n")
  }
  return((factorial(n))/(factorial(k)*factorial(n-k)))
}

bin_choose(n = 5, k = 2)
bin_choose(5, 0)
bin_choose(5, 1:3)

#' @title: Bin Probability
#' @description: 
#' @param: 
#' @return: 
#' @export: 
#' @examples: 
bin_probability <- function(success = 0, trials = 0, prob = 0){
  if (check_prob(prob) == TRUE){
    if (check_trials(trials) == TRUE){
      if(check_success(success, trials) == TRUE){
        return(bin_choose(trials, success)*(prob^success)*((1-prob)^(trials-success)))
      }
    }
  }
}

# probability of getting 2 successes in 5 trials
# (assuming prob of success = 0.5)
bin_probability(success = 2, trials = 5, prob = 0.5)

# probabilities of getting 2 or less successes in 5 trials
# (assuming prob of success = 0.5)
bin_probability(success = 0:2, trials = 5, prob = 0.5)

# 55 heads in 100 tosses of a loaded coin with 45% chance of heads
bin_probability(success = 55, trials = 100, prob = 0.45)

#' @title: Bin Distribution
#' @description: 
#' @param: 
#' @return: 
#' @export
#' @examples: 
bin_distribution <- function(trials, prob){
  success <- c()
  probability <- c()
  for (i in 0:trials){
    success <- append(success, i)
    probability <- append(probability, bin_probability(success = i, trials = trials, prob = prob))
    }
  a <- data.frame(success, probability)
  class(a) <- c("bindis", "data.frame")
  return(a)
}

bin_distribution(trials = 5, prob = .5)

#' @export
plot.bindis <- function(bindis = 0){
  barplot(height = bindis$probability, names.arg = bindis$success, xlab = "successes", ylab = "probability")
}
dis1 <- bin_distribution(trials = 5, prob = 0.5)
plot.bindis(dis1)
#barplot(c(1,2,3), names.arg = c("a", "b", "c"))

#' @title: Bin Cumulative
#' @description: 
#' @param: 
#' @return: 
#' @export: 
#' @examples: 
bin_cumulative <- function(trials = 0, prob = 0){
  success <- c()
  probability <- c()
  cumulative <- c()
  for (i in 0:trials){
    success <- append(success, i)
    probability <- append(probability, bin_probability(success = i, trials = trials, prob = prob))
  }
  cumulative <- cumsum(probability)
  a <- data.frame(success, probability, cumulative)
  class(a) <- c("bincum", "data.frame")
  return(a)
}

bin_cumulative(trials = 5, prob = 0.5)

#' @export
plot.bincum <- function(bincum = 0){
  plot(bincum$success, bincum$cumulative, xlab = "successes", ylab = "probability", type = "o")
}


dis2 <- bin_cumulative(trials = 5, prob = 0.5)
plot(dis2)


#' @title: Bin Variable
#' @description: 
#' @param: 
#' @return: 
#' @export: 
#' @examples: 

bin_variable <- function(trials = trials, prob = prob){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      list("trials" = trials, "prob" = prob)
    }
  }
}

#' @export
print.binvar <- function(){
  
}


  
#' @title: Bin Mean
#' @description: 
#' @param: 
#' @return: 
#' @export: 
#' @examples: 
bin_mean <- function(trials = 0, prob = 0){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      return (aux_mean(trials = trials, prob = prob))
    }
  }
}

#' @title: Bin Variance
#' @description: 
#' @param: 
#' @return: 
#' @export: 
#' @examples: 
bin_variance <- function(trials = 0, prob = 0){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      return (aux_variance(trials = trials, prob = prob))
    }
  }
}

#' @title: Bin Mode
#' @description: 
#' @param: 
#' @return: 
#' @export: 
#' @examples: 
bin_mode <- function(trials = 0, prob = 0){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      return (aux_mode(trials = trials, prob = prob))
    }
  }
}


#' @title: Bin Skewness
#' @description: 
#' @param: 
#' @return: 
#' @export: 
#' @examples: 
bin_skewness <- function(trials = 0, prob = 0){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      return (aux_skewness(trials = trials, prob = prob))
    }
  }
}


#' @title: Bin Kurtosis
#' @description: 
#' @param: 
#' @return: 
#' @export: 
#' @examples: 
bin_kurtosis <- function(trials = 0, prob = 0){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      return (aux_kurtosis(trials = trials, prob = prob))
    }
  }
}
  
bin_mean(10, 0.3)
bin_variance(10, 0.3)  
bin_mode(10, 0.3)
bin_skewness(10, 0.3)
bin_kurtosis(10, 0.3)
