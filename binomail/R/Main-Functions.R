
#' @title Bin Choose
#' @description after putting in number of successes and trials, this outputs the number of times that a certain number of successes can occur.
#' @param k successes
#' @param n trials
#' @return a single value representing amount of possibilities for k successes
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
bin_choose <- function(n = 0, k = 0){
  if (k > n) {
    stop("k cannot be greater than n")
  }
  return((factorial(n))/(factorial(k)*factorial(n-k)))
}

#' @title Bin Probability
#' @description takes into account successes, trials, and probability in order to relay the probability of achieving a certain binomial distribution.
#' @param success the amount of successes testing for
#' @param trials the amount of trials in the test
#' @param prob the probability that a given success will occur within one trial
#' @return this returns a single value representing the probability of the given parameters inputted.
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability <- function(success = 0, trials = 0, prob = 0){
  if (check_prob(prob) == TRUE){
    if (check_trials(trials) == TRUE){
      if(check_success(success, trials) == TRUE){
        return(bin_choose(trials, success)*(prob^success)*((1-prob)^(trials-success)))
      }
    }
  }
}


#' @title Bin Distribution
#' @description this takes into account trials and probability in order to relay the overall distribution of the probability of success from 0 to the amount of trials.
#' @param trials the amount of trials being tested for
#' @param prob the probability that a success will occur within one trial
#' @return a table of the probability distribution (with classes bindis and data.frame) from 0 successes to the same amount of success as trials.
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = .5)
#' dis1 <- bin_distribution(trials = 5, prob = 0.5)
#' plot.bindis(dis1)
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

#' @export
plot.bindis <- function(bindis = 0){
  barplot(height = bindis$probability, names.arg = bindis$success, xlab = "successes", ylab = "probability")
}


#' @title Bin Cumulative
#' @description this takes into account trials and probability in order to relay the overal distribution of probability of success from 0 to the amount of trails, while also taking into account the cumulative amount of probability used.
#' @param trials the amount of trials the test is occuring
#' @param prob the probability that a success will occur within a trial
#' @return a table, with classes bincum and data.frame, with three columns, the number of successes, the probability of it occuring, and the cumulative probability as number of successes increases.
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
#' dis2 <- bin_cumulative(trials = 5, prob = 0.5)
#' plot(dis2)
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

#' @export
plot.bincum <- function(bincum = 0){
  plot(bincum$success, bincum$cumulative, xlab = "successes", ylab = "probability", type = "o")
}


#' @title Bin Variable
#' @description this takes into account trials and probability in order to output a list which states the number of trials and probability that you entered.
#' @param trials the amount of trials being tested for
#' @param prob the probability that a success will occur within a trial
#' @return a stringed list, with class binvar, of the number of trials and probability of success, while also having several methods in order to print a list of the summary data based on these parameters.
#' @export
#' @examples
#' bin1 <- bin_variable(trials = 10, p = 0.3)
#' binsum1 <- summary(bin1)
#' binsum1

bin_variable <- function(trials = trials, prob = prob){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      a <- list("trials" = trials, "prob" = prob)
      class(a) <- c("binvar")
      return(a)
    }
  }
}

#' @export
print.binvar <- function(binvar){
  cat("\" Binomial Variable\"")
  cat("\n")
  cat("\n Parameters")
  cat("\n - number of trials:", binvar$trials)
  cat("\n - number of success:", binvar$prob)
}

#' @export
summary.binvar <- function(binvar){
  a <- list(trials=binvar$trials, prob = binvar$binvar, 
            mean = aux_mean(binvar$trials, binvar$prob), 
            variance = aux_variance(binvar$trials, binvar$prob), 
            mode = aux_mode(binvar$trials, binvar$prob), 
            skewness = aux_skewness(binvar$trials, binvar$prob), 
            kurtosis = aux_kurtosis(binvar$trials, binvar$prob))
  class(a) <- c("summary.binvar")
  return(a)
}

#' @export
print.summary.binvar <- function(binvar){
  cat("\" Binomial Variable\"")
  cat("\n")
  cat("\n Parameters")
  cat("\n - number of trials:", binvar$trials)
  cat("\n - number of success:", binvar$prob)
  cat("\n")
  cat("\n Measures")
  cat("\n - mean:", binvar$mean)
  cat("\n - variance:", binvar$variance)
  cat("\n - mode:", binvar$mode)
  cat("\n - skewness:", binvar$skewness)
  cat("\n - kurtosis:", binvar$kurtosis)
}

#' @title Bin Mean
#' @description this function finds the mean given a tests number of trials and probability of success.
#' @param trials the amount of trials being tested for
#' @param prob the probability that a success will occur within a trial
#' @return the value of mean given the above parameters
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean <- function(trials = 0, prob = 0){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      return (aux_mean(trials = trials, prob = prob))
    }
  }
}

#' @title Bin Variance
#' @description this function finds the variance given a tests number of trials and probability of success.
#' @param trials the amount of trials being tested for
#' @param prob the probability that a success will occur within a trial
#' @return the value of variance given the above parameters
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance <- function(trials = 0, prob = 0){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      return (aux_variance(trials = trials, prob = prob))
    }
  }
}

#' @title Bin Mode
#' @description this function finds the mode given a tests number of trials and probability of success.
#' @param trials the amount of trials being tested for
#' @param prob the probability that a success will occur within a trial
#' @return the value of mode given the above parameters
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode <- function(trials = 0, prob = 0){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      return (aux_mode(trials = trials, prob = prob))
    }
  }
}


#' @title Bin Skewness
#' @description this function finds the skewness given a tests number of trials and probability of success.
#' @param trials the amount of trials being tested for
#' @param prob the probability that a success will occur within a trial
#' @return the value of skewness given the above parameters
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness <- function(trials = 0, prob = 0){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      return (aux_skewness(trials = trials, prob = prob))
    }
  }
}


#' @title Bin Kurtosis
#' @description this function finds the kurtosis given a tests number of trials and probability of success.
#' @param trials the amount of trials being tested for
#' @param prob the probability that a success will occur within a trial
#' @return the value of kurtosis given the above parameters
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials = 0, prob = 0){
  if (check_trials(trials) == TRUE){
    if (check_prob(prob) == TRUE){
      return (aux_kurtosis(trials = trials, prob = prob))
    }
  }
}

