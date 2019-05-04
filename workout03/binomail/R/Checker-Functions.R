#title: Probability Checker Function
#description: this will tell if a given value is a proper probability value
#input: the input of this function will be a single "prob" / probability value
#output: the output of this function will either be TRUE or FALSE depending on if it is a valid value

check_prob <- function(prob){
  if (prob > 1 | prob < 0){
    stop('invalid prob value')
  }
  return (TRUE)
}

#title: Trials Checker Function
#description: this will tell if a given value is a proper trial value
#input: the input of this function will be a single "trial" value
#output: the output of this function will either be TRUE or FALSE depending on if it is a valid value
check_trials <- function(trials){
  if (trials < 0){
    stop('invalid trials value')
  }
  return (TRUE)
}


#title: Success Checker Function
#description: this will tell if a given value is a proper success value
#input: the input of this function will be a "success" value and a "trial" value
#output: the output of this function will either be TRUE or FALSE depending on if it is a valid value
check_success <- function(success, trials){
  if (success < 0){
    stop('invalid success value')
  }
  if (success > trials){
    stop('success cannot be greater than trials')
  }
  return (TRUE)
}

