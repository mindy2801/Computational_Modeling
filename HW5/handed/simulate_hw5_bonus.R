##BONUS1
#Simulation parameters
seed <- 08826    # do not change the seed number!
num_subjs  <- 30 # number of subjects
num_trials <- 100 # number of trials per subject
pr_correct_option1 <- 0.5  # reward probability in option 1
pr_correct_option2 <- 0.8  # reward probability in option 2

# Set seed
set.seed(seed)   # always set a seed number for this homework!

# True parameters 
simul_pars = data.frame(alpha_pos = rnorm(num_subjs, 0.20, 0.08),
                        alpha_neg = rnorm(num_subjs, 0.30, 0.10),
                        beta = rnorm(num_subjs, 2.00, 0.70),
                        subjID  = 1:num_subjs) 

# For storing simulated choice data for all subjects
all_data <- NULL

for (i in 1:num_subjs) {
  # Individual-level (i.e. per subject) parameter values
  alpha_pos <- simul_pars$alpha_pos[i]
  alpha_neg <- simul_pars$alpha_neg[i]
  beta <- simul_pars$beta[i]
  
  # geneate payoff structure for each subject
  # Defaults for the two options 
  # option 1: 50% win (+1), 50% loss (-1)
  # option 2: 80% win (+1), 20% loss (-1)
  payoff_option1 = rbinom(size=1, n = num_trials, prob = pr_correct_option1)
  payoff_option2 = rbinom(size=1, n = num_trials, prob = pr_correct_option2)
  
  # Replace 0 with -1
  payoff_option1[payoff_option1 == 0] = -1   # if 0 --> replace it with -1
  payoff_option2[payoff_option2 == 0] = -1   # if 0 --> replace it with -1
  payoff_both = data.frame(payoff_option1, payoff_option2)
  
  # For storing simulated data for current subject
  # subjID = subject ID
  # trial = trial number
  # choice = choice made on each trial (1 or 2)
  # outcome = outcome reveived on each trial (1 or -1)
  tmp_data = data.frame( subjID=NULL, trial=NULL, choice=NULL, outcome=NULL)
  
  # initialize some variables
  sv = c(0, 0)  # stimulus value of two options
  
  for (t in 1:num_trials)  {
    # Prob of choosing option 2
    prob_choose2 = 1 / (1 + exp(beta * (sv[1] - sv[2])))  # exploration/exploitation parameter is set to 1
    
    # choice
    choice = rbinom(size=1, n = 1, prob = prob_choose2 )
    choice = choice + 1  # 0 or 1 --> 1 (option 1) or 2 (option 2)
    
    # outcome
    outcome = payoff_both[t, choice]
    
    # after receiving outcome (feedback), update sv[t+1]
    # prediction error (PE)
    PE = outcome - sv[choice]
    
    # update stimulus value (sv) of the chosen option
    if (PE >= 0){
      sv[choice] = sv[choice] + alpha_pos * (outcome - sv[choice] )
    } else{
      sv[choice] = sv[choice] + alpha_neg * (outcome - sv[choice] )
    }
    
    
    # append simulated task/response to subject data
    tmp_data[t, "subjID"] = i
    tmp_data[t, "trial"] = t
    tmp_data[t, "choice"] = choice
    tmp_data[t, "outcome"] = outcome
  } # end of t loop
  # Append current subject with all subjects' data
  all_data = rbind(all_data, tmp_data)
}

# Write out data
write.table(all_data, file = "simul_data_hw5_bonus1.txt", row.names = F, col.names = TRUE, sep = "\t")


##BONUS2
rm(list=ls())

# Simulation parameters
seed <- 08826    # do not change the seed number!
num_subjs  <- 30 # number of subjects
num_trials <- 300 # number of trials per subject
pr_correct_option1 <- 0.3  # reward probability in option 1
pr_correct_option2 <- 0.6  # reward probability in option 2

# Set seed
set.seed(seed)   # always set a seed number for this homework!

# True parameters 
simul_pars = data.frame(alpha_pos = rnorm(num_subjs, 0.20, 0.08),
                        alpha_neg = rnorm(num_subjs, 0.30, 0.10),
                        beta = rnorm(num_subjs, 2.00, 0.70),
                        subjID  = 1:num_subjs) 

# For storing simulated choice data for all subjects
all_data <- NULL

for (i in 1:num_subjs) {
  # Individual-level (i.e. per subject) parameter values
  alpha_pos <- simul_pars$alpha_pos[i]
  alpha_neg <- simul_pars$alpha_neg[i]
  beta <- simul_pars$beta[i]
  
  # geneate payoff structure for each subject
  # Defaults for the two options 
  # option 1: 50% win (+1), 50% loss (-1)
  # option 2: 80% win (+1), 20% loss (-1)
  payoff_option1 = rbinom(size=1, n = num_trials, prob = pr_correct_option1)
  payoff_option2 = rbinom(size=1, n = num_trials, prob = pr_correct_option2)
  
  # Replace 0 with -1
  payoff_option1[payoff_option1 == 0] = -1   # if 0 --> replace it with -1
  payoff_option2[payoff_option2 == 0] = -1   # if 0 --> replace it with -1
  payoff_both = data.frame(payoff_option1, payoff_option2)
  
  # For storing simulated data for current subject
  # subjID = subject ID
  # trial = trial number
  # choice = choice made on each trial (1 or 2)
  # outcome = outcome reveived on each trial (1 or -1)
  tmp_data = data.frame( subjID=NULL, trial=NULL, choice=NULL, outcome=NULL)
  
  # initialize some variables
  sv = c(0, 0)  # stimulus value of two options
  
  for (t in 1:num_trials)  {
    # Prob of choosing option 2
    prob_choose2 = 1 / (1 + exp(beta * (sv[1] - sv[2])))  # exploration/exploitation parameter is set to 1
    
    # choice
    choice = rbinom(size=1, n = 1, prob = prob_choose2 )
    choice = choice + 1  # 0 or 1 --> 1 (option 1) or 2 (option 2)
    
    # outcome
    outcome = payoff_both[t, choice]
    
    # after receiving outcome (feedback), update sv[t+1]
    # prediction error (PE)
    PE = outcome - sv[choice]
    
    # update stimulus value (sv) of the chosen option
    if (PE >= 0){
      sv[choice] = sv[choice] + alpha_pos * (outcome - sv[choice] )
    } else{
      sv[choice] = sv[choice] + alpha_neg * (outcome - sv[choice] )
    }
    
    
    # append simulated task/response to subject data
    tmp_data[t, "subjID"] = i
    tmp_data[t, "trial"] = t
    tmp_data[t, "choice"] = choice
    tmp_data[t, "outcome"] = outcome
  } # end of t loop
  # Append current subject with all subjects' data
  all_data = rbind(all_data, tmp_data)
}

write.table(all_data, file = "simul_data_hw5_bonus2.txt", row.names = F, col.names = TRUE, sep = "\t")

