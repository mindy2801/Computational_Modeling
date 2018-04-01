raw_data <- read.table("ra_exampleData.txt", header=T)
source("ra_prospect.R")

subj_list = unique(raw_data$subjID) 
N = length(subj_list)  # number of subjects
T = length(raw_data[raw_data$subjID==2,"outcome"]) # number of trials per subject

bound_low <- c(0, 0, 0); bound_up <- c(2, 5, 10)
par_init <- runif(3)*bound_up
rho <- par_init[1]; tau <- par_init[2]; lambda <- par_init[3]

# for a single subject=2
sub2_data <- raw_data[raw_data$subjID==2,]
mle_model <- optim(par_init, ra_prospect, T=T, data=sub2_data, method="L-BFGS-B", lower=bound_low, upper=bound_up)

  #iteration
  for (i in 1:100){
    par_init <- runif(3)*bound_up
    temp_model <- optim(par_init, ra_prospect, T=T, data=sub2_data, method="L-BFGS-B", lower=bound_low, upper=bound_up)
  
    if(temp_model$value < mle_model$value) mle_model <- temp_model
    
  }

  # calculate AIC and BIC
  k <- 3
  N <- 140 #IS IT CORRECT?
  sub2_AIC <- 2*mle_model$value + 2*k
  sub2_BIC <- 2*mle_model$value + k*log(N)
  
# for every subject. NEED TO CHECK FROM HERE

allsub_m1_pars <- c()
allsub_m1_value <- c()
  
    
  # loop for subject
  for (subj in subj_list){
    
    par_init <- runif(3)*bound_up
    data <- raw_data[raw_data$subjID==subj,]
    m1 <- optim(par_init, ra_prospect, T=T, data=data, method="L-BFGS-B", lower=bound_low, upper=bound_up)
    allsub_m1_pars <- m1$pars
    allsub_m1_value <- m1$value
    
    #loop for itteration
    for (i in 1:100){
      par_init <- runif(3)*bound_up
      temp_m1 <- optim(par_init, ra_prospect, T=T, data=data, method="L-BFGS-B", lower=bound_low, upper=bound_up)
      
      if(temp_m1$value < m1$value){
        allsub_m1_pars <- temp_m1$pars
        allsub_m1_value <- temp_m1$value
        } 
      
    }
  }

allsub_AIC <- c()
allsub_BIC <- c()

for (subj in subj_list){

allsub_AIC <- c(allsub_AIC, 2*allsub_m1_value + 2*k)
allsub_BIC <- c(allsub_BIC, 2*allsub_m1_value + k*log(N))
}



### ra_noLA
allsub_m2_pars <- c()
allsub_m2_value <- c()

# loop for subject
for (subj in subj_list){
  
  par_init <- runif(2)*bound_up[1:2]
  data <- raw_data[raw_data$subjID==subj,]
  m2 <- optim(par_init, ra_noLA, T=T, data=data, method="L-BFGS-B", lower=bound_low[1:2], upper=bound_up[1:2])
  
  #loop for itteration
  for (i in 1:100){
    par_init <- runif(2)*bound_up[1:2]
    temp_m2 <- optim(par_init, ra_noLA, T=T, data=data, method="L-BFGS-B", lower=bound_low[1:2], upper=bound_up[1:2])
    
    if(temp_m2$value < m2$value){
      allsub_m2_pars <- temp_m2$pars
      allsub_m2_value <- temp_m2$value}
    
  }
}

k = 2
N = 140
allsub_AIC2 <- c()
allsub_BIC2 <- c()

for (subj in subj_list){
  allsub_AIC2 <- c(allsub_AIC2, 2*allsub_m2_value + 2*k)
  allsub_BIC2 <- c(allsub_BIC2, 2*allsub_m2_value + k*log(N))
}

###