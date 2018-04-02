rm(list=ls())
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
attach(sub2_data)
mle_model <- optim(par_init, ra_prospect, T=T, method="L-BFGS-B", lower=bound_low, upper=bound_up)

#iteration
attach(sub2_data)
for (i in 1:100){
  par_init <- runif(3)*bound_up
  temp_model <- optim(par_init, ra_prospect, T=T, method="L-BFGS-B", lower=bound_low, upper=bound_up)
  
  if(temp_model$value < mle_model$value) mle_model <- temp_model

}
detach(sub2_data)
mle_model$par

# calculate AIC and BIC
k <- 3
N <- 140
sub2_AIC <- 2*mle_model$value + 2*k
sub2_BIC <- 2*mle_model$value + k*log(N)



# for every subject. NEED TO CHECK FROM HERE

allsub_m1 <- list()
par_init <- runif(3)*bound_up

# loop for subject
for (subj in subj_list){
  
  data <- raw_data[raw_data$subjID==subj,]
  attach(data)
  m1 <- optim(par_init, ra_prospect, T=T, method="L-BFGS-B", lower=bound_low, upper=bound_up)

  #loop for itteration
  for (i in 1:50){
    par_init <- runif(3)*bound_up
    temp_m1 <- optim(par_init, ra_prospect, T=T, method="L-BFGS-B", lower=bound_low, upper=bound_up)
    
    if(temp_m1$value < m1$value){allsub_m1[[subj]] <- temp_m1} 

  }
  detach(data)
}



allsub_m1_par <- c()
allsub_m1_value <- c()
for(subj in subj_list){
  allsub_m1_par <- rbind(allsub_m1_par, allsub_m1[[subj]]$par)
  allsub_m1_value <- rbind(allsub_m1_value, allsub_m1[[subj]]$value)
  
}

##AIC and BIC
k <- 3
N <- 140*5
AIC <- 2*colSums(allsub_m1_value) + 2*k;AIC
BIC <- 2*colSums(allsub_m1_value) + k*log(N);BIC

### ra_noLA

allsub_m2 <- list()
# loop for subject
for (subj in subj_list){
  
  par_init <- runif(2)*bound_up[1:2]
  data <- raw_data[raw_data$subjID==subj,]
  attach(data)
  m2 <- optim(par_init, ra_noLA, T=T, method="L-BFGS-B", lower=bound_low[1:2], upper=bound_up[1:2])

  #loop for iteration
  for (i in 1:100){
    par_init <- runif(2)*bound_up[1:2]
    temp_m2 <- optim(par_init, ra_noLA, T=T, method="L-BFGS-B", lower=bound_low[1:2], upper=bound_up[1:2])
    
    if(temp_m2$value < m2$value){allsub_m2[[subj]] <- temp_m2}

  }
  detach(data)
}


allsub_m2_par <- c()
allsub_m2_value <- c()
for(subj in subj_list){
  allsub_m2_par <- rbind(allsub_m2_par, allsub_m2[[subj]]$par)
  allsub_m2_value <- rbind(allsub_m2_value, allsub_m2[[subj]]$value)
  
}


k = 2
N <- 140*5
AIC2 <- 2*colSums(allsub_m2_value) + 2*k;AIC2
BIC2<- 2*colSums(allsub_m2_value) + k*log(N);BIC2


###

