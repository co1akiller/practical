
library(testthat)
prob_y1 <- function(y1, n1, theta) {
  # Calculate the probability of observing y_1 responses in n_1 trials 
  # conditioned on theta
  return(dbinom(y1, n1, theta))
}

test_that("y1 probs sum to one", {
  n1 <- 30
  s <- sum(prob_y1(0:n1, n1,theta=0.5))
  expect_equal(s, 1)
})


evaluate_design_exact <- function(lambda, gamma, n1, n2, theta) {
  # Calculate the expected sample size of a design defined by its 
  # decision rule parameters (lambda, gamma), sample size 
  # parameters (n1, n2) and theta
  
  # Threshold to determine progression, based on the decision rule.
  C1 <- 1 - lambda * (n1 / n2)^gamma
  
  # Vector of possible stage 1 outcomes.
  y_1s <- 0:n1
  
  # Vector of corresponding progression decisions.
  stops <- pbeta(0.5, y_1s + 0.5, n1 - y_1s + 0.5) > C1
  
  # For each outcome, calculate its probability.
  y_1_probs <- prob_y1(y_1s, n1, theta)
  
  sum(n1 * stops * y_1_probs + n2 * (!stops) * y_1_probs)
}



alpha <- function(lambda,gamma,n1,n2,theta){
  # Calculate type I error defined by its 
  # decision rule parameters (lambda, gamma) and sample size 
  # parameters (n1, n2)
  # Set the number of simulations.
  M <- 10^4
  # Create an empty vector to store simulated NS.
  Ns <- rep(NA, M)
  for (i in 1:M) {
    # Simulate theta from its prior, and then the stage 1 data conditional
    # on theta.
    y1 <- rbinom(1, n1, theta)
    
    # Get posterior Beta(a1, b1) parameters.
    a1 <- 0.5 + y1
    b1 <- 0.5 + n1 - y1
    
    # Probability of futility.
    fut1 <- pbeta(0.5, a1, b1)
    
    # Threshold to determine progression, based on the decision rule.
    C1 <- 1 - lambda * (n1 / n2)^gamma
    
    # Note the final total sample size and store in the vector Ns.
    if (fut1 < C1) {
      # Simulate theta from its prior, and then the stage 2 data conditional
      # on theta.
      y2 <- rbinom(1, n2, theta)
      # Get posterior Beta(a1, b1) parameters.
      a2 <- 0.5 + y2
      b2 <- 0.5 + n2 - y2
      # Probability of futility.
      fut2 <- pbeta(0.5, a2, b2)
      # Threshold to determine progression, based on the decision rule.
      C2 <- 1 - lambda * (n2 / n2)^gamma
      if (fut2 < C2) {
        Ns[i] <- TRUE
      } 
      else{
        Ns[i] <- FALSE
      }
    }
    else{
      Ns[i] <- FALSE
    }
  }
  return(mean(Ns))
}


# Type II error
# beta <- 1-alpha(lambda,gamma,n1,n2,theta)
# A grid of values to be evaluated:
to_eval <- expand.grid(lambda = seq(0.1, 1, 0.1),
                       gamma = seq(0.1, 1, 0.1),
                       n1 = seq(10,100,10),
                       n2 = seq(10,100,10))

# For each pair in the grid, find the expected sample size and store in a vector.
exp_n <- rep(NA, nrow(to_eval))

ptm <- proc.time()
# Record calculation time
for(i in 1:nrow(to_eval)) {
  # Restrict the type I error and type II error to be less than alpha=0.05 and beta=0.2
  if (alpha(to_eval[i, 1], to_eval[i, 2], n1 = to_eval[i, 3], n2 =to_eval[i, 4],theta=0.5)<=0.05&
    (1-alpha(to_eval[i, 1], to_eval[i, 2], n1 = to_eval[i, 3], n2 =to_eval[i, 4],theta=0.7))<=0.2){
    exp_n[i] <- evaluate_design_exact(to_eval[i, 1], to_eval[i, 2], n1 = to_eval[i, 3], n2 =to_eval[i, 4],theta=0.5)
    }
}
proc.time() - ptm

# Merge by column 
a <- cbind(to_eval,exp_n) 


# Create a vector with values of theta
theta <- seq(0,1,0.01)
# # For every theta, find the expected sample size and store in a vector.
expectation <- rep(NA,length(theta))

for (i in 1:length(theta)){
  # Compute expected sample size 
  expectation[i] <- evaluate_design_exact(0.8, 0.1, 20, 30,theta[i])
}
# Plot the relationship between theta and expected sample size
plot(theta,expectation,ylab="sample size")



