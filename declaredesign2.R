rm(list = ls())
library(DeclareDesign)

# Replicating Fig 5 in Moore 2012
# http://www.ryantmoore.org/files/papers/blockPA.pdf

# M
N <- 100
population <- declare_population(N = N,
                                 x1 = rnorm(N),
                                 x2 = runif(N),
                                 x3 = rchisq(n = N, df = 2),
                                 u = rnorm(N))
ate <- 1
potential_outcomes <- declare_potential_outcomes(Y ~ ate*Z + 1 + x1 + 2*x2 + 3*x3 + u)

# I
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

# D
assignment_c <- declare_assignment(prob = 0.5) #under complete randomization
assignment_b <- declare_assignment(handler = function(data){

  # multivariate blocking, 2 units per block
  out <- blockTools::block(data = data,
                           n.tr = 2,
                           id.vars = "ID", 
                           block.vars = c("x1", "x2", "x3"),
                           algorithm = "optGreedy",
                           distance = "mahalanobis",
                           vcov.data = NULL)
  
  # data cleaning
  blocks <- data.frame(ID = c(out$blocks$`1`$`Unit 1`, out$blocks$`1`$`Unit 2`),
                       block_id = rep(1:50, 2))

  # add block id as a variable in the data
  data <- plyr::join(data, blocks, by = "ID")
  
  # randomly assign treatment within blocks
  data$Z <-  block_ra(blocks = data$block_id)
  
  return(data)
})
reveal_Y <- declare_reveal()

# A
estimator <- declare_estimator(Y ~ Z, model = difference_in_means)

# Full designs (note: could use replace_step function)
design_c <- population + potential_outcomes + estimand + assignment_c + reveal_Y + estimator
design_b <- population + potential_outcomes + estimand + assignment_b + reveal_Y + estimator

# Check them out
head(draw_data(design_c))
head(draw_data(design_b))


# Diagnosis
diagnosis_c <- diagnose_design(design_c, sims = 1000, bootstrap_sims = 0)
diagnosis_b <- diagnose_design(design_b, sims = 1000, bootstrap_sims = 0)


# (Note: grabbing vector of simulated estimates)
boxplot(diagnosis_c$simulations_df$estimate,
        diagnosis_b$simulations_df$estimate,
        names = c("Complete rand", "Blocked random"),
        ylab = "DIM Estimates")
abline(h = 1)
