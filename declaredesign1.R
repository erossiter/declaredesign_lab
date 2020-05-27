# You'll need these packages installed:
#install.packages(c("DeclareDesign", "fabricatr", "randomizr", "estimatr", "DesignLibrary"))
#install.packages(c("plyr", "blockTools"))


# A few things about the Declare Design paradigm before we start
# 
# * The hardest part is speaking their language (framework & matching syntax)
#   (This following is summarized from their intro here: https://declaredesign.org/mida/)
#   
#   The design has 4 general components
#    
#   M - your model
#   I - your inquiry
#   D - your data strategy
#   A - your answer strategy
#   
#   ... You should "declare" the details of each of these
#     
#   Then you diagnose your design --- simulate the design lots of times to calculate "diagnosands,"
#   statistically properties of the design
#   
# * What does the DeclareDesign package do?
#   - provides the "structure" of functions for each step of a research design (e.g., treatment assignment)
#     - you fill in the specifics for *your* design
#   - simulates the design (calls the functions over and over...) to calculate diagnosands
#   - easy to "redesign" (e.g., vary N or ATE)
#   - easy-ish to customize entire step (e.g., your own sampling process)



# Example 1
# Simple design from their website, with slight changes
# Two arm trial
# https://declaredesign.org/getting-started/

rm(list = ls())
library(DeclareDesign)

# M: model -----

# a 100-unit population with an unobserved shock 'u'
N <- 100
population <- declare_population(N = N, u = rnorm(N))
  
# two potential outcomes, Y_Z_0 and Y_Z_1
# Y_Z_0 is the control potential outcome (what would happen if the unit is untreated)
#   it is equal to the unobserved shock 'u'
# Y_Z_1 is the treated potential outcome 
#   it is equal to the control potential outcome plus a treatment effect of .25
ate <- .25
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + ate)


# I: inquiry -----

# we are interested in the average treatment effect in the population (PATE)
estimand <- declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0))


# D: data strategy -----
  
# sampling: we randomly sample half of the units in the population
sampling <- declare_sampling(prob = .5)

# assignment: we randomly assign half of the sampled units to treatment (half to control)
assignment <- declare_assignment(prob = 0.5)

  
# reveal outcomes: construct outcomes from the potential outcomes named Y depending on 
#   the realized value of their assignment variable named Z
reveal_Y <- declare_reveal(outcome_variables = Y, assignment_variables = Z)


# A: answer strategy -----
  
# calculate the difference-in-means of Y depending on Z 
# we link this estimator to PATE because this is our estimate of our inquiry
estimator <- declare_estimator(Y ~ Z, model = difference_in_means, estimand = "PATE")


# The design! -----

# Link functions together (order matters!)
simple_design <- population + potential_outcomes + estimand + sampling + assignment + reveal_Y + estimator


# Check it out -----

dat <- draw_data(simple_design, start = 1, end = 2)
draw_estimates(simple_design)


# Diagnosis  -----

diagnosis <- diagnose_design(simple_design, sims = 500)
diagnosis


# Redesign  -----

new_design1 <- redesign(simple_design, N = seq(100, 500, 100))
new_diagnosis1 <- diagnose_design(new_design1, sims = 500)
new_diagnosis1


# Redesign  -----

new_design2 <- redesign(simple_design, N = seq(100, 500, 100), ate = c(.25, 1))
new_diagnosis2 <- diagnose_design(new_design2, sims = 100)
new_diagnosis2
