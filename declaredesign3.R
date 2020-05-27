rm(list = ls())
library(DeclareDesign)

# Goal: compare RMSE & power under two designs
# 
# (1) status-quo procedure for assigning ppl to groups/partnerships
# for discussion (often in lab experiments)
#   - randomly assign ppl into partnerships
#   - randomly assign treatment at partnerships level
# 
# (2) a proposed way to improve efficiency in these small-n
# studies using blocking
#   - randomly assign ppl into partnerships, BUT
#   - *block-randomize* treatment at partnerships level
#   - (see diagram)


# M
N <- 500
n_clust <- 100
ate <- 1
population_naive <- declare_population(N = N, handler = function(N){
  # Same data as before, just adding a party indicator
  data <- data.frame(ID = 1:N,
                     party_dummy = sample(x = c("R", "D"), size = N, replace = TRUE, prob = c(.4, .6)),
                     x1 = rnorm(N),
                     x2 = runif(N),
                     x3 = rchisq(n = N, df = 2),
                     u = rnorm(N))
  
  # Loop to assign partners/clusters
  data$partner_id <- NA
  data$cluster_id <- NA
  data$u_c <- NA
  cluster_counter <- 1
  for(i in 1:nrow(data)){
    # if already been assigned, skip
    if(!is.na(data$cluster[i])) next
    
    # index of randomly chosen partner *from people who still need partner*
    poss_partners <- (data$party_dummy != data$party_dummy[i]) & (is.na(data$partner_id))
    if(sum(poss_partners) == 1){
      partner_idx <- which(poss_partners)
    }else if(sum(poss_partners) == 0){
      next
    }
    else{
      partner_idx <- sample(which(poss_partners), 1)
    }
    
    data$partner_id[i] <- data$ID[partner_idx] # fill in partner's unique ID
    data$partner_id[partner_idx] <- data$ID[i] # fill in partner's unique ID
    
    # assign cluster ID
    data$cluster_id[i] <- data$cluster_id[partner_idx] <- cluster_counter
    cluster_counter <- cluster_counter + 1
    
    # cluster-level error
    data$u_c[i] <- data$u_c[partner_idx] <- rnorm(1)
  }
  
  return(data[!is.na(data$cluster_id), ])
})
population_brc <- declare_population(N = N, handler = function(N){
  # Same data as before, just adding a party indicator
  data <- data.frame(ID = 1:N,
                     party_dummy = sample(x = c("R", "D"), size = N, replace = TRUE, prob = c(.4, .6)),
                     x1 = rnorm(N),
                     x2 = runif(N),
                     x3 = rchisq(n = N, df = 2),
                     u = rnorm(N))

  out <- block(data = data,
               n.tr = 2,
               groups = "party_dummy",
               id.vars = "ID", 
               block.vars = c("x1", "x2", "x3"),
               algorithm = "optGreedy",
               distance = "mahalanobis",
               vcov.data = NULL)
  
  # subset down to full pairs
  outD <- out$blocks$D[!is.na(out$blocks$D$`Distance`) & !is.na(out$blocks$D$`Unit 2`),]
  outR <- out$blocks$R[!is.na(out$blocks$R$`Distance`) & !is.na(out$blocks$R$`Unit 2`),]
  
  # Shuffle individuals within their pairs/trios
  outD <- plyr::adply(outD[,1:2], 1, function(x){ x[sample(1:2, 2, replace = F)] })
  outR <- plyr::adply(outR[,1:2], 1, function(x){ x[sample(1:2, 2, replace = F)] })
  
  # Temporary w/in party groups randomly assigned across party to
  # (1)randomize ind-level distance within cross-party pairs, and
  # (2) minimize pair-level distance within blocks
  clusterD_idx <- sample(x = 1:nrow(outD), size = nrow(outR), replace = F)
  paired_df <- data.frame("ID" = numeric(), "block_id" = numeric(), "cluster_id" = numeric(), "u_c" = numeric())
  counter <- 1
  for(i in 1:nrow(outR)){
    # go through temp R groups
    tmp1 <- data.frame(t(outR[i,]))
    colnames(tmp1) <- "ID"
    tmp1$block_id <- i
    tmp1$cluster_id <- counter:(counter+1)
    rownames(tmp1) <- NULL
    
    # randomly choose a temp D group
    tmp2 <- data.frame(t(outD[clusterD_idx[i],]))
    colnames(tmp2) <- "ID"
    tmp2$block_id <- i
    tmp2$cluster_id <- counter:(counter+1)
    rownames(tmp2) <- NULL
    
    tmp <- rbind(tmp1, tmp2)
    tmp$u_c <- NA
    tmp$u_c[tmp$cluster_id == counter] <- rnorm(1)
    tmp$u_c[tmp$cluster_id == (counter+1)] <- rnorm(1)
    
    paired_df <- rbind(paired_df, tmp)
    
    counter <- counter + 2
  }
  
  data <- plyr::join(data, paired_df, by = "ID", type = "right")
  return(data)
})
potential_outcomes <- declare_potential_outcomes(Y ~ ate*Z + x1 + x2 + x3 + u + u_c)

head(population_naive())
head(population_brc())

# I
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

# D
sampling_naive <- declare_sampling(clusters = cluster_id, n = n_clust)
sampling_brc <- declare_sampling(clusters = block_id, n = floor(n_clust/2))

assignment_naive <- declare_assignment(clusters = cluster_id)
assignment_brc <- declare_assignment(blocks = block_id, clusters = cluster_id)

reveal_Y <- declare_reveal()

# A
estimator_naive <- declare_estimator(Y ~ Z, estimand = estimand, model = difference_in_means, clusters = cluster_id)
estimator_brc <- declare_estimator(Y ~ Z, estimand = estimand, model = difference_in_means, clusters = cluster_id, blocks = block_id)

# The designs
design_naive <- population_naive + potential_outcomes + estimand + sampling_naive + assignment_naive + reveal_Y + estimator_naive
design_brc <- population_brc + potential_outcomes + estimand + sampling_brc + assignment_brc + reveal_Y + estimator_brc

# The diagnoses
diagnosands <- declare_diagnosands(select = c("bias", "rmse", "power", "mean_estimate", "sd_estimate", "mean_estimand"))
redesign_naive <- redesign(design_naive, n_clust = seq(20, 100, 20))
redesign_brc <- redesign(design_brc, n_clust = seq(20, 100, 20))
diagnosis_naive <- diagnose_design(redesign_naive, diagnosands = diagnosands, sims = 500)
diagnosis_brc <- diagnose_design(redesign_brc, diagnosands = diagnosands, sims = 500)

#save.image(file = "lab_workspace.Rdata")

# Load & plot results -----------------------------------------------------------------------
rm(list = ls())
load("lab_workspace.Rdata")


# RMSE 
plot(x = diagnosis_naive$diagnosands_df$n_clust,
     y = diagnosis_naive$diagnosands_df$rmse,
     xlim = c(20,100),
     ylim = c(0,1),
     type = "b",
     xlab = "Number of clusters",
     ylab = "RMSE")
lines(x = diagnosis_brc$diagnosands_df$n_clust,
      y = diagnosis_brc$diagnosands_df$rmse,
      type = "b",
      col = "blue")
legend("topleft", legend = c("Naive", "Proposed"), col = c("black", "blue"), lty = 1, bty = "n")

# Power 
plot(x = diagnosis_naive$diagnosands_df$n_clust,
     y = diagnosis_naive$diagnosands_df$power,
     xlim = c(20,100),
     ylim = c(0,1),
     type = "b",
     xlab = "Number of clusters",
     ylab = "RMSE")
lines(x = diagnosis_brc$diagnosands_df$n_clust,
      y = diagnosis_brc$diagnosands_df$power,
      type = "b",
      col = "blue")
legend("topleft", legend = c("Naive", "Proposed"), col = c("black", "blue"), lty = 1, bty = "n")

# Bias mean(estimate - estimand)
plot(x = diagnosis_naive$diagnosands_df$n_clust,
     y = diagnosis_naive$diagnosands_df$bias,
     xlim = c(20,100),
     ylim = c(-.25, .25),
     type = "b",
     xlab = "Number of clusters",
     ylab = "Bias",
     pch = 16)
abline(h = 0, col = "grey", lty = 2)
segments(x0 = diagnosis_naive$diagnosands_df$n_clust,
         y0 = diagnosis_naive$diagnosands_df$bias + 2*diagnosis_naive$diagnosands_df$`se(bias)`,
         y1 = diagnosis_naive$diagnosands_df$bias - 2*diagnosis_naive$diagnosands_df$`se(bias)`,
         lty = 2)
lines(x = diagnosis_brc$diagnosands_df$n_clust,
      y = diagnosis_brc$diagnosands_df$bias,
      col = "blue",
      type = "b",
      pch = 16)
segments(x0 = diagnosis_brc$diagnosands_df$n_clust,
         y0 = diagnosis_brc$diagnosands_df$bias + 2*diagnosis_brc$diagnosands_df$`se(bias)`,
         y1 = diagnosis_brc$diagnosands_df$bias - 2*diagnosis_brc$diagnosands_df$`se(bias)`,
         col = "blue",
         lty = 2)
legend("topleft", legend = c("Naive", "Proposed"), col = c("black", "blue"), lty = 1, bty = "n")



# Msc... did my blocked, randomized cluster algorithm do its job? -----------------------------------

set.seed(406)
dat_naive <- draw_data(design_naive)
dat_brc <- draw_data(design_brc)

# Individual-level differences in x1 within partnerships (want to be random)
summary(tapply(dat_naive$x1, dat_naive$cluster_id, function(x) abs(diff(x))))
summary(tapply(dat_brc$x1, dat_brc$cluster_id, function(x) abs(diff(x))))

# Partnership-level differences in x1 within blocks (want to be minimized)
block_diff_func <- function(dat){
  within_b_diffs <- rep(NA, length(unique(dat$block_id)))
  for(i in 1:length(within_b_diffs)){
    b_id <- unique(dat$block_id)[i]
    c_ids <- unique(dat$cluster_id[dat$block_id == b_id])
    partners_1 <- dat$x1[dat$block_id == b_id & dat$cluster_id == c_ids[1]]
    partners_2 <- dat$x1[dat$block_id == b_id & dat$cluster_id == c_ids[2]]
    within_b_diffs[i] <- abs(abs(diff(partners_1)) - abs(diff(partners_2)))
  }
  return(within_b_diffs)
}

# proposed design
within_b_diffs_brc <- block_diff_func(dat_brc)

# ... relative to random blocks
dat_naive <- dat_naive[order(dat_naive$cluster_id),]
dat_naive$block_id <- rep(1:(nrow(dat_naive)/4), each = 4)
within_b_diffs_naive <- block_diff_func(dat_naive)

col1 <- rgb(1, 0, 0, .5, maxColorValue = 1)
col2 <- rgb(0, 0, 1, .5, maxColorValue = 1)
hist(within_b_diffs_brc, breaks = seq(0,3.5,.5), col = col1)
hist(within_b_diffs_naive, breaks = seq(0,3.5,.5), add = T, col = col2)