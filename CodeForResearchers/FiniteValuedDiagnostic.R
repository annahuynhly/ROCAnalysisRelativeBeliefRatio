################################################
# Codes for the Finite Valued Diagnostic       #
################################################

library(LaplacesDemon) # for rdirichlet()
library(varhandle) # for check.numeric()

source("./helper_functions.R")
source("./section3.2/finite_val_diag_compute_functions.R")
source("./section3.2/finite_val_diag_graph_functions.R")

# setting the seed to get results
set.seed(1)

#########################################################################
# PART 1: Inferences for the AUC                                        #
#########################################################################

# Simulation Sizes
nMonteCarlo = 100000 # simulation sample size
delta = 0.04 # meaningful difference for the AUC
# Hyperparameters to Specify the Dirichlet:
alpha_ND = c(1, 1, 1, 1, 1)
alpha_D = c(1, 1, 1, 1, 1)
# Data (sample count):
nND = 50 # total non diseased 
nD = 100 # total diseased
n = nND + nD # total diseased and non diseased
# Data (counts at c_{1}, ..., c_{m}):
fND = c(29, 7, 4, 5, 5)
fD = c(14, 7, 25, 33, 21)

# The user now must pick between whether they want the conditional or the unconditional prior.

##################################################################
# THE UNCONDITIONAL PRIOR CASE

condition = "unconditional"
# resampling only matters for the conditional case (you'll see the options below)
resample = FALSE

##################################################################
# THE CONDITIONAL PRIOR CASE 1
# (The conditional sample size does NOT match the unconditional sample size)

condition = "conditional"
resample = FALSE

##################################################################
# THE CONDITIONAL PRIOR CASE 2
# (The conditional sample size DOES match the unconditional sample size)

condition = "conditional"
resample = TRUE

# Back to regular computations

AUC_prior = finite_val_prior(condition, resample, 
                             nMonteCarlo, alpha_ND, alpha_D)
AUC_post = finite_val_post(condition, resample, 
                           nMonteCarlo, alpha_ND, alpha_D, fND, fD)

AUC_prior = finite_val_prior(condition = "conditional", resample = TRUE, 
                             nMonteCarlo, alpha_ND, alpha_D)
AUC_post = finite_val_post(condition = "conditional", resample = TRUE, 
                           nMonteCarlo, alpha_ND, alpha_D, fND, fD)

# Calculating the relative belief ratio

AUC_RBR = finite_val_RBR(delta, AUC_prior$AUC, AUC_post$AUC)

# Getting the plausible region

pr = compute_AUC_plausible_region(delta, AUC_RBR$AUC_RBR)

pr

# Computing the posterior content of the plausible region

post_content = compute_AUC_post_content(delta,  AUC_post$AUC, 
                                        plausible_region = pr$plausible_region)
post_content

# Computing the credible region

# input gamma, for the credible region
gamma = 0.5

compute_AUC_credible_region(gamma, grid = pr$grid, density = pr$density, 
                            AUC_post$AUC, post_content, pr$plausible_region)

##################################################################
# Making a plot for the AUC

par(mfrow = c(1, 2))

density_hist_AUC_prior_post(delta, AUC_prior = AUC_prior$AUC, AUC_post = AUC_post$AUC,
                            densityplot = TRUE, 
                            showbars = FALSE, 
                            colour_choice = c("#FF6666", "#6699FF"),
                            lty_type = c(2, 2),
                            transparency = 0.2,
                            legend_position = "topleft")

density_hist_AUC_RBR(delta, AUC_RBR = AUC_RBR$AUC_RBR,
                     rb_line = FALSE, densityplot = TRUE, showbars = FALSE, 
                     colour_choice = c("#05DEB2", "#3333FF", "#5b10a7"),
                     lty_type = c(2, 2, 3),
                     transparency = 0.2,
                     legend_position = "topleft")

#########################################################################
# PART 2: The Prevalence                                                #
#########################################################################

# If the prevalence is known: input the prevalence.
w = 0.65
alpha1w = NA
alpha2w = NA
use_version = NA

# If the prevalence w is unknown, insert alpha1w and alpha2d, 
# where they are beta prior parameters
w = FALSE
alpha1w = 391.72
alpha2w = 211.39

#########################################################################
# PART 3: Inferences for the Prevalence                                 #
#########################################################################

# if the sampling regime is a sample of of nD from diseased and nND from non-diseased:
use_version = "prior"

# If the prevalence w is unknown and the sampling regime is
# a sample of n from the population, observe nD diseased and nND non-diseased, then:
use_version = "post"

#########################################################################
# PART 4: Inferences for the Optimal Cutoff                             #
#########################################################################

delta = 0.001 # the meaningful difference for the prevalence

# The user now must pick between multiple ways to select the cutoff:
# 1. Find the cutoff copt minimizing Error(c)

# 2. Find the cutoff maximizing Youden's index
# In this case, Youden’s index will be obtained simply by putting w=1/2
if(is.na(use_version)){
  w = 0.5
}

# 3. Specify the cutoff

copt_method = "error"

AUC_prior_copt = simulate_AUC_mc_prior(condition, resample, nND, nD, 
                                       nMonteCarlo, w, alpha1w, alpha2w, 
                                       alpha_ND, alpha_D, copt_method)

# Prior Probabilities for the Possible Diagnostic Values
AUC_prior_copt$priorc_opt

AUC_post_copt = simulate_AUC_mc_post(condition, resample,
                                     nND, nD, nMonteCarlo, w, 
                                     alpha1w, alpha2w, version = use_version,
                                     alpha_ND, alpha_D, fND, fD, copt_method)

# Posterior Probabilities for the Possible Diagnostic Values
AUC_post_copt$postc_opt

AUC_rbr_copt = compute_AUC_RBR(delta, AUC_prior = AUC_prior_copt$AUC, 
                               AUC_post = AUC_post_copt$AUC, 
                               priorc_opt = AUC_prior_copt$priorc_opt, 
                               postc_opt = AUC_post_copt$postc_opt)

#Relative Belief Ratio Probabilities for the Possible Diagnostic Values
AUC_rbr_copt$RBc_opt

# Cutoff Minimizing Error(c):
AUC_rbr_copt$c_optfDfND

# Plausible Region of the Cutoff
AUC_rbr_copt$plausible_region

# Posterior Content of the Plausible Region of the Cutoff
AUC_rbr_copt$posterior_content

# Error Characteristics
c_optfDfND = AUC_rbr_copt$c_optfDfND

# Note: if you want to specify the cutoff, do so below (otherwise, skip.):
c_optfDfND = 2

AUC_prior_copt_err = AUC_prior_error_char_copt(c_optfDfND, nMonteCarlo, w, 
                          alpha1w, alpha2w, delta, 
                          pND_array = AUC_prior_copt$pND_array, 
                          pD_array = AUC_prior_copt$pD_array, 
                          FNR = AUC_prior_copt$FNR, 
                          FPR = AUC_prior_copt$FPR, 
                          ERROR_w = AUC_prior_copt$ERROR_w, 
                          PPV = AUC_prior_copt$PPV, 
                          priorc_opt = AUC_prior_copt$priorc_opt)

AUC_post_copt_err = AUC_post_error_char_copt(c_optfDfND, nMonteCarlo, w, alpha1w, 
                         alpha2w, nD, nND, version, delta, 
                         pND_array = AUC_post_copt$pND_array, 
                         pD_array = AUC_post_copt$pD_array, 
                         FNR = AUC_post_copt$FNR, 
                         FPR = AUC_post_copt$FPR, 
                         ERROR_w = AUC_post_copt$ERROR_w, 
                         PPV = AUC_post_copt$PPV, 
                         postc_opt = AUC_post_copt$postc_opt)

compute_AUC_error_char_copt(delta, c_optfDfND, 
                            priorFPRc_opt = AUC_prior_copt_err$priorFPRc_opt, 
                            priorFNRc_opt = AUC_prior_copt_err$priorFNRc_opt, 
                            priorERROR_wc_opt = AUC_prior_copt_err$priorERROR_wc_opt, 
                            priorFDRc_opt = AUC_prior_copt_err$priorFDRc_opt, 
                            priorFNDRc_opt = AUC_prior_copt_err$priorFNDRc_opt,
                            priorPPVc_opt = AUC_prior_copt_err$priorPPVc_opt,
                            postFPRc_opt = AUC_post_copt_err$postFPRc_opt, 
                            postFNRc_opt = AUC_post_copt_err$postFNRc_opt, 
                            postERROR_wc_opt = AUC_post_copt_err$postERROR_wc_opt, 
                            postFDRc_opt = AUC_post_copt_err$postFDRc_opt, 
                            postFNDRc_opt = AUC_post_copt_err$postFNDRc_opt,
                            postPPVc_opt = AUC_post_copt_err$postPPVc_opt)

##################################################################
# Making a plot for the optimal cutoff

par(mfrow = c(1, 2))

plots_AUC_copt(priorc_opt = AUC_prior_copt$priorc_opt, 
               postc_opt = AUC_post_copt$postc_opt, 
               RBc_opt = FALSE,
               prior_label = 3, post_label = 4, rb_label = 8,
               colour_choice = c("red", "blue", "#16957C"),
               legend_position = "topright")

plots_AUC_copt(priorc_opt = FALSE, postc_opt = FALSE,
               RBc_opt = AUC_rbr_copt$RBc_opt,
               prior_label = 3, post_label = 4, rb_label = 8,
               colour_choice = c("red", "blue", "#16957C"),
               legend_position = "topright")

