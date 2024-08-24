
setwd("C:/Users/AnnaH/OneDrive/Desktop/Stats RA/ROCAnalysis_RelativeBelief/CodeForResearchers")

# Helper Functions & Similar
source("./helper_functions.R")

######################################################################
# Section 3.1 - The Prevalence
source("./section3.1/prevalence_setup_functions.R")

###################################################
# VERSION 1:
# The following assumes that the prevalence omega is unknown, and we have the sampling regime:
# a sample of n from the population, observe nD diseased and nND nondiseased.

# alpha1w and alpha2w are beta prior parameters
alpha1w = 391.72
alpha2w = 211.39
nND = 50 # total non diseased 
nD = 100 # total diseased
n = nND + nD # total diseased and non diseased
delta = 0.04 # the meaningful difference for the AUC

grid = open_bracket_grid(delta)

RBR_compute_values(alpha1w, alpha2w, n, nD, grid)

prior_compute_values(alpha1w, alpha2w, grid)

# input gamma, for the credible region

compute_credible_region(alpha1w, alpha2w, n, nD, grid, gamma, delta, relative_belief_ratio, 
                        posterior_content, plausible_region)

w0_compute_values(alpha1w, alpha2w, n, nD, w0, relative_belief_ratio, grid)






# Genetating the plots

generate_prior_post_graph(prior, post, grid, colour_choice = c("blue", "green"),
                          lty_type = c(2, 2), transparency = 0.1,
                          legend_position = "topleft")

generate_rbr_graph(relative_belief_ratio, grid, rb_line = FALSE, 
                   colour_choice = c("red", "royalblue1", "#81ddff"),
                   lty_type = c(2, 3, 3), transparency = 0.1,
                   legend_position = "bottomleft")



######################################################################
# Section 3.2
source("./section3.2/finite_val_diag_compute_functions.R")
source("./section3.2/finite_val_diag_graph_functions.R")


######################################################################
# Section 3.3
source("./section3.3/binormal_diag_compute_functions.R")
source("./section3.3/binormal_diag_compute_functions_unequal.R")
source("./section3.3/binormal_diag_graph_functions.R")


######################################################################
# Section 3.4
source("./section3.4/nonpara_bayes_compute_functions.R")
source("./section3.4/nonpara_bayes_graph_functions.R")