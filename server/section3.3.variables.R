################################################################
# VARIABLES                                                    #
################################################################

sect3.3_hyperpara = reactive({
  binormal_compute_post_hyperpara(mu0 = input$binormal_val_mu0, 
                                  tau0 = input$binormal_val_tau0, 
                                  lambda1 = input$binormal_val_lambda1, 
                                  lambda2 = input$binormal_val_lambda2, 
                                  nND = input$binormal_val_nND, 
                                  meanND = input$binormal_val_meanND, 
                                  sND_squared = input$binormal_val_sND_squared, 
                                  nD = input$binormal_val_nD, 
                                  meanD = input$binormal_val_meanD, 
                                  sD_squared = input$binormal_val_sD_squared)
})

sect3.3_AUC_prior = reactive({
  binormal_val_diag_prior(nMonteprior = input$binormal_val_nMonteCarlo, 
                          delta = input$binormal_val_delta, 
                          lambda1 = input$binormal_val_lambda1, 
                          lambda2 = input$binormal_val_lambda2, 
                          mu0 = input$binormal_val_mu0, 
                          tau0 = input$binormal_val_tau0)
})

sect3.3_AUC_post = reactive({
  binormal_val_diag_post(nMontepost = input$binormal_val_nMonteCarlo, 
                         delta = input$binormal_val_delta, 
                         lambda1post = sect3.3_hyperpara()$lambda1post, 
                         lambda2post = sect3.3_hyperpara()$lambda2post, 
                         mu0Dpost = sect3.3_hyperpara()$mu0Dpost, 
                         mu0NDpost = sect3.3_hyperpara()$mu0NDpost, 
                         tau0D = sect3.3_hyperpara()$tau0D, 
                         tau0ND = sect3.3_hyperpara()$tau0ND)
})

sect3.3_AUC_RBR = reactive({
  binormal_val_diag_RBR(delta = input$binormal_val_delta, 
                        probAUCprior = sect3.3_AUC_prior()$probAUCprior, 
                        probAUCpost = sect3.3_AUC_post()$probAUCpost,
                        priorAUC = sect3.3_AUC_prior()$priorAUC, 
                        postAUC = sect3.3_AUC_post()$postAUC)
})
