################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################





################################################################
# HISTOGRAMS                                                   #
################################################################

output$binormal_val_postprior_graph = renderPlot({
  binorm_val_diag_prior_post_graph(delta = input$binormal_val_delta, 
                                   prior = sect3.3_AUC_prior()$priorAUCdensity, 
                                   post = sect3.3_AUC_post()$postAUCdensity, 
                                   plausible_region = sect3.3_AUC_RBR()$plausible_region)
})

output$binormal_val_RB_graph = renderPlot({
  binorm_val_diag_rbr_graph(delta = input$binormal_val_delta,
                            relative_belief_ratio = sect3.3_AUC_RBR()$RB_AUC, 
                            plausible_region = sect3.3_AUC_RBR()$plausible_region)
})

################################################################
# DOWNLOAD DATAFRAME                                           #
################################################################