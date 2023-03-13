################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

output$binormal_val_hypoAUC_value = renderPrint({
  list("Plausible Region for the AUC" = sect3.3_AUC_RBR()$plausible_region,
       "Posterior Content of the Plausible Region for the AUC" = sect3.3_AUC_RBR()$postPl_AUC,
       "Credible region for the AUC" = "NEED TO COMPUTE (?)",
       "P(AUC > 1/2)" = sect3.3_AUC_prior()$probAUCprior,
       "P(AUC > 1/2 | data) / Strength of the evidence" = sect3.3_AUC_post()$probAUCpost,
       "Relative Belief Ratio of AUC > 1/2" = sect3.3_AUC_RBR()$RBprobAUC)
})

################################################################
# HISTOGRAMS                                                   #
################################################################

binormal_val_colours = reactive({
  # Total order of ALL colours: prior, posterior, relative belief ratio, 
  # plausible region, y = 1 line, credible region, 
  if(input$binormal_val_colour == 'default1'){
    c("#FF6666", "#6699FF", "#05DEB2", "#947aff", "#3333FF", "#5b10a7")
  } else if(input$binormal_val_colour == 'default2'){
    c("blue", "green", "red", "#b3bfff", "royalblue1", "#81ddff")
  } else if (input$binormal_val_colour == 'manual'){
    c(convert_to_hex(input$binormal_val_colour_prior),
      convert_to_hex(input$binormal_val_colour_post),
      convert_to_hex(input$binormal_val_colour_rbr),
      convert_to_hex(input$binormal_val_colour_pr),
      convert_to_hex(input$binormal_val_colour_line_1),
      convert_to_hex(input$binormal_val_colour_cr)
    )
  }
})

binormal_val_copt_colours = reactive({
  if(input$binormal_val_c_opt_carry_colour != 'manual'){
    binormal_val_colours()[c(1, 2, 3)]
  } else if (input$binormal_val_c_opt_carry_colour == 'manual'){
    c(convert_to_hex(input$binormal_val_priorc_opt_colour),
      convert_to_hex(input$binormal_val_postc_opt_colour),
      convert_to_hex(input$binormal_val_rbrc_opt_colour))
  }
})

output$binormal_val_postprior_graph = renderPlot({
  binorm_val_diag_prior_post_graph(delta = input$binormal_val_delta, 
                                   prior = sect3.3_AUC_prior()$priorAUCdensity, 
                                   post = sect3.3_AUC_post()$postAUCdensity, 
                                   plausible_region = sect3.3_AUC_RBR()$plausible_region,
                                   colour_choice = binormal_val_colours()[c(1, 2, 4, 6)],
                                   transparency = input$binormal_val_col_transparency)
})

output$binormal_val_RB_graph = renderPlot({
  binorm_val_diag_rbr_graph(delta = input$binormal_val_delta,
                            relative_belief_ratio = sect3.3_AUC_RBR()$RB_AUC, 
                            plausible_region = sect3.3_AUC_RBR()$plausible_region,
                            colour_choice = binormal_val_colours()[c(3:6)],
                            transparency = input$binormal_val_col_transparency)
})

################################################################
# DOWNLOAD DATAFRAME                                           #
################################################################

binormal_val_generate_dataframe = reactive({
  df = data.frame(open_bracket_grid(input$binormal_val_delta),
                  sect3.3_AUC_prior()$priorAUCdensity, 
                  sect3.3_AUC_post()$postAUCdensity, 
                  sect3.3_AUC_RBR()$RB_AUC)
  colnames(df) = c("Grid point", "Prior of the AUC", "Posterior of the AUC", 
                   "Relative Belief Ratio of the AUC")
  df
})

binormal_val_download = reactive({
  binormal_val_generate_dataframe()
})

output$binormal_val_dataframe = renderDataTable({
  binormal_val_download()
})

output$binormal_val_downloadData = downloadHandler(
  filename = function() {
    paste(input$binormal_val_filename, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(binormal_val_download(), file, row.names = FALSE)
  }
)
