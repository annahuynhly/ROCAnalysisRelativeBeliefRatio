
source("ShinyHelperFunctions.R")
# Try to have each page be on a separate file? Same with server?

# the '_1' at the end to signify it's the first webpage

page_ROC = div(
  titlePanel("Section 3.2: ROC"),
  sidebarLayout(
    sidebarPanel(
      #inputs: pND, pD, nND, nD
      textInput(inputId = "ROC_pND",
                   tags$p('Set the probability distribution for diagniostic variable in ND', 
                          style = "font-size: 90%;"),value = "0.5, 0.2, 0.1, 0.1, 0.1"),
      numericInput(inputId = "ROC_nND",
                   tags$p('Set the sample size from ND and generate the data', 
                          style = "font-size: 90%;"),value = 50, min = 1),
      textInput(inputId = "ROC_pD",
                tags$p('Set the probability distribution for diagniostic variable in D', 
                       style = "font-size: 90%;"), value = "0.1, 0.1, 0.2, 0.3, 0.3"),
      numericInput(inputId = "ROC_nD",
                tags$p('Set the sample size from D and generate the data', 
                       style = "font-size: 90%;"), value = 100, min = 1),
      numericInput(inputId = "ROC_w",
                  tags$p('Set the relevant prevalence', 
                      style = "font-size: 90%;"), value = 0.65),
    ),
    # The panel displays outputs
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Calculator_1", verbatimTextOutput("ROC_value_1")),
                  tabPanel("Calculator_2", verbatimTextOutput("ROC_value_2")),
      #            tabPanel("Calculator_3", verbatimTextOutput("realdataROC_value_3")),
      #            tabPanel("Calculator_4", verbatimTextOutput("realdataROC_value_4")),
      )
    )
  )
)

#pND = "0.5, 0.2, 0.1, 0.1, 0.1"
#nND = 50
#pD = "0.1, 0.1, 0.2, 0.3, 0.3"
#nD = 100

#simulate_data_ROC(pND, pD, nND, nD)

simulate_data_ROC = function(pND, pD, nND, nD){
  if (!is.double(pND) & !is.double(pD)) {
    # The case where it's inputted as a string
    pND = convert_char_to_vector(pND)
    pD = convert_char_to_vector(pD)
    if (!(valid_vector(pND) == TRUE & valid_vector(pD) == TRUE)) {
      return("Error: Either pND or pD provide invalid responses.")
    }
  }
  if (length(pND) == length(pD)){
    m = length(pND)
    fND = t(rmultinom(1,nND,pND))
    fD = t(rmultinom(1,nD,pD))
    newlist = list("fND" = fND, "fD" = fD)
    return(newlist)
  } else {
    return("Length of pND and length of PD must be the same.")
  }
}

ROC_compute_some_outputs_1 = function(w, pND, pD){
  if (!is.double(pND) & !is.double(pD)) {
    # The case where it's inputted as a string
    pND = convert_char_to_vector(pND)
    pD = convert_char_to_vector(pD)
    if (!(valid_vector(pND) == TRUE & valid_vector(pD) == TRUE)) {
      return("Error: Either pND or pD provide invalid responses.")
    }
  }
  if (length(pND) == length(pD)){
    m = length(pND)
  } else {
    return("Length of pND and length of PD must be the same.")
  }
  w = .65
  FPR = rep(0,m)
  FNR = rep(0,m)
  ERROR_w = rep(0,m)
  FDR = rep(0,m)
  FNDR = rep(0,m)
  for(i in 1:m){
    FPR[i] = 1 - sum(pND[1:i])
    FNR[i] = sum(pD[1:i])
    ERROR_w[i] = (w*FNR[i] + (1 - w)*FPR[i])
    if ((w*(1 - FNR[i]) + (1 - w)*FPR[i]) != 0){
      FDR[i] = (1 - w)*FPR[i]/(w*(1 - FNR[i]) + (1 - w)*FPR[i])}
    if ((w*FNR[i] + (1 - w)*(1 - FPR[i])) != 0){
      FNDR[i] = w*FNR[i]/(w*FNR[i] + (1 - w)*(1 - FPR[i]))}
  }
  truec_opt = which.min(ERROR_w)
  trueFPRc_opt = FPR[truec_opt]
  trueFNRc_opt = FNR[truec_opt]
  trueERROR_wc_opt = ERROR_w[truec_opt]
  trueFDRc_opt = FDR[truec_opt]
  trueFNDRc_opt = FNDR[truec_opt]
  trueAUC = sum(pND*(1 - FNR))
  newlist = list("FPR" = FPR, "FNR" = FNR, "ERROR_w" = ERROR_w, "trueAUC" = trueAUC,
                 "FDR" = FDR, "FNDR" = FNDR,
                 "truec_opt" = truec_opt, "trueFPRc_opt" = trueFPRc_opt,
                 "trueFNRc_opt" = trueFNRc_opt, "trueERROR_wc_opt" = trueERROR_wc_opt,
                 "trueFDRc_opt" = trueFDRc_opt, "trueFNDRc_opt" = trueFNDRc_opt)
  return(newlist)
}

