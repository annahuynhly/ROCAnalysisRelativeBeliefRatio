
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
    ),
    # The panel displays outputs
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Calculator_1", verbatimTextOutput("ROC_value_1")),
      #            tabPanel("Calculator_2", verbatimTextOutput("realdataROC_value_2")),
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



