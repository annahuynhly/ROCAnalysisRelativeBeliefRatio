################################################################
# VARIABLES                                                    #
################################################################

# Setting the data frame

# Initilizing the new entries
nonpara_bayes_new_vals_vector = reactive({
  convert_char_to_vector(input$nonpara_bayes_data_values)
})

# Initializing the table
nonpara_bayes_diseased_vector = reactiveValues(
  x = c(0.89345810, -0.09544302,  1.52694609,  2.30531596,  0.45009081,  0.97189716,
        0.85430995,  2.40987144,  1.44936186, -0.31305846,  0.19524931,  0.75202021,
        1.63136183,  1.31617751, -0.26481975,  1.69469220,  1.67520405,  1.50587628,
        -1.18927465,  1.75076313)
)
nonpara_bayes_nondiseased_vector = reactiveValues(
  x = c(-0.11315894,  0.03273954, -0.69180664, -0.05459313, -1.22760962, -0.25705819,
        -1.55799712, -0.34339482, -1.11229004,  0.11031882,  0.37785845,  0.76029521,
        -0.34052122,  1.03882232, -0.26665494,  0.48965747,  0.80441378, -1.31205550,
        -1.09934759,  1.55522803, -0.19981736,  0.51936199,  0.95234605,  1.56027376,
        -1.42501031)
)

# This is for adding the rows
observeEvent(input$nonpara_bayes_add_values_diseased, {
  nonpara_bayes_diseased_vector$x = c(nonpara_bayes_diseased_vector$x, 
                                      nonpara_bayes_new_vals_vector())
})

observeEvent(input$nonpara_bayes_add_values_nondiseased, {
  nonpara_bayes_nondiseased_vector$x = c(nonpara_bayes_nondiseased_vector$x, 
                                         nonpara_bayes_new_vals_vector())
})

# This is for removing the last entry
observeEvent(input$nonpara_bayes_reset_last_diseased, {
  nonpara_bayes_diseased_vector$x = nonpara_bayes_diseased_vector$x[-length(nonpara_bayes_diseased_vector$x)]
})

observeEvent(input$nonpara_bayes_reset_last_nondiseased, {
  nonpara_bayes_nondiseased_vector$x = nonpara_bayes_nondiseased_vector$x[-length(nonpara_bayes_nondiseased_vector$x)]
})

# This is for resetting the tables
observeEvent(input$nonpara_bayes_reset_diseased, {
  nonpara_bayes_diseased_vector$x = c()
})
observeEvent(input$nonpara_bayes_reset_nondiseased, {
  nonpara_bayes_nondiseased_vector$x = c()
})

output$nonpara_bayes_show_diseased_vec = renderPrint({
  nonpara_bayes_diseased_vector$x
})

output$nonpara_bayes_show_nondiseased_vec = renderPrint({
  nonpara_bayes_nondiseased_vector$x
})


# Setting the seed
SECT3.2_SEED = reactive(input$nonpara_bayes_seed)

#in the first computable function, make sure to include: set.seed(SECT3.4_SEED()) 