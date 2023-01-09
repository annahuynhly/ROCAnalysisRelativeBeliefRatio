
################################################################
# DESCRIPTION                                                  #
################################################################

BNPdata_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("Need to work on this section. Come back later!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_BNPdata = div( # CHANGE THIS
  titlePanel("Section 3.4: BNPdata"), # CHANGE THIS
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      radioButtons("BNPdata_gender", h3("Gender"),
                   choices = list("Male" = "male", "Female" = "female"), 
                   selected = "male"),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", BNPdata_description), # CHANGE THIS
                  tabPanel("Calculator", verbatimTextOutput("BNPdata_output")),
                  tabPanel("Plot", plotOutput(outputId = "BNPdata_plot")),
                  
      )
    )
  )
)

################################################################
# MAIN FUNCTIONS                                               #
################################################################

BNPdata_output = function(gender){
  if(gender == "F" | gender == "Female" | gender == "f" | gender == "female"){
    #Females
    NDfemales=subset(Females_Covid_Data, Females_Covid_Data$Death=='0', select=c(Age))
    Dfemales=subset(Females_Covid_Data, Females_Covid_Data$Death=='1', select=c(Age))
    #nrow(NDfemales)
    #nrow(Dfemales)
    
    # count the number of unique values in Female subgroups and
    # get the unique values and means and sum of squared deviations
    nND=nrow(count(NDfemales,var=Age))
    xx=count(NDfemales,var=Age)
    NDAgeunique=xx$var
    xND=mean(NDAgeunique)
    s2ND=sum((NDAgeunique-xND)**2)
    nD=nrow(count(Dfemales,var=Age))
    xx=count(Dfemales,var=Age)
    DAgeunique=xx$var
    xD=mean(DAgeunique)
    s2D=sum((DAgeunique-xD)**2)
  }
  else if(gender == "M" | gender == "Male" | gender == "m" | gender == "male"){
    # Males
    NDmales = subset(Males_Covid_Data, Males_Covid_Data$Death=='0', select=c(Age))
    Dmales = subset(Males_Covid_Data, Males_Covid_Data$Death=='1', select=c(Age))
    #nNDmales = nrow(NDmales)
    #nDmales = nrow(Dmales)
    
    # count the number of unique values in Male subgroups and
    # get the unique values and means and sum of squared deviations
    nND = nrow(count(NDmales,var=Age))
    xx = count(NDmales,var=Age)
    NDAgeunique = xx$var
    xND = mean(NDAgeunique)
    s2ND = sum((NDAgeunique-xND)**2)
    nD=nrow(count(Dmales,var=Age))
    xx=count(Dmales,var=Age)
    DAgeunique=xx$var
    xD=mean(DAgeunique)
    s2D=sum((DAgeunique-xD)**2)
  }
  else {
    return("gender must be 'male' or 'female'.")
  }
  newlist = list("nND" = nND, "xND" = xND, "s2ND" = s2ND, "nD" = nD, "xD" = xD, "s2D" = s2D)
  return(newlist)
}

BNPdata_graphs = function(gender){ #Code for which graph to choose
  if(gender == "F" | gender == "Female" | gender == "f" | gender == "female"){
    ggplot(data=Females_Covid_Data) + geom_histogram(mapping=aes(x=Age),binwidth=1) + ggtitle("Female Data")
  }
  else if(gender == "M" | gender == "Male" | gender == "m" | gender == "male"){
    ggplot(data=Males_Covid_Data) + geom_histogram(mapping=aes(x=Age), binwidth=1) + ggtitle("Male Data")
  }
  else {
    return("gender must be 'male' or 'female'.")
  }
}
