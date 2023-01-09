
################################################################
# DESCRIPTION                                                  #
################################################################

itsforgammaprior_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("Need to work on this section. Come back later!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_itsforgammaprior = div( # CHANGE THIS
  titlePanel("Section 3.4: itsforgammaprior"), # CHANGE THIS
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "itsforgammaprior_gam", # CHANGE THIS
                   tags$p('gam', style = "font-size: 90%;"),value = 0.99),
      numericInput(inputId = "itsforgammaprior_l0", # CHANGE THIS
                   tags$p('l0', style = "font-size: 90%;"),value = 20),
      numericInput(inputId = "itsforgammaprior_u0", # CHANGE THIS
                   tags$p('u0', style = "font-size: 90%;"),value = 50),
      numericInput(inputId = "itsforgammaprior_maxits", # CHANGE THIS
                   tags$p('maxits', style = "font-size: 90%;"),value = 100),
      numericInput(inputId = "itsforgammaprior_alphaup", # CHANGE THIS
                   tags$p('alphaup', style = "font-size: 90%;"),value = 50),
      numericInput(inputId = "itsforgammaprior_alphalow", # CHANGE THIS
                   tags$p('alphalow', style = "font-size: 90%;"),value = 0),
      numericInput(inputId = "itsforgammaprior_eps", # CHANGE THIS
                   tags$p('eps', style = "font-size: 90%;"),value = 0.0001),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", itsforgammaprior_description), # CHANGE THIS
                  tabPanel("Calculator", verbatimTextOutput("itsforgammaprior_value")),
                  tabPanel("Plots",
                           fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                    plotOutput("itsforgammaprior_plot_1"), 
                                    plotOutput("itsforgammaprior_plot_2"),
                                    plotOutput("itsforgammaprior_plot_3"),
                          ))),
      )
    )
  )
)
################################################################
# MAIN FUNCTIONS                                               #
################################################################

itsforgammapriorfunction = function(gam, l0, u0, maxits, alphaup, alphalow, eps){
  p=(1+gam)/2
  z0=qnorm(p,0,1)
  up=(z0/l0)**2
  low=(z0/u0)**2
  
  for (i in 1:maxits){
    alpha=(alphalow+alphaup)/2
    beta=qgamma(p,alpha,1)/up
    #cat(i,alpha,beta,pgamma(beta*up,alpha,1)-pgamma(beta*low,alpha,1),"\n")
    test=pgamma(beta*low,alpha,1)
    if (abs(test-(1-p)) <= eps) {
      break }
    if( test < 1-p){
      alphaup=alpha}
    if( test > 1-p){
      alphalow=alpha}
  }
  
  # plot the prior density of 1/sigma^2
  x=c(2:20)/1000
  dens1= dgamma(x,alpha,beta)
  
  #plot the prior density of sigma^2
  y=1/x
  dens2=alpha*log(beta)-lgamma(alpha)-(alpha+1)*log(x)-beta/y
  dens2=exp(dens2)
  
  z=sqrt(y)
  #plot the prior density of sigma
  dens3=log(2)+alpha*log(beta)-lgamma(alpha)-(2*alpha+1)*log(z)-beta/(z**2)
  dens3=exp(dens3)
  
  #interval for 1/sigma^2
  interval_1 = c(low, up)
  # mode of prior for 1/sigma^2
  prior_mode_1 = (alpha-1)/beta
  
  #interval for sigma^2
  interval_2 = c(1/up,1/low)
  # mode of prior for 1/sigma^2
  prior_mode_2 = (alpha-1)/beta
  
  #interval for sigma
  interval_3 = c(sqrt(1/up),sqrt(1/low))
  # mode of prior for sigma
  prior_mode_3 = sqrt(2*beta/(1+2*alpha))
  
  newlist = list("x" = x, "y" = y, "z" = z, "dens1" = dens1, "dens2" = dens2, "dens3" = dens3, 
                 "interval_1" = interval_1, "interval_2" = interval_2, "interval_3" = interval_3,
                 "prior_mode_1" = prior_mode_1, "prior_mode_2" = prior_mode_2, "prior_mode_3" = prior_mode_3) 
  return(newlist)
}