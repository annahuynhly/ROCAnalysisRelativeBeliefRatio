
home_page = fluidPage(
  titlePanel("Home"),
  sidebarLayout(
    sidebarPanel(width = 4, 
      p("Placeholder text.")
    ),
    mainPanel(
      p("Hello and welcome! This is the home page. When proceeding to any of the computations, please
        ensure that you go through sections \"Getting Started\" before entering any of the computations 
        to select that you are selecting from the proper populations."),
      # MAY NEED TO ADD THIS COMMENT ELSEWHERE
      #p("Note that the relevant prevalence w is the proportion of this subpopulation who are diseased."),
      p("Some of the computations take a long time to run, hence, switching between pages may take awhile."),
      p("The calculator is based on the works of Al Labadi, L., Evans, M. and Liang, Q. (2022)."),
      tags$div(
        tags$a(href="https://www.mdpi.com/1099-4300/24/12/1710", 
               "The paper is found here."),
        "It is fortunately open access! :)"
      )
    )
  ),
  br(style = "line-height:7;")
)