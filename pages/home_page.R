
home_page = fluidPage(
  titlePanel("Getting Started"),
  sidebarLayout(
    sidebarPanel(width = 4, 
      p("Placeholder text.")
    ),
    mainPanel(
      p("Hello and welcome! This is the home page. Please select one of the cases you are dealing with
        before proceeding with the calculator."),
      p("Note that the relevant prevalence w is the proportion of this subpopulation who are diseased."),
      p("Some of the computations take a long time to run, hence, switching between 
        pages may take awhile."),
      p("The calculator is based on the works of Al Labadi, L., Evans, M. and Liang, Q. (2022).
        Specifically, this website is currently based on Section 3.2."),
      p("We plan to implement more sections (section 3.3, section 3.4) in the future. The links currently have no useful information."),
      tags$div(
        tags$a(href="https://www.mdpi.com/1099-4300/24/12/1710", 
               "The paper is found here."),
        "It is fortunately open access! :)"
      )
    )
  ),
  br(style = "line-height:7;")
)