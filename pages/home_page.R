# This is specifically for the home page

home_page = div(
  titlePanel("Home Page"),
  hr(style = "border-top: 1px solid #363e4f;"),
  p("Hello and welcome! This is the home page. Please click on one of the following links 
    to use the calculator!"),
  p("Note that some of the computations take a long time to run, hence, switching between 
    pages may take awhile."),
  p("This website was constructed using R Shiny. Theme is flatly from shinythemes."),
  p("The calculator is based on the works of Al Labadi, L., Evans, M. and Liang, Q. (2022).
    Specifically, this website is currently based on Section 3.2."),
  p("We plan to implement more sections (section 3.3, section 3.4) in the future. The links currently have no useful information."),
  tags$div(
    tags$a(href="https://www.mdpi.com/1099-4300/24/12/1710", 
           "The paper is found here."),
    "It is fortunately open access! :)"
  ),
)