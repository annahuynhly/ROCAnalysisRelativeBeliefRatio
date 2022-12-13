
# Try to have each home page be on a separate file? Same with server?
home_page = div(
  titlePanel("Home Page"),
  p("Hello and welcome! This is the home page. Please click on one of the following links 
    to use the calculator!"),
  p("Note that some of the computations take a long time to run, hence, switching between 
    pages may take awhile."),
  p("This website was constructed using R Shiny and R Shiny.Router."),
  p("The calculator is based on the works of Al Labadi, L., Evans, M. and Liang, Q. (2022).
    Specifically, this website is currently based on Section 3.2."),
  p("We plan to implement more sections (section 3.3, section 3.4) in the future."),
  tags$div(
  tags$a(href="https://www.mdpi.com/1099-4300/24/12/1710", 
         "The paper is found here."),
  "It is fortunately open access! :)"
  )
)

# tags$div(
#"If you enjoyed this tool, ",
#tags$a(href="https://www.gofundme.com/f/fantasy-football-mental-health-initiative?utm_medium=copy_link&utm_source=customer&utm_campaign=p_lico+share-sheet", 
#       "please consider donating to the Fantasy Football Mental Health Initiative!")
#),

# the '_1' at the end to signify it's the first webpage
page_conditionalROC_1 = div(
  titlePanel("Section 3.2: conditionalROC"),
  p("Placeholder page. Will be updated once the error has been fixed.")
)

page_readdata = div(
  titlePanel("Section 3.2: readdata"),
  p("Placeholder page. Will be updated once the error has been fixed.")
)

page_ROC = div(
  titlePanel("Section 3.2: ROC"),
  p("Placeholder page. Will be updated once the error has been fixed.")
)

contact_page = div(
  titlePanel("Contact"),
  p("This website was constructed by Anna Ly. If you find any bugs on this website, please contact:
    <firstname>huynh.<lastname>@mail.utoronto.ca"),
  tags$div(
    "You may also find them on Github ",
    tags$a(href="https://github.com/annahuynhly", "here!"),
    " Or their Linkedin" ,
    tags$a(href="https://www.linkedin.com/in/anna-ly-statistics-specialist/", "here!")),
  p(""),
  p("To find information on the authors who wrote the paper and the original R code, 
    you can find their contact below."),
  tags$div(
    "Mike Evans: ",
    tags$a(href="https://utstat.toronto.edu/mikevans/", "Website"),
    "Luai Al Labadi: ",
    tags$a(href="https://www.utm.utoronto.ca/math-cs-stats/people/luai-al-labadi", "UofT Webpage")),
)