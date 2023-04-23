#p {
#  line-height: 1.6; 
#  font-family: Helvetica;
#  text-align: justify;
#  margin: 0;
#  font-size: 14px;
#}


getting_started_CSS = "
.DiseasedImg {
  float: left;
  width: 360px;
  shape-margin: 20px;
  margin-right: 20px;
  margin-bottom: 20px;
}
"
getting_started_start_text = div(
  p("This website is based on ROC (Receiver Operating Characteristic) analysis, which is commonly 
  used to determine whether a real-valued diagnostic variable for a disease or condition is useful 
  in medical studies."),
  br(),
)

getting_started_img_text = div(
  p("The prevalence is the proportion of individuals in a population who have a specific 
          characteristic at a given point in time. It is a commonly used term in medical sciences, 
          such as epidemiology, to find the proportion of a population that are diseased at a 
          specified time."),
  br(),
  p("The phrase “relevant prevalence” refers to the proportion of a certain subpopulation 
        who are exhibiting certain symptoms, or commonly, the diseased group. The value of w 
        represents the sub-population of those who exhibit the aforementioned symptoms of 
        interest."),
)

getting_started_end_text = div(
  p("To use our online calculator, you need to select the seed and determine whether the prevalence 
  w is known. The seed is a number that determines the randomization of the data and we use the 
  same seeding method as base R."),
  br(),
  p("If the prevalence is unknown, then you need to select the sampling regime for your experiment. 
  The sampling regime is the method of obtaining information about w from the data. Each sampling 
  regime has its own assumptions and calculations, depending on the type and amount of information 
  available."),
  br(),
  p("Note that the seed is sensitive by the order in which you open certain pages."),
  br(),
  p("Once you have filled out the important information, you can click on the \"Next\" button 
  to proceed to the next step of the calculator."),
  br(),
)


getting_started_default_description_1 = div(
  getting_started_start_text, # start text
  div(
    tags$head(
      tags$style(HTML(getting_started_CSS))
    ),
    fluidRow(
      column(
        width = 12,
        img(id = "diseased_group", class = "DiseasedImg",
          src = "diseased_group.jpg", style = 'border-radius: 50%'),
        getting_started_img_text, # mid text
        tags$div(
          "The photo of masked individuals is from Cottonbro Studio:",
          tags$a(href="https://www.pexels.com/photo/health-workers-wearing-face-mask-3957987/", 
                 "Click here to access.")
        ),
      ) 
    ), 
  ),
  getting_started_end_text, # end text
)

getting_started_default_description_2 = div(
  getting_started_start_text, # start text
  div(
    tags$head(
      tags$style(HTML(getting_started_CSS))
    ),
    fluidRow(
      column(
        width = 12,
        img(id = "diseased_group2", class = "DiseasedImg",
            src = "diseased_group2.jpg", style = 'border-radius: 50%'),
        getting_started_img_text, # mid text
        tags$div(
          "The photo of masked individuals is from Cottonbro Studio:",
          tags$a(href="https://www.pexels.com/photo/people-taking-picture-of-a-painting-of-mona-lisa-with-face-mask-3957980/", 
                 "Click here to access.")
        ),
      ) 
    ), 
  ),
  getting_started_end_text, # end text
)

getting_started_default_description_3 = div(
  getting_started_start_text, # start text
  div(
    tags$head(
      tags$style(HTML(getting_started_CSS))
    ),
    fluidRow(
      column(
        width = 12,
        img(id = "diseased_group3", class = "DiseasedImg",
            src = "diseased_group3.jpg", style = 'border-radius: 50%'),
        getting_started_img_text, # mid text
        tags$div(
          "The photo of masked individuals is from Cottonbro Studio:",
          tags$a(href="https://www.pexels.com/photo/group-of-people-taking-a-group-selfie-3957989/", 
                 "Click here to access.")
        ),
      ) 
    ), 
  ),
  getting_started_end_text, # end text
)

