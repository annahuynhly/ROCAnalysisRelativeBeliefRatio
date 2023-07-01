
getting_started_CSS = "
.DiseasedImg {
  float: left;
  width: 360px;
  shape-margin: 20px;
  margin-right: 20px;
  margin-bottom: 20px;
}
"

getting_started_img_text = div(
  p("The prevalence is the proportion of individuals in the population who have the disease."),
  p("For a sample of $n$ from $\\Omega$ then the relative belief inferences can be made about $w$, 
    otherwise only the prior of $w$ is used. If the prevalence is unknown, then you need to select 
    the sampling regime for your experiment.")
)

getting_started_end_text = div(
  br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
)


getting_started_default_description_1 = div(
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

