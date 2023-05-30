# contact page

contact_page = fluidPage(
  
  
  tags$script(src = "https://kit.fontawesome.com/5e940c3ade.js"),
  withAnim(),
  
  #div(id = "title_test", titlePanel("Contact & Credits")),
  titlePanel("Contact & Credits"),
  tags$div(
    "The computations are based on the works of ",
    tags$a(href="https://www.mdpi.com/1099-4300/24/12/1710", 
           "Al Labadi, L., Evans, M. and Liang, Q. (2022)."),
    "The paper is open access."
  ),
  p(""),
  tags$div(
    "We constructed this website using ",
    tags$a(href="https://www.r-project.org/about.html", "R."),
    "Specifically, we used the ",
    tags$a(href="https://shiny.rstudio.com/", "R Shiny "),
    "package. The website theme is flatly from ",
    tags$a(href="https://rstudio.github.io/shinythemes/", "shinythemes."),
  ),
  p(""),
  p('This website is maintained by Anna Ly. If you find any bugs on this website, please contact 
    [firstname]huynh.[lastname]@mail.utoronto.ca.'),
  p(""),
  p("The weird formatting of the email is to avoid spam."),
  
  tags$style("#project-grid {
                      display: grid;
                      grid-template-columns: 120px 1fr;
                      grid-gap: 10px;
                      }"),
  div(id = "project-grid",
      div(id = "AnnaImg", img(src = "anna_ly.png", style = 'border-radius: 50%', width = '120px')),
      div(h3('Anna Ly'),
          h4('Research Assistant @ University of Toronto'),
          p('Main programmer & maintainer of this website. An undergraduate statistics & 
            math student at the University of', style = "color:#61646b"),
          p("Toronto and an experienced teaching assistant for the Department of Mathematical 
          & Computational Sciences at the Mississauga campus.", 
            style = "color:#61646b"),
          #tags$script(src = "https://kit.fontawesome.com/5e940c3ade.js"),
          tags$div(
            tags$i(class = "fa-brands fa-github"),
            tags$a(href="https://github.com/annahuynhly", "Github"), " | ",
            tags$i(class = "fa-brands fa-linkedin"),
            tags$a(href="https://www.linkedin.com/in/anna-ly-statistics-specialist/", "Linkedin"), " | ",
            tags$i(class = "fa-solid fa-graduation-cap"),
            tags$a(href="https://scholar.google.ca/citations?user=9w41oS8AAAAJ&hl=en", "Google Scholar")
          ),
      ),
      div(id = "MikeImg", img(src = "mike_evans.PNG", style = 'border-radius: 50%', width = '120px')),
      div(h3('Michael Evans'),
          h4('Department Chair of Statistical Sciences & Professor @ University of Toronto'),
          p('The main author of the paper. Supplied most of the R codes for graph generation & computations.
            Assisted with', style = "color:#61646b"),
          p('designing the user layout of the website.', style = "color:#61646b"),
          tags$div(
            tags$i(class = "fa-solid fa-user"),
            tags$a(href="https://utstat.toronto.edu/mikevans/", "Personal Website"), " | ",
            tags$i(class = "fa-solid fa-graduation-cap"),
            tags$a(href="https://scholar.google.ca/citations?user=i4Z5iW4AAAAJ&hl=en", "Google Scholar"),
          ),
      ),
      
      div(id = "LuaiImg", img(src = "luai_labadi.PNG", style = 'border-radius: 50%', width = '120px')),
      div(h3('Luai Al Labadi'),
          h4('Associate Chair & Assistant Professor @ University of Toronto'),
          p('One of the authors of the paper and assisted with codes pertaining to 
            the dirichlet process (the nonparametric bayes model.)', 
            style = "color:#61646b"),
          tags$div(
            tags$i(class = "fa-solid fa-building-columns"),
            tags$a(href="https://www.utm.utoronto.ca/math-cs-stats/people/luai-al-labadi", "UofT Webpage"),
            tags$i(class = "fa-solid fa-graduation-cap"), " | ",
            tags$a(href="https://scholar.google.ca/citations?user=DIin_xEAAAAJ&hl=en", "Google Scholar"),
          ),
      ),
      
      div(id = "QiaoyuImg", img(src = "qiaoyu_liang.PNG", style = 'border-radius: 50%', width = '120px')),
      div(h3('Qiaoyu Liang'),
          h4('PhD Student @ University of Toronto'),
          p('One of the authors of the paper and assisted Mike Evans with R-codes.', style = "color:#61646b"),
          tags$div(
            tags$i(class = "fa-solid fa-building-columns"),
            tags$a(href="https://www.statistics.utoronto.ca/people/directories/graduate-students/qiaoyu-liang", 
                          "UofT Webpage"), " | ",
            tags$i(class = "fa-solid fa-graduation-cap"),
            tags$a(href="https://scholar.google.ca/citations?hl=en&user=7puofJYAAAAJ", "Google Scholar"),
          ),
      ),
      
  ),
  
  
)