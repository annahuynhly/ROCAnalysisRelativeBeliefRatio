# contact page

contact_page = div(
  titlePanel("Contact & Credits"),
  
  p('This website is maintained by Anna Ly. If you find any bugs on this website, please contact 
    [firstname]huynh.[lastname]@mail.utoronto.ca. The weird formatting of the email is to avoid spam.'),
  
  tags$style("#project-grid {
                      display: grid;
                      grid-template-columns: 100px 1fr;
                      grid-gap: 10px;
                      }"),
  div(id = "project-grid",
      div(img(src = "anna_ly.png", style = 'border-radius: 50%', width = '100px')),
      div(h4('Anna Ly'),
          h5('Research Assistant @ University of Toronto'),
          p('Main programmer of this website. An undergraduate statistics & math student at the University 
            of Toronto and an experienced teaching assistant for the Department of Mathematical & 
            Computational Sciences at the Mississauga campus.'),
          tags$div(
            tags$a(href="https://github.com/annahuynhly", "Github"), " | ",
            tags$a(href="https://www.linkedin.com/in/anna-ly-statistics-specialist/", "Linkedin"), " | ",
            tags$a(href="https://scholar.google.ca/citations?user=9w41oS8AAAAJ&hl=en", "Google Scholar")
          ),
      ),
      div(img(src = "mike_evans.png", style = 'border-radius: 50%', width = '100px')),
      div(h4('Michael Evans'),
          h5('Department Chair of Statistical Sciences & Professor @ University of Toronto'),
          p('The main author of the paper and assisted with designing the user layout.'),
          tags$div(
            tags$a(href="https://utstat.toronto.edu/mikevans/", "Personal Website"), " | ",
            tags$a(href="https://scholar.google.ca/citations?user=i4Z5iW4AAAAJ&hl=en", "Google Scholar"),
          ),
      ),
      
      div(img(src = "generic_photo.png", style = 'border-radius: 50%', width = '100px')),
      div(h4('Qiaoyu Liang'),
          h5('Senior Scientist'),
          p('One of the authors of the paper. (There is no image of them online, so the placeholder image 
            was drawn by Anna Ly.)'),
          tags$div(
            tags$a(href="https://www.statistics.utoronto.ca/people/directories/graduate-students/qiaoyu-liang", 
                          "UofT Webpage"), " | ",
            tags$a(href="https://scholar.google.ca/citations?hl=en&user=7puofJYAAAAJ", "Google Scholar"),
          ),
      ),
      
      div(img(src = "luai_labadi.png", style = 'border-radius: 50%', width = '100px')),
      div(h4('Luai Al Labadi'),
          h5('Associate Chair & Assistant Professor @ University of Toronto'),
          p('One of the authors of the paper.'),
          tags$div(
            tags$a(href="https://www.utm.utoronto.ca/math-cs-stats/people/luai-al-labadi", "UofT Webpage"), " | ",
            tags$a(href="https://scholar.google.ca/citations?user=DIin_xEAAAAJ&hl=en", "Google Scholar"),
          ),
      )
  )
)