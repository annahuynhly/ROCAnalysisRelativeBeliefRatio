# contact page

contact_page = div(
  titlePanel("Contact"),
  p("This website was constructed by Anna Ly. If you find any bugs on this website, please contact:
    [firstname]huynh.[lastname]@mail.utoronto.ca"),
  p("The weird formatting of the email is to avoid spam."),
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
    tags$a(href="https://utstat.toronto.edu/mikevans/", "Website")),
  tags$div(
    "Qiaoyu Liang: ",
    tags$a(href="https://www.statistics.utoronto.ca/people/directories/graduate-students/qiaoyu-liang", "UofT Webpage")),
  tags$div(
    "Luai Al Labadi: ",
    tags$a(href="https://www.utm.utoronto.ca/math-cs-stats/people/luai-al-labadi", "UofT Webpage")),
)