# Source all of the different pages.
# They are ordered.

source("./pages/home_page.R")
source("./pages/conditionalROC_1_page.R")
source("./pages/conditionalAUCbig_page.R")
source("./pages/ex1prog_page.R")
source("./pages/readdata_page.R")
source("./pages/realdataROC_1_page.R")
source("./pages/ROC_page.R")
source("./pages/contact_page.R")

router = make_router(
  route("/", home_page),
  route("conditionalROC_1", page_conditionalROC_1),
  route("conditionalAUCbig", page_conditionalAUCbig),
  route("ex1prog", page_ex1prog),
  route("readdata", page_readdata),
  route("realdataROC_1", page_realdataROC_1),
  route("ROC", page_ROC),
  route("contact", contact_page)
)

ui = fluidPage(
  theme = "main.css",
  tags$ul(
    tags$li(a(href = route_link("/"), "Home Page")),
    tags$li(a(href = route_link("conditionalROC_1"), "Section 3.2: conditionalROC")),
    tags$li(a(href = route_link("conditionalAUCbig"), "Section 3.2: conditionalROCbig")),
    tags$li(a(href = route_link("ex1prog"), "Section 3.2: ex1prog")),
    tags$li(a(href = route_link("readdata"), "Section 3.2: readdata")),
    tags$li(a(href = route_link("realdataROC_1"), "Section 3.2: realdataROC")),
    tags$li(a(href = route_link("ROC"), "Section 3.2: ROC")),
    tags$li(a(href = route_link("contact"), "Contact Page"))
  ),
  router$ui
)