
home_page = fluidPage(
  titlePanel("Welcome to our ROC Calculator!"),
  hr(),
  sidebarLayout(
    mainPanel(
      p("ROC stands for Receiver Operating Characteristic, which is a method used to 
        evaluate the performance of a binary classifier. A ROC curve plots the true positive 
        rate (TPR) against the false positive rate (FPR). 
        The area under the curve (AUC) is a measure of how well the classifier can discriminate 
        between the two classes."),
      br(),
      tags$div(
        "Our calculator is based on a research paper called",
        tags$a(href="https://www.mdpi.com/1099-4300/24/12/1710", 
               "\"ROC Analyses Based on Measuring Evidence Using the Relative Belief Ratio\""),
        "by Michael Evans, Luai Al Labadi, and Qiaoyu Liang. All authors are from the 
        University of Toronto. An undergraduate researcher, Anna Ly, programmed this website 
        using R Shiny. The computations here utilize the use of a relative belief ratio 
        and the Dirichlet process."
      ),
      br(),
      p("This website allows you to select from two sampling regimes and choose from 
        different types of data, such as a finite valued diagnostic, a binormal 
        diagnostic, and a nonparametric Bayes model. Depending on the sampling regime, 
        inferences are provided. For example, if interest is in the AUC, then the calculator 
        provides an estimate as well as plots of the prior, posterior, and the 
        relative belief ratio, the values of the plausible region, posterior 
        content of the plausible region, and credible region. The same
        inferences are also supplied for the optimal cutoff as well as estimates of quantities
        such as the FPR, FNR, FDR, FNDR, and PPV."),
      br(),
      p("Depending on the Monte Carlo sample sizes, some of the computations may take a long time to 
        run, hence, switching between pages may take awhile."),
      br(),
      p("Ready to try it out? Click on one of the pages above and begin with the \"Getting 
        Started\" page!"),
      br(),
      #br(style = "line-height:2;"),
      tags$div(
        "The photo of the calculator is by Mikhail Nilov from Pexels:",
        tags$a(href="https://www.pexels.com/photo/wood-couple-people-woman-6963017/", 
               "Click here to access.")
      ),
    ),
    sidebarPanel(width = 4,
      div(id = "calculator", 
          img(src = "calculator.jpeg", style = 'border-radius: 50%', width = '420px'),
          style="text-align: center;"
      )
    ),
  ),
  br(style = "line-height:1;")
)