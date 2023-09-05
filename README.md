# ROC Calculator Based on the Relative Belief Ratio
The website has been deployed using shinyapps.io: https://annaly.shinyapps.io/ROCAnalysisRelativeBelief/

## Note that this is not complete as there are still minor bugs, specifically for using Youden's index as a methodology for computing the optimal cutoff, and the nonparametric Bayesian case needs to be tested in depth. Some more customizable options for the graphs are planned for future releases. However, the majority of these functions should be working.

ROC stands for Receiver Operating Characteristic, which is a method used to evaluate the performance of a binary classifier. A ROC curve plots the true positive rate (TPR) against the false positive rate (FPR). The area under the curve (AUC) is a measure of how well the classifier can discriminate between the two classes.

Our calculator is based on a research paper called "ROC Analyses Based on Measuring Evidence Using the Relative Belief Ratio" by Michael Evans, Luai Al Labadi, and Qiaoyu Liang. All authors are from the University of Toronto. The paper is open-access and can be found here: https://www.mdpi.com/1099-4300/24/12/1710 An undergraduate researcher, (me), programmed this website using R Shiny (and a little bit of HTML and CSS). The computations here utilize the use of a relative belief ratio and the Dirichlet process.

This website allows you to select from two sampling regimes and choose from different types of data, such as a finite valued diagnostic, a binormal diagnostic, and a nonparametric Bayes model. Depending on the sampling regime, inferences are provided. For example, if interest is in the AUC, then the calculator provides an estimate as well as plots of the prior, posterior, and the relative belief ratio, the values of the plausible region, posterior content of the plausible region, and credible regions. The same inferences are also supplied for the optimal cutoff as well as estimates of quantities such as the FPR, FNR, FDR, FNDR, and PPV.

Due to the complexity of the formulas, some of the computations take a long time to run, hence, switching between pages may take awhile. Additionally, I only have a starter subscription for shinyapps.io, so depending on the size of the simulation sample sizes, the website may cause the user to disconnect due to the insufficient amount of RAM allocated. However, if your computer has lots of ram... You can definitely deploy this by yourself without issue. ;)

## Instructions for Deploying Locally (For COMPLETE Newbies!)

This tutorial assumes you have R downloaded on your computer and you are completely new to GitHub. Although to frequent GitHub users this part seems trivial, there are a plethora of statistical and medical researchers that are unaware of the existence of deploying R Shiny applications and how to use GitHub. This is meant to serve as a guide for these researchers.

If you are completely new to GitHub, then I recommend instead of going through the process of downloading Git... You can simply just download the zip files here by going through Code then Download Zip:
![githubpart1](https://github.com/annahuynhly/ROCAnalysisRelativeBeliefRatio/assets/97189987/6d807329-a67c-4efb-87bc-6ea4f2b09773)

Then, un-zip the file anywhere in your computer. Note that you will open this folder so please un-zip it within a good location.

Although you can run R Shiny in base R, in order for all of the features to load properly (mostly images, HTML, and CSS) you should use R Studio instead.
Read the instructions for installing R Studio here: https://posit.co/download/rstudio-desktop/

Once you have R Studio installed, you should install R Shiny by doing the following:
```
install.packages("shiny")
```
Which is relatively straight-forward. The application is quite large so it actually requires a bunch of libraries. All of the necessary libraries can be found in the "app.R" file - to see which ones to install, look for those contained within library. I.e., DT, varhandle, etc...

As of this version, the following are required if you don't have it already:
```
install.packages("DT")
install.packages("varhandle")
install.packages("tableHTML")
install.packages("shinyanimate")
install.packages("shinycssloaders")
install.packages("rBeta2009")
install.packages("LaplacesDemon")
install.packages("tidyverse")
```
Note that for some reason if there are actual errors with installing these packages (not warnings), try to run R Studio in adminstrator mode instead (if you are using Windows).

Now while you have the "app.R" file open (hopefully in R Studio!) in the "source" pane (in default R Studio, it is the top left quandrant) you should see a "Run App" button here:
![githubpart2](https://github.com/annahuynhly/ROCAnalysisRelativeBeliefRatio/assets/97189987/8dead77e-aaee-4f8f-bab6-cfc6edc6a9a4)

Click on that, and the application should run. Sometimes there are bugs in R Studio where for some reason, it gives a cryptic error messsage. An easy solution is to highlight the whole text in app.R and then use ctrl + enter (on Windows). The command for Mac should be command + return.
