# ROC Calculator Based on the Relative Belief Ratio
The website has been deployed using shinyapps.io: https://annaly.shinyapps.io/ROCAnalysisRelativeBelief/

ROC stands for Receiver Operating Characteristic, which is a graphical method to evaluate the performance of a binary classifier. A ROC curve plots the true positive rate (TPR) against the false positive rate (FPR) at different threshold levels. The area under the curve (AUC) is a measure of how well the classifier can discriminate between the two classes.

Our calculator is based on a research paper called "ROC Analyses Based on Measuring Evidence Using the Relative Belief Ratio" by Michael Evans, Luai Al Labadi, and Qiaoyu Liang. All authors are from the University of Toronto. The paper is open-access and can be found here: https://www.mdpi.com/1099-4300/24/12/1710 An undergraduate researcher, (me), programmed this website using R Shiny (and a little bit of HTML and CSS). The computations here utilize the use of a relative belief ratio and the dirichlet process.

This website allows you to select from three sampling regimes and choose from different types of data, such as the finite valued diagnostic, the binormal diagnostic, and the nonparametric bayes model. Depending on the sampling regime, inferences for the sampling regime are provided. The calculator provides plots, the values of the prior, posterior, and the relative belief ratio, the values of the plausible region, posterior content of the plausible region, credible region, error characteristics (such as FPR, FNR, Error, FDR, FNDR, PPV), and plots that show the plots of the optimal cutoff.

Due to the complexity of the formulas, some of the computations take a long time to run, hence, switching between pages may take awhile. Additionally, I only have a starter subscription for shinyapps.io, so depending on the size of the simulation sample sizes, the website may cause the user to disconnect due to the insufficient amount of RAM allocated. However, if your computer has lots of ram... You can definitely deploy this by yourself without issue. ;)

## Instructions for Deploying Locally (For Newbies!)

This tutorial assumes you have R downloaded on your computer and you are completely new to GitHub. Although to frequent GitHub users this part seems trivial, there are a plethora of statistical and medical researchers that are unaware of the existence of deploying R Shiny applications and how to use GitHub. This is meant to serve as a guide for these researchers.

If you are completely new to GitHub, then I recommend instead of going through the process of downloading Git... You can simply just download the zip files here by going through Code then Download Zip:
![image](https://github.com/annahuynhly/ROCAnalysis_RelativeBelief/assets/97189987/abfaeac1-6ee7-4941-aa68-f4dede1ea427)

Then, un-zip the file anywhere in your computer.

Although you can run R Shiny in base R, in order for all of the features to load properly (mostly images, HTML, and CSS) you should use R Studio instead.
Read the instructions for installing R Studio here: https://posit.co/download/rstudio-desktop/

Once you have R Studio installed, you should install R Shiny by doing the following:
```
install.packages("shiny")
```
Which is relatively straight-forward. The application is quite large so it actually requires a bunch of libraries. All of the necessary libraries can be found in the "app.R" file - to see which ones to install, look for those contained within library. I.e., DT, varhandle, etc...

Now while you have the "app.R" file open (hopefully in R Studio!) in the "source" pane (in default R Studio, it is the top left quandrant) you should see a "Run App" button here:

![image](https://github.com/annahuynhly/ROCAnalysis_RelativeBelief/assets/97189987/3492ef96-727d-4696-95ca-9de8a8f060e2)

Click on that, and the application should run.




