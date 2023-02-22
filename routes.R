
# Source to all different tabs

# Helper functions
source("./pages/section3.2/helperfunctions3.2.R")
source("./pages/section3.3/helperfunctions3.3.R")
source("./pages/section3.4/helperfunctions3.4.R")

# Other Pages
source("./pages/home_page.R")
source("./pages/contact_page.R")

# Section 3.1
source("./pages/section3.1/RB_setup_page.R")

# Section 3.2
source("./pages/section3.2/theAUC_page.R")
source("./pages/section3.2/definitions.R")

# Section 3.3
source("./pages/section3.3/variables3.3_page.R")
source("./pages/section3.3/binormalAUCequalvariance_page.R")
source("./pages/section3.3/binormalAUCunequalvariance_page.R")
#source("./pages/section3.3/binormalcoptequalvariance_page.R")
#source("./pages/section3.3/binormalcoptunequalvariance_page.R")
#source("./pages/section3.3/coptpriorprevalence_page.R")
#source("./pages/section3.3/plotROC_page.R")

# Section 3.4
source("./pages/section3.4/variables3.4_page.R")
source("./pages/section3.4/betaprior_page.R")
source("./pages/section3.4/BNPAUC_page.R")
#source("./pages/section3.4/BNPcfixedMales_page.R")
#source("./pages/section3.4/BNPcoptFemales_page.R")
#source("./pages/section3.4/BNPcoptMales_page.R")
#source("./pages/section3.4/BNPdata_page.R")
#source("./pages/section3.4/empiricals_page.R")
#source("./pages/section3.4/itsforgammaprior_page.R")
#source("./pages/section3.4/smoother_page.R")
#source("./pages/section3.4/storeBNPcoptFemales_page.R")

# Relevant files, datasets
gender_covid_dataset = read.csv("./pages/section3.4/gender_covid.csv")

Females_Covid_Data = subset(gender_covid_dataset, gender_covid_dataset$Gender=='female', select=c(Age, Death))
Males_Covid_Data = subset(gender_covid_dataset, gender_covid_dataset$Gender=='male', select=c(Age, Death))





