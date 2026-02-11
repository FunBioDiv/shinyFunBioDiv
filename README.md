# shinyFunBioDiv

Shiny app for exploring [FunBioDiv](https://www.fondationbiodiversite.fr/en/the-frb-in-action/programs-and-projects/le-cesab/funbiodiv/) datasets. You can run the shinyApp locally, or use it [online](https://rfrelat-cesab.shinyapps.io/shinyFunBioDiv/).



## Run the shiny-app locally

First, you need to install this R-package with the following step:

```r
## Install < remotes > package (if not already installed) ----
if (!requireNamespace(c("remotes"), quietly = TRUE)) {
  install.packages(c("remotes"))
}

## Install < shinyFunBioDiv > from GitHub ----
remotes::install_github("FRBCesab/shinyFunBioDiv")
```

Then you can run the Shiny app locally with the function `runShiny()`:  

```r
## load < shinyFunBioDiv> package
library(shinyFunBioDiv)

# run the Shiny app locally
runShiny()
```



## Update metadata and export on shinyapps.io

To follow this procedure, you need to clone this repository in Github. Then follow the three steps below.

#### 1. Update the metadata

```r
devtools::load_all()
source("analysis/01_prepare_data.R")
source("analysis/02_contingency.R")
```

#### 2. Test the shiny app

```r
# run the Shiny app locally
runShiny()
```

#### 3. Deploy the shiny app to shinyapps.io

```r
# deploy the shinyapp to online server
rsconnect::deployApp(
    appDir = "app",
    appFiles = rsconnect::listDeploymentFiles("app"),
    appName = "shinyFunBioDiv",
    appTitle = "FunBioDiv data explorer"
)
```


## Create the metadata Dashboard

```r
quarto::quarto_render("analysis/04_explore.qmd")
file.rename("analysis/04_explore.html", "docs/metadata_Funbiodiv.html")
```