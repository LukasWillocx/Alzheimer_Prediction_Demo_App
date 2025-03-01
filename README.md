## Alzheimer prediction (demo) Application

This folder contains all files as it relates to running the Alzheimer_Prediction_Demo_App, featured in the user's portfolio. 

### Files, scripts and functions

* app.R: **the code to run the actual application**

* pf_styles.css: **css file to address the layout, color schemes, dark mode, fonts, etc of the application**

* functions_pf.R: **A script containing all the necessary functions, called by app.R, mostly pertaining to the application's plots**

* all_models.RDS: **saved list of the three featured trained models, generated in alzheimers.Rmd. These models are particularly large (~43MB) and would be a huge performance hit if they were to be calculated on the fly within the application.** 

* application_functions.Rmd / alzheimers.Rmd: **R scripts in markdown format to be embedded in the app. It concerns the functions_pf.R and the original model training and data exploration. This allows the review of the formatted code contained within the app.**

* alzheimer_prediction_dataset.csv / testData.csv / trainData.csv : **the original dataset and its subdivided test and training set, as generated in alzheimer.Rmd. **

### Running the app

The app can be ran from the app.R file, provided all aforementioned files are present. If the embedded markdown files are not provided in the www subfolder, they will not be incorporated, but this won't affect any of the app's functionality. 

#### Dependent libraties
* shiny 
* ggplot2
* dplyr
* plotly 
* caret
* sf
* rnaturalearth
* leaflet
* countrycode
* grid
* gridExtra
* cowplot
* randomForest
* pROC
* rpart.plot
* RColorBrewer
* shinythemes
* shinycssloaders
* lattice
* markdown
* shinylive
