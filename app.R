# List of packages
packages <- c(
  "shiny", "shinythemes", "shinycssloaders", "plotly", "ggplot2", "caret", 
  "dplyr", "sf", "rnaturalearth", "leaflet", "countrycode", "gridExtra", 
  "grid", "cowplot", "randomForest", "pROC", "rpart.plot", "RColorBrewer", "markdown",
  "rnaturalearthdata", "rattle","doParallel"
)

# Check and install packages if not already installed
for(pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

source('functions_pf.R')

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

ui <- fluidPage(
  includeCSS("pf_styles.css"),
  tags$script(HTML("
    function toggleTheme() {
      const body = document.body;
      body.dataset.theme = body.dataset.theme === 'dark' ? 'light' : 'dark';
    }
  ")),
  
  titlePanel(
    div("Alzheimer's disease prediction model", 
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$button(
          id = "themeToggle",
          onclick = "toggleTheme()",
          "🌓",
          class = "theme-toggle-btn"
        )
    ), 
    windowTitle = "Alzheimer Prediction Demo"
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Navigation - Model overview"),
      selectInput("model", "Choose prediction model:",
                  choices = c('Random Forest',
                              'Recursive partioning & regression trees',
                              'Neural Network')),
      selectInput("space", "Choose PCA plotting dimension:",
                  choices = c('2D',
                              '3D')),
      hr(),
      tags$div(tags$div(style = "text-align: center",h5('Dataset Information')),
               hr(),
               HTML(paste("<strong>Dataset Name:</strong> ",'alzheimers_prediction_dataset','<br>')),
               HTML(paste("<strong>Format:</strong>",'.csv','<br>')),
               HTML(paste("<strong>Sample Size:</strong> ",'74 283','<br>')),
               HTML(paste("<strong>Number of Variables:</strong> ", '25','<br>')),
               HTML(paste("<strong>File Size:</strong>",'10.12 MB','<br>')),
               HTML(paste('<strong>Source:</strong> <a href="https://www.kaggle.com/datasets/ankushpanday1/alzheimers-prediction-dataset-global" target="_blank">Kaggle</a>','<br>')),
               HTML(paste("<strong>Downloaded on:</strong>",'February 9th, 2025','<br>'))
               ),
      hr(),
      tags$div(style = "text-align: center",h5('R Libraries')),
      hr(),
      tags$ul(style = "list-style-type: none; padding-left: 0;", 
              lapply(packages, function(lib) {
                tags$li(style = "display: inline-block; margin: 5px; text-align: center;",
                        tags$span(
                          style = "border-radius: 20%; background-color: var(--background-color); padding: 5px;",
                          tags$b(lib)))})),
      hr(),
      tags$div(style = "text-align: center",h5('About the application')),
      hr(),
      p('This application concerns a demo, featuring three machine learning models to predict Alzheimer\'s disease
        status from 24 variables, provided in a public kaggle dataset. The models were chosen for their particular performant
        inference, an important metric given the application is web hosted in a limited and free environment. The three models also have their
        individual merits. Random Forests cope well with overfitting, due to it being an ensemble methodology. Recursive Partioning and
        Regression Trees have the benefit of interpretability through decision trees. Neural Networks on the other hand have an advantage by
        capturing complex non-linear relationships between variables.')
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel("Data exploration",
                 fluidRow(
                   column(12,div(class = "custom-panel",
                                 h3("Distribution of categorical variables"),
                                 withSpinner(plotlyOutput("pies")))),
                   column(8,div(class = "custom-panel",
                                h3("Distribution of numerical variables"),
                                withSpinner(plotlyOutput("bars")))),
                   column(4,div(class = "custom-panel",
                                h3("Participant distribution per country"),
                                withSpinner(leafletOutput("map")))
                   )
                 )
        ),
        tabPanel("Model overview",
                 fluidRow(
                   column(6, div(class = "custom-panel",
                                 h3("Model accuracy"),
                                 withSpinner(plotlyOutput("metric1")))),
                   column(6, div(class = "custom-panel",
                                 h3("Model metrics"),
                                 withSpinner(plotlyOutput("metric2"))))
                 ),
                 fluidRow(
                   column(8, div(class = "custom-panel",
                                  h3("Model accuracy in principal component vector space"),
                                  withSpinner(plotlyOutput("metric3")))),
                   column(4, div(class = "custom-panel",
                                 h3("Identifying dominant variables"),
                                 withSpinner(tableOutput("metric4"))))
                 )
        ),
        tabPanel('Model comparison',
                 fluidRow(
                   column(12,div(class = "custom-panel",
                                 h3("Receiver-Operating Characteristics (ROC) curves &
                                    Area Under the Curve (AUC) values"),
                                 withSpinner(plotlyOutput("roc")))),
                   column(12,div(class = "custom-panel",
                                 h3("Decision making - Recursive partitioning & regression trees"),
                                 withSpinner(plotOutput("rpart"))))
                 )
      ),
      tabPanel('Model training code',
               includeMarkdown('www/alzheimers.Rmd')
      ),
      tabPanel('Functions code',
               includeMarkdown('www/application_functions.Rmd')
      ),
    )
  )
)
)
  

server <- function(input, output) {
  
  models<-readRDS("all_models.RDS")
  trainData<-read.csv('trainData.csv')
  testData<-read.csv('testData.csv')
  
  alzheimer_data<-read.csv('alzheimers_prediction_dataset.csv')
  
  testData <- testData %>%
    mutate(across(where(is.character), as.factor))
  

  output$metric1<-renderPlotly({
    preds<-make_predictions(models[input$model],testData)
    confm<-calculate_confusion_matrix(preds$actual,preds$pred)
    p<-plot_confusion_matrix(confm)
    ggplotly(p)
  })
  
  output$metric2<-renderPlotly({
    plot_model_accuracy(models[input$model],testData)
  })
  output$metric3<-renderPlotly({
    pca_accuracy_plotter(testData,models[input$model],input$space)
  })
  output$metric4<-renderTable({
    extract_dominant_variables(pca_result,input$space,4)
  })
  
  output$pies<-renderPlotly({
    plot_categorical_variables(alzheimer_data)
  })
  output$bars<-renderPlotly({
    plot_numerical_variables(alzheimer_data)
  })
  
  output$map<-renderLeaflet({
    plot_country_count(alzheimer_data)
  })
  
  output$roc<-renderPlotly({
    create_roc_plots(models,testData)
  })
  
  output$rpart<-renderPlot({
    rpart_plot(models[[2]])
  },bg='transparent')
}

shinyApp(ui = ui, server = server)