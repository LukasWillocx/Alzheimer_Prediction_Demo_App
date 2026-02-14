# Horizontal Navbar App - Alzheimer's Disease Prediction Model

# List of packages
all_packages <- c(
  "shiny", "shinythemes", "shinycssloaders", "plotly", "ggplot2", "caret", 
  "dplyr", "sf", "rnaturalearth", "leaflet", "bslib", "countrycode", "gridExtra", 
  "grid", "cowplot", "randomForest", "pROC", "rpart.plot", "RColorBrewer", "markdown",
  "rattle" , "rnaturalearthdata", "doParallel", "luwitemplate"
)

packages <- c(
  "shiny", "shinycssloaders", "plotly", "ggplot2", 'caret',
  "dplyr", "sf", "rnaturalearth", "leaflet", "bslib", "countrycode", "gridExtra", 
  "grid", "randomForest", "pROC", "rpart.plot", "markdown",
  "rattle" , "rnaturalearthdata", "luwitemplate"
)

for(pkg in packages) {
  library(pkg, character.only = TRUE)
}

source('functions_pf.R')

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# UI
ui <- bslib::page_navbar(
  title = "Alzheimer's Disease Prediction Model",
  theme = my_theme(),
  dark_mode_css(),
  window_title = "Alzheimer Prediction Demo",
  
  # Data Exploration Tab
  nav_panel(
    "Data Exploration",
    layout_sidebar(
      sidebar = sidebar(
        width = 600,
        tags$div(
          tags$div(style = "text-align: center", h4('Dataset Information')),
          hr(),
          HTML(paste("<strong>Dataset:</strong> ", 'alzheimers_prediction_dataset', '<br>')),
          HTML(paste("<strong>Format:</strong>", '.csv', '<br>')),
          HTML(paste("<strong>Sample Size:</strong> ", '74 283', '<br>')),
          HTML(paste("<strong>Number of Variables:</strong> ", '25', '<br>')),
          HTML(paste("<strong>File Size:</strong>", '10.12 MB', '<br>')),
          HTML(paste('<strong>Source:</strong> <a href="https://www.kaggle.com/datasets/ankushpanday1/alzheimers-prediction-dataset-global" target="_blank">Kaggle</a>', '<br>')),
          HTML(paste("<strong>Downloaded on:</strong>", 'February 9th, 2025', '<br>'))
        ),
        tags$div(
          tags$div(style = "text-align: center", h5('R Libraries')),
          hr(),
          tags$ul(
            style = "list-style-type: none; padding-left: 0;", 
            lapply(all_packages, function(lib) {
              # Create documentation URL
              if (lib == "luwitemplate") {
                doc_url <- "https://github.com/LukasWillocx/luwitemplate"
              } else {
                doc_url <- paste0("https://CRAN.R-project.org/package=", lib)
              }
              
              tags$li(
                style = "display: inline-block; margin: 5px; text-align: center;",
                tags$a(
                  href = doc_url,
                  target = "_blank",
                  style = "text-decoration: none;",
                  tags$span(
                    style = "border-radius: 15px; background-color: rgba(128, 128, 128, 0.1);
                    border: 1px solid rgba(128, 128, 128, 0.3); padding: 5px 10px;
                    display: inline-block; cursor: pointer;
                    transition: background-color 0.2s, border-color 0.2s;",
                    onmouseover = "this.style.backgroundColor='rgba(128, 128, 128, 0.2)'; this.style.borderColor='rgba(128, 128, 128, 0.5)';",
                    onmouseout = "this.style.backgroundColor='rgba(128, 128, 128, 0.1)'; this.style.borderColor='rgba(128, 128, 128, 0.3)';",
                    tags$b(lib)
                  )
                )
              )
            })
          )
        ),
        tags$div(
          tags$div(style = "text-align: center", h5('About the Application')),
          hr(),
          p('This demonstration features three non-linear machine learning models to predict Alzheimer\'s disease
            status from 24, predominantly demographic variables. The models were chosen for their particular performant inference. The three models also have their
            individual merits. Recursive Partioning and Regression Trees have the benefit of interpretability through a single decision tree. Random Forests cope well with overfitting, due to it being an ensemble methodology, through the generation of multiple trees.
            Neural Networks on the other hand have an advantage by
            capturing complex interactions, but is the most black-box in nature.')
        )
      ),
      layout_columns(
        col_widths = c(12, 8, 4),
        fill = T,
        card(
          full_screen = T,
          card_header("Distribution of Categorical Variables"),
          card_body(
            withSpinner(plotlyOutput("pies", height = "360px"))
          )
        ),
        card(
          full_screen = T,
          card_header("Distribution of Numerical Variables"),
          card_body(
            withSpinner(plotlyOutput("bars", height = "360px"))
          )
        ),
        card(
          full_screen = T,
          card_header("Participant Distribution per Country"),
          card_body(
            withSpinner(leafletOutput("map", height = "360px"))
          )
        )
      )
    )
  ),
  
  # Model Overview Tab
  nav_panel(
    "Model Overview",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Model selection"),
        selectInput("model", "Choose prediction model:",
                    choices = c('Random Forest',
                                'Recursive partioning & regression trees',
                                'Neural Network')),
      ),
      layout_columns(
        col_widths = c(6, 6,12),
        fill = FALSE,
        card(
          full_screen = T,
          card_header("Model Accuracy"),
          card_body(
            withSpinner(plotlyOutput("metric1", height = "360px"))
          )
        ),
        card(
          full_screen = T,
          card_header("Model Metrics"),
          card_body(
            withSpinner(plotlyOutput("metric2", height = "360px"))
          )
        ),
        card(
          full_screen = T,
          card_header("Receiver-Operating Characteristics (ROC) Curves & Area Under the Curve (AUC) Values"),
          card_body(
            withSpinner(plotlyOutput("roc", height = "400px"))
          )
        ),
      )
    )
  ),
  
  # Model Comparison Tab
  nav_panel(
    "Rpart - decision tree",
      card(
        full_screen = T,
        card_header("Decision Making - Recursive Partitioning & Regression Trees"),
        card_body(withSpinner(plotOutput("rpart", height = "800px"))),
        card_footer('Intuitively, Age, Genetic Risk (familial history & ApoE4 genotype) and Country surface as important variables in this determination scheme'),
      )
  ),
  
  # Model Training Code Tab
  nav_panel(
    "Model Training Code",
    card(
      card_body(
        includeMarkdown('www/alzheimers.Rmd')
      )
    )
  ),
  
  # Functions Code Tab
  nav_panel(
    "Functions Code",
    card(
      card_body(
        includeMarkdown('www/application_functions.Rmd')
      )
    )
  ),
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode")),
)

# Server
server <- function(input, output, session) {
  
  colors <- get_theme_colors()
  dm <- use_dark_mode(input, session)
  
  models <- readRDS("all_models.RDS")
  trainData <- read.csv('trainData.csv')
  testData <- read.csv('testData.csv')
  
  alzheimer_data <- read.csv('alzheimers_prediction_dataset.csv')
  
  testData <- testData %>%
    mutate(across(where(is.character), as.factor))
  
  
  output$metric1 <- renderPlotly({
    preds <- make_predictions(models[input$model], testData)
    confm <- calculate_confusion_matrix(preds$actual, preds$pred)
    p <- plot_confusion_matrix(confm, theme = dm$theme())
    luwi_ggplotly(p, theme=dm$theme(), tooltip = c("x", "y", "text"))
  })
  
  output$metric2 <- renderPlotly({
    p <- plot_model_accuracy(models[input$model], testData, theme = dm$theme())
    luwi_ggplotly(p, theme=dm$theme(), tooltip = c("x", "y", "text"))
  })
  
  output$pies <- renderPlotly({
    plot_categorical_variables(alzheimer_data, theme= dm$theme())
  })
  
  output$bars <- renderPlotly({
    plot_numerical_variables(alzheimer_data, theme = dm$theme())
  })
  
  output$map <- renderLeaflet({
    plot_country_count(alzheimer_data)
  })
  
  output$roc <- renderPlotly({
    create_roc_plots(models, testData, theme = dm$theme())
  })
  
  output$rpart <- renderPlot({
    rpart_plot(models[[2]], theme = dm$theme())
  }, bg = 'transparent')
}

# Run the application
shinyApp(ui = ui, server = server)