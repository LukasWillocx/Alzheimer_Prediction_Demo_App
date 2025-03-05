# functions.R
# Dependent libraries in app.R file

# Function to make predictions using the trained model and test data
make_predictions <- function(model, test_data) {
  
  acts<-data.frame(test_data$Alzheimer.s.Diagnosis)
  
  preds <- data.frame(predict(model, 
                              newdata =test_data))
  
  preds <- preds
  
  predictions<-cbind(acts,preds)
  names(predictions)<-c('actual','pred')
  
  predictions$actual<-factor(predictions$actual,levels=c('Alzheimer\'s','healthy'))
  predictions$pred <-factor(predictions$pred,levels=c('Alzheimer\'s','healthy'))
  
  return(predictions)
}

# Function to calculate confusion matrix elements
calculate_confusion_matrix <- function(true_labels, predicted_labels) {
  tp <- sum((true_labels == 'Alzheimer\'s') & (predicted_labels == 'Alzheimer\'s'))
  tn <- sum((true_labels == 'healthy') & (predicted_labels == 'healthy'))
  fp <- sum((true_labels == 'healthy') & (predicted_labels == 'Alzheimer\'s'))
  fn <- sum((true_labels == 'Alzheimer\'s') & (predicted_labels == 'healthy'))
  
  return(list(tp = tp, tn = tn, fp = fp, fn = fn))
}

# Function to plot a bar graph of true positives, true negatives, false positives, and false negatives
plot_confusion_matrix <- function(conf_matrix) {
  # Create a data frame for plotting
  conf_data <- data.frame(
    Category = c("True Positives", "True Negatives", "False Positives", "False Negatives"),
    acc_id=c(1,1,0,0),
    Count = c(conf_matrix$tp, conf_matrix$tn, conf_matrix$fp, conf_matrix$fn)
  )
  # Define the fill colors for each category
  color_palette <- c("False Negatives" = "#FDCDAC",  
                     "False Positives" = '#FBB4AE',  
                     "True Negatives" = "#B3CDE0",    
                     "True Positives" = "#A8DDB5") 
  # Create the bar plot
  ggplot(conf_data, aes(x = Category, y = Count, fill = Category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=color_palette) +
    theme_minimal() +  
    theme(
      plot.background = element_rect(fill = NA), # Make the plot background transparent
      panel.background = element_rect(fill = NA), 
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y =element_line(color = "#BEBEBE"),
      legend.position='none',
      axis.title = element_text(color = "#7aa6a1"),
      axis.text = element_text(color = "#7aa6a1"),
      plot.title = element_text(color = "#7aa6a1"),
    ) +
    labs(title = "", x = "", y = "")
}

plot_model_accuracy<-function(model,test_data){
  
  pred_data<-make_predictions(model,test_data)

  cm <- confusionMatrix(pred_data$pred, pred_data$actual)
  
  # Extract accuracy, sensitivity (recall), and specificity from the confusion matrix
  accuracy <- cm$overall['Accuracy']
  sensitivity <- cm$byClass['Sensitivity']
  specificity <- cm$byClass['Specificity']
  kappa <- cm$overall['Kappa']
  
  # Create a data frame for plotting
  metrics_df <- data.frame(
    Metric = c("Accuracy", "Sensitivity", "Specificity", "Kappa"),
    Value = c(accuracy, sensitivity, specificity, kappa)
  )
  
ggplot(metrics_df, aes(x = Metric, y = Value)) +
    geom_bar(stat = "identity",aes(fill=Value)) +
    coord_flip() +  # Flip the coordinates to make bars horizontal
    scale_fill_gradient(low = "#A8bDB5", high = "#A8DDB5")  + 
    scale_y_continuous(limits = c(0, 1)) +
  
  theme_minimal() +  # Start with a minimal theme
  theme(
    plot.background = element_rect(fill = NA), # Make the plot background transparent
    panel.background = element_rect(fill = NA), 
    legend.position='none',
    
    # Change text color to #7aa6a1 for all relevant elements
    axis.title = element_text(color = "#7aa6a1"),
    axis.text = element_text(color = "#7aa6a1"),
    plot.title = element_text(color = "#7aa6a1"),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor.y = element_line(color = "#BEBEBE"),
  ) +
    labs(title = "",
         x = "",
         y = "") +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))  # Rotate x-axis labels for readability
  
}


pca_accuracy_plotter<-function(test_data,model,space='3D'){
  
  test_data_dummy<-test_data %>%
    select(-Alzheimer.s.Diagnosis)
  test_data_dummy<-model.matrix(~ . - 1, data = test_data_dummy)
  
  pred<-make_predictions(model,test_data)
  pca_result<<-prcomp(test_data_dummy, # perform PCA and cache results
                     scale.=T)
  pca_df <- as.data.frame(pca_result$x[, 1:3])
  pca_df$Diagnosis <- test_data[["Alzheimer.s.Diagnosis"]]
  pred <-  pred %>%
    mutate(accuracy = case_when(
      actual == 'Alzheimer\'s' & pred == 'Alzheimer\'s' ~ 'True Positive',
      actual == 'healthy' & pred == 'healthy' ~ 'True Negative',
      actual == 'healthy' & pred == 'Alzheimer\'s' ~ 'False Positive',
      actual == 'Alzheimer\'s' & pred == 'healthy' ~ 'False Negative'))
  
  pca_df$accuracy<-pred$accuracy
  
  color_palette <- c(
    `True Positive` = "#A8DDB5",
    `True Negative` = "#B3CDE0",
    `False Positive` = "#FBB4AE",
    `False Negative` = "#FDCDAC"
  )
  
  if (space == '2D'){
    # Plot the PCA results using ggplot2
    set.seed(123)
    plot<-ggplot(pca_df%>% sample_frac(size = 0.07), aes(x = PC1, y = PC2)) +
      geom_point(aes(color = accuracy), size=3.5, alpha = 0.7) +
      scale_color_manual(values = color_palette) +
      guides(colour = guide_legend(title = NULL))+
      labs(title = "",
           x = "PC1",
           y = "PC2") +
      theme_minimal() +  # Start with a minimal theme
      theme(
        plot.background = element_rect(fill = NA), # Make the plot background transparent
        panel.background = element_rect(fill = NA), 
        
        # Change text color to #7aa6a1 for all relevant elements
        axis.title = element_text(color = "#7aa6a1"),
        axis.text = element_text(color = "#7aa6a1"),
        plot.title = element_text(color = "#7aa6a1"),
        legend.text = element_text(color = "#7aa6a1"),
        legend.position = "right",                      # Position legend on right side
        legend.justification = c(1, 0.5),               # Center vertically
      ) 
    return(plot)
  }
  if (space =='3D'){
    plot<-plot_ly(pca_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~accuracy,
                  colors=color_palette,
                  type = 'scatter3d', mode = 'markers') %>%
      layout(
        paper_bgcolor = "rgba(0, 0, 0, 0)",  # Transparent background
        plot_bgcolor = "rgba(0, 0, 0, 0)",
        legend = list(font = list(color = "#7aa6a1")),  # Legend color
        scene = list(
          xaxis = list(
            title='PC1',
            titlefont = list(color = "#7aa6a1"),  # Axis text and tick colors
            showticklabels = TRUE,
            tickfont = list(color = "#7aa6a1")
          ),
          yaxis = list(
            title='PC2',
            titlefont = list(color = "#7aa6a1"),
            showticklabels = TRUE,
            tickfont = list(color = "#7aa6a1")
          ),
          zaxis = list(
            title='PC3',
            titlefont = list(color = "#7aa6a1"),
            showticklabels = TRUE,
            tickfont = list(color = "#7aa6a1")
          )
        )
      )
    return(plot)
  }
}


extract_dominant_variables <- function(pca_result, space='2D', n = 5) {
  
  # Extract the rotation matrix (loadings)
  loadings <- pca_result$rotation
  
  # Remove the last character 'D' and convert to numeric
  number <- as.numeric(sub("D$", "", space))
  
  # Initialize an empty list to store the results
  results_list <- list()
  
  for (i in 1:number){
  component_loadings <- data.frame(round(loadings[, i, drop = FALSE],2))
  component_loadings$variable <-rownames(component_loadings)
  component_loadings <- component_loadings %>% 
    arrange(desc(abs(.[[1]])))%>%
    select(variable,everything())
     
  results_list[[i]] <-head(component_loadings,n)
  
  
  }
  var_list<-bind_rows(results_list)
  return(lapply(var_list, function(x) {
    x[is.na(x)] <- " "
    return(x)}))
}

######################EDA- functions ###############################

plot_categorical_variables <- function(df) {
  # Convert character columns to factors if they aren't already
  df <- df %>%
    mutate(across(where(is.character), as.factor))
  
  # Remove the 'Country' column if it exists
  df <- select(df, -Country)
  df<-df%>%
    select(-where(is.numeric))
  # Identify categorical columns (factors)
  categorical_cols <- sapply(df, is.factor)

  # Create plots for each categorical column using a for loop
  plots <- list()  # Initialize empty list to store plots
  num_plots <- sum(categorical_cols)  # Number of categorical variables
  
  for(var in names(categorical_cols)) {
    
    data <- df %>%
      count(!!sym(var))
    
    # coloration of outcome variable
    data$group <- data[[var]]  # stores factor names 
    if (var == 'Alzheimer.s.Diagnosis'){
      text_color<-'darkorange'
    }else{
      text_color<-"#7aa6a1"
    }
    
    p <- ggplot(data, aes(x=var,y = n, fill = group)) +
      geom_col(position = 'stack', width = 1) +
      labs(x='',y='')+
      geom_text(aes(label = group),
                position = position_stack(vjust = 0.5),
                size=3)+
      theme(
        legend.position = 'none',
        plot.background = element_rect(fill = NA), # Make the plot background transparent
        panel.background = element_rect(fill = NA), 
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        # Change text color for all relevant elements + identify outcome variable
        axis.title = element_text(color = text_color),
        axis.text = element_text(color = text_color),
        axis.ticks.y = element_line(color = text_color),
        axis.ticks.x = element_line(color = text_color),
        plot.title = element_text(color = text_color),
      )+
      scale_x_discrete(position='top')+
      scale_fill_manual(values=c('#c5c98d','#c6bfd7','#ffd7c6'))
    
    suppressWarnings(plots[var] <-ggplotly(p)) # Store each plot in the list
}
    #plotting the ggplotly categorical variables in a grid
    subplot(plots,
            nrows=4,
            titleX=T,
            margin=c(0.01,0.01,0.04,0.04),
            shareY = T)
}



# Function to plot numerical variables 

plot_numerical_variables <- function(df) {
  numeric_vars <- df[, sapply(df, is.numeric)]
  plots <- list()
  
  # Create a plot for each numeric variable
  for (var in names(numeric_vars)) {
    p <- ggplot(numeric_vars, aes_string(x = var)) +
      geom_histogram(binwidth = 1,fill = "#ffd7c6", color = "#c6bfd7") +
      xlab(var) + 
      ylab("") + 
      ggtitle('')+
      theme(
        plot.background = element_rect(fill = NA), # Make the plot background transparent
        panel.background = element_rect(fill = NA), 
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        # Change text color to #7aa6a1 for all relevant elements
        axis.title = element_text(color = "#7aa6a1"),
        axis.text = element_text(color = "#7aa6a1"),
        axis.ticks = element_line(color = "#7aa6a1"),
        plot.title = element_text(color = "#7aa6a1"),
      )
    suppressWarnings(plots[var] <- ggplotly(p))
  }
  
  # Use subplot to arrange in a 2x2 grid
  subplot(plots,
          nrows = 2,
          titleX = T,
          margin=c(0.05,0.05,0.05,0.15))
}


plot_country_count<-function(df){
  country_freq<-df%>% group_by(Country) %>% count()
  
  # Reformat country names that aren't in long format
  country_freq$Country[19:20]<-c('United Kingdom','United States of America') 
  
  # Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Merge data
  merged_data <- country_freq %>%
    left_join(world %>% select(name, geometry), by = c("Country" = "name"))
  
  # Extract coordinate data
  merged_data <- merged_data %>%
    mutate(lat=st_centroid(geometry)[[1]][2],
           lon=st_centroid(geometry)[[1]][1])
  
  # plot on an interactive world map through leaflet
  leaflet(data = merged_data) %>%
    addTiles() %>%
    addMarkers(
      lng = ~lon,
      lat = ~lat,
      popup = ~paste(Country, "<br>", "Amount of participants:", n))%>%
    setView(lng = 0, lat = 48.0, zoom = 1.2) 
}



create_roc_plots <- function(models, test_data) {
  
  # Force 'healthy' to be the reference level
  data<-data.frame(factor(test_data$Alzheimer.s.Diagnosis,levels=c('healthy','Alzheimer\'s')))
  
  names(data)<-'true_labels'
  
  for (i in 1:length(models)){
    p<-make_predictions(models[i],test_data)
    p<-data.frame(p$pred)
    names(p)<-names(models[i])
    data<-cbind(data,p)
    
  }
  roc_list<-list()
  for (model in names(models)){
    roc_list[[model]]<-
      roc(as.numeric(data$true_labels)-1,
          as.numeric(data[[model]])-1)
  }
  # Function to extract data from roc object
  extract_roc_data <- function(roc_obj, model_name) {
    coords <- coords(roc_obj)
    df <- as.data.frame(coords)
    df$model <- model_name
    return(df)
  }
  
  # Extract data from each roc object in the list
  roc_data_list <- mapply(extract_roc_data, roc_list, names(roc_list), SIMPLIFY = FALSE)
  
  # Calculate AUC for each ROC object and store in a list
  auc_values <- sapply(roc_list, auc)
  
  # Combine all data frames into one
  roc_combined_df <- do.call(rbind, roc_data_list)
  max_sensitivity <- max(roc_combined_df$sensitivity)
  
  
  # Plot using ggplot2
  p<-ggplot(roc_combined_df, aes(x = 1-specificity, y = sensitivity, color = model)) +
    geom_line() +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") + # Add the random guess line
    geom_text(data = data.frame(model = names(roc_list), auc_values),
              aes(x = 0.8,
                  y = c(0.35,0.45,0.55),
                  color=model), 
              label = paste("AUC =", round(auc_values, 2)),
              hjust = 0.5,
              vjust = 1) +
    labs(title = "ROC Curves", x = "1-Specificity", y = "Sensitivity") +
    theme_minimal()+ 
    ggtitle('')+
    theme(
      plot.background = element_rect(fill = NA), # Make the plot background transparent
      panel.background = element_rect(fill = NA), 
      panel.grid.minor = element_blank(),
      panel.grid.major =element_line(color = "#BEBEBE", linetype = 4),
      axis.title = element_text(color = "#7aa6a1"),
      axis.text = element_text(color = "#7aa6a1"),
      axis.ticks = element_line(color = "#7aa6a1"),
      plot.title = element_text(color = "#7aa6a1"),
      legend.text = element_text(color = "#7aa6a1")
    )+
    guides(color = guide_legend(title = NULL))
  
  ggplotly(p)
}

rpart_plot<-function(rpart_model){

  rpart.plot(rpart_model$finalModel,
             split.font = 2,        # Font style for splits
             branch.lty = 3,        # Line type for branches
             cex = 1.1,             # Text size
             box.palette = "BuRd",  # Color palette for boxes
             split.box.col = "transparent", # Background color for splits
             split.border.col = "transparent", # Border color for splits
             split.col='#7aa6a1',
             yspace=1
             )
}
  
