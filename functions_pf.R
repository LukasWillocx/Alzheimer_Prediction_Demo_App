# functions.R
# Dependent libraries in app.R file

library(luwitemplate)
colors<-get_theme_colors()

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
plot_confusion_matrix <- function(conf_matrix,theme) {
  # Create a data frame for plotting
  conf_data <- data.frame(
    Category = c("True Positives", "True Negatives", "False Positives", "False Negatives"),
    acc_id=c(1,1,0,0),
    Count = c(conf_matrix$tp, conf_matrix$tn, conf_matrix$fp, conf_matrix$fn)
  )
  # Define the fill colors for each category
  color_palette <- c("False Negatives" = "#D48050",  
                     "False Positives" = '#D05A4F',  
                     "True Negatives" = "#5580A3",    
                     "True Positives" = "#4A9466") 
  # Create the bar plot
  ggplot(conf_data, aes(x = Category, y = Count, fill = Category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=color_palette) +
    theme_luwi(theme)+
    labs(title = "", x = "", y = "")+
    guides(fill='none')
}

plot_model_accuracy<-function(model,test_data, theme){
  
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
      scale_fill_gradient(low = 'darkgrey', high = colors$success)  + 
      scale_y_continuous(limits = c(0, 1)) +
    theme_luwi(theme) +
    guides(fill='none')+
      labs(title = "",
           x = "",
           y = "") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))  # Rotate x-axis labels for readability
}

######################EDA- functions ###############################

plot_categorical_variables <- function(df, theme) {
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
    
    p <- ggplot(data, aes(x=var,y = n, fill = group)) +
      geom_col(position = 'stack', width = 1) +
      labs(x='',y='')+
      geom_text(aes(label = group),
                position = position_stack(vjust = 0.5),
                size=3)+
      theme_luwi(theme)+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank())+
      guides(fill='none')+
      scale_fill_manual(values=c(colors$secondary,colors$info,colors$warning))
    
    suppressWarnings(plots[var] <-ggplotly(p, theme = theme)) # Store each plot in the list
}
    #plotting the ggplotly categorical variables in a grid
    subplot(plots,
            nrows=4,
            titleX=T,
            margin=c(0.01,0.01,0.04,0.04),
            shareY = T)
}

# Function to plot numerical variables 

plot_numerical_variables <- function(df, theme) {
  numeric_vars <- df[, sapply(df, is.numeric)]
  plots <- list()
  
  # Create a plot for each numeric variable
  for (var in names(numeric_vars)) {
    p <- ggplot(numeric_vars, aes_string(x = var)) +
      geom_histogram(binwidth = 1,fill = colors$secondary, color = colors$primary) +
      xlab(var) + 
      ylab("") + 
      ggtitle('') +
      theme_luwi(theme) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank())
    suppressWarnings(plots[var] <- ggplotly(p, theme=theme))
  }
  
  # Use subplot to arrange in a 2x2 grid
  subplot(plots,
          nrows = 2,
          titleX = TRUE,
          heights = c(0.45, 0.45),  # Slightly reduce heights to create more space
          margin = c(0.02,0.02,0.17,0.17))
}

plot_country_count <- function(df) {
  country_freq <- df %>% group_by(Country) %>% count()
  
  # Reformat country names that aren't in long format
  country_freq$Country[19:20] <- c('United Kingdom', 'United States of America')
  
  # Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Merge data
  merged_data <- world %>%
    left_join(country_freq, by = c("name" = "Country"))
  
  # Color palette for choropleth
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = merged_data$n,
    na.color = "transparent"
  )
  
  # Build the map
  leaflet(data = merged_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addPolygons(
      fillColor   = ~pal(n),
      fillOpacity = 0.7,
      color       = "#444444",
      weight      = 0.8,
      opacity     = 1,
      highlight   = highlightOptions(
        weight      = 2.5,
        color       = "#333333",
        fillOpacity = 0.85,
        bringToFront = TRUE
      ),
      label = ~ifelse(
        is.na(n),
        name,
        paste0(name, ": ", n, " participant", ifelse(n > 1, "s", ""))
      ),
      labelOptions = labelOptions(
        style     = list("font-weight" = "bold", "padding" = "4px 8px"),
        textsize  = "13px",
        direction = "auto"
      ),
      popup = ~ifelse(
        is.na(n),
        paste0("<b>", name, "</b><br>No participants"),
        paste0(
          "<div style='font-family: sans-serif;'>",
          "<b style='font-size:14px;'>", name, "</b><br>",
          "<span style='font-size:22px; color:", pal(n), ";'>", n, "</span>",
          " participant", ifelse(n > 1, "s", ""),
          "</div>"
        )
      )
    ) %>%
    
    addLegend(
      position = "bottomright",
      pal      = pal,
      values   = ~n,
      title    = "Participants",
      opacity  = 0.8,
      na.label = "No data"
    ) %>%
    
    setView(lng = 0, lat = 30, zoom = 2)
}



create_roc_plots <- function(models, test_data, theme) {
  
  colors <- get_theme_colors(theme)
  
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
  p<-ggplot(roc_combined_df, aes(x = 1-specificity, y = sensitivity)) +
    geom_line(aes(color=model),size=0.5,alpha=0.6) +
    geom_point(aes(color=model),size=3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = colors$info) + # Add the random guess line
    geom_text(data = data.frame(model = names(roc_list), auc_values),
              aes(x = 0.35,
                  y = c(0.54,0.59,0.64),
                  color=model), 
              label = paste("AUC =", signif(auc_values, 3)),
              hjust = 0.5,
              vjust = 1) +
    labs(title = "ROC Curves", x = "1-Specificity", y = "Sensitivity") +
    theme_luwi(theme)+ scale_color_luwi_d(theme)+
    ggtitle('') + guides(color='none')
  
  luwi_ggplotly(p, theme=theme,tooltip = c('x','y','model'))
}

rpart_plot<-function(rpart_model, theme){
  
  colors <- get_theme_colors(theme) 
  fonts <- get_theme_fonts()
  
  rpart.plot(rpart_model$finalModel,
             split.font = 1,
             branch.lty = 4,
             branch.col = colors$secondary,
             cex = 1.4,
             box.palette = "BuRd",
             split.box.col = colors$light,
             split.border.col = colors$primary,
             split.col = colors$primary,
             split.prefix = "  ",  # Add padding
             split.suffix = "  ",
             yspace = 1.2,  # Increase vertical space
             gap = 0.2)  # Gap between boxes
}
  
