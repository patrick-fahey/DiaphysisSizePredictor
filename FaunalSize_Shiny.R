library(shiny)
library(shinythemes)
library(tidyverse)

load("linearmodel.RData")  # Ensure this file exists in the app directory

# ------------------ Define UI for data upload app -----------------
ui <- fluidPage(theme = shinytheme("yeti"),
  titlePanel("Predicting Size"),
  
  navbarPage("", 
             tabPanel("Process Data", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          
                          tags$hr(),
                          
                          radioButtons("measuretype", "Thickness Measures",
                                       choices = c(`Min and Max` = "MinMax",
                                                   Average = "Avg")),
                          
                          conditionalPanel(
                            condition = "input.measuretype == 'MinMax'",
                            textInput("MaxId", "Max Column Name", value = ""),
                            textInput("MinId", "Min Column Name", value = "")
                          ),
                          
                          conditionalPanel(
                            condition = "input.measuretype == 'Avg'",
                            textInput("AvgId", "Avg Column Name", value = "")
                          ),
                          
                          actionButton("process_data", "Process Data"), # Button to modify the dataframe
                          tags$hr(),
                          
                          downloadButton("download_data", "Download Processed Data")
                          
                        ), #end sidebar
                        
                        mainPanel(
                          tableOutput("contents")
                        )
                      ) #end sidebar
             ), # end process data
             
             
             tabPanel("Visualization",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxInput("use_intervals", "Use Interval Classes", value = FALSE),
                          
                          tags$hr(),
                          
                          checkboxInput("use_strat_checkbox", "Use Strat Units", value = FALSE),
                          
                          
                          conditionalPanel(
                            condition = "input.use_strat_checkbox == true",
                            textInput("StratID", "Strat Unit Column Name", value = "", placeholder = "Enter column name"),
                            textInput("StratOrderID", "Choose Units and Order", value = "", placeholder = "List units in order, separated with comma")
                            
                          ), #end strat conditional
                          
                          tags$hr(),
                          
                          numericInput("plot_width", "Plot Width (mm)", value = 190, min = 4, max = 1000),
                          numericInput("plot_height", "Plot Height (mm)", value = 100, min = 4, max = 1000),
                          downloadButton("download_plot", "Download Plot"),
                        ), #end sidebar
                        
                        mainPanel(
                          plotOutput("size_distribution_plot")
                        ) # end main panel
                      ) # end sidebar
             ), # end Viz
             
             
             tabPanel("Instructions",  # New tab for instructions
                      fluidPage(
                        h2("How to Use This App"),
                        p("This app allows you to predict body mass and size class on diaphysis faunal specimens."),
                        tags$hr(),
                        h3("Process Data Page"),
                        p("Follow these steps:"),
                        tags$ol(
                          tags$li("Choose CSV File: Click", tags$b("Browse") ,"to upload specimen database as a CSV file, specifying the type of separator your file uses. Most CSVs are comma separated."),
                          tags$li("Thickness Measures: Select the method in which cortical thickness was measured.",
                                  tags$ul(  # This creates the sub-list
                                    tags$li("Min and Max: 2 measures were taken per specimen (approximate min and max)."),
                                    tags$li("Average: 1 thickness was taken per specimen (approximate average).")
                                  )
                          ),
                          tags$li("Enter Column Names: Enter the relevant column name exactly as it appears in the database."),
                          tags$li("Click", tags$b("Process Data")),
                          tags$li("View the first 10 columns and 30 rows of the processed database to ensure size class and body mass were calculated."),
                          tags$li("Click", tags$b("Download Processed Data"), " to save the new database to your computer.")
                        ),
                        
                        # Add horizontal rule and new section for Visualization Page
                        tags$hr(),
                        h3("Visualization Page"),
                        p("After data is processed on the", tags$b("Process Data"), " page, a relative frequency bar chart of prey sizes will be generated and available for download."),
                        p("Follow these steps:"),
                        tags$ol(
                        tags$li("Use Interval Classes: when selected, will use an interval class system in which specimens that have their assigned size class probability below 75% are assigned to an intermediate class between adjacent size classes."),
                        tags$li("Use Strat Units: When selected, the plot will be split by stratigraphic units or other grouping variables.",
                                tags$ul(
                                  tags$li("Strat Unit Column Name: Enter the relevant column name exactly as it appears in the database."),
                                  tags$li("Choose Units and Order: List the strat units you want included in the order you want them to appear,", tags$i("seperated by commas."), " Any unit not included will be omitted.")
                                ),
                                ),
                        tags$li("Export Plot:",
                          tags$ul(
                            tags$li("Plot Width (mm): enter the desired width in mm for exported plot."), 
                            tags$li("Plot Height (mm): enter the desired height in mm for exported plot."),
                            tags$li("Click", tags$b("Download Plot."))
                        ),
                        ),
                        ),
                        
                        tags$hr(),
                        p("For questions, contact: bpfahey@asu.com")
                      )
             )
             
             
  ) # end navbar
  
)






# ------------------- Function for classification probabilities -------------------
predict_class_probs <- function(AvgThickness) {
  # Hard-coded model parameters
  beta <- 12.98
  thresholds <- c(11.71, 21.28, 26.76, 32.91)  # 1|2, 2|3, 3|4, 4|5
  
  # Log-transform X as per model
  X <- log(AvgThickness)
  
  # Compute cumulative probabilities P(Y ≤ j)
  cum_probs <- 1 / (1 + exp(-(thresholds - beta * X)))
  
  # Compute individual class probabilities
  p1 <- cum_probs[1]
  p2 <- cum_probs[2] - cum_probs[1]
  p3 <- cum_probs[3] - cum_probs[2]
  p4 <- cum_probs[4] - cum_probs[3]
  p5 <- 1 - cum_probs[4]
  
  # Return named list
  return(c(p1, p2, p3, p4, p5))
}








# ------------------- Define server logic -------------------
server <- function(input, output, session) {
  
  # Reactive value to store the uploaded and modified dataframe
  processed_data <- reactiveVal(NULL)
  
  observeEvent(input$process_data, {
    req(input$file1)  # Ensure a file is uploaded
    
    
    
    df <- tryCatch({
      read.csv(input$file1$datapath,
               header = TRUE,
               fill = TRUE,
               sep = input$sep,
               quote = '"',
               fileEncoding = "UTF-8", 
               stringsAsFactors = FALSE)  # Avoid converting strings to factors
    }, error = function(e) {
      stop(safeError(e))
    })
    
    
    
    
    # Rename columns based on user input
    if (input$measuretype == "MinMax") {
      req(input$MaxId, input$MinId)  # Ensure inputs are provided
      
      df <- df %>%
        rename(MaxThickness = !!sym(input$MaxId), MinThickness = !!sym(input$MinId)) %>%
        mutate(AvgThickness = (MaxThickness + MinThickness) / 2) # Compute AvgThickness
    } else if (input$measuretype == "Avg") {
      req(input$AvgId)
      
      df <- df %>%
        rename(AvgThickness = !!sym(input$AvgId))
    }
    
    
    
    # Compute class probabilities and store as new columns
    probs_df <- as.data.frame(t(apply(df["AvgThickness"], 1, predict_class_probs)))
    colnames(probs_df) <- c("1_ClassProb", "2_ClassProb", "3_ClassProb", "4_ClassProb", "5_ClassProb")
    
    # Combine probability columns with original data (add to the end of df)
    df <- bind_cols(df, probs_df)
    
    
    
    #---------------- Assign size class based on POLR
    df <- df %>%
      mutate(
        #Get highest probability
        SizeClass = as.character(max.col(select(., ends_with("_ClassProb")), ties.method = "first")),
        SizeClass_prob = pmax(`1_ClassProb`, `2_ClassProb`, `3_ClassProb`, `4_ClassProb`, `5_ClassProb`),
        # Get second highest probability
        SizeClass_2nd = as.character(max.col(select(., ends_with("_ClassProb")) * 
                                               (select(., ends_with("_ClassProb")) != SizeClass_prob),
                                             ties.method = "first")),
        # If prob < 0.75, concatenate top two; otherwise, just keep the top one
        SizeClass_interval = ifelse(SizeClass_prob < 0.75, 
                                    paste0(pmin(SizeClass, SizeClass_2nd), "-", pmax(SizeClass, SizeClass_2nd)), 
                                    SizeClass)
      )
    
    
    #---------------- Predict body mass
    # Create a new dataframe with log(AvgThickness)
    new_data <- df %>%
      mutate(AvgAll_log = log(AvgThickness))
    
    # Get prediction intervals
    pred_intervals <- predict(linearmodel, newdata = new_data, interval = "prediction")
    
    # Convert predictions back to normal scale (exponentiate)
    df <- df %>%
      mutate(
        BodyMassln = pred_intervals[, "fit"],  # Predicted log(BodyMass)
        BodyMass = exp(BodyMassln),
        BodyMass_lwrPI = exp(pred_intervals[, "lwr"]),  # Lower bound
        BodyMass_uprPI = exp(pred_intervals[, "upr"]),  # Upper bound
        BodyMass_corrected = BodyMass * 1.2
      )
    
    # Remove unecessary columns
    df <- df %>% select(-BodyMassln, -SizeClass_2nd)
    
    # Rearange columns
    df <- df %>%
      select(
        1,  # Keep the first column in the original dataframe as the first
        
        SizeClass, SizeClass_prob, SizeClass_interval, BodyMass, BodyMass_lwrPI, BodyMass_uprPI, AvgThickness, everything()  # Reorder the rest
      )
    
    # Store modified dataframe in reactive value
    processed_data(df)
  })
  
  output$contents <- renderTable({
    req(processed_data()) # Ensure processed data exists
    df <- processed_data()
    
    # Select only the first 5 columns (modify as needed)
    df[, 1:min(10, ncol(df))]
  })
  
  
  
  # ========================== Download button ==========================
  
  # Function to handle file download
  output$download_data <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(processed_data())  # Ensure there is data to download
      df <- processed_data()
      
      # Format SizeClass_interval so Excel treats it as text
      df$SizeClass_interval <- paste0("=\"", df$SizeClass_interval, "\"")
      
      write.csv(df, file, row.names = FALSE, quote = FALSE)  # Disable auto-quoting
    }
  )
  
  
  # -------------------------- Strat unit code
  
  updated_data <- reactive({
    df <- processed_data()
    req(df)
    
    # Assign StratUnit based on checkbox
    if (input$use_strat_checkbox && input$StratID %in% colnames(df)) {
      
      df <- df %>%
        mutate(StratUnit = .data[[input$StratID]])
      
      # Process StratOrderID input
      if (input$StratOrderID != "") {
        strat_levels <- unlist(strsplit(input$StratOrderID, "\\s*,\\s*"))  # Split by commas and trim spaces
        
        # Temporarily treat unlisted values as NA
        df <- df %>%
          mutate(StratUnit = ifelse(StratUnit %in% strat_levels, StratUnit, NA_character_)) %>%
          mutate(StratUnit = factor(StratUnit, levels = strat_levels))  # Set factor levels
        
        # Explicitly drop NA levels
        df <- df %>% filter(!is.na(StratUnit))
      }
    } else {
      df <- df %>%
        mutate(StratUnit = "All")
    }
    
    df
  })
  
  
  
  # ========================== Render Plot ==========================
  output$size_distribution_plot <- renderPlot({
    df <- updated_data()
    req(df)
    
    # Exclude NA values from plotting
    df <- df 
    
    default_colors_5 <- c("#6b50b0", "#d1495b", "#DAA520", "#66a182", "#00798c")
    default_colors_5interval <- c("#6b50b0", "#9E4D86", "#d1495b", "#D6773E", "#DAA520","#A0A351", "#66a182", "#338D87", "#00798c")
    
    # Determine fill variable and color scale based on checkbox state
    fill_var <- if (input$use_intervals) "SizeClass_interval" else "SizeClass"
    fill_colors <- if (input$use_intervals) default_colors_5interval else default_colors_5
    
    # Calculate sample sizes
    sample_sizes <- df %>%
      count(StratUnit)
    
    df %>% filter(!is.na(StratUnit)) %>%
      ggplot(aes_string(fill = fill_var, y = 1, x = "StratUnit")) +  # Use aes_string() for dynamic variable
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = fill_colors) +
      coord_flip() +
      labs(y = "Frequency", x = "", fill = "Size Class") +
      scale_y_reverse(limits = c(1, -0.05)) +  
      geom_text(data = sample_sizes, 
                aes(x = StratUnit, y = -0.01, label = paste0("", n)), 
                inherit.aes = FALSE,
                hjust = 0,
                size = 5) +
      theme_minimal()+
      theme(
        axis.text.x = element_text(size = 10),  # X-axis text size
        axis.text.y = element_text(size = 10),  # Y-axis text size
        axis.title.x = element_text(size = 10),  # X-axis title
        axis.title.y = element_text(size = 10),  # Y-axis title
        legend.text = element_text(size = 11),  # Legend text size
        legend.title = element_text(size = 12, face = "bold"),
        panel.grid.major.y = element_blank(),  
        panel.grid.minor.y = element_blank()# Legend title size
      )
  })
  
  
  # ------------------ Export plot -------------------------#
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("size_distribution_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      df <- updated_data()
      req(df)
      
      # Determine fill variable and color scale based on checkbox state
      fill_var <- if (input$use_intervals) "SizeClass_interval" else "SizeClass"
      fill_colors <- if (input$use_intervals) default_colors_5interval else default_colors_5
      
      # Calculate sample sizes
      sample_sizes <- df %>%
        count(StratUnit)
      
      # Create the ggplot object
      p <- df %>% filter(!is.na(StratUnit)) %>%
        ggplot(aes_string(fill = fill_var, y = 1, x = "StratUnit")) +
        geom_bar(position = "fill", stat = "identity") +
        scale_fill_manual(values = fill_colors) +
        coord_flip() +
        labs(y = "Frequency", x = "", fill = "Size Class") +
        scale_y_reverse(limits = c(1, -0.05)) +
        geom_text(data = sample_sizes, 
                  aes(x = StratUnit, y = -0.01, label = paste0("", n)), 
                  inherit.aes = FALSE,
                  hjust = 0,
                  size = 6) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 10),  # X-axis text size
          axis.text.y = element_text(size = 10),  # Y-axis text size
          axis.title.x = element_text(size = 10),  # X-axis title
          axis.title.y = element_text(size = 10),  # Y-axis title
          legend.text = element_text(size = 12),  # Legend text size
          legend.title = element_text(size = 11, face = "bold"),  # Legend title size
          panel.grid.major.y = element_blank(),  
          panel.grid.minor.y = element_blank()
        )
      
      # Save plot as PNG with user-defined dimensions
      ggsave(file, plot = p, width = input$plot_width, height = input$plot_height, dpi = 300, units = "mm")
    }
  )
  
} # End Server


# ------------------- Create Shiny app -------------------
shinyApp(ui = ui, server = server)
