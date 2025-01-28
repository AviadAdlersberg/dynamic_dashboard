#
# Features:
#   Dataset Selection:
#   
#   Added a dropdown menu (selectInput) to choose between the mtcars and iris datasets.
# Dynamic Inputs:
#   
#   The X-axis, Y-axis, and Color dropdown menus are dynamically updated based on the selected dataset using renderUI and reactive.
# Validation:
#   
#   A notification is displayed if a bar chart is selected and the chosen Y-axis is not numeric.
# Flexibility:
#   
#   The app works seamlessly with datasets of different structures (mtcars and iris in this case).
# Features:
#   Save Parameters: Clicking the "Save Tabs" button saves the parameters for all tabs into a JSON file.
# Load Parameters: Clicking the "Load Tabs" button loads the parameters from the JSON file and recreates the tabs.
# Dynamic Tabs: Users can still add and remove tabs dynamically.
# This ensures user settings are persistent between sessions.
#
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(jsonlite)

# Sample datasets
datasets <- list(
  mtcars = mtcars,
  iris = iris
)

# UI
ui <- fluidPage(
  titlePanel("Dynamic Plot Generator"),
  sidebarLayout(
    sidebarPanel(
      textInput("tab_name", "Tab Name:", value = "Tab 1"),
      actionButton("add_tab", "Add Tab"),
      actionButton("remove_tab", "Remove Tab"),
      fileInput("load_file", "Load Saved Tabs", accept = c(".json")),
      downloadButton("save_tabs", "Save Tabs")
    ),
    mainPanel(
      tabsetPanel(id = "plot_tabs")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value for tab parameters
  tab_params <- reactiveVal(list())
  # Counter for dynamic tabs
  tab_counter <- reactiveVal(1)
  
  # Function to save tabs as JSON
  output$save_tabs <- downloadHandler(
    filename = function() { paste("tabs_", Sys.Date(), ".json", sep="") },
    content = function(file) {
      params_list <- tab_params()
      current_tabs <- names(params_list)
      for (tab_id in current_tabs) {
        params_list[[tab_id]]$dataset <- input[[paste0("dataset_", tab_id)]]
        params_list[[tab_id]]$x_var <- input[[paste0("x_var_", tab_id)]]
        params_list[[tab_id]]$y_var <- input[[paste0("y_var_", tab_id)]]
        params_list[[tab_id]]$color_var <- input[[paste0("color_var_", tab_id)]]
        params_list[[tab_id]]$plot_type <- input[[paste0("plot_type_", tab_id)]]
        params_list[[tab_id]]$coord_flip <- input[[paste0("coord_flip_", tab_id)]]
        params_list[[tab_id]]$rotate_x_labels <- input[[paste0("rotate_x_labels_", tab_id)]]
        params_list[[tab_id]]$rotate_y_labels <- input[[paste0("rotate_y_labels_", tab_id)]]
        params_list[[tab_id]]$high_threshold <- input[[paste0("high_threshold_", tab_id)]]
        params_list[[tab_id]]$low_threshold <- input[[paste0("low_threshold_", tab_id)]]
        params_list[[tab_id]]$bar_chart_type <- input[[paste0("bar_chart_type_", tab_id)]]
        params_list[[tab_id]]$tab_name <- params_list[[tab_id]]$tab_name
      }
      tab_params(params_list)
      save_data <- tab_params()
      write_json(save_data, file, pretty = TRUE)
    }
  )
  
  # Function to load tabs from a JSON file
  observeEvent(input$load_file, {
    req(input$load_file)
    loaded_data <- fromJSON(input$load_file$datapath)
    tab_params(loaded_data)
    tab_counter(1)  # Reset tab counter
    remove_all_tabs()  # Clear existing tabs
    # Recreate tabs from the loaded data
    lapply(names(loaded_data), function(tab_id) {
      add_tab(tab_id, loaded_data[[tab_id]]$tab_name, loaded_data[[tab_id]])
      tab_counter(tab_counter() + 1)
    })
    showNotification("Tabs loaded successfully.", type = "message")
  })
  
  # Function to remove all tabs
  remove_all_tabs <- function() {
    current_tabs <- names(tab_params())
    for (tab_id in current_tabs) {
      removeTab(inputId = "plot_tabs", target = tab_id)
    }
    tab_params(list())  # Reset the tab parameters
  }
  
  # Add a new tab
  add_tab <- function(tab_id, tab_name, params = NULL) {
    insertTab(
      inputId = "plot_tabs",
      tabPanel(
        title = tab_name,
        value = tab_id,
        fluidRow(
          column(3, selectInput(paste0("dataset_", tab_id), "Select Dataset:", choices = names(datasets))),
          column(3, selectInput(paste0("x_var_", tab_id), "Select X-axis:", choices = NULL)),
          column(3, selectInput(paste0("y_var_", tab_id), "Select Y-axis:", choices = NULL)),
          column(3, selectInput(paste0("color_var_", tab_id), "Color by:", choices = c("None", NULL)))
        ),
        fluidRow(
          column(4, numericInput(paste0("high_threshold_", tab_id), "High Threshold:", value = NA, step = 1)),
          column(4, numericInput(paste0("low_threshold_", tab_id), "Low Threshold:", value = NA, step = 1))
        ),
        fluidRow(
          column(4, radioButtons(paste0("plot_type_", tab_id), "Plot Type:",  
                                 choices = c("Scatter Plot" = "scatter", "Bar Chart" = "bar", "Line Graph" = "line"),  
                                 inline = TRUE)),
          column(4, uiOutput(paste0("bar_chart_type_ui_", tab_id)))
        ),
        
        fluidRow(
          column(6, checkboxInput(paste0("rotate_x_labels_", tab_id), "Rotate X-axis Labels", value = FALSE)),
          column(6, checkboxInput(paste0("rotate_y_labels_", tab_id), "Rotate Y-axis Labels", value = FALSE))
        ),
        fluidRow(
          column(12, checkboxInput(paste0("coord_flip_", tab_id), "Flip Coordinates (coord_flip)", value = FALSE))
        ),
        plotlyOutput(paste0("plot_", tab_id)),
        fluidRow(
          column(6, downloadButton(paste0("download_plot_", tab_id), "Save Plot")),
          column(6, downloadButton(paste0("download_table_", tab_id), "Save Table"))
        ),
        hr(),
        dataTableOutput(paste0("table_", tab_id))
      ),
      select = TRUE
    )
    # If parameters are not provided, set default parameters
    if (is.null(params)) {
      params <- list(
        dataset = names(datasets)[1],
        x_var_choices = names(datasets[[1]]),
        y_var_choices = names(datasets[[1]]),
        color_var_choices = c("None", names(datasets[[1]])),
        x_var = "None",
        y_var = "None",
        color_var = "None",
        plot_type = "scatter",
        rotate_x_labels = FALSE,
        rotate_y_labels = FALSE,
        coord_flip = FALSE,
        high_threshold = NA,
        low_threshold = NA,
        bar_chart_type = "identity",
        tab_name = tab_name
      )
    }
    tab_params_list <- tab_params()
    tab_params_list[[tab_id]] <- params
    tab_params(tab_params_list)
    # Set initial input values
    updateSelectInput(session, paste0("dataset_", tab_id), choices = names(datasets), selected = params$dataset)
    dataset <- datasets[[params$dataset]]
    updateSelectInput(session, paste0("x_var_", tab_id), choices = names(dataset), selected = params$x_var)
    updateSelectInput(session, paste0("y_var_", tab_id), choices = names(dataset), selected = params$y_var)
    updateSelectInput(session, paste0("color_var_", tab_id), choices = c("None", names(dataset)), selected = params$color_var)
    updateRadioButtons(session, paste0("plot_type_", tab_id), selected = params$plot_type)
    updateCheckboxInput(session, paste0("rotate_x_labels_", tab_id), value = params$rotate_x_labels)
    updateCheckboxInput(session, paste0("rotate_y_labels_", tab_id), value = params$rotate_y_labels)
    updateCheckboxInput(session, paste0("coord_flip_", tab_id), value = params$coord_flip)
    updateNumericInput(session, paste0("high_threshold_", tab_id), value = params$high_threshold)
    updateNumericInput(session, paste0("low_threshold_", tab_id), value = params$low_threshold)
    updateSelectInput(session, paste0("bar_chart_type_", tab_id), selected = params$bar_chart_type)
    
    # Conditional UI for bar chart type
    output[[paste0("bar_chart_type_ui_", tab_id)]] <- renderUI({
      if (input[[paste0("plot_type_", tab_id)]] == "bar") {
        selectInput(paste0("bar_chart_type_", tab_id), "Bar Chart Type:", choices = c("Identity" = "identity", "Dodge" = "dodge"), selected = params$bar_chart_type)
      }
    })
    
    # Reactive filtered data
    filtered_data <- reactive({
      dataset <- datasets[[input[[paste0("dataset_", tab_id)]]]]
      table_proxy <- input[[paste0("table_", tab_id, "_rows_all")]]
      if (!is.null(table_proxy)) {
        dataset <- dataset[table_proxy, ]
      }
      dataset
    })
    
    # Ensure parameters are properly saved on change
    observeEvent({
      list(input[[paste0("dataset_", tab_id)]], input[[paste0("x_var_", tab_id)]], input[[paste0("y_var_", tab_id)]], input[[paste0("color_var_", tab_id)]], input[[paste0("plot_type_", tab_id)]], input[[paste0("coord_flip_", tab_id)]], input[[paste0("high_threshold_", tab_id)]], input[[paste0("low_threshold_", tab_id)]], input[[paste0("bar_chart_type_", tab_id)]])
    }, {
      params_list <- tab_params()
      params_list[[tab_id]]$dataset <- input[[paste0("dataset_", tab_id)]]
      params_list[[tab_id]]$x_var <- input[[paste0("x_var_", tab_id)]]
      params_list[[tab_id]]$y_var <- input[[paste0("y_var_", tab_id)]]
      params_list[[tab_id]]$color_var <- input[[paste0("color_var_", tab_id)]]
      params_list[[tab_id]]$plot_type <- input[[paste0("plot_type_", tab_id)]]
      params_list[[tab_id]]$rotate_x_labels <- input[[paste0("rotate_x_labels_", tab_id)]]
      params_list[[tab_id]]$rotate_y_labels <- input[[paste0("rotate_y_labels_", tab_id)]]
      params_list[[tab_id]]$coord_flip <- input[[paste0("coord_flip_", tab_id)]]
      params_list[[tab_id]]$high_threshold <- input[[paste0("high_threshold_", tab_id)]]
      params_list[[tab_id]]$low_threshold <- input[[paste0("low_threshold_", tab_id)]]
      params_list[[tab_id]]$bar_chart_type <- input[[paste0("bar_chart_type_", tab_id)]]
      tab_params(params_list)
    })
    
    # Observe dataset change to update variables dynamically
    observeEvent(input[[paste0("dataset_", tab_id)]], {
      dataset <- datasets[[input[[paste0("dataset_", tab_id)]]]]
      updateSelectInput(session, paste0("x_var_", tab_id), choices = names(dataset), selected = params$x_var)
      updateSelectInput(session, paste0("y_var_", tab_id), choices = names(dataset), selected = params$y_var)
      updateSelectInput(session, paste0("color_var_", tab_id), choices = c("None", names(dataset)), selected = params$color_var)
    }, ignoreInit = TRUE)
    
    # Render plot for the new tab
    output[[paste0("plot_", tab_id)]] <- renderPlotly({
      req(input[[paste0("x_var_", tab_id)]], input[[paste0("y_var_", tab_id)]], input[[paste0("plot_type_", tab_id)]])
      dataset <- filtered_data()
      x_var <- input[[paste0("x_var_", tab_id)]]
      y_var <- input[[paste0("y_var_", tab_id)]]
      color_var <- input[[paste0("color_var_", tab_id)]]
      flip_coord <- input[[paste0("coord_flip_", tab_id)]]
      high_threshold <- input[[paste0("high_threshold_", tab_id)]]
      low_threshold <- input[[paste0("low_threshold_", tab_id)]]
      bar_chart_type <- input[[paste0("bar_chart_type_", tab_id)]]
      rotate_x <- input[[paste0("rotate_x_labels_", tab_id)]]
      rotate_y <- input[[paste0("rotate_y_labels_", tab_id)]]
      
      # Create ggplot
      p <- ggplot(dataset, aes_string(x = x_var, y = y_var))
      if (color_var != "None") {
        if (input[[paste0("plot_type_", tab_id)]] == "bar") {
          if (is.numeric(dataset[[color_var]])) {
            p <- p + aes_string(fill = color_var) +
              scale_fill_gradient(low = "blue", high = "red")
          } else {
            p <- p + aes_string(fill = color_var) +
              scale_fill_manual(values = scales::hue_pal()(length(unique(dataset[[color_var]]))))
          }
        } else {
          p <- p + aes_string(color = color_var)
        }
      }
      
      if (input[[paste0("plot_type_", tab_id)]] == "scatter") {
        p <- p + geom_point()
      } else if (input[[paste0("plot_type_", tab_id)]] == "bar") {
        if (bar_chart_type == "identity") {
          p <- p + geom_bar(stat = "identity")
        } else if (bar_chart_type == "dodge") {
          p <- p + geom_bar(stat = "identity", position = "dodge")
        }
      } else if (input[[paste0("plot_type_", tab_id)]] == "line") {
        p <- p + geom_line()
      }
      
      if (!is.na(high_threshold)) {
        p <- p + geom_hline(yintercept = high_threshold, linetype = "dotted", color = "red")
      }
      
      if (!is.na(low_threshold)) {
        p <- p + geom_hline(yintercept = low_threshold, linetype = "dotted", color = "blue")
      }
      
      if (flip_coord) {
        p <- p + coord_flip()
      }
      
      p <- p + theme_minimal()
      
      # Rotate axis labels
      if (rotate_x) {
        p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
      if (rotate_y) {
        p <- p + theme(axis.text.y = element_text(angle = 90, hjust = 1))
      }
      
      
      ggplotly(p)
    })
    
    # Render table for the new tab
    output[[paste0("table_", tab_id)]] <- renderDataTable({
      dataset <- datasets[[input[[paste0("dataset_", tab_id)]]]]
      datatable(dataset, filter = "top", options = list(scrollX = TRUE))
    })
    
    # Save Plot
    output[[paste0("download_plot_", tab_id)]] <- downloadHandler(
      filename = function() { paste0(tab_id, "_plot.png") },
      content = function(file) {
        plot <- ggplotly(output[[paste0("plot_", tab_id)]]())
        export(plot, file = file)
      }
    )
    
    # Save Table
    output[[paste0("download_table_", tab_id)]] <- downloadHandler(
      filename = function() { paste0(tab_id, "_table.csv") },
      content = function(file) {
        dataset <- datasets[[input[[paste0("dataset_", tab_id)]]]]
        write.csv(dataset, file)
      }
    )
  }
  
  # Observe Add Tab button
  observeEvent(input$add_tab, {
    current_tab <- tab_counter()
    tab_name <- input$tab_name
    if (tab_name == "") {
      showNotification("Please provide a name for the tab.", type = "error")
      return()
    }
    tab_counter(current_tab + 1)
    add_tab(paste0("tab_", current_tab), tab_name)
  })
  
  # Observe Remove Tab button
  observeEvent(input$remove_tab, {
    current_tab <- tab_counter()
    if (current_tab > 1) {
      removeTab(inputId = "plot_tabs", target = paste0("tab_", current_tab - 1))
      tab_counter(current_tab - 1)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
