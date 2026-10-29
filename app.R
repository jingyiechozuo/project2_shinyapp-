
# author: Jingyi Zuo
# O*NET Occupational Skills Explorer
# Explore 2026 O*NET occupations by skill dimensions, occupation group type and preparation levels

library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(shinycssloaders)
source("helper.R")

ui <- page_sidebar(
  title = "O*NET Occupational Skills Explorer",
  
  # sidebar is designed: subset the data
  sidebar = sidebar(
    # 
    # categorical variables checkbox, and at first all levels checked by default
    checkboxGroupInput(inputId = "group_sub", label = "Occupation Group", choices = group_levels, selected = group_levels),
    checkboxGroupInput(inputId = "prep_sub", label = "Preparation level", choices = prep_vars, selected = prep_vars),
    checkboxGroupInput(inputId = "skill_sub", label = "Skill Type", choices = skill_levels, selected = skill_levels),
    # Because I added skill dimension as the categorical subset also the quant selection, I added a text to explain the role of the skill dimension here. 
    helpText("The skill dimension each occupation relies on most."),
    
    # numeric variables selection: let users pick a variable
    selectizeInput("num1", "numeric variable 1", choices = factor_vars, selected = factor_vars[1]),
    uiOutput("slider1"),
    selectizeInput("num2", "numeric variable 2", choices = factor_vars, selected = factor_vars[3]),
    uiOutput("slider2"),
    
    actionButton("subset", "Apply Filters")
  ),
  
  # main panel: three tabs
  navset_tab(
    
    
    # "About" tab: introduction of this app interaction
    nav_panel("About",
              h3("Purpose"),
              p("This explorer allows you to explore how skill profiles differ across occupation groups, the skill dimension they rely most on and levels of preparation."),
              h3("Data"),
              p("Data come from the",
                a("O*NET 31.0 Database (2026)", href = "https://www.onetcenter.org/database.html"),
                "by the U.S. Department of Labor. A factor analysis of 35 skills found four skill dimensions: Communication, Technical, Analytical, and Management.",
                "Factor scores are standardized: 0 is the average; Skill type is the dimension an occupation scores highest on, and preparation level is the O*NET Job Zone (the preparation needed to do this job, including education, experience, and training needed)."),
              h3("How to Use"),
              tags$ul(
                tags$li("Sidebar: choose subsets of the data, then click 'Apply Filters'."),
                tags$li("Data Download: view the data you chose, and save it as a csv. file."),
                tags$li("Data Exploration: one-way and two ways contigency tables, numerical summaries, and different types of plots, depending on which variables you chose. Attention: The heatmap shows the average of the selected numeric variable by occupation group and preparation level, which are fixed.")
              ),
              img(src = "onet.png")
    ),
    
    # Data download tab
    nav_panel("Data Download",
              downloadButton("download", "Download CSV"),
              DT::dataTableOutput("data_table")
    ),
    
    # Data exploration tab
    nav_panel("Data Exploration",
              radioButtons("summary_type", "Summary Type",
                           choices = c("Categorical", "Numeric"), inline = TRUE),
              
              
              # categorical summaries
              conditionalPanel("input.summary_type == 'Categorical'",
                               selectizeInput("cat1", "Variable", choices = cat_vars),
                               selectizeInput("cat2", "Second Variable (optional)",
                                              choices = c("None" = "none", cat_vars)),
                               tableOutput("cat_table"),
                               withSpinner(plotOutput("cat_plot"))
                               
              ),
              
              # numeric summaries
              conditionalPanel("input.summary_type == 'Numeric'",
                               selectizeInput("num_x", "Numeric Variable",
                                              choices = numeric_vars, selected = "programming"),
                               selectizeInput("group_var", "Summarize Across", choices = cat_vars),
                               radioButtons("plot_type", "Plot Type",
                                            choices = c("Histogram", "Box Plot", "Scatter Plot","Heatmap"),
                                            inline = TRUE),
                               conditionalPanel("input.plot_type == 'Scatter Plot'",
                                                selectizeInput("num_y", "Y Variable",
                                                               choices = numeric_vars, selected = "mathematics")
                               ),
                               selectizeInput("color_var", "Color By",
                                              choices = c("None" = "none", cat_vars)),
                               selectizeInput("facet_var", "Facet By",
                                              choices = c("None" = "none", cat_vars)),
                               tableOutput("num_table"),
                               withSpinner(plotOutput("num_plot"))
              )
    )
  )
)
server <- function(input, output, session) {
  
  # dynamic sliders: the range follows the selected variable, but round to integers and use 0.1 as interval. 
  output$slider1 <- renderUI({
    x <- data[[input$num1]]
    sliderInput("range1", paste("Range of", label(input$num1)),
                min = floor(min(x)), max = ceiling(max(x)),
                value = c(floor(min(x)), ceiling(max(x))), step = 0.1)
  })
  
  output$slider2 <- renderUI({
    x <- data[[input$num2]]
    sliderInput("range2", paste("Range of", label(input$num2)),
                min = floor(min(x)), max = ceiling(max(x)),
                value = c(floor(min(x)), ceiling(max(x))), step = 0.1)
  })
  
  ## subsetted data (which user selected): starts as the full data, updates only when the button is clicked
  subset_data <- reactiveValues(df = data)
  
  observeEvent(input$subset, {
    subset_data$df <- data |>
      filter(major_group %in% input$group_sub,
             preparation %in% input$prep_sub,
             skill_type %in% input$skill_sub,
             between(.data[[input$num1]], input$range1[1], input$range1[2]),
             between(.data[[input$num2]], input$range2[1], input$range2[2]))
  })
  # data table of the selected data
  output$data_table <- DT::renderDataTable({
    validate(need(nrow(subset_data$df) > 0,
                  "No occupations match your filters. Please widen them."))
    subset_data$df
  })
  
  # download the subsetted data as a CSV file
  output$download <- downloadHandler(
    filename = function() {
      "onet_subset.csv"
    },
    content = function(file) {
      write.csv(subset_data$df, file, row.names = FALSE)
    }
  )
  
  # Data exploration tab: categorical summaries
  # one-way or two-way contingency table
  output$cat_table <- renderTable({
    df <- subset_data$df
    # if no occupations satisfy the selection criteria
    validate(need(nrow(df) > 0, "No occupations match your filters. Please select again."))
    if (input$cat2 == "none") {
      count(df, .data[[input$cat1]])
    } else {
      # if the same variable is chosen twice, show a message
      validate(need(input$cat1 != input$cat2, "Please choose two different variables."))
      counts <- count(df, .data[[input$cat1]], .data[[input$cat2]])
      pivot_wider(counts, names_from = all_of(input$cat2), values_from = n, values_fill = 0)
    }
  })
  
  # bar chart
  output$cat_plot <- renderPlot({
    df <- subset_data$df
    validate(need(nrow(df) > 0, "No occupations match your filters."))
    
    if (input$cat2 == "none") {
      ggplot(df, aes(x = .data[[input$cat1]])) +
        geom_bar() +
        labs(title = paste("Number of Occupations by", label(input$cat1)),
             x = label(input$cat1), y = "Number of occupations")
    } else {
      validate(need(input$cat1 != input$cat2, "Please choose two different variables."))
      ggplot(df, aes(x = .data[[input$cat1]], fill = .data[[input$cat2]])) +
        geom_bar(position = "dodge") +
        labs(title = paste(label(input$cat1), "by", label(input$cat2)),
             x = label(input$cat1), y = "Number of occupations",
             fill = label(input$cat2))
    }
  })
  
  # Data exploration tab: numeric summaries
  
  # mean, median, and SD of the numeric variable at each level of the categorical variables
  output$num_table <- renderTable({
    df <- subset_data$df
    validate(need(nrow(df) > 0, "No occupations match your filters. Please select again."))
    summarise_by(df, input$group_var, input$num_x)
  })
  
  # histogram, scatter plot, box plot, or heatmap
  output$num_plot <- renderPlot({
    df <- subset_data$df
    validate(need(nrow(df) > 0, "No occupations match your filters. Please select again."))
    
    if (input$plot_type == "Histogram") {
      p <- ggplot(df,
                  aes(x = .data[[input$num_x]])) +
        geom_histogram() +
        labs(title = paste(label(input$num_x), "scores"),
             x = paste(label(input$num_x), "scores"),
             y = "Number of occupations")
      
    } else if (input$plot_type == "Scatter Plot") {
      p <- ggplot(df,
                  aes(x = .data[[input$group_var]], y = .data[[input$num_x]])) +
        geom_point() +
        labs(title = paste("Scatter of", label(input$num_x), "scores by", label(input$group_var)),
             y = paste(label(input$num_x), "scores"),
             x = label(input$group_var))
      
    } else if (input$plot_type == "Box Plot") {
      p <- ggplot(df,
                  aes(x = .data[[input$num_x]], y = .data[[input$group_var]])) +
        geom_boxplot() +
        labs(title = paste("Box Plot of", label(input$num_x), "scores by", label(input$group_var)),
             y = label(input$group_var),
             x = paste(label(input$num_x), "scores"))
      
    } else {
      # heatmap
      grouped <- group_by(df, major_group, preparation)
      means <- summarise(grouped, mean = mean(.data[[input$num_x]]))
      p <- ggplot(means, aes(x = preparation, y = major_group, fill = mean)) +
        geom_tile(color = "blue") +
        scale_fill_gradient() +
        labs(title = paste("Average score",label(input$num_x),"by occupation groups and preparation levels"),
             x = "preparation level", y = "occupation groups", fill = paste("Mean score of",label(input$num_x)))
      return(p)
    }
    
    # color
    if (input$color_var != "none") {
      if (input$plot_type == "Scatter Plot") {
        p <- p + aes(color = .data[[input$color_var]]) + labs(color = label(input$color_var))
      } else {
        p <- p + aes(fill = .data[[input$color_var]]) + labs(fill = label(input$color_var))
      }
    }
    
    # facet
    if (input$facet_var != "none") {
      p <- p + facet_wrap(vars(.data[[input$facet_var]]))
    }
    p
  })
}

shinyApp(ui = ui, server = server)

