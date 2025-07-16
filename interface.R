#interface.R                                                  


#-------------------------------------------------------------------------------
#
#  Script for generating the Interface and overall appearance
#
# Copyright (C) 2025 Jean-Pierre Gnimatin, Marlon Grodd, Susanne Weber, Derek Hazard, Martin Wolkewitz
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#-----------------------------------------------------------------------------------------------------------




# Defining UI ----------------------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  tags$i("Copyright © 2025 Jean-Pierre Gnimatin, Marlon Grodd, Susanne Weber, Derek Hazard, Martin Wolkewitz"),
  
  # Adding logos at the top, centered
  tags$div(
    style = "text-align: center; margin-bottom: 10px;",
    tags$img(src = "fdm_logo.png", height = "120px", style = "margin-right: 200px;"),
    tags$img(src = "ukl_logo.png", height = "70px")
  ),
  
  # Title and text in the UI
  tags$h3("Stacked probAbility Visualization & Comparison (StaViC) Shiny App", 
          style = "text-align: center; font-weight: bold;"),
  tags$p("This Shiny app visualizes stacked probability plots, for investigating the effects of implemented startegies by comparing the initial condition (No Intervention) with various intervention strategies (incorporating α, β, and θ)", 
         style = "text-align: center; font-weight: bold;"),
  
  tags$p("States: 0- Admission into hospital, 1- Hospital-Acquired Infection, 2-Discharged alive, 3-Death, 4-Discharged Alive after HAI, 5-Death after HAI", 
         style = "text-align: center; font-weight: bold;"),
  
  
  # Instructions and image side-by-side
  fluidRow(
    column(6,
           
           # Text on the left
           tags$div(
             style = "padding: 10px;",
             h4("Instructions:",  style ="text-align: left; font-weight: bold;"),
             tags$ol(
               tags$li("This app is particularly useful for comparing interventions, showing how they influence the probabilities of state transitions."),
               tags$li("The sidebar of the app allows users to input hazard rates and specify values for α, β and θ.",
                       tags$ul(
                         tags$li("α, β and θ ∈ [0, 1], where 1 indicates Min. effect (no effect) and 0 is Max. effect."),
                         tags$li("α is the effect of ENHANCED TREATMENT interventions on DISCHARGE and β on DEATH."),
                         tags$li("θ is the IMPROVED INFECTION PREVENTION factor."),
                         tags$li("α and β are the treatment enhancement factors while θ is the infection prevention factor.")
                       )),
               tags$li("For ENHANCED TREATMENT ONLY, input values of only α and β (keeping θ at default 1)."),
               tags$li("For INFECTION PREVENTION ONLY, input values of only θ (keeping α and β at default 1)."),
               tags$li("For both, input values for all three parameters.")
             )
           )
    ),
    column(6,
           
           # Image on the right
           tags$div(
             style = "text-align: left;",
             tags$img(src = "Stavic_setting.jpg", height = "180px")
           )
    )
  ),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      width = 3,# Reduce the width of the sidebar
      
      # Initial Lambda Parameters
      numericInput("lambda1", "λ01 (Infection hazard rate):", value = 0.05, step = 0.01),
      numericInput("lambda2", "λ02 (Discharge hazard rate without infection):", value = 0.1, step = 0.01),
      numericInput("lambda3", "λ03 (Death hazard rate without infection):", value = 0.05, step = 0.01),
      numericInput("lambda4", "λ14 (Discharge hazard rate with infection):", value = 0.1, step = 0.01),
      numericInput("lambda5", "λ15 (Death hazard rate with infection):", value = 0.05, step = 0.01),
      
      # Enhanced Treatment Parameters
      numericInput("alpha", "Alpha:", value = 1, min = 0, max = 1, step = 0.01),
      numericInput("beta", "Beta:", value = 1, min = 0, max = 1, step = 0.01),
      numericInput("theta", "Theta:", value = 1, min = 0, max = 1, step = 0.01),
      
      # Time
      numericInput("t", "Time (in days):", value = 30, step = 1),
      
      #  Stacking order input (codes 0–5)
      textInput(
        "order",
        "State stacking order by code (0 = In-patient w/o infection, 1 = Infection, 2 = Discharge w/o infection, 3 = Death w/o infection, 4 = Discharge after infection, 5 = Death after infection):",
        value = "2,3,0,5,4,1"
      ),
      
      # Run button
      actionButton("run_btn", "Run Simulation")
    ),
    
    # Main panel for plots
    mainPanel(
      fluidRow(
        column(6, highchartOutput("initial_plot", height = "650px")),
        column(6, highchartOutput("intervention_plot", height = "650px"))
      )
    )
  )
)




# Defining the Server -----------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output) {
  
  compute_probabilities <- function(Q, t) {
    trans_prob <- data.frame(time_t = 0:t, P11 = 0, P12 = 0, P13 = 0, P14 = 0, P25 = 0, P26 = 0)
    
    for (i in seq_len(t + 1)) {
      t_ <- i - 1
      P  <- expm(Q * t_)
      trans_prob[i, ] <- c(t_, P[1, ])
    }
    
    p_time <- NULL
    for (name in names(trans_prob)[-1]) {
      acc <- data.frame(
        t    = trans_prob$time_t,
        p    = trans_prob[[name]],
        name = name,
        stringsAsFactors = FALSE
      )
      p_time <- rbind(p_time, acc)
    }
    
    p_time$name <- factor(
      p_time$name,
      levels = c("P13","P14","P11","P26","P25","P12"),
      labels = c(
        "Discharge without Infection",
        "Death without Infection", 
        "In-Patient Without infection",
        "Death after Infection", 
        "Discharge after Infection",
        "Infection"
      )
    )
    
    return(p_time)
  }
  
  observeEvent(input$run_btn, {
    # Read inputs
    lambda1 <- input$lambda1; lambda2 <- input$lambda2
    lambda3 <- input$lambda3; lambda4 <- input$lambda4; lambda5 <- input$lambda5
    alpha   <- input$alpha;   beta    <- input$beta;   theta   <- input$theta
    t       <- input$t
    
    # Build initial Q matrix
    Q_initial <- matrix(0, 6, 6)
    Q_initial[1,2] <- lambda1
    Q_initial[1,3] <- lambda2
    Q_initial[1,4] <- lambda3
    Q_initial[2,5] <- lambda4
    Q_initial[2,6] <- lambda5
    Q_initial[1,1] <- -sum(Q_initial[1,])
    Q_initial[2,2] <- -sum(Q_initial[2,])
    
    # Build enhanced Q matrix
    enh_lambda1 <- theta * lambda1
    enh_lambda2 <- lambda2
    enh_lambda3 <- lambda3
    enh_lambda4 <- alpha * (lambda4 - lambda2) + lambda2
    enh_lambda5 <- beta  * (lambda5 - lambda3) + lambda3
    
    Q_enhanced <- matrix(0, 6, 6)
    Q_enhanced[1,2] <- enh_lambda1
    Q_enhanced[1,3] <- enh_lambda2
    Q_enhanced[1,4] <- enh_lambda3
    Q_enhanced[2,5] <- enh_lambda4
    Q_enhanced[2,6] <- enh_lambda5
    Q_enhanced[1,1] <- -sum(Q_enhanced[1,])
    Q_enhanced[2,2] <- -sum(Q_enhanced[2,])
    
    # Compute probability data
    p_time_initial  <- compute_probabilities(Q_initial,  t)
    p_time_enhanced <- compute_probabilities(Q_enhanced, t)
    
    #CUSTOM CODE‐TO‐NAME MAPPING FOR 0–5
    code_to_name <- c(
      "0" = "In-Patient Without infection",
      "1" = "Infection",
      "2" = "Discharge without Infection",
      "3" = "Death without Infection",
      "4" = "Discharge after Infection",
      "5" = "Death after Infection"
    )
    
    # Parse and validate the zero-based codes
    input_codes <- as.numeric(unlist(strsplit(input$order, ",")))
    valid_codes <- input_codes[input_codes %in% 0:5]
    event_order <- code_to_name[as.character(valid_codes)]
    
    # # Function to create a highchart for a given dataset
    create_highchart <- function(data, title, event_order) {
      hc <- highchart() %>%
        hc_chart(type = "area") %>%
        hc_plotOptions(area = list(
          stacking    = "percent",
          lineWidth   = 1,
          marker      = list(enabled = FALSE, radius = 0),
          states      = list(hover = list(lineWidthPlus = 0, marker = list(enabled = FALSE)))
        )) %>%
        hc_xAxis(
          title        = list(text = "Time"),
          min          = 0,
          max          = max(data$t),
          allowDecimals = FALSE,
          labels       = list(formatter = JS("function() { return 'Day ' + this.value; }"))
        ) %>%
        hc_yAxis(
          title  = list(text = "Probability"),
          labels = list(format = "{value}%")
        ) %>%
        hc_tooltip(
          headerFormat = "<b>Day {point.x}</b><br/>",
          pointFormat  = "<span style=\"color:{series.color}\">{series.name}</span>: <b>{point.y:.2f}%</b><br/>",
          shared       = TRUE
        ) %>%
        hc_title(text = title) %>%
        hc_legend(
          enabled   = TRUE,
          itemStyle = list(fontWeight = "bold", color = "#333333")
        )
      
      # Add series in user‐defined order
      for (event_name in event_order) {
        event_data <- data[data$name == event_name, ]
        hc <- hc %>% hc_add_series(
          name = paste0("<b>", event_name, "</b>"),
          data = list_parse(data.frame(x = event_data$t, y = round(event_data$p * 100, 2))),
          color = switch(event_name,
                         "Discharge without Infection"    = "#1f77b4",
                         "Death without Infection"        = "#ff7f0e",
                         "In-Patient Without infection"   = "#2ca02c",
                         "Death after Infection"          = "#d62728",
                         "Discharge after Infection"      = "#9467bd",
                         "Infection"                      = "#8c564b")
        )
      }
      hc
    }
    
    # Render plots
    output$initial_plot <- renderHighchart({
      create_highchart(p_time_initial,  "<b>Initial Conditions</b>", event_order)
    })
    output$intervention_plot <- renderHighchart({
      create_highchart(p_time_enhanced, "<b>After Interventions</b>", event_order)
    })
  })
}





# Run the app ------------------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)


