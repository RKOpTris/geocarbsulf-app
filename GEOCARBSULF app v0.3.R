### potential additions:
# add geologic time scale to plots
# change to hover interface for drawing alterations to time arrays (with hover greyed predicted line, and solid if clicked)
# change model output to plotly to make it interactive
# add facility to store a plot (or 5?) in memory and allow viewing of them as overlays

setwd("~/Documents/Coding/GEOCARB")

library(ggplot2)
library(shiny)
library(shinyjs)
library(dplyr)
library(stringr)

pack_list <- function(objects){
  new_list <- objects
  names(new_list) <- as.character(substitute(objects))[-1]
  new_list
}

time_array_points <- 8 #not editable

ui <- fluidPage(
  useShinyjs(),
  titlePanel("==== GEOCARBSULF Interactive v0.3.0 ===="),
  fluidRow(
    column(2,
           wellPanel(
             fluidRow(
               column(6,
                      numericInput("seed", "Reproducible seed", value = 1)
               ),
               column(6,
                      numericInput("no_resamples", "# of resamples", value = 200)
               )
             ),
             fluidRow(
               column(9,
                      h4("==== Time arrays ====")
               ),
               column(3,
                      actionButton("help", "Help")
               )
             ),
             selectInput("select_parameter", "Select time array", choices = "", selected = ""),
             fluidRow(
               # column(2, 
               #        checkboxInput("use01", "", value = T)
               # ),
               column(4,
                      h4("Age")
               ),
               column(4,
                      h4("Ratio")
               ),
               column(4,
                      actionButton("save_settings", "Save", class = "btn btn-success action-button")
               )
             ),
             fluidRow(
               # column(2, 
               #        checkboxInput("use01", "", value = T)
               # ),
               column(4,
                      numericInput("age01", "", value = "")
               ),
               column(8,
                      sliderInput("slider01", "", min = 0.5, max = 1.5, value = 1, step = 0.05)
               )
             ),
             fluidRow(
               # column(2, 
               #        checkboxInput("use02", "", value = T)
               # ),
               column(4, 
                      numericInput("age02", "", value = "")
               ),
               column(8, 
                      sliderInput("slider02", "", min = 0.5, max = 1.5, value = 1, step = 0.05)
               )
             ),
             fluidRow(
               # column(2, 
               #        checkboxInput("use03", "", value = T)
               # ),
               column(4, 
                      numericInput("age03", "", value = "")
               ),
               column(8, 
                      sliderInput("slider03", "", min = 0.5, max = 1.5, value = 1, step = 0.05)
               )
             ),
             fluidRow(
               # column(2, 
               #        checkboxInput("use04", "", value = T)
               # ),
               column(4, 
                      numericInput("age04", "", value = "")
               ),
               column(8, 
                      sliderInput("slider04", "", min = 0.5, max = 1.5, value = 1, step = 0.05)
               )
             ),
             fluidRow(
               # column(2, 
               #        checkboxInput("use05", "", value = T)
               # ),
               column(4, 
                      numericInput("age05", "", value = "")
               ),
               column(8, 
                      sliderInput("slider05", "", min = 0.5, max = 1.5, value = 1, step = 0.05)
               )
             ),
             fluidRow(
               # column(2, 
               #        checkboxInput("use06", "", value = T)
               # ),
               column(4, 
                      numericInput("age06", "", value = "")
               ),
               column(8, 
                      sliderInput("slider06", "", min = 0.5, max = 1.5, value = 1, step = 0.05)
               )
             ),
             fluidRow(
               # column(2, 
               #        checkboxInput("use07", "", value = T)
               # ),
               column(4, 
                      numericInput("age07", "", value = "")
               ),
               column(8, 
                      sliderInput("slider07", "", min = 0.5, max = 1.5, value = 1, step = 0.05)
               )
             ),
             fluidRow(
               # column(2, 
               #        checkboxInput("use08", "", value = T)
               # ),
               column(4, 
                      numericInput("age08", "", value = "")
               ),
               column(8, 
                      sliderInput("slider08", "", min = 0.5, max = 1.5, value = 1, step = 0.05)
               )
             ),
             h4("==== Constants ===="),
             fluidRow(
               column(6,
                      numericInput("deltaT2X", "deltaT2X", value = ""),
                      numericInput("GLAC", "GLAC", value = "")
               ),
               column(6,
                      numericInput("LIFE", "LIFE", value = ""),
                      numericInput("GYM", "GYM", value = "")
               )
             ),
             radioButtons("old_new", "Use GEOCARBSULF input arrays from:", choices = c("Royer et al., 2014", "Marcilly et al., 2021"), selected = "Marcilly et al., 2021", inline = T),
             actionButton("reset_settings", "Reset all"),
             actionButton("run_model", "Run GEOCARBSULF", class = "btn btn-success action-button")
           )
           
    ),
    column(5,
           plotOutput("model_settings", height = 600),
           plotOutput("view_settings", height = 600)
    ),
    column(5,
           radioButtons("model_data", "Compare against published data from: (shown in red)", choices = c("Royer et al., 2014", "Marcilly et al., 2021", "None"), selected = "Marcilly et al., 2021", inline = T),
           fluidRow(
             column(6,
                    checkboxInput("plot_confidence", "Plot confidence intervals (0.025-0.975)", value = T)
             ),
             column(6,
                    checkboxInput("plot_log", "Plot log scale", value = T)
             )
           ),
           plotOutput("model_run", height = 1100),
           downloadButton("model_output", "Download model ouput")#, class = "btn btn-success action-button")
           # verbatimTextOutput("psets")
    )
  )
)

server <- function(input, output, session){
  
  original_data <- reactive({
    if(input$model_data == "Royer et al., 2014"){
      read.csv("GEOCARB_Royer_output.csv")
    } else {
      read.csv("GEOCARB_Marcilly_output.csv")
    }
  })
  
  parameter_settings <- reactiveValues(fSR = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                        ratio = rep(1, time_array_points)),
                                       d13C = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                         ratio = rep(1, time_array_points)),
                                       d34S = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                         ratio = rep(1, time_array_points)),
                                       Sr = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                       ratio = rep(1, time_array_points)),
                                       fR = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                       ratio = rep(1, time_array_points)),
                                       fL = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                       ratio = rep(1, time_array_points)),
                                       fA = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                       ratio = rep(1, time_array_points)),
                                       fD = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                       ratio = rep(1, time_array_points)),
                                       RT = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                       ratio = rep(1, time_array_points)),
                                       GEOG = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                         ratio = rep(1, time_array_points)),
                                       fC = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                       ratio = rep(1, time_array_points)),
                                       fAw_fA = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                           ratio = rep(1, time_array_points))
  )
  
  # output$psets <- renderText({
  #   str(reactiveValuesToList(parameter_settings))
  # })
  
  original_parameter_settings <- list(fSR = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                       ratio = rep(1, time_array_points)),
                                      d13C = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                        ratio = rep(1, time_array_points)),
                                      d34S = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                        ratio = rep(1, time_array_points)),
                                      Sr = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                      ratio = rep(1, time_array_points)),
                                      fR = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                      ratio = rep(1, time_array_points)),
                                      fL = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                      ratio = rep(1, time_array_points)),
                                      fA = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                      ratio = rep(1, time_array_points)),
                                      fD = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                      ratio = rep(1, time_array_points)),
                                      RT = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                      ratio = rep(1, time_array_points)),
                                      GEOG = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                        ratio = rep(1, time_array_points)),
                                      fC = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                      ratio = rep(1, time_array_points)),
                                      fAw_fA = data.frame(age = round(seq(570, 0, length.out = time_array_points)),
                                                          ratio = rep(1, time_array_points))
  )
  
  constant_values <- reactiveValues(deltaT2X = 3,
                                    GLAC = 2,
                                    LIFE = 0.25,
                                    GYM = 0.875)
  
  original_constant_values <- list(deltaT2X = 3,
                                   GLAC = 2,
                                   LIFE = 0.25,
                                   GYM = 0.875)
  
  observeEvent(input$reset_settings, {
    constant_values$deltaT2X <- original_constant_values$deltaT2X
    constant_values$GLAC <- original_constant_values$GLAC
    constant_values$LIFE <- original_constant_values$LIFE
    constant_values$GYM <- original_constant_values$GYM
    updateNumericInput(session, "deltaT2X", value = constant_values$deltaT2X)
    updateNumericInput(session, "GLAC", value = constant_values$GLAC)
    updateNumericInput(session, "LIFE", value = constant_values$LIFE)
    updateNumericInput(session, "GYM", value = constant_values$GYM)
    parameter_settings$fSR <- original_parameter_settings$fSR
    parameter_settings$d13C <- original_parameter_settings$d13C
    parameter_settings$d34S <- original_parameter_settings$d34S
    parameter_settings$Sr <- original_parameter_settings$Sr
    parameter_settings$fR <- original_parameter_settings$fR
    parameter_settings$fL <- original_parameter_settings$fL
    parameter_settings$fA <- original_parameter_settings$fA
    parameter_settings$fD <- original_parameter_settings$fD
    parameter_settings$RT <- original_parameter_settings$RT
    parameter_settings$GEOG <- original_parameter_settings$GEOG
    parameter_settings$fC <- original_parameter_settings$fC
    parameter_settings$fAw_fA <- original_parameter_settings$fAw_fA
  })
  
  observe({
    updateSelectInput(session, "select_parameter", choices = sort(names(parameter_settings)), selected = sort(names(parameter_settings))[1])
    updateNumericInput(session, "deltaT2X", value = constant_values$deltaT2X)
    updateNumericInput(session, "GLAC", value = constant_values$GLAC)
    updateNumericInput(session, "LIFE", value = constant_values$LIFE)
    updateNumericInput(session, "GYM", value = constant_values$GYM)
  })
  
  adjust_settings <- reactive({
    reactiveValuesToList(parameter_settings)[[input$select_parameter]]
  })
  
  current_settings <- reactive({
    data.frame(age = c(input$age01, input$age02, input$age03, input$age04, input$age05, input$age06, input$age07, input$age08),
               ratio = c(input$slider01, input$slider02, input$slider03, input$slider04, input$slider05, input$slider06, input$slider07, input$slider08)
    )
  })
  
  write_settings <- observeEvent(input$save_settings, {
    parameter_settings[[input$select_parameter]] <- current_settings()
  })
  
  output$model_settings <- renderPlot({
    model_settings <- current_settings()
    ggplot(model_settings, aes(age, ratio)) + 
      geom_line(size = 2) + 
      geom_point(size = 10) + 
      labs(x = "Age (Ma)", y = "Ratio", title = paste0("Edit time array (", input$select_parameter, ")")) +
      scale_y_continuous(limits = c(0.5, 1.5)) +
      scale_x_reverse(breaks = seq(500, 0, by = -100)) +
      theme_classic() +
      theme(axis.title = element_text(size = 20),
            plot.title = element_text(size = 24),
            axis.text = element_text(size = 16),
            axis.ticks.length = unit(-0.25, "cm"))
  })
  
  prep_settings <- function(){
    parameter_settings <- reactiveValuesToList(parameter_settings)
    for(i in seq_along(parameter_settings)){
      x <- names(parameter_settings)[i]
      parameter_settings[[x]]$parameter <- x
    }  
    do.call(rbind, parameter_settings)
  }
  
  
  
  output$view_settings <- renderPlot({
    ggplot(prep_settings(), aes(age, ratio, color = parameter)) + 
      geom_line(size = 2, aes(linetype = parameter)) +
      scale_linetype_manual(values = rep(c(1, 2), length.out = length(unique(prep_settings()$parameter)))) +
      labs(x = "Age (Ma)", y = "Ratio", title = "Current time array settings") +
      scale_y_continuous(limits = c(0.5, 1.5)) +
      scale_x_reverse(breaks = seq(500, 0, by = -100)) +
      theme_classic() +
      theme(axis.title = element_text(size = 20),
            plot.title = element_text(size = 24),
            axis.text = element_text(size = 16),
            axis.ticks.length = unit(-0.25, "cm")) +
      scale_color_manual(values = scico::scico(length(unique(prep_settings()$parameter)), palette = "batlow"))
  })
  
  observe({
    updateNumericInput(session, "age01", value = adjust_settings()$age[1])
    updateNumericInput(session, "age02", value = adjust_settings()$age[2])
    updateNumericInput(session, "age03", value = adjust_settings()$age[3])
    updateNumericInput(session, "age04", value = adjust_settings()$age[4])
    updateNumericInput(session, "age05", value = adjust_settings()$age[5])
    updateNumericInput(session, "age06", value = adjust_settings()$age[6])
    updateNumericInput(session, "age07", value = adjust_settings()$age[7])
    updateNumericInput(session, "age08", value = adjust_settings()$age[8])
    updateSliderInput(session, "slider01", value = adjust_settings()$ratio[1], min = slider_min(), max = slider_max(), step = slider_step())
    updateSliderInput(session, "slider02", value = adjust_settings()$ratio[2], min = slider_min(), max = slider_max(), step = slider_step())
    updateSliderInput(session, "slider03", value = adjust_settings()$ratio[3], min = slider_min(), max = slider_max(), step = slider_step())
    updateSliderInput(session, "slider04", value = adjust_settings()$ratio[4], min = slider_min(), max = slider_max(), step = slider_step())
    updateSliderInput(session, "slider05", value = adjust_settings()$ratio[5], min = slider_min(), max = slider_max(), step = slider_step())
    updateSliderInput(session, "slider06", value = adjust_settings()$ratio[6], min = slider_min(), max = slider_max(), step = slider_step())
    updateSliderInput(session, "slider07", value = adjust_settings()$ratio[7], min = slider_min(), max = slider_max(), step = slider_step())
    updateSliderInput(session, "slider08", value = adjust_settings()$ratio[8], min = slider_min(), max = slider_max(), step = slider_step())
  })
  
  slider_min <- reactive({
    ifelse(input$select_parameter %in% c("d13C", "d34S", "Sr"), 0.8, 0.5)
  })
  
  slider_max <- reactive({
    ifelse(input$select_parameter %in% c("d13C", "d34S", "Sr"), 1.2, 1.5)
  })
  
  slider_step <- reactive({
    ifelse(input$select_parameter %in% c("d13C", "d34S", "Sr"), 0.025, 0.05)
  })
  
  model_prep <- function(df){
    ratio_ramp <- sapply(1:(nrow(df) - 1), function(x){
      seq(df$ratio[x], df$ratio[x + 1], length.out = df$age[x] - df$age[x + 1])
    })
    ratio_sequence <- do.call(c, ratio_ramp)
    ratio_sequence[571] <- ratio_sequence[500]  
    ratio_df <- data.frame(age = 570:0, ratio = ratio_sequence)
    ratio_df[ratio_df$age %% 10 == 0, -1]
  }
  
  output$model_run <- renderPlot({
    GEOCARB_output <- geocarbsulf()$GEOCARB_output
    CO2 <- c(GEOCARB_output$CO2_0.025, rev(GEOCARB_output$CO2_0.975))
    ages <- c(GEOCARB_output$age, rev(GEOCARB_output$age))
    CO2_poly <- bind_cols(age = ages, CO2_unc = CO2)
    
    original_data <- original_data()
    oCO2 <- c(original_data$CO2_0.025, rev(original_data$CO2_0.975))
    oages <- c(original_data$age, rev(original_data$age))
    oCO2_poly <- bind_cols(age = oages, CO2_unc = oCO2)
    
    p <- ggplot(GEOCARB_output, aes(age, CO2_ppm)) + 
      scale_x_reverse(breaks = seq(500, 0 , by = -100))
    
    if(input$plot_log){
      p <- p + scale_y_continuous(trans = "log10", 
                                  breaks = c(seq(100, 900, by = 100), 
                                             seq(1000, 9000, by = 1000), 
                                             seq(10000, 90000, by = 10000)), 
                                  limits = c(100, 50000))
    }
    
    if(input$model_data != "None"){
      if(input$plot_confidence){
        p <- p + 
          geom_polygon(data = oCO2_poly, aes(age, CO2_unc), fill = "#FF000022") +
          geom_line(data = oCO2_poly[1:58, ], aes(age, CO2_unc), col = "#FF0000AA") +
          geom_line(data = oCO2_poly[59:116, ], aes(age, CO2_unc), col = "#FF0000AA")
      }
      p <- p +
        geom_line(data = original_data, aes(age, CO2_ppm), col = "#FF0000FF", size = 2)
    }
    
    if(input$plot_confidence){
      p <- p + 
        geom_polygon(data = CO2_poly, aes(age, CO2_unc), fill = "#0000FF22") +
        geom_line(data = CO2_poly[1:58, ], aes(age, CO2_unc), col = "#0000FFAA") +
        geom_line(data = CO2_poly[59:116, ], aes(age, CO2_unc), col = "#0000FFAA")
    }
    
    p <- p +
      geom_line(size = 2, col = "#0000FFFF") +
      theme_classic() +
      #coord_cartesian(xlim = c(500, 300)) +
      labs(x = "Age (Ma)", y = "CO2 (ppm)", title = "Model output") +
      theme(axis.title = element_text(size = 20),
            plot.title = element_text(size = 24),
            axis.text = element_text(size = 16),
            legend.position = "bottom",
            axis.ticks.length = unit(-0.25, "cm"))
    
    p
  })
  
  output$model_output <- downloadHandler(
    filename = "GEOCARBSULF_App_output.RDS",
    content = function(con) {
      saveRDS(list(time = Sys.time(),
                   reproducible_seed = input$seed,
                   # time_arrays = reactiveValuesToList(parameter_settings),
                   time_arrays_ratios = data.frame(reactiveValuesToList(parameter_settings)[[1]][1], 
                                                   sapply(reactiveValuesToList(parameter_settings), "[[", 2)),
                   constants = data.frame(constant = c("deltaT2X", "GLAC", "LIFE", "GYM"),
                                          value = c(input$deltaT2X, input$GLAC, input$LIFE, input$GYM)),
                   original_input_arrays = geocarbsulf()$original_arrays,
                   tinkered_input_arrays = geocarbsulf()$time_arrays,
                   model = input$old_new,
                   model_results = geocarbsulf()$GEOCARB_output), con)
    }
  )
  
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Input explanations",
      HTML(
        '
  <h4>Time arrays (isotopes)</h4>
  <b>Sr</b> - 87Sr/86Sr of shallow-marine carbonate ([87Sr/86Sr - 0.7] x 10^4)<br>
  <b>d13C</b> - d13C of shallow-marine carbonate (per mil); called "DLCOC" in BASIC code<br>
  <b>d34S</b> - d34S of marine sulfate sulfur (per mil) (from Wu et al, 2010); called "DLSOC" in BASIC code<br>
  <br>
  <h4>Time arrays (geobiological processes)</h4>
  <b>fR</b> - effect of relief on chemical weathering at time (t) relative to the present-day; calculated from equation (5) in Berner (2006b)<br>
  <b>fL</b> - land area covered by carbonates at time (t) relative to the present-day<br>
  <b>fA</b> - land area at time (t) relative to the present-day<br>
  <b>fAw_fA</b> - fraction of land area experiencing chemical weathering (runoff > 0)<br>
  <b>fD</b> - change in global river runoff at time (t) relative to the present-day in the absence of changes in solar luminosity and CO2 (i.e., mainly due to changes in paleogeography)<br>
  <b>GEOG</b> - change in land mean surface temperature for areas experiencing chemical weathering (runoff > 0) at time (t) relative to the present-day in the absence of changes in solar luminosity and CO2 (i.e., mainly due to changes in paleogeography) (K)<br>
  <b>RT</b> - coefficient relating continental runoff to temperature change (runoff/runoff(0)=1+RT*(T-T0)), as determined from the GCM simulations of Godderis et al 2012 (1/K); RT is called "Y" in Berner 2004 and "RUN" in GEOCARB III; in the BASIC scripts, RT is assigned a value of 0.045 during times with large ice sheets and a value of 0.025 for all other times; there is little difference in estimated CO2 between these two approaches for RT<br>
  <b>fSR</b> - seafloor creation rate at time (t) relative to the present-day<br>
  <b>fC</b> - effect of carbonate content of subducting oceanic crust on CO2 degassing rate at time (t) relative to the present-day<br>
  <br>
  <h4>Constants</h4>
  <b>LIFE</b> - rate ratio of chemical weathering in a minimally-vegetated to present-day (angiosperm dominated) world<br>
  <b>GYM</b> - rate ratio of chemical weathering by gymnosperms to angiosperms<br>
  <b>deltaT2X</b> - climate sensitivity (K per CO2 doubling)<br>
  <b>GLAC</b> - factor by which deltaT2X changes during times with large continental ice sheets
  '      
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  geocarbsulf <- eventReactive(input$run_model, {
    set.seed(input$seed)
    resampleN <- input$no_resamples  #number of resamples for the Monte Carlo error analysis; if you wish to bypass resampling, set to 1
    percentile_values <- c(0.025,0.975) #list of percentiles (of any length) used to evaluate a resampled data set; the median (0.5) is outputted by default and doesn't need to be included here
    Godderis <- TRUE  #set to "TRUE" to run time arrays of fA, fAw/fA, fD, and GEOG from Godd?ris et al, 2012; set to "FALSE" to run standard time arrays from GEOCARBSULF.
    input_distribution <- FALSE #set to "TRUE" to return the means and standard deviations of the input parameter choices that are associated with successful (non-failed) runs; only works when loop_parameters is set to "FALSE"; this section of code was not used in Royer et al (2014) (i.e., parameter set to "FALSE")
    loop_parameters <- FALSE #set to "TRUE" to test the effect on calculated CO2 and O2 by sequentially varying one input parameter at a time (figures 3-4 in Royer et al 2014); BEWARE: this will take a long time to run (68X longer than a single resampled run)
    iteration_threshold <- 10 #maximum number of times the convergence equation for CO2 will iterate before signaling a failed run; in a test with reasonably-well-constrained input parameters (similar to simulations presented in Royer et al, 2014), the number of iterations never exceeded 7; this variable is here mostly as a failsafe stop-gap.
    
    #reading in the two input files
    userInput <- read.csv("GEOCARB_input_summaries.csv")
    userInput[userInput$parameter == "deltaT2X", ]$mean <- input$deltaT2X
    userInput[userInput$parameter == "GLAC", ]$mean <- input$GLAC
    userInput[userInput$parameter == "LIFE", ]$mean <- input$LIFE
    userInput[userInput$parameter == "GYM", ]$mean <- input$GYM
    if(input$old_new == "Royer et al., 2014"){
      original_arrays <- time_arrays <- read.csv("GEOCARB_input_arrays_Royer.csv")
    } else {
      
      original_arrays <- time_arrays <- read.csv("GEOCARB_input_arrays_Marcilly.csv")
    }
    desired_settings <- reactiveValuesToList(parameter_settings)
    parameter_names <- names(desired_settings)
    for(i in 1:length(parameter_names)){
      pname <- parameter_names[i]
      data <- desired_settings[[pname]]
      time_arrays[pname] <- time_arrays[pname] * model_prep(data)
    }
    
    ageN <- length(time_arrays$age) #number of time-steps
    #preparing the output files
    age <- matrix (time_arrays$age, nrow=ageN, ncol=1); colnames(age) <- "age (Myrs ago)"
    failed_runs <- matrix (nrow=ageN, ncol=1); colnames(failed_runs) <- "failed runs (%)" #percent of resampled runs for a given time-step that failed because at least one of the input fluxes was <0, estimated oxygen was <5% or >50% (or <19% or >23% at time t=0), or estimated CO2 was <150 ppm >50000 ppm (or <200 ppm or >300 ppm at time=0)
    O2 <- matrix(nrow=ageN, ncol=1); colnames(O2) <- "O2 (%)" #O2 output
    CO2 <- matrix(nrow=ageN, ncol=1); colnames(CO2) <- "CO2 (ppm)"  #CO2 output
    percentiles_O2 <- matrix(nrow=ageN, ncol=length(percentile_values)); colnames(percentiles_O2) <- percentile_values
    percentiles_CO2 <- matrix(nrow=ageN, ncol=length(percentile_values)); colnames(percentiles_CO2) <- percentile_values
    if (loop_parameters==TRUE)  {
      y <- (4+2*length(percentile_values)) #number of columns needed in summary output file for each parameter loop
      z <- length(userInput[,"parameter"])  #number of input parameters
      column_header <- matrix(nrow=1,ncol=y*z)  #setting up the column headers for the GEOCARB_output file
      GEOCARB_output <- matrix(nrow=ageN, ncol=y*z)
    } else {
      z <- 1}
    
    
    ######################################################
    ### code for generating and filling input matrices ###
    ######################################################
    
    #########
    #function for generating resamples for time-dependent arrays, including the potential clipping of distributions
    resamples_array <- function(x, name, row_position) {
      x <- matrix(nrow=ageN, ncol=resampleN)  #create empty matrix
      col_num <- which(colnames(time_arrays)==name) #matches a column number in the input arrays file to the array in question
      
      #if resampling=1 or if resampling is turned off for the time-dependent array in question (a full matrix is generated but with resampleN mean values at each time-step)
      if (resampleN==1 || userInput[row_position, "resample"]==FALSE) {
        x <- matrix(time_arrays[, col_num], nrow=ageN, ncol=resampleN)  }
      
      #resampling following a normal distribution
      if (userInput[row_position, "resample"]==TRUE & userInput[row_position, "distribution_type"]=="gaussian" & resampleN>1) {
        for (i in 1:ageN) {
          temp_resample <- NULL
          temp_resample <- rnorm(n=resampleN, mean=time_arrays[i,col_num], sd=time_arrays[i,col_num+1]/2)
          temp_resample <- replace(temp_resample,temp_resample<=userInput[row_position,"lower_limit"],userInput[row_position,"lower_limit"]+0.0001)
          temp_resample <- replace(temp_resample,temp_resample>=userInput[row_position,"upper_limit"],userInput[row_position,"upper_limit"]-0.0001)   
          x[i,] <- matrix(temp_resample, nrow=1, ncol=resampleN)
        } #end of time-sequence loop (i)      
      }    
      
      #resampling following a lognormal distribution
      if (userInput[row_position, "resample"]==TRUE & userInput[row_position, "distribution_type"]=="lognormal" & resampleN>1) {
        for (i in 1:ageN) {
          temp_resample <- NULL
          temp_resample <- rlnorm(n=resampleN, meanlog=log(time_arrays[i,col_num]), sdlog=log(time_arrays[i,col_num+1])/2)
          temp_resample <- replace(temp_resample,temp_resample<=userInput[row_position,"lower_limit"],userInput[row_position,"lower_limit"]+0.0001)
          temp_resample <- replace(temp_resample,temp_resample>=userInput[row_position,"upper_limit"],userInput[row_position,"upper_limit"]-0.0001)   
          x[i,] <- matrix(temp_resample, nrow=1, ncol=resampleN)
        } #end of time-sequence loop (i) 
      }
      x <- x    
    } #end of function "resamples_array"
    
    #########
    #function for generating resamples for constant parameters, including the potential clipping of distributions
    resamples_constant <- function(x, row_position) {
      
      #if resampling=1 or if resampling is turned off for the constant parameter in question (a full matrix is filled with the mean value)
      if (resampleN==1 || userInput[row_position, "resample"]==FALSE)  {
        x <- matrix(userInput[row_position, "mean"], nrow=ageN, ncol=resampleN)  }
      
      #resampling following a normal distribution
      if (userInput[row_position, "resample"]==TRUE & userInput[row_position, "distribution_type"]=="gaussian" & resampleN>1) {
        temp_resample <- NULL
        temp_resample <- rnorm(n=resampleN, mean=userInput[row_position,"mean"], sd=userInput[row_position,"two_sigma"]/2)
        temp_resample <- replace(temp_resample,temp_resample<=userInput[row_position,"lower_limit"],userInput[row_position,"lower_limit"]+0.0001)
        temp_resample <- replace(temp_resample,temp_resample>=userInput[row_position,"upper_limit"],userInput[row_position,"upper_limit"]-0.0001)
        x <- matrix(temp_resample, nrow=ageN, ncol=resampleN, byrow=TRUE)
      }
      
      #resampling following a lognormal distribution
      if (userInput[row_position, "resample"]==TRUE & userInput[row_position, "distribution_type"]=="lognormal" & resampleN>1) {
        temp_resample <- NULL
        temp_resample <- rlnorm(n=resampleN, meanlog=log(userInput[row_position,"mean"]), sdlog=log(userInput[row_position,"two_sigma"])/2)
        temp_resample <- replace(temp_resample,temp_resample<=userInput[row_position,"lower_limit"],userInput[row_position,"lower_limit"]+0.0001)
        temp_resample <- replace(temp_resample,temp_resample>=userInput[row_position,"upper_limit"],userInput[row_position,"upper_limit"]-0.0001)
        x <- matrix(temp_resample, nrow=ageN, ncol=resampleN, byrow=TRUE)
      }    
      x <- x
    } #end of function "resamples_constant"
    
    for (h in 1:z)  { #start the input parameter loop
      if (loop_parameters==TRUE)  { #identify the single input parameter that will be resampled
        userInput[,"resample"] <- FALSE
        userInput[h,"resample"] <- TRUE
      }
      
      #y=young; a=old; p=pyrite; s=sulfate; c=carbonate; si=silicates; g=organic matter; b=burial; m=degassing; w=weathering
      #masses are in units of 10^18 mol
      #fluxes ("F" prefix) are in units of 10^18 mol Myrs-1
      #rates ("k" prefix) are in units of Myrs-1
      #stable isotopic compositions ("d" prefix) are in per mil units
      
      #generate random distributions for the time-dependent parameters using the function "resamples_array"
      Sr <- resamples_array(Sr, "Sr", row_position=which(userInput[,"parameter"]=="Sr"))  #87Sr/86Sr of shallow-marine carbonate ([87Sr/86Sr - 0.7] x 10^4)
      d13C <- resamples_array(d13C, "d13C", row_position=which(userInput[,"parameter"]=="d13C"))  #d13C of shallow-marine carbonate (per mil); called "DLCOC" in BASIC code
      d34S <- resamples_array(d34S, "d34S", row_position=which(userInput[,"parameter"]=="d34S"))  #d34S of marine sulfate sulfur (per mil) (from Wu et al, 2010); called "DLSOC" in BASIC code
      fR <- resamples_array(fR, "fR", row_position=which(userInput[,"parameter"]=="fR"))  #effect of relief on chemical weathering at time (t) relative to the present-day; calculated from equation (5) in Berner (2006b)
      fL <- resamples_array(fL, "fL", row_position=which(userInput[,"parameter"]=="fL"))  #land area covered by carbonates at time (t) relative to the present-day
      if (Godderis==TRUE) {
        fA <- resamples_array(fA, "fA_Godderis", row_position=which(userInput[,"parameter"]=="fA")) #land area at time (t) relative to the present-day
        fAw_fA <- resamples_array(fAw_fA, "fAw_fA_Godderis", row_position=which(userInput[,"parameter"]=="fAw_fA"))  #fraction of land area experiencing chemical weathering (runoff > 0)
        fD <- resamples_array(fD, "fD_Godderis", row_position=which(userInput[,"parameter"]=="fD")) #change in global river runoff at time (t) relative to the present-day in the absence of changes in solar luminosity and CO2 (i.e., mainly due to changes in paleogeography)
        GEOG <- resamples_array(GEOG, "GEOG_Godderis", row_position=which(userInput[,"parameter"]=="GEOG")) #change in land mean surface temperature for areas experiencing chemical weathering (runoff > 0) at time (t) relative to the present-day in the absence of changes in solar luminosity and CO2 (i.e., mainly due to changes in paleogeography) (K)
      } else {
        fA <- resamples_array(fA, "fA", row_position=which(userInput[,"parameter"]=="fA"))
        fAw_fA <- resamples_array(fAw_fA, "fAw_fA", row_position=which(userInput[,"parameter"]=="fAw_fA"))
        fD <- resamples_array(fD, "fD", row_position=which(userInput[,"parameter"]=="fD"))
        GEOG <- resamples_array(GEOG, "GEOG", row_position=which(userInput[,"parameter"]=="GEOG"))
      } #end of if...else loop for Godd?ris parameters
      RT <- resamples_array(RT, "RT", row_position=which(userInput[,"parameter"]=="RT"))  #coefficient relating continental runoff to temperature change (runoff/runoff(0)=1+RT*(T-T0)), as determined from the GCM simulations of Godd?ris et al 2012 (1/K); RT is called "Y" in Berner 2004 and "RUN" in GEOCARB III; in the BASIC scripts, RT is assigned a value of 0.045 during times with large ice sheets and a value of 0.025 for all other times; there is little difference in estimated CO2 between these two approaches for RT
      fSR <- resamples_array(fSR, "fSR", row_position=which(userInput[,"parameter"]=="fSR"))  #seafloor creation rate at time (t) relative to the present-day
      fC <- resamples_array(fC, "fC", row_position=which(userInput[,"parameter"]=="fC"))  #effect of carbonate content of subducting oceanic crust on CO2 degassing rate at time (t) relative to the present-day
      
      #generate random distributions for the constant parameters using the function "resamples_constant"
      ACT <- resamples_constant(ACT, row_position=which(userInput[,"parameter"]=="ACT"))  #activation energy (E) for dissolution of Ca- and Mg-silicates on land, where ACT = E/RTT0 (1/K); called "Z" in Berner (2004)
      ACTcarb <- resamples_constant(ACTcarb, row_position=which(userInput[,"parameter"]=="ACTcarb"))  #activation energy (E) for dissolution of carbonates on land (see p. 53 in Berner 2004)
      VNV <- resamples_constant(VNV, row_position=which(userInput[,"parameter"]=="VNV"))  #rate ratio of chemical weathering in volcanic to non-volcanic silicate rocks (called "Wv/Wnv in Berner 2006b, and "basalt/granite" in Berner 2008)
      NV <- resamples_constant(NV, row_position=which(userInput[,"parameter"]=="NV")) #coefficient relating physical erosion to the mean 87Sr/86Sr of non-volcanic silicate rocks
      exp_NV <- resamples_constant(exp_NV, row_position=which(userInput[,"parameter"]=="exp_NV")) #exponent relating physical erosion to the mean 87Sr/86Sr of non-volcanic silicate rocks (see Berner 2008)
      LIFE <- resamples_constant(LIFE, row_position=which(userInput[,"parameter"]=="LIFE")) #rate ratio of chemical weathering in a minimally-vegetated to present-day (angiosperm dominated) world
      GYM <- resamples_constant(GYM, row_position=which(userInput[,"parameter"]=="GYM"))  #rate ratio of chemical weathering by gymnosperms to angiosperms
      FERT <- resamples_constant(FERT, row_position=which(userInput[,"parameter"]=="FERT")) #exponent reflecting the fraction of vegetation whose growth is stimulated by elevated CO2; FERT is related to enhanced chemical weathering by the Michaelis-Menton expression [2RCO2/(1+RCO2)]^FERT, which is called fBb(CO2) in Berner 2004 (p. 24)
      exp_fnBb <- resamples_constant(exp_fnBb, row_position=which(userInput[,"parameter"]=="exp_fnBb")) #exponent used to describe the effect of climate on silicate or carbonate weathering in the absence of vascular plants at time (t) relative to the present-day (see pp. 25 & 53-54 in Berner 2004 and pp. 67-68 in Berner 1994)
      deltaT2X <- resamples_constant(deltaT2X, row_position=which(userInput[,"parameter"]=="deltaT2X")) #climate sensitivity (K per CO2 doubling)
      GLAC <- resamples_constant(GLAC, row_position=which(userInput[,"parameter"]=="GLAC")) #factor by which deltaT2X changes during times with large continental ice sheets
      J <- resamples_constant(J, row_position=which(userInput[,"parameter"]=="J"))  #coefficient used to calculate CAPd13C (called "alphac" in BASIC code), the stable carbon isotopic fractionation between shallow-marine carbonate and shallow-marine organic matter
      n <- resamples_constant(n, row_position=which(userInput[,"parameter"]=="n"))  #coefficient used to calculate CAPd34S (called "alphas" in BASIC code), the stable sulfur isotopic fractionation between marine sulfate sulfur and marine pyrite sulfur
      Ws <- resamples_constant(Ws, row_position=which(userInput[,"parameter"]=="Ws")) #effect on temperature from the linear increase in solar luminosity over time (K per 570 Myrs)
      exp_fD <- resamples_constant(exp_fD, row_position=which(userInput[,"parameter"]=="exp_fD")) #exponent that scales the dilution of dissolved HCO3- with runoff (fD) (see pp. 29-31 & 34-36 in Berner 2004)
      #GEOCARBSULF is run forward in time; the following are present-day (t = 0 Myrs ago) or initial values (t = 570 Myrs ago) of parameters that are subsequently recalculated at each time-step (see Berner 2004, 2006a for details)
      Fwpa_0 <- resamples_constant(Fwpa_0, row_position=which(userInput[,"parameter"]=="Fwpa_0")) #sulfate flux from oxidative weathering of old pyrite at present-day
      Fwsa_0 <- resamples_constant(Fwsa_0, row_position=which(userInput[,"parameter"]=="Fwsa_0")) #sulfate flux from weathering of CaSO4 sulfur at present-day
      Fwga_0 <- resamples_constant(Fwga_0, row_position=which(userInput[,"parameter"]=="Fwga_0")) #carbon flux from weathering of old sedimentary organic matter at present-day
      Fwca_0 <- resamples_constant(Fwca_0, row_position=which(userInput[,"parameter"]=="Fwca_0")) #carbon flux from weathering of old Ca and Mg carbonates at present-day
      Fmg_0 <- resamples_constant(Fmg_0, row_position=which(userInput[,"parameter"]=="Fmg_0"))  #carbon degassing flux from volcanism, metamorphism, and diagenesis of organic matter at present-day
      Fmc_0 <- resamples_constant(Fmc_0, row_position=which(userInput[,"parameter"]=="Fmc_0"))  #carbon degassing flux from volcanism, metamorphism, and diagenesis of carbonates at present-day
      Fmp_0 <- resamples_constant(Fmp_0, row_position=which(userInput[,"parameter"]=="Fmp_0"))  #sulfur degassing flux from volcanism, metamorphism, and diagenesis of pyrite at present-day
      Fms_0 <- resamples_constant(Fms_0, row_position=which(userInput[,"parameter"]=="Fms_0"))  #sulfur degassing flux from volcanism, metamorphism, and diagenesis of CaSO4 sulfur at present-day
      Fwsi_0 <- resamples_constant(Fwsi_0, row_position=which(userInput[,"parameter"]=="Fwsi_0")) #weathering flux for all Ca and Mg silicates at present-day
      Xvolc_0 <- resamples_constant(Xvolc_0, row_position=which(userInput[,"parameter"]=="Xvolc_0"))  #fraction of total Ca and Mg silicate weathering drived from volcanic rocks at present-day
      CAPd13C_0 <- resamples_constant(CAPd13C_0, row_position=which(userInput[,"parameter"]=="CAPd13C_0"))  #stable carbon isotopic fractionation between shallow-marine carbonate and shallow-marine organic matter at present-day
      CAPd34S_0 <- resamples_constant(CAPd34S_0, row_position=which(userInput[,"parameter"]=="CAPd34S_0"))  #stable sulfur isotopic fractionation between shallow-marine CaSO4 sulfur and pyrite sulfur at present-day
      oxygen_570 <- resamples_constant(oxygen_570, row_position=which(userInput[,"parameter"]=="oxygen_570")) #mass of atmospheric O2 at 570 Myrs ago
      Gy_570 <- resamples_constant(Gy_570, row_position=which(userInput[,"parameter"]=="Gy_570")) #mass of young crustal organic carbon at 570 Myrs ago
      Cy_570 <- resamples_constant(Cy_570, row_position=which(userInput[,"parameter"]=="Cy_570")) #mass of young crustal carbonate carbon at 570 Myrs ago
      Ca_570 <- resamples_constant(Ca_570, row_position=which(userInput[,"parameter"]=="Ca_570")) #mass of old crustal carbonate carbon at 570 Myrs ago
      Ssy_570 <- resamples_constant(Ssy_570, row_position=which(userInput[,"parameter"]=="Ssy_570"))  #mass of young CaSO4 sulfur at 570 Myrs ago
      Spy_570 <- resamples_constant(Spy_570, row_position=which(userInput[,"parameter"]=="Spy_570"))  #mass of young pyrite sulfur at 570 Myrs ago
      dlsy_570 <- resamples_constant(dlsy_570, row_position=which(userInput[,"parameter"]=="dlsy_570")) #d34S of young CaSO4 sulfur at 570 Myrs ago
      dlcy_570 <- resamples_constant(dlcy_570, row_position=which(userInput[,"parameter"]=="dlcy_570")) #d13C of young carbonate carbon at 570 Myrs ago
      dlpy_570 <- resamples_constant(dlpy_570, row_position=which(userInput[,"parameter"]=="dlpy_570")) #d34S of young pyrite sulfur at 570 Myrs ago
      dlpa_570 <- resamples_constant(dlpa_570, row_position=which(userInput[,"parameter"]=="dlpa_570")) #d34S of old pyrite sulfur at 570 Myrs ago
      dlgy_570 <- resamples_constant(dlgy_570, row_position=which(userInput[,"parameter"]=="dlgy_570")) #d13C of young organic matter at 570 Myrs ago
      dlga_570 <- resamples_constant(dlga_570, row_position=which(userInput[,"parameter"]=="dlga_570")) #d13C of old organic matter at 570 Myrs ago
      Rcy_570 <- resamples_constant(Rcy_570, row_position=which(userInput[,"parameter"]=="Rcy_570"))  #87Sr/86Sr of young carbonates undergoing weathering at 570 Myrs ago
      Rca_570 <- resamples_constant(Rca_570, row_position=which(userInput[,"parameter"]=="Rca_570"))  #87Sr/86Sr of old carbonates undergoing weathering at 570 Myrs ago
      Rv_570 <- resamples_constant(Rv_570, row_position=which(userInput[,"parameter"]=="Rv_570")) #87Sr/86Sr of sub-aerial and submarine volcanic rocks at 570 Myrs ago
      Rg_570 <- resamples_constant(Rg_570, row_position=which(userInput[,"parameter"]=="Rg_570")) #87Sr/86Sr of non-volcanic silicates (granites) at 570 Myrs ago
      Fob <- resamples_constant(Fob, row_position=which(userInput[,"parameter"]=="Fob"))  #Ca and Mg flux between basalt and seawater
      COC <- resamples_constant(COC, row_position=which(userInput[,"parameter"]=="COC"))  #mass of carbon in ocean
      Ga <- resamples_constant(Ga, row_position=which(userInput[,"parameter"]=="Ga")) #mass of old crustal organic carbon
      Ssa <- resamples_constant(Ssa, row_position=which(userInput[,"parameter"]=="Ssa"))  #mass of old CaSO4 sulfur
      Spa <- resamples_constant(Spa, row_position=which(userInput[,"parameter"]=="Spa"))  #mass of old pyrite sulfur
      ST <- resamples_constant(ST, row_position=which(userInput[,"parameter"]=="ST")) #mass of sulfur in oceans + "interacting rocks" (i.e., carbon in rocks undergoing weathering, burial, etc.)
      dlst <- resamples_constant(dlst, row_position=which(userInput[,"parameter"]=="dlst")) #d34S of ST
      CT <- resamples_constant(CT, row_position=which(userInput[,"parameter"]=="CT")) #mass of carbon in oceans + "interacting rocks" (i.e., carbon in rocks undergoing weathering, burial, etc.)
      dlct <- resamples_constant(dlct, row_position=which(userInput[,"parameter"]=="dlct")) #d13C of CT
      kwpy <- resamples_constant(kwpy, row_position=which(userInput[,"parameter"]=="kwpy")) #rate constant expressing mass dependence for young pyrite sulfur
      kwsy <- resamples_constant(kwsy, row_position=which(userInput[,"parameter"]=="kwsy")) #rate constant expressing mass dependence for young CaSO4 sulfur
      kwgy <- resamples_constant(kwgy, row_position=which(userInput[,"parameter"]=="kwgy")) #rate constant expressing mass dependence for young organic matter weathering
      kwcy <- resamples_constant(kwcy, row_position=which(userInput[,"parameter"]=="kwcy")) #rate constant expressing mass dependence for young carbonate weathering
      
      #generate empty matrices for individual calculations of O2 and CO2
      O2_resamples <- matrix(nrow=ageN, ncol=resampleN)
      CO2_resamples <- matrix(nrow=ageN, ncol=resampleN)
      print("Done filling input arrays")
      
      
      ######################################
      ### code for GEOCARBSULFvolc model ###
      ######################################
      
      Dt <- 10  #time-step (millions of years, Myrs)
      oxygen_0 <- 38 #mass of atmospheric O2 at the present-day
      
      #start of resampling loop (i)
      for (i in 1:resampleN) {
        RCO2 <- 10  #atmospheric CO2 (ratio between CO2 at time t to the Pleistocene mean [taken as 250 ppm]); this initial value is a place-holder (it is solved for explicitly)
        #these variables are recalculated at each time-step
        oxygen <- oxygen_570[1,i]
        Gy <- Gy_570[1,i]
        Cy <- Cy_570[1,i]
        Ca <- Ca_570[1,i]
        Ssy <- Ssy_570[1,i]
        Spy <- Spy_570[1,i]
        dlsy <- dlsy_570[1,i]
        dlcy <- dlcy_570[1,i]
        dlpy <- dlpy_570[1,i]
        dlpa <- dlpa_570[1,i]
        dlgy <- dlgy_570[1,i]
        dlga <- dlga_570[1,i]
        dlsa <- (dlst[1,i]*ST[1,i]-(dlpy*Spy+dlsy*Ssy+dlpa*Spa[1,i]))/Ssa[1,i]  #d34S value of old CaSO4 sulfur
        dlca <- (dlct[1,i]*CT[1,i]-(dlgy*Gy+dlcy*Cy+dlga*Ga[1,i]))/Ca  #d13C value of old crustal carbonate carbon   
        Rcy <- Rcy_570[1,i]
        Rca <- Rca_570[1,i]
        
        #start the nested time loop (j)
        for (j in 1:ageN) {
          failed_run <- FALSE  #flag for failed runs
          t <- time_arrays[j,"age"] #age of time-step, in Myrs ago
          
          #calculate factors that are influenced by glacial vs. non-glacial state ("GCM")
          if ((t<=330 & t>=260) || t<=40) { #for glacial periods between 260 and 330 Myrs ago and between 35 and 0 Myrs ago; BASIC code calls for a 270-340 Myrs ago interval, but see Fielding et al 2008 (GSA Special Paper 441: 343-354) for justification
            GCM <- GLAC[j,i]*deltaT2X[j,i]/log(2) #in GEOCARBSULF, deltaT2X = GCM*ln(2); called capital gamma in Berner (2004)
          } else {
            GCM <- deltaT2X[j,i]/log(2) }
          
          #calculate factors related to vegetation type
          if (t<=570 & t>380) { #vegetation = domination by non-vascular plants
            fE <- LIFE[j,i] #effect of plants on weathering rate at time (t) to the present-day
            fBB <- (1+ACTcarb[j,i]*GCM*log(RCO2)-ACTcarb[j,i]*Ws[j,i]*(t/570)+ACTcarb[j,i]*GEOG[j,i])*RCO2^exp_fnBb[j,i]  }  #effect of CO2 on plant-assisted weathering for carbonates at time (t) to the present-day
          if (t<=380 & t>350) { #vegetation = ramp-up to gymnosperm domination
            fE <- (GYM[j,i]-LIFE[j,i])*((380-t)/30)+LIFE[j,i]
            fBB <- ((380-t)/30)*((1+ACTcarb[j,i]*GCM*log(RCO2)-ACTcarb[j,i]*Ws[j,i]*(t/570)+ACTcarb[j,i]*GEOG[j,i])*(2*RCO2/(1+RCO2))^FERT[j,i])+((t-350)/30)*(1+ACTcarb[j,i]*GCM*log(RCO2)-ACTcarb[j,i]*Ws[j,i]*(t/570)+ACTcarb[j,i]*GEOG[j,i])*RCO2^exp_fnBb[j,i] }
          if (t<=350)  {
            fBB <- (1+ACTcarb[j,i]*GCM*log(RCO2)-ACTcarb[j,i]*Ws[j,i]*(t/570)+ACTcarb[j,i]*GEOG[j,i])*(2*RCO2/(1+RCO2))^FERT[j,i] }
          if (t<=350 & t>130)  { #vegetation = gymnosperm domination
            fE <- GYM[j,i]  }
          if (t<=130 & t>80) { #vegetation = ramp-up to angiosperm domination
            fE <- (1-GYM[j,i])*((130-t)/50)+GYM[j,i]  }
          if (t<=80)  { #vegetation = angiosperm domination
            fE <- 1 }
          
          #calculate source fluxes (weathering and degassing); see pp. 5660-5661 in Berner (2006a) for a discussion of the "Fwp", "Fws", and "Fwg" fluxes
          Fwpy <- fA[j,i]*fR[j,i]*kwpy[j,i]*Spy  #weathering flux of young pyrite
          Fwsy <- fA[j,i]*fD[j,i]*kwsy[j,i]*Ssy  #weathering flux of young CaSO4 sulfur
          Fwgy <- fA[j,i]*fR[j,i]*kwgy[j,i]*Gy #weathering flux of young organic carbon
          Fwcy <- fA[j,i]*fD[j,i]*fL[j,i]*fE*fBB*kwcy[j,i]*Cy  #weathering flux of young carbonate carbon
          Fwpa <- fR[j,i]*Fwpa_0[j,i]  #weathering flux of old pyrite
          Fwsa <- fA[j,i]*fD[j,i]*Fwsa_0[j,i]  #weathering flux of old CaSO4 sulfur
          Fwga <- fR[j,i]*Fwga_0[j,i]  #weathering flux of old organic carbon
          Fwca <- fA[j,i]*fD[j,i]*fL[j,i]*fE*fBB*Fwca_0[j,i] #weathering flux of old carbonate carbon
          Fmp <- fSR[j,i]*Fmp_0[j,i] #sulfur degassing flux from volcanism, metamorphism, and diagenesis of pyrite
          Fms <- fSR[j,i]*Fms_0[j,i] #sulfur degassing flux from volcanism, metamorphism, and diagenesis of CaSO4
          Fmg <- fSR[j,i]*Fmg_0[j,i] #carbon degassing flux from volcanism, metamorphism, and diagenesis of organics
          Fmc <- fSR[j,i]*fC[j,i]*Fmc_0[j,i] #carbon degassing flux from volcanism, metamorphism, and diagenesis of carbonate
          Fyop <- Fwpa+Fmp  #degassing + weathering flux of pyrite
          Fyos <- Fwsa+Fms  #degassing + weathering flux of CaSO4 sulfur
          Fyog <- Fwga+Fmg  #degassing + weathering flux of organic carbon
          Fyoc <- Fwca+Fmc  #degassing + weathering flux of carbonate carbon
          
          #calculate sink fluxes (burial)
          CAPd34S <- CAPd34S_0[j,i]*((oxygen/oxygen_0)^n[j,i]) #isotopic fractionation between sulfate sulfur and pyrite sulfur (see Berner 2006a)
          CAPd13C <- CAPd13C_0[j,i]+J[j,i]*(oxygen/oxygen_0-1) #isotopic fractionation between carbonate and organic matter (see Berner 2006a)
          Fbp <- (1/CAPd34S)*((d34S[j,i]-dlsy)*Fwsy+(d34S[j,i]-dlsa)*Fwsa+(d34S[j,i]-dlpy)*Fwpy+(d34S[j,i]-dlpa)*Fwpa+(d34S[j,i]-dlsa)*Fms+(d34S[j,i]-dlpa)*Fmp) #burial flux of pyrite
          Fbg <- (1/CAPd13C)*((d13C[j,i]-dlcy)*Fwcy+(d13C[j,i]-dlca)*Fwca+(d13C[j,i]-dlgy)*Fwgy+(d13C[j,i]-dlga)*Fwga+(d13C[j,i]-dlca)*Fmc+(d13C[j,i]-dlga)*Fmg) #burial flux of organic carbon
          Fbs <- Fwpy+Fwpa+Fwsy+Fwsa+Fms+Fmp-Fbp  #burial flux of CaSO4 sulfur
          Fbc <- Fwgy+Fwga+Fwcy+Fwca+Fmc+Fmg-Fbg  #burial flux of carbonate carbon
          
          #incorporate the volcanic/non-volcanic components (the "volc" in GEOCARBSULFvolc; see Berner 2006b & 2008)
          Roc <- (Sr[j,i]/10000)+0.7  #87Sr/86Sr of seawater as recorded in carbonates
          Rg <- Rg_570[j,i]-NV[j,i]*(1-fR[j,i]^(1/exp_NV[j,i]))  #87Sr/86Sr of non-volcanic silicates (same as "Rnv" in Berner 2006b)
          A <- ((Rv_570[j,i]-Roc)*fSR[j,i]*Fob[j,i])/(Fbc-Fwcy-Fwca) #note: this is always a negative number because Roc > Rv
          B <- Fwcy/(Fbc-Fwcy-Fwca)
          D <- Fwca/(Fbc-Fwcy-Fwca)
          E <- Fbc/(Fbc-Fwcy-Fwca)
          Xvolc <- (A+B*Rcy+D*Rca-E*Roc+Rg)/(Rg-Rv_570[j,i]) #fraction of total Ca and Mg silicate weathering drived from volcanic rocks
          fvolc <- (VNV[j,i]*Xvolc+1-Xvolc)/(VNV[j,i]*Xvolc_0[j,i]+1-Xvolc_0[j,i])  #volcanic weathering effect at time (t) relative to present-day
          
          #calculate oxygen mass for the time-step
          if (t<570)  {
            oxygen <- oxygen+(Fbg+(15/8)*Fbp)*Dt-(Fwgy+Fwga+Fmg)*Dt-(15/8)*(Fwpy+Fwpa+Fmp)*Dt }    
          
          #expression that summarizes the chemical weathering of silicates at time (t) relative to the present-day in the absence of climatic effects (other than the effect of CO2 on carbonate weathering); it is calculated by dividing total silicate chemical weathering at time (t)--as determined by mass balance between burial and degassing (numerator)--by the non-climatic processes that affect silicate chemical weathering multiplied by the present-day chemical weathering flux, Fwsi_0 (denominator) (see equation 5.2 in Berner 2004) (called "fB" in BASIC scripts)
          fwsi_no_climate <- (Fbc-Fwcy-Fwca)/(((fAw_fA[j,i]*fA[j,i]*fD[j,i])^exp_fD[j,i])*fE*fR[j,i]*fvolc*Fwsi_0[j,i])
          
          #test for failed runs (negative numbers, NaN's, or oxygen <5% or >50%)
          if (min(B,D,E,fwsi_no_climate,Fwpy,Fwsy,Fwgy,Fwcy,Fwpa,Fwsa,Fwga,Fwca,Fmp,Fms,Fmg,Fmc,CAPd13C,CAPd34S,Fbp,Fbg,Fbs,Fbc,Spy,Ssy,Gy,Cy,Ca,Xvolc,fvolc)<=0 || is.nan(fwsi_no_climate+A+B+D+E+Fwpy+Fwsy+Fwgy+Fwcy+Fwpa+Fwsa+Fwga+Fwca+Fmp+Fms+Fmg+Fmc+CAPd13C+CAPd34S+Fbp+Fbg+Fbs+Fbc+Spy+Ssy+Gy+Cy+Ca+dlpy+dlpa+dlsy+dlsa+dlgy+dlga+dlcy+dlca+Rcy+Rca+Xvolc+fvolc+GCM+oxygen) || 100*(oxygen/(oxygen+143))<5 || 100*(oxygen/(oxygen+143))>50)  {
            failed_run <- TRUE }
          
          #calculate atmospheric CO2 (ppm) through iterative convergence (see FORTRAN scripts of Park & Royer 2011 for details); this is done by calculating the climatic factors that affect silicate weathering rates normalized to the present-day (fwsi_climate, calculated below; called "Fbbs" in BASIC scripts); fwsi_climate combines the expressions "fBt(CO2)" and "fBb(CO2)" in Berner (2004) (see his equations 2.6-7, 2.29, & 5.2); through inversion of fwsi_climate ("W", "V", & "X" below) and comparison to fwsi_no_climate (calculated above), RCO2 can be determined; see pp. 72-76 in Berner (2004) for details; iterative convergence is necessary because RCO2--taken from the previous time-step--is needed to calculate some of the dependent parameters; the initial calculation of the new time-step for RCO2 is therefore not likely to be correct
          RCO2_old <- 2*RCO2  #this is here simply to ensure that the convergence isn't satisfied on the first step
          iteration_count <- 0
          if (t<=570 & t>380 & failed_run==FALSE)  {
            while  (abs(RCO2/RCO2_old-1) > 0.01) {
              iteration_count <- iteration_count+1
              RCO2_old <- RCO2
              fwsi_climate <- (RCO2^(exp_fnBb[j,i]+ACT[j,i]*GCM))*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^exp_fD[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])          
              W <- ((exp_fnBb[j,i]+ACT[j,i]*GCM)*RCO2^(-exp_fnBb[j,i]+ACT[j,i]*GCM))*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^exp_fD[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              V <- (RCO2^(exp_fnBb[j,i]+ACT[j,i]*GCM))*exp_fD[j,i]*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^-(1-exp_fD[j,i]))*(RT[j,i]*GCM/RCO2)*exp(-ACT[j,i]*Ws[j,i]*t/570)*exp(ACT[j,i]*GEOG[j,i])
              if (is.nan(fwsi_climate+W+V)==TRUE || iteration_count==iteration_threshold)  {
                failed_run <- TRUE
                break()
              }
              if (RCO2 > ((fwsi_climate - fwsi_no_climate)/(W+V))) {
                RCO2 <- RCO2-0.9*((fwsi_climate - fwsi_no_climate)/(W+V))  #damp the iteration to avoid overshoot
              } else {
                RCO2 <- 0.2*RCO2  #damp the iteration (convert the iteration to geometric shrinkage to avoid nonpositive value in overshoot)
              }
            } #end of while loop
          } #end of t>380 loop
          
          if (t<=380 & t>350 & failed_run==FALSE) { #the expressions for this time interval are more complex because the effects of plants on weathering are linearly mixed in; this helps to prevent model failure
            while  (abs(RCO2/RCO2_old-1) > 0.01) {
              iteration_count <- iteration_count+1
              RCO2_old <- RCO2
              fwsi_climate_old <- (RCO2^(exp_fnBb[j,i]+ACT[j,i]*GCM))*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^exp_fD[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              W_old <- (exp_fnBb[j,i]+ACT[j,i]*GCM)*RCO2^(-exp_fnBb[j,i]+ACT[j,i]*GCM)*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^exp_fD[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              V_old <- RCO2^(exp_fnBb[j,i]+ACT[j,i]*GCM)*exp_fD[j,i]*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^-(1-exp_fD[j,i]))*(RT[j,i]*GCM/RCO2)*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              
              fwsi_climate_new <- ((2^FERT[j,i])*RCO2^(FERT[j,i]+ACT[j,i]*GCM))*((1+RCO2)^(-FERT[j,i]))*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^exp_fD[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              W_new <- (2^FERT[j,i])*(FERT[j,i]+ACT[j,i]*GCM)*(RCO2^(FERT[j,i]+ACT[j,i]*GCM-1))*((1+RCO2)^-FERT[j,i])*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^exp_fD[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              V_new <- (-FERT[j,i]*(1+RCO2)^-(1+FERT[j,i]))*((2^FERT[j,i])*RCO2^(FERT[j,i]+ACT[j,i]*GCM))*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^exp_fD[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              X_new <- exp_fD[j,i]*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^-(1-exp_fD[j,i]))*(RT[j,i]*GCM/RCO2)*(2^FERT[j,i]*RCO2^(FERT[j,i]+ACT[j,i]*GCM))*((1+RCO2)^-FERT[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              
              fwsi_climate <- (t-350)/31*fwsi_climate_old+(381-t)/31*fwsi_climate_new
              Fw_v_x <- (t-350)/31*(W_old + V_old)+(381-t)/31*(W_new + V_new + X_new)
              
              if (is.nan(fwsi_climate_old + W_old + V_old + fwsi_climate_new + W_new + V_new + X_new + fwsi_climate + Fw_v_x)==TRUE  || iteration_count==iteration_threshold)  {
                failed_run <- TRUE
                break()
              }
              if (RCO2>((fwsi_climate - fwsi_no_climate)/Fw_v_x))  {
                RCO2 <- RCO2-0.9*((fwsi_climate - fwsi_no_climate)/Fw_v_x)  #damp the iteration to avoid overshoot
              } else {
                RCO2 <- 0.2*RCO2  #damp the iteration (convert the iteration to geometric shrinkage to avoid nonpositive value in overshoot)
              }
            } #end of while loop
          } #end of t<=380 & t>350 loop
          
          if (t<=350 & failed_run==FALSE) {
            while  (abs(RCO2/RCO2_old-1) > 0.01) {
              iteration_count <- iteration_count+1
              RCO2_old <- RCO2
              fwsi_climate <- ((2^FERT[j,i])*RCO2^(FERT[j,i]+ACT[j,i]*GCM))*((1+RCO2)^(-FERT[j,i]))*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^exp_fD[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              W <- (2^FERT[j,i])*(FERT[j,i]+ACT[j,i]*GCM)*(RCO2^(FERT[j,i]+ACT[j,i]*GCM-1))*((1+RCO2)^(-FERT[j,i]))*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^exp_fD[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              V <- (-FERT[j,i]*(1+RCO2)^-(1+FERT[j,i]))*((2^FERT[j,i])*RCO2^(FERT[j,i]+ACT[j,i]*GCM))*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^exp_fD[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              X <- exp_fD[j,i]*((1+RT[j,i]*GCM*log(RCO2)-RT[j,i]*Ws[j,i]*(t/570)+RT[j,i]*GEOG[j,i])^-(1-exp_fD[j,i]))*(RT[j,i]*GCM/RCO2)*(2^FERT[j,i]*RCO2^(FERT[j,i]+ACT[j,i]*GCM))*((1+RCO2)^-FERT[j,i])*exp(-ACT[j,i]*Ws[j,i]*(t/570))*exp(ACT[j,i]*GEOG[j,i])
              if (is.nan(fwsi_climate + W+V+X)==TRUE  || iteration_count==iteration_threshold)  {
                failed_run <- TRUE
                break()
              }
              if (RCO2 > ((fwsi_climate - fwsi_no_climate)/(W+V+X))) {
                RCO2 <- RCO2-0.9*((fwsi_climate - fwsi_no_climate)/(W+V+X))  #damp the iteration to avoid overshoot
              } else {
                RCO2 <- 0.2*RCO2  #damp the iteration (convert the iteration to geometric shrinkage to avoid nonpositive value in overshoot)
              }
            } #end of while loop
          } #end of t<=350 loop
          
          if (RCO2<0.6 || RCO2>200) { #test for failed runs when CO2 < 150 ppm or > 50000 ppm
            failed_run <- TRUE}
          
          #calculate new masses for the next time-step
          Spy <- Spy+(Fbp-Fwpy-Fyop)*Dt  #mass of young pyrite sulfur
          Ssy <- Ssy+(Fbs-Fwsy-Fyos)*Dt  #mass of young CaSO4 sulfur
          Gy <- Gy+(Fbg-Fwgy-Fyog)*Dt  #mass of young crustal organic carbon
          Cy <- Cy+(Fbc-Fwcy-Fyoc)*Dt  #mass of young crustal carbonate carbon
          Ca <- CT[j,i]-Gy-Ga[j,i]-Cy-COC[j,i]  #mass of old crustal carbonate carbon
          
          #calculate new isotopic values for the next time-step
          dlpy <- dlpy+((d34S[j,i]-dlpy-CAPd34S)*Fbp/Spy)*Dt  #d34S of young pyrite sulfur
          dlpa <- dlpa+(Fyop*(dlpy-dlpa)/Spa[j,i])*Dt  #d34S of old pyrite sulfur
          dlsy <- dlsy+((d34S[j,i]-dlsy)*Fbs/Ssy)*Dt  #d34S value of young CaSO4 sulfur
          dlsa <- dlsa+(Fyos*(dlsy-dlsa)/Ssa[j,i])*Dt  #d34S value of old CaSO4 sulfur
          dlgy <- dlgy+((d13C[j,i]-dlgy-CAPd13C)*Fbg/Gy)*Dt  #d13C of young organic matter
          dlga <- dlga+(Fyog*(dlgy-dlga)/Ga[j,i])*Dt  #d13C of old organic matter
          dlcy <- dlcy+((d13C[j,i]-dlcy)*Fbc/Cy)*Dt  #d13C value of young crustal carbonate carbon
          dlca <- dlca+(Fyoc*(dlcy-dlca)/Ca)*Dt  #d13C value of old crustal carbonate carbon
          Rcy <- Rcy+((Roc-Rcy)*Fbc/Cy)*Dt  #87Sr/86Sr of young carbonates undergoing weathering over the time-step
          Rca <- Rca+((Rcy-Rca)*Fyoc/Ca)*Dt  #87Sr/86Sr of old carbonates undergoing weathering over the time-step
          
          #fill in the calculated values for O2 (%) and CO2 (ppm) (by default, failed runs will be filled with "NAs")        
          if (failed_run==FALSE) {
            CO2_resamples[j,i] <- RCO2*250  #RCO2 is multiplied by 250 (Pleistocene mean CO2) to convert to ppm units
            O2_resamples[j,i] <- 100*(oxygen/(oxygen+143))  #converts O2 mass to O2 (%)      
          }  else {
            RCO2 <- 10 #reset RCO2 seed for next run
            if (input_distribution==TRUE & loop_parameters==FALSE) {
              Sr[j,i] <- NA; d13C[j,i] <- NA; d34S[j,i] <- NA; fR[j,i] <- NA; fL[j,i] <- NA; fA[j,i] <- NA; fAw_fA[j,i] <- NA; fD[j,i] <- NA; GEOG[j,i] <- NA; RT[j,i] <- NA; fSR[j,i] <- NA; fC[j,i] <- NA    
              ACT[j,i] <- NA; ACTcarb[j,i] <- NA; VNV[j,i] <- NA; NV[j,i] <- NA; exp_NV[j,i] <- NA; LIFE[j,i] <- NA; GYM[j,i] <- NA; FERT[j,i] <- NA; exp_fnBb[j,i] <- NA; deltaT2X[j,i] <- NA; GLAC[j,i] <- NA; J[j,i] <- NA; n[j,i] <- NA; Ws[j,i] <- NA; exp_fD[j,i] <- NA; Fwpa_0[j,i] <- NA; Fwsa_0[j,i] <- NA; Fwga_0[j,i] <- NA; Fwca_0[j,i] <- NA; Fmg_0[j,i] <- NA; Fmc_0[j,i] <- NA; Fmp_0[j,i] <- NA; Fms_0[j,i] <- NA; Fwsi_0[j,i] <- NA; Xvolc_0[j,i] <- NA; CAPd13C_0[j,i] <- NA; CAPd34S_0[j,i] <- NA; oxygen_570[j,i] <- NA; Gy_570[j,i] <- NA; Cy_570[j,i] <- NA; Ca_570[j,i] <- NA; Ssy_570[j,i] <- NA; Spy_570[j,i] <- NA; dlsy_570[j,i] <- NA; dlcy_570[j,i] <- NA; dlpy_570[j,i] <- NA; dlpa_570[j,i] <- NA; dlgy_570[j,i] <- NA; dlga_570[j,i] <- NA; Rcy_570[j,i] <- NA; Rca_570[j,i] <- NA; Rv_570[j,i] <- NA; Rg_570[j,i] <- NA; Fob[j,i] <- NA; COC[j,i] <- NA; Ga[j,i] <- NA; Ssa[j,i] <- NA; Spa[j,i] <- NA; ST[j,i] <- NA; dlst[j,i] <- NA; CT[j,i] <- NA; dlct[j,i] <- NA; kwpy[j,i] <- NA; kwsy[j,i] <- NA; kwgy[j,i] <- NA; kwcy[j,i] <- NA
            }   
          }  #end of if..else loop
          
          #test for estimated oxygen at present-day to be between 19-23%, and estimated CO2 at present-day to be between 200-300 ppm; if not, the whole time-series is considered a failed run    
          if (t==0 & (is.nan(oxygen+RCO2) || 100*(oxygen/(oxygen+143))<19 || 100*(oxygen/(oxygen+143))>23 || RCO2<0.8 || RCO2>1.2))  {
            CO2_resamples[,i] <- NA
            O2_resamples[,i] <- NA
            if (input_distribution==TRUE & loop_parameters==FALSE) {
              Sr[,i] <- NA; d13C[,i] <- NA; d34S[,i] <- NA; fR[,i] <- NA; fL[,i] <- NA; fA[,i] <- NA; fAw_fA[,i] <- NA; fD[,i] <- NA; GEOG[,i] <- NA; RT[,i] <- NA; fSR[,i] <- NA; fC[,i] <- NA    
              ACT[,i] <- NA; ACTcarb[,i] <- NA; VNV[,i] <- NA; NV[,i] <- NA; exp_NV[,i] <- NA; LIFE[,i] <- NA; GYM[,i] <- NA; FERT[,i] <- NA; exp_fnBb[,i] <- NA; deltaT2X[,i] <- NA; GLAC[,i] <- NA; J[,i] <- NA; n[,i] <- NA; Ws[,i] <- NA; exp_fD[,i] <- NA; Fwpa_0[,i] <- NA; Fwsa_0[,i] <- NA; Fwga_0[,i] <- NA; Fwca_0[,i] <- NA; Fmg_0[,i] <- NA; Fmc_0[,i] <- NA; Fmp_0[,i] <- NA; Fms_0[,i] <- NA; Fwsi_0[,i] <- NA; Xvolc_0[,i] <- NA; CAPd13C_0[,i] <- NA; CAPd34S_0[,i] <- NA; oxygen_570[,i] <- NA; Gy_570[,i] <- NA; Cy_570[,i] <- NA; Ca_570[,i] <- NA; Ssy_570[,i] <- NA; Spy_570[,i] <- NA; dlsy_570[,i] <- NA; dlcy_570[,i] <- NA; dlpy_570[,i] <- NA; dlpa_570[,i] <- NA; dlgy_570[,i] <- NA; dlga_570[,i] <- NA; Rcy_570[,i] <- NA; Rca_570[,i] <- NA; Rv_570[,i] <- NA; Rg_570[,i] <- NA; Fob[,i] <- NA; COC[,i] <- NA; Ga[,i] <- NA; Ssa[,i] <- NA; Spa[,i] <- NA; ST[,i] <- NA; dlst[,i] <- NA; CT[,i] <- NA; dlct[,i] <- NA; kwpy[,i] <- NA; kwsy[,i] <- NA; kwgy[,i] <- NA; kwcy[,i] <- NA
            }   
            
          }   #end of if statement for t=0
          
        } #end of nested time loop (j)
        
        zz <- i/10000 #printing progress of run (every 10000 runs)
        if (round(zz)==zz) {
          cat(i,"runs of",resampleN,"are complete",date(),"\n")
        }
        
      } #end of resampling loop (i)
      
      
      ########################################
      ### code for outputting summary file ###
      ########################################
      
      #calculate median and percentiles for O2 and CO2
      for (j in 1:ageN) {
        CO2[j,] <- median(CO2_resamples[j,], na.rm=TRUE)
        O2[j,] <- median(O2_resamples[j,], na.rm=TRUE)
        failed_runs[j,] <- sum(is.na(CO2_resamples[j,]))/resampleN*100    
        if (resampleN>1)  {
          for (k in 1:length(percentile_values))  {
            percentiles_CO2[j,k] <- quantile(x=CO2_resamples[j,], probs=percentile_values[k], na.rm=TRUE)
            percentiles_O2[j,k] <- quantile(x=O2_resamples[j,], probs=percentile_values[k], na.rm=TRUE)
          }
        } #end of if statement
      } #end of loop for filling the median and percentile arrays
      
      #merge results and export summary file to working directory
      if (resampleN==1)  {
        GEOCARB_output <- cbind(age, failed_runs, CO2, O2)
      } else {
        if (loop_parameters==TRUE)  {
          start_column <- y*h-(y-1)
          end_column <- y*h
          GEOCARB_output[,start_column:end_column] <- cbind(age, failed_runs, CO2, percentiles_CO2, O2, percentiles_O2)
          column_header[,start_column:end_column] <- c("age (Myrs ago)","failed runs (%)",paste("CO2 for",userInput[h,"parameter"],"(ppm)",sep=" "),percentile_values,paste("O2 for",userInput[h,"parameter"],"(%)",sep=" "),percentile_values)
          cat(date(),"   finished with parameter # ",h,"of",z,"\n")
        }
        else {
          GEOCARB_output <- cbind(age, failed_runs, CO2, percentiles_CO2, O2, percentiles_O2)
        }
      } #end of if...else statement
      
    } #end of input parameter loop (h)
    
    if (loop_parameters==TRUE)  {
      colnames(GEOCARB_output) <- column_header }
    write.csv(GEOCARB_output, "GEOCARB_output.csv")
    
    if (input_distribution==TRUE & loop_parameters==FALSE) {
      resampled_input_constants <- matrix(nrow=length(userInput[,"parameter"]), ncol=2)
      colnames(resampled_input_constants) <- c("resampled_mean", "resampled_two_sigma")
      rownames(resampled_input_constants) <- userInput[, "parameter"]
      resampled_input_constants <- cbind(userInput[, -c(7:11)], resampled_input_constants)
      resampled_input_constants["ACT", "resampled_mean"] <- mean(ACT, na.rm=TRUE); resampled_input_constants["ACT", "resampled_two_sigma"] <- 2*sd(ACT, na.rm=TRUE)
      resampled_input_constants["ACTcarb", "resampled_mean"] <- mean(ACTcarb, na.rm=TRUE); resampled_input_constants["ACTcarb", "resampled_two_sigma"] <- 2*sd(ACTcarb, na.rm=TRUE)
      resampled_input_constants["VNV", "resampled_mean"] <- mean(VNV, na.rm=TRUE); resampled_input_constants["VNV", "resampled_two_sigma"] <- 2*sd(VNV, na.rm=TRUE)
      resampled_input_constants["NV", "resampled_mean"] <- mean(NV, na.rm=TRUE); resampled_input_constants["NV", "resampled_two_sigma"] <- 2*sd(NV, na.rm=TRUE)
      resampled_input_constants["exp_NV", "resampled_mean"] <- mean(exp_NV, na.rm=TRUE); resampled_input_constants["exp_NV", "resampled_two_sigma"] <- 2*sd(exp_NV, na.rm=TRUE)
      resampled_input_constants["LIFE", "resampled_mean"] <- mean(LIFE, na.rm=TRUE); resampled_input_constants["LIFE", "resampled_two_sigma"] <- 2*sd(LIFE, na.rm=TRUE)
      resampled_input_constants["GYM", "resampled_mean"] <- mean(GYM, na.rm=TRUE); resampled_input_constants["GYM", "resampled_two_sigma"] <- 2*sd(GYM, na.rm=TRUE)
      resampled_input_constants["FERT", "resampled_mean"] <- mean(FERT, na.rm=TRUE); resampled_input_constants["FERT", "resampled_two_sigma"] <- 2*sd(FERT, na.rm=TRUE)
      resampled_input_constants["exp_fnBb", "resampled_mean"] <- mean(exp_fnBb, na.rm=TRUE); resampled_input_constants["exp_fnBb", "resampled_two_sigma"] <- 2*sd(exp_fnBb, na.rm=TRUE)
      log_deltaT2X <- log(deltaT2X); resampled_input_constants["deltaT2X", "resampled_mean"] <- exp(mean(log_deltaT2X, na.rm=TRUE)); resampled_input_constants["deltaT2X", "resampled_two_sigma"] <- exp(2*sd(log_deltaT2X, na.rm=TRUE))
      resampled_input_constants["GLAC", "resampled_mean"] <- mean(GLAC, na.rm=TRUE); resampled_input_constants["GLAC", "resampled_two_sigma"] <- 2*sd(GLAC, na.rm=TRUE)
      resampled_input_constants["J", "resampled_mean"] <- mean(J, na.rm=TRUE); resampled_input_constants["J", "resampled_two_sigma"] <- 2*sd(J, na.rm=TRUE)
      resampled_input_constants["n", "resampled_mean"] <- mean(n, na.rm=TRUE); resampled_input_constants["n", "resampled_two_sigma"] <- 2*sd(n, na.rm=TRUE)
      resampled_input_constants["Ws", "resampled_mean"] <- mean(Ws, na.rm=TRUE); resampled_input_constants["Ws", "resampled_two_sigma"] <- 2*sd(Ws, na.rm=TRUE)
      resampled_input_constants["exp_fD", "resampled_mean"] <- mean(exp_fD, na.rm=TRUE); resampled_input_constants["exp_fD", "resampled_two_sigma"] <- 2*sd(exp_fD, na.rm=TRUE)
      resampled_input_constants["Fwpa_0", "resampled_mean"] <- mean(Fwpa_0, na.rm=TRUE); resampled_input_constants["Fwpa_0", "resampled_two_sigma"] <- 2*sd(Fwpa_0, na.rm=TRUE)
      resampled_input_constants["Fwsa_0", "resampled_mean"] <- mean(Fwsa_0, na.rm=TRUE); resampled_input_constants["Fwsa_0", "resampled_two_sigma"] <- 2*sd(Fwsa_0, na.rm=TRUE)
      resampled_input_constants["Fwga_0", "resampled_mean"] <- mean(Fwga_0, na.rm=TRUE); resampled_input_constants["Fwga_0", "resampled_two_sigma"] <- 2*sd(Fwga_0, na.rm=TRUE)
      resampled_input_constants["Fwca_0", "resampled_mean"] <- mean(Fwca_0, na.rm=TRUE); resampled_input_constants["Fwca_0", "resampled_two_sigma"] <- 2*sd(Fwca_0, na.rm=TRUE)
      resampled_input_constants["Fmg_0", "resampled_mean"] <- mean(Fmg_0, na.rm=TRUE); resampled_input_constants["Fmg_0", "resampled_two_sigma"] <- 2*sd(Fmg_0, na.rm=TRUE)
      resampled_input_constants["Fmc_0", "resampled_mean"] <- mean(Fmc_0, na.rm=TRUE); resampled_input_constants["Fmc_0", "resampled_two_sigma"] <- 2*sd(Fmc_0, na.rm=TRUE)
      resampled_input_constants["Fmp_0", "resampled_mean"] <- mean(Fmp_0, na.rm=TRUE); resampled_input_constants["Fmp_0", "resampled_two_sigma"] <- 2*sd(Fmp_0, na.rm=TRUE)
      resampled_input_constants["Fms_0", "resampled_mean"] <- mean(Fms_0, na.rm=TRUE); resampled_input_constants["Fms_0", "resampled_two_sigma"] <- 2*sd(Fms_0, na.rm=TRUE)
      resampled_input_constants["Fwsi_0", "resampled_mean"] <- mean(Fwsi_0, na.rm=TRUE); resampled_input_constants["Fwsi_0", "resampled_two_sigma"] <- 2*sd(Fwsi_0, na.rm=TRUE)
      resampled_input_constants["Xvolc_0", "resampled_mean"] <- mean(Xvolc_0, na.rm=TRUE); resampled_input_constants["Xvolc_0", "resampled_two_sigma"] <- 2*sd(Xvolc_0, na.rm=TRUE)
      resampled_input_constants["CAPd13C_0", "resampled_mean"] <- mean(CAPd13C_0, na.rm=TRUE); resampled_input_constants["CAPd13C_0", "resampled_two_sigma"] <- 2*sd(CAPd13C_0, na.rm=TRUE)
      resampled_input_constants["CAPd34S_0", "resampled_mean"] <- mean(CAPd34S_0, na.rm=TRUE); resampled_input_constants["CAPd34S_0", "resampled_two_sigma"] <- 2*sd(CAPd34S_0, na.rm=TRUE)
      resampled_input_constants["oxygen_570", "resampled_mean"] <- mean(oxygen_570, na.rm=TRUE); resampled_input_constants["oxygen_570", "resampled_two_sigma"] <- 2*sd(oxygen_570, na.rm=TRUE)
      resampled_input_constants["Gy_570", "resampled_mean"] <- mean(Gy_570, na.rm=TRUE); resampled_input_constants["Gy_570", "resampled_two_sigma"] <- 2*sd(Gy_570, na.rm=TRUE)
      resampled_input_constants["Cy_570", "resampled_mean"] <- mean(Cy_570, na.rm=TRUE); resampled_input_constants["Cy_570", "resampled_two_sigma"] <- 2*sd(Cy_570, na.rm=TRUE)
      resampled_input_constants["Ca_570", "resampled_mean"] <- mean(Ca_570, na.rm=TRUE); resampled_input_constants["Ca_570", "resampled_two_sigma"] <- 2*sd(Ca_570, na.rm=TRUE)
      resampled_input_constants["Ssy_570", "resampled_mean"] <- mean(Ssy_570, na.rm=TRUE); resampled_input_constants["Ssy_570", "resampled_two_sigma"] <- 2*sd(Ssy_570, na.rm=TRUE)
      resampled_input_constants["Spy_570", "resampled_mean"] <- mean(Spy_570, na.rm=TRUE); resampled_input_constants["Spy_570", "resampled_two_sigma"] <- 2*sd(Spy_570, na.rm=TRUE)
      resampled_input_constants["dlsy_570", "resampled_mean"] <- mean(dlsy_570, na.rm=TRUE); resampled_input_constants["dlsy_570", "resampled_two_sigma"] <- 2*sd(dlsy_570, na.rm=TRUE)
      resampled_input_constants["dlcy_570", "resampled_mean"] <- mean(dlcy_570, na.rm=TRUE); resampled_input_constants["dlcy_570", "resampled_two_sigma"] <- 2*sd(dlcy_570, na.rm=TRUE)
      resampled_input_constants["dlpy_570", "resampled_mean"] <- mean(dlpy_570, na.rm=TRUE); resampled_input_constants["dlpy_570", "resampled_two_sigma"] <- 2*sd(dlpy_570, na.rm=TRUE)
      resampled_input_constants["dlpa_570", "resampled_mean"] <- mean(dlpa_570, na.rm=TRUE); resampled_input_constants["dlpa_570", "resampled_two_sigma"] <- 2*sd(dlpa_570, na.rm=TRUE)
      resampled_input_constants["dlgy_570", "resampled_mean"] <- mean(dlgy_570, na.rm=TRUE); resampled_input_constants["dlgy_570", "resampled_two_sigma"] <- 2*sd(dlgy_570, na.rm=TRUE)
      resampled_input_constants["dlga_570", "resampled_mean"] <- mean(dlga_570, na.rm=TRUE); resampled_input_constants["dlga_570", "resampled_two_sigma"] <- 2*sd(dlga_570, na.rm=TRUE)
      resampled_input_constants["Rcy_570", "resampled_mean"] <- mean(Rcy_570, na.rm=TRUE); resampled_input_constants["Rcy_570", "resampled_two_sigma"] <- 2*sd(Rcy_570, na.rm=TRUE)
      resampled_input_constants["Rca_570", "resampled_mean"] <- mean(Rca_570, na.rm=TRUE); resampled_input_constants["Rca_570", "resampled_two_sigma"] <- 2*sd(Rca_570, na.rm=TRUE)
      resampled_input_constants["Rv_570", "resampled_mean"] <- mean(Rv_570, na.rm=TRUE); resampled_input_constants["Rv_570", "resampled_two_sigma"] <- 2*sd(Rv_570, na.rm=TRUE)
      resampled_input_constants["Rg_570", "resampled_mean"] <- mean(Rg_570, na.rm=TRUE); resampled_input_constants["Rg_570", "resampled_two_sigma"] <- 2*sd(Rg_570, na.rm=TRUE)
      resampled_input_constants["Fob", "resampled_mean"] <- mean(Fob, na.rm=TRUE); resampled_input_constants["Fob", "resampled_two_sigma"] <- 2*sd(Fob, na.rm=TRUE)
      resampled_input_constants["COC", "resampled_mean"] <- mean(COC, na.rm=TRUE); resampled_input_constants["COC", "resampled_two_sigma"] <- 2*sd(COC, na.rm=TRUE)
      resampled_input_constants["Ga", "resampled_mean"] <- mean(Ga, na.rm=TRUE); resampled_input_constants["Ga", "resampled_two_sigma"] <- 2*sd(Ga, na.rm=TRUE)
      resampled_input_constants["Ssa", "resampled_mean"] <- mean(Ssa, na.rm=TRUE); resampled_input_constants["Ssa", "resampled_two_sigma"] <- 2*sd(Ssa, na.rm=TRUE)
      resampled_input_constants["Spa", "resampled_mean"] <- mean(Spa, na.rm=TRUE); resampled_input_constants["Spa", "resampled_two_sigma"] <- 2*sd(Spa, na.rm=TRUE)
      resampled_input_constants["ST", "resampled_mean"] <- mean(ST, na.rm=TRUE); resampled_input_constants["ST", "resampled_two_sigma"] <- 2*sd(ST, na.rm=TRUE)
      resampled_input_constants["dlst", "resampled_mean"] <- mean(dlst, na.rm=TRUE); resampled_input_constants["dlst", "resampled_two_sigma"] <- 2*sd(dlst, na.rm=TRUE)
      resampled_input_constants["CT", "resampled_mean"] <- mean(CT, na.rm=TRUE); resampled_input_constants["CT", "resampled_two_sigma"] <- 2*sd(CT, na.rm=TRUE)
      resampled_input_constants["dlct", "resampled_mean"] <- mean(dlct, na.rm=TRUE); resampled_input_constants["dlct", "resampled_two_sigma"] <- 2*sd(dlct, na.rm=TRUE)
      resampled_input_constants["kwpy", "resampled_mean"] <- mean(kwpy, na.rm=TRUE); resampled_input_constants["kwpy", "resampled_two_sigma"] <- 2*sd(kwpy, na.rm=TRUE)
      resampled_input_constants["kwsy", "resampled_mean"] <- mean(kwsy, na.rm=TRUE); resampled_input_constants["kwsy", "resampled_two_sigma"] <- 2*sd(kwsy, na.rm=TRUE)
      resampled_input_constants["kwgy", "resampled_mean"] <- mean(kwgy, na.rm=TRUE); resampled_input_constants["kwgy", "resampled_two_sigma"] <- 2*sd(kwgy, na.rm=TRUE)
      resampled_input_constants["kwcy", "resampled_mean"] <- mean(kwcy, na.rm=TRUE); resampled_input_constants["kwcy", "resampled_two_sigma"] <- 2*sd(kwcy, na.rm=TRUE)
      
      resampled_input_arrays <- time_arrays
      for (j in 1:ageN) {
        resampled_input_arrays[j, match("Sr", colnames(resampled_input_arrays))] <- mean(Sr[j,], na.rm=TRUE); resampled_input_arrays[j, match("Sr", colnames(resampled_input_arrays))+1] <- 2*sd(Sr[j,], na.rm=TRUE)
        resampled_input_arrays[j, match("d13C", colnames(resampled_input_arrays))] <- mean(d13C[j,], na.rm=TRUE); resampled_input_arrays[j, match("d13C", colnames(resampled_input_arrays))+1] <- 2*sd(d13C[j,], na.rm=TRUE)
        resampled_input_arrays[j, match("d34S", colnames(resampled_input_arrays))] <- mean(d34S[j,], na.rm=TRUE); resampled_input_arrays[j, match("d34S", colnames(resampled_input_arrays))+1] <- 2*sd(d34S[j,], na.rm=TRUE)
        resampled_input_arrays[j, match("fR", colnames(resampled_input_arrays))] <- mean(fR[j,], na.rm=TRUE); resampled_input_arrays[j, match("fR", colnames(resampled_input_arrays))+1] <- 2*sd(fR[j,], na.rm=TRUE)
        resampled_input_arrays[j, match("fL", colnames(resampled_input_arrays))] <- mean(fL[j,], na.rm=TRUE); resampled_input_arrays[j, match("fL", colnames(resampled_input_arrays))+1] <- 2*sd(fL[j,], na.rm=TRUE)
        if (Godderis==TRUE) {
          resampled_input_arrays[j, match("fA_Godderis", colnames(resampled_input_arrays))] <- mean(fA[j,], na.rm=TRUE); resampled_input_arrays[j, match("fA_Godderis", colnames(resampled_input_arrays))+1] <- 2*sd(fA[j,], na.rm=TRUE)
          resampled_input_arrays[j, match("fAw_fA_Godderis", colnames(resampled_input_arrays))] <- mean(fAw_fA[j,], na.rm=TRUE); resampled_input_arrays[j, match("fAw_fA_Godderis", colnames(resampled_input_arrays))+1] <- 2*sd(fAw_fA[j,], na.rm=TRUE)
          resampled_input_arrays[j, match("fD_Godderis", colnames(resampled_input_arrays))] <- mean(fD[j,], na.rm=TRUE); resampled_input_arrays[j, match("fD_Godderis", colnames(resampled_input_arrays))+1] <- 2*sd(fD[j,], na.rm=TRUE)
          resampled_input_arrays[j, match("GEOG_Godderis", colnames(resampled_input_arrays))] <- mean(GEOG[j,], na.rm=TRUE); resampled_input_arrays[j, match("GEOG_Godderis", colnames(resampled_input_arrays))+1] <- 2*sd(GEOG[j,], na.rm=TRUE) }
        else {
          resampled_input_arrays[j, match("fA", colnames(resampled_input_arrays))] <- mean(fA[j,], na.rm=TRUE); resampled_input_arrays[j, match("fA", colnames(resampled_input_arrays))+1] <- 2*sd(fA[j,], na.rm=TRUE)
          resampled_input_arrays[j, match("fAw_fA", colnames(resampled_input_arrays))] <- mean(fAw_fA[j,], na.rm=TRUE); resampled_input_arrays[j, match("fAw_fA", colnames(resampled_input_arrays))+1] <- 2*sd(fAw_fA[j,], na.rm=TRUE)
          resampled_input_arrays[j, match("fD", colnames(resampled_input_arrays))] <- mean(fD[j,], na.rm=TRUE); resampled_input_arrays[j, match("fD", colnames(resampled_input_arrays))+1] <- 2*sd(fD[j,], na.rm=TRUE)
          resampled_input_arrays[j, match("GEOG", colnames(resampled_input_arrays))] <- mean(GEOG[j,], na.rm=TRUE); resampled_input_arrays[j, match("GEOG", colnames(resampled_input_arrays))+1] <- 2*sd(GEOG[j,], na.rm=TRUE)
        }
        resampled_input_arrays[j, match("RT", colnames(resampled_input_arrays))] <- mean(RT[j,], na.rm=TRUE); resampled_input_arrays[j, match("RT", colnames(resampled_input_arrays))+1] <- 2*sd(RT[j,], na.rm=TRUE)
        resampled_input_arrays[j, match("fSR", colnames(resampled_input_arrays))] <- mean(fSR[j,], na.rm=TRUE); resampled_input_arrays[j, match("fSR", colnames(resampled_input_arrays))+1] <- 2*sd(fSR[j,], na.rm=TRUE)
        resampled_input_arrays[j, match("fC", colnames(resampled_input_arrays))] <- mean(fC[j,], na.rm=TRUE); resampled_input_arrays[j, match("fC", colnames(resampled_input_arrays))+1] <- 2*sd(fC[j,], na.rm=TRUE)      
      }
      write.csv(resampled_input_arrays, "resampled_input_arrays.csv")
      write.csv(resampled_input_constants, "resampled_input_constants.csv")
    }
    
    GEOCARB_output <- data.frame(GEOCARB_output)
    names(GEOCARB_output) <- c("age", "failed_runs", "CO2_ppm", "CO2_0.025", "CO2_0.975", "O2_ppm", "O2_0.025", "O2_0.975")
    pack_list(list(GEOCARB_output, time_arrays, original_arrays))
  })
  
  
}

shinyApp(ui, server)

# geop <- readRDS("/Users/mukappa/Downloads/GEOCARBSULF_App_output.RDS")
# # geop1 <- readRDS("/Users/mukappa/Downloads/GEOCARBSULF_App_output (1).RDS")
# geologic_periods <- data.frame(Period = c("Quaternary", "Neogene", "Paleogene", "Cretaceous", "Jurassic", "Triassic", "Permian", "Carboniferous", "Devonian", "Silurian", "Ordovician", "Cambrian"),
#                                From = c(2.6, 23, 66, 145, 201.3, 251.9, 298.9, 358.9, 419.2, 443.8, 485.4, 538.8))
# geologic_periods$To <- c(0, geologic_periods$From[-nrow(geologic_periods)])
# #geologic_periods$Hex_Colour <- 
# 
# rgb2h <- function(x){
#   l <- unlist(str_split(x, "/"))
#   l <- as.hexmode(as.numeric(l))
#   paste0(l, collapse = "")
# }
# 
# x <- "249/249/127"
# rgb2h("249/249/127")


