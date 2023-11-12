#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny ggplot2
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  #Sys.setenv(
  #  OPENAI_API_KEY = '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'
  #)
  
  
  globConfig <- reactiveValues()
  
  globConfig$heartrate_color <- "success"
  globConfig$lowFreq_color <- "success"
  globConfig$highFreq_color <- "success"
  globConfig$ratio_color <- "success"
  globConfig$agent_color <- "green"
  globConfig$agent_icon <- "leaf"
  globConfig$agent_text <- "Dormant"
  
  
  mod_mainDash_server("mainDash_1", globConfig = globConfig)
  
}
