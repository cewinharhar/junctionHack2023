#' mainDash UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import echarts4r
mod_mainDash_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),

    fluidRow(
      column(9,
             # Plots
             fluidRow(
              bs4ValueBoxOutput(outputId = ns("heartrate"), width = 3),
              plotOutput(ns("heartrate_plot"), height="100px", width="600px"),
             ),
             fluidRow(
               bs4ValueBoxOutput(outputId = ns("lowFreq"), width = 3),
              plotOutput(ns("lowFreq_plot"), height="100px", width="600px"),
             ),
             fluidRow(
               bs4ValueBoxOutput(outputId = ns("highFreq"), width = 3),
              plotOutput(ns("highFreq_plot"), height="100px", width="600px"),
             ),
             fluidRow(
              bs4ValueBoxOutput(outputId = ns("ratio"), width = 3),
              plotOutput(ns("ratio_plot"), height="100px", width="600px"),
             ),
             uiOutput(ns("statusPic"))
  
             # echarts4rOutput(ns("gaugeChart")),
             # tableOutput(ns("table")),

      ),
      # column(),
      column(3, align = "left",
             # Image
             fluidRow(
              column(8, align="left",
                uiOutput(ns("agent"))
              ),
              column(8, align="left",
                uiOutput(ns("actionElements"))
              )
               
             )
             

      )
    )
    
    #--
  )
}
    
#' mainDash Server Functions
#'
#' @noRd 
mod_mainDash_server <- function(id, globConfig){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Load Data
    df <- read.csv("inst/app/www/data.csv")[1:216,]
    df$ts <- seq(1:216)
    
    dfStream <- df[1,]
    
    counter <-  reactiveVal(0)
    
    


    sendDataAndGetResponse <- function(scenario = 1, ip_address="localhost:8001", Heartrate = 0.1, lowFreq = 0.1, highFreq = 0.1, ratio = 0.1, sleep = 0.1, mood = 0.1) {
      # Preparing the data to be sent
      postData <- list(
        heartrate = Heartrate, 
        lowFreq = lowFreq, 
        highFreq = highFreq, 
        ratio = ratio, 
        sleep = sleep, 
        mood = mood
      )
      
      # Making the POST request
      # url <- paste0("http://", ip_address, "/prevent")
      # response <- httr::POST(url, body = postData, encode = "json")
      # 
      # # Assuming the response is in JSON format
      # responseContent <- httr::content(response, "parsed")
      # 
      # #THIS IS THE REAL ACTION OUTPUT
      # action <- responseContent$action
      #THIS IS FAKE BECAUSE MODEL IS NOT PRETRAINED
      action <- scenario
      
      # browser()
      # Sending data to GPT model and getting response
      gptResponds <- openai::create_chat_completion(
        model = "gpt-4-1106-preview",
        message = list(
          list(
            "role" = "user",
            "content" = paste(
              "profile of Jonnas: Jonnas is a 53 year old single guy that like x, y, z. 
              Problem: Jonnas is misusing opioids for his chronic pain. This has negative effects on him. He is trying to quit and prevent a relaps. 
              you are: you are an ai agent reacting to the actual state of jonnass. The different states are:
              1. Light urge of opioids requires a positive reinforcement notification that helps him to stay strong
              2. Medium urge of opioids requires something more active like meeting a friend, go walk with the dog etc
              3. Strong urge of opiods requires to call his doctor or theurpeut to handle the urge
              
              Your task: 
                For a given state write the message that Jonas is going to see on his phone. Try to be motivating and personalized. 
              
              Only answer with the notification without title and make it short! This is very important for me
              State:", action
            )
          )
        ),
        max_tokens = 500
      )
      
      return(gptResponds$choices$message.content)
    }

    #------------------------------------
    values <- reactiveValues(df = dfStream, requestSent = FALSE)
    #------------------------------------
    
    
    observeEvent(reactiveTimer(500)(),{ # Trigger every 2 seconds
      
      counter(counter() + 1)  # Increment the counter
      
      values$df <- isolate({
        # get and bind the new data
        values_df <- rbind(values$df, df[counter(),]) 
      })
      
      
      
      if (df[counter(),]$Scenario == 0){
        
        output$actionElements <- renderUI({
          fluidRow(
            img(src = "www/robot.png", width="100px")
          )

        })
        output$statusPic <- renderUI({
          fluidRow(
            column(3, align="left",
                   img(src = "www/relax.png", width="275px", style = "opacity: 0.2;")
            ),
            column(3,align="center",
                   img(src = "www/friends.png", width="275px", style = "opacity: 0.2;")
            ),
            column(3,align="right",
                   img(src = "www/doctor.png", width="275px", style = "opacity: 0.2;")
            )
          )
          #--
        })
      }
      
        
      if (df[counter(),]$Scenario == 1 && values$requestSent == FALSE){
        # browser()
        
        globConfig$heartrate_color <- "warning"
        globConfig$lowFreq_color <- "success"
        globConfig$highFreq_color <- "warning"
        globConfig$ratio_color <- "success"
        globConfig$agent_color <- "orange"
        globConfig$agent_text <- "State: Light Urge"
        
        output$actionElements <- renderUI({
          # browser()

          fluidRow(
            img(src = "https://cdn2.iconfinder.com/data/icons/robotics-butterscotch-vol-2/512/Robot_Head-512.png", width="100px"),
            br(),
            br(),
            HTML(paste0("<b style='font-size: 25px;'>"
                        ,sendDataAndGetResponse(
                          scenario = isolate({df[counter(),]$Scenario}),
                          ip_address="localhost:8001", 
                          Heartrate = isolate({df[counter(),]$Heartrate}) , 
                          lowFreq = isolate({df[counter(),]$HRV.Low.Frequency.Changes}), 
                          highFreq = isolate({df[counter(),]$HRV.High.Frequency.Changes}), 
                          ratio = isolate({df[counter(),]$HRV.Ratio.Changes}), 
                          sleep = 0.6, 
                          mood = 0.4
                        ), "</b>"))
            
          ) })
        output$statusPic <- renderUI({
          fluidRow(
            column(3, align="left",
                   img(src = "www/relax.png", width="275px")
            ),
            column(3,align="center",
                   img(src = "www/friends.png", width="275px", style = "opacity: 0.2;")
            ),
            column(3,align="right",
                   img(src = "www/doctor.png", width="275px", style = "opacity: 0.2;")
            )
          )
          #--
        })

          values$requestSent <- TRUE
          
        }
      
      
      if (df[counter(),]$Scenario == 2 && values$requestSent == TRUE){
        globConfig$heartrate_color <- "warning"
        globConfig$lowFreq_color <- "success"
        globConfig$highFreq_color <- "danger"
        globConfig$ratio_color <- "success"
        globConfig$agent_color <- "orange"
        globConfig$agent_text <- "State: Medium Urge"
        
        # browser()
        
        output$actionElements <- renderUI({
          # browser()
          
          fluidRow(
            img(src = "https://cdn2.iconfinder.com/data/icons/robotics-butterscotch-vol-2/512/Robot_Head-512.png", width="100px"),
            br(),
            br(),
            HTML(paste0("<b style='font-size: 25px;'>"
                        ,sendDataAndGetResponse(
                          scenario = isolate({df[counter(),]$Scenario}),
                          ip_address="localhost:8001", 
                          Heartrate = isolate({df[counter(),]$Heartrate}) , 
                          lowFreq = isolate({df[counter(),]$HRV.Low.Frequency.Changes}), 
                          highFreq = isolate({df[counter(),]$HRV.High.Frequency.Changes}), 
                          ratio = isolate({df[counter(),]$HRV.Ratio.Changes}), 
                          sleep = 0.6, 
                          mood = 0.4
                        ), "</b>"))
            
            
          )
        })
        output$statusPic <- renderUI({
          fluidRow(
            column(3, align="left",
                   img(src = "www/relax.png", width="275px", style = "opacity: 0.2;")
            ),
            column(3,align="center",
                   img(src = "www/friends.png", width="275px")
            ),
            column(3,align="right",
                   img(src = "www/doctor.png", width="275px", style = "opacity: 0.2;")
            )
          )
          #--
        })
        
        values$requestSent <- FALSE

        
      }
      
      if (df[counter(),]$Scenario == 3 && values$requestSent == FALSE){
        globConfig$heartrate_color <- "danger"
        globConfig$lowFreq_color <- "warning"
        globConfig$highFreq_color <- "danger"
        globConfig$ratio_color <- "success"
        globConfig$agent_color <- "red"
        globConfig$agent_text <- "State: Emergency"
        
        output$actionElements <- renderUI({
          # browser()
          
          fluidRow(
            img(src = "https://cdn2.iconfinder.com/data/icons/robotics-butterscotch-vol-2/512/Robot_Head-512.png", width="100px"),
            br(),
            br(),
            HTML(paste0("<b style='font-size: 25px;'>"
                        ,sendDataAndGetResponse(
                          scenario = isolate({df[counter(),]$Scenario}),
                          ip_address="localhost:8001", 
                          Heartrate = isolate({df[counter(),]$Heartrate}) , 
                          lowFreq = isolate({df[counter(),]$HRV.Low.Frequency.Changes}), 
                          highFreq = isolate({df[counter(),]$HRV.High.Frequency.Changes}), 
                          ratio = isolate({df[counter(),]$HRV.Ratio.Changes}), 
                          sleep = 0.6, 
                          mood = 0.4
                        ), "</b>"))
            
            
          )
        })
        
        output$statusPic <- renderUI({
          fluidRow(
            column(3, align="left",
                   img(src = "www/relax.png", width="275px", style = "opacity: 0.2;")
            ),
            column(3,align="left",
                   img(src = "www/friends.png", width="275px", style = "opacity: 0.2;")
            ),
            column(3,align="left",
                   img(src = "www/doctor.png", width="275px")
            )
          )
          #--
        })
        values$requestSent <- TRUE
        
      }
      
      output$agent <- renderUI({
        HTML(paste0("<b style='color:",globConfig$agent_color ,"; font-size: 20px;'>",globConfig$agent_text, "</b>"))
      }) 

      #------------------------------------------------------------
      output$heartrate_plot <- renderPlot({
        ggplot(values_df, aes(x = ts, y = Heartrate)) +
          geom_line(color = "black", size = 1.5) + # Enhanced line aesthetics (thicker line)
          geom_point(color = "red", size = 3, shape = 21, fill = "white") + # Enhanced point aesthetics
          theme_minimal() +
          ylim(c(0, 120)) +
          theme(
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA), # Transparent background
            plot.background = element_rect(fill = "transparent", colour = NA) # Transparent plot background
          )
      })
      output$lowFreq_plot <- renderPlot({
        ggplot(values_df, aes(x = ts, y = HRV.Low.Frequency.Changes)) +
          geom_line(color = "black", size = 1.5) + # Enhanced line aesthetics (thicker line)
          geom_point(color = "red", size = 3, shape = 21, fill = "white") + # Enhanced point aesthetics
          theme_minimal() +
          # ylim(c(0, 120)) +
          theme(
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA), # Transparent background
            plot.background = element_rect(fill = "transparent", colour = NA) # Transparent plot background
          )
      })
      output$highFreq_plot <- renderPlot({
        ggplot(values_df, aes(x = ts, y = HRV.High.Frequency.Changes)) +
          geom_line(color = "black", size = 1.5) + # Enhanced line aesthetics (thicker line)
          geom_point(color = "red", size = 3, shape = 21, fill = "white") + # Enhanced point aesthetics
          theme_minimal() +
          # ylim(c(0, 120)) +
          theme(
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA), # Transparent background
            plot.background = element_rect(fill = "transparent", colour = NA) # Transparent plot background
          )
      })
      output$ratio_plot <- renderPlot({
        ggplot(values_df, aes(x = ts, y = HRV.Ratio.Changes)) +
          geom_line(color = "black", size = 1.5) + # Enhanced line aesthetics (thicker line)
          geom_point(color = "red", size = 3, shape = 21, fill = "white") + # Enhanced point aesthetics
          theme_minimal() +
          # ylim(c(0, 120)) +
          theme(
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA), # Transparent background
            plot.background = element_rect(fill = "transparent", colour = NA) # Transparent plot background
          )
      })
      
      
      
      #------------------------------------------------------------
      
      output$heartrate <- renderbs4ValueBox({
        bs4ValueBox(
          value = h1(values_df$Heartrate[nrow(values_df)] |> round(2) |> as.character()),
          subtitle = "",
          icon = icon("heart"),
          color = globConfig$heartrate_color,
          gradient = TRUE,
          footer = "Heartrate"
        )
      })
      
      output$lowFreq <- renderbs4ValueBox({
        bs4ValueBox(
          value = h1(values_df$HRV.Low.Frequency.Changes[nrow(values_df)] |> round(2)|> as.character()),
          subtitle = "",
          icon = icon("wave-square"),
          color = globConfig$lowFreq_color,
          gradient = TRUE,
          footer = "Low Frequency"
        )
      })
      
      output$highFreq <- renderbs4ValueBox({
        bs4ValueBox(
          value = h1(values_df$HRV.High.Frequency.Changes[nrow(values_df)] |> round(2)|> as.character()),
          subtitle = "",
          icon = icon("wave-square"),
          color = globConfig$highFreq_color,
          gradient = TRUE,
          footer = "High Frequency"
        )
      })
      
      output$ratio <- renderbs4ValueBox({
        bs4ValueBox(
          value = h1(values_df$HRV.Ratio.Changes[nrow(values_df)] |> round(2)|> as.character()),
          subtitle = "",
          icon = icon("percent"),
          color = globConfig$ratio_color,
          gradient = TRUE,
          footer = "ratio"
        )
      })
      
    })
    
    # # Render the gauge chart
    # output$gaugeChart <- renderEcharts4r({
    #   # browser()
    #   # if (is.null(gauge_value())){
    #   #   numi <- 5
    #   # } else {
    #   #   numi <- isolate(gauge_value())
    #   # }
    #   e_charts() |>
    #     e_gauge(10, "percent")  # Use the reactive gauge value
    # })    
    
    #-----------------------
    

    
    # # Render Plots
    # output$plot1 <- renderPlot({ 
    #   ggplot(df, aes(x = ts, y = Heartrate)) +
    #     geom_point(color = "blue") + # Change line color
    #     labs(title = "Your Plot Title", 
    #          x = "Time Step", 
    #          y = "Value on Y-Axis") + # Add titles and labels
    #     theme_minimal() + # Apply a minimal theme
    #     theme(text = element_text(size = 12))  
    # })
    # Similarly for plot2 and plot3 with temperature and rhb
    
    # observeEvent(input$nextScenario, {
    #   # Example data to send in POST request

    # 
    #   postData <- list(
    #     Heartrate = 0.1,            # Replace with actual data
    #     lowFreq = 0.1,            # Replace with actual data
    #     highFreq = 0.1,     # Replace with actual data
    #     ratio = 0.1,             # Replace with actual data
    #     sleep = 0.1,             # Replace with actual data
    #     mood = 0.1             # Replace with actual data
    #   )
    # #   
    # #   # Making the POST request
    #   response <- httr::POST("localhost:8001/prevent", body = postData, encode = "json")
    # #   
    # #   # Assuming the response is in JSON format
    #   responseContent <- content(response, "parsed")
    # #   
    # #   #------------------------------------------------
    #   gptResponds <- openai::create_chat_completion(
    #     model = "gpt-4-1106-preview",
    #     message  = list(
    #       list(
    #         "role" = "user",
    #         "content" = paste(
    #           "This is a test, i give you a number and you describe this number:",
    #           responseContent$action
    # 
    #         )
    #       )
    #     ),
    #     max_tokens = 500
    #   )
    #   
    #   # Update the reactive value or output to display the response
    #   # For example, if you want to display part of the response in a text block
    #   output$textBlock <- renderText({
    #     gptResponds$choices$message.content
    #   })
    # })
    # 
    # Update Text Block
    output$textBlock <- renderUI({
      # Code to display text received from API
    })    
    
  })
}
    
## To be copied in the UI
# mod_mainDash_ui("mainDash_1")
    
## To be copied in the server
# mod_mainDash_server("mainDash_1")
