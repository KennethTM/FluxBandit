library(shiny);library(dplyr);library(lubridate)

#FluxBandit-v0.2 
#Shiny app for interactive processing and calculation of greenhouse gas emissions using commercial and DIY type sensor systems
#Kenneth Thorø Martinsen
#https://github.com/KennethTM/FluxBandit

version <- "FluxBandit-v0.2"
options(shiny.maxRequestSize=50*1024^2)

ui <- fluidPage(
  
  titlePanel(version),
  
  em("'Making flux calculations so simple it should be criminal'"),
  
  p("Follow the 6 steps to select, save and export your flux measurements!"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tags$b("1) Select sensor type"),
      
      radioButtons("filetype", "", choices = c("DIY", "LGR"), selected = ""),
      
      
      tags$b("2) Upload CSV File"),
      
      
      fileInput("file", "",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      tags$hr(),
      
      tags$b("Step 3) Enter metadata"),
      
      tags$p("Chamber volume (L):"), numericInput("chamber_vol", NULL, 25, min = 0, width = "100px"),
      
      tags$p("Chamber area (m2):"), numericInput("chamber_area", NULL, 0.5, min = 0, width = "100px"),
      
      tags$p("Atmos. pressure (atm):"), numericInput("atm_pres", NULL, 1, min = 0, width = "100px"),
      
      tags$b("Step 4) Adjust time range"),
      
      sliderInput("range", "",
                  ymd_hm("2021-01-01 12:00"),
                  ymd_hm("2021-12-31 12:00"),
                  c(ymd_hm("2021-01-01 12:00"), 
                    ymd_hm("2021-12-31 12:00")),
                  60*60*24, 
                  timezone="+0000"),
      
      tags$hr(),
      
      tags$b("5) Save flux (repeat)"),
      
      tags$br(),
      
      tags$p("Sample ID (optional):"), textInput("sample_id", NULL, "ID", width = "200px"),
      
      actionButton("save", "Save"),
      
      tags$hr(),
      
      tags$b("6) Download data"),
      
      tags$br(),
      
      #Button to save the file
      downloadButton('download', 'Download')
      
    ),
    
    mainPanel(
      
      tags$b("Main plot"),
      p("Use slider (step 4) to adjust the x-axis"),
      
      plotOutput("plot",
                 brush = brushOpts(
                   id = "plot_brush",
                   resetOnNew = TRUE
                 )),
      
      tags$hr(),
      
      tags$b("Zoom plot"),
      p("Use mouse to select measurements in the main plot"),
      
      plotOutput("plot_zoom"),
      
      tags$hr(),
      
      tags$b("Saved data"),
      p("Table with saved data, export table as '.csv' file using the download button"),
      tableOutput("results")
      
    )
    
  )
)

server <- function(input, output, session){
  data <- reactive({
    
    req(input$file)
    req(input$filetype)
    
    if(input$filetype == "DIY"){
      df <- read.csv(input$file$datapath) %>% 
        filter(!is.na(datetime)) |> 
        filter(lead(!is.na(SampleNumber)), !is.na(SampleNumber)) %>% 
        mutate(datetime = ymd_hms(datetime),
               airt = as.numeric(tempC),
               abs_H = (6.112*exp((17.67*airt)/(airt+243.5))*rh*18.02)/((273.15 +airt)*100*0.08314),
               ppm_H20 = 1358.326542*abs_H,
               co2 = (K30_CO2/(1-(ppm_H20/10^6)))) |> 
        rename(rh = RH., co2, ch4_smv = CH4smV, ch4_rmv = CH4rmV) %>% 
        group_by(datetime) %>% 
        summarise_at(vars(rh, airt, co2, ch4_smv, ch4_rmv), list(mean)) %>% 
        select(datetime, rh, airt, co2, ch4_smv, ch4_rmv)
    }else if(input$filetype == "LGR"){
      df <- read.csv(input$file$datapath, skip=1) %>% 
        mutate(datetime = dmy_hms(Time)) %>% 
        select(datetime, co2 = X.CO2.d_ppm, ch4 = X.CH4.d_ppm, airt = GasT_C)
    }
    
    
    time_start <- min(df$datetime)
    time_end <- max(df$datetime)
    
    updateSliderInput(session, "range", value = c(time_start, time_end),
                      min = time_start, max = time_end, step = 60)
    
    return(df)
    
  })
  
  data_out <- data.frame()
  
  output$plot <- renderPlot({
    req(input$file)
    
    data_subset <- data() %>% 
      filter(between(datetime, input$range[1], input$range[2]))
    
    y_limits <- quantile(data_subset$co2, c(0.01, 0.99))
    
    plot(x = data_subset$datetime,
         y = data_subset$co2,
         ylab="CO2 (ppm)", xlab="Datetime",
         main = "Overview plot",
         ylim = y_limits)
  })
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  data_subset <- reactive({
    req(input$file)
    
    if (!is.null(ranges2$x)) {
      ranges2$x <- as_datetime(ranges2$x)
      
      if(input$filetype == "DIY"){
        data_subset <- data() %>%
          filter(between(datetime, ranges2$x[1], ranges2$x[2]),
                 between(co2, ranges2$y[1], ranges2$y[2])) %>% 
          mutate(sec = cumsum(c(0, diff(as.numeric(datetime))))) %>% 
          mutate(abs_H = (6.112*exp((17.67*airt)/(airt+243.5))*rh*18.02)/((273.15 +airt)*100*0.08314),
                 V0 = abs_H*5.160442+268.39739,
                 RsR0 = ((5000/ch4_smv)-1)/((5000/V0)-1),
                 ch4 = 19.969879*(RsR0^-1.5626939)+-0.0093223822*abs_H*(19.969879*RsR0^-1.5626939)-15.366139)
      }else if(input$filetype == "LGR"){
        data_subset <- data() %>%
          filter(between(datetime, ranges2$x[1], ranges2$x[2]),
                 between(co2, ranges2$y[1], ranges2$y[2])) %>% 
          mutate(sec = cumsum(c(0, diff(as.numeric(datetime)))))
      }
      
      lm_model_co2 <- lm(co2~sec, data = data_subset)
      slope_co2 <- coef(lm_model_co2)[2]
      intercept_co2 <- coef(lm_model_co2)[1]
      r2_co2 <- summary(lm_model_co2)$r.squared
      
      lm_model_ch4 <- lm(ch4~sec, data = data_subset)
      slope_ch4 <- coef(lm_model_ch4)[2]
      intercept_ch4 <- coef(lm_model_ch4)[1]
      r2_ch4 <- summary(lm_model_ch4)$r.squared
      
      mean_temp <- mean(data_subset$airt)
      
      results_string <- paste0("CO2: Slope = ", round(slope_co2, 2),
                               "; R-squared = ", round(r2_co2, 2),
                               " \n", 
                               "CH4: Slope = ", round(slope_ch4, 2),
                               "; R-squared = ", round(r2_ch4, 2))
      
      R <- 0.08206 #L atm K^-1 mol^-1
      
      results <- data.frame("version" = version,
                            "processing_date" = Sys.time(),
                            "sensor_type" = input$filetype,
                            "id" = as.character(input$sample_id),
                            "start" = as.character(ranges2$x[1]),
                            "end" = as.character(ranges2$x[2]),
                            "slope_co2" = slope_co2,
                            "intercept_co2" = intercept_co2,
                            "r_squared_co2" = r2_co2,
                            "slope_ch4" = slope_ch4,
                            "intercept_ch4" = intercept_ch4,
                            "r_squared_ch4" = r2_ch4,
                            "temperature" = mean_temp, 
                            "chamber_vol" = input$chamber_vol,
                            "chamber_area" = input$chamber_area,
                            "co2_flux_umol_m2_s" = (slope_co2*(input$chamber_vol*input$atm_pres))/(R*(273.15+mean_temp)/input$chamber_area),
                            "ch4_flux_umol_m2_s" = (slope_ch4*(input$chamber_vol*input$atm_pres))/(R*(273.15+mean_temp)/input$chamber_area)
      )
      
    }else{
      data_subset <- data() %>% 
        mutate(sec = cumsum(c(0, diff(as.numeric(datetime)))))
      results <- data.frame()
      results_string <- ""
    }
    
    return(list("df" = data_subset, "results" = results, 
                "results_string" = results_string))
    
  })
  
  output$plot_zoom <- renderPlot({
    
    data <- data_subset()
    
    plot(x = data$df$sec,
         y = data$df$co2,
         ylab="CO2 (ppm)", xlab="Seconds",
         main= "Zoom plot",
         xaxt="n")
    
    if (!is.null(ranges2$x)){
      
      text(x = (max(data$df$sec)-min(data$df$sec))*0.2,
           y = max(data$df$co2) - 2,
           labels = data$results_string)
      
      abline(data$results$intercept_co2,
             data$results$slope_co2,
             col = "coral", lwd = 6)
    }
    
  })
  
  observe({
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  observeEvent(input$save,{
    data_out <<- rbind(data_out, data_subset()$results)
    output$results <- renderTable(data_out[, c("id", "start", "end", 
                                               "r_squared_co2", "co2_flux_umol_m2_s", 
                                               "r_squared_ch4", "ch4_flux_umol_m2_s")])
  })
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0(version, "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data_out, file, row.names = FALSE)
    })  
  
}

shinyApp(ui, server)