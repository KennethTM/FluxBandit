library(shiny);library(dplyr);library(lubridate)

#FluxBandit
#Shiny app for interactive processing and calculation of greenhouse gas emissions using commercial and DIY type sensor systems
#Kenneth Thorø Martinsen
#https://github.com/KennethTM/FluxBandit

version <- "FluxBandit-v0.6"
options(shiny.maxRequestSize=50*1024^2)

ui <- fluidPage(
  
  titlePanel(version),
  
  em("'Making flux calculations so simple it should be criminal'"),
  
  p("Follow the 6 steps to select, save and export your flux measurements!"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tags$b("1) Select sensor type"),
      
      radioButtons("filetype", "", choices = c("DIY", "LGR"), selected = ""),
      
      tags$p("Optional: Sensor start time (yyyy-mm-dd hh:mm:ss):"), textInput("time_input", NULL, value="", width = "200px"),
      
      tags$b("2) Upload CSV File"),
      
      
      fileInput("file", "",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      tags$hr(),
      
      tags$b("Step 3) Enter metadata"),
      
      tags$p("Chamber volume (L):"), numericInput("chamber_vol", NULL, 280, min = 0, width = "100px"),
      
      tags$p("Chamber area (m2):"), numericInput("chamber_area", NULL, 0.33, min = 0, width = "100px"),
      
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
      
      htmlOutput("result_string"), 
      
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
      df <- read.csv(input$file$datapath) |> 
        filter(!is.na(datetime)) |> 
        filter(lead(!is.na(SampleNumber)), !is.na(SampleNumber)) |>
        rename(rh = RH., ch4_smv=CH4smV) |> 
        mutate(datetime = ymd_hms(datetime),
               airt = as.numeric(tempC),
               abs_H = (6.112*exp((17.67*airt)/(airt+243.5))*rh*18.02)/((273.15+airt)*100*0.08314),
               ppm_H20 = 1358.326542*abs_H,
               co2 = (K30_CO2/(1-(ppm_H20/10^6))),
               V0 = abs_H*5.160442+268.39739,
               RsR0 = ((5000/ch4_smv)-1)/((5000/V0)-1),
               ch4 = 19.969879*(RsR0^-1.5626939)+-0.0093223822*abs_H*(19.969879*RsR0^-1.5626939)-15.366139) |> 
        rename(water = ppm_H20) %>% 
        group_by(datetime) %>% 
        summarise_at(vars(rh, airt, co2, ch4, water), list(mean)) |> 
        select(datetime, rh, airt, co2, ch4, water)
    }else if(input$filetype == "LGR"){
      df <- read.csv(input$file$datapath, skip=1) |> 
        mutate(datetime = dmy_hms(SysTime)) |> 
        select(datetime, co2 = X.CO2.d_ppm, ch4 = X.CH4.d_ppm, airt = GasT_C, water = X.H2O._ppm)
    }
    
    if(input$time_input != ""){
      time_input <- ymd_hms(input$time_input)
      time_step <- median(diff(df$datetime))
      df <- df %>% 
        mutate(datetime = seq(time_input, by=time_step, length.out = n()))
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
    
    plot(x = data_subset$datetime,
         y = data_subset$co2,
         ylab=expression("CO"[2]*" (ppm)"), 
         xlab="Datetime",
         main = "Overview plot")
    
    co2_min = min(data_subset$co2)
    co2_max = max(data_subset$co2)
    ch4_min = min(data_subset$ch4)
    ch4_max = max(data_subset$ch4)
    ch4_scaled = (co2_max - co2_min)*((data_subset$ch4-ch4_min)/(ch4_max - ch4_min))+co2_min
    
    ch4_labels = pretty(data_subset$ch4)
    ch4_at = (co2_max - co2_min)*((ch4_labels-ch4_min)/(ch4_max - ch4_min))+co2_min
      
    points(x = data_subset$datetime, y = ch4_scaled, col="coral")
    axis(4, at = ch4_at, labels = ch4_labels, col="coral", col.ticks="coral")
    
    legend("topright", c(expression("CO"[2]), expression("CH"[4])), 
           col = c("black", "coral"), pch=19)
    
  })
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  data_subset <- reactive({
    req(input$file)
    
    if (!is.null(ranges2$x)) {
      ranges2$x <- as_datetime(ranges2$x)

      data_subset <- data() %>%
        filter(between(datetime, ranges2$x[1], ranges2$x[2]),
               between(co2, ranges2$y[1], ranges2$y[2])) %>%
        mutate(sec = cumsum(c(0, diff(as.numeric(datetime)))))
      
      lm_model_co2 <- lm(co2~sec, data = data_subset)
      slope_co2 <- coef(lm_model_co2)[2]
      intercept_co2 <- coef(lm_model_co2)[1]
      r2_co2 <- summary(lm_model_co2)$r.squared
      
      lm_model_ch4 <- lm(ch4~sec, data = data_subset)
      slope_ch4 <- coef(lm_model_ch4)[2]
      intercept_ch4 <- coef(lm_model_ch4)[1]
      r2_ch4 <- summary(lm_model_ch4)$r.squared
      
      mean_temp <- mean(data_subset$airt)

      R <- 0.08206 #L atm K^-1 mol^-1
      
      co2_flux <- (slope_co2*(input$chamber_vol*input$atm_pres))/(R*(273.15+mean_temp)*input$chamber_area)
      ch4_flux <- (slope_ch4*(input$chamber_vol*input$atm_pres))/(R*(273.15+mean_temp)*input$chamber_area)
      
      results_string <- paste0("<b>CO<sub>2</sub></b>: slope = ", round(slope_co2, 2), " (ppm s<sup>-1</sup>)", 
                               ", flux = ", round(co2_flux, 2), " (µmol m<sup>-2</sup> s<sup>-1</sup>)", 
                               ", R<sup>2</sup> = ", round(r2_co2, 2),
                               "<br>", 
                               "<b>CH<sub>4</sub></b>: slope = ", round(slope_ch4, 2), " (ppm s<sup>-1</sup>)",
                               ", flux = ", round(ch4_flux, 2), " (µmol m<sup>-2</sup> s<sup>-1</sup>)",
                               ", R<sup>2</sup> = ", round(r2_ch4, 2))
      
      results <- data.frame("version" = version,
                            "processing_date" = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S", tz="UTC"),
                            "sensor_type" = input$filetype,
                            "id" = as.character(input$sample_id),
                            "start" = strftime(ranges2$x[1], "%Y-%m-%d %H:%M:%S", tz="UTC"),
                            "end" = strftime(ranges2$x[2], "%Y-%m-%d %H:%M:%S", tz="UTC"),
                            "CO2_slope" = slope_co2,
                            "CO2_intercept" = intercept_co2,
                            "CO2_R2" = r2_co2,
                            "CH4_slope" = slope_ch4,
                            "CH4_intercept" = intercept_ch4,
                            "CH4_R2" = r2_ch4,
                            "temperature" = mean_temp, 
                            "chamber_volume" = input$chamber_vol,
                            "chamber_area" = input$chamber_area,
                            "CO2_flux_umol_m2_s" = co2_flux,
                            "CH4_flux_umol_m2_s" = ch4_flux)
      
    }else{
      data_subset <- data() %>% 
        mutate(sec = cumsum(c(0, diff(as.numeric(datetime)))))
      results <- data.frame()
      results_string <- ""
    }
    
    return(list("df" = data_subset, 
                "results" = results, 
                "results_string" = results_string))
    
  })
  
  output$plot_zoom <- renderPlot({
    
    data <- data_subset()
    
    plot(x = data$df$sec,
         y = data$df$co2,
         ylab=expression("CO"[2]*" (ppm)"), 
         xlab="Time steps",
         main= "Zoom plot")
    
    co2_min = min(data$df$co2)
    co2_max = max(data$df$co2)
    water_min = min(data$df$water)
    water_max = max(data$df$water)
    water_scaled = (co2_max - co2_min)*((data$df$water-water_min)/(water_max - water_min))+co2_min
    
    points(x = data$df$sec, y = water_scaled, col="lightblue", type="l")
    
    ch4_min = min(data$df$ch4)
    ch4_max = max(data$df$ch4)
    ch4_scaled = (co2_max - co2_min)*((data$df$ch4-ch4_min)/(ch4_max - ch4_min))+co2_min
    
    ch4_labels = pretty(data$df$ch4)
    ch4_at = (co2_max - co2_min)*((ch4_labels-ch4_min)/(ch4_max - ch4_min))+co2_min
    
    points(x = data$df$sec, y = ch4_scaled, col="coral")
    axis(4, at = ch4_at, labels = ch4_labels, col="coral", col.ticks="coral")
    
    if (!is.null(ranges2$x)){
      
      output$result_string <- renderText(data$results_string)
      
      abline(data$results$CO2_intercept,
             data$results$CO2_slope,
             col = "black", lwd = 4)
      
      lm_model_ch4_scaled <- lm(ch4_scaled~data$df$sec)
      slope_ch4_scaled <- coef(lm_model_ch4_scaled)[2]
      intercept_ch4_scaled <- coef(lm_model_ch4_scaled)[1]
      
      abline(intercept_ch4_scaled,
             slope_ch4_scaled,
             col = "coral", lwd = 4)

    }
    
    legend("topright", 
           c(expression("CO"[2]), expression("CH"[4]), expression("H"[2]*"O")), 
           col = c("black", "coral", "lightblue"), pch=19)
    
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
                                               "CO2_R2", "CO2_flux_umol_m2_s", 
                                               "CH4_R2", "CH4_flux_umol_m2_s")])
    
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