lapply(c("shiny", "fredr", "purrr", "memoise", "tidyr",
         "ggplot2", "forecast", "reshape2", "dplyr",
         "scales"),
       library, character.only = TRUE)

# fredr_key("YOUR_API_KEY") https://research.stlouisfed.org/docs/api/api_key.html

tags <- memoise::memoise(function() {
  fredr(endpoint = "tags")
})

series <- memoise::memoise(function(.tag_name) {
  fredr(endpoint = "tags/series", tag_names = .tag_name)
})

observations_by_series <- memoise::memoise(function(.series_id) {
  fredr("series/observations", series_id = .series_id)
})

shinyServer(function(input, output) {

  output$tag_selector <- renderUI({
    selectInput("tag_selection", label = "1. Choose a tag", choices = sort(tags()$name),
                selected = "gdp")
  })
  output$tag_series_selector <- renderUI({
    req(input$tag_selection)
    selectInput("series_selection", label = "2. Choose a series",
                choices = sort(unique(series(input$tag_selection)$title)),
                selected = "Personal saving")
  })
  output$tag_series_id_selector <- renderUI({
    req(input$series_selection)
    ids <- series(input$tag_selection)
    ids <- ids[grepl(input$series_selection, ids$title), ] %>%
      arrange(id) %>% { .$id }
    selectInput("series_id_selection", label = "3. Choose an id",
                choices =  ids)
  })
  output$series_id_information <- renderTable({
    req(input$series_id_selection)
    series(input$tag_selection) %>%
      filter(id == input$series_id_selection) %>%
      select(Id = id, Title = title, Start = observation_start, End = observation_end, Freq = frequency, Units = units) %>%
      gather()
  })

  get_frequency <- reactive({
    req(input$tag_selection, input$series_id_selection)
    freq <- series(input$tag_selection) %>%
      filter(id == input$series_id_selection) %>% {
        .$frequency_short
      }
    if(length(freq) == 0)
      freq <- 1

    return(freq)
  })

  prepared_data <- reactive({
    req(input$series_id_selection, get_frequency())
    d <- observations_by_series(input$series_id_selection) %>%
      mutate(date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC")) %>%
      mutate(value = ifelse(value == ".", NA, value),
             value = as.numeric(value)) %>%
      filter(!is.na(value), !is.na(date)) %>%
      select(date, value)

    list(
      series = ts(d$value,
                  frequency = switch(
                    get_frequency(),
                    "A" = 1,
                    "Q" = 4,
                    "M" = 12,
                    stop("No match for frequency found!")
                  )),
      data = d
    )
  })

  fit_model <- reactive({
    req(prepared_data())
    list(model = auto.arima(prepared_data()$series),
         data = prepared_data()$data
    )
  })

  forecast_data <- reactive({
    req(fit_model())
    forecast(fit_model()$model, level = c(80, 95), h = 2)
  })

  apply_model <- reactive({
    req(fit_model())
    fit_model()$data %>%
      as.data.frame() %>%
      { data.frame(date = as.POSIXct(as.character(.$date), origin = "1970-01-01"),
                   actual = .$value,
                   fitted = fitted(fit_model()$model))
      }
  })

  periods_to_add <- reactive({
    req(forecast_data(), apply_model())

    d <- apply_model() %>%
      melt("date") %>%
      tbl_df() %>% arrange(date)

    get_next_periods <- function(.last_periods, .by) {
      lapply(.last_periods,
             function(x) { seq.POSIXt(x, length.out = 3, by = "year")[2:3] }) %>%
        unlist() %>% as.POSIXct(., origin = "1970-01-01")
    }

    unique(d$date) %>% sort() %>% tail(1) %>% {
      get_next_periods((.), switch(get_frequency(),
                                   "A" = "year",
                                   "M" = "month",
                                   "Q" = "quarter"
      ))
    }
  })

  funggcast <- function(dn, fcast, .dates, .periods_to_add){
    # modified from http://davenportspatialanalytics.squarespace.com/blog/2012/3/14/plotting-forecast-objects-in-ggplot-part-1-extracting-the-da.html
    require(zoo) #needed for the 'as.yearmon()' function

    en <- max(.dates) #extract the max date used in the forecast

    # Extract Source and Training Data
    ds <- data.frame(observed = dn, date = .dates)

    #Extract the Fitted Values (need to figure out how to grab confidence intervals)
    dfit <- as.data.frame(fcast$fitted)
    dfit$date <- .dates
    names(dfit)[1] <- 'fitted'

    ds <- merge(ds, dfit, all.x = T)

    dfcastn <- as.data.frame(fcast)
    dfcastn$date <- .periods_to_add
    names(dfcastn) <-c('forecast','lo80','hi80','lo95','hi95','date')

    pd <- ds %>% tbl_df() %>%
      full_join(
        dfcastn %>% tbl_df(), by = "date"
      )
    return(pd)
  }

  output$series_plot <- renderPlot({
    req(forecast_data())
    pd <- funggcast(dn = prepared_data()$data$value,
                    fcast = forecast_data(),
                    .dates = prepared_data()$data$date,
                    .periods_to_add = periods_to_add())

    forecast_part <- pd %>% tail(3) %>%
      melt(c("date", "observed", "fitted")) %>%
      tbl_df() %>%
      mutate(value = ifelse(!is.na(observed), observed, value))

    ggplot() +
      geom_line(data = pd, aes(x = date, y = observed), colour = "black", size = 2) +
      geom_line(data = pd, aes(x = date, y = fitted), colour = "blue", size = 1, linetype = "dashed") +
      geom_ribbon(data = forecast_part %>% filter(grepl("80", variable)) %>%
                    dcast(date + observed + fitted ~ variable, value.var = "value"),
                  aes(x = date, ymin = lo80, ymax = hi80),
                  fill = "blue", colour = "black",
                  alpha = 0.6) +
      geom_ribbon(data = forecast_part %>% filter(grepl("95", variable)) %>%
                    dcast(date + observed + fitted ~ variable, value.var = "value"),
                  aes(x = date, ymin = lo95, ymax = hi95),
                  alpha = 0.2,
                  fill = "blue", colour = "black") +
      ggtitle(paste(input$tag_selection, input$series_selection, input$series_id_selection, sep = " - ")) +
      ylab(series(input$tag_selection) %>%
             filter(id == input$series_id_selection) %>% { .$units }
      ) +
      scale_y_continuous(breaks = pretty_breaks(6)) +
      scale_x_datetime(breaks = pretty_breaks(20)) +
      xlab("") +
      theme_bw(base_size = 18) +
      theme(panel.grid.major = element_line(colour = "black", linetype = "dashed"))
  })

  output$tabular_forecast_data <- DT::renderDataTable({
    req(forecast_data())
    forecast_data() %>%
      tbl_df() %>%
      mutate(Date = periods_to_add()) %>%
      arrange(desc(Date)) %>%
      mutate(Date = format(Date, "%Y-%m-%d")) %>%
      melt("Date") %>%
      tbl_df() %>% mutate(value = round(value, 2)) %>%
      dcast(Date ~ variable, value.var = "value") %>%
      tbl_df() %>%
      arrange(desc(Date)) %>%
      select(Date,
             `Lower 95% PI` = `Lo 95`, `L80%` = `Lo 80`,
             `U85%` = `Hi 80`,
             `U95%` = `Hi 95`)
  })

  output$tabular_actual_v_fitted_data <- DT::renderDataTable({
    req(apply_model())
    apply_model() %>%
      mutate(date = format(date, "%Y-%m-%d"),
             fitted = round(fitted, 2)) %>%
      arrange(desc(date)) %>%
      select(Date = date, Actual = actual, Fitted = fitted)
  })
})
