

output$dashboard = renderUI({
  argonTabSet(
    id = "analysisSettingsTabs",
    card_wrapper = T,
    horizontal = TRUE,
    circle = F,
    size = "sm",
    width = 12,
    iconList = list(
      icon("home"),
      icon("tachometer-alt"), 
      icon("laptop-code")
    ),
    argonTab(
      tabName = "Home",
      active = F,
      argonRow(
        argonColumn(
          width = 4,
          img(src = 'home.jpg',width = "80%"),
          h6("Source: The World Bank",style = 'text-align:center;
             font-style: italic;font-weight: bold;
             ')
          ),
        argonColumn(
          width = 5,
          p("'World Development Indicators' is the World Bank's premier annual compilation of data about development. 
            This indispensable statistical reference allows you to consult over 800 indicators for more than 150 economies and 14 
            country groups in more than 90 tables",style = 'text-align:justify;'),
          p(" It provides a current overview of the most recent data available as well as important regional data and income group analysis 
            in six thematic sections: World View, People, Environment, Economy, States and Markets, and Global Links. 'World Development Indicators 
            2012' presents the most current and accurate development data on both a national level and aggregated globally",style = 'text-align:justify;'),
          tags$br(),
          p("This dashboard has 2 tabs: Dashboard and Comparison. Dashboard allows user to view a high level picture of key indicators. 
        User can also click on any country in the map to view the numbers in that country. In Comparison tab user can compare indicators across countries and
        time period.",
            style = 'color:blue;text-align:justify;')
        ),
        argonColumn(
          width = 3,
          img(src = 'world.gif',width = "100%",height = "80%"),
          h6("Source: The World Bank",style = 'text-align:center;font-style: italic;font-weight: bold;')
        )
        
          ),
      tags$br(),
      h4("Important Note:",style = 'color:Red;font-size:15px;text-align:Left;'),
      p("1. The data used in this dashboard taken from the World Bank website.",
        style = 'color:Red;font-size:13px;text-align:Left;'),
      p(paste0("3. Last View: ",lastUpdate),style = 'color:Red;font-size:13px;text-align:Left;')
      
      
    ),
    # analysis setting tab -----
    argonTab(
      tabName = "Dashboard",
      active = F,
      tags$head(tags$style(type = "text/css", "
             #loadmessage {
                           position: fixed;
                           top: 150px;
                           left: 50px;
                           width: 93%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #CCFF66;
                           z-index: 105;
}
  ")),
      argonRow(
        argonColumn(
          width = 12,
          uiOutput("chartUI") %>% withSpinner()
        )
      ),
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                       tags$div("Loading Page!!! Please wait...",id = "loadmessage")),
      argonRow(
        argonColumn(
          width = 12,
          dataTableOutput("dataTableCountryWise") %>% withSpinner()
          
        )
      )
    ),
  argonTab(
    tabName = "Comparision",
    active = T,
    uiOutput("countryComparisionChartsUI") %>% withSpinner(),
    tags$hr(),
    argonRow(
      argonColumn(
        width = 12,
        dataTableOutput("dataTableCountryCompare") %>% withSpinner()
        
      )
    )
  )
  )
})
outputOptions(output, "dashboard", suspendWhenHidden = FALSE)

output$chartUI = renderUI({
  tagList(
  argonRow(
    argonColumn(
      width = 6,
      argonRow(
        argonColumn(
          width = 3,
          tags$strong("Economic Indicators")
        ),
        argonColumn(
          width = 1,
          dropdownButton(
            tagList(
              prettyRadioButtons(
                inputId = "aggregatePlotOptions",
                label = NULL,
                choices = setNames(c(1:6),c("GDP",
                                            "GDP Per Capita",
                                            "Mobile Phone Usage",
                                            "Internet Usage",
                                            "Energy Usage",
                                            "CO2 Emission"
                                            )
                                   ),
                selected = "1",
                shape = c("round"),
                outline = T,
                fill = T,
                width = "100%"
              )
            ),
            status = "primary",
            size = "sm",
            circle = T,
            icon = icon("wrench"),
            right = T,
            margin = "10px",
            inputId = "PlotOptionsDropdown"
          ),
          bsPopover("PlotOptionsDropdown", title = NULL, content = "Click to specify the type of plot", placement = "left", trigger = "hover",
                    options = NULL)
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          div(
            id = "GDPDiv",
            highchartOutput("GDPDivPlot",width = "100%") %>% withSpinner()
          ),
          div(
            id = "GDPpercapitaDiv",
            highchartOutput("GDPpercapitaDivplot",width = "100%") %>% withSpinner()
          ),
          div(
            id = "phoneUsageDiv",
            highchartOutput("phoneUsageDivplot",width = "100%") %>% withSpinner()
          ),
          div(
            id = "internetUsageDiv",
            highchartOutput("internetUsageDivplot",width = "100%") %>% withSpinner()
          ),
          div(
            id = "energyUsageDiv",
            highchartOutput("energyUsageDivplot",width = "100%") %>% withSpinner()
          ),
          div(
            id = "carbonEmissionDiv",
            highchartOutput("carbonEmissionDivplot",width = "100%") %>% withSpinner()
          )
        )
      )
    ),
    argonColumn(
      width = 6,
      argonRow(
        argonColumn(
          width = 8,
          tags$strong("Health Indicators- Click on a country to view country specific results")
        ),
        argonColumn(
          width = 1,
          dropdownButton(
            tagList(
              prettyRadioButtons(
                inputId = "highchartOption",
                label = NULL,
                choices = setNames(c(1:4),c("Health Exp to GDP","Health Exp per Capita","Infant Mortality Rate","Life Expectancy")),
                selected = "1",
                shape = c("round"),
                outline = T,
                fill = T,
                width = "100%"
              )
            ),
            status = "primary",
            size = "sm",
            circle = T,
            icon = icon("wrench"),
            right = T,
            margin = "10px",
            inputId = "worldMapOption"
          ),
          bsPopover("worldMapOption", title = NULL, content = "Click to specify the outcome", placement = "left", trigger = "hover",
                    options = NULL)
        )
      ),
      highchartOutput("worldMap",width = "100%") %>% withSpinner()
    )
  ),
  tags$hr(),
  argonRow(

  )
  )

})

output$worldMap <- renderHighchart({
  req(!is.null(data))
  canvasClickFunction <- JS("function(event) {Shiny.setInputValue('canvasClicked', [event.point.name]);}")
  x = input$highchartOption %>% as.numeric()
  value = switch(x,"HEALTH_EXP_GDP","HEALTH_EXP_PER_CAPITA","INFANT_MORTALITY_RATE","LIFE_EXPECTANCY")
  colnames(data)[1] = "name"
  highchart(type = "map",width = "100%",height = "100%") %>%
    hc_add_series_map(map = worldgeojson, df = data, value = value, joinBy = "name") %>%
    hc_colorAxis(stops = color_stops(5)) %>%
    hc_tooltip(useHTML = TRUE,
               headerFormat = '',
               pointFormat = paste0('{point.name}: {point.',value,'} ')) %>%
    hc_exporting(enabled = TRUE,filename = value) %>% 
    hc_add_theme(hc_theme_ffx()) %>%
    hc_chart(zoomType = "xy") %>%
    hc_mapNavigation(enabled = TRUE) %>%
    hc_plotOptions(series = list( 
      events = list(click = canvasClickFunction),allowPointSelect = T))
})

output$GDPpercapitaDivplot = renderHighchart({
  x = data %>%
    filter(YEAR == 2012) %>%
    select(COUNTRY, GDP_PER_CAPITA) %>%
    arrange(-GDP_PER_CAPITA) %>%
    top_n(n = 25,wt = GDP_PER_CAPITA)
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "GDP_PER_CAPITA in USD (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$COUNTRY) %>%
    hc_yAxis(title = list(text = "USD")) %>%
    hc_add_series(name = "USD:",
                  data = x$GDP_PER_CAPITA,
                  showInLegend = F) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$GDPDivPlot = renderHighchart({
  x = data %>%
    filter(YEAR == 2012) %>%
    select(COUNTRY, GDP) %>%
    arrange(-GDP) %>%
    top_n(n = 25,wt = GDP)
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "GDP in USD (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$COUNTRY) %>%
    hc_yAxis(title = list(text = "USD")) %>%
    hc_add_series(name = "USD:",
                  data = x$GDP,
                  showInLegend = F) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$phoneUsageDivplot = renderHighchart({
  x = data %>%
    filter(YEAR == 2012) %>%
    select(COUNTRY, MOBILE_PHONE_USAGE) %>%
    arrange(-MOBILE_PHONE_USAGE) %>%
    top_n(n = 25,wt = MOBILE_PHONE_USAGE)
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "Mobile Phone Usage (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$COUNTRY) %>%
    hc_yAxis(title = list(text = "Usage %")) %>%
    hc_add_series(name = "Usage %:",
                  data = x$MOBILE_PHONE_USAGE,
                  showInLegend = F) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})


output$internetUsageDivplot = renderHighchart({
  x = data %>%
    filter(YEAR == 2012) %>%
    select(COUNTRY, INTERNET_USAGE) %>%
    arrange(-INTERNET_USAGE) %>%
    top_n(n = 25,wt = INTERNET_USAGE)
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "Internet Usage (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$COUNTRY) %>%
    hc_yAxis(title = list(text = "Usage %")) %>%
    hc_add_series(name = "Usage %:",
                  data = x$INTERNET_USAGE,
                  showInLegend = F) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})
output$energyUsageDivplot = renderHighchart({
  x = data %>%
    filter(YEAR == 2012) %>%
    select(COUNTRY, ENERGY_USAGE) %>%
    arrange(-ENERGY_USAGE) %>%
    top_n(n = 25,wt = ENERGY_USAGE)
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "Energy Usage (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$COUNTRY) %>%
    hc_yAxis(title = list(text = "Usage MWh")) %>%
    hc_add_series(name = "Usage MWh:",
                  data = x$ENERGY_USAGE,
                  showInLegend = F) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$carbonEmissionDivplot = renderHighchart({
  x = data %>%
    filter(YEAR == 2010) %>%
    select(COUNTRY, CO2_EMISSION) %>%
    arrange(-CO2_EMISSION) %>%
    top_n(n = 25,wt = CO2_EMISSION)
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "CO2 Emission (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$COUNTRY) %>%
    hc_yAxis(title = list(text = "Usage thousand MT")) %>%
    hc_add_series(name = "Emission thousand MT:",
                  data = x$CO2_EMISSION,
                  showInLegend = F) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$dataTableCountryWise = renderDataTable({
  req(!is.null(data))
  x = data %>%
        filter(YEAR == 2012) %>%
        select(-CO2_EMISSION) %>%
        cbind(data %>% filter(YEAR == 2010) %>% select(CO2_EMISSION)) %>%
        arrange(desc(GDP)) %>%
    select(Country = COUNTRY,  "Birth Rate (%)" = BIRTH_RATE, "CO2 Emission (000 MT)" = CO2_EMISSION, "Energy Usage (GWh)" = ENERGY_USAGE, 
           "GDP (USD)" = GDP,"GDP per Capita (USD)" = GDP_PER_CAPITA, "Health Exp to GDP (%)" = HEALTH_EXP_GDP ,"Infant Mortality Rate (%)" = INFANT_MORTALITY_RATE,
           "Life Expectancy" = LIFE_EXPECTANCY, Population = POPULATION_TOTAL, "Internet Usage (%)" = INTERNET_USAGE, "Mobile Usage (%)" = MOBILE_PHONE_USAGE)

  datatable(x,
            extensions = 'Buttons',
            rownames = FALSE,
            filter = 'top',
            options = list(
              searchHighlight = TRUE,
              pageLength = 25,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons =
                list(
                  list(
                    extend = 'collection',
                    buttons = c('csv', 'pdf'),
                    text = 'Download'
                  )
                )
            )
  ) %>%
  formatPercentage('Birth Rate (%)',2) %>%
  formatPercentage('Health Exp to GDP (%)',2) %>%
  formatPercentage('Infant Mortality Rate (%)',2) %>%
  formatPercentage('Internet Usage (%)',2) %>%
  formatPercentage('Mobile Usage (%)',2) %>%
  formatCurrency('GDP (USD)', digits = 0) %>%
  formatCurrency('GDP per Capita (USD)', digits = 0) %>% 
  formatCurrency("CO2 Emission (000 MT)", currency = "", digits = 0) %>%
  formatCurrency("Energy Usage (GWh)", currency = "", digits = 0) %>%
  formatCurrency("Population", currency = "")
})

#### Country Specific tab

countrySpecificModal <- function(){
  COUNTRY = input$canvasClicked

  healthExpToGDP = data %>%
                                filter(COUNTRY == input$canvasClicked) %>%
                                .["HEALTH_EXP_GDP"]

  healthExpPerCapita = data %>%
                                filter(COUNTRY == input$canvasClicked) %>%
                                .["HEALTH_EXP_PER_CAPITA"]

  infantMortality = data %>%
                                  filter(COUNTRY == input$canvasClicked) %>%
                                  .["INFANT_MORTALITY_RATE"]

  LifeExpe = data %>%
                              filter(COUNTRY == input$canvasClicked) %>%
                              .["LIFE_EXPECTANCY"]
  modalDialog(
  tagList(
    argonRow(
      argonColumn(
        center = T,
        width = 12,
        h1(COUNTRY)
      )
    ),
    tags$hr(),
    argonRow(
    argonColumn(
      width = 12,
      argonRow(
        argonColumn(
          width = 5,
          # tags$strong("Country specific plots"),
          numericRangeInput( inputId = "dateRange", 
                         label = NULL,
                         value = c(2000, 2012),
                         width = "100%"
                         )
        ),
        argonColumn(
          width = 1,
          offset = 1,
          dropdownButton(
            tagList(
              prettyRadioButtons(
                inputId = "countryPlotOptions",
                label = NULL,
                choices = setNames(c(1:4),c("Health Exp to GDP (%)",
                                            paste0("Health Exp per Capita"),
                                            paste0("Infant Mortality Rate"),
                                            paste0("Life Expectancy")
                )
                ),
                selected = "1",
                shape = c("round"),
                outline = T,
                fill = T,
                width = "100%"
              )
            ),
            status = "primary",
            size = "sm",
            circle = T,
            icon = icon("wrench"),
            right = T,
            margin = "10px",
            inputId = "countryPlotOptionsDropdown"
          ),
          bsPopover("countryPlotOptionsDropdown", title = NULL, content = "Click to specify the type of plot", placement = "left", trigger = "hover",
                    options = NULL)
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          div(
            id = "healthToGDPDIV",
            highchartOutput("healthToGDPDIVplot",width = "100%") %>% withSpinner()
          ),
          hidden(
            div(
            id = "healthPerCapitaDiv",
            highchartOutput("healthPerCapitaDivplot",width = "100%") %>% withSpinner()
           )
          ),
          hidden(
            div(
            id = "infantMortalityDiv",
            highchartOutput("infantMortalityDivplot",width = "100%") %>% withSpinner()
          )
          ),
          hidden(
            div(
            id = "lifeExpDiv",
            highchartOutput("lifeExpDivplot",width = "100%") %>% withSpinner()
          )
          )
        )
      )
     )
    )
  ),
  title = NULL,
  size = "l", 
  align = "center",
  easyClose = TRUE,
  fade = T, 
  footer = NULL
)
}


output$healthToGDPDIVplot = renderHighchart({

  df = data %>% 
    select(YEAR,COUNTRY,HEALTH_EXP_GDP) %>%
    dplyr::filter(COUNTRY == input$canvasClicked)  %>% 
    filter(YEAR >= input$dateRange[1] & YEAR <= input$dateRange[2] ) %>% 
    arrange(YEAR)
  x = df
  
  hc <- highchart() %>% 
    hc_subtitle(text = "Health Expenditure as % of GDP",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$YEAR) %>%
    hc_yAxis(title = list(text = "Health Expenditure")) %>%
    hc_add_series(name = "Countries",data = x$HEALTH_EXP_GDP) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors('red') %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$healthPerCapitaDivplot = renderHighchart({
  df = data %>% 
              select(YEAR,COUNTRY,HEALTH_EXP_PER_CAPITA) %>%
              dplyr::filter(COUNTRY == input$canvasClicked)  %>% 
              filter(YEAR >= input$dateRange[1] & YEAR <= input$dateRange[2]) %>% 
              arrange(YEAR)
  x = df

  hc <- highchart() %>% 
    hc_subtitle(text = "Health Expenditure per Capita",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$YEAR) %>%
    hc_yAxis(title = list(text = "Health Expenditure")) %>%
    hc_add_series(name = "Countries",data = x$HEALTH_EXP_PER_CAPITA) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors('red') %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$infantMortalityDivplot = renderHighchart({
  df = data %>% 
    select(YEAR,COUNTRY,INFANT_MORTALITY_RATE) %>%
    dplyr::filter(COUNTRY == input$canvasClicked)  %>% 
    filter(YEAR >= input$dateRange[1] & YEAR <= input$dateRange[2]) %>% 
    arrange(YEAR)
  x = df
  
  hc <- highchart() %>% 
    hc_subtitle(text = "Infant Mortality Rate",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$YEAR) %>%
    hc_yAxis(title = list(text = "Rate")) %>%
    hc_add_series(name = "Countries",data = x$INFANT_MORTALITY_RATE) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors('red') %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$lifeExpDivplot = renderHighchart({
  df = data %>% 
    select(YEAR,COUNTRY,LIFE_EXPECTANCY) %>%
    dplyr::filter(COUNTRY == input$canvasClicked)  %>% 
    filter(YEAR >= input$dateRange[1] & YEAR <= input$dateRange[2]) %>% 
    arrange(YEAR)
  x = df
  
  hc <- highchart() %>% 
    hc_subtitle(text = "Life Expectancy",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$YEAR) %>%
    hc_yAxis(title = list(text = "Year")) %>%
    hc_add_series(name = "Countries",data = x$LIFE_EXPECTANCY) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors('red') %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})


#### Country Comparision tab ----

output$countryComparisionChartsUI = renderUI({
  tagList(
    argonRow(
      argonColumn(
        width = 12,
        selectInput(
          inputId = "countryCompareList",
          label = strong("Select all the countries you want to compare in graphs"),
          choices = data$COUNTRY %>% unique(),
          selected = c("United States", "United Kingdom", "Canada", "China", "India"),
          selectize = T,
          multiple = T,
          width = "100%"
        )
      ),
      argonColumn(
        width = 3,
        dropdownButton(
          prettyRadioButtons(
            inputId = "countryCompareTypeofPlot",
            label = NULL,
            choices = setNames(c(1:10),c("GDP",
                                        "GDP Per Capita",
                                        "Mobile Phone Usage",
                                        "Internet Usage",
                                        "Energy Usage",
                                        "CO2 Emission",
                                        "Health Exp to GDP (%)",
                                        "Health Exp per Capita",
                                        "Infant Mortality Rate",
                                        "Life Expectancy")),
            selected = "1",
            shape = c("round"),
            outline = T,
            fill = T,
            width = "100%"
          ),
          label = "Outcome to be compared:",
          status = "primary",
          circle = F,
          icon = icon("wrench"),
          right = T,
          margin = "10px",
          inputId = "countryCompareTypeofPlotDropdown"
        )
      ),
      argonColumn(
        width = 4,
        numericRangeInput( inputId = "dateRangeCompare", 
                            label = NULL,
                            value = c(2000, 2012),
                            width = "100%"
        )
      )
    ),
    tags$hr(),
    argonRow(
      argonColumn(
        width = 12,
        highchartOutput("countryCompareChart") %>% withSpinner()
      )
    )
  )
})

output$countryCompareChart = renderHighchart({
  req(!is.null(input$countryCompareList))
  countryList <- input$countryCompareList
  dateRange <- input$dateRangeCompare
  plotType <- switch(as.numeric(input$countryCompareTypeofPlot),
                    "GDP",
                    "GDP_PER_CAPITA",
                    "MOBILE_PHONE_USAGE",
                    "INTERNET_USAGE",
                    "ENERGY_USAGE",
                    "CO2_EMISSION",
                    "HEALTH_EXP_GDP" ,
                    "HEALTH_EXP_PER_CAPITA",
                    "INFANT_MORTALITY_RATE",
                    "LIFE_EXPECTANCY")

    data1 <-  data %>%
      filter(COUNTRY %in% countryList) %>%
      filter(YEAR >= dateRange[1] & YEAR <= dateRange[2] ) %>%
      select(YEAR, COUNTRY, plotType) %>% arrange(YEAR) %>%
      reshape2::dcast(YEAR~COUNTRY)
    
    new_df <-
      data1
    
    Type = 'line'
    xaxisCols = c(1)
    yAxisCols = 2:ncol(data1)
    ChartTitle = list(
      align = 'center',
      color = "red",
      fontSize = "24px" ,
      text = paste("Comparing ", plotType)
    )
    ChartSubTitle = list(
      align = 'left',
      color = "red",
      fontSize = "20px" ,
      text = '<strong></strong>'
    )
    xAxisTitle = list(
      text = "",
      align = 'middle',
      color = "red",
      fontSize = "20px"
    )
    yAxisTitle = list(
      text = "",
      align = 'middle',
      color = "red",
      fontSize = "20px"
    )
    legend = TRUE
    
    hc <-
      CreateChart(
        new_df,
        Type,
        xaxisCols,
        yAxisCols,
        ChartTitle,
        ChartSubTitle,
        yAxisTitle,
        xAxisTitle,
        NULL,
        legend
      )
    
    hc %>% 
      hc_chart(borderColor = '#EBBA95',
               borderRadius = 10,
               borderWidth = 2
      ) %>%
      hc_exporting(
        enabled = TRUE
      ) %>%
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 5,table = T)

  
})

output$dataTableCountryCompare = renderDataTable({
  req(!is.null(input$countryCompareList))
  countryList = input$countryCompareList
  x = data %>%
    filter(COUNTRY %in% countryList) %>%
    arrange(desc(GDP)) %>%
    select(Country = COUNTRY, Year = YEAR, "Birth Rate (%)" = BIRTH_RATE, "CO2 Emission (000 MT)" = CO2_EMISSION, "Energy Usage (GWh)" = ENERGY_USAGE, 
           "GDP (USD)" = GDP,"GDP per Capita (USD)" = GDP_PER_CAPITA, "Health Exp to GDP (%)" = HEALTH_EXP_GDP ,"Infant Mortality Rate (%)" = INFANT_MORTALITY_RATE,
           "Life Expectancy" = LIFE_EXPECTANCY, Population = POPULATION_TOTAL, "Internet Usage (%)" = INTERNET_USAGE, "Mobile Usage (%)" = MOBILE_PHONE_USAGE)
  datatable(x,
            extensions = 'Buttons',
            rownames = FALSE,
            filter = 'top',
            options = list(
              searchHighlight = TRUE,
              pageLength = 25,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons =
                list(
                  list(
                    extend = 'collection',
                    buttons = c('csv', 'pdf'),
                    text = 'Download'
                  )
                )
              
            )
            
  ) %>%
    formatPercentage('Birth Rate (%)',2) %>%
    formatPercentage('Health Exp to GDP (%)',2) %>%
    formatPercentage('Infant Mortality Rate (%)',2) %>%
    formatPercentage('Internet Usage (%)',2) %>%
    formatPercentage('Mobile Usage (%)',2) %>%
    formatCurrency('GDP (USD)', digits = 0) %>%
    formatCurrency('GDP per Capita (USD)', digits = 0) %>% 
    formatCurrency("CO2 Emission (000 MT)", currency = "", digits = 0) %>%
    formatCurrency("Energy Usage (GWh)", currency = "", digits = 0) %>%
    formatCurrency("Population", currency = "") 

})


observeEvent(input$aggregatePlotOptions,{
  x = as.numeric(input$aggregatePlotOptions)
  divlist = c("GDPDiv","GDPpercapitaDiv","phoneUsageDiv","internetUsageDiv","energyUsageDiv", "carbonEmissionDiv")
  hideAllBut(divlist,x)
})
observeEvent(input$countryPlotOptions,{
  x = as.numeric(input$countryPlotOptions)
  divlist = c("healthToGDPDIV","healthPerCapitaDiv",
              "infantMortalityDiv","lifeExpDiv")
  hideAllBut(divlist,x)
})


observeEvent(input$canvasClicked,{
  showModal(countrySpecificModal())
})


##### debug server logic #####
output$runRCodeOutput = renderPrint({
  req(rcode())
  isolate({
    eval(parse(text = rcode()$text))
  })
})
rcode = reactiveVal()
observeEvent(input$runRCodeButton, {
  rcode(list("text" = input$runRCode, "type" = "runRCode", "rand" = runif(1)))
}, ignoreNULL = TRUE, ignoreInit = TRUE)



