##### Global: options #####
Production = T
options(scipen = 1000, expressions = 10000)
appVersion = "v1.0"
appLongName = "World Indicators Dashboard"
lastUpdate = Sys.Date()

loader <- tagList(
  waiter::spin_loaders(42),
  br(),
  h3("Loading data")
)

jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
var element = document.documentElement,
enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
enterFS.call(element);
} else {
exitFS.call(document);
}
}'

source("appFiles/packageLoad.R")
source("appFiles/dataLoad.R")
source("appFiles/CSS.R", local = TRUE)
source("appFiles/dashboardPage.R", local = TRUE)
##### User interface #####
ui <- tagList( # dependencies
  use_waiter(),
  useSweetAlert(),
  useShinyjs(),
  extendShinyjs(text = jsToggleFS,functions =  c("foo", "bar")),
  waiter::waiter_show_on_load(loader, color = "#000"),
# shows before anything else
  ##### CSS and style functions #####
  CSS, #CSS.R
  # Loading message
  argonDash::argonDashPage(
    title = appLongName,
    header = argonDash::argonDashHeader(
      gradient = T,
      color = NULL,
      top_padding = 2,
      bottom_padding = 0,
      background_img = "bg.jpg",
      height = 70,
      argonRow(
        argonColumn(width = 8,
                    h4(appLongName, style = 'color:white;
                       text-align:left;
                       vertical-align: middle;
                       font-size:40px;')
                    ),
        argonColumn(
          width = 4,
          h6(HTML(paste0("Owner: <a href='https://www.linkedin.com/in/davidportertpc' target = '_blank'>David Porter</a>")), style = 'color:white;
                                  text-align: right;
                                  font-size:15px;
                                  margin-bottom: 0em')
        ),
        fixedPanel(
          div(
            actionBttn("fullScreen",
                       style = "material-circle",
                       icon = icon("arrows-alt"),
                       size = "xs",
                       color = "warning"),
            bsPopover("fullScreen", title = NULL, content = "Click to view in full screen", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "shinyjs.toggleFullScreen();"
          ),
          top = 55,
          right = 10
          
        ),
        fixedPanel(
          div(
            actionBttn("about",
                       style = "material-circle",
                       icon = icon("linkedin"),
                       size = "xs",
                       color = "success"),
            bsPopover("about", title = NULL, content = "Contact me", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "window.open('https://www.linkedin.com/in/davidportertpc/', '_blank')"
          ),
          top = 55,
          right = 40
          
        ),
        fixedPanel(
          div(
            actionBttn("userGuide",
                       style = "material-circle",
                       icon = icon("info"),
                       size = "xs",
                       color = "royal"),
            bsPopover("userGuide", title = NULL, content = "Go to app help page", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "window.open('', '_blank')"
          ),
          top = 55,
          right = 70
          
        ),
        fixedPanel(
          div(
            actionBttn("webSite",
                       style = "material-circle",
                       icon = icon("address-card"),
                       size = "xs",
                       color = "primary"),
            bsPopover("webSite", title = NULL, content = "About developer", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "window.open('', '_blank')"
          ),
          top = 55,
          right = 100
          
        )
                    )
      
      
      ),
    sidebar = NULL,
    body = argonDashBody(
      tags$head( tags$meta(name = "viewport", content = "width=1600"),uiOutput("body")),
      tags$br(),
           dashboardUI
    )
  )
  )

##### server #####
server <- function(input, output, session) {
  printLogJs = function(x, ...) {
    logjs(x)
    T
  }
  # addHandler(printLogJs)
  if (!Production) options(shiny.error = NULL)
  options(shiny.sanitize.errors = TRUE, width = 160)
  
  session$onSessionEnded(function() {
    stopApp()
    # q("no")
  })
  source("appFiles/dashboardServer.R", local = TRUE)
  # Hide the loading message when the rest of the server function has executed
  waiter_hide() # will hide *on_load waiter
}

# Run the application
shinyApp(ui = ui, server = server)