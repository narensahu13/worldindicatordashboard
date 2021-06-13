
load("www/processed_data.Rdata") 


hideAllBut = function(divList, butNdx) {
  library("shinyjs")
  divList[-butNdx] %>% sapply(function(x) {shinyjs::hide(x)})
  shinyjs::show(divList[butNdx])
}


CreateChart <-
  function(my_df,
           Type = 'line',
           xaxisCols,
           yAxisCols,
           ChartTitle = NULL,
           ChartSubTitle = NULL,
           yAxisTitle = NULL,
           xAxisTitle = NULL,
           chartColors = NULL,
           legend = T) {
    #' Create a chart from a dataframe using highcharter package
    #' @param my_df: input dataframe
    #' @param Type: Type of chart: default to 'line'. Values can take 'line', 'scatter', 'spline'. See hchart documentation
    #' @param xaxisCols: the X-axis column index in the dataframe
    #' @param yAxisCols: vector of the Y-axis column indices
    #' @param ChartTitle: Give a list of chart title text, align, color, fontsize. See example for detail structure.
    #' @param ChartSubTitle: Give a list of chart subtitle text, align, color, fontsize. See example for detail structure.
    #' @param yAxisTitle: Give a list of Y-axis title text, align, color, fontsize. See example for detail structure.
    #' @param xAxisTitle: Give a list of X-axis title text, align, color, fontsize. See example for detail structure.
    #' @param chartColors: Give a vector of colors for the chart. If 3 columns are given in Y-axis, then 3 colors should be provided. By default, it automatically choose the colors
    #' @return A Viewer chart object
    #' @param legend: Boolean. If to include legend or not. Default is TRUE
    #' @examples
    #' new_df <-  EuStockMarkets[1:100,] %>% cbind('Date' = 1:100)  %>% as.data.frame()
    #' Type = 'line'
    #' xaxisCols = c(5)
    #' yAxisCols = c(1,2,3)
    #' ChartTitle = list(align='center', color = "red", fontSize= "24px" , text= '<strong>Chart title</strong>')
    #' ChartSubTitle = list(align='left', color = "red",  fontSize= "20px",  text= '<strong>Chart sub title</strong>')
    #' xAxisTitle = list( text= "x Axis at bottom", align= 'middle', color = "red",  fontSize= "20px")
    #' yAxisTitle = list( text= "Y Axis middle",align= 'middle',color = "red", fontSize= "20px" )
    #' chartColors = c("green", "red", 'black')
    #' legend = TRUE
    
    #' CreateChart(new_df, Type, xaxisCols, yAxisCols, ChartTitle, ChartSubTitle, yAxisTitle, xAxisTitle, chartColors, legend)
    #' @export
    #' @importFrom base paste
    #'
    
    # If more than 1 X-axis is given, stop the execution
    if (length(xaxisCols) > 1) {
      stop("Execution error. Number of columns in X-axis can be only 1.")
    }
    
    # Get column name of X-axis
    col_x <- my_df %>% .[xaxisCols] %>% colnames()
    # Process the dataframe based on X-axis and Y-axes given. (melt the dataframe)
    processed_my_df <-
      my_df[, c(xaxisCols, yAxisCols)] %>% reshape::melt(col_x)
    # Reassign the column names of the processed dataframe
    colnames(processed_my_df) <- c('x_col', 'variable', 'value')
    
    # Plot the basic highcharter plot without any customization
    fig <-
      highcharter::hchart(processed_my_df,
                          Type,
                          highcharter::hcaes(x = x_col, y = value, group = variable)) %>%
      highcharter::hc_legend(enabled = legend)
    
    # If chart color is provided, the use the colors accordingly
    if (!is.null(chartColors)) {
      fig <- fig %>% highcharter::hc_colors(chartColors)
    }
    
    if (Type == 'column') {
      fig <- fig %>%   highcharter::hc_plotOptions(column = list(
        dataLabels = list(enabled = FALSE),
        stacking = "normal",
        enableMouseTracking = TRUE
      ))
    }
    
    # If title is given, include to the chart
    if (!is.null(ChartTitle)) {
      fig <- fig %>% highcharter::hc_title(
        align = ChartTitle$align,
        color = ChartTitle$color,
        fontSize = ChartTitle$fontSize ,
        text = ChartTitle$text
      )
    }
    
    # If subtitle is given, include to the chart
    if (!is.null(ChartSubTitle)) {
      fig <- fig %>% highcharter::hc_subtitle(
        align = ChartSubTitle$align,
        color = ChartSubTitle$color,
        fontSize = ChartSubTitle$fontSize ,
        text = ChartSubTitle$text
      )
    }
    
    # If X-axis title is given, include to the chart
    if (!is.null(xAxisTitle)) {
      fig <- fig %>% highcharter::hc_xAxis(
        title = list(text = xAxisTitle$text , align = xAxisTitle$align),
        opposite = FALSE,
        style = list(
          fontSize = xAxisTitle$fontSize,
          color = xAxisTitle$color,
          fontWeight = 'bold'
        )
      )
    }
    
    # If Y-axis title is given, include to the chart
    if (!is.null(yAxisTitle)) {
      fig <- fig %>% highcharter::hc_yAxis(
        title = list(text = yAxisTitle$text, align = yAxisTitle$align),
        opposite = FALSE,
        style = list(
          fontSize = yAxisTitle$fontSize,
          color = yAxisTitle$color,
          fontWeight = 'bold'
        )
      )
    }
    
    return(fig)
  }

