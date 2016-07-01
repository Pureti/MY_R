plotlyCandleStick <- function(symbol = "MSFT",   ###----https://github.com/royr2/StockPriceAnalytics  WEB APP
                              fillcolor = "#ff6666",
                              hollowcolor = "#39ac73",
                              linewidth = 4,
                              plotcolor = "#3E3E3E",
                              papercolor = "#1E2022",
                              fontcolor = "#B3A78C",
                              startdate = "2007-01-01"){
  
  # Get OHLC prices using quantmod
  prices <- getSymbols(symbol, auto.assign = F)
  prices <- prices[index(prices) >= startdate]
  
  # Convert to dataframe
  prices <- data.frame(time = index(prices),
                       open = as.numeric(prices[,1]),
                       high = as.numeric(prices[,2]),
                       low = as.numeric(prices[,3]),
                       close = as.numeric(prices[,4]),
                       volume = as.numeric(prices[,5]))
  
  # Create line segments for high and low prices
  plot.base <- data.frame()
  plot.hollow <- data.frame()
  plot.filled <- data.frame()
  
  for(i in 1:nrow(prices)){
    x <- prices[i, ]
    
    # For high / low
    mat <- rbind(c(x[1], x[3]), 
                 c(x[1], x[4]),
                 c(NA, NA))
    
    plot.base <- rbind(plot.base, mat)
    
    # For open / close
    if(x[2] > x[5]){
      mat <- rbind(c(x[1], x[2]), 
                   c(x[1], x[5]),
                   c(NA, NA))
      
      plot.filled <- rbind(plot.filled, mat)
    }else{
      mat <- rbind(c(x[1], x[2]), 
                   c(x[1], x[5]),
                   c(NA, NA))
      
      plot.hollow <- rbind(plot.hollow, mat)
    }
  }  
  
  colnames(plot.base) <- colnames(plot.hollow) <- colnames(plot.filled) <- c("x", "y")
  plot.base$x <- as.Date(as.numeric(plot.base$x))
  plot.hollow$x <- as.Date(as.numeric(plot.hollow$x))
  plot.filled$x <- as.Date(as.numeric(plot.filled$x))
  
  hovertxt <- paste("Date: ", round(prices$time,2), "<br>",
                    "High: ", round(prices$high,2),"<br>",
                    "Low: ", round(prices$low,2),"<br>",
                    "Open: ", round(prices$open,2),"<br>",
                    "Close: ", round(prices$close,2))
  
  
  # Base plot for High / Low prices
  p <- plot_ly(plot.base, x = x, y = y, mode = "lines", 
               marker = list(color = '#9b9797'),
               line = list(width = 1),
               showlegend = F,
               hoverinfo = "none")
  
  # Trace for when open price > close price
  p <- add_trace(p, data = plot.filled, x = x, y = y, mode = "lines", 
                 marker = list(color = fillcolor),
                 line = list(width = linewidth),
                 showlegend = F,
                 hoverinfo = "none")
  
  # Trace for when open price < close price
  p <- add_trace(p, data = plot.hollow, x = x, y = y, mode = "lines", 
                 marker = list(color = hollowcolor),
                 line = list(width = linewidth),
                 showlegend = F,
                 hoverinfo = "none")
  
  # Trace for volume
  p <- add_trace(p, data = prices, x = time, y = volume/1e6, type = "bar",
                 marker = list(color = "#ff9933"),
                 showlegend = F,
                 hoverinfo = "x+y",
                 yaxis = "y2")
  
  # Trace for hover info
  p <- add_trace(p, data = prices, x = time, y = high, opacity = 0, hoverinfo = "text",
                 text = hovertxt, showlegend = F)
  
  # Layout options
  p <- layout(p, xaxis = list(title = "", showgrid = F, 
                              tickformat = "%b-%Y", 
                              tickfont = list(color = fontcolor),
                              rangeselector = list(
                                x = 0.85, y = 0.97, bgcolor = "fontcolor",
                                buttons = list(
                                  list(
                                    count = 3, 
                                    label = "3 mo", 
                                    step = "month",
                                    stepmode = "backward"),
                                  list(
                                    count = 6, 
                                    label = "6 mo", 
                                    step = "month",
                                    stepmode = "backward"),
                                  list(
                                    count = 1, 
                                    label = "1 yr", 
                                    step = "year",
                                    stepmode = "backward"),
                                  list(
                                    count = 1, 
                                    label = "YTD", 
                                    step = "year",
                                    stepmode = "todate"),
                                  list(step = "all")))),
              
              yaxis = list(title = "Price", gridcolor = "#8c8c8c",
                           tickfont = list(color = fontcolor), 
                           titlefont = list(color = fontcolor),
                           domain = c(0.30, 0.95)),
              
              yaxis2 = list(gridcolor = "#8c8c8c",
                            tickfont = list(color = fontcolor), 
                            titlefont = list(color = fontcolor),
                            side = "right", 
                            domain = c(0, 0.2)),
              
              paper_bgcolor = papercolor,
              plot_bgcolor = plotcolor,
              margin = list(r = 50, t = 50),
              
              annotations = list(
                list(x = 0.02, y = 0.25, text = "Volume(mil)", ax = 0, ay = 0, align = "left",
                     xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
                     font = list(size = 20, color = fontcolor)),
                
                list(x = 0, y = 1, text = symbol, ax = 0, ay = 0, align = "left",
                     xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
                     font = list(size = 20, color = fontcolor)), 
                
                list(x = 0.1, y = 1, 
                     text = paste("Start: ", format(min(prices$time), "%b-%Y"),
                                  "<br>End: ", format(max(prices$time), "%b-%Y")),
                     ax = 0, ay = 0, align = "left",
                     xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
                     font = list(size = 10, color = fontcolor))
              ))
  
  return(p)
}
