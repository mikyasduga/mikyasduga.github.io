## Helper functions for graphics

Maizgreen <- "#555C4EFF"
Maizgray <- "#A5A79EFF"
Maizltgray <- "#D2D7D1FF"


nord_silver <- c("#4B644BFF",
                 "#647D4BFF",
                 "#E1E1E1FF",
                 "#7D96AFFF",
                 "#647D96FF")

near_white_bg <- "#f2f2f2"

img_dir <- paste0(getwd(), "/img")



## gets rid of legend and axis labels (x). 
custom_style2 <- function() {
  font <- "Montserrat"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=16, ## change size to 16 later. 
                                       face="bold",
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=15,
                                          margin=ggplot2::margin(9,0,9,0)),
    # plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "none",
    # legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    # legend.title = ggplot2::element_blank(),
    # legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=14,
                                        color="#222222", 
                                        hjust = 0),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    # axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=14,
                                      color="#222222"),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0, 
                                       family = font), 
    
    plot.caption = element_text(family = font, 
                                size = 13), 
    plot.title.position = "plot"
  )
}


prepbar <- function(df, v,...) {
  
  
  my_df <- df %>% 
    filter(!is.na(val)) %>% 
    select(val, frq, valid.prc)
  
  
  names(my_df) <- c("z", "ct", "pct")
  
  
  my_df$ypos <- cumsum(rev(my_df$ct)) - rev(my_df$ct)/2
  
  my_df$z <- sjlabelled::as_factor(my_df$z)
  
  # my_df$z.cat <- cnvt_to_labs(my_df$z)
  
  my_df<<-my_df
  
}




## custom style 2 with major panel grids added for y axis. 
custom_style3 <- function() {
  font <- "Montserrat"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=16,
                                       face="bold",
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=15,
                                          margin=ggplot2::margin(9,0,9,0)),
    # plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "none",
    #legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    # legend.title = ggplot2::element_blank(),
    # legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=14,
                                        color="#222222", 
                                        hjust = 0),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    # axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=14,
                                      color="#222222"),
    # axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    # panel.grid.minor = ggplot2::element_blank(),
    # # panel.grid.major.y = ggplot2::element_blank(),
    # panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0, 
                                       family = font), 
    
    plot.caption = element_text(family = font, 
                                size = 13), 
    plot.title.position = "plot"
  )
}



add_pct_sign <- function(x){
  
  scales::percent(x, 
                  scale = 1)
  
  
}



readable_values <- function(x){  ##remove underscore and replace with spaces. 
  x <- str_replace_all(x, "_", " ")
  
}


remove_period <- function(x) {
  x <- str_remove_all(x, "\\.")
}


thm <- hc_theme(
  chart = list(
    backgroundColor = "#f4fcfe", 
    style = list(
      fontFamily = "Montserrat"
    )
  ), 
  title = list(align = 'left', 
               style = list(fontweight = "bold")), 
  subtitle = list(align = 'left')
)


hc_theme_cstm <- function (...) {  ## A custom theme adopted from 538
  theme <- list(
    colors = c("#FF2700", "#008FD5", "#77AB43", "#636464", "#C4C4C4"),
    chart = list(
      backgroundColor = "#F0F0F0",
      plotBorderColor = "#606063",
      style = list(fontFamily = "Montserrat", color = "#3C3C3C")
    ),
    title = list(align = "left", style = list(fontWeight = "bold")),
    subtitle = list(align = "left"),
    caption = list(align = "right"),
    xAxis = list(
      gridLineWidth = 1,
      gridLineColor = "#D7D7D8",
      labels = list(style = list(
        fontFamily = "Montserrat", color = "#3C3C3C"
      )),
      lineColor = "#D7D7D8",
      minorGridLineColor = "#505053",
      tickColor = "#D7D7D8",
      tickWidth = 1,
      title = list(style = list(color = "#A0A0A3"))
    ),
    yAxis = list(
      gridLineColor = "#D7D7D8",
      labels = list(style = list(
        fontFamily = "Montserrat", color = "#3C3C3C"
      )),
      lineColor = "#D7D7D8",
      minorGridLineColor = "#505053",
      tickColor = "#D7D7D8",
      tickWidth = 1,
      title = list(style = list(color = "#A0A0A3"))
    ),
    tooltip = list(
      backgroundColor = "rgba(0, 0, 0, 0.85)",
      style = list(color = "#F0F0F0")
    ),
    legend = list(
      itemStyle = list(color = "#3C3C3C"),
      itemHiddenStyle = list(color = "#606063")
    ),
    credits = list(style = list(color = "black")),
    labels = list(style = list(color = "#D7D7D8")),
    legendBackgroundColor = "rgba(0, 0, 0, 0.5)",
    background2 = "#505053",
    dataLabelsColor = "#B0B0B3",
    textColor = "#C0C0C0",
    contrastTextColor = "#F0F0F3",
    maskColor = "rgba(255,255,255,0.3)"
  )
  theme <- structure(theme, class = "hc_theme")
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(theme, hc_theme(...))
  }
  theme
}


