#' Mustang News web theme
#'
#' @description Format ggplot2 graphs in the proper style for web stories.
#'
#' @import ggplot2
#'
#' @export
web_theme <- function(){
  theme(axis.ticks = element_blank(),
        plot.title = 	element_text(size="18",
                                 family="Georgia",
                                 color="#081D37",
                                 face = "bold"),
      plot.caption = element_text(size = 8),
      text = element_text(size = 10, family="Source Sans Pro", color="#081D37"),
      panel.background = element_blank(),
      axis.line = element_line(colour = "#081D37"),
      axis.text = element_text(colour="#081D37", size = 8),
      axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
}

#' Mustang News print theme
#'
#' @description Format ggplot2 graphs in the proper style for print stories
#'
#' @import ggplot2
#'
#' @export
print_theme <- function(){
  theme(axis.ticks = element_blank(),
        plot.title = 	element_text(size = "18",
                                   family = "Soleil",
                                   color = "#081D37",
                                   face = "bold"),
        plot.caption = element_text(size = 8),
        text = element_text(size = 10, family = "Soleil", color = "#081D37"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "#081D37"),
        axis.text = element_text(colour = "#081D37", size = 8),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
}

#' Choose colors for graphs
#'
#' @description Add this function on to an existing line or bar graph
#' to change the colors of the graph
#'
#' @param palette A string indicating which color palette to see.
#' Options are "cp" for the Cal Poly palette or "mn" for the
#' Mustang News palette
#' @param graph A string indicating which type of graph you made.
#' Options are "line" or "bar"
#' @param ... The colors that you want to use, unquoted, separated
#' by commas. There should be one color for each group in variable you
#' want to color by.
#'
#' @import ggplot2
#'
#' @export
set_colors <- function(palette, graph, ...){

  if(palette != "mn" & palette != "cp"){
    stop("Please choose valid color palette")
  }
  if(graph != "line" & graph != "bar"){
    stop("Please choose valid graph type")
  }

  if(palette == "mn"){
    cols <- c(
      teal = "#3cb181",
      navy  = "#022049",
      light_blue = "#19859b",
      orange = "#ef5f2a",
      yellow  = "#e5d42d",
      light_green = "#4cb154",
      red = "#e72f35",
      black = "#2c2d2c",
      cream = "#f5f1c2"
    )
  }
  if(palette == "cp"){
    cols <- c(
      cp_green = "#154734",
      cp_gold = "#BD8B13",
      poly_canyon = "#F2C75C",
      farmers_market = "#3A913F",
      surf_blue = "#5CB8B2",
      morro_blue = "#ABCAE9",
      pismo_sand = "#CAC7A7",
      sycamore = "#789F90",
      seal_gray = "#54585A"
    )
  }
  cols_hex <- paletti::get_hex(cols)

  if(graph == "line"){
    scale_color_manual(values = cols_hex(...))
  }
  if(graph == "bar"){
    scale_fill_manual(values = cols_hex(...))
  }
}

#' See colors in the Cal Poly and Mustang News color palettes
#'
#' @param palette A string indicating which color palette to see.
#' Options are "cp" for the Cal Poly palette or "mn" for the
#' Mustang News palette
#'
#' @return An image of all the colors in the palette
#'
#' @source \url{https://universitymarketing.calpoly.edu/brand-guidelines/colors/}
#'
#' @export
see_colors <- function(palette){
  if(palette != "mn" & palette != "cp"){
    stop("Please choose valid color palette")
  }

  if(palette == "mn"){
    mn_cols <- c(
      teal = "#3cb181",
      navy  = "#022049",
      light_blue = "#19859b",
      orange = "#ef5f2a",
      yellow  = "#e5d42d",
      light_green = "#4cb154",
      red = "#e72f35",
      black = "#2c2d2c",
      cream = "#f5f1c2"
    )
    pal <- paletti::viz_palette(mn_cols, ttl = "Mustang News Color Palette
  teal, navy, light_blue, orange, yellow, light_green, red, black, cream
  ")
  }

  if(palette == "cp"){
    cp_cols <- c(
      cp_green = "#154734",
      cp_gold = "#BD8B13",
      poly_canyon = "#F2C75C",
      farmers_market = "#3A913F",
      surf_blue = "#5CB8B2",
      morro_blue = "#ABCAE9",
      pismo_sand = "#CAC7A7",
      sycamore = "#789F90",
      seal_gray = "#54585A"
    )
    pal <- paletti::viz_palette(cp_cols, ttl = "Cal Poly Color Palette
  cp_green, cp_gold, poly_canyon, farmers_market, surf_blue, morro_blue, pismo_sand, sycamore, seal_gray
  ")
  }
  return(pal)
}



#' Create a simple line graph
#'
#' @param data A data frame with 2-4 columns. Data should be formatted in
#' the following way: column 1 is the x-axis variable, column 2 is the
#' y-axis variable, column 3 is the coloring variable, column 4 is the
#' variable split the data set on to create side by side graphs
#' @param title A string containing the title of the graph
#' @param subtitle A string containing the subtitle of the graph. Default
#' is no subtitle
#' @param caption string containing the caption of the graph. Default
#' is no caption
#' @param grid Shows major grid lines, default is FALSE
#'
#' @return A ggplot graph
#'
#' @import ggplot2
#'
#' @export
simple_line <- function(data, title, subtitle = NULL, caption = NULL,
                        grid = FALSE){
  labels <- names(data)
  if(length(data) == 2){
    names(data) <- c("x", "y")
    data <- data %>%
      mutate(group = 1)
    plot <- ggplot(data = data, aes(x = x, y = y,
                                    color = "black", group = group)) +
      geom_line() +
      labs(x = labels[1],
           y = labels[2],
           title = title,
           subtitle = subtitle,
           caption = caption) +
      theme(legend.position = "none")
    data <- data %>%
      select(-group)
    }
  if(length(data) == 3){
    names(data) <- c("x", "y", "color")
    plot <- ggplot(data = data, aes(x = x, y = y, color = color,
                                    group = color)) +
      geom_line(aes(linetype = color)) +
      labs(x = labels[1],
           y = labels[2],
           color = labels[3],
           linetype = labels[3],
           title = title,
           subtitle = subtitle,
           caption = caption)
    }
  if(length(data) == 4){
    names(data) <- c("x", "y", "color", "window")
    plot <- ggplot(data = data, aes(x = x, y = y, color = color,
                                    group = color)) +
      geom_line(aes(linetype = color)) +
      facet_grid(cols = vars(window)) +
      labs(x = labels[1],
           y = labels[2],
           color = labels[3],
           linetype = labels[3],
           title = title,
           subtitle = subtitle,
           caption = caption)
  }

  if(grid){
    plot <- plot +
      theme(panel.grid.major = element_line(color = "light grey",
                                            linetype = 3,
                                            size = 0.5))
  }

  return(plot)
}

#' Create a simple bar graph
#'
#' @param data A data frame with 2-4 columns. Data should be formatted in
#' the following way: column 1 is the x-axis variable, column 2 is the
#' y-axis variable, column 3 is the coloring variable, column 4 is the
#' variable split the data set on to create side by side graphs
#' @param title A string containing the title of the graph
#' @param subtitle A string containing the subtitle of the graph. Default
#' is no subtitle
#' @param caption string containing the caption of the graph. Default
#' is no caption
#' @param stacked Stacks bars on top of each other instead of putting them
#' next to each other. Only works for data sets of 3 or 4 variables.
#' Default is FALSE
#' @param percent Converts stacked bars to percentages so they are all
#' the same height. Default is FALSE.
#' @param grid Shows major grid lines, default is FALSE
#'
#' @return A ggplot graph
#'
#' @import ggplot2
#'
#' @export
simple_bar <- function(data, title, subtitle = NULL, caption = NULL,
                       stacked = FALSE, percent = FALSE, grid = FALSE){
  position <- "dodge"
  if(stacked){
    position <- "stack"
  }
  if(percent){
    position <- "fill"
  }
  labels <- names(data)
  if(length(data) == 2){
    names(data) <- c("x", "y")
    plot <- ggplot(data = data, aes(x = x, y = y, fill = x)) +
      geom_bar(stat = "identity") +
      labs(x = labels[1],
           y = labels[2],
           title = title,
           subtitle = subtitle,
           caption = caption) +
      theme(legend.position = "none")
  }
  if(length(data) == 3){
    names(data) <- c("x", "y", "color")
    plot <- ggplot(data = data, aes(x = x, y = y, fill = color)) +
      geom_bar(stat = "identity", position = position) +
      labs(x = labels[1],
           y = labels[2],
           fill = labels[3],
           title = title,
           subtitle = subtitle,
           caption = caption)
  }
  if(length(data) == 4){
    names(data) <- c("x", "y", "color", "window")
    plot <- ggplot(data = data, aes(x = x, y = y, fill = color)) +
      geom_bar(stat = "identity", position = position) +
      facet_grid(cols = vars(window)) +
      labs(x = labels[1],
           y = labels[2],
           fill = labels[3],
           title = title,
           subtitle = subtitle,
           caption = caption)
  }

  if(grid){
    plot <- plot +
      theme(panel.grid.major = element_line(color = "light grey",
                                            linetype = 3,
                                            size = 0.5))
  }

  return(plot)
}




