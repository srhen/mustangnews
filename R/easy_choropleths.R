#' Creates a choropleth of USA by county
#'
#' @param data Your data frame to make a choropleth. Contains three columns,
#' county name, county state and your variable of intrest, in that order
#'
#' @return An interactive map, which is a HTML widget object
#'
#' @importFrom usdata abbr2state
#' @importFrom dplyr mutate arrange
#' @import sp leaflet
#'
#' @export
#'
#' @examples
#' state_map(life_exp)
#'
state_map <- function(data){
  variable_name <- names(data)[2]
  names(data) <- c("NAME", "variable")

  # Convert state abbriviation to full name
  if (nchar(data$NAME[1]) == 2){
    data <- data %>%
      mutate(state_name = abbr2state(state_name))
  }

  map_shapes <- geojsonio::geojson_read(system.file("extdata",
                                                    "us_states.json",
                                                    package = "mustangnews"),
                                        what = "sp")
  map_shapes <- merge(map_shapes, data)

  pal <- colorBin("YlOrRd", domain = map_shapes$variable)
  lab <- paste0(map_shapes@data$NAME, ": ",
                format(map_shapes@data$variable, big.mark = ","))

  map <- leaflet(data = map_shapes) %>%
    setView(-96, 37.8, 4) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal(variable),
                fillOpacity = .7,
                weight = 2,
                color = "white",
                label = lab,
                popup = lab) %>%
    addLegend(position = "bottomright",
              pal = pal,
              values = ~variable,
              title = paste("<strong>", variable_name, "</strong>"))
  return(map)
}



#' Creates a choropleth of USA by county
#'
#' @param data Your data frame to make a choropleth. Contains three columns,
#' county name, county state and your variable of intrest, in that order
#' @param state A string contianing the state name or abbriviation that
#' you would like to map. Default is "all" which maps the entire USA
#'
#' @return An interactive map, which is a HTML widget object
#'
#' @import sp leaflet dplyr
#' @importFrom usdata abbr2state
#'
#' @export
#'
#' @examples
#' county_map(median_income, state = "CA")
#'
county_map <- function(data, state = "all"){

  # Convert state abbr to full name
  if(nchar(state) == 2){
    state <- abbr2state(state)
  }
  # Save variable name for later
  variable_name <- names(data)[3]
  # Rename to make joins easier
  names(data) <- c("NAME", "state_name", "variable")
  # Remove territorial designation
  data <- data %>%
    mutate(NAME = gsub(" County", "", NAME),
           NAME = gsub(" Borough", "", NAME),
           NAME = gsub(" Census Area", "", NAME),
           NAME = gsub(" Municipality", "", NAME),
           NAME = gsub(" City and", "", NAME),
           NAME = gsub(" Parish", "", NAME),
           NAME = gsub(" city", "", NAME))

  # Convert state abbriviation to full name
  if (nchar(data$state_name[1]) == 2){
    data <- data %>%
      mutate(state_name = abbr2state(state_name))
  }

  # Get state names and centers
  dc <- c("District of Columbia", "DC", -77.0369, 38.9072)
  pr <- c("Puerto Rico", "PR", -66.5901, 18.2208)

  state_names <- datasets::state.name %>%
    cbind(datasets::state.abb,
          datasets::state.center$x,
          datasets::state.center$y)
  state_names <- rbind(state_names[1:8, ], dc, state_names[9:50, ], pr) %>%
    as.data.frame()
  names(state_names) <- c("state_name", "state_abb", "lon", "lat")
  state_names <- state_names %>%
    mutate(lon = as.numeric(as.character(lon)),
           lat = as.numeric(as.character(lat)),
           state_name = as.character(state_name),
           state_abb = as.character(state_abb))

  # Import counties
  map_shapes <- geojsonio::geojson_read(system.file("extdata",
                                                    "us_counties.json",
                                                    package = "mustangnews"), what = "sp")

  # Link state id numbers with state names
  ids <- map_shapes@data %>%
    group_by(STATE) %>%
    count() %>%
    as.data.frame() %>%
    cbind(state_names)
  map_shapes <- merge(map_shapes, ids)

  # Edit old names
  map_shapes@data$NAME[2] <- "Kusilvak"
  map_shapes@data$NAME[2724] <- "Dona Ana"
  map_shapes@data$NAME[2730] <- "Oglala Lakota"
  map_shapes <- subset(map_shapes, state_abb != "PR")
  if (state != "all"){
    map_shapes <- subset(map_shapes, state_name == state)
    view <- unlist(c(unname(state_names[state_names$state_name == state, ][3]),
                     unname(state_names[state_names$state_name == state, ][4]),
                     5))
  } else {
    view = c(-96, 37.8, 4)
  }

  map_shapes <- merge(map_shapes, data, duplicateGeoms = TRUE)

  pal <- colorBin("YlOrRd", domain = map_shapes@data$variable)
  lab <- paste0(map_shapes@data$NAME, ": ",
                format(map_shapes@data$variable, big.mark = ","))

  map <- leaflet(data = map_shapes) %>%
    setView(view[1], view[2], view[3]) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal(variable),
                fillOpacity = .8,
                weight = 1,
                color = "white",
                label = lab,
                popup = lab) %>%
    addLegend(position = "bottomright",
              pal = pal,
              values = ~variable,
              title = paste("<strong>", variable_name, "</strong>"))

  return(map)
}


#' Check your data for use in 'county_map()` function
#'
#' Checks your dataframe of counties against the county names in the
#' `county_map()` to identify any missspellings or spacing issues
#'
#' @param data Your data frame to make a choropleth. Contains three columns,
#' county name, county state and your variable of intrest, in that order
#' @param state A string contianing the state name or abbriviation that
#' you would like to map. Default is "all" which maps the entire USA
#'
#' @return A data frame or a list identifying which counties are in the function
#' data set, but were not found in yours, and which counties are in your data set
#' but were not found in the function's. Please change the names of the counties
#' in your data set to match those in the function if you'd like to graph those
#' counties
#'
#' @importFrom usdata abbr2state
#' @importFrom dplyr mutate arrange filter select
#'
#' @export
#'
#' @examples
#' check_county_names(median_income)
#'
check_county_names <- function(data, state = "all"){

  all_counties = all_counties
  if(nchar(state) == 2){
    state <- abbr2state(state)
  }

  # format data correctly
  names(data) <- c("county", "state_name", "variable")
  if(nchar(data[1, 2]) == 2){
    data$state_name <- abbr2state(data$state_name)
  }
  # Remove territorial designation
  data <- data %>%
    mutate(county = gsub(" County", "", county),
           county = gsub(" Borough", "", county),
           county = gsub(" Census Area", "", county),
           county = gsub(" Municipality", "", county),
           county = gsub(" City and", "", county),
           county = gsub(" Parish", "", county),
           county = gsub(" city", "", county))

  in_yours <- left_join(data, all_counties) %>%
    filter(is.na(state_abb),
           county != state_name) %>%
    arrange(state_name, county) %>%
    select(county, state_name)

  in_function <- left_join(all_counties, data) %>%
    filter(is.na(variable),
           state_abb != "PR") %>%
    arrange(state_name, county) %>%
    select(county, state_name)

  if(state != "all"){
    in_yours <- in_yours %>%
      filter(state_name == state)
    in_function <- in_function %>%
      filter(state_name == state)
  }

  if(length(in_yours$county) == length(in_function$county)) {
    if(length(in_yours$county) == 0 & length(in_function$county) == 0){
      return("Good dataset")
    }
    non_matching <- cbind(in_function, in_yours)
    names(non_matching) <- c("function_counties", "function_state",
                             "your_counties", "your_state")
  } else {
    non_matching <- list(function_counties = in_function,
                         your_counties = in_yours)
  }
  return(non_matching)
}



