#' Turns SLO PD Public Log into a useable dataframe from its original text file
#' format
#'
#' @param current Scrapes current log posted on slocity.org. Defualt is TRUE
#' @param file A text file of a SLO PD Public Log to convert
#' @param location Gets latitiude and longitude for each adress mentioned in
#' the log. Must have a valid Google API key to use. Default is FALSE
#' @param google_api A string of a valid Google API
#'
#' @return A data frame with each row containing the information for one entry
#' from the log
#'
#' @importFrom stringr str_squish
#' @importFrom tidyr separate
#' @importFrom ggmap register_google geocode
#'
#' @export
#'
#' @examples
#' slopd_log()
#'
slopd_log <- function(current = TRUE, file = "txt", location = FALSE, google_api = "a"){

  if(current){
    if(nchar(file) > 3){
      stop("current is set to TRUE and txt file has been supplied. Please set current
           to FALSE to evaluate the txt file or remove the file agrument to get the
           current log posted online")
    }
    # Read in data line by line - scraped from online
    log <- as.data.frame(readLines("https://pdreport.slocity.org/policelog/rpcdsum.txt"))
  } else {
    if(nchar(file) <= 3){
      stop("Please provide the name of a file to evaluate")
    }
    # Read in data line by line - from file
    log <- as.data.frame(readLines(file))
    # Confirm that file is a SLOPD log
    if(log[1, ] != "AE"){
      stop("This file does not appear to be a SLO PD Public Log txt file")
    }
  }


  # Remove header (first three lines)
  log <- log[-c(1:3),]
  # Turn file into single character vector
  log <- paste(log, collapse = "")
  # Split off ending comments
  log <- strsplit(log, "--------------------------------------------------------------------------------")
  log <- unlist(log)
  # Keep only records, removing ending comments
  log <- ifelse(log[1] == "", log[2], log[1])
  # Split string by record
  log <- strsplit(log, "===============================================================================")
  log <- unlist(log)
  log <- log[-1]

  # Turn into data frame (each row is on record)
  log <- as.data.frame(matrix(data = log, ncol=2, byrow =T))
  names(log) <- c("Date","Info")
  # Seperate Date/Time information into variables
  log$Date <- str_squish(log$Date)
  variables <- c("Incid","Date","Received","Dispatched","Arrived","Cleared")
  log <- separate(data = log, col = Date, into = variables, sep = " ")
  # Reformat as date/time variable
  for (i in c(3:6)){
    log[,i] <- gsub("^[^:]+:", "", log[,i])
    log[,i] <- paste(log$Date, log[,i])
    log[,i] <- as.POSIXct(log[,i], format = "%m/%d/%y %H:%M")
    log[,i] <- as.character(log[,i])
  }

  # Splitting up Report information
  log <- log %>% separate(col = Info, into = c("Type","Info"), sep = "Location:")
  log$Type <- str_squish(gsub("^[^:]+: ", "", log$Type))
  log <- log %>% separate(col = Info, into = c("Location","Info"), sep = "As Observed:") %>%
    separate(col = Info, into = c("As_Observed","Info"),
                          sep = "Addr:")
  log$As_Observed <- str_squish(log$As_Observed)
  log <- log %>% separate(col = Info, into = c("Address","Info"), sep = "Clearance Code:") %>%
    separate(col = Address, into = c("Address","City"), sep = ", ") %>%
    separate(col = Address, into = c("Address","Grid"), sep = "; GRID ") %>%
    separate(col = Address, into = c("Address","Location_Info"), sep = ";", extra = "merge")
  log$Location_Info <- gsub("; G|; GR|; GRI|;G|;GR|;GRI", "", log$Location_Info)
  log$Location_Info <- str_squish(log$Location_Info)
  log$Address <- str_squish(log$Address)
  log <- log %>% separate(col = Address, into = c("Address","Apt_num"),
                          sep = "#")
  log$Address <- paste(log$Address, ", San Luis Obispo, CA", sep = "")
  log <- log %>% separate(col = Info, into = c("Clearance_Code","Info"),
                          sep = "Responsible Officer: ") %>%
    separate(col = Info, into = c("Responsible_Officer","Info"), sep = "Units: ") %>%
    separate(col = Info, into = c("Units","Info"), sep = "Des: ") %>%
    separate(col = Info, into = c("Des","Call_Comments"), sep = "CALL COMMENTS:")
  log$Des <- gsub("incid#=\\d{9}","", log$Des)
  log <- log %>% separate(col = Des, into = c("Des", "Call"), sep = "call=")
  log$MDC <- grepl("(MDC)", log$Des)
  log$Des <- gsub("\\(MDC\\)", "", log$Des)
  log <- log %>% separate(col = Des, into = c("Des","oc"), sep = "oc:") %>%
    separate(col = Des, into = c("Des","clr"), sep = "clr:") %>%
    separate(col = Des, into = c("Des","disp"), sep = "disp:")

  if (location){
    #Get Long and Lat for addresses
    register_google(google_api)
    geo <- geocode(log$Address, output = "latlon", source = "google")
    # Remove values that were incorrectly coded
    geo <- geo %>%
      mutate(lon = ifelse(lon < -120 & lon > -121, lon, NA),
             lat = ifelse(lat < 36 & lat > 35, lat, NA))
    log <- cbind(log, geo)
  }

  return(log)
}
