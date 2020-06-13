#' Scrapes Cal Poly UPD's Activity Log from the department's website and converts
#' it into a useable dataframe
#'
#' @param start A string with the date to start collecting reports from. Please
#' use "YYYY-MM-DD" format. Keep in mind that the online log only goes back
#' 60 days from the current date. Default is "oldest" and starts collecting
#' from the oldest available report
#' @param end A string with the date to finish collecting reports from.Please
#' use "YYYY-MM-DD" format. Keep in mind that the online log only goes back
#' 60 days from the current date. Default is "newest" and stops collecting
#' from the most recent report
#'
#' @return A data frame with each row containing the information for one entry
#' from the log. If a single entry had mulitple inicident types, then there is one
#' row for each incident type, so take care when reporting total number of
#' incidents.
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom dplyr mutate
#' @importFrom tidyr separate replace_na gather
#' @importFrom car Recode
#' @importFrom stringr str_detect
#'
#' @export
#'
#' @examples
#' upd_log(start = "2020-05-22")
#'
upd_log <- function(start = "oldest", end = "newest"){

  if(start != "oldest" & str_detect(start, "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]") == FALSE){
    stop("Invalid start date. Please check start date format.")
  }
  if(end != "newest" & str_detect(end, "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]") == FALSE){
    stop("Invalid end date. Please check end date format.")
  }
  if(start != "oldest" & end != "newest" & start > end){
    stop("Start date is after end date. Please choose new dates")
  }

  # Scrape Data
  logurl <- "https://afd.calpoly.edu/police/campus-reports/logs"
  log <- logurl %>%
    read_html() %>%
    html_nodes(css="#content td") %>%
    html_text()
  header <- logurl %>%
    read_html() %>%
    html_nodes(css="th") %>%
    html_text()

  # Convert log to data frame
  table <- matrix(data = log, ncol = 5, byrow = T)
  table <- as.data.frame(table)
  names(table) <- header

  # Format Date Reported
  table$`Date/Time Reported` <- as.POSIXct(table$`Date/Time Reported`,  format = "%m/%d/%Y %I:%M %p")

  # Get CAD #
  table$Details <- as.character(table$Details)
  table <- separate(table, Details, into = c("Type", "CAD"), sep ="CAD #:")

  # Get Report #
  table <- table %>%
    separate(Type, into = c("Type", "Report1"), sep ="Report #:") %>%
    separate(CAD, into = c("CAD", "Report2"), sep ="Report #:") %>%
    mutate(Report1 = replace_na(Report1, ""),
           Report2 = replace_na(Report2, ""),
           Report = paste(Report1, Report2, sep = ""))
  table <- table[,-c(5,7)]

  # Update Type of Incident
  table$Type <- gsub("Incident: ", "", table$Type)
  # Each Type of capitilized crime gets its own line
  table <- table %>%
    separate(Type, into = c("Type 1", "Type 2", "Type 3", "Type 4", "Type 5",
                            "Type 6", "Type 7", "Type 8", "Type 9", "Type 10",
                            "Type 11", "Type 12"),
             sep =" // ") %>%
    gather(remove, Type, `Type 1`:`Type 12`)
  table <- table[!is.na(table$Type), ]
  # Seperating out extra details from type
  table <- separate(table, Type,
                    into = c("Type", "Subtype1", "Subtype2"), sep =" - ")

  # Splitting up occurance variable into start and end time
  table <- table %>%
    separate(`Date/Time Occurred`,
                    into = c("Occurance_Began", "Occurance_Ended"),
                    sep = " - ") %>%
    mutate(Occurance_Began = as.POSIXct(Occurance_Began,  format = "%m/%d/%Y %I:%M %p"),
           Occurance_Ended = as.POSIXct(Occurance_Ended,  format = "%m/%d/%Y %I:%M %p"))

  # Recode Location
  table <- table %>%
    mutate(Location = gsub("x - ", "x: ", Location)) %>%
    separate(Location, into = c("Location", "Off_campus_Location"), sep = "ff Campus - ") %>%
    mutate(Area = Recode(Location,
                       'c("Yosemite Hall", "Yosemite Hall Area", "Sierra Madre Hall Area",
                       "Sierra Madre (towers 1-5)") = "Towers";
                     c("Ytt A", "Ytt B", "Ytt C", "Ytt D", "Ytt E", "Ytt F", "Ytt G",
                       "Ytt H", "Ytt B1h", "Ytt Area Of Dorms") = "Ytt";
                     c("Tenaya Hall", "Muir Hall", "Sequoia Hall", "Trinity Hall",
                       "Fremont Hall", "Santa Lucia Hall","Tenaya Hall Area",
                       "Muir Hall Area", "Sequoia Hall Area", "Trinity Hal Areal",
                       "Fremont Hall Area", "Santa Lucia Hall Area") = "Red Bricks";
                     c("Diablo Hall", "Palomar Hall", "Lassen Hall", "Whitney Hall",
                       "Shasta Hall", "Diablo Hall Area", "Palomar Hall Area",
                       "Lassen Hall Area", "Whitney Hall Area",
                       "Shasta Hall Area") = "North Mountian";
                     c("Cerro Vista Housing", "Cerro Vista  Cabrillo",
                       "Cerro Vista  Islay", "Cerro Vista  Romauldo",
                       "Cerro Vista  Hollister", "Cerro Vista Housing Area",
                       "Cerro Vista Bishop", "Cerro Vista  Morro",
                       "Cerro Vista Circle") = "Cerro Vista";
                     c("Pcv Corralitos", "Pcv Huasna", "Pcv Estrella", "Pcv Buena Vista",
                       "Pcv Aliso", "Pcv Gypsum", "Pcv Foxen", "Pcv Inyo", "Pcv Dover",
                       "Poly Canyon Village", "Poly Canyon Village Area") = "PCV";
                     c("Canyon Circle Parking Structure", "Grand Avenue Parking Structure",
                       "Village Drive Parking Structure","K-1 Parking Lot","H-2 Parking Lot",
                       "H-4 Parking Lot", "H-16 Parking Lot", "Ytt Parking Structure",
                       "H-1 Parking Lot", "A-1 Parking", "H-14 Parking Lot", "C-7 Parking Lot",
                       "G-2 Parking Lot", "R-1 Parking Lot", "H-12 Parking Lot",
                       "C-1 Parking Lot", "H-10 Parking Lot", "C-3 Parking Lot",
                       "C-4 Parking Lot", "Grand Avenue Parking Structure Area",
                       "H-15 Parking Lot", "C-2 Parking Lot", "C-5 Parking Lot","C-6 Parking Lot",
                       "H-11 Parking Lot", "H-13 Parking Lot", "K-2 Parking Lot",
                       "R-3 Parking Lot") = "Parking Lot/Structure";
                     c("Off Campus") = "Off Campus";
                     else = "On Campus"'),
           Area2 = Recode(Area, 'c("Ytt", "Red Bricks", "North Mountian",
                         "Towers") = "Freshman Dorms";
                         c("Cerro Vista", "PCV") = "On Campus Apartments"'),
           Location = gsub("O", "", Location),
           Off_campus_Location = replace_na(Off_campus_Location, ""),
           Location = paste(Location, Off_campus_Location, sep = ""))
  names(table)[1] <- "Date_Time_Reported"

  # Reorder columns
  table <- table[,c(1:3, 6, 8, 10:12, 7, 4, 13:14)]
  table <- arrange(table, desc(Date_Time_Reported))

  # Time frame
  if(start == "oldest"){
    start <- min(table$Date_Time_Reported)
  }
  if(end == "newest"){
    end <- max(table$Date_Time_Reported)
  } else {
    end <- paste(end, "23:59:59")
  }
  to_return <- table %>%
    filter(table$Date_Time_Reported >= start & table$Date_Time_Reported <= end)

  return(to_return)
}

