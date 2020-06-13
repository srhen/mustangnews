#' Get CP Greek Life sanction information
#'
#' @description Scrapes Cal Poly Greek Life's chapter sanction webpage to get
#' data frame of current sanctions
#'
#' @param save Saves the outputted data frame as a csv file named
#' "cpgl_sanctions_MMDDYY.csv" to your current directory. Default is FALSE
#'
#' @return A data frame with 7 columns containing chapter sanction information
#'
#' @references \url{https://greeklife.calpoly.edu/chapter-sanction-information}
#'
#' @examples
#'
#' get_gl_sanctions()
#
#' ## To save the outputted data frame of sanction info as a csv
#' \dontrun{
#' get_gl_sanctions(save = TRUE)
#' }
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom tidyr separate
#' @importFrom dplyr mutate arrange
#' @importFrom stringr str_split str_detect
#' @importFrom janitor remove_empty
#'
#'
#' @export
get_gl_sanctions <- function(save = FALSE){
  # Scrape Data
  sanctionurl <- "https://greeklife.calpoly.edu/chapter-sanction-information"
  text <- sanctionurl %>%
    read_html() %>%
    html_nodes(css=".even") %>%
    html_text()

  # Date comes in as single string, need to split it up
  text <- text %>%
    str_split("Chapters Currently on Sanctions")
  text <- text[[1]][2] %>%
    str_split("Open Investigations")
  sanc <- text[[1]][1]
  open <- text[[1]][2]

  # Clean Data
  sanctions <- clean_gl_entries(sanc) %>%
    mutate(Section = "On Sanctions")
  open_invest <- clean_gl_entries(open) %>%
    mutate(Section = "Open Investigations")
  site_info <- bind_rows(sanctions, open_invest)

  if(save){
    day <- format(Sys.Date(), "%m%d%y")
    write.csv(site_info, paste0("cpgl_sanctions_", day, ".csv"))
  }

  return(site_info)

}


clean_gl_entries <- function(section){

  section <- gsub("\t", "", section)
  sanc <- section %>%
    str_split("\n") %>%
    unlist
  to_remove <- c("")
  sanc <- sanc[! sanc %in% to_remove]
  sanc <- sanc[nchar(sanc) > 1]

  chapter <- 0
  col <- 1
  n <- length(sanc)
  sanctions <- as.data.frame(matrix(NA, nrow = round(n/2), ncol = 5))
  for (i in 1:n){
    if(str_detect(sanc[i], "\\p{Greek}") |
       str_detect(sanc[i], "[A-Z][A-Z]-") |
       str_detect(sanc[i], "[A-Z][A-Z] -")){
      chapter <- chapter + 1
      col <- 1
    }
    sanctions[chapter, col] <- sanc[i]
    col <- col + 1
  }
  names(sanctions)[1] <- "Chapter"
  # Remove unknown number of empty columns
  sanctions <- sanctions %>%
    remove_empty(which = c("rows", "cols"))
  if(dim(sanctions)[1] == 0){
    Chapter <- "No chapters listed"
    to_return <- as.data.frame(Chapter) %>%
      mutate(Letters = NA,
             V2 = NA)
  } else {
    to_return <- sanctions %>%
      separate(Chapter, into = c("Letters", "Chapter"), sep = "-") %>%
      mutate(Chapter = str_squish(Chapter),
             Letters = str_squish(Letters)) %>%
      arrange(Chapter)
  }
  # Add empty columns to keep data frames all the same length
  while(length(to_return) < 6){
    last_col <- names(to_return)[length(to_return)]
    last_col <- as.numeric(gsub("V", "", last_col))
    new_col <- paste0("V", last_col + 1)
    to_return <- to_return %>%
      mutate(new = NA)
    names(to_return)[length(to_return)] <- new_col
  }
  names(to_return)[3:6] <- c("Addtional_Info_1", "Addtional_Info_2",
                             "Addtional_Info_3", "Addtional_Info_4")

  return(to_return)
}



#' Compare CP Greek Life sanction information
#'
#' @description Compares old Cal Poly Greek Life sanction information with
#' what is currently posted on the website
#'
#' @param old A chapter sanction information data frame with 7 columns created
#' by `get_gl_sanctions()`
#' @param new A chapter sanction information data frame with 7 columns created
#' by `get_gl_sanctions()`. Default is "current" which scrapes the Greek Life
#' Sanctions website for the current sanction list
#'
#' @return An html table. Green indicates information that has been added and
#' red indicates information that has been removed from the site.
#'
#'
#' @references \url{https://greeklife.calpoly.edu/chapter-sanction-information}
#'
#' @importFrom compareDF compare_df create_output_table
#'
#' @export
#'
#' @examples
#' comp_gl_sanctions(old_sanctions)
#'
comp_gl_sanctions <- function(old, new = "current"){

  if(new == "current"){
    new <- get_gl_sanctions()
  }

  comp <- compare_df(new, old, c("Chapter"), keep_unchanged_cols = FALSE)
  comp_table <- create_output_table(comp)

  return(comp_table)
}


