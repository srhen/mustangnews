#' @importFrom tibble tibble
NULL

#' USA county names
#'
#' @docType data
#'
#' @usage data(all_counties)
#'
#' @format A data frame with three variables: \code{county}, \code{state_name},
#'   \code{state_abb}{state abbriviation}
#'
#' @source \url{https://eric.clst.org/tech/usgeojson/}
#' @source \url{https://www.census.gov/}
"all_counties"

#' USA Median Household Income for 2018
#'
#' @docType data
#'
#' @usage data(median_income)
#'
#' @format A data frame with three variables: \code{Name}{County name},
#'   \code{Postal Code}{State name}, \code{Median Household Income}
#'
#' @source \url{https://www.census.gov/data/datasets/2018/demo/saipe/2018-state-and-county.html}
"median_income"


#' USA Life Expectancy
#'
#' @docType data
#'
#' @usage data(life_exp)
#'
#' @format A data frame with two variables: \code{state},
#'   \code{Life Expectancy}
#'
#' @source state.x77
"life_exp"

#' Cal Poly Greek Life Sanctions
#'
#' Greek life Sanctions from pulled from website archive 4/25/2020
#'
#' @docType data
#'
#' @usage data(old_sanctions)
#'
#' @format A data frame with seven variables: \code{Letters},
#'   \code{Chapter}, \code{Additional_Info_1},  \code{Additional_Info_2},
#'   \code{Additional_Info_3},  \code{Additional_Info_4}, \code{Section}
#'
#' @source \url{https://web.archive.org/web/20200425094029/https://greeklife.calpoly.edu/chapter-sanction-information}
"old_sanctions"

#' Sexual Assult Data
#'
#' Sexual assult data from 2014-18 from 4 CA universities
#'
#' @docType data
#'
#' @usage data(sexual_assault_4)
#'
#' @format A data frame with four variables: \code{Year},
#'   \code{Proportion of Students}{prop of students who reported experiencing this type of assult},
#'   \code{University},  \code{Offense}
#'
#' @source University Cleary Reports
"sexual_assault_4"

#' Stalking Data
#'
#' Information on stalking occurances from 2014-18 from 4 CA universities
#'
#' @docType data
#'
#' @usage data(stalking_3)
#'
#' @format A data frame with four variables: \code{Year},
#'   \code{Proportion of Students}{prop of students who reported experiencing stalking}
#'
#' @source University Cleary Reports
"stalking_3"

#' Cal Poly Stalking Data
#'
#' Information on stalking occurances from 2014-18 at Cal Poly
#'
#' @docType data
#'
#' @usage data(cal_poly_stalking)
#'
#' @format A data frame with four variables: \code{Year},
#'   \code{Proportion of Students}{prop of students who reported experiencing this type of assult},
#'   \code{University},  \code{Offense}
#'
#' @source University Cleary Reports
"cal_poly_stalking"

#' Presidental Election Adverstisment Spending
#'
#' Amount spent on advertisments for the 2004, 08, 12, and 16
#' presidential elections
#'
#' @docType data
#'
#' @usage data(ad_spending_overall)
#'
#' @format A data frame with two variables: \code{Year},
#'   \code{Amount Spent on Ads}
#'
#' @source \url{https://www.kaggle.com/fec/independent-political-ad-spending}
"ad_spending_overall"

#' Presidental Election Ad Spending by Party
#'
#' Amount spent on advertisments by politcal parties for the
#' 2004, 08, 12, and 16 presidential elections
#'
#' @docType data
#'
#' @usage data(ad_spending_total)
#'
#' @format A data frame with three variables: \code{Year},
#'   \code{Amount Spent on Ads}, \code{Party}
#'
#' @source \url{https://www.kaggle.com/fec/independent-political-ad-spending}
"ad_spending_total"

#' Detailed Presidental Election Adverstisment Spending
#'
#' Amount spent on advertisments for the 2004, 08, 12, and 16
#' presidential elections in support of and opposing each of the major
#' party candiates
#'
#' @docType data
#'
#' @usage data(ad_support_oppose)
#'
#' @format A data frame with four variables: \code{Year},
#'   \code{Amount Spent on Ads}, \code{Party}, \code{Position}
#'
#' @source \url{https://www.kaggle.com/fec/independent-political-ad-spending}
"ad_support_oppose"





