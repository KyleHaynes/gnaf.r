#' @name create_street_address

#' @title Creates street address strings from G-NAF.

#' @description Creates street address strings from G-NAF. 


#' @param gnaf_data G-NAF data.table input.

#' @param string_var A character string for the output street address string. Default is \code{FALSE}.


#' @return Returns a new column on an existing G-NAF data.table.


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update


#' @import data.table
#' @export
create_street_address <- function(gnaf_data,
                                string_var = "STREET_ADDRESS",
                                verbose = TRUE){
               
    for (i in names(gnaf_data)){
        gnaf_data[is.na(get(i)), (i) := ""]
    }

    gnaf_data[, (string_var) := NA_character_]
    # Lots with no numbers
    gnaf_data[LOT_NUMBER != "" & NUMBER_FIRST == "", (string_var) := paste0("LOT ", LOT_NUMBER,
                                                                               " ", FLAT_TYPE,
                                                                               " ", FLAT_NUMBER_PREFIX, FLAT_NUMBER, FLAT_NUMBER_SUFFIX,
                                                                               " ", STREET_NAME, " ", STREET_TYPE, " ", STREET_SUFFIX)]
    # With 1 number
    gnaf_data[NUMBER_FIRST != "" & NUMBER_LAST == "", (string_var) := paste0(FLAT_TYPE,
                                                                               " ", FLAT_NUMBER_PREFIX, FLAT_NUMBER, FLAT_NUMBER_SUFFIX,
                                                                               " ", NUMBER_FIRST, NUMBER_FIRST_SUFFIX,
                                                                               " ", STREET_NAME, " ", STREET_TYPE, " ", STREET_SUFFIX)]
    # With range number
    gnaf_data[NUMBER_FIRST != "" & NUMBER_LAST != "", (string_var) := paste0(FLAT_TYPE,
                                                                               " ", FLAT_NUMBER_PREFIX, FLAT_NUMBER, FLAT_NUMBER_SUFFIX,
                                                                               " ", NUMBER_FIRST, NUMBER_FIRST_SUFFIX,
                                                                               "-", NUMBER_LAST, NUMBER_LAST_SUFFIX,
                                                                               " ", STREET_NAME, " ", STREET_TYPE, " ", STREET_SUFFIX)]

    gnaf_data[, (string_var)  := trimws(gsub("  +", " ", get(..string_var), perl = TRUE))]

    for (i in names(gnaf_data)){
        gnaf_data[get(i) == "", (i) := NA]
    }
}