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
create_street_variations <- function(...,
                                string_var = "STREET_ADDRESS",
                                verbose = TRUE) {


    # Build G-NAF
    gnaf_data <- build_gnaf(...)

    # Add new variable ...
    gnaf_data[, (string_var) := NA_character_]
    

    # ---- First Run ----
    # With range number
    gnaf_data[, (string_var) := paste0(
        FLAT_TYPE, " ", FLAT_NUMBER_PREFIX, FLAT_NUMBER, FLAT_NUMBER_SUFFIX,
        # Add street number info if it exists...
        fifelse(NUMBER_FIRST != "", paste0(" ", NUMBER_FIRST, NUMBER_FIRST_SUFFIX),
        # Otherwise, add lot number if it exists... 
        fifelse(NUMBER_FIRST == "" & LOT_NUMBER != "", paste0(" LOT ", LOT_NUMBER, LOT_NUMBER_SUFFIX), "YYYYYY"), "XXXXXX"
        ),
        # If Street number is a range, add that.
        fifelse(NUMBER_LAST != "", paste0("-", NUMBER_LAST, NUMBER_LAST_SUFFIX), ""),
        " ", STREET_NAME, " ", STREET_TYPE, " ", STREET_SUFFIX)]


    vec_vars <- c("ADDRESS_DETAIL_PID", "CONFIDENCE", "STREET_ADDRESS", "POSTCODE", "LOCALITY_NAME")

    o <- gnaf_data[, ..vec_vars]

    # ---- Unit variations ----
    # Remove flat Type
    gnaf_data[, (string_var) := NA_character_]
    gnaf_data[l <<- FLAT_TYPE != "", (string_var) := paste0(
        FLAT_NUMBER_PREFIX, FLAT_NUMBER, FLAT_NUMBER_SUFFIX,
        # Add street number info if it exists...
        fifelse(NUMBER_FIRST != "", paste0(" ", NUMBER_FIRST, NUMBER_FIRST_SUFFIX),
        # Otherwise, add lot number if it exists... 
        fifelse(NUMBER_FIRST == "" & LOT_NUMBER != "", paste0(" LOT ", LOT_NUMBER, LOT_NUMBER_SUFFIX), "YYYYYY"), "XXXXXX"
        ),
        # If Street number is a range, add that.
        fifelse(NUMBER_LAST != "", paste0("-", NUMBER_LAST, NUMBER_LAST_SUFFIX), ""),
        " ", STREET_NAME, " ", STREET_TYPE, " ", STREET_SUFFIX)]

    o <- rbind(o, gnaf_data[l, ..vec_vars])

    # Add / sep for flats ...
    gnaf_data[, (string_var) := NA_character_]
    gnaf_data[l <<- FLAT_TYPE != "", (string_var) := paste0(
        FLAT_NUMBER_PREFIX, FLAT_NUMBER, FLAT_NUMBER_SUFFIX,
        # Add street number info if it exists...
        fifelse(NUMBER_FIRST != "", paste0("/", NUMBER_FIRST, NUMBER_FIRST_SUFFIX),
        # Otherwise, add lot number if it exists... 
        fifelse(NUMBER_FIRST == "" & LOT_NUMBER != "", paste0(" LOT ", LOT_NUMBER, LOT_NUMBER_SUFFIX), "YYYYYY"), "XXXXXX"
        ),
        # If Street number is a range, add that.
        fifelse(NUMBER_LAST != "", paste0("-", NUMBER_LAST, NUMBER_LAST_SUFFIX), ""),
        " ", STREET_NAME, " ", STREET_TYPE, " ", STREET_SUFFIX)]

    o <- rbind(o, gnaf_data[l, ..vec_vars])

    # Remove ranges (i.e add both range variation) ...
    gnaf_data[, (string_var) := NA_character_]
    gnaf_data[l <<- NUMBER_LAST != "", (string_var) := paste0(
        FLAT_NUMBER_PREFIX, FLAT_NUMBER, FLAT_NUMBER_SUFFIX,
        # Add street number info if it exists...
        fifelse(NUMBER_FIRST != "", paste0(" ", NUMBER_FIRST, NUMBER_FIRST_SUFFIX),
        # Otherwise, add lot number if it exists... 
        fifelse(NUMBER_FIRST == "" & LOT_NUMBER != "", paste0(" LOT ", LOT_NUMBER, LOT_NUMBER_SUFFIX), "YYYYYY"), "XXXXXX"
        ),
        # # If Street number is a range, add that.
        # fifelse(NUMBER_LAST != "", paste0("-", NUMBER_LAST, NUMBER_LAST_SUFFIX), ""),
        " ", STREET_NAME, " ", STREET_TYPE, " ", STREET_SUFFIX)]

    o <- rbind(o, gnaf_data[l, ..vec_vars])

    # Finally, trim and remove redundant space.
    o[, (string_var)  := trimws(gsub("  +", " ", get(..string_var), perl = TRUE))]

    return(o[])
}