
# Basic data.table function for quickly replacing NAs.
# Answer comes from here: https://stackoverflow.com/a/60321651/2449656

#' @import data.table
char_na_to_empty <- function(dt) {
    for (i in names(dt)){
        dt[is.na(get(i)), (i) := ""]
    }
    return(dt[])
}