

test_mesh_block_2011 <- function(setup = gnaf_setup_data){

    tmp <- merge(get_address_mesh_block_2011(), get_mb_2011(), by = "MB_2011_PID", all.x = T)

    if(all(gsub("^MB11", "", tmp$MB_2011_PID) == tmp$MB_2011_CODE)){
        message("---- Function has passed ----\n\n")
        message("Once the `MB_2011_PID` has had \"MB11\" removed from the start of the string, this matches the `MB_2011_CODE` variable.")
        message("Therefore, `get_mb_2011()` is redundant for adding Mesh Blocks to G-NAF as we can derive this from `get_address_mesh_block_2011()`")
    } else {
        stop("Mesh Blocks 2011 have not been derived correctly. An issue should be reported at: https://github.com/KyleHaynes/gnaf.r/issues")
    }
}

test_mesh_block_2016 <- function(setup = gnaf_setup_data){

    tmp <- merge(get_address_mesh_block_2016(), get_mb_2016(), by = "MB_2016_PID", all.x = T)

    if(all(gsub("^MB16", "", tmp$MB_2016_PID) == tmp$MB_2016_CODE)){
        message("---- Function has passed ----\n\n")
        message("Once the `MB_2016_PID` has had \"MB16\" removed from the start of the string, this matches the `MB_2016_CODE` variable.")
        message("Therefore, `get_mb_2016()` is redundant for adding Mesh Blocks to G-NAF as we can derive this from `get_address_mesh_block_2016()`")
    } else {
        stop("Mesh Blocks 2016 have not been derived correctly. An issue should be reported at: https://github.com/KyleHaynes/gnaf.r/issues")
    }
}

# Following tests require a file to be built.
tests <- function(setup = gnaf_setup_data){

    g <- build_gnaf(setup = gnaf_setup_data)

    # Check 1.1
    if(nrow(g[is.na(LOT_NUMBER) & is.na(NUMBER_FIRST)]) > 0){
        stop("Check 1.1 failed. An address should always have a `LOT_NUMBER` or ideally a `NUMBER_FIRST`.")
    }

    if(nrow(g[!is.na(FLAT_TYPE) & is.na(FLAT_NUMBER)]) > 0){
        message("A `FLAT_TYPE` type can have no `FLAT_NUMBER`")
    }

    # Check 1.2
    if(nrow(g[!is.na(FLAT_TYPE) & is.na(FLAT_NUMBER) & is.na(FLAT_NUMBER_SUFFIX)]) > 0){
        stop("Check 1.2 failed. A `FLAT_TYPE` type must have a `FLAT_NUMBER` or `FLAT_NUMBER_SUFFIX`.")
    }

    # Check 1.3
    if(nrow(g[is.na(FLAT_TYPE) & (!is.na(FLAT_NUMBER) | !is.na(FLAT_NUMBER_SUFFIX))]) > 0){
        stop("Check 1.3 failed. A `FLAT_TYPE` type must be specified if a `FLAT_NUMBER` or `FLAT_NUMBER_SUFFIX` is present.")
    }

    # Check 1.4
    if(nrow(g[is.na(NUMBER_FIRST) & !is.na(NUMBER_LAST)]) > 0){
        stop("Check 1.4 failed. A `NUMBER_FIRST` can not be missing if there is a `NUMBER_LAST`.")
    }

    # Check 1.5
    if(nrow(g[is.na(NUMBER_FIRST) & !is.na(NUMBER_FIRST_SUFFIX)]) > 0){
        stop("Check 1.5 failed. A `NUMBER_FIRST` can not be missing if there is a `NUMBER_FIRST_SUFFIX`.")
    }

    # Check 1.6
    if(nrow(g[is.na(NUMBER_LAST) & !is.na(NUMBER_LAST_SUFFIX)]) > 0){
        message("There are a few (n=", nrow(g[is.na(NUMBER_LAST) & !is.na(NUMBER_LAST_SUFFIX)]), ") instances when a `NUMBER_LAST` can be missing if there is a `NUMBER_LAST_SUFFIX`.")
    }
    
}


