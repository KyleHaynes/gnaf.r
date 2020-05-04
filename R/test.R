

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



