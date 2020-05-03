#' @name setup

#' @title A mandatory function call for the rest of this package to work.

#' @description The function identifies all relevant inputs and metadata for the importing of the G-NAF in to R. 


#' @param dir A character vector identifying the root directory of where the G-NAF extract is located.

#' @param states A regular expression of which Australian State jurisdictions to import.

#' @param verbose A logical argument to determine if the function should be verbose or not. Default is \code{TRUE}.


#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update


#' @import data.table
#' @export
setup <- function(dir = "C:\\temp\\gnaf\\G-NAF\\G-NAF FEBRUARY 2020\\", states = NA, verbose = TRUE){
    # Reassign dir.
    root <- dir

    # Normalise path, it must exist. It will throw an error otherwise.
    root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  

    g_naf_dir <- paste0(root, "/standard")
    ac_dir <- paste0(root, "/Authority Code")


    files <- list.files(g_naf_dir, pattern = "\\.psv$")
    files_full <- list.files(g_naf_dir, pattern = "\\.psv$",  full.names = TRUE)
    dt <- data.table(files = files)
    # Remove redundancy.
    dt[, names := gsub("_psv\\.psv$", "", files)]
    dt[, full_paths := files_full]
    dt[, state := gsub("_.*", "", names)]
    dt[, file := gsub("^[A-Z]+_", "", names)]
    
    if(!states %in% NA){
        dt <- dt[state %ilike% states]
    }


    files <- list.files(ac_dir, pattern = "\\.psv$")
    files_full <- list.files(ac_dir, pattern = "\\.psv$",  full.names = TRUE)
    ac <- data.table(files = files)
    # Remove redundancy.
    ac[, names := gsub("_AUT_psv\\.psv$", "", files)]
    ac[, names := gsub("^Authority_Code_", "", names, ignore.case = TRUE)]
    ac[, full_paths := files_full]
    ac[, file := gsub("^[A-Z]+_", "", names)]    


    if(verbose){
        len_files <- length(files)
        if(len_files == 170){
            message("There are, ", len_files, "*.psv files in the defined directory. This appears the correct number of files.")
        } else if (len_files < 170){
            message("There are, ", len_files, "*.psv files in the defined directory. This is less than the expected number of 170 files.")
        } else {
            message("There are, ", len_files, "*.psv files in the defined directory. This exceeds the expected number of 170 files. This may be a user intentional issue")
        }

        # Summary
        
        message("\nFile type (N = 9 indicates all jurisdictions) counts:")
        print(dt[, .N, .(file)])

        message("\nFiles per state (19 indicate all G-NAF related files):")
        print(dt[, .N, .(state)])
    }


    auth_data <- lapply(files_full, fread)
    names(auth_data) <- ac$names

    g_naf_setup <- list(dir = g_naf_dir,
                        schema = dt,
                        ac_schema = ac,
                        ac_md = auth_data

    )

    # Return the path.
    invisible(g_naf_setup)

    ##
    ## environment(iris) <- asNamespace('xxx')

}

# gn <- setup(states = "QLD")


# get street_locality
get_street_locality <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_STREET_LOCALITY_psv"]$full_paths

    # fread and bind together as a single data.table.
    street_locality <- rbindlist(lapply(paths, fread))

    # Return the object
    return(street_locality)
}

# get street_locality_point
get_street_locality_point <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_STREET_LOCALITY_POINT_psv"]$full_paths

    # fread and bind together as a single data.table.
    street_locality_point <- rbindlist(lapply(paths, fread))

    # Return the object
    return(street_locality_point)
}

# get street_locality_alias
get_street_locality_alias <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_STREET_LOCALITY_ALIAS_psv"]$full_paths

    # fread and bind together as a single data.table.
    street_locality_alias <- rbindlist(lapply(paths, fread))

    # Return the object
    return(street_locality_alias)
}


# get primary_secondary
get_primary_secondary <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_primary_secondary_psv"]$full_paths

    # fread and bind together as a single data.table.
    primary_secondary <- rbindlist(lapply(paths, fread))

    # Return the object
    return(primary_secondary)
}

# get locality
get_locality <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_locality_psv"]$full_paths
    paths <- paths[!paths %ilike% "_STREET_"]

    # fread and bind together as a single data.table.
    locality <- rbindlist(lapply(paths, fread))

    # Return the object
    return(locality)
}

# get locality_point
get_locality_point <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_locality_point_psv"]$full_paths
    paths <- paths[!paths %ilike% "_STREET_"]

    # fread and bind together as a single data.table.
    locality_point <- rbindlist(lapply(paths, fread))

    # Return the object
    return(locality_point)
}

# get locality_neighbour
get_locality_neighbour <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_locality_neighbour_psv"]$full_paths
    paths <- paths[!paths %ilike% "_STREET_"]

    # fread and bind together as a single data.table.
    locality_neighbour <- rbindlist(lapply(paths, fread))

    # Return the object
    return(locality_neighbour)
}

# get locality_alias
get_locality_alias <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_locality_alias_psv"]$full_paths
    paths <- paths[!paths %ilike% "_STREET_"]

    # fread and bind together as a single data.table.
    locality_alias <- rbindlist(lapply(paths, fread))

    # Return the object
    return(locality_alias)
}


# get address_site
get_address_site <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_site_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_site <- rbindlist(lapply(paths, fread))

    # Return the object
    return(address_site)
}



# get address_site_geocode
get_address_site_geocode <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_site_geocode_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_site_geocode <- rbindlist(lapply(paths, fread))

    # Return the object
    return(address_site_geocode)
}



# get address_feature
get_address_feature <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_feature_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_feature <- rbindlist(lapply(paths, fread))

    # Return the object
    return(address_feature)
}


# get address_detail
get_address_detail <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_detail_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_detail <- rbindlist(lapply(paths, fread))

    # Return the object
    return(address_detail)
}


# get address_default_geocode
get_address_default_geocode <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_default_geocode_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_default_geocode <- rbindlist(lapply(paths, fread))

    # Return the object
    return(address_default_geocode)
}


# get address_alias
get_address_alias <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_alias_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_alias <- rbindlist(lapply(paths, fread))

    # Return the object
    return(address_alias)
}


# get state
get_state <- function(setup = gn){
    
    # Return the scheme from the setup object.
    dt <- gn$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_state_psv"]$full_paths

    # fread and bind together as a single data.table.
    state <- rbindlist(lapply(paths, fread))

    # Creation date doesn't matter, not does retire state.
    state[, DATE_CREATED := NULL]
    state[, DATE_RETIRED := NULL]
    # Don't need an expanded and abbreviation. 
    state[, STATE_ABBREVIATION := NULL]

    # Return the object
    return(state[])
}


build_gnaf <- function(setup = gn,
                       remove_empty_vars = TRUE,
                       drop_date_variables = TRUE,
                       verbose = TRUE){
    
    # ---- Import address detail ----
    dt <- get_address_detail()
    if(remove_empty_vars){
        dt <-  dt[, which(unlist(lapply(dt, function(x) !all(is.na(x))))), with = FALSE]
    }
    if(drop_date_variables){
        drop <- names(dt)[grepl("^DATE", names(dt))]
        set(dt, , drop, NULL)
    }

    # Start a list of unique variables on each dataset (as they merge).
    vars <- list(address_detail = names(dt))

    # ---- Import locality & merge ----
    tmp <- get_locality()
    if(remove_empty_vars){
        tmp <-  tmp[, which(unlist(lapply(tmp, function(x) !all(is.na(x))))), with = FALSE]
    }
    if(drop_date_variables){
        drop <- names(tmp)[grepl("^DATE", names(tmp))]
        set(tmp, , drop, NULL)
    }

    # Merge.
    dt <- merge(dt, tmp, by = "LOCALITY_PID", all.x = TRUE, suffixes = c("", "_locality"))
    # Append new variables.
    vars <- c(vars,  list(locality = setdiff(names(dt), unlist(vars, use.names = FALSE))))

    # ---- Import street locality & merge ----
    tmp <- get_street_locality()
    if(remove_empty_vars){
        tmp <-  tmp[, which(unlist(lapply(tmp, function(x) !all(is.na(x))))), with = FALSE]
    }
    if(drop_date_variables){
        drop <- names(tmp)[grepl("^DATE", names(tmp))]
        set(tmp, , drop, NULL)
    }
    # Merge.
    dt <- merge(dt, tmp, by = "STREET_LOCALITY_PID", all.x = TRUE, suffixes = c("", "_street_locality"))
    # Can drop the following (as we have it)
    set(dt, , "LOCALITY_PID_street_locality", NULL)
    
    # Append new variables.
    vars <- c(vars,  list(street_locality = setdiff(names(dt), unlist(vars, use.names = FALSE))))


    # ---- Import state & merge ----
    tmp <- get_state()

    # Merge.
    dt <- merge(dt, tmp, by = "STATE_PID", all.x = TRUE, suffixes = c("", "_state"))
    # Append new variables.
    vars <- c(vars,  list(locality = setdiff(names(dt), unlist(vars, use.names = FALSE))))

    # `STATE_PID` will be of no use now. Remove it
    dt[, STATE_PID := NULL]

    # ---- Authority code replacement ----
    names(dt)[grepl("CLASS", names(dt))]


    dt$STREET_SUFFIX <- gn$ac_md$STREET_SUFFIX$NAME[match(dt$STREET_SUFFIX_CODE, gn$ac_md$STREET_SUFFIX$CODE)]
    set(dt, , "STREET_SUFFIX_CODE", NULL)

    dt$FLAT_TYPE <- gn$ac_md$FLAT_TYPE$NAME[match(dt$FLAT_TYPE_CODE, gn$ac_md$FLAT_TYPE$CODE)]
    set(dt, , "FLAT_TYPE_CODE", NULL)

    dt$LEVEL_TYPE <- gn$ac_md$LEVEL_TYPE$NAME[match(dt$LEVEL_TYPE_CODE, gn$ac_md$LEVEL_TYPE$CODE)]
    set(dt, , "LEVEL_TYPE_CODE", NULL)

    # Not interested in street_type, as the codes are the expanded format.
    setnames(dt, "STREET_TYPE_CODE", "STREET_TYPE")

    dt$LOCALITY_CLASS <- gn$ac_md$LOCALITY_CLASS$NAME[match(dt$LOCALITY_CLASS_CODE, gn$ac_md$LOCALITY_CLASS$CODE)]
    set(dt, , "LOCALITY_CLASS_CODE", NULL)

    dt$STREET_CLASS <- gn$ac_md$STREET_CLASS$NAME[match(dt$STREET_CLASS_CODE, gn$ac_md$STREET_CLASS$CODE)]
    set(dt, , "STREET_CLASS_CODE", NULL)




    # # ---- Import XXXXXXXXXXXX & merge ----
    # tmp <- get_()
    # if(remove_empty_vars){
    #     tmp <-  tmp[, which(unlist(lapply(tmp, function(x) !all(is.na(x))))), with = FALSE]
    # }
    # # Merge.
    # dt <- merge(dt, tmp, by = "LOCALITY_PID", all.x = TRUE, suffixes = c("", "_locality"))
    # # Append new variables.
    # vars <- c(vars,  list(locality = setdiff(names(dt), unlist(vars, use.names = FALSE))))

    # # ---- Import XXXXXXXXXXXX & merge ----
    # tmp <- get_()
    # if(remove_empty_vars){
    #     tmp <-  tmp[, which(unlist(lapply(tmp, function(x) !all(is.na(x))))), with = FALSE]
    # }
    # # Merge.
    # dt <- merge(dt, tmp, by = "LOCALITY_PID", all.x = TRUE, suffixes = c("", "_locality"))
    # # Append new variables.
    # vars <- c(vars,  list(locality = setdiff(names(dt), unlist(vars, use.names = FALSE))))

    # # ---- Import XXXXXXXXXXXX & merge ----
    # tmp <- get_()
    # if(remove_empty_vars){
    #     tmp <-  tmp[, which(unlist(lapply(tmp, function(x) !all(is.na(x))))), with = FALSE]
    # }
    # # Merge.
    # dt <- merge(dt, tmp, by = "LOCALITY_PID", all.x = TRUE, suffixes = c("", "_locality"))
    # # Append new variables.
    # vars <- c(vars,  list(locality = setdiff(names(dt), unlist(vars, use.names = FALSE))))

    # dt[tmp, on = 'a', bb := i.b]
    # system.time(dt[tmp, on = .(LOCALITY_PID), names(tmp) := mget(paste0("i.", names(tmp)))])


}

# build_gnaf()


# s <- get_street_locality()
# s_p <- get_street_locality_point()




                    
# address_alias                   ### ALIAS info ... nothing of detail, think it joins stuff ... more invest required.
# address_default_geocode         ### Lat / Longs of parcel
# *address_detail                  ### MAIN SPINE / MAIN INFO
# address_feature                 ####### LOOK INTO More, could be useful. Dunno why looks like it contains changes to an address 
# address_site_geocode            ### Gecoded ... not the same size of address site (300k missing from NSW/QLD)
# address_site                    ### Think this is the spine...
# locality_alias                  ### alias names, of interest for sure. ALso includes postcode (i guess sometimes)
# locality_neighbour              ### assume links bordering localities
# locality_point                  ### Lat lons of locality
# *locality                        ### Returns localities, looks to have other important stuff as well 
# street_locality_alias           ### gives different street name / street type alias, could be interesting
# street_locality_point           ### gives lat longs of the street. doesn't appear to be a centroid of the street.
# *street_locality                 ### Street Name / Street Type (Spelt out)
# *state                           ### Adding.



# address_mesh_block_2011         ########### ignoring mesh blocks for the moment
# address_mesh_block_2016         ########### ignoring mesh blocks for the moment
# mb_2011                         ########### ignoring mesh blocks for the moment
# mb_2016                         ########### ignoring mesh blocks for the moment

# Don't think we need the following
# primary_secondary               ########### Gives a tiny bit more info re primary (unit base address). Not required atm