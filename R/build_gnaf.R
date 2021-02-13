#' @name build_gnaf

#' @title Builds G-NAF into a flat `data.table` object.

#' @description Builds G-NAF into a flat `data.table` object. 


#' @param setup defining the default `gnaf_setup_data` object.

#' @param simple A logical argument to remove (potentially) unused, technical fields. Default is \code{FALSE}.

#' @param drop_empty_vars A logical argument to remove empty fields. Default is \code{TRUE}.

#' @param drop_date_variables A logical argument to remove date (creation / retire / modification) fields. Default is \code{TRUE}.

#' @param missing A character or NA to determine what empty (missing) observations should be. Default is \code{""}.

#' @param concatenate A logical argument to add determine if a full address string should be
#'        constructed. Default is \code{TRUE}.

#' @param add_geo_coordinate A logical argument to add geographical coordinate latitude and longitude fields. Default is \code{TRUE}.

#' @param add_meshblock_2011 A logical argument to Mesh Block 2011 fields. Default is \code{TRUE}.

#' @param add_meshblock_2016 A logical argument to Mesh Block 2016 fields. Default is \code{TRUE}.

#' @param add_locality_alias_variants A logical argument to include locality alias variants. Default is \code{FALSE}.

#' @param add_alias_links A logical argument to allow inclusion of primary id on alias addresses. Default is \code{FALSE}.

#' @param verbose A logical argument to verbose.  Default is \code{TRUE}.


#' @return Returns a flat `data.table` object of Australian (G-NAF) address information.


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update


#' @import data.table
#' @export
build_gnaf <- function(setup = gnaf_setup_data,
                       simple = FALSE,
                       drop_empty_vars = TRUE,
                       drop_date_variables = TRUE,
                       missing = "",
                       concatenate = TRUE,
                       add_geo_coordinate = TRUE,
                       add_meshblock_2011 = TRUE,
                       add_meshblock_2016 = TRUE,
                       add_locality_alias_variants = FALSE,
                       add_alias_links = FALSE,
                       verbose = TRUE){
    
    # ---- Import address detail ----
    dt <- get_address_detail()
    if(drop_empty_vars){
        dt <-  dt[, which(unlist(lapply(dt, function(x) !all(is.na(x))))), with = FALSE]
    }
    if(drop_date_variables){
        drop <- names(dt)[grepl("^DATE", names(dt))]
        set(dt, , drop, NULL)
    }
    # if(simple){
    #     drop <- c("", "", "", "", "", "", "", "", "", "", "")
    #     drop <- drop[drop %in% names(dt)]
    #     set(dt, , drop, NULL)
    # }

    # Start a list of unique variables on each dataset (as they merge).
    vars <- list(address_detail = names(dt))

    # ---- Import locality & merge ----
    tmp <- get_locality()
    if(drop_empty_vars){
        tmp <-  tmp[, which(unlist(lapply(tmp, function(x) !all(is.na(x))))), with = FALSE]
    }
    if(drop_date_variables){
        drop <- names(tmp)[grepl("^DATE", names(tmp))]
        set(tmp, , drop, NULL)
    }
    if(simple){
        drop <- c("PRIMARY_POSTCODE", "LOCALITY_CLASS_CODE", "GNAF_LOCALITY_PID", "GNAF_RELIABILITY_CODE")
        drop <- drop[drop %in% names(tmp)]
        set(tmp, , drop, NULL)
    }

    # Merge.
    dt <- merge(dt, tmp, by = "LOCALITY_PID", all.x = TRUE, suffixes = c("", "_locality"))
    # Append new variables.
    vars <- c(vars,  list(locality = setdiff(names(dt), unlist(vars, use.names = FALSE))))

    # ---- Import street locality & merge ----
    tmp <- get_street_locality()
    if(drop_empty_vars){
        tmp <-  tmp[, which(unlist(lapply(tmp, function(x) !all(is.na(x))))), with = FALSE]
    }
    if(drop_date_variables){
        drop <- names(tmp)[grepl("^DATE", names(tmp))]
        set(tmp, , drop, NULL)
    }
    if(simple){
        # NB "LOCALITY_PID" is required for Locality alias. It get's dropped later.
        drop <- c("STREET_CLASS_CODE", "GNAF_STREET_PID", "GNAF_STREET_CONFIDENCE", "GNAF_RELIABILITY_CODE")
        drop <- drop[drop %in% names(tmp)]
        set(tmp, , drop, NULL)
    }

    # Merge.
    dt <- merge(dt, tmp, by = "STREET_LOCALITY_PID", all.x = TRUE, suffixes = c("", "_street_locality"))
    # Can drop the following (as we have it already)
    if("LOCALITY_PID_street_locality" %in% colnames(dt)){
        set(dt, , "LOCALITY_PID_street_locality", NULL)
    }

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

    # ---- Geo coordinates ----
    if(add_geo_coordinate){
        tmp <- get_address_default_geocode()
        if(drop_empty_vars){
            tmp <-  tmp[, which(unlist(lapply(tmp, function(x) !all(is.na(x))))), with = FALSE]
        }
        if(drop_date_variables){
            drop <- names(tmp)[grepl("^DATE", names(tmp))]
            set(tmp, , drop, NULL)
        }
        if(simple){
            drop <- c("ADDRESS_DEFAULT_GEOCODE_PID", "GEOCODE_TYPE_CODE")
            drop <- drop[drop %in% names(tmp)]
            set(tmp, , drop, NULL)
        }
        # Merge.
        dt <- merge(dt, tmp, by = "ADDRESS_DETAIL_PID", all.x = TRUE, suffixes = c("", "_locality"))
        # Append new variables.
        vars <- c(vars,  list(locality = setdiff(names(dt), unlist(vars, use.names = FALSE))))
    }

    # ---- Mesh Block 2011 ----
    if(add_meshblock_2011){
        tmp <- get_address_mesh_block_2011()
        if(drop_empty_vars){
            tmp <-  tmp[, which(unlist(lapply(tmp, function(x) !all(is.na(x))))), with = FALSE]
        }
        if(drop_date_variables){
            drop <- names(tmp)[grepl("^DATE", names(tmp))]
            set(tmp, , drop, NULL)
        }
        if(simple){
            drop <- c("ADDRESS_MESH_BLOCK_2011_PID", "MB_MATCH_CODE")
            drop <- drop[drop %in% names(tmp)]
            set(tmp, , drop, NULL)
        }
        # We can derive the Mesh Block Code from the `MB_2011_PID` variable.
        tmp[, MB_2011_PID := gsub("^MB11", "", MB_2011_PID, perl = TRUE)]
        setnames(tmp, "MB_2011_PID", "MB_2011_CODE")

        # Merge.
        dt <- merge(dt, tmp, by = "ADDRESS_DETAIL_PID", all.x = TRUE, suffixes = c("", "_locality"))
        # Append new variables.
        vars <- c(vars,  list(locality = setdiff(names(dt), unlist(vars, use.names = FALSE))))
    }

    # ---- Mesh Block 2016 ----
    if(add_meshblock_2016){
        tmp <- get_address_mesh_block_2016()
        if(drop_empty_vars){
            tmp <-  tmp[, which(unlist(lapply(tmp, function(x) !all(is.na(x))))), with = FALSE]
        }
        if(drop_date_variables){
            drop <- names(tmp)[grepl("^DATE", names(tmp))]
            set(tmp, , drop, NULL)
        }
        if(simple){
            drop <- c("ADDRESS_MESH_BLOCK_2016_PID", "MB_MATCH_CODE")
            drop <- drop[drop %in% names(tmp)]
            set(tmp, , drop, NULL)
        }
        # We can derive the Mesh Block Code from the `MB_2016_PID` variable.
        tmp[, MB_2016_PID := gsub("^MB16", "", MB_2016_PID, perl = TRUE)]
        setnames(tmp, "MB_2016_PID", "MB_2016_CODE")

        # Merge.
        dt <- merge(dt, tmp, by = "ADDRESS_DETAIL_PID", all.x = TRUE, suffixes = c("", "_locality"))
        # Append new variables.
        vars <- c(vars,  list(locality = setdiff(names(dt), unlist(vars, use.names = FALSE))))
    }


    # ---- Authority code replacement ----
    if("STREET_SUFFIX_CODE" %in% names(dt)){
        dt$STREET_SUFFIX <- setup$ac_md$STREET_SUFFIX$NAME[match(dt$STREET_SUFFIX_CODE, setup$ac_md$STREET_SUFFIX$CODE)]
        set(dt, , "STREET_SUFFIX_CODE", NULL)
    }
    if("FLAT_TYPE_CODE" %in% names(dt)){
        dt$FLAT_TYPE <- setup$ac_md$FLAT_TYPE$NAME[match(dt$FLAT_TYPE_CODE, setup$ac_md$FLAT_TYPE$CODE)]
        set(dt, , "FLAT_TYPE_CODE", NULL)
    }
    if("LEVEL_TYPE_CODE" %in% names(dt)){
        dt$LEVEL_TYPE <- setup$ac_md$LEVEL_TYPE$NAME[match(dt$LEVEL_TYPE_CODE, setup$ac_md$LEVEL_TYPE$CODE)]
        set(dt, , "LEVEL_TYPE_CODE", NULL)
    }
    
    # Not interested in street_type, as the codes are the expanded format.
    setnames(dt, "STREET_TYPE_CODE", "STREET_TYPE")

    if("LOCALITY_CLASS_CODE" %in% names(dt)){
        dt$LOCALITY_CLASS <- setup$ac_md$LOCALITY_CLASS$NAME[match(dt$LOCALITY_CLASS_CODE, setup$ac_md$LOCALITY_CLASS$CODE)]
        set(dt, , "LOCALITY_CLASS_CODE", NULL)
    }

    if("STREET_CLASS_CODE" %in% names(dt)){
        dt$STREET_CLASS <- setup$ac_md$STREET_CLASS$NAME[match(dt$STREET_CLASS_CODE, setup$ac_md$STREET_CLASS$CODE)]
        set(dt, , "STREET_CLASS_CODE", NULL)
    }

    # If simple, remove any final vars we don't need.
    if(simple){
        # Again, except for "LOCALITY_PID", as we "may" need it in the following.
        drop <- c("STREET_LOCALITY_PID", "LEGAL_PARCEL_ID", "ADDRESS_SITE_PID", "GNAF_PROPERTY_PID", "PRIMARY_SECONDARY", "ALIAS_PRINCIPAL", "LEVEL_GEOCODED_CODE", "CONFIDENCE")
        drop <- drop[drop %in% names(dt)]
        set(dt, , drop, NULL)
    }

    if(add_alias_links){
        a <- get_address_alias()
        setnames(a, c("PRINCIPAL_PID", "ALIAS_PID"), c("PRINCIPAL_LINKED_PID", "ADDRESS_DETAIL_PID"))
        a <- a[, c("PRINCIPAL_LINKED_PID", "ADDRESS_DETAIL_PID")]
        dt <- merge(dt, a, by = "ADDRESS_DETAIL_PID", all.x = TRUE)
    }

    # NB, This section creates a lot of extra rows. Therefore, it should sit where it can benefit `simple = T` (memory mgmt).
    if(add_locality_alias_variants){
        # Localities can have alias', G-NAF captures these.
        # We can add variants, though, in saying so, depending on the jurisdiction it will make the data
        # much longer. E.g. for QLD, it expands ~3.2M records to 13M records. 
        
        # Get locality alias information.
        a <- get_locality_alias()

        # Create a group id sequence.
        a[, gid := sequence(.N), by = LOCALITY_PID]

        # Cast localities into wide format by the gid.
        a <- dcast(a, LOCALITY_PID ~ gid, value.var = c("NAME"))

        # Identify the alias variables (they're just numerics) and make them more indicative.
        vars <- paste0("LOCALITY_ALIAS_NAME_", names(a)[names(a) %like% "^\\d+$"])
        # Update names to be more indicative
        setnames(a, names(a)[names(a) %like% "^\\d+$"], vars)
        # Merge onto the G-NAF data.
        dt <- merge(dt, a, by = "LOCALITY_PID", all.x = T)
        # Melt the wide Alias' and the Correct Locality into 1 variable.
        dt <- melt(dt, measure = list(c("LOCALITY_NAME", vars)), value.name = c("LOCALITY_NAME"), na.rm = TRUE)

        # Identify alias vs principal localities.
        dt[, LOCALITY_ALIAS_PRINCIPAL := "A"]
        dt[variable == "LOCALITY_NAME", LOCALITY_ALIAS_PRINCIPAL := "P"]
        
        # Remove the `variable` column as it serves no purpose now.
        set(dt, , "variable", NULL)
    }

    # If simple, remove any final vars we don't need.
    if(simple){
        # Again, except for "LOCALITY_PID", as we "may" need it in the following.
        drop <- c("LOCALITY_PID")
        drop <- drop[drop %in% names(dt)]
        set(dt, , drop, NULL)
    }

    order_vars <- c("ADDRESS_DETAIL_PID", "BUILDING_NAME", "LOT_NUMBER", "FLAT_NUMBER_PREFIX",
                    "FLAT_TYPE",
                    "FLAT_NUMBER", "FLAT_NUMBER_SUFFIX",
                    "LEVEL_TYPE",
                    "LEVEL_NUMBER_PREFIX", "LEVEL_NUMBER", 
                    "NUMBER_FIRST_PREFIX", "NUMBER_FIRST", "NUMBER_FIRST_SUFFIX", "NUMBER_LAST", "NUMBER_LAST_SUFFIX", 
                    "STREET_NAME", "STREET_TYPE", "STREET_SUFFIX", 
                    "LOCALITY_NAME", "STATE_NAME", "POSTCODE",
                    "LONGITUDE", "LATITUDE", "MB_2011_CODE", "MB_2016_CODE")

    order_vars <- order_vars[order_vars %in% names(dt)]
    setcolorder(dt, order_vars)

    # If missing is character, replace NAs.
    if(missing == ""){
        gnaf.r:::char_na_to_empty(dt)
    }

    # If concatenate, concatenate address.
    if(concatenate){
        browser()

        # Units
        dt[FLAT_NUMBER_PREFIX != "" | FLAT_NUMBER != "" | FLAT_NUMBER_SUFFIX != "", address := paste(paste0(
                                    FLAT_NUMBER_PREFIX,
                                    FLAT_NUMBER,
                                    FLAT_NUMBER_SUFFIX),
                                paste0(
                                    NUMBER_FIRST,
                                    NUMBER_FIRST_SUFFIX,
                                    "-",
                                    NUMBER_LAST,
                                    NUMBER_LAST_SUFFIX
                                ),
                                STREET_NAME,
                                STREET_TYPE,
                                STREET_SUFFIX,
                                LOCALITY_NAME,
                                STATE_NAME,
                                POSTCODE
            )]
    
        # Non-units (street number)
        dt[FLAT_NUMBER_PREFIX == "" & FLAT_NUMBER == "" & FLAT_NUMBER_SUFFIX == "" & NUMBER_FIRST != "", address := paste(
                                paste0(
                                    NUMBER_FIRST,
                                    NUMBER_FIRST_SUFFIX,
                                    "-",
                                    NUMBER_LAST,
                                    NUMBER_LAST_SUFFIX
                                ),
                                STREET_NAME,
                                STREET_TYPE,
                                STREET_SUFFIX,
                                LOCALITY_NAME,
                                STATE_NAME,
                                POSTCODE
            )]

        # Non-units (lots)
        dt[FLAT_NUMBER_PREFIX == "" & FLAT_NUMBER == "" & FLAT_NUMBER_SUFFIX == "" & NUMBER_FIRST == "", address := paste(
                                "LOT",
                                LOT_NUMBER,
                                STREET_NAME,
                                STREET_TYPE,
                                STREET_SUFFIX,
                                LOCALITY_NAME,
                                STATE_NAME,
                                POSTCODE
            )]

        dt[, address := trimws(gsub("\\- ", " ", address, perl = TRUE))]
        dt[, address := gsub("  +", " ", address, perl = TRUE)]
    }

    return(dt[])
}
