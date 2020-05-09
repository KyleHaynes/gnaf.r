#' @name get_street_locality

#' @title Returns a list of street names.

#' @description Returns a list of street names.


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update


#' @import data.table
#' @export
# get street_locality
get_street_locality <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_STREET_LOCALITY_psv"]$full_paths

    # fread and bind together as a single data.table.
    street_locality <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(street_locality)
}

################################################################################
################################################################################
################################################################################

#' @name get_street_locality_point

#' @title Returns a list of street points.

#' @description Returns a list of street points. 


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
# get street_locality_point
get_street_locality_point <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_STREET_LOCALITY_POINT_psv"]$full_paths

    # fread and bind together as a single data.table.
    street_locality_point <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(street_locality_point)
}

################################################################################
################################################################################
################################################################################

#' @name get_street_locality_alias

#' @title Returns a list of locality alias'.

#' @description Returns a list of locality alias'. 


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
# get street_locality_point
# get street_locality_alias
get_street_locality_alias <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_STREET_LOCALITY_ALIAS_psv"]$full_paths

    # fread and bind together as a single data.table.
    street_locality_alias <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(street_locality_alias)
}

################################################################################
################################################################################
################################################################################

#' @name get_primary_secondary

#' @title Returns a list of primary and secondary connections.

#' @description Returns a list of primary and secondary connections. 


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
# get street_locality_point
# get street_locality_alias
get_primary_secondary <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_primary_secondary_psv"]$full_paths

    # fread and bind together as a single data.table.
    primary_secondary <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(primary_secondary)
}

################################################################################
################################################################################
################################################################################

#' @name get_locality

#' @title Returns a list of locality names.

#' @description Returns a list of locality names. 


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_locality <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_locality_psv"]$full_paths
    paths <- paths[!paths %ilike% "_STREET_"]

    # fread and bind together as a single data.table.
    locality <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(locality)
}

################################################################################
################################################################################
################################################################################

#' @name get_locality_point

#' @title Returns a list of locality points.

#' @description Returns a list of locality points. 


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_locality_point <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_locality_point_psv"]$full_paths
    paths <- paths[!paths %ilike% "_STREET_"]

    # fread and bind together as a single data.table.
    locality_point <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(locality_point)
}

################################################################################
################################################################################
################################################################################

#' @name get_locality_neighbour

#' @title Returns a list of locality neighbours.

#' @description Returns a list of locality points. 


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_locality_neighbour <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_locality_neighbour_psv"]$full_paths
    paths <- paths[!paths %ilike% "_STREET_"]

    # fread and bind together as a single data.table.
    locality_neighbour <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(locality_neighbour)
}

################################################################################
################################################################################
################################################################################

#' @name get_locality_alias

#' @title Returns a list of locality alias points.

#' @description Returns a list of locality alias points. 


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_locality_alias <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_locality_alias_psv"]$full_paths
    paths <- paths[!paths %ilike% "_STREET_"]

    # fread and bind together as a single data.table.
    locality_alias <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(locality_alias)
}

################################################################################
################################################################################
################################################################################

#' @name get_address_site

#' @title Returns the address site information.

#' @description Returns the address site information. 


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_address_site <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_site_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_site <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(address_site)
}

################################################################################
################################################################################
################################################################################

#' @name get_address_site_geocode

#' @title Returns the geocoded address site information.

#' @description Returns the geocoded address site information.


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_address_site_geocode <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_site_geocode_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_site_geocode <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(address_site_geocode)
}

################################################################################
################################################################################
################################################################################

#' @name get_address_feature

#' @title Returns the geocoded address site information.

#' @description Returns the geocoded address site information.


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_address_feature <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_feature_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_feature <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(address_feature)
}

################################################################################
################################################################################
################################################################################

#' @name get_address_detail

#' @title Returns the address site detail information.

#' @description Returns the address site detail information.


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_address_detail <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_detail_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_detail <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(address_detail)
}

################################################################################
################################################################################
################################################################################

#' @name get_address_default_geocode

#' @title Returns the address_default_geocode information.

#' @description Returns address_default_geocode information.


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_address_default_geocode <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_default_geocode_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_default_geocode <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(address_default_geocode)
}

################################################################################
################################################################################
################################################################################

#' @name get_address_alias

#' @title Returns the address_alias information.

#' @description Returns address_alias information.


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_address_alias <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_address_alias_psv"]$full_paths


    # fread and bind together as a single data.table.
    address_alias <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(address_alias)
}

################################################################################
################################################################################
################################################################################

#' @name get_state

#' @title Returns the get_state information.

#' @description Returns state information.


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_state <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_state_psv"]$full_paths

    # fread and bind together as a single data.table.
    state <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Creation date doesn't matter, not does retire state.
    state[, DATE_CREATED := NULL]
    state[, DATE_RETIRED := NULL]
    # Don't need an expanded and abbreviation. 
    state[, STATE_ABBREVIATION := NULL]

    # Return the object
    return(state[])
}

################################################################################
################################################################################
################################################################################

#' @name get_address_mesh_block_2011

#' @title Returns address_mesh_block_2011 information.

#' @description Returns address_mesh_block_2011 information.


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_address_mesh_block_2011 <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_ADDRESS_MESH_BLOCK_2011_psv"]$full_paths

    # fread and bind together as a single data.table.
    address_mesh_block_2011 <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(address_mesh_block_2011[])
}

################################################################################
################################################################################
################################################################################

#' @name get_address_mesh_block_2016

#' @title Returns address_mesh_block_2016 information.

#' @description Returns address_mesh_block_2016 information.


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_address_mesh_block_2016 <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_ADDRESS_MESH_BLOCK_2016_psv"]$full_paths

    # fread and bind together as a single data.table.
    address_mesh_block_2016 <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(address_mesh_block_2016[])
}

################################################################################
################################################################################
################################################################################

#' @name get_mb_2011

#' @title Returns mb_2011 information.

#' @description Returns mb_2011 information.


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_mb_2011 <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_mb_2011_psv"]$full_paths

    # fread and bind together as a single data.table.
    mb_2011 <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(mb_2011[])
}

################################################################################
################################################################################
################################################################################

#' @name get_mb_2016

#' @title Returns mb_2016 information.

#' @description Returns mb_2016 information.


#' @param setup A character vector identifying the setup object run by calling `setup()`.

#' @return Returns ***UPDATE***


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @examples
#' # Update
#' @export
get_mb_2016 <- function(setup = gnaf_setup_data){
    
    # Return the scheme from the setup object.
    dt <- setup$schema

    # Subset to relevant files.
    paths <- dt[full_paths %ilike% "_mb_2016_psv"]$full_paths

    # fread and bind together as a single data.table.
    mb_2016 <- rbindlist(lapply(paths, fread, colClasses = "character", na.strings = ""))

    # Return the object
    return(mb_2016[])
}


                    
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