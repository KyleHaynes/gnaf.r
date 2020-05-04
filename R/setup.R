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
setup <- function(dir, states = NA, verbose = TRUE){
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
    # .pkgglobalenv <- new.env(parent=emptyenv())
    assign("gnaf_setup_data", g_naf_setup, envir=parent.frame())
    
    return(invisible(NULL))
    ##
    ## environment(iris) <- asNamespace('xxx')

}

