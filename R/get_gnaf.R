#' @name get_gnaf

#' @title Download and unpack the latest version of G-NAF.

#' @description Convenience function for downloading and unpacking G-NAF


#' @param url A character vector identifying the root directory of where the G-NAF extract is located.

#' @param dest_folder A regular expression of which Australian State jurisdictions to import.  Default is \code{"./"}.

#' @param recursive If file paths should allow to be recursively created. Default is \code{TRUE}.

#' @param verbose A logical argument to determine if the function should be verbose or not. Default is \code{TRUE}.


#' @return Returns The latest version of G-NAF.


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @import data.table
#' @export

# For reference, following was the feb link:
# https://data.gov.au/data/dataset/19432f89-dc3a-4ef3-b943-5326ef1dbecc/resource/4b084096-65e4-4c8e-abbe-5e54ff85f42f/download/feb20_gnaf_pipeseparatedvalue.zip

get_gnaf <- function(url = "https://data.gov.au/data/dataset/19432f89-dc3a-4ef3-b943-5326ef1dbecc/resource/4b084096-65e4-4c8e-abbe-5e54ff85f42f/download/nov20_gnaf_pipeseparatedvalue.zip",
                    dest_folder = "./",
                    recursive = TRUE,
                    verbose = TRUE){

    if(Sys.info()['sysname'] == "Windows"){
        slash <- "\\"
    } else {
        slash <- "/"
    }


    dest_folder <-  normalizePath(dest_folder,  winslash = slash)
    dir.create(dest_folder, showWarnings = FALSE, recursive = recursive)
    if(!file.exists(dest_folder)){
        if(!recursive) stop("The destination folder (\"", dest_folder, "\") can't be created due to potential permission errors.")
        if(recursive) stop("The destination folder (\"", dest_folder, "\") can't be created. Retry this function and set the `recursive` argument to `TRUE`.")
    }

    dest_zip <- paste(normalizePath(dest_folder, winslash = slash), gsub(".*/", "", url), sep = slash)

    message("------------------")
    message("The download is approximately 1.5Gb, depending on your internet speed, the following may take a while.")
    message("The G-NAF zip file is currently being downloaded to: ", dest_zip)
    flush.console()
    download.file(url = url, destfile = dest_zip, quiet = TRUE)
    message("------------------")
    message("G-NAF has been download and is now uncompressing.")
    flush.console() 
    unzip(zipfile = dest_zip, exdir = dest_folder)

    possible_extracts <- list.files(paste0(dest_folder, slash, "G-NAF"))
    possible_extracts <- possible_extracts[grepl(paste0("^G-NAF ", paste0("(", paste0(toupper(month.name), collapse = "|"), ")"), " \\d{4}$"), possible_extracts, ignore.case = TRUE)]
    
    d <- data.table(extracts = possible_extracts, date = possible_extracts, full_paths = paste0(dest_folder, slash, "G-NAF", slash, possible_extracts))
    d[, date := as.character(as.Date(paste0("1 ", date), "%d G-NAF %B %Y"))]
    d <- d[order(date)]
    d[, full_paths := gsub("\\\\", "\\\\\\\\", full_paths)]

    message("------------------")
    if(verbose){
        if(nrow(d) == 1){
            message("You can now call the `setup()` to begin the initial setup of G-NAF. Be sure to toggle the `states` argument to only import relevant jurisdictions.\n")
            message("Example setup call: setup(dir = \"", d$full_paths, "\", states = \"tas|act\")")
        } else {
            message("Multiple extracts identified:\n")
            print(d)
            message("\nIf you want to setup the latest extract:")
            latest <- gsub("\\", "\\\\", d$full_paths[nrow(d)])
            message("\nExample setup call: setup(dir = \"", latest, "\", states = \"tas|act\")")
        }
    } else {
        message("Latest extract located: ", d$full_paths[nrow(d)])
    }
}
