install.packages("devtools")

setwd("C:\\Users\\kyleh\\OneDrive\\Desktop\\gnaf_r")

# Create package
devtools::create('gnaf.r')

setwd("C:\\Users\\kyleh\\OneDrive\\Desktop\\gnaf_r")
devtools::document()


# Usage
library(gnaf.r)



require(data.table)
library(gnaf.r)
setup("C:\\temp\\G-NAF\\G-NAF FEBRUARY 2020", states = "qld")


g <- build_gnaf()

x <- get_locality_alias()
x[, gid := sequence(.N), by = LOCALITY_PID]

xx <- dcast(x, LOCALITY_PID ~ gid, value.var = c("NAME"))
gg <- merge(g, xx, by = "LOCALITY_PID", all.x = T)

vars <- paste0("loc_", names(gg)[names(gg) %like% "^\\d+$"])
names(gg)[names(gg) %like% "^\\d+$"] <- paste0("loc_", names(gg)[names(gg) %like% "^\\d+$"])

ggg <- melt(gg, measure = list(c("LOCALITY_NAME", vars)), value.name = c("LOCAL"), na.rm = TRUE)

zz <- merge(g, s, by = "PRINCIPAL_PID", all.x = T)

g[ADDRESS_DETAIL_PID %in% c("GAQLD720196666", "GAQLD158652172")]

address alias just offers the link between primary and alais (for a few reason)
