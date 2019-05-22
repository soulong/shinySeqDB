
#'
#' @param port run app on localhost, define a port, default port is 5000
#'
#' @param default_dbPath provide a .rda database, if no existing db, use can create a new db, and default_dbPath will be the saved db path
#'
#' @param admin_password admin password, used to delete datasets from existing db
#'
#' @export
runshinySeqDB <- function(port=5000, default_dbPath="~/Desktop/seqdb_hulab.rda", admin_password="delete") {

  appDir <- system.file("example", "app", package="shinySeqDB")

  if (appDir=="") {
    stop("Could not find example directory. Try re-installing `shinySeqDB`.", call.=F)
  }

  options(shiny.maxRequestSize=100*1024^2)
  default_dbPath <<- default_dbPath
  admin_password <<- admin_password

  shiny::runApp(appDir, host="0.0.0.0", port=port, display.mode="normal")
}
