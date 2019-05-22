
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
