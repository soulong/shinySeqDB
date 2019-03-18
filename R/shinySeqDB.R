

#' start shiny app
#'
#' @param port
#' the port number when you run the shiny app
#'
#' @param default_dbPath
#' the database file path when you lauch the shiny app,
#' if the file does not exist, the new database will be
#' created and file path will be the default_dbPath
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' runshinySeqDB(port=5000, default_dbPath="~/Desktop/seqdb_hulab.rds")
#'
runshinySeqDB <- function(port=5000, default_dbPath="~/Desktop/seqdb_hulab.rds") {

  print(getwd())
  source("R/global.R", local=T)
  source("R/ui.R", local=T)
  source("R/server.R", local=T)

  app <- shinyApp(ui, server)
  runApp(app, host="0.0.0.0", port=port)

}
