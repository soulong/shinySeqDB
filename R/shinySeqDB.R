

#' run shinySeqDB
#'
#' @param port
#' @param default_dbPath
#'
#' @return NULL
#' @export
#'
#' @examples
#'
#'
runshinySeqDB <- function(port=5000, default_dbPath="~/Desktop/seqdb_hulab.rds") {

  source("R/ui.R", local=T)
  source("R/server.R", local=T)

  app <- shinyApp(ui, server)
  runApp(app, host="0.0.0.0", port=port)

}
