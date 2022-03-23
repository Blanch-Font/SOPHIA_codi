#' Eunomia Connection
#'
#' Make a SQL connection to Eunomia database.
#'
#' @return SQL connection
#'
#' @examples
#' connection <- connect_Eunomia()
#' DatabaseConnector::querySql(connection, "SELECT COUNT(*) FROM person;")
#' DatabaseConnector::getTableNames(connection, databaseSchema = 'main')
connect_Eunomia <-  function(){
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  connection <- DatabaseConnector::connect(connectionDetails)
  return(connection)
}
