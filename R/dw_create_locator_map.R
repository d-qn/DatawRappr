#' Creates a new Datawrapper Locator Map
#'
#' \lifecycle{experimental}
#' This function creates a new Locator Map in Datawrapper using the Datawrapper API. This function starts the map-making process
#'
#' @param markers Optional. A list of markers with details such as latitude, longitude, and label.
#' @param options Optional. A list of map options (e.g., basemap, zoom level).
#' @param title Optional. Will set the locator map's title on creation
#' @param api_key API key for Datawrapper. If not provided, the function will use the key set with `dw_set_api_key()`.
#' @param folderId Optional. Creates chart in specified folder.
#'
#' @return It prints the new chart's id and returns a S3-structure of type \strong{dw_chart} with the elements from the Datawrapper-API, the same as in \code{\link{dw_retrieve_chart_metadata}}.
#' @author Duc Quang Nguyen
#' @note If not specified, the new chart will by default be created without a title.
#' @importFrom utils str
#' @examples
#' @export
dw_create_locator_map <- function(
    markers = list(), options = list(),
    title = NULL, folderId = NULL,
    api_key = "environment"
)
  {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }


  # Build call_body
  # required elements:

  call_body <- list(metadata = list())
  call_body <- rlist::list.append(call_body, type = "locator-map")

   # optional elements TO TEST !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if (length(markers) > 0 || length(options) > 0) {
    call_body$metadata <- list(
      locator_map = list(
        markers = markers,
        options = options
      )
    )
  }

  if (!is.null(title)) {call_body <- rlist::list.append(call_body, title = title)}
  if (!is.null(folderId)) {call_body <- rlist::list.append(call_body, folderId = folderId)}

  # Call to API:
  parsed <- dw_call_api("POST", "https://api.datawrapper.de/v3/charts", httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                        body = call_body, encode = "json", .DATAWRAPPR_UA)

  message(paste0("New maps's id: ", parsed[["id"]], "\n"))

  structure(
    list(
      content = parsed,
      path = "https://api.datawrapper.de/v3/charts",
      id = parsed[["id"]]
    ),
    class = "dw_chart"
  )
}

#' @export

print.dw_chart <- function(x, ...) {
  message("<Datawrapper ", x$path, ">\n", sep = "")
  message("Chart-ID: ", x$id, "\n", sep = "")
  str(x$content)
  invisible(x)
}
