#' Creates a new Datawrapper Chroropleth Map
#'
#' \lifecycle{experimental}
#' Creates and returns a new Datawrapper Chroropleth map object. This function starts the map-making process
#'
#' @param basemap_id Required. Can be retrieved by looking at \code{\link{dw_basemaps}}.
#' @param basemap_value Required. Can be retrieved by looking at \code{\link{dw_basemaps}}.
#' @param value_col Required. Which column contains the values to be plotted on the map?
#' @param keys_col Required. Which column contains the keys for the map (as specified in \code{\link{dw_basemaps}})?
#' @param api_key Required. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @param title Optional. Will set a map's title on creation.
#' @param tooltip Optional. Specify a list including these vectors: \code{title}, \code{body}, \code{fields}. Include all used variables in fields. Use "{{ variable name }}" as placeholders in \code{title} and \code{body}.
#' @param folderId Optional. Creates chart in specified folder.
#'
#' @return It prints the new chart's id and returns a S3-structure of type \strong{dw_chart} with the elements from the Datawrapper-API, the same as in \code{\link{dw_retrieve_chart_metadata}}.
#' @author Benedict Witzenberger
#' @note If not specified, the new chart will by default be created without a title.
#' @importFrom utils str
#' @examples
#'
#' ## Simple example:
#'
#' \dontrun{
#'
#' dw_create_choropleth_map(
#' basemap_id = "world-2019",
#' basemap_value = "DW_STATE_CODE",
#' value_cols = "percentage",
#' key_cols = "iso_codes"
#' )
#'
#' }
#'
#' ## Include a tooltip:
#'
#' \dontrun{
#'
#' dw_create_choropleth_map(
#' basemap_id = "world-2019",
#' basemap_value = "DW_STATE_CODE",
#' value_cols = "percentage",
#' key_cols = "iso_codes",
#' tooltip = list(
#' title = "{{ State_Name }}",
#' body = "In {{ State_Name }} the value is {{percentage}} percent.",
#' fields = c("State_Name", "percentage")
#' ))
#'
#' }
#'
#' @rdname dw_create_choropleth_map
#' @export
dw_create_choropleth_map <- function(basemap_id, basemap_value,
                                     values_col, keys_col, api_key = "environment",
                                     title = NULL, tooltip = list(title, body, fields = c()), folderId = NULL) {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  # Build call_body
  # required elements:

  call_body <- list(metadata = list())

  call_body <- rlist::list.append(call_body, type = "d3-maps-choropleth")

  call_body$metadata$axes <- list("keys" = keys_col, "values" = values_col)
  call_body$metadata$visualize <- list("basemap" = basemap_id, "map-key-attr" = basemap_value)

  # optional elements:
  if (length(tooltip) > 0) {
    call_body$metadata$visualize$tooltip <- list(
    body = tooltip$body,
    title = tooltip$title,
    fields = as.list(
      setNames(tooltip$fields, tooltip$fields)
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
