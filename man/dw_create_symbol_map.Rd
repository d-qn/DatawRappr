% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dw_create_symbol_map.R
\name{dw_create_symbol_map}
\alias{dw_create_symbol_map}
\title{Creates a new Datawrapper Symbol Map}
\usage{
dw_create_symbol_map(
  basemap_id,
  lat_col,
  lon_col,
  color_col = "",
  size_col = "",
  shape = "",
  api_key = "environment",
  title = "",
  tooltip = list(title, body, fields = c()),
  folderId = ""
)
}
\arguments{
\item{basemap_id}{Required. Can be retrieved by looking at \code{\link{dw_basemaps}}.}

\item{lat_col}{Required. Which column contains the Latitude values to be plotted on the map?}

\item{lon_col}{Required. Which column contains the Latitude values to be plotted on the map?}

\item{color_col}{Optional. Which column contains the values that should determine the color.}

\item{size_col}{Optional. Which column contains the values that should determine the size.}

\item{shape}{Optional. Select a shape. Defaults to circle.}

\item{api_key}{Required. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.}

\item{title}{Optional. Will set a map's title on creation.}

\item{tooltip}{Optional. Specify a list including these vectors: \code{title}, \code{body}, \code{fields}. Include all used variables in fields. Use "{{ variable name }}" as placeholders in \code{title} and \code{body}.}

\item{folderId}{Optional. Creates chart in specified folder.}
}
\value{
It prints the new chart's id and returns a S3-structure of type \strong{dw_chart} with the elements from the Datawrapper-API, the same as in \code{\link{dw_retrieve_chart_metadata}}
}
\description{
\lifecycle{experimental}
Creates and returns a new Datawrapper Symbol map object. This function starts the map-making process
}
\note{
If not specified, the new chart will by default be created without a title.
}
\examples{

## Simple example:

\dontrun{

dw_create_symbol_map(
basemap_id = "world-2019",
basemap_value = "DW_STATE_CODE",
lat_col = "LAT",
lon_col = "LONG"
)

## Include a tooltip:

dw_create_choropleth_map(
basemap_id = "world-2019",
basemap_value = "DW_STATE_CODE",
value_cols = "percentage",
key_cols = "iso_codes",
tooltip = list(
title = "{{ State_Name }}",
body = "In {{ State_Name }} the value is {{percentage}} percent.",
fields = c("State_Name", "percentage")
))

}

}
\author{
Benedict Witzenberger
}
