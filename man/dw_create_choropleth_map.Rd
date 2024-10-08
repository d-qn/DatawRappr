% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dw_create_choropleth_map.R
\name{dw_create_choropleth_map}
\alias{dw_create_choropleth_map}
\title{Creates a new Datawrapper Chroropleth Map}
\usage{
dw_create_choropleth_map(
  basemap_id,
  basemap_value,
  values_col,
  keys_col,
  api_key = "environment",
  title = NULL,
  tooltip = list(title, body, fields = c()),
  folderId = NULL
)
}
\arguments{
\item{basemap_id}{Required. Can be retrieved by looking at \code{\link{dw_basemaps}}.}

\item{basemap_value}{Required. Can be retrieved by looking at \code{\link{dw_basemaps}}.}

\item{keys_col}{Required. Which column contains the keys for the map (as specified in \code{\link{dw_basemaps}})?}

\item{api_key}{Required. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.}

\item{title}{Optional. Will set a map's title on creation.}

\item{tooltip}{Optional. Specify a list including these vectors: \code{title}, \code{body}, \code{fields}. Include all used variables in fields. Use "{{ variable name }}" as placeholders in \code{title} and \code{body}.}

\item{folderId}{Optional. Creates chart in specified folder.}

\item{value_col}{Required. Which column contains the values to be plotted on the map?}
}
\value{
It prints the new chart's id and returns a S3-structure of type \strong{dw_chart} with the elements from the Datawrapper-API, the same as in \code{\link{dw_retrieve_chart_metadata}}.
}
\description{
\lifecycle{experimental}
Creates and returns a new Datawrapper Chroropleth map object. This function starts the map-making process
}
\note{
If not specified, the new chart will by default be created without a title.
}
\examples{

## Simple example:

\dontrun{

dw_create_choropleth_map(
basemap_id = "world-2019",
basemap_value = "DW_STATE_CODE",
value_cols = "percentage",
key_cols = "iso_codes"
)

}

## Include a tooltip:

\dontrun{

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
