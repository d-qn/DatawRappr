% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dw_call_api.R
\name{dw_call_api}
\alias{dw_call_api}
\title{Calls the Datawrapper API, retrying when necessary, and validating the JSON response}
\usage{
dw_call_api(..., return_raw_response = F, enforce_json_response = T)
}
\value{
parsed JSON result
}
\description{
\lifecycle{experimental}
Light wrapper around httr::RETRY
}
