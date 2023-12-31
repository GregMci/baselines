% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overlay_margin.R
\name{overlay_margin}
\alias{overlay_margin}
\title{Overlay current margins on an active plot, visual query}
\usage{
overlay_margin()
}
\value{
overlay on the active plot showing margin region and margin lines, the plot region is shown with min/max data values as dashed rect
}
\description{
Overlay current margins on an active plot, visual query
}
\examples{
par( mfrow= c(3,3) )
plot_blank()
overlay_margin()
}
