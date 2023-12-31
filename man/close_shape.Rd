% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/close_shape.R
\name{close_shape}
\alias{close_shape}
\title{Add start coordinates as end coordinates to close a shape}
\usage{
close_shape(my_shape)
}
\arguments{
\item{my_shape}{values for the coordinates of the shape (x,y)}
}
\description{
Add start coordinates as end coordinates to close a shape
}
\examples{
par( mfrow=c(1,2))
plot( arc_points(arc_length = 270 ), type='l', asp=1)
plot( close_shape( arc_points(arc_length = 270 ) ), type='l', asp=1 )
}
