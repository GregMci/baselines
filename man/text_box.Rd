% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_box.R
\name{text_box}
\alias{text_box}
\title{Draw text box over current device}
\usage{
text_box(
  x1 = 0.3,
  y1 = 0.1,
  x2 = 0.9,
  y2 = 0.4,
  text = NULL,
  cex = 1,
  font = 1,
  family = "",
  adj = 0,
  srt = 0,
  border = F,
  lty = 1,
  col_box = "grey97",
  col_text = "grey30",
  left_margin = 0.025,
  right_margin = 0.025,
  top_margin = 0.025,
  bottom_margin = 0.025,
  line_spacing = 0.05,
  vertical_alignment = "top",
  horizontal_alignment = "left",
  ...
)
}
\arguments{
\item{x1}{value for left, horizontal position in relation to device (in range 0:1)}

\item{y1}{value for bottom, vertical position in relation to device (in range 0:1)}

\item{x2}{value for right}

\item{y2}{value for top}

\item{text}{a character vector or expression specifying the text to be written with "Lorem ipsum..." default}

\item{cex}{text size, character expansion value (see par)}

\item{font}{default to the values of the global graphical parameters in par()}

\item{family}{default to the values of the global graphical parameters in par()}

\item{adj}{alignment of text (but see arguments below)}

\item{srt}{rotation (not currently implemented)}

\item{border}{logical or value for the colour of the box border}

\item{lty}{value for the line type, e.g. 1, 2, 3 or 4... or "22"}

\item{col_box}{fill colour of the box}

\item{col_text}{colour of the text}

\item{left_margin}{padding around text within box}

\item{right_margin}{padding around text within box}

\item{top_margin}{padding around text within box}

\item{bottom_margin}{padding around text within box}

\item{line_spacing}{padding between lines}

\item{vertical_alignment}{top, centre or bottom alignment}

\item{horizontal_alignment}{left, middle or right alignment}

\item{...}{other parameters, see global graphical parameters in par()}
}
\description{
Draw text box over current device
}
\examples{
text_box()

}
