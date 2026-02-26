\name{launch_ggplot_studio}
\alias{launch_ggplot_studio}
\title{Launch ggplotStudioR Interactive Editor}
\usage{
launch_ggplot_studio(plot)
}
\arguments{
\item{plot}{A \code{ggplot} object to edit.}
}
\value{
Invisibly returns a \code{shiny.appobj}.
}
\description{
Open a Shiny-based MVP editor for a ggplot object.
}
\examples{
if (interactive()) {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point()
  launch_ggplot_studio(p)
}
}
