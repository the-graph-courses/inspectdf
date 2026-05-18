\name{show_plot}
\alias{show_plot}
\alias{show_plot.inspect_cat}
\alias{show_plot.inspect_num}
\title{Plot inspection summaries}
\usage{
show_plot(x, ...)
}
\arguments{
\item{x}{Output from \code{inspect_cat()} or \code{inspect_num()}.}
\item{...}{Optional plotting arguments such as \code{text_labels},
\code{label_thresh}, \code{high_cardinality}, \code{plot_layout},
\code{col_palette}, and \code{alpha}.}
}
\value{
A \code{ggplot} object.
}
\description{
Create ggplot summaries from \code{inspect_cat()} and \code{inspect_num()}
outputs, including categorical bar plots, numeric histograms, numeric pair
heat plots, and grouped numeric heat plots.
}
\examples{
show_plot(inspect_cat(iris))
show_plot(inspect_num(iris))
}
