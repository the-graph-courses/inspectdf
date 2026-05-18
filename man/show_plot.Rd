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
\code{label_thresh}, \code{high_cardinality}, and \code{plot_layout}.}
}
\value{
A \code{ggplot} object.
}
\description{
Create ggplot summaries from \code{inspect_cat()} and \code{inspect_num()}
outputs.
}
\examples{
show_plot(inspect_cat(iris))
show_plot(inspect_num(iris))
}
