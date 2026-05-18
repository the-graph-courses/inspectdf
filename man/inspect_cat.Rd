\name{inspect_cat}
\alias{inspect_cat}
\title{Summarise categorical columns}
\usage{
inspect_cat(df1, df2 = NULL, include_int = FALSE)
}
\arguments{
\item{df1}{A data frame.}
\item{df2}{Optional second data frame for simple comparisons.}
\item{include_int}{Whether integer columns should be treated as categorical.}
}
\value{
A tibble with categorical summaries and class \code{inspect_cat}.
}
\description{
Summarise character, factor, logical, date, and datetime columns. The result can
be passed to \code{show_plot()}.
}
\examples{
inspect_cat(iris)
show_plot(inspect_cat(iris))
}
