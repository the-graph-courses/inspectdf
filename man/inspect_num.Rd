\name{inspect_num}
\alias{inspect_num}
\title{Summarise numeric columns}
\usage{
inspect_num(df1, df2 = NULL, breaks = 20, include_int = TRUE)
}
\arguments{
\item{df1}{A data frame.}
\item{df2}{Optional second data frame for simple comparisons.}
\item{breaks}{Number of histogram breaks, passed to \code{hist()}.}
\item{include_int}{Whether integer columns should be treated as numeric.}
}
\value{
A tibble with numeric summaries and class \code{inspect_num}.
}
\description{
Summarise numeric columns and prepare histogram data. The result can be passed
to \code{show_plot()}.
}
\examples{
inspect_num(iris)
show_plot(inspect_num(iris))
}
