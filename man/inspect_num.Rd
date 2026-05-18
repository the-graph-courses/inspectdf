\name{inspect_num}
\alias{inspect_num}
\title{Summary and comparison of numeric columns}
\usage{
inspect_num(df1, df2 = NULL, breaks = 20, include_int = TRUE)
}
\arguments{
\item{df1}{A data frame.}
\item{df2}{Optional second data frame for comparing numeric distributions.}
\item{breaks}{Number of histogram breaks, or a named list of per-column breaks, passed to \code{hist()}.}
\item{include_int}{Whether integer columns should be treated as numeric. Defaults to \code{TRUE}.}
}
\value{
A tibble with numeric summaries and class \code{inspect_num}. For a single data
frame, the result contains summary statistics, \code{pcnt_na}, and a
\code{hist} list column whose entries contain \code{value} and \code{prop}.
For two data frames, the result contains \code{col_name}, \code{hist_1},
\code{hist_2}, \code{jsd}, and \code{pval}. Histogram breaks are stored in the
\code{brks_list} attribute.
}
\description{
Summarise numeric columns and prepare histogram data. If two data frames are
supplied, compare numeric columns present in either data frame. Grouped data
frames are summarised separately for each group.
}
\examples{
inspect_num(iris)
show_plot(inspect_num(iris))
}
