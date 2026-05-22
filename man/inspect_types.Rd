\name{inspect_types}
\alias{inspect_types}
\title{Summary and comparison of column types}
\usage{
inspect_types(df1, df2 = NULL, compare_index = FALSE)
}
\arguments{
\item{df1}{A data frame.}
\item{df2}{Optional second data frame for comparison.}
\item{compare_index}{Whether to check column positions as well as types when comparing data frames. Defaults to \code{FALSE}.}
}
\value{
A tibble summarising the count and percentage of different column types for one
or a pair of data frames. For a single data frame, the result contains
\code{type}, \code{cnt}, \code{pcnt}, and \code{col_name}. For two data frames,
the result contains \code{type}, \code{equal}, \code{cnt_1}, \code{cnt_2},
\code{columns}, and \code{issues}.
}
\description{
Summarise column types in a data frame. If two data frames are supplied, compare
column type composition and report missing or mismatched columns. Grouped data
frames are summarised separately for each group.
}
\examples{
inspect_types(iris)
show_plot(inspect_types(iris))
}
