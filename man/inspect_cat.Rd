\name{inspect_cat}
\alias{inspect_cat}
\title{Summary and comparison of the levels in categorical columns}
\usage{
inspect_cat(df1, df2 = NULL, include_int = FALSE)
}
\arguments{
\item{df1}{A data frame.}
\item{df2}{Optional second data frame for comparing categorical levels.}
\item{include_int}{Whether integer columns should be treated as categorical. Defaults to \code{FALSE}.}
}
\value{
A tibble summarising or comparing categorical features, with class
\code{inspect_cat}. For a single data frame, the result contains
\code{col_name}, \code{cnt}, \code{common}, \code{common_pcnt}, and a
\code{levels} list column whose entries contain \code{value}, \code{prop}, and
\code{cnt}. For two data frames, the result contains \code{col_name},
\code{jsd}, \code{pval}, \code{lvls_1}, and \code{lvls_2}.
}
\description{
Summarise character, factor, logical, date, and datetime columns. If two data
frames are supplied, compare categorical columns present in either data frame.
Grouped data frames are summarised separately for each group.
}
\examples{
inspect_cat(iris)
show_plot(inspect_cat(iris))
}
