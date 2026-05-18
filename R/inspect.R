utils::globalVariables(c(
  "col_name", "cnt", "dataset", "label", "lower", "pcnt_plot", "upper", "value",
  "xmax", "xmin", "ymax"
))

inspect_cat <- function(df1, df2 = NULL, include_int = FALSE) {
  check_data_frame(df1, "df1")

  if (!is.null(df2)) {
    check_data_frame(df2, "df2")
    return(inspect_cat_compare(df1, df2, include_int = include_int))
  }

  cols <- names(df1)[vapply(df1, is_categorical, logical(1), include_int = include_int)]

  if (length(cols) == 0) {
    out <- tibble::tibble(
      col_name = character(),
      cnt = integer(),
      common = character(),
      common_pcnt = numeric(),
      levels = list()
    )
    return(new_inspect(out, "inspect_cat"))
  }

  summaries <- lapply(cols, function(col) cat_summary(df1[[col]]))

  out <- tibble::tibble(
    col_name = cols,
    cnt = vapply(summaries, function(x) nrow(x), integer(1)),
    common = vapply(summaries, common_value, character(1)),
    common_pcnt = vapply(summaries, common_percent, numeric(1)),
    levels = stats::setNames(summaries, cols)
  )

  new_inspect(out, "inspect_cat")
}

inspect_num <- function(df1, df2 = NULL, breaks = 20, include_int = TRUE) {
  check_data_frame(df1, "df1")

  if (!is.null(df2)) {
    check_data_frame(df2, "df2")
    return(inspect_num_compare(df1, df2, breaks = breaks, include_int = include_int))
  }

  cols <- names(df1)[vapply(df1, is_numeric_column, logical(1), include_int = include_int)]

  if (length(cols) == 0) {
    out <- tibble::tibble(
      col_name = character(),
      min = numeric(),
      q1 = numeric(),
      median = numeric(),
      mean = numeric(),
      q3 = numeric(),
      max = numeric(),
      sd = numeric(),
      pcnt_na = numeric(),
      hist = list()
    )
    return(new_inspect(out, "inspect_num"))
  }

  summaries <- lapply(cols, function(col) num_summary(df1[[col]], breaks = breaks))

  out <- tibble::tibble(
    col_name = cols,
    min = vapply(summaries, `[[`, numeric(1), "min"),
    q1 = vapply(summaries, `[[`, numeric(1), "q1"),
    median = vapply(summaries, `[[`, numeric(1), "median"),
    mean = vapply(summaries, `[[`, numeric(1), "mean"),
    q3 = vapply(summaries, `[[`, numeric(1), "q3"),
    max = vapply(summaries, `[[`, numeric(1), "max"),
    sd = vapply(summaries, `[[`, numeric(1), "sd"),
    pcnt_na = vapply(summaries, `[[`, numeric(1), "pcnt_na"),
    hist = stats::setNames(lapply(summaries, `[[`, "hist"), cols)
  )

  new_inspect(out, "inspect_num")
}

show_plot <- function(x, ...) {
  UseMethod("show_plot")
}

show_plot.default <- function(x, ...) {
  stop("show_plot() expects output from inspect_cat() or inspect_num().", call. = FALSE)
}

show_plot.inspect_cat <- function(x,
                                  text_labels = TRUE,
                                  high_cardinality = 0,
                                  label_thresh = 0.1,
                                  label_size = 3,
                                  label_color = "white",
                                  col_palette = 0,
                                  plot_layout = NULL,
                                  ...) {
  if ("levels" %in% names(x)) {
    return(plot_cat_single(
      x,
      text_labels = text_labels,
      high_cardinality = high_cardinality,
      label_thresh = label_thresh,
      label_size = label_size,
      label_color = label_color,
      col_palette = col_palette,
      plot_layout = plot_layout
    ))
  }

  plot_cat_compare(
    x,
    text_labels = text_labels,
    high_cardinality = high_cardinality,
    label_thresh = label_thresh,
    label_size = label_size,
    label_color = label_color,
    col_palette = col_palette,
    plot_layout = plot_layout
  )
}

show_plot.inspect_num <- function(x,
                                  text_labels = TRUE,
                                  label_thresh = 0.1,
                                  label_size = 2.7,
                                  label_color = "#222222",
                                  col_palette = 0,
                                  plot_layout = NULL,
                                  ...) {
  if ("hist" %in% names(x)) {
    return(plot_num_single(
      x,
      text_labels = text_labels,
      label_thresh = label_thresh,
      label_size = label_size,
      label_color = label_color,
      col_palette = col_palette,
      plot_layout = plot_layout
    ))
  }

  plot_num_compare(
    x,
    text_labels = text_labels,
    label_thresh = label_thresh,
    label_size = label_size,
    label_color = label_color,
    col_palette = col_palette,
    plot_layout = plot_layout
  )
}

check_data_frame <- function(df, arg) {
  if (!is.data.frame(df)) {
    stop(sprintf("%s must be a data frame.", arg), call. = FALSE)
  }
}

new_inspect <- function(x, class) {
  structure(x, class = c(class, class(x)))
}

is_categorical <- function(x, include_int = FALSE) {
  is.factor(x) ||
    is.character(x) ||
    is.logical(x) ||
    inherits(x, "Date") ||
    inherits(x, "POSIXt") ||
    (include_int && is.integer(x))
}

is_numeric_column <- function(x, include_int = TRUE) {
  is.numeric(x) &&
    !inherits(x, "Date") &&
    !inherits(x, "POSIXt") &&
    (include_int || !is.integer(x))
}

cat_summary <- function(x) {
  n <- length(x)
  if (n == 0) {
    return(tibble::tibble(value = character(), cnt = integer(), pcnt = numeric()))
  }

  values <- as.character(x)
  values[is.na(x)] <- NA_character_
  tab <- sort(table(values, useNA = "ifany"), decreasing = TRUE)
  level_names <- names(tab)
  level_names[is.na(level_names)] <- "NA"

  tibble::tibble(
    value = level_names,
    cnt = as.integer(tab),
    pcnt = as.numeric(tab) / n * 100
  )
}

common_value <- function(x) {
  if (nrow(x) == 0) {
    return(NA_character_)
  }
  x$value[[1]]
}

common_percent <- function(x) {
  if (nrow(x) == 0) {
    return(NA_real_)
  }
  x$pcnt[[1]]
}

num_summary <- function(x, breaks = 20) {
  non_missing <- x[!is.na(x)]

  if (length(non_missing) == 0) {
    stats <- c(
      min = NA_real_,
      q1 = NA_real_,
      median = NA_real_,
      mean = NA_real_,
      q3 = NA_real_,
      max = NA_real_,
      sd = NA_real_
    )
  } else {
    quantiles <- stats::quantile(non_missing, probs = c(0.25, 0.5, 0.75), names = FALSE)
    stats <- c(
      min = min(non_missing),
      q1 = quantiles[[1]],
      median = quantiles[[2]],
      mean = mean(non_missing),
      q3 = quantiles[[3]],
      max = max(non_missing),
      sd = stats::sd(non_missing)
    )
  }

  c(
    as.list(stats),
    pcnt_na = list(mean(is.na(x)) * 100),
    hist = list(hist_summary(x, breaks = breaks))
  )
}

hist_summary <- function(x, breaks = 20) {
  finite_x <- x[is.finite(x)]

  if (length(finite_x) == 0) {
    return(tibble::tibble(
      lower = numeric(),
      upper = numeric(),
      mid = numeric(),
      cnt = integer(),
      pcnt = numeric()
    ))
  }

  hist <- graphics::hist(finite_x, breaks = breaks, plot = FALSE)
  tibble::tibble(
    lower = utils::head(hist$breaks, -1),
    upper = utils::tail(hist$breaks, -1),
    mid = hist$mids,
    cnt = as.integer(hist$counts),
    pcnt = hist$counts / length(finite_x) * 100
  )
}

inspect_cat_compare <- function(df1, df2, include_int = FALSE) {
  cat1 <- names(df1)[vapply(df1, is_categorical, logical(1), include_int = include_int)]
  cat2 <- names(df2)[vapply(df2, is_categorical, logical(1), include_int = include_int)]
  cols <- intersect(cat1, cat2)

  if (length(cols) == 0) {
    out <- tibble::tibble(
      col_name = character(),
      jsd = numeric(),
      pval = numeric(),
      lvls_1 = list(),
      lvls_2 = list()
    )
    return(new_inspect(out, "inspect_cat"))
  }

  summaries_1 <- lapply(cols, function(col) cat_summary(df1[[col]]))
  summaries_2 <- lapply(cols, function(col) cat_summary(df2[[col]]))

  out <- tibble::tibble(
    col_name = cols,
    jsd = mapply(jsd_from_levels, summaries_1, summaries_2),
    pval = mapply(chisq_from_levels, summaries_1, summaries_2),
    lvls_1 = stats::setNames(summaries_1, cols),
    lvls_2 = stats::setNames(summaries_2, cols)
  )

  new_inspect(out, "inspect_cat")
}

inspect_num_compare <- function(df1, df2, breaks = 20, include_int = TRUE) {
  num1 <- names(df1)[vapply(df1, is_numeric_column, logical(1), include_int = include_int)]
  num2 <- names(df2)[vapply(df2, is_numeric_column, logical(1), include_int = include_int)]
  cols <- intersect(num1, num2)

  if (length(cols) == 0) {
    out <- tibble::tibble(
      col_name = character(),
      jsd = numeric(),
      pval = numeric(),
      hist_1 = list(),
      hist_2 = list()
    )
    return(new_inspect(out, "inspect_num"))
  }

  hist_1 <- vector("list", length(cols))
  hist_2 <- vector("list", length(cols))

  for (i in seq_along(cols)) {
    col <- cols[[i]]
    pair <- paired_hist_summary(df1[[col]], df2[[col]], breaks = breaks)
    hist_1[[i]] <- pair$hist_1
    hist_2[[i]] <- pair$hist_2
  }

  names(hist_1) <- cols
  names(hist_2) <- cols

  out <- tibble::tibble(
    col_name = cols,
    jsd = mapply(jsd_from_hist, hist_1, hist_2),
    pval = mapply(chisq_from_hist, hist_1, hist_2),
    hist_1 = hist_1,
    hist_2 = hist_2
  )

  new_inspect(out, "inspect_num")
}

paired_hist_summary <- function(x, y, breaks = 20) {
  finite_x <- x[is.finite(x)]
  finite_y <- y[is.finite(y)]
  combined <- c(finite_x, finite_y)

  if (length(combined) == 0) {
    empty <- tibble::tibble(
      lower = numeric(),
      upper = numeric(),
      mid = numeric(),
      cnt = integer(),
      pcnt = numeric()
    )
    return(list(hist_1 = empty, hist_2 = empty))
  }

  breaks_vec <- graphics::hist(combined, breaks = breaks, plot = FALSE)$breaks
  list(
    hist_1 = hist_summary_with_breaks(finite_x, breaks_vec),
    hist_2 = hist_summary_with_breaks(finite_y, breaks_vec)
  )
}

hist_summary_with_breaks <- function(x, breaks) {
  if (length(x) == 0) {
    return(tibble::tibble(
      lower = utils::head(breaks, -1),
      upper = utils::tail(breaks, -1),
      mid = utils::head(breaks, -1) + diff(breaks) / 2,
      cnt = integer(length(breaks) - 1),
      pcnt = numeric(length(breaks) - 1)
    ))
  }

  hist <- graphics::hist(x, breaks = breaks, plot = FALSE)
  tibble::tibble(
    lower = utils::head(hist$breaks, -1),
    upper = utils::tail(hist$breaks, -1),
    mid = hist$mids,
    cnt = as.integer(hist$counts),
    pcnt = hist$counts / length(x) * 100
  )
}

jsd_from_levels <- function(x, y) {
  all_levels <- union(x$value, y$value)
  p <- level_probs(x, all_levels)
  q <- level_probs(y, all_levels)
  jsd(p, q)
}

chisq_from_levels <- function(x, y) {
  all_levels <- union(x$value, y$value)
  counts <- rbind(level_counts(x, all_levels), level_counts(y, all_levels))
  chisq_pvalue(counts)
}

jsd_from_hist <- function(x, y) {
  jsd(x$pcnt / 100, y$pcnt / 100)
}

chisq_from_hist <- function(x, y) {
  chisq_pvalue(rbind(x$cnt, y$cnt))
}

level_probs <- function(x, levels) {
  counts <- level_counts(x, levels)
  if (sum(counts) == 0) {
    return(rep(0, length(levels)))
  }
  counts / sum(counts)
}

level_counts <- function(x, levels) {
  counts <- stats::setNames(x$cnt, x$value)
  out <- counts[levels]
  out[is.na(out)] <- 0
  as.numeric(out)
}

jsd <- function(p, q) {
  if (length(p) == 0 || length(q) == 0 || sum(p) == 0 || sum(q) == 0) {
    return(NA_real_)
  }

  p <- p / sum(p)
  q <- q / sum(q)
  m <- (p + q) / 2
  (kl_divergence(p, m) + kl_divergence(q, m)) / 2 / log(2)
}

kl_divergence <- function(p, q) {
  keep <- p > 0 & q > 0
  sum(p[keep] * log(p[keep] / q[keep]))
}

chisq_pvalue <- function(counts) {
  if (ncol(counts) < 2 || any(rowSums(counts) == 0) || sum(counts) == 0) {
    return(NA_real_)
  }

  suppressWarnings(stats::chisq.test(counts)$p.value)
}

collapse_rare_levels <- function(levels, high_cardinality = 0) {
  if (high_cardinality <= 0 || nrow(levels) == 0) {
    return(levels)
  }

  keep <- levels$cnt >= high_cardinality
  if (all(keep)) {
    return(levels)
  }

  kept <- levels[keep, , drop = FALSE]
  other <- tibble::tibble(
    value = "(other)",
    cnt = sum(levels$cnt[!keep]),
    pcnt = sum(levels$pcnt[!keep])
  )

  kept <- rbind(kept, other)
  kept[order(kept$cnt, decreasing = TRUE), , drop = FALSE]
}

plot_cat_single <- function(x,
                            text_labels = TRUE,
                            high_cardinality = 0,
                            label_thresh = 0.1,
                            label_size = 3,
                            label_color = "white",
                            col_palette = 0,
                            plot_layout = NULL) {
  plot_data <- bind_plot_parts(seq_len(nrow(x)), function(i) {
    levels <- collapse_rare_levels(x$levels[[i]], high_cardinality = high_cardinality)
    if (nrow(levels) == 0) {
      return(NULL)
    }
    levels$col_name <- x$col_name[[i]]
    levels
  })

  if (nrow(plot_data) == 0) {
    return(empty_plot("No categorical columns to plot"))
  }

  plot_data$pcnt_plot <- plot_data$pcnt / 100
  plot_data$label <- ifelse(
    plot_data$pcnt_plot >= label_thresh,
    paste0(plot_data$value, "\n", percent_label(plot_data$pcnt_plot)),
    ""
  )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = col_name, y = pcnt_plot, fill = value)) +
    ggplot2::geom_col(width = 0.72, color = "white", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = percent_label, expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = NULL, y = "Percent of rows", fill = NULL) +
    inspect_theme()

  if (text_labels) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        position = ggplot2::position_stack(vjust = 0.5),
        size = label_size,
        color = label_color,
        lineheight = 0.9,
        check_overlap = TRUE
      )
  }

  apply_palette(p, plot_data$value, col_palette) +
    ggplot2::theme(legend.position = "none")
}

plot_cat_compare <- function(x,
                             text_labels = TRUE,
                             high_cardinality = 0,
                             label_thresh = 0.1,
                             label_size = 3,
                             label_color = "white",
                             col_palette = 0,
                             plot_layout = NULL) {
  plot_data <- bind_plot_parts(seq_len(nrow(x)), function(i) {
    one <- collapse_rare_levels(x$lvls_1[[i]], high_cardinality = high_cardinality)
    two <- collapse_rare_levels(x$lvls_2[[i]], high_cardinality = high_cardinality)
    one$dataset <- "df1"
    two$dataset <- "df2"
    both <- rbind(one, two)
    both$col_name <- x$col_name[[i]]
    both
  })

  if (nrow(plot_data) == 0) {
    return(empty_plot("No categorical columns to plot"))
  }

  plot_data$pcnt_plot <- plot_data$pcnt / 100
  plot_data$label <- ifelse(
    plot_data$pcnt_plot >= label_thresh,
    paste0(plot_data$value, "\n", percent_label(plot_data$pcnt_plot)),
    ""
  )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = dataset, y = pcnt_plot, fill = value)) +
    ggplot2::geom_col(width = 0.72, color = "white", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ col_name"), ncol = facet_ncol(plot_layout)) +
    ggplot2::scale_y_continuous(labels = percent_label, expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = NULL, y = "Percent of rows", fill = NULL) +
    inspect_theme()

  if (text_labels) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        position = ggplot2::position_stack(vjust = 0.5),
        size = label_size,
        color = label_color,
        lineheight = 0.9,
        check_overlap = TRUE
      )
  }

  apply_palette(p, plot_data$value, col_palette) +
    ggplot2::theme(legend.position = "none")
}

plot_num_single <- function(x,
                            text_labels = TRUE,
                            label_thresh = 0.1,
                            label_size = 2.7,
                            label_color = "#222222",
                            col_palette = 0,
                            plot_layout = NULL) {
  plot_data <- bind_plot_parts(seq_len(nrow(x)), function(i) {
    hist <- x$hist[[i]]
    if (nrow(hist) == 0) {
      return(NULL)
    }
    hist$col_name <- x$col_name[[i]]
    hist
  })

  if (nrow(plot_data) == 0) {
    return(empty_plot("No numeric values to plot"))
  }

  plot_data$pcnt_plot <- plot_data$pcnt / 100
  plot_data$label <- ifelse(
    plot_data$pcnt_plot >= label_thresh,
    percent_label(plot_data$pcnt_plot),
    ""
  )

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = lower, xmax = upper, ymin = 0, ymax = pcnt_plot),
      fill = palette_for(col_palette, 1)[[1]],
      color = "white",
      linewidth = 0.2
    ) +
    ggplot2::facet_wrap(stats::as.formula("~ col_name"), scales = "free_x", ncol = facet_ncol(plot_layout)) +
    ggplot2::scale_y_continuous(labels = percent_label, expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Percent of rows") +
    inspect_theme()

  if (text_labels) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(x = (lower + upper) / 2, y = pcnt_plot, label = label),
        vjust = -0.25,
        size = label_size,
        color = label_color,
        check_overlap = TRUE
      )
  }

  p
}

plot_num_compare <- function(x,
                             text_labels = TRUE,
                             label_thresh = 0.1,
                             label_size = 2.7,
                             label_color = "#222222",
                             col_palette = 0,
                             plot_layout = NULL) {
  plot_data <- bind_plot_parts(seq_len(nrow(x)), function(i) {
    one <- x$hist_1[[i]]
    two <- x$hist_2[[i]]
    one$dataset <- "df1"
    two$dataset <- "df2"
    both <- rbind(one, two)
    both$col_name <- x$col_name[[i]]
    both
  })

  if (nrow(plot_data) == 0) {
    return(empty_plot("No numeric values to plot"))
  }

  plot_data$pcnt_plot <- plot_data$pcnt / 100
  plot_data$label <- ifelse(
    plot_data$pcnt_plot >= label_thresh,
    percent_label(plot_data$pcnt_plot),
    ""
  )

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = lower, xmax = upper, ymin = 0, ymax = pcnt_plot, fill = dataset),
      alpha = 0.5,
      color = "white",
      linewidth = 0.2
    ) +
    ggplot2::facet_wrap(stats::as.formula("~ col_name"), scales = "free_x", ncol = facet_ncol(plot_layout)) +
    ggplot2::scale_y_continuous(labels = percent_label, expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::scale_fill_manual(values = palette_for(col_palette, 2)) +
    ggplot2::labs(x = NULL, y = "Percent of rows", fill = NULL) +
    inspect_theme()

  if (text_labels) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(x = (lower + upper) / 2, y = pcnt_plot, label = label),
        vjust = -0.25,
        size = label_size,
        color = label_color,
        check_overlap = TRUE
      )
  }

  p
}

bind_plot_parts <- function(index, fn) {
  parts <- lapply(index, fn)
  parts <- Filter(Negate(is.null), parts)

  if (length(parts) == 0) {
    return(tibble::tibble())
  }

  tibble::as_tibble(do.call(rbind, parts))
}

empty_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, size = 4) +
    ggplot2::theme_void()
}

inspect_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      plot.title.position = "plot"
    )
}

percent_label <- function(x) {
  paste0(round(x * 100), "%")
}

facet_ncol <- function(plot_layout = NULL) {
  if (is.null(plot_layout) || length(plot_layout) < 2) {
    return(NULL)
  }
  plot_layout[[2]]
}

apply_palette <- function(plot, values, col_palette = 0) {
  unique_values <- unique(values)
  plot + ggplot2::scale_fill_manual(values = palette_for(col_palette, length(unique_values)))
}

palette_for <- function(col_palette = 0, n = 1) {
  palettes <- list(
    c("#3b82f6", "#ef4444", "#22c55e", "#f59e0b", "#8b5cf6", "#06b6d4", "#f97316", "#84cc16"),
    c("#0072b2", "#d55e00", "#009e73", "#cc79a7", "#f0e442", "#56b4e9", "#e69f00", "#000000"),
    c("#ff6f59", "#254441", "#43aa8b", "#b2b09b", "#ef3054", "#4d9de0", "#e1bc29", "#7768ae")
  )

  palette <- palettes[[as.integer(col_palette) + 1]]
  if (is.null(palette)) {
    palette <- palettes[[1]]
  }

  rep(palette, length.out = n)
}
