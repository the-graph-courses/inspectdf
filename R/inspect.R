utils::globalVariables(c(
  "bar_name", "bin", "col_name", "dataset", "fill_key", "group_label",
  "label", "lower", "mid", "pcnt_plot", "prop", "prop_z", "upper", "value",
  "xmax", "xmin", "ymax"
))

inspect_cat <- function(df1, df2 = NULL, include_int = FALSE) {
  input_type <- check_df_cols(df1, df2)
  df_names <- df_name_attrs(!is.null(df2))

  if (input_type == "pair") {
    out <- inspect_cat_compare(df1, df2, include_int = include_int)
  } else if (input_type == "grouped") {
    out <- inspect_cat_grouped(df1, include_int = include_int)
  } else {
    out <- inspect_cat_single(df1, include_int = include_int)
  }

  new_inspect(out, "inspect_cat", method = "cat", input_type = input_type, df_names = df_names)
}

inspect_num <- function(df1, df2 = NULL, breaks = 20, include_int = TRUE) {
  input_type <- check_df_cols(df1, df2)
  df_names <- df_name_attrs(!is.null(df2))

  if (input_type == "pair") {
    res <- inspect_num_compare(df1, df2, breaks = breaks, include_int = include_int)
    out <- res$out
  } else if (input_type == "grouped") {
    res <- inspect_num_grouped(df1, breaks = breaks, include_int = include_int)
    out <- res$out
  } else {
    res <- inspect_num_single(df1, breaks = breaks, include_int = include_int)
    out <- res$out
  }

  new_inspect(
    out,
    "inspect_num",
    method = "num",
    input_type = input_type,
    df_names = df_names,
    brks_list = res$brks_list,
    inspected = res$inspected,
    group_lengths = res$group_lengths
  )
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
                                  label_color = NULL,
                                  col_palette = 0,
                                  plot_layout = NULL,
                                  ...) {
  input_type <- attr(x, "type")$input_type
  if (identical(input_type, "grouped")) {
    stop("Grouped comparison plots for inspect_cat() are not implemented upstream.", call. = FALSE)
  }

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
                                  alpha = 0.05,
                                  ...) {
  input_type <- attr(x, "type")$input_type
  if (identical(input_type, "grouped")) {
    return(plot_num_grouped(
      x,
      text_labels = text_labels,
      label_size = label_size,
      label_color = label_color,
      col_palette = col_palette,
      plot_layout = plot_layout
    ))
  }

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
    label_size = label_size,
    label_color = label_color,
    col_palette = col_palette,
    plot_layout = plot_layout,
    alpha = alpha
  )
}

check_df_cols <- function(df1, df2 = NULL) {
  if (inherits(df1, "grouped_df") && inherits(df2, "grouped_df")) {
    stop(
      "Both dataframes seem to be grouped: when comparing dataframes, only ungrouped are supported at present",
      call. = FALSE
    )
  }
  if (!is.data.frame(df1)) {
    stop("df1 is not a data.frame!", call. = FALSE)
  }
  if (ncol(df1) == 0) {
    stop("df1 seems to have no columns!", call. = FALSE)
  }
  if (!is.null(df2)) {
    if (!is.data.frame(df2)) {
      stop("df2 is not a data.frame!", call. = FALSE)
    }
    if (ncol(df2) == 0) {
      stop("df2 seems to have no columns!", call. = FALSE)
    }
  }

  if (is.null(df2) && inherits(df1, "grouped_df")) {
    "grouped"
  } else if (is.null(df2)) {
    "single"
  } else {
    "pair"
  }
}

df_name_attrs <- function(has_df2 = FALSE) {
  list(df1 = "df1", df2 = if (has_df2) "df2" else NULL)
}

new_inspect <- function(x,
                        class,
                        method,
                        input_type,
                        df_names,
                        brks_list = NULL,
                        inspected = NULL,
                        group_lengths = NULL) {
  attr(x, "type") <- list(method = method, input_type = input_type)
  attr(x, "df_names") <- df_names
  if (!is.null(brks_list)) attr(x, "brks_list") <- brks_list
  if (!is.null(inspected)) attr(x, "inspected") <- inspected
  if (!is.null(group_lengths)) attr(x, "group_lengths") <- group_lengths
  structure(x, class = c(class, setdiff(class(x), class)))
}

is_categorical <- function(x, include_int = FALSE) {
  is.factor(x) ||
    is.character(x) ||
    is.logical(x) ||
    inherits(x, "Date") ||
    inherits(x, "POSIXt") ||
    inherits(x, "datetime") ||
    (include_int && is.integer(x))
}

is_numeric_column <- function(x, include_int = TRUE) {
  is.numeric(x) &&
    !inherits(x, "Date") &&
    !inherits(x, "POSIXt") &&
    (include_int || is.double(x))
}

empty_cat_summary <- function() {
  tibble::tibble(
    col_name = character(),
    cnt = integer(),
    common = character(),
    common_pcnt = numeric(),
    levels = list()
  )
}

empty_num_summary <- function() {
  tibble::tibble(
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
}

inspect_cat_single <- function(df1, include_int = FALSE) {
  df1 <- as.data.frame(df1)
  cols <- names(df1)[vapply(df1, is_categorical, logical(1), include_int = include_int)]
  cols <- sort(cols)

  if (length(cols) == 0) {
    return(empty_cat_summary())
  }

  summaries <- stats::setNames(lapply(cols, function(col) cat_summary(df1[[col]])), cols)
  out <- tibble::tibble(
    col_name = cols,
    cnt = unname(vapply(summaries, nrow, integer(1))),
    common = unname(vapply(summaries, common_value, character(1))),
    common_pcnt = unname(vapply(summaries, common_percent, numeric(1))),
    levels = summaries
  )
  names(out$levels) <- out$col_name
  out
}

inspect_cat_grouped <- function(df1, include_int = FALSE) {
  groups <- grouped_keys_and_rows(df1)
  group_vars <- names(groups$keys)
  data_cols <- setdiff(names(df1), group_vars)
  df_plain <- as.data.frame(df1)

  parts <- lapply(seq_along(groups$rows), function(i) {
    sub <- df_plain[groups$rows[[i]], data_cols, drop = FALSE]
    out <- inspect_cat_single(sub, include_int = include_int)
    add_group_keys(out, groups$keys[i, , drop = FALSE])
  })

  out <- bind_tibbles(parts)
  if (nrow(out) == 0) {
    out <- grouped_empty(groups$keys, empty_cat_summary())
  }
  order_by_first_group(out, group_vars)
}

inspect_cat_compare <- function(df1, df2, include_int = FALSE) {
  s1 <- inspect_cat_single(df1, include_int = include_int)
  s2 <- inspect_cat_single(df2, include_int = include_int)
  cols <- c(s1$col_name, setdiff(s2$col_name, s1$col_name))

  lvls_1 <- lapply(cols, function(col) list_lookup(s1$levels, s1$col_name, col))
  lvls_2 <- lapply(cols, function(col) list_lookup(s2$levels, s2$col_name, col))
  names(lvls_1) <- cols
  names(lvls_2) <- cols

  tibble::tibble(
    col_name = cols,
    jsd = mapply(jsd_from_levels, lvls_1, lvls_2),
    pval = mapply(chisq_from_levels, lvls_1, lvls_2, MoreArgs = list(n_1 = nrow(df1), n_2 = nrow(df2))),
    lvls_1 = lvls_1,
    lvls_2 = lvls_2
  )
}

cat_summary <- function(x) {
  n <- length(x)
  if (n == 0) {
    return(tibble::tibble(value = character(), prop = numeric(), cnt = integer()))
  }

  values <- as.character(x)
  vals <- unique(sort(values, na.last = TRUE, method = "quick"))
  counts <- tabulate(match(values, vals), nbins = length(vals))
  props <- counts / n
  out <- tibble::tibble(value = vals, prop = as.numeric(props), cnt = as.integer(counts))
  out[order(-out$prop, seq_len(nrow(out))), , drop = FALSE]
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
  x$prop[[1]] * 100
}

inspect_num_single <- function(df1, breaks = 20, include_int = TRUE) {
  df1 <- as.data.frame(df1)
  cols <- names(df1)[vapply(df1, is_numeric_column, logical(1), include_int = include_int)]

  if (length(cols) == 0) {
    return(list(out = empty_num_summary(), brks_list = list(), inspected = NULL, group_lengths = NULL))
  }

  summaries <- lapply(cols, function(col) num_summary(df1[[col]], breaks = breaks_for_col(col, breaks)))
  brks_list <- stats::setNames(lapply(summaries, `[[`, "breaks"), cols)
  hists <- stats::setNames(lapply(summaries, `[[`, "hist"), cols)

  out <- tibble::tibble(
    col_name = cols,
    min = unname(vapply(summaries, `[[`, numeric(1), "min")),
    q1 = unname(vapply(summaries, `[[`, numeric(1), "q1")),
    median = unname(vapply(summaries, `[[`, numeric(1), "median")),
    mean = unname(vapply(summaries, `[[`, numeric(1), "mean")),
    q3 = unname(vapply(summaries, `[[`, numeric(1), "q3")),
    max = unname(vapply(summaries, `[[`, numeric(1), "max")),
    sd = unname(vapply(summaries, `[[`, numeric(1), "sd")),
    pcnt_na = unname(vapply(summaries, `[[`, numeric(1), "pcnt_na")),
    hist = hists
  )
  names(out$hist) <- out$col_name
  names(brks_list) <- out$col_name

  list(out = out, brks_list = brks_list, inspected = NULL, group_lengths = NULL)
}

inspect_num_grouped <- function(df1, breaks = 20, include_int = TRUE) {
  groups <- grouped_keys_and_rows(df1)
  group_vars <- names(groups$keys)
  data_cols <- setdiff(names(df1), group_vars)
  df_plain <- as.data.frame(df1)
  global <- inspect_num_single(df_plain[, data_cols, drop = FALSE], breaks = breaks, include_int = include_int)

  parts <- lapply(seq_along(groups$rows), function(i) {
    sub <- df_plain[groups$rows[[i]], data_cols, drop = FALSE]
    out <- inspect_num_single(sub, breaks = global$brks_list, include_int = include_int)$out
    add_group_keys(out, groups$keys[i, , drop = FALSE])
  })

  out <- bind_tibbles(parts)
  if (nrow(out) == 0) {
    out <- grouped_empty(groups$keys, empty_num_summary())
  }

  group_lengths <- tibble::as_tibble(groups$keys)
  group_lengths$rows <- lengths(groups$rows)

  list(
    out = order_by_first_group(out, group_vars),
    brks_list = global$brks_list,
    inspected = NULL,
    group_lengths = group_lengths
  )
}

inspect_num_compare <- function(df1, df2, breaks = 20, include_int = TRUE) {
  s1_temp <- inspect_num_single(df1, breaks = breaks, include_int = include_int)
  s2_temp <- inspect_num_single(df2, breaks = breaks, include_int = include_int)

  common_cols <- intersect(names(s1_temp$brks_list), names(s2_temp$brks_list))
  common_breaks <- list()
  if (length(common_cols) > 0) {
    combined <- rbind(
      as.data.frame(df1[, common_cols, drop = FALSE]),
      as.data.frame(df2[, common_cols, drop = FALSE])
    )
    common_breaks <- inspect_num_single(combined, breaks = breaks, include_int = include_int)$brks_list
  }

  df1_specific <- setdiff(names(s1_temp$brks_list), names(s2_temp$brks_list))
  df2_specific <- setdiff(names(s2_temp$brks_list), names(s1_temp$brks_list))
  brks_list <- c(
    s1_temp$brks_list[df1_specific],
    common_breaks,
    s2_temp$brks_list[df2_specific]
  )

  s1 <- inspect_num_single(df1, breaks = brks_list, include_int = include_int)
  s2 <- inspect_num_single(df2, breaks = brks_list, include_int = include_int)
  cols <- c(s1$out$col_name, setdiff(s2$out$col_name, s1$out$col_name))

  hist_1 <- lapply(cols, function(col) list_lookup(s1$out$hist, s1$out$col_name, col))
  hist_2 <- lapply(cols, function(col) list_lookup(s2$out$hist, s2$out$col_name, col))
  names(hist_1) <- cols
  names(hist_2) <- cols

  out <- tibble::tibble(
    col_name = cols,
    hist_1 = hist_1,
    hist_2 = hist_2,
    jsd = mapply(jsd_from_hist, hist_1, hist_2),
    pval = mapply(chisq_from_hist, hist_1, hist_2, MoreArgs = list(n_1 = nrow(df1), n_2 = nrow(df2)))
  )

  brks_list <- c(s1$brks_list, s2$brks_list[setdiff(names(s2$brks_list), names(s1$brks_list))])
  inspected <- list(
    df1 = drop_list_column(s1$out, "hist"),
    df2 = drop_list_column(s2$out, "hist")
  )
  group_lengths <- tibble::tibble(name = c("df1", "df2"), rows = c(nrow(df1), nrow(df2)))

  list(out = out, brks_list = brks_list, inspected = inspected, group_lengths = group_lengths)
}

breaks_for_col <- function(col, breaks) {
  if (is.list(breaks)) {
    if (!is.null(names(breaks)) && col %in% names(breaks)) {
      return(breaks[[col]])
    }
    return(20)
  }
  breaks[[1]]
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
    hist <- missing_hist_summary(x, breaks)
  } else {
    quantiles <- stats::quantile(non_missing, probs = c(0.25, 0.5, 0.75), names = FALSE, na.rm = TRUE)
    stats <- c(
      min = min(non_missing, na.rm = TRUE),
      q1 = quantiles[[1]],
      median = stats::median(non_missing, na.rm = TRUE),
      mean = mean(non_missing, na.rm = TRUE),
      q3 = quantiles[[3]],
      max = max(non_missing, na.rm = TRUE),
      sd = stats::sd(non_missing, na.rm = TRUE)
    )
    hist <- hist_summary(x, breaks = breaks)
  }

  c(
    as.list(stats),
    pcnt_na = list(mean(is.na(x)) * 100),
    hist = list(hist$hist),
    breaks = list(hist$breaks)
  )
}

missing_hist_summary <- function(x, breaks = 20) {
  if (length(breaks) > 1 && is.numeric(breaks)) {
    hist <- suppressWarnings(graphics::hist(x, breaks = breaks, plot = FALSE, right = TRUE))
    return(list(hist = prop_value(hist), breaks = hist$breaks))
  }
  list(hist = tibble::tibble(value = NA_character_, prop = 1), breaks = NA_real_)
}

hist_summary <- function(x, breaks = 20) {
  hist <- graphics::hist(x, breaks = breaks, plot = FALSE, right = TRUE)
  list(hist = prop_value(hist), breaks = hist$breaks)
}

prop_value <- function(hist) {
  starts <- utils::head(hist$breaks, -1)
  ends <- utils::tail(hist$breaks, -1)
  total <- sum(hist$counts)
  prop <- if (total == 0) rep(0, length(hist$counts)) else hist$counts / total
  tibble::tibble(value = paste0("[", starts, ", ", ends, ")"), prop = as.numeric(prop))
}

list_lookup <- function(values, keys, key) {
  idx <- match(key, keys)
  if (is.na(idx)) NULL else values[[idx]]
}

drop_list_column <- function(x, col) {
  x[, setdiff(names(x), col), drop = FALSE]
}

grouped_keys_and_rows <- function(df) {
  groups <- attr(df, "groups")
  if (is.null(groups) || !(".rows" %in% names(groups))) {
    stop("Grouped data frames must include dplyr group metadata.", call. = FALSE)
  }
  group_vars <- setdiff(names(groups), ".rows")
  list(
    keys = as.data.frame(groups[, group_vars, drop = FALSE]),
    rows = as.list(groups$.rows)
  )
}

add_group_keys <- function(out, key) {
  if (nrow(out) == 0) {
    return(NULL)
  }
  key <- key[rep(1, nrow(out)), , drop = FALSE]
  key <- tibble::as_tibble(key)
  for (nm in names(out)) {
    key[[nm]] <- out[[nm]]
  }
  key
}

grouped_empty <- function(keys, summary) {
  out <- tibble::as_tibble(keys[0, , drop = FALSE])
  for (nm in names(summary)) {
    out[[nm]] <- summary[[nm]]
  }
  out
}

order_by_first_group <- function(out, group_vars) {
  if (nrow(out) == 0 || length(group_vars) == 0) {
    return(out)
  }
  out[order(out[[group_vars[[1]]]]), , drop = FALSE]
}

bind_tibbles <- function(parts) {
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) {
    return(tibble::tibble())
  }
  tibble::as_tibble(do.call(rbind, parts))
}

jsd_from_levels <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    return(NA_real_)
  }
  all_levels <- union_values(x$value, y$value)
  jsd(level_props(x, all_levels), level_props(y, all_levels))
}

chisq_from_levels <- function(x, y, n_1, n_2) {
  if (is.null(x) || is.null(y)) {
    return(NA_real_)
  }
  all_levels <- union_values(x$value, y$value)
  counts <- rbind(level_counts(x, all_levels), level_counts(y, all_levels))
  chisq_pvalue(counts)
}

jsd_from_hist <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    return(NA_real_)
  }
  all_bins <- union_values(x$value, y$value)
  jsd(hist_props(x, all_bins), hist_props(y, all_bins))
}

chisq_from_hist <- function(x, y, n_1, n_2) {
  if (is.null(x) || is.null(y)) {
    return(NA_real_)
  }
  all_bins <- union_values(x$value, y$value)
  counts <- rbind(
    as.integer(n_1 * hist_props(x, all_bins)),
    as.integer(n_2 * hist_props(y, all_bins))
  )
  chisq_pvalue(counts)
}

union_values <- function(x, y) {
  unique(c(x, y))
}

level_props <- function(x, levels) {
  counts <- level_counts(x, levels)
  if (sum(counts) == 0) {
    return(rep(0, length(levels)))
  }
  counts / sum(counts)
}

level_counts <- function(x, levels) {
  idx <- match(levels, x$value)
  out <- x$cnt[idx]
  out[is.na(out)] <- 0
  as.numeric(out)
}

hist_props <- function(x, bins) {
  idx <- match(bins, x$value)
  out <- x$prop[idx]
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
  counts <- counts[, colSums(counts) > 0, drop = FALSE]
  if (ncol(counts) < 2 || any(rowSums(counts) == 0) || sum(counts) == 0) {
    return(NA_real_)
  }

  small_cats <- which(colSums(counts) <= 10)
  if (length(small_cats) > 1) {
    counts <- cbind(counts[, -small_cats, drop = FALSE], rowSums(counts[, small_cats, drop = FALSE]))
  }
  if (ncol(counts) < 2) {
    return(NA_real_)
  }

  out <- try(suppressWarnings(stats::chisq.test(counts)$p.value), silent = TRUE)
  if (inherits(out, "try-error")) NA_real_ else out
}

collapse_rare_levels <- function(levels, high_cardinality = 0) {
  if (is.null(levels) || high_cardinality <= 0 || nrow(levels) == 0) {
    return(levels)
  }

  keep <- levels$cnt > high_cardinality
  if (all(keep)) {
    return(levels)
  }

  kept <- levels[keep, , drop = FALSE]
  high <- tibble::tibble(
    value = "High cardinality",
    prop = sum(levels$prop[!keep], na.rm = TRUE),
    cnt = sum(levels$cnt[!keep], na.rm = TRUE)
  )
  out <- rbind(kept, high)
  out[order(out$value == "High cardinality", -out$prop), , drop = FALSE]
}

plot_cat_single <- function(x,
                            text_labels = TRUE,
                            high_cardinality = 0,
                            label_thresh = 0.1,
                            label_size = 3,
                            label_color = NULL,
                            col_palette = 0,
                            plot_layout = NULL) {
  plot_data <- bind_plot_parts(seq_len(nrow(x)), function(i) {
    levels <- collapse_rare_levels(x$levels[[i]], high_cardinality = high_cardinality)
    if (is.null(levels) || nrow(levels) == 0) {
      return(NULL)
    }
    levels$col_name <- x$col_name[[i]]
    levels
  })

  if (nrow(plot_data) == 0) {
    return(empty_plot("No categorical columns to plot"))
  }

  plot_data$pcnt_plot <- plot_data$prop
  plot_data$label <- ifelse(
    plot_data$prop >= label_thresh & !is.na(plot_data$value),
    as.character(plot_data$value),
    NA_character_
  )

  segment_fill <- shade_within_bars(plot_data$col_name, plot_data$prop, plot_data$value, col_palette)
  plot_data$fill_key <- factor(seq_len(nrow(plot_data)))
  subtitle <- if (any(is.na(plot_data$value))) "Gray segments are missing values" else NULL

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(col_name, levels = rev(unique(col_name))), y = pcnt_plot, fill = fill_key)) +
    ggplot2::geom_col(width = 0.72, color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = percent_label, expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::scale_fill_manual(values = segment_fill) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = subtitle) +
    inspect_theme() +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_blank())

  if (text_labels) {
    label_color <- if (is.null(label_color)) "white" else label_color[[1]]
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        position = ggplot2::position_stack(vjust = 0.5),
        size = label_size,
        color = label_color,
        lineheight = 0.9,
        na.rm = TRUE,
        check_overlap = TRUE
      )
  }

  p
}

plot_cat_compare <- function(x,
                             text_labels = TRUE,
                             high_cardinality = 0,
                             label_thresh = 0.1,
                             label_size = 3,
                             label_color = NULL,
                             col_palette = 0,
                             plot_layout = NULL) {
  df_names <- attr(x, "df_names")
  dataset_names <- c(non_empty_name(df_names$df1, "df1"), non_empty_name(df_names$df2, "df2"))

  plot_data <- bind_plot_parts(seq_len(nrow(x)), function(i) {
    one <- collapse_rare_levels(x$lvls_1[[i]], high_cardinality = high_cardinality)
    two <- collapse_rare_levels(x$lvls_2[[i]], high_cardinality = high_cardinality)
    parts <- list()
    if (!is.null(one) && nrow(one) > 0) {
      one$dataset <- dataset_names[[1]]
      parts[[length(parts) + 1]] <- one
    }
    if (!is.null(two) && nrow(two) > 0) {
      two$dataset <- dataset_names[[2]]
      parts[[length(parts) + 1]] <- two
    }
    if (length(parts) == 0) {
      return(NULL)
    }
    both <- bind_tibbles(parts)
    both$col_name <- x$col_name[[i]]
    both$col_name2 <- x$col_name[[i]]
    both
  })

  if (nrow(plot_data) == 0) {
    return(empty_plot("No categorical columns to plot"))
  }

  plot_data$pcnt_plot <- plot_data$prop
  plot_data$bar_name <- paste0(plot_data$col_name, ": ", plot_data$dataset)
  plot_data$label <- ifelse(
    plot_data$prop >= label_thresh & !is.na(plot_data$value),
    as.character(plot_data$value),
    NA_character_
  )
  plot_data$fill_key <- factor(seq_len(nrow(plot_data)))

  bar_levels <- as.vector(t(outer(rev(unique(plot_data$col_name)), rev(dataset_names), paste, sep = ": ")))
  bar_levels <- bar_levels[bar_levels %in% unique(plot_data$bar_name)]
  segment_fill <- shade_within_bars(plot_data$col_name2, plot_data$prop, plot_data$value, col_palette)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(bar_name, levels = bar_levels), y = pcnt_plot, fill = fill_key)) +
    ggplot2::geom_col(width = 0.72, color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = percent_label, expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::scale_fill_manual(values = segment_fill) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = "Gray segments are missing values") +
    inspect_theme() +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_blank())

  if (text_labels) {
    label_color <- if (is.null(label_color)) "white" else label_color[[1]]
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        position = ggplot2::position_stack(vjust = 0.5),
        size = label_size,
        color = label_color,
        lineheight = 0.9,
        na.rm = TRUE,
        check_overlap = TRUE
      )
  }

  p
}

plot_num_single <- function(x,
                            text_labels = TRUE,
                            label_thresh = 0.1,
                            label_size = 2.7,
                            label_color = "#222222",
                            col_palette = 0,
                            plot_layout = NULL) {
  if (is.null(plot_layout)) plot_layout <- c(NA, 3)

  brks_list <- attr(x, "brks_list")
  plot_data <- bind_plot_parts(seq_len(nrow(x)), function(i) {
    hist_to_rects(x$hist[[i]], brks_list[[x$col_name[[i]]]], col_name = x$col_name[[i]])
  })

  if (nrow(plot_data) == 0) {
    return(empty_plot("No numeric values to plot"))
  }

  plot_data$prop_z <- stats::ave(
    plot_data$prop, plot_data$col_name,
    FUN = function(z) if (max(z, na.rm = TRUE) > 0) z / max(z, na.rm = TRUE) else z
  )

  ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = lower, xmax = upper, ymin = 0, ymax = prop, fill = prop_z),
      color = "white",
      linewidth = 0.2
    ) +
    ggplot2::facet_wrap(stats::as.formula("~ col_name"), scales = "free_x", ncol = facet_ncol(plot_layout)) +
    ggplot2::scale_y_continuous(labels = percent_label, expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::scale_fill_gradientn(colours = palette_pair(col_palette)) +
    ggplot2::labs(x = NULL, y = "Probability") +
    inspect_theme() +
    ggplot2::theme(legend.position = "none")
}

plot_num_compare <- function(x,
                             text_labels = TRUE,
                             label_size = 2.7,
                             label_color = "#222222",
                             col_palette = 0,
                             plot_layout = NULL,
                             alpha = 0.05) {
  if (is.null(plot_layout)) plot_layout <- c(NA, 3)

  df_names <- attr(x, "df_names")
  dataset_names <- c(non_empty_name(df_names$df1, "df1"), non_empty_name(df_names$df2, "df2"))
  plot_data <- bind_plot_parts(seq_len(nrow(x)), function(i) {
    pair_hist_tiles(x$hist_1[[i]], x$hist_2[[i]], x$col_name[[i]], dataset_names)
  })

  if (nrow(plot_data) == 0) {
    return(empty_plot("No numeric values to plot"))
  }

  plot_data$bin <- factor(plot_data$value, levels = rev(unique(plot_data$value)))
  plot_data$label <- ifelse(is.na(plot_data$prop), NA_character_, sprintf("%.1f", plot_data$prop * 100))
  sig <- ifelse(is.na(x$pval), "not tested", ifelse(x$pval < alpha, "different", "similar"))
  sig <- stats::setNames(sig, x$col_name)
  plot_data$col_name <- paste0(plot_data$col_name, " (", sig[plot_data$col_name], ")")

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = dataset, y = bin, fill = prop)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::facet_wrap(stats::as.formula("~ col_name"), scales = "free_y", ncol = facet_ncol(plot_layout)) +
    ggplot2::scale_fill_gradient(low = "white", high = palette_pair(col_palette)[[2]], na.value = "gray85") +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL, title = "Heat plot comparison of numeric columns") +
    inspect_theme() +
    ggplot2::theme(legend.position = "none")

  if (text_labels) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        color = label_color,
        size = label_size,
        na.rm = TRUE,
        check_overlap = TRUE
      )
  }

  p
}

plot_num_grouped <- function(x,
                             text_labels = TRUE,
                             label_size = 2.7,
                             label_color = "#222222",
                             col_palette = 0,
                             plot_layout = NULL) {
  if (is.null(plot_layout)) plot_layout <- c(NA, 3)

  brks_list <- attr(x, "brks_list")
  group_vars <- setdiff(names(x), names(empty_num_summary()))
  plot_data <- bind_plot_parts(seq_len(nrow(x)), function(i) {
    out <- hist_to_rects(x$hist[[i]], brks_list[[x$col_name[[i]]]], col_name = x$col_name[[i]])
    if (is.null(out)) {
      return(NULL)
    }
    out$group_label <- group_label(x[i, group_vars, drop = FALSE])
    out
  })

  if (nrow(plot_data) == 0) {
    return(empty_plot("No numeric values to plot"))
  }

  plot_data$bin <- factor(plot_data$value, levels = rev(unique(plot_data$value)))
  plot_data$label <- ifelse(is.na(plot_data$prop), NA_character_, sprintf("%.1f", plot_data$prop * 100))

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = group_label, y = bin, fill = prop)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::facet_wrap(stats::as.formula("~ col_name"), scales = "free_y", ncol = facet_ncol(plot_layout)) +
    ggplot2::scale_fill_gradient(low = "white", high = palette_pair(col_palette)[[2]], na.value = "gray85") +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    inspect_theme() +
    ggplot2::theme(legend.position = "none")

  if (text_labels) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        color = label_color,
        size = label_size,
        na.rm = TRUE,
        check_overlap = TRUE
      )
  }

  p
}

hist_to_rects <- function(hist, breaks, col_name) {
  if (is.null(hist) || length(breaks) < 2 || all(is.na(breaks))) {
    return(NULL)
  }
  n <- min(nrow(hist), length(breaks) - 1)
  if (n <= 0) {
    return(NULL)
  }
  tibble::tibble(
    col_name = col_name,
    value = hist$value[seq_len(n)],
    lower = utils::head(breaks, -1)[seq_len(n)],
    upper = utils::tail(breaks, -1)[seq_len(n)],
    mid = utils::head(breaks, -1)[seq_len(n)] + diff(breaks)[seq_len(n)] / 2,
    prop = hist$prop[seq_len(n)]
  )
}

pair_hist_tiles <- function(hist_1, hist_2, col_name, dataset_names) {
  if (is.null(hist_1) && is.null(hist_2)) {
    return(NULL)
  }
  bins <- union_values(if (is.null(hist_1)) character() else hist_1$value, if (is.null(hist_2)) character() else hist_2$value)
  tibble::tibble(
    col_name = rep(col_name, length(bins) * 2),
    dataset = rep(dataset_names, each = length(bins)),
    value = rep(bins, times = 2),
    prop = c(
      if (is.null(hist_1)) rep(NA_real_, length(bins)) else hist_props(hist_1, bins),
      if (is.null(hist_2)) rep(NA_real_, length(bins)) else hist_props(hist_2, bins)
    )
  )
}

group_label <- function(row) {
  paste(vapply(row, as.character, character(1)), collapse = ": ")
}

non_empty_name <- function(x, fallback) {
  if (is.null(x) || is.na(x) || !nzchar(x)) fallback else x
}

bind_plot_parts <- function(index, fn) {
  parts <- lapply(index, fn)
  bind_tibbles(parts)
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

# Palettes mirroring upstream {inspectdf} (R/palletes.R):
#   0: ggplot2 default (HCL hues via gg_color_hue)
#   1: colorblind-friendly (Okabe-Ito + grey)
#   2: 80s
#   3: rainbow (repeated without interpolation, as upstream does)
#   4: mario
#   5: pokemon
palette_for <- function(col_palette = 0, n = 1) {
  i <- suppressWarnings(as.integer(col_palette))
  if (length(i) != 1 || is.na(i) || i < 0L || i > 5L) i <- 0L

  if (i == 0L) {
    return(gg_color_hue(n))
  }

  named <- list(
    cbfriendly = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                   "#0072B2", "#D55E00", "#CC79A7"),
    `80s`      = c("#ff48c4", "#2bd1fc", "#f3ea5f", "#c04df9", "#ff3f3f"),
    rainbow    = c("#e70000", "#ff8c00", "#ffef00", "#00811f", "#0044ff", "#760089"),
    mario      = c("#fed1b0", "#ee1c25", "#0065b3", "#ffffff", "#894c2f"),
    pokemon    = c("#b3ffa9", "#ffa9b3", "#a9b3ff", "#fdff98", "#dfc189")
  )
  base <- named[[i]]

  if (i == 3L) {
    return(rep(base, length.out = n))
  }

  grDevices::colorRampPalette(base)(n)
}

# Two-colour palette pair used for histogram density gradients
# (mirrors upstream print_palette_pairs()).
palette_pair <- function(col_palette = 0) {
  i <- suppressWarnings(as.integer(col_palette))
  if (length(i) != 1 || is.na(i) || i < 0L || i > 5L) i <- 0L

  pairs <- list(
    gg_color_hue(5)[c(3, 1)],
    c("#E69F00", "#56B4E9"),
    c("#2bd1fc", "#ff3f3f"),
    c("#e70000", "#ff8c00", "#ffef00", "#00811f", "#0044ff", "#760089")[c(3, 1)],
    c("#fed1b0", "#ee1c25", "#0065b3", "#ffffff", "#894c2f")[c(1, 3)],
    c("#b3ffa9", "#ffa9b3", "#a9b3ff", "#fdff98", "#dfc189")[c(2, 3)]
  )
  pairs[[i + 1L]]
}

gg_color_hue <- function(n) {
  if (n <= 0) return(character(0))
  hues <- seq(15, 375, length.out = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[seq_len(n)]
}

get_shade_ramp <- function(col) {
  grDevices::colorRampPalette(c(col, "white"))(1001)
}

# Compute per-segment fill colours for stacked categorical bars: each column
# gets one base colour, and segments fade toward white along the bar.
shade_within_bars <- function(col_name, prop, value, col_palette) {
  cols <- unique(col_name)
  base_cols <- palette_for(col_palette, length(cols))
  ramps <- stats::setNames(lapply(base_cols, get_shade_ramp), cols)

  fill <- character(length(col_name))
  for (cn in cols) {
    idx <- which(col_name == cn)
    cum <- cumsum(prop[idx])
    if (length(idx) == 1) {
      stretch <- 0
    } else {
      stretch <- (cum - min(cum) + 0.001) / (max(cum) - min(cum) + 0.001)
      stretch <- stretch * (1 - 0.8 / length(idx))
    }
    ramp_idx <- pmin(pmax(round(stretch * 1000) + 1L, 1L), 1001L)
    fill[idx] <- ramps[[cn]][ramp_idx]
  }
  fill[is.na(value)] <- "gray65"
  fill[!is.na(value) & value == "High cardinality"] <- "darkmagenta"
  fill
}
