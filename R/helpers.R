# Helper functions -----------------------------------------------------

read_any <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path)
  } else if (ext %in% c("rdata", "rda")) {
    e <- new.env()
    obj_names <- load(path, envir = e)
    if (length(obj_names) == 0) stop("No objects found in RData file.")

    obj_list <- mget(obj_names, envir = e)
    df_candidates <- obj_list[vapply(obj_list, inherits, logical(1), "data.frame")]
    if (length(df_candidates) == 0) stop("No data.frame objects found in RData file.")

    tibble::as_tibble(df_candidates[[1]])
  } else {
    stop(
      "Unsupported file type: ",
      ext,
      ". Please upload CSV, XLSX/XLS, or RData/RDA."
    )
  }
}

is_dateish <- function(x) inherits(x, c("Date", "POSIXct", "POSIXlt"))
is_ordinal <- function(x) is.ordered(x) || inherits(x, "ordered")

default_var_types <- function(dat, id_var) {
  vars <- setdiff(names(dat), c(id_var, ".included"))

  out <- list(
    numeric = character(0),
    categorical = character(0),
    ordinal = character(0),
    date = character(0)
  )

  for (v in vars) {
    x <- dat[[v]]
    if (is_dateish(x)) out$date <- c(out$date, v)
    else if (is_ordinal(x)) out$ordinal <- c(out$ordinal, v)
    else if (is.numeric(x)) out$numeric <- c(out$numeric, v)
    else out$categorical <- c(out$categorical, v)
  }

  out
}

safe_t_test_p <- function(x_in, x_out) {
  x_in <- x_in[!is.na(x_in)]
  x_out <- x_out[!is.na(x_out)]
  if (length(x_in) < 2 || length(x_out) < 2) return(NA_real_)
  out <- tryCatch(stats::t.test(x_in, x_out)$p.value, error = function(e) NA_real_)
  as.numeric(out)
}

safe_wilcox_p <- function(x_in, x_out) {
  x_in <- x_in[!is.na(x_in)]
  x_out <- x_out[!is.na(x_out)]
  if (length(x_in) < 1 || length(x_out) < 1) return(NA_real_)
  out <- tryCatch(stats::wilcox.test(x_in, x_out)$p.value, error = function(e) NA_real_)
  as.numeric(out)
}

safe_fisher_p <- function(inc, x_factor) {
  tab <- table(inc, x_factor)

  if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)

  out <- tryCatch(stats::fisher.test(tab)$p.value, error = function(e) NA_real_)
  as.numeric(out)
}

make_var_plot <- function(dat, var_name, missing_as_level = TRUE) {
  x <- dat[[var_name]]

  if (is.numeric(x)) {
    ggplot2::ggplot(dat, ggplot2::aes(x = .data[[var_name]], fill = .included)) +
      ggplot2::geom_density(alpha = 0.35, na.rm = TRUE) +
      ggplot2::labs(x = var_name, y = "Density", fill = "") +
      thekidsbiostats::theme_thekids()
  } else {
    dat2 <- dat %>%
      dplyr::mutate(
        .x = as.factor(.data[[var_name]]),
        .x = if (isTRUE(missing_as_level)) {
          forcats::fct_explicit_na(.x, "(Missing)")
        } else {
          .x
        }
      )

    ggplot2::ggplot(dat2, ggplot2::aes(x = .x, fill = .included)) +
      ggplot2::geom_bar(position = "fill") +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::labs(x = var_name, y = "Proportion", fill = "") +
      thekidsbiostats::theme_thekids() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
}

make_var_summary <- function(dat, var_name, missing_as_level = TRUE) {
  x <- dat[[var_name]]

  if (is.numeric(x)) {
    dat %>%
      dplyr::group_by(.included) %>%
      dplyr::summarise(
        N = sum(!is.na(.data[[var_name]])),
        Missing = round(100 * mean(is.na(.data[[var_name]])), 1),
        Mean = mean(.data[[var_name]], na.rm = TRUE),
        SD = stats::sd(.data[[var_name]], na.rm = TRUE),
        Median = stats::median(.data[[var_name]], na.rm = TRUE),
        Q1 = stats::quantile(.data[[var_name]], 0.25, na.rm = TRUE),
        Q3 = stats::quantile(.data[[var_name]], 0.75, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    dat2 <- dat %>%
      dplyr::mutate(
        .x = as.factor(.data[[var_name]]),
        .x = if (isTRUE(missing_as_level)) {
          forcats::fct_explicit_na(.x, "(Missing)")
        } else {
          .x
        }
      )

    dat2 %>%
      dplyr::count(.included, .x) %>%
      dplyr::group_by(.included) %>%
      dplyr::mutate(Percent = round(100 * n / sum(n), 1)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(Level = .x, N = n) %>%
      dplyr::arrange(.included, dplyr::desc(N))
  }
}
