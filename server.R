library(thekidsbiostats)

server <- function(input, output, session) {
  pop_data <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$use_example, {
    dat <- ggplot2::mpg %>%
      dplyr::mutate(id = dplyr::row_number())
    pop_data(dat)
    shiny::showNotification(
      "Loaded ggplot2::mpg example data with an 'id' column.",
      type = "message"
    )
  })

  shiny::observeEvent(input$pop_file, {
    shiny::req(input$pop_file)
    dat <- read_any(input$pop_file$datapath)
    pop_data(dat)
    shiny::showNotification("Population dataset loaded.", type = "message")
  })

  output$id_var_ui <- shiny::renderUI({
    shiny::req(pop_data())
    shiny::selectInput(
      "id_var",
      "ID column (used to determine inclusion)",
      choices = names(pop_data()),
      selected = if ("id" %in% names(pop_data())) "id" else names(pop_data())[1]
    )
  })

  study_data <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$study_file, {
    shiny::req(input$study_file)
    dat <- read_any(input$study_file$datapath)
    study_data(dat)
    shiny::showNotification("Study dataset loaded.", type = "message")
  })

  shiny::observeEvent(input$draw_sample, {
    shiny::req(pop_data())
    shiny::req(input$id_var)

    pop <- pop_data()
    id_var <- input$id_var

    shiny::validate(shiny::need(id_var %in% names(pop), "ID column not found in population data."))

    if (!is.null(input$sample_seed) && is.finite(input$sample_seed)) {
      set.seed(as.integer(input$sample_seed))
    }

    ids <- pop %>% dplyr::distinct(.data[[id_var]])
    n_ids <- nrow(ids)

    sampled_ids <- ids %>%
      dplyr::slice_sample(n = min(input$sample_n, n_ids))

    stud <- pop %>%
      dplyr::semi_join(sampled_ids, by = setNames(id_var, id_var))

    study_data(stud)
    shiny::showNotification("Sampled study dataset from population.", type = "message")
  })

  output$var_select_ui <- shiny::renderUI({
    shiny::req(pop_data())
    shiny::req(study_data())
    shiny::req(input$id_var)

    common_vars <- intersect(names(pop_data()), names(study_data()))
    vars <- setdiff(common_vars, input$id_var)

    shiny::validate(
      shiny::need(
        length(vars) > 0,
        "No common variables between population and study (apart from the ID column)."
      )
    )

    shiny::selectInput("var", "Variable:", choices = vars)
  })

  report_vars <- shiny::reactive({
    shiny::req(pop_data())
    shiny::req(study_data())
    shiny::req(input$id_var)
    common_vars <- intersect(names(pop_data()), names(study_data()))
    setdiff(common_vars, input$id_var)
  })

  output$report_vars_ui <- shiny::renderUI({
    shiny::req(report_vars())
    vars <- report_vars()
    shiny::selectizeInput(
      "report_vars",
      "Variables to include",
      choices = vars,
      selected = head(vars, 10),
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Select variables...")
    )
  })

  combined_data <- shiny::reactive({
    shiny::req(pop_data())
    shiny::req(study_data())
    shiny::req(input$id_var)

    pop <- pop_data()
    stud <- study_data()
    id_var <- input$id_var

    shiny::validate(
      shiny::need(id_var %in% names(pop), "ID column not found in population data."),
      shiny::need(id_var %in% names(stud), "ID column not found in study data.")
    )

    included_ids <- stud %>%
      dplyr::distinct(.data[[id_var]]) %>%
      dplyr::pull(.data[[id_var]])

    pop_ids <- pop %>%
      dplyr::distinct(.data[[id_var]]) %>%
      dplyr::pull(.data[[id_var]])
    missing_in_pop <- setdiff(included_ids, pop_ids)
    if (length(missing_in_pop) > 0) {
      shiny::showNotification(
        paste0("Warning: ", length(missing_in_pop), " study IDs not found in population."),
        type = "warning",
        duration = 6
      )
    }

    pop %>%
      dplyr::mutate(
        .included = dplyr::if_else(.data[[id_var]] %in% included_ids, "Included", "Not included")
      )
  })

  comparison_labels <- shiny::reactive({
    list(
      included = "Included",
      comparison = if (isTRUE(input$compare_to_population)) "Population" else "Not included"
    )
  })

  comparison_values <- function(x, inc) {
    if (isTRUE(input$compare_to_population)) {
      list(
        included = x[inc == "Included"],
        comparison = x
      )
    } else {
      list(
        included = x[inc == "Included"],
        comparison = x[inc == "Not included"]
      )
    }
  }

  comparison_data_for_var <- function(var_name) {
    base <- combined_data()
    inc <- base$.included
    x <- base[[var_name]]

    if (isTRUE(input$compare_to_population)) {
      tibble::tibble(
        .included = c(rep("Included", sum(inc == "Included")), rep("Population", length(x))),
        !!var_name := c(x[inc == "Included"], x)
      )
    } else {
      base %>% dplyr::select(.included, !!var_name)
    }
  }

  default_bucket_assignments <- shiny::reactive({
    shiny::req(combined_data())
    shiny::req(input$id_var)
    defaults <- default_var_types(combined_data(), input$id_var)
    list(
      numeric = defaults$numeric,
      categorical = c(defaults$categorical, defaults$date, defaults$ordinal)
    )
  })

  output$var_type_manager_ui <- shiny::renderUI({
    shiny::req(default_bucket_assignments())
    defaults <- default_bucket_assignments()

    numeric_vars <- if (is.null(input$var_types_numeric)) defaults$numeric else input$var_types_numeric
    categorical_vars <- if (is.null(input$var_types_categorical)) {
      defaults$categorical
    } else {
      input$var_types_categorical
    }

    sortable::bucket_list(
      header = NULL,
      group_name = "var-types",
      orientation = "horizontal",
      sortable::add_rank_list(
        text = "Numeric",
        labels = numeric_vars,
        input_id = "var_types_numeric"
      ),
      sortable::add_rank_list(
        text = "Categorical",
        labels = categorical_vars,
        input_id = "var_types_categorical"
      )
    )
  })

  output$var_type_notes_ui <- shiny::renderUI({
    shiny::req(combined_data())
    shiny::req(input$id_var)
    defaults <- default_var_types(combined_data(), input$id_var)
    notes <- list()

    if (length(defaults$date) > 0) {
      notes <- c(
        notes,
        list(
          shiny::tags$li(
            shiny::strong("Date/time detected: "),
            paste(defaults$date, collapse = ", ")
          )
        )
      )
    }

    if (length(defaults$ordinal) > 0) {
      notes <- c(
        notes,
        list(
          shiny::tags$li(
            shiny::strong("Ordinal detected: "),
            paste(defaults$ordinal, collapse = ", ")
          )
        )
      )
    }

    if (length(notes) == 0) return(NULL)

    shiny::tags$div(
      shiny::tags$hr(),
      shiny::tags$p("Other detected variable types:"),
      shiny::tags$ul(notes)
    )
  })

  var_type_map <- shiny::reactive({
    shiny::req(combined_data())
    shiny::req(input$id_var)
    defaults <- default_var_types(combined_data(), input$id_var)
    vars <- setdiff(names(combined_data()), c(input$id_var, ".included"))

    type_map <- setNames(rep("categorical", length(vars)), vars)
    type_map[defaults$numeric] <- "numeric"
    type_map[defaults$date] <- "date"
    type_map[defaults$ordinal] <- "ordinal"

    if (!is.null(input$var_types_numeric) || !is.null(input$var_types_categorical)) {
      default_bucket <- setNames(rep("categorical", length(vars)), vars)
      default_bucket[defaults$numeric] <- "numeric"

      bucket_override <- default_bucket
      if (!is.null(input$var_types_numeric)) {
        bucket_override[intersect(input$var_types_numeric, vars)] <- "numeric"
      }
      if (!is.null(input$var_types_categorical)) {
        bucket_override[intersect(input$var_types_categorical, vars)] <- "categorical"
      }

      moved <- bucket_override != default_bucket
      for (v in names(bucket_override)[moved]) {
        type_map[v] <- bucket_override[v]
      }
    }

    type_map
  })

  balance_table_numeric_data <- shiny::reactive({
    shiny::req(combined_data())
    shiny::req(input$id_var)

    dat <- combined_data()

    vars <- names(var_type_map())[var_type_map() == "numeric"]

    res <- purrr::map_dfr(vars, function(v) {
      x <- coerce_numeric(dat[[v]])
      inc <- dat$.included

      groups <- comparison_values(x, inc)

      miss_in <- mean(is.na(groups$included))
      miss_out <- mean(is.na(groups$comparison))

      pval_t <- safe_t_test_p(groups$included, groups$comparison)
      pval_w <- safe_wilcox_p(groups$included, groups$comparison)

      tibble::tibble(
        variable = v,
        missing_included = round(100 * miss_in, 1),
        missing_not_included = round(100 * miss_out, 1),
        mean_included = mean(groups$included, na.rm = TRUE),
        mean_not_included = mean(groups$comparison, na.rm = TRUE),
        p_ttest = pval_t,
        p_wilcox = pval_w
      )
    })

    if (nrow(res) == 0) return(tibble::tibble())

    res %>%
      dplyr::mutate(
        p_ttest = dplyr::if_else(is.nan(p_ttest), NA_real_, p_ttest),
        p_wilcox = dplyr::if_else(is.nan(p_wilcox), NA_real_, p_wilcox)
      ) %>%
      dplyr::arrange(p_ttest)
  })

  balance_table_categorical_data <- shiny::reactive({
    shiny::req(combined_data())
    shiny::req(input$id_var)

    dat <- combined_data()

    vars <- names(var_type_map())[var_type_map() != "numeric"]

    res <- purrr::map_dfr(vars, function(v) {
      x <- dat[[v]]
      inc <- dat$.included

      groups <- comparison_values(x, inc)

      miss_in <- mean(is.na(groups$included))
      miss_out <- mean(is.na(groups$comparison))

      if (isTRUE(input$compare_to_population)) {
        inc_vec <- c(
          rep("Included", length(groups$included)),
          rep("Population", length(groups$comparison))
        )
        x_vec <- c(groups$included, groups$comparison)
        pval <- safe_fisher_p(inc_vec, as.factor(x_vec))
      } else {
        pval <- safe_fisher_p(inc, as.factor(x))
      }

      tibble::tibble(
        variable = v,
        n_levels = nlevels(as.factor(x)),
        missing_included = round(100 * miss_in, 1),
        missing_not_included = round(100 * miss_out, 1),
        p_value = pval
      )
    })

    if (nrow(res) == 0) return(tibble::tibble())

    res %>%
      dplyr::mutate(p_value = dplyr::if_else(is.nan(p_value), NA_real_, p_value)) %>%
      dplyr::arrange(p_value)
  })

  output$balance_table_numeric <- shiny::renderTable({
    shiny::req(balance_table_numeric_data())
    labels <- comparison_labels()
    out <- balance_table_numeric_data() %>%
      dplyr::transmute(
        Variable = variable,
        missing_included = missing_included,
        missing_comparison = missing_not_included,
        mean_included = round(mean_included, 2),
        mean_comparison = round(mean_not_included, 2),
        `t-test p` = signif(p_ttest, 3),
        `wilcox p` = signif(p_wilcox, 3)
      )

    names(out)[names(out) == "missing_included"] <- paste0("Missing % (", labels$included, ")")
    names(out)[names(out) == "missing_comparison"] <- paste0("Missing % (", labels$comparison, ")")
    names(out)[names(out) == "mean_included"] <- paste0("Mean (", labels$included, ")")
    names(out)[names(out) == "mean_comparison"] <- paste0("Mean (", labels$comparison, ")")
    out
  })

  output$balance_table_categorical <- shiny::renderTable({
    shiny::req(balance_table_categorical_data())
    labels <- comparison_labels()
    out <- balance_table_categorical_data() %>%
      dplyr::transmute(
        Variable = variable,
        `# levels` = n_levels,
        missing_included = missing_included,
        missing_comparison = missing_not_included,
        `Fisher p` = signif(p_value, 3)
      )

    names(out)[names(out) == "missing_included"] <- paste0("Missing % (", labels$included, ")")
    names(out)[names(out) == "missing_comparison"] <- paste0("Missing % (", labels$comparison, ")")
    out
  })

  output$var_title <- shiny::renderText({
    shiny::req(input$var)
    paste0("Variable: ", input$var)
  })

  output$dist_plot <- shiny::renderPlot({
    shiny::req(combined_data())
    shiny::req(input$var)
    var_type <- var_type_map()[[input$var]]
    plot_data <- comparison_data_for_var(input$var)
    make_var_plot(plot_data, input$var, var_type = var_type)
  })

  output$summary_table <- shiny::renderTable({
    shiny::req(combined_data())
    shiny::req(input$var)
    var_type <- var_type_map()[[input$var]]
    summary_data <- comparison_data_for_var(input$var)
    make_var_summary(summary_data, input$var, var_type = var_type)
  })

  output$download_report <- shiny::downloadHandler(
    filename = function() {
      paste0("representativeness-report-", format(Sys.Date(), "%Y-%m-%d"), ".html")
    },
    content = function(file) {
      shiny::req(combined_data())

      shiny::withProgress(message = "Preparing report...", value = 0, {
        payload <- list(
          generated_on = Sys.time(),
          id_var = input$id_var,
          balance_numeric = balance_table_numeric_data(),
          balance_categorical = balance_table_categorical_data(),
          combined = combined_data(),
          var_type_map = var_type_map(),
          comparison_label = comparison_labels()$comparison,
          compare_to_population = isTRUE(input$compare_to_population),
          include_drilldowns = isTRUE(input$report_include_drilldowns),
          vars = if (!is.null(input$report_vars)) input$report_vars else character(0)
        )

        tmpdir <- tempfile("repcheck_")
        dir.create(tmpdir, recursive = TRUE)

        rds_path <- file.path(tmpdir, "payload.rds")
        saveRDS(payload, rds_path)

        shiny::incProgress(0.2, detail = "Setting up report template...")

        app_dir <- normalizePath(getShinyOption("appDir"), winslash = "/", mustWork = TRUE)
        qmd_src <- file.path(app_dir, "report.qmd")
        if (!file.exists(qmd_src)) stop("report.qmd not found at: ", qmd_src)

        qmd_tmp <- file.path(tmpdir, "report.qmd")
        file.copy(qmd_src, qmd_tmp, overwrite = TRUE)

        ext_src <- file.path(app_dir, "_extensions")
        ext_dst <- file.path(tmpdir, "_extensions")

        if (dir.exists(ext_src)) {
          dir.create(ext_dst, recursive = TRUE, showWarnings = FALSE)

          src_files <- list.files(ext_src, full.names = TRUE, all.files = TRUE, no.. = TRUE)

          ok <- file.copy(src_files, ext_dst, recursive = TRUE, overwrite = TRUE)
          if (!all(ok)) warning("Could not fully copy _extensions into temp render directory.")
        } else {
          warning("No _extensions folder found at: ", ext_src)
        }

        shiny::incProgress(0.6, detail = "Rendering report...")

        oldwd <- getwd()
        on.exit(setwd(oldwd), add = TRUE)
        setwd(tmpdir)

        quarto::quarto_render(
          input = qmd_tmp,
          execute_params = list(payload_rds = rds_path),
          output_file = "report.html",
          quiet = FALSE
        )

        shiny::incProgress(0.9, detail = "Finalizing download...")

        file.copy(file.path(tmpdir, "report.html"), file, overwrite = TRUE)
      })
    }
  )
}
