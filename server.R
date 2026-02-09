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

  balance_table_numeric_data <- shiny::reactive({
    shiny::req(combined_data())
    shiny::req(input$id_var)

    dat <- combined_data()
    id_var <- input$id_var

    vars <- setdiff(names(dat), c(id_var, ".included"))

    res <- purrr::map_dfr(vars, function(v) {
      x <- dat[[v]]
      inc <- dat$.included

      if (!is.numeric(x)) return(NULL)

      miss_in <- mean(is.na(x[inc == "Included"]))
      miss_out <- mean(is.na(x[inc == "Not included"]))

      pval_t <- safe_t_test_p(x[inc == "Included"], x[inc == "Not included"])
      pval_w <- safe_wilcox_p(x[inc == "Included"], x[inc == "Not included"])

      tibble::tibble(
        variable = v,
        missing_included = round(100 * miss_in, 1),
        missing_not_included = round(100 * miss_out, 1),
        mean_included = mean(x[inc == "Included"], na.rm = TRUE),
        mean_not_included = mean(x[inc == "Not included"], na.rm = TRUE),
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
    id_var <- input$id_var

    vars <- setdiff(names(dat), c(id_var, ".included"))

    res <- purrr::map_dfr(vars, function(v) {
      x <- dat[[v]]
      inc <- dat$.included

      if (is.numeric(x)) return(NULL)

      miss_in <- mean(is.na(x[inc == "Included"]))
      miss_out <- mean(is.na(x[inc == "Not included"]))

      xx <- as.factor(x)
      pval <- safe_fisher_p(inc, xx)

      tibble::tibble(
        variable = v,
        n_levels = nlevels(xx),
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
    balance_table_numeric_data() %>%
      dplyr::transmute(
        Variable = variable,
        `Missing % (Included)` = missing_included,
        `Missing % (Not included)` = missing_not_included,
        `Mean (Included)` = round(mean_included, 2),
        `Mean (Not included)` = round(mean_not_included, 2),
        `t-test p` = signif(p_ttest, 3),
        `wilcox p` = signif(p_wilcox, 3)
      )
  })

  output$balance_table_categorical <- shiny::renderTable({
    shiny::req(balance_table_categorical_data())
    balance_table_categorical_data() %>%
      dplyr::transmute(
        Variable = variable,
        `# levels` = n_levels,
        `Missing % (Included)` = missing_included,
        `Missing % (Not included)` = missing_not_included,
        `Fisher p` = signif(p_value, 3)
      )
  })

  output$var_title <- shiny::renderText({
    shiny::req(input$var)
    paste0("Variable: ", input$var)
  })

  output$dist_plot <- shiny::renderPlot({
    shiny::req(combined_data())
    shiny::req(input$var)
    make_var_plot(combined_data(), input$var)
  })

  output$summary_table <- shiny::renderTable({
    shiny::req(combined_data())
    shiny::req(input$var)
    make_var_summary(combined_data(), input$var)
  })

  output$download_report <- shiny::downloadHandler(
    filename = function() {
      paste0("representativeness-report-", format(Sys.Date(), "%Y-%m-%d"), ".html")
    },
    content = function(file) {
      shiny::req(combined_data())

      payload <- list(
        generated_on = Sys.time(),
        id_var = input$id_var,
        balance_numeric = balance_table_numeric_data(),
        balance_categorical = balance_table_categorical_data(),
        combined = combined_data(),
        include_drilldowns = isTRUE(input$report_include_drilldowns),
        vars = if (!is.null(input$report_vars)) input$report_vars else character(0)
      )

      tmpdir <- tempfile("repcheck_")
      dir.create(tmpdir, recursive = TRUE)

      rds_path <- file.path(tmpdir, "payload.rds")
      saveRDS(payload, rds_path)

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

      oldwd <- getwd()
      on.exit(setwd(oldwd), add = TRUE)
      setwd(tmpdir)

      quarto::quarto_render(
        input = qmd_tmp,
        execute_params = list(payload_rds = rds_path),
        output_file = "report.html",
        quiet = FALSE
      )

      file.copy(file.path(tmpdir, "report.html"), file, overwrite = TRUE)
    }
  )
}
