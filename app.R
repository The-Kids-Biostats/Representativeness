# app.R ---------------------------------------------------------------
# Interactive canvas version — edit this file directly.
#
# Roadmap of improvements already scaffolded below:
#  1) Unique-ID inclusion + mismatch warnings
#  2) Better sampling (distinct IDs, seed)
#  3) Missingness toggle (treat NA as category)
#  4) Proportion plots for categorical variables
#  5) Balance/effect-size overview table (SMD, Cramér's V, missingness)
#
# NOTE: This is a working starting point. Replace/extend sections marked "TODO".

library(shiny)
library(bslib)  # for clean Bootstrap 5 theming
library(thekidsbiostats)
library(readxl)
library(tools)

# Report rendering
library(quarto)  # quarto_render()
library(knitr)
library(scales)

# --------------------------- Branding --------------------------------
# Put your logo files in a folder called `www/` next to app.R, e.g.
#   www/thekids-logo.jpg
#   www/partner-logo.jpg
# Then update the paths below.
brand_primary   <- "#1F3B73"  # TODO: set your primary brand colour
brand_secondary <- "#F56B00"  # TODO: set your accent colour
brand_bg        <- "#F7F9FB"  # subtle app background
brand_card      <- "#FFFFFF"  # card background
logo_left_path  <- "thekids_logo.png"   # TODO: file in www/
logo_right_path <- "origins_logo.png"   # TODO: file in www/ (optional)

# Helper to read multiple file types ---------------------------------- ----------------------------------
read_any <- function(path) {
  ext <- tolower(file_ext(path))
  
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
    
    as_tibble(df_candidates[[1]])
    
  } else {
    stop("Unsupported file type: ", ext,
         ". Please upload CSV, XLSX/XLS, or RData/RDA.")
  }
}

# ------------------------------ UI -----------------------------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = brand_bg,
    fg = "#1B1F23",
    primary = brand_primary,
    secondary = brand_secondary,
    base_font = font_google("Barlow"),
    heading_font = font_google("Barlow"),
    code_font = font_google("JetBrains Mono")
  ),
  tags$head(
    tags$style(HTML(paste0(
      ":root{--brand-primary:", brand_primary, ";--brand-secondary:", brand_secondary, ";}",
      "body{background:", brand_bg, ";}",
      ".app-header{display:flex;align-items:center;justify-content:space-between;gap:12px;margin-bottom:12px;}",
      ".app-header .title{font-weight:700;margin:0;}",
      ".app-logo{height:44px;object-fit:contain;}",
      ".well{background:", brand_card, ";border:1px solid rgba(0,0,0,0.06);border-radius:14px;box-shadow:0 6px 18px rgba(0,0,0,0.04);}",
      ".tabbable > .nav{margin-bottom:12px;}",
      ".tab-pane{padding-top:6px;}",
      "table{background:", brand_card, ";}",
      "h4{margin-top:0.6rem;}",
      ".shiny-notification{border-radius:12px;box-shadow:0 6px 18px rgba(0,0,0,0.12);}",
      ".btn{border-radius:10px;}",
      ".form-control, .selectize-control .selectize-input{border-radius:10px;}",
      ".control-label{font-weight:600;}",
      "hr{opacity:0.12;}"
    )))
  ),
  div(
    class = "app-header",
    div(
      style = "display:flex; align-items:center; gap:12px;",
      tags$img(src = logo_left_path, class = "app-logo", alt = "Logo"),
      tags$h2("Representativeness checker", class = "title")
    ),
    # Right logo optional: if you don't want it, set logo_right_path <- "" above
    if (nzchar(logo_right_path)) tags$img(src = logo_right_path, class = "app-logo", alt = "Logo")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Population data"),
      fileInput(
        "pop_file",
        "Upload population dataset (CSV, XLSX, RData)",
        accept = c(".csv", ".xlsx", ".xls", ".RData", ".rda")
      ),
      actionButton("use_example", "Use example data (ggplot2::mpg)"),
      tags$hr(),
      
      uiOutput("id_var_ui"),
      
      tags$hr(),
      h4("2. Study sample"),
      radioButtons(
        "sample_mode",
        "How will you define the study population?",
        choices = c(
          "Upload study dataset (subset of population)" = "upload",
          "Sample from population within the app" = "sample"
        )
      ),
      
      conditionalPanel(
        condition = "input.sample_mode == 'upload'",
        fileInput(
          "study_file",
          "Upload study dataset (CSV, XLSX, RData)",
          accept = c(".csv", ".xlsx", ".xls", ".RData", ".rda")
        )
      ),
      
      conditionalPanel(
        condition = "input.sample_mode == 'sample'",
        numericInput("sample_n", "Sample size (number of unique IDs)", value = 100, min = 1),
        numericInput("sample_seed", "Random seed (optional)", value = 1, min = 1),
        actionButton("draw_sample", "Draw random sample")
      ),
      
      tags$hr(),
      h4("3. Options"),
      checkboxInput("missing_as_level", "Treat missing values as a category (for categorical vars)", value = TRUE),
      
      tags$hr(),
      h4("4. Variable to inspect"),
      uiOutput("var_select_ui"),
      
      tags$hr(),
      h4("5. Report"),
      checkboxInput("report_include_drilldowns", "Include drilldowns for selected variables", value = TRUE),
      uiOutput("report_vars_ui"),
      downloadButton("download_report", "Download report (HTML)")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          p("Separate balance tables for numeric vs categorical variables. Includes t-test and Fisher's exact test p-values."),
          h4("Numeric variables"),
          tableOutput("balance_table_numeric"),
          tags$hr(),
          h4("Categorical variables"),
          tableOutput("balance_table_categorical")
        ),
        tabPanel(
          "Drilldown",
          h3(textOutput("var_title")),
          plotOutput("dist_plot", height = 420),
          tags$hr(),
          h4("Summary table"),
          tableOutput("summary_table")
        )
      )
    )
  )
)

# ----------------------------- Server --------------------------------
server <- function(input, output, session) {
  
  # --- Variable typing helpers ----------------------------------------
  
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
  
  # 1) Load population data -----------------------------------------
  pop_data <- reactiveVal(NULL)
  
  observeEvent(input$use_example, {
    dat <- ggplot2::mpg %>%
      mutate(id = dplyr::row_number())
    pop_data(dat)
    showNotification("Loaded ggplot2::mpg example data with an 'id' column.", type = "message")
  })
  
  observeEvent(input$pop_file, {
    req(input$pop_file)
    dat <- read_any(input$pop_file$datapath)
    pop_data(dat)
    showNotification("Population dataset loaded.", type = "message")
  })
  
  output$id_var_ui <- renderUI({
    req(pop_data())
    selectInput(
      "id_var",
      "ID column (used to determine inclusion)",
      choices = names(pop_data()),
      selected = if ("id" %in% names(pop_data())) "id" else names(pop_data())[1]
    )
  })
  
  # 2) Define study sample ------------------------------------------
  study_data <- reactiveVal(NULL)
  
  observeEvent(input$study_file, {
    req(input$study_file)
    dat <- read_any(input$study_file$datapath)
    study_data(dat)
    showNotification("Study dataset loaded.", type = "message")
  })
  
  observeEvent(input$draw_sample, {
    req(pop_data())
    req(input$id_var)
    
    pop <- pop_data()
    id_var <- input$id_var
    
    validate(need(id_var %in% names(pop), "ID column not found in population data."))
    
    # Optional reproducibility
    if (!is.null(input$sample_seed) && is.finite(input$sample_seed)) {
      set.seed(as.integer(input$sample_seed))
    }
    
    ids <- pop %>% distinct(.data[[id_var]])
    n_ids <- nrow(ids)
    
    sampled_ids <- ids %>%
      slice_sample(n = min(input$sample_n, n_ids))
    
    stud <- pop %>%
      semi_join(sampled_ids, by = setNames(id_var, id_var))
    
    study_data(stud)
    showNotification("Sampled study dataset from population.", type = "message")
  })
  
  # 3) Variable selection -------------------------------------------
  output$var_select_ui <- renderUI({
    req(pop_data())
    req(study_data())
    req(input$id_var)
    
    common_vars <- intersect(names(pop_data()), names(study_data()))
    vars <- setdiff(common_vars, input$id_var)
    
    validate(need(length(vars) > 0,
                  "No common variables between population and study (apart from the ID column)."))
    
    selectInput("var", "Variable:", choices = vars)
  })
  
  # Variables available for reporting (same as drilldown choices)
  report_vars <- reactive({
    req(pop_data()); req(study_data()); req(input$id_var)
    common_vars <- intersect(names(pop_data()), names(study_data()))
    setdiff(common_vars, input$id_var)
  })
  
  output$report_vars_ui <- renderUI({
    req(report_vars())
    vars <- report_vars()
    selectizeInput(
      "report_vars",
      "Variables to include",
      choices = vars,
      selected = head(vars, 10),
      multiple = TRUE,
      options = list(plugins = list('remove_button'), placeholder = 'Select variables...')
    )
  })
  
  # 4) Combined data with inclusion flag (unique IDs + warnings) -----
  combined_data <- reactive({
    req(pop_data())
    req(study_data())
    req(input$id_var)
    
    pop <- pop_data()
    stud <- study_data()
    id_var <- input$id_var
    
    validate(
      need(id_var %in% names(pop), "ID column not found in population data."),
      need(id_var %in% names(stud), "ID column not found in study data.")
    )
    
    # Use UNIQUE IDs for inclusion
    included_ids <- stud %>% distinct(.data[[id_var]]) %>% pull(.data[[id_var]])
    
    # Warn if study has IDs not in population
    pop_ids <- pop %>% distinct(.data[[id_var]]) %>% pull(.data[[id_var]])
    missing_in_pop <- setdiff(included_ids, pop_ids)
    if (length(missing_in_pop) > 0) {
      showNotification(
        paste0("Warning: ", length(missing_in_pop), " study IDs not found in population."),
        type = "warning",
        duration = 6
      )
    }
    
    pop %>%
      mutate(.included = if_else(.data[[id_var]] %in% included_ids, "Included", "Not included"))
  })
  
  # 5) Overview balance tables (split numeric vs categorical) ----------
  
  safe_t_test_p <- function(x_in, x_out) {
    x_in  <- x_in[!is.na(x_in)]
    x_out <- x_out[!is.na(x_out)]
    if (length(x_in) < 2 || length(x_out) < 2) return(NA_real_)
    out <- tryCatch(stats::t.test(x_in, x_out)$p.value, error = function(e) NA_real_)
    as.numeric(out)
  }
  
  safe_wilcox_p <- function(x_in, x_out) {
    x_in  <- x_in[!is.na(x_in)]
    x_out <- x_out[!is.na(x_out)]
    if (length(x_in) < 1 || length(x_out) < 1) return(NA_real_)
    out <- tryCatch(stats::wilcox.test(x_in, x_out)$p.value, error = function(e) NA_real_)
    as.numeric(out)
  }
  
  safe_wilcox_p2 <- function(x_in, x_out) {
    x_in  <- x_in[!is.na(x_in)]
    x_out <- x_out[!is.na(x_out)]
    if (length(x_in) < 1 || length(x_out) < 1) return(NA_real_)
    out <- tryCatch(stats::wilcox.test(x_in, x_out)$p.value, error = function(e) NA_real_)
    as.numeric(out)
  }
  
  safe_fisher_p <- function(inc, x_factor) {
    # inc: character/factor with levels Included/Not included
    # x_factor: factor (may include (Missing) if desired)
    tab <- table(inc, x_factor)
    
    # Need at least 2 rows and 2 cols to test
    if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)
    
    out <- tryCatch(stats::fisher.test(tab)$p.value, error = function(e) NA_real_)
    as.numeric(out)
  }
  
  balance_table_numeric_data <- reactive({
    req(combined_data())
    req(input$id_var)
    
    dat <- combined_data()
    id_var <- input$id_var
    
    vars <- setdiff(names(dat), c(id_var, ".included"))
    
    res <- map_dfr(vars, function(v) {
      x <- dat[[v]]
      inc <- dat$.included
      
      if (!is.numeric(x)) return(NULL)
      
      miss_in  <- mean(is.na(x[inc == "Included"]))
      miss_out <- mean(is.na(x[inc == "Not included"]))
      
      pval_t  <- safe_t_test_p(x[inc == "Included"], x[inc == "Not included"])
      pval_w <- safe_wilcox_p(x[inc == "Included"], x[inc == "Not included"])
      
      tibble(
        variable = v,
        missing_included = round(100 * miss_in, 1),
        missing_not_included = round(100 * miss_out, 1),
        mean_included = mean(x[inc == "Included"], na.rm = TRUE),
        mean_not_included = mean(x[inc == "Not included"], na.rm = TRUE),
        p_ttest = pval_t,
        p_wilcox = pval_w
      )
    })
    
    if (nrow(res) == 0) return(tibble())
    
    res %>%
      mutate(
        p_ttest = if_else(is.nan(p_ttest), NA_real_, p_ttest),
        p_wilcox = if_else(is.nan(p_wilcox), NA_real_, p_wilcox)
      ) %>%
      arrange(p_ttest)
  })
  
  balance_table_ordinal_data <- reactive({
    req(combined_data(), input$id_var, var_types())
    dat <- combined_data()
    inc <- dat$.included
    
    vars <- var_types()$ordinal
    
    res <- purrr::map_dfr(vars, function(v) {
      x_raw <- dat[[v]]
      x <- as.integer(x_raw)  # ordinal score
      
      miss_in  <- mean(is.na(x_raw[inc == "Included"]))
      miss_out <- mean(is.na(x_raw[inc == "Not included"]))
      
      tibble::tibble(
        variable = v,
        missing_included = round(100 * miss_in, 1),
        missing_not_included = round(100 * miss_out, 1),
        median_included = suppressWarnings(stats::median(x[inc == "Included"], na.rm = TRUE)),
        median_not_included = suppressWarnings(stats::median(x[inc == "Not included"], na.rm = TRUE)),
        p_wilcox = safe_wilcox_p2(x[inc == "Included"], x[inc == "Not included"])
      )
    })
    
    if (nrow(res) == 0) return(tibble::tibble())
    res %>% dplyr::arrange(p_wilcox)
  })
  
  balance_table_date_data <- reactive({
    req(combined_data(), input$id_var, var_types())
    dat <- combined_data()
    inc <- dat$.included
    
    vars <- var_types()$date
    
    res <- purrr::map_dfr(vars, function(v) {
      x_raw <- dat[[v]]
      x_num <- as.numeric(x_raw)  # days since origin (Date) / seconds (POSIXct)
      
      miss_in  <- mean(is.na(x_raw[inc == "Included"]))
      miss_out <- mean(is.na(x_raw[inc == "Not included"]))
      
      med_in  <- suppressWarnings(stats::median(x_raw[inc == "Included"], na.rm = TRUE))
      med_out <- suppressWarnings(stats::median(x_raw[inc == "Not included"], na.rm = TRUE))
      
      tibble::tibble(
        variable = v,
        missing_included = round(100 * miss_in, 1),
        missing_not_included = round(100 * miss_out, 1),
        median_included = med_in,
        median_not_included = med_out,
        p_wilcox = safe_wilcox_p2(x_num[inc == "Included"], x_num[inc == "Not included"])
      )
    })
    
    if (nrow(res) == 0) return(tibble::tibble())
    res %>% dplyr::arrange(p_wilcox)
  })
  
  balance_table_categorical_data <- reactive({
    req(combined_data())
    req(input$id_var)
    
    dat <- combined_data()
    id_var <- input$id_var
    
    vars <- setdiff(names(dat), c(id_var, ".included"))
    
    res <- map_dfr(vars, function(v) {
      x <- dat[[v]]
      inc <- dat$.included
      
      # treat non-numeric as categorical
      if (is.numeric(x)) return(NULL)
      
      miss_in  <- mean(is.na(x[inc == "Included"]))
      miss_out <- mean(is.na(x[inc == "Not included"]))
      
      xx <- as.factor(x)
      if (isTRUE(input$missing_as_level)) {
        xx <- fct_explicit_na(xx, na_level = "(Missing)")
      }
      
      pval <- safe_fisher_p(inc, xx)
      
      # quick summary: number of levels (useful to interpret Fisher feasibility)
      tibble(
        variable = v,
        n_levels = nlevels(xx),
        missing_included = round(100 * miss_in, 1),
        missing_not_included = round(100 * miss_out, 1),
        p_value = pval
      )
    })
    
    if (nrow(res) == 0) return(tibble())
    
    res %>%
      mutate(p_value = if_else(is.nan(p_value), NA_real_, p_value)) %>%
      arrange(p_value)
  })
  
  output$balance_table_numeric <- renderTable({
    req(balance_table_numeric_data())
    balance_table_numeric_data() %>%
      transmute(
        Variable = variable,
        `Missing % (Included)` = missing_included,
        `Missing % (Not included)` = missing_not_included,
        `Mean (Included)` = round(mean_included, 2),
        `Mean (Not included)` = round(mean_not_included, 2),
        `t-test p` = signif(p_ttest, 3),
        `wilcox p` = signif(p_wilcox, 3)
      )
  })
  
  output$balance_table_categorical <- renderTable({
    req(balance_table_categorical_data())
    balance_table_categorical_data() %>%
      transmute(
        Variable = variable,
        `# levels` = n_levels,
        `Missing % (Included)` = missing_included,
        `Missing % (Not included)` = missing_not_included,
        `Fisher p` = signif(p_value, 3)
      )
  })
  
  # 6) Drilldown plot + table ----------------------------------------
  output$var_title <- renderText({
    req(input$var)
    paste0("Variable: ", input$var)
  })
  
  # Helper: generate plot for a given variable (used by app + report)
  make_var_plot <- function(dat, var_name, missing_as_level = TRUE) {
    x <- dat[[var_name]]
    
    if (is.numeric(x)) {
      ggplot(dat, aes(x = .data[[var_name]], fill = .included)) +
        geom_density(alpha = 0.35, na.rm = TRUE) +
        labs(x = var_name, y = "Density", fill = "") +
        theme_thekids()
      
    } else {
      dat2 <- dat %>%
        mutate(
          .x = as.factor(.data[[var_name]]),
          .x = if (isTRUE(missing_as_level)) forcats::fct_explicit_na(.x, "(Missing)") else .x
        )
      
      ggplot(dat2, aes(x = .x, fill = .included)) +
        geom_bar(position = "fill") +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(x = var_name, y = "Proportion", fill = "") +
        theme_thekids() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  }
  
  # Helper: generate summary table for a given variable
  make_var_summary <- function(dat, var_name, missing_as_level = TRUE) {
    x <- dat[[var_name]]
    
    if (is.numeric(x)) {
      dat %>%
        group_by(.included) %>%
        summarise(
          N = sum(!is.na(.data[[var_name]])),
          Missing = round(100 * mean(is.na(.data[[var_name]])), 1),
          Mean = mean(.data[[var_name]], na.rm = TRUE),
          SD = stats::sd(.data[[var_name]], na.rm = TRUE),
          Median = median(.data[[var_name]], na.rm = TRUE),
          Q1 = quantile(.data[[var_name]], 0.25, na.rm = TRUE),
          Q3 = quantile(.data[[var_name]], 0.75, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      dat2 <- dat %>%
        mutate(
          .x = as.factor(.data[[var_name]]),
          .x = if (isTRUE(missing_as_level)) forcats::fct_explicit_na(.x, "(Missing)") else .x
        )
      
      dat2 %>%
        count(.included, .x) %>%
        group_by(.included) %>%
        mutate(Percent = round(100 * n / sum(n), 1)) %>%
        ungroup() %>%
        rename(Level = .x, N = n) %>%
        arrange(.included, desc(N))
    }
  }
  
  output$dist_plot <- renderPlot({
    req(combined_data())
    req(input$var)
    make_var_plot(combined_data(), input$var, input$missing_as_level)
  })
  
  output$summary_table <- renderTable({
    req(combined_data())
    req(input$var)
    make_var_summary(combined_data(), input$var, input$missing_as_level)
  })
  
  # 7) Quarto report download ----------------------------------------
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("representativeness-report-", format(Sys.Date(), "%Y-%m-%d"), ".html")
    },
    content = function(file) {
      req(combined_data())
      
      payload <- list(
        generated_on = Sys.time(),
        id_var = input$id_var,
        missing_as_level = isTRUE(input$missing_as_level),
        balance_numeric = balance_table_numeric_data(),
        balance_categorical = balance_table_categorical_data(),
        combined = combined_data(),
        include_drilldowns = isTRUE(input$report_include_drilldowns),
        vars = if (!is.null(input$report_vars)) input$report_vars else character(0)
      )
      
      # 1) Local temp dir (NOT on the network)
      tmpdir <- tempfile("repcheck_")
      dir.create(tmpdir, recursive = TRUE)
      
      rds_path <- file.path(tmpdir, "payload.rds")
      saveRDS(payload, rds_path)
      
      # 2) Find report.qmd from the app directory (can be UNC)
      app_dir <- normalizePath(getShinyOption("appDir"), winslash = "/", mustWork = TRUE)
      qmd_src <- file.path(app_dir, "report.qmd")
      if (!file.exists(qmd_src)) stop("report.qmd not found at: ", qmd_src)
      
      # 3) Copy report.qmd into tmpdir so Quarto runs locally
      qmd_tmp <- file.path(tmpdir, "report.qmd")
      file.copy(qmd_src, qmd_tmp, overwrite = TRUE)
      
      # 3b) Copy _extensions into tmpdir so css: _extensions/... resolves
      ext_src <- file.path(app_dir, "_extensions")
      ext_dst <- file.path(tmpdir, "_extensions")
      
      if (dir.exists(ext_src)) {
        dir.create(ext_dst, recursive = TRUE, showWarnings = FALSE)
        
        # copy the CONTENTS of _extensions (not the folder itself)
        src_files <- list.files(ext_src, full.names = TRUE, all.files = TRUE, no.. = TRUE)
        
        ok <- file.copy(src_files, ext_dst, recursive = TRUE, overwrite = TRUE)
        if (!all(ok)) warning("Could not fully copy _extensions into temp render directory.")
      } else {
        warning("No _extensions folder found at: ", ext_src)
      }
      
      # 4) Render while temporarily setting working dir to tmpdir (local)
      oldwd <- getwd()
      on.exit(setwd(oldwd), add = TRUE)
      setwd(tmpdir)
      
      out_html <- quarto::quarto_render(
        input = qmd_tmp,
        execute_params = list(payload_rds = rds_path),
        output_file = "report.html",
        quiet = FALSE
      )
      
      # out_html should now be tmpdir/report.html
      file.copy(file.path(tmpdir, "report.html"), file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)

