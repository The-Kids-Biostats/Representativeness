library(thekidsbiostats)

brand_primary <- "#1F3B73"
brand_secondary <- "#F56B00"
brand_bg <- "#F7F9FB"
brand_card <- "#FFFFFF"
logo_left_path <- "thekids_logo.png"
logo_right_path <- "origins_logo.png"

ui <- shiny::fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bg = brand_bg,
    fg = "#1B1F23",
    primary = brand_primary,
    secondary = brand_secondary,
    base_font = bslib::font_google("Barlow"),
    heading_font = bslib::font_google("Barlow"),
    code_font = bslib::font_google("JetBrains Mono")
  ),
  shiny::tags$head(
    shiny::tags$style(
      shiny::HTML(
        paste0(
          ":root{--brand-primary:", brand_primary, ";--brand-secondary:", brand_secondary, ";}",
          "body{background:", brand_bg, ";}",
          ".app-header{display:flex;align-items:center;justify-content:space-between;gap:12px;margin-bottom:12px;}",
          ".app-header .title{font-weight:700;margin:0;}",
          ".app-logo{height:44px;object-fit:contain;}",
          ".well{background:", brand_card, ";border:1px solid rgba(0,0,0,0.06);",
          "border-radius:14px;box-shadow:0 6px 18px rgba(0,0,0,0.04);}",
          ".tabbable > .nav{margin-bottom:12px;}",
          ".tab-pane{padding-top:6px;}",
          "table{background:", brand_card, ";}",
          "h4{margin-top:0.6rem;}",
          ".shiny-notification{border-radius:12px;box-shadow:0 6px 18px rgba(0,0,0,0.12);}",
          ".btn{border-radius:10px;}",
          ".form-control, .selectize-control .selectize-input{border-radius:10px;}",
          ".control-label{font-weight:600;}",
          "hr{opacity:0.12;}"
        )
      )
    )
  ),
  shiny::div(
    class = "app-header",
    shiny::div(
      style = "display:flex; align-items:center; gap:12px;",
      shiny::tags$img(src = logo_left_path, class = "app-logo", alt = "Logo"),
      shiny::tags$h2("Representativeness checker", class = "title")
    ),
    if (nzchar(logo_right_path)) {
      shiny::tags$img(src = logo_right_path, class = "app-logo", alt = "Logo")
    }
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h4("1. Population data"),
      shiny::fileInput(
        "pop_file",
        "Upload population dataset (CSV, XLSX, RData)",
        accept = c(".csv", ".xlsx", ".xls", ".RData", ".rda")
      ),
      shiny::actionButton("use_example", "Use example data (ggplot2::mpg)"),
      shiny::tags$hr(),
      shiny::uiOutput("id_var_ui"),
      shiny::tags$hr(),
      shiny::h4("2. Study sample"),
      shiny::radioButtons(
        "sample_mode",
        "How will you define the study population?",
        choices = c(
          "Upload study dataset (subset of population)" = "upload",
          "Sample from population within the app" = "sample"
        )
      ),
      shiny::conditionalPanel(
        condition = "input.sample_mode == 'upload'",
        shiny::fileInput(
          "study_file",
          "Upload study dataset (CSV, XLSX, RData)",
          accept = c(".csv", ".xlsx", ".xls", ".RData", ".rda")
        )
      ),
      shiny::conditionalPanel(
        condition = "input.sample_mode == 'sample'",
        shiny::numericInput("sample_n", "Sample size (number of unique IDs)", value = 100, min = 1),
        shiny::numericInput("sample_seed", "Random seed (optional)", value = 1, min = 1),
        shiny::actionButton("draw_sample", "Draw random sample")
      ),
      shiny::checkboxInput(
        "compare_to_population",
        "Compare sample to whole population",
        value = FALSE
      ),
      shiny::tags$hr(),
      shiny::h4("3. Report"),
      shiny::checkboxInput(
        "report_include_drilldowns",
        "Include drilldowns for selected variables",
        value = TRUE
      ),
      shiny::uiOutput("report_vars_ui"),
      shiny::downloadButton("download_report", "Download report (HTML)")
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Overview",
          shiny::p(
            "Separate balance tables for numeric vs categorical variables.",
            "Includes t-test and Fisher's exact test p-values."
          ),
          shiny::h4("Numeric variables"),
          shiny::tableOutput("balance_table_numeric"),
          shiny::tags$hr(),
          shiny::h4("Categorical variables"),
          shiny::tableOutput("balance_table_categorical")
        ),
        shiny::tabPanel(
          "Variable types",
          shiny::p(
            "Review the detected variable types. Drag variables between boxes to override their type."
          ),
          shiny::uiOutput("var_type_manager_ui"),
          shiny::uiOutput("var_type_notes_ui")
        ),
        shiny::tabPanel(
          "Drilldown",
          shiny::uiOutput("var_select_ui"),
          shiny::tags$hr(),
          shiny::h3(shiny::textOutput("var_title")),
          shiny::plotOutput("dist_plot", height = 420),
          shiny::tags$hr(),
          shiny::h4("Summary table"),
          shiny::tableOutput("summary_table")
        )
      )
    )
  )
)
