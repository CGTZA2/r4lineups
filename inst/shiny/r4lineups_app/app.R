if (file.exists("global.R")) {
  source("global.R")
}

if (!exists("parse_numeric_vector", mode = "function")) {
  parse_numeric_vector <- function(x) {
    if (is.null(x) || !nzchar(x)) {
      return(numeric(0))
    }
    vals <- unlist(regmatches(x, gregexpr("-?\\d+\\.?\\d*", x)))
    as.numeric(vals)
  }
}

if (!exists("parse_bins", mode = "function")) {
  parse_bins <- function(x) {
    if (is.null(x) || !nzchar(x)) {
      return(NULL)
    }
    vals <- parse_numeric_vector(x)
    if (length(vals) < 2) {
      return(NULL)
    }
    sort(unique(vals))
  }
}

if (!exists("save_plot", mode = "function")) {
  save_plot <- function(p, filename) {
    ggplot2::ggsave(filename, plot = p, width = 7, height = 5, dpi = 300)
  }
}

round_df <- function(df, digits = 3) {
  if (is.null(df)) {
    return(df)
  }
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) {
      round(x, digits)
    } else {
      x
    }
  })
  df
}

app_storage_dir <- function() {
  dir <- Sys.getenv("R4LINEUPS_APP_DATA", unset = NA_character_)
  if (is.na(dir) || !nzchar(dir)) {
    dir <- tools::R_user_dir("r4lineups", which = "data")
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

with_counter_lock <- function(lock_path, code) {
  on.exit(if (file.exists(lock_path)) unlink(lock_path), add = TRUE)
  tries <- 0
  while (file.exists(lock_path) && tries < 20) {
    Sys.sleep(0.05)
    tries <- tries + 1
  }
  ok <- file.create(lock_path)
  if (!ok) {
    return(code())
  }
  code()
}

increment_app_counter <- function() {
  dir <- app_storage_dir()
  counter_path <- file.path(dir, "app_counter.rds")
  lock_path <- file.path(dir, "app_counter.lock")
  with_counter_lock(lock_path, function() {
    current <- 0
    if (file.exists(counter_path)) {
      current <- tryCatch(readRDS(counter_path), error = function(e) 0)
    }
    current <- current + 1
    tmp <- paste0(counter_path, ".tmp")
    saveRDS(current, tmp)
    file.rename(tmp, counter_path)
    current
  })
}

read_app_counter <- function() {
  counter_path <- file.path(app_storage_dir(), "app_counter.rds")
  if (file.exists(counter_path)) {
    tryCatch(readRDS(counter_path), error = function(e) NA_integer_)
  } else {
    NA_integer_
  }
}

fetch_cran_downloads <- function(pkg, date_end = Sys.Date()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(NA_integer_)
  }
  url <- sprintf("https://cranlogs.r-pkg.org/downloads/total/1900-01-01/%s/%s",
                 date_end, pkg)
  out <- jsonlite::fromJSON(url)
  as.integer(out$downloads)
}

fetch_github_downloads <- function(repo) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(NA_integer_)
  }
  url <- sprintf("https://api.github.com/repos/%s/releases", repo)
  releases <- jsonlite::fromJSON(url)
  if (is.null(releases) || nrow(releases) == 0) {
    return(NA_integer_)
  }
  assets <- releases$assets
  if (is.null(assets) || nrow(assets) == 0) {
    return(0L)
  }
  sum(assets$download_count, na.rm = TRUE)
}

get_download_counts <- function(pkg = "r4lineups", repo = "CGTZA2/r4lineups", cache_hours = 24) {
  dir <- app_storage_dir()
  cache_path <- file.path(dir, "download_counts.rds")
  if (file.exists(cache_path)) {
    cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(cached) && difftime(Sys.time(), cached$timestamp, units = "hours") < cache_hours) {
      return(cached)
    }
  }
  cran <- tryCatch(fetch_cran_downloads(pkg), error = function(e) NA_integer_)
  gh <- tryCatch(fetch_github_downloads(repo), error = function(e) NA_integer_)
  result <- list(cran = cran, github = gh, timestamp = Sys.time())
  saveRDS(result, cache_path)
  result
}

build_lineup_table <- function(values, input_type, k) {
  if (length(values) == 0) {
    return(NULL)
  }
  values <- values[!is.na(values)]
  if (input_type == "Vector") {
    if (is.null(k) || is.na(k)) {
      stop("Nominal lineup size (k) is required.")
    }
    vals <- values[values != 0]
    if (any(vals %% 1 != 0)) {
      stop("Lineup vector must use integer positions.")
    }
    if (any(vals < 0)) {
      stop("Lineup vector cannot include negative positions.")
    }
    if (any(vals > k)) {
      stop("Lineup vector includes positions larger than k.")
    }
    return(table(factor(as.integer(vals), levels = seq_len(k))))
  }
  if (!is.null(k) && length(values) != k) {
    stop("Lineup table length must equal k.")
  }
  tab <- as.numeric(values)
  if (any(tab < 0)) {
    stop("Lineup table cannot include negative counts.")
  }
  names(tab) <- seq_len(length(tab))
  return(tab)
}

compute_malpass_sizes <- function(lineup_table, k) {
  ea <- sum(lineup_table) / k
  x <- sum(abs(lineup_table - ea) / (2 * ea))
  esize_adjusted <- k - x

  if (0 %in% names(lineup_table)) {
    ka <- k - 1
    lineup_table_a <- lineup_table[-1]
  } else {
    ka <- k
    lineup_table_a <- lineup_table
  }
  ea_a <- sum(lineup_table_a) / ka
  xa <- sum(abs(lineup_table_a - ea_a) / (2 * ea_a))
  esize_original <- ka - xa

  list(original = esize_original, adjusted = esize_adjusted)
}

resize_face_image <- function(path, width, out_dir) {
  if (is.null(width) || is.na(width) || width <= 0) {
    return(path)
  }
  out_path <- file.path(
    out_dir,
    paste0(tools::file_path_sans_ext(basename(path)), "_", width, ".jpg")
  )
  img <- magick::image_read(path)
  img <- magick::image_scale(img, paste0(width, "x"))
  magick::image_write(img, out_path, format = "jpg")
  out_path
}

prep_face_paths <- function(target_path, foil_paths, width) {
  foil_names <- tools::file_path_sans_ext(basename(foil_paths))
  if (is.null(width) || is.na(width) || width <= 0) {
    return(list(target = target_path, foils = foil_paths, foil_names = foil_names))
  }
  out_dir <- tempfile("r4lineups_faces_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  target_resized <- resize_face_image(target_path, width, out_dir)
  foil_resized <- vapply(
    foil_paths,
    resize_face_image,
    character(1),
    width = width,
    out_dir = out_dir
  )
  list(target = target_resized, foils = foil_resized, foil_names = foil_names)
}

ui <- navbarPage(
  "r4lineups Studio",
  header = tagList(
    tags$head(
      tags$style(HTML(
        "body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
         .panel-heading { font-weight: 600; }
         .help-block { font-size: 12px; color: #555; }
         .download-btn { margin-top: 6px; }
         .section-title { margin-top: 10px; font-weight: 600; }
         .tab-author { font-size: 13px; margin-left: 6px; color: #555; font-weight: 700; }
         .app-counts { position: absolute; right: 20px; top: 8px; font-size: 12px; color: #555; }"
      ))
    ),
    tags$div(class = "navbar-text app-counts", uiOutput("header_counts"))
  ),
  tabPanel(
    "Setup",
    sidebarLayout(
      sidebarPanel(
        helpText("Check or install Python dependencies for face similarity."),
        shinyWidgets::switchInput("setup_confirm", "I understand this installs Python packages", value = FALSE),
        actionButton("setup_check", "Check Python deps"),
        actionButton("setup_install", "Install Python deps"),
        actionButton("setup_runinfo", "Run locally")
      ),
      mainPanel(
        tags$div(class = "section-title", "Status"),
        verbatimTextOutput("setup_status"),
        tags$div(class = "section-title", "Available models"),
        verbatimTextOutput("setup_models"),
        tags$div(class = "section-title", "Available detectors"),
        verbatimTextOutput("setup_detectors")
      )
    )
  ),
  tabPanel(
    "Lineup Bias",
    sidebarLayout(
      sidebarPanel(
        helpText("A lineup vector lists the position (1..k) each mock witness chose (e.g., 3,2,5,...). A lineup table lists counts per position 1..k (e.g., 2,5,1,0,3,1)."),
        radioButtons("bias_input_type", "Input type", choices = c("Vector", "Table"), inline = TRUE),
        textAreaInput("bias_vec", "Lineup input", rows = 5,
                      placeholder = "Vector: 3,2,5,6,1,3,3,5,6,4  |  Table: 2,5,1,0,3,1"),
        numericInput("bias_target", "Target position", value = 3, min = 1),
        numericInput("bias_k", "Nominal lineup size (k)", value = 6, min = 2),
        numericInput("bias_boot", "Bootstrap R", value = 1000, min = 100, step = 100),
        actionButton("bias_example", "Load example (nortje2012$lineup_1)")
      ),
      mainPanel(
        helpText("Summary reports the target-position proportion with bootstrap CI and the chance rate (1/k)."),
        tags$div(class = "section-title", "Summary"),
        verbatimTextOutput("bias_summary"),
        tags$div(class = "section-title", "Bootstrap distribution"),
        plotOutput("bias_boot_plot"),
        tags$div(class = "section-title", "All proportions"),
        DTOutput("bias_allprop"),
        tags$div(class = "section-title", "Plot"),
        plotOutput("bias_plot"),
        downloadButton("bias_download_csv", "Download CSV", class = "download-btn"),
        downloadButton("bias_download_png", "Download Plot", class = "download-btn")
      )
    )
  ),
  tabPanel(
    "Effective Size",
    sidebarLayout(
      sidebarPanel(
        helpText("Provide a lineup vector (positions chosen) or a table of counts per position."),
        radioButtons("esize_input_type", "Input type", choices = c("Vector", "Table"), inline = TRUE),
        textAreaInput("esize_vec", "Lineup input", rows = 5,
                      placeholder = "Vector: 3,2,5,6,1,3,3,5,6,4  |  Table: 2,5,1,0,3,1"),
        numericInput("esize_k", "Nominal lineup size (k)", value = 6, min = 2),
        selectInput("esize_metric", "Effective size statistic",
                    choices = c("E (Tredoux, 1998)" = "tredoux",
                                "E (Malpass, 1981)" = "malpass")),
        conditionalPanel(
          condition = "input.esize_metric == 'tredoux'",
          selectInput("esize_ci_type", "Tredoux E' CI type",
                      choices = c("Bootstrap (percentile)" = "boot", "Normal (95%)" = "normal"))
        ),
        numericInput("esize_boot", "Bootstrap R", value = 1000, min = 100, step = 100),
        actionButton("esize_example", "Load example (nortje2012$lineup_1)")
      ),
      mainPanel(
        helpText("Summary includes the selected effective size statistic with confidence intervals. Malpass uses bootstrap CIs."),
        tags$div(class = "section-title", "Summary"),
        DTOutput("esize_summary"),
        tags$div(class = "section-title", "Bootstrap distribution"),
        plotOutput("esize_boot_plot"),
        downloadButton("esize_download_csv", "Download CSV", class = "download-btn")
      )
    )
  ),
  tabPanel(
    "Face Similarity",
    sidebarLayout(
      sidebarPanel(
        helpText("Upload target face and a set of foils to compare against. Images are resized for speed; original filenames are preserved."),
        fileInput("face_target", "Target image", accept = c(".jpg", ".jpeg", ".png")),
        fileInput("face_foils", "Foil images", multiple = TRUE,
                  accept = c(".jpg", ".jpeg", ".png")),
        numericInput("face_resize", "Resize width (px)", value = 200, min = 80, step = 10),
        numericInput("face_preview", "Preview width (px)", value = 120, min = 60, step = 10),
        shinyWidgets::switchInput("face_labels", "Show labels in preview", value = FALSE),
        shinyWidgets::pickerInput("face_model", "Model", choices = available_models(), selected = "ArcFace"),
        shinyWidgets::pickerInput("face_detector", "Detector", choices = available_detectors(), selected = "retinaface"),
        actionButton("face_check", "Check Python deps")
      ),
      mainPanel(
        tags$div(class = "section-title", "Status"),
        verbatimTextOutput("face_status"),
        tags$div(class = "section-title", "Lineup preview"),
        imageOutput("face_lineup", width = "auto", height = "auto"),
        helpText("Similarity table reports distance, similarity, and rank for each foil (lower distance/higher similarity = closer match)."),
        tags$div(class = "section-title", "Similarity table"),
        DTOutput("face_table"),
        tags$div(class = "section-title", "Plot"),
        plotOutput("face_plot"),
        tags$div(class = "section-title", "Pairwise similarity matrix"),
        DTOutput("face_matrix"),
        downloadButton("face_download_csv", "Download CSV", class = "download-btn"),
        downloadButton("face_download_png", "Download Plot", class = "download-btn"),
        downloadButton("face_matrix_csv", "Download Matrix", class = "download-btn")
      )
    )
  ),
  tabPanel(
    "ROC & CAC",
    sidebarLayout(
      sidebarPanel(
        helpText("Upload CSV with target_present, identification, confidence. Example format is shown in the preview table on the right."),
        fileInput("roc_file", "Data CSV", accept = c(".csv")),
        textInput("roc_bins", "Confidence bins (optional)", value = "0, 60, 80, 100"),
        selectInput("roc_compare_col", "Optional: compare ROC by column", choices = c("None" = "")),
        uiOutput("roc_compare_levels_ui"),
        numericInput("roc_compare_boot", "pAUC bootstrap R", value = 1000, min = 200, step = 100),
        shinyWidgets::switchInput("roc_full", "Also compute full ROC", value = TRUE),
        actionButton("roc_example", "Load example (lineup_example)")
      ),
      mainPanel(
        tags$div(class = "section-title", "Data preview"),
        helpText("Expected columns: target_present, identification, confidence."),
        DTOutput("roc_table"),
        tags$div(class = "section-title", "Summary"),
        verbatimTextOutput("roc_summary"),
        tags$div(class = "section-title", "ROC comparison (optional)"),
        verbatimTextOutput("roc_compare"),
        tags$div(class = "section-title", "ROC plot"),
        plotOutput("roc_plot"),
        tags$div(class = "section-title", "CAC plot"),
        plotOutput("cac_plot"),
        tags$div(class = "section-title", "Full ROC plot"),
        plotOutput("fullroc_plot"),
        downloadButton("roc_download_csv", "Download ROC/CAC data", class = "download-btn"),
        downloadButton("roc_download_png", "Download ROC Plot", class = "download-btn"),
        downloadButton("cac_download_png", "Download CAC Plot", class = "download-btn"),
        downloadButton("fullroc_download_png", "Download Full ROC Plot", class = "download-btn")
      )
    )
  ),
  tabPanel(
    title = tagList(
      "EIG & PPV",
      tags$span("Tredoux & Naylor (2026)", class = "tab-author")
    ),
    sidebarLayout(
      sidebarPanel(
        helpText("Uses the same data format as ROC/CAC."),
        fileInput("eig_file", "Data CSV", accept = c(".csv")),
        textInput("eig_bins", "Confidence bins (optional)", value = "0, 60, 80, 100"),
        actionButton("eig_example", "Load example (lineup_example)")
      ),
      mainPanel(
        tags$div(class = "section-title", "Summary"),
        verbatimTextOutput("eig_summary"),
        tags$div(class = "section-title", "EIG response data"),
        DTOutput("eig_table"),
        tags$div(class = "section-title", "EIG plots"),
        plotOutput("eig_plot"),
        plotOutput("eig_post_plot"),
        tags$div(class = "section-title", "PPV range"),
        DTOutput("ppv_table"),
        plotOutput("ppv_plot"),
        plotOutput("ppv_eff_plot"),
        downloadButton("eig_download_csv", "Download EIG data", class = "download-btn"),
        downloadButton("ppv_download_csv", "Download PPV data", class = "download-btn"),
        downloadButton("eig_download_png", "Download EIG Plot", class = "download-btn"),
        downloadButton("ppv_download_png", "Download PPV Plot", class = "download-btn")
      )
    )
  )
)

server <- function(input, output, session) {
  session_count <- increment_app_counter()
  download_counts <- reactiveVal(NULL)

  observe({
    download_counts(get_download_counts())
  })

  output$header_counts <- renderUI({
    counts <- download_counts()
    cran <- if (is.null(counts) || is.na(counts$cran)) "CRAN: n/a" else paste0("CRAN: ", format(counts$cran, big.mark = ","))
    gh <- if (is.null(counts) || is.na(counts$github)) "GitHub: n/a" else paste0("GitHub: ", format(counts$github, big.mark = ","))
    app <- if (is.na(session_count)) "App sessions: n/a" else paste0("App sessions: ", format(session_count, big.mark = ","))
    tags$span(paste(app, cran, gh, sep = " | "))
  })

  setup_status <- reactiveVal("Click 'Check Python deps' to validate deepface setup.")

  observeEvent(input$setup_check, {
    setup_status(tryCatch({
      capture.output(check_python_deps(verbose = TRUE))
    }, error = function(e) {
      paste("Python deps check failed:", e$message)
    }))
  })

  observeEvent(input$setup_install, {
    if (!isTRUE(input$setup_confirm)) {
      setup_status("Check the confirmation box before installing Python deps.")
      return()
    }
    setup_status("Installing Python dependencies... this may take several minutes.")
    tryCatch({
      install_r4lineups_python()
      setup_status("Install complete. Re-run check to verify.")
    }, error = function(e) {
      setup_status(paste("Install failed:", e$message))
    })
  })

  observeEvent(input$setup_runinfo, {
    showModal(modalDialog(
      title = "Run locally",
      tags$p("In an R session:"),
      tags$pre("library(r4lineups)\nrun_r4lineups_app()"),
      tags$p("Or directly via system.file:"),
      tags$pre("shiny::runApp(system.file(\"shiny/r4lineups_app\", package = \"r4lineups\"))"),
      easyClose = TRUE
    ))
  })

  output$setup_status <- renderPrint({
    setup_status()
  })

  output$setup_models <- renderPrint({
    available_models()
  })

  output$setup_detectors <- renderPrint({
    available_detectors()
  })

  observeEvent(input$bias_example, {
    data(nortje2012)
    updateTextAreaInput(session, "bias_vec", value = paste(nortje2012$lineup_1, collapse = ", "))
    updateNumericInput(session, "bias_k", value = max(nortje2012$lineup_1, na.rm = TRUE))
  })

  observeEvent(input$esize_example, {
    data(nortje2012)
    updateTextAreaInput(session, "esize_vec", value = paste(nortje2012$lineup_1, collapse = ", "))
    updateNumericInput(session, "esize_k", value = max(nortje2012$lineup_1, na.rm = TRUE))
  })

  observeEvent(input$roc_example, {
    data(lineup_example)
    updateTextInput(session, "roc_bins", value = "0, 60, 80, 100")
    showNotification("Using lineup_example data.", type = "message")
  })

  observeEvent(input$eig_example, {
    data(lineup_example)
    updateTextInput(session, "eig_bins", value = "0, 60, 80, 100")
  })

  bias_vals <- reactive({
    parse_numeric_vector(input$bias_vec)
  })

  bias_table <- reactive({
    vals <- bias_vals()
    validate(need(length(vals) > 0, "Enter a lineup vector or table."))
    tab <- build_lineup_table(vals, input$bias_input_type, input$bias_k)
    validate(need(!is.null(tab), "Invalid lineup input."))
    validate(need(sum(tab) > 0, "Lineup input has no selections."))
    tab
  })

  bias_vector_expanded <- reactive({
    tab <- bias_table()
    rep(as.integer(names(tab)), as.integer(tab))
  })

  bias_boot_obj <- reactive({
    vec <- bias_vector_expanded()
    validate(need(length(vec) > 1, "Need at least two selections for bootstrap."))
    boot::boot(vec, lineup_prop_boot, target_pos = input$bias_target, R = input$bias_boot)
  })

  bias_allprop <- reactive({
    tab <- bias_table()
    props <- as.numeric(tab) / sum(tab)
    df <- data.frame(prop = round(props, 3))
    rownames(df) <- names(tab)
    df
  })

  bias_plot <- reactive({
    props <- bias_allprop()
    props$member <- as.integer(rownames(props))
    ggplot(props, aes(x = factor(member), y = prop)) +
      geom_col(fill = "steelblue") +
      geom_hline(yintercept = 1 / input$bias_k, linetype = "dashed", color = "red") +
      labs(x = "Lineup member", y = "Proportion", title = "Lineup Proportions") +
      theme_minimal(base_size = 12)
  })

  output$bias_summary <- renderPrint({
    tab <- bias_table()
    k <- input$bias_k
    target <- input$bias_target
    validate(need(target >= 1 && target <= k, "Target position must be between 1 and k."))
    target_count <- if (as.character(target) %in% names(tab)) tab[[as.character(target)]] else 0
    prop <- if (sum(tab) > 0) target_count / sum(tab) else NA_real_
    ci <- c(NA_real_, NA_real_)
    boot_obj <- bias_boot_obj()
    ci <- tryCatch(boot::boot.ci(boot_obj, type = "perc")$percent[4:5],
                   error = function(e) c(NA_real_, NA_real_))
    list(
      n_witnesses = length(bias_vector_expanded()),
      target_count = target_count,
      target_proportion = round(prop, 3),
      ci_lower = round(ci[1], 3),
      ci_upper = round(ci[2], 3),
      chance = round(1 / k, 3)
    )
  })

  output$bias_boot_plot <- renderPlot({
    boot_obj <- bias_boot_obj()
    df <- data.frame(value = boot_obj$t)
    ggplot(df, aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#4c78a8", alpha = 0.6) +
      geom_density(color = "#1f2d3d", linewidth = 1) +
      labs(x = "Target proportion", y = "Density",
           title = "Bootstrap distribution of target proportion") +
      theme_minimal(base_size = 12)
  })

  output$bias_allprop <- renderDT({
    datatable(round_df(bias_allprop()), options = list(pageLength = 10))
  })

  output$bias_plot <- renderPlot({
    bias_plot()
  })

  output$bias_download_csv <- downloadHandler(
    filename = function() "lineup_bias_allprop.csv",
    content = function(file) {
      write.csv(bias_allprop(), file, row.names = TRUE)
    }
  )

  output$bias_download_png <- downloadHandler(
    filename = function() "lineup_bias_plot.png",
    content = function(file) {
      save_plot(bias_plot(), file)
    }
  )

  esize_vals <- reactive({
    parse_numeric_vector(input$esize_vec)
  })

  esize_summary <- reactive({
    vals <- esize_vals()
    validate(need(length(vals) > 0, "Enter a lineup vector or table."))
    k <- input$esize_k
    tab <- build_lineup_table(vals, input$esize_input_type, k)
    validate(need(!is.null(tab), "Invalid lineup input."))
    validate(need(sum(tab) > 0, "Lineup input has no selections."))
    vec <- rep(as.integer(names(tab)), as.integer(tab))
    e_t <- esize_T(tab)
    malpass <- compute_malpass_sizes(tab, k)
    metric_choice <- input$esize_metric

    ci_type <- if (identical(metric_choice, "tredoux")) input$esize_ci_type else "boot"
    ci_tredoux <- c(NA_real_, NA_real_)
    ci_label <- "Bootstrap (percentile)"
    if (identical(ci_type, "normal")) {
      ci_norm <- esize_T_ci_n(tab, 0.95)
      ci_tredoux <- c(ci_norm$ci_low, ci_norm$ci_high)
      ci_label <- "Normal (95%)"
    }

    malpass_ci <- c(NA_real_, NA_real_)
    if (length(vec) > 1) {
      if (identical(metric_choice, "tredoux") && !identical(ci_type, "normal")) {
        esize_t_boot_vec <- function(lineup_vec, d) {
          boot_tab <- table(factor(lineup_vec[d], levels = seq_len(k)))
          esize_T(boot_tab)
        }
        boot_obj <- boot::boot(vec, esize_t_boot_vec, R = input$esize_boot)
        ci_tredoux <- tryCatch(boot::boot.ci(boot_obj, type = "perc")$percent[4:5],
                               error = function(e) c(NA_real_, NA_real_))
      }
      if (identical(metric_choice, "malpass")) {
        malpass_boot <- function(lineup_vec, d, k) {
          boot_tab <- table(factor(lineup_vec[d], levels = seq_len(k)))
          sizes <- compute_malpass_sizes(boot_tab, k)
          sizes$adjusted
        }
        boot_obj_m <- boot::boot(vec, malpass_boot, k = k, R = input$esize_boot)
        malpass_ci <- tryCatch(
          boot::boot.ci(boot_obj_m, type = "perc")$percent[4:5],
          error = function(e) c(NA_real_, NA_real_)
        )
      }
    }

    if (identical(metric_choice, "malpass")) {
      data.frame(
        metric = "E (Malpass, 1981)",
        estimate = round(malpass$adjusted, 3),
        ci_lower = round(malpass_ci[1], 3),
        ci_upper = round(malpass_ci[2], 3),
        ci_type = "Bootstrap (percentile)",
        nominal_size = k
      )
    } else {
      data.frame(
        metric = "E (Tredoux, 1998)",
        estimate = round(e_t, 3),
        ci_lower = round(ci_tredoux[1], 3),
        ci_upper = round(ci_tredoux[2], 3),
        ci_type = ci_label,
        nominal_size = k
      )
    }
  })

  esize_boot_dists <- reactive({
    vals <- esize_vals()
    validate(need(length(vals) > 0, "Enter a lineup vector or table."))
    k <- input$esize_k
    tab <- build_lineup_table(vals, input$esize_input_type, k)
    validate(need(!is.null(tab), "Invalid lineup input."))
    validate(need(sum(tab) > 0, "Lineup input has no selections."))
    vec <- rep(as.integer(names(tab)), as.integer(tab))
    validate(need(length(vec) > 1, "Need at least two selections for bootstrap."))
    metric_choice <- input$esize_metric

    if (identical(metric_choice, "malpass")) {
      malpass_boot <- function(lineup_vec, d, k) {
        boot_tab <- table(factor(lineup_vec[d], levels = seq_len(k)))
        sizes <- compute_malpass_sizes(boot_tab, k)
        sizes$adjusted
      }
      boot_obj_m <- boot::boot(vec, malpass_boot, k = k, R = input$esize_boot)
      data.frame(
        metric = "E (Malpass, 1981)",
        value = boot_obj_m$t
      )
    } else {
      esize_t_boot_vec <- function(lineup_vec, d) {
        boot_tab <- table(factor(lineup_vec[d], levels = seq_len(k)))
        esize_T(boot_tab)
      }
      boot_obj_t <- boot::boot(vec, esize_t_boot_vec, R = input$esize_boot)
      data.frame(
        metric = "E (Tredoux, 1998)",
        value = boot_obj_t$t
      )
    }
  })

  output$esize_boot_plot <- renderPlot({
    df <- esize_boot_dists()
    ggplot(df, aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#72b7b2", alpha = 0.6) +
      geom_density(color = "#1f2d3d", linewidth = 1) +
      labs(x = "Effective size", y = "Density",
           title = "Bootstrap distributions of effective size") +
      theme_minimal(base_size = 12)
  })

  output$esize_summary <- renderDT({
    datatable(round_df(esize_summary()), options = list(pageLength = 10))
  })

  output$esize_download_csv <- downloadHandler(
    filename = function() "effective_size_summary.csv",
    content = function(file) {
      write.csv(esize_summary(), file, row.names = FALSE)
    }
  )

  output$face_status <- renderPrint({
    if (input$face_check == 0) {
      return("Click 'Check Python deps' to validate deepface setup.")
    }
    tryCatch({
      check_python_deps(verbose = FALSE)
    }, error = function(e) {
      paste("Python deps check failed:", e$message)
    })
  })

  face_paths <- reactive({
    req(input$face_target, input$face_foils)
    target_path <- normalizePath(input$face_target$datapath)
    foil_paths <- normalizePath(input$face_foils$datapath)
    validate(need(length(foil_paths) > 0, "Upload at least one foil image."))
    prep_face_paths(target_path, foil_paths, input$face_resize)
  })

  face_similarity <- reactive({
    withProgress(message = "Computing face similarity", value = 0, {
      incProgress(0.1, detail = "Preparing images")
      paths <- face_paths()
      incProgress(0.6, detail = "Extracting embeddings")
      res <- lineup_similarity(
        paths$target,
        paths$foils,
        model = input$face_model,
        detector = input$face_detector,
        foil_names = paths$foil_names
      )
      incProgress(0.3, detail = "Finalizing")
      res
    })
  })

  face_table <- reactive({
    res <- face_similarity()
    out <- res[, c("foil_name", "distance", "similarity", "rank")]
    out$distance <- round(out$distance, 3)
    out$similarity <- round(out$similarity, 3)
    out
  })

  face_matrix_data <- reactive({
    withProgress(message = "Computing pairwise similarities", value = 0, {
      incProgress(0.2, detail = "Preparing images")
      paths <- face_paths()
      incProgress(0.6, detail = "Extracting embeddings")
      mat <- lineup_pairwise_matrix(
        paths$target,
        paths$foils,
        model = input$face_model,
        detector = input$face_detector
      )
      incProgress(0.2, detail = "Finalizing")
      mat
    })
  })

  output$face_lineup <- renderImage({
    paths <- face_paths()
    preview_ncol <- 3
    img <- tryCatch(
      display_lineup(paths$target, paths$foils, ncol = preview_ncol, scale = input$face_preview,
                     show_labels = isTRUE(input$face_labels)),
      error = function(e) {
        display_lineup(paths$target, paths$foils, ncol = preview_ncol, scale = input$face_preview,
                       show_labels = FALSE)
      }
    )
    outfile <- tempfile(fileext = ".png")
    magick::image_write(img, outfile, format = "png")
    list(src = outfile, contentType = "image/png",
         width = paste0(preview_ncol * input$face_preview, "px"))
  }, deleteFile = TRUE)

  output$face_table <- renderDT({
    datatable(round_df(face_table()), options = list(pageLength = 10))
  })

  output$face_plot <- renderPlot({
    plot_lineup_similarity(face_similarity(), show_threshold = FALSE)
  })

  output$face_matrix <- renderDT({
    datatable(round(face_matrix_data(), 3), options = list(pageLength = 10))
  })

  output$face_download_csv <- downloadHandler(
    filename = function() "face_similarity_results.csv",
    content = function(file) {
      write.csv(face_table(), file, row.names = FALSE)
    }
  )

  output$face_download_png <- downloadHandler(
    filename = function() "face_similarity_plot.png",
    content = function(file) {
      save_plot(plot_lineup_similarity(face_similarity()), file)
    }
  )

  output$face_matrix_csv <- downloadHandler(
    filename = function() "face_similarity_matrix.csv",
    content = function(file) {
      write.csv(round(face_matrix_data(), 3), file, row.names = TRUE)
    }
  )

  roc_data <- reactive({
    if (isTruthy(input$roc_file)) {
      df <- read.csv(input$roc_file$datapath, stringsAsFactors = FALSE)
    } else {
      data(lineup_example)
      df <- lineup_example
    }
    df
  })

  observe({
    df <- roc_data()
    candidate_cols <- setdiff(names(df), c("target_present", "identification", "confidence"))
    updateSelectInput(
      session,
      "roc_compare_col",
      choices = c("None" = "", candidate_cols),
      selected = ""
    )
  })

  output$roc_compare_levels_ui <- renderUI({
    req(nzchar(input$roc_compare_col))
    df <- roc_data()
    vals <- unique(df[[input$roc_compare_col]])
    if (length(vals) < 2) {
      return(helpText("Select a column with at least two groups."))
    }
    selectInput("roc_compare_levels", "Pick two levels", choices = vals, multiple = TRUE)
  })

  output$roc_table <- renderDT({
    datatable(round_df(head(roc_data(), 10)), options = list(pageLength = 10))
  })

  roc_bins <- reactive({
    parse_bins(input$roc_bins)
  })

  roc_summary <- reactive({
    df <- roc_data()
    validate(
      need(all(c("target_present", "identification", "confidence") %in% names(df)),
           "Data must include target_present, identification, confidence columns.")
    )
    roc_obj <- make_rocdata(df, lineup_size = 6)
    cac_obj <- make_cac(df, lineup_size = 6, confidence_bins = roc_bins(), show_plot = FALSE)
    list(
      roc_pauc = roc_obj$pauc,
      cac_overall = cac_obj$overall_accuracy
    )
  })

  output$roc_summary <- renderPrint({
    vals <- roc_summary()
    list(
      roc_pauc = round(vals$roc_pauc, 3),
      cac_overall = round(vals$cac_overall, 3)
    )
  })

  output$roc_compare <- renderPrint({
    if (!nzchar(input$roc_compare_col)) {
      return("Select a grouping column to compare two ROC curves.")
    }
    req(input$roc_compare_levels)
    if (length(input$roc_compare_levels) != 2) {
      return("Pick exactly two levels for pAUC comparison.")
    }
    df <- roc_data()
    lvls <- input$roc_compare_levels
    data1 <- df[df[[input$roc_compare_col]] == lvls[1], ]
    data2 <- df[df[[input$roc_compare_col]] == lvls[2], ]
    validate(need(nrow(data1) > 0 && nrow(data2) > 0, "Selected groups have no rows."))
    res <- withProgress(message = "Comparing pAUC", value = 0, {
      incProgress(0.2, detail = "Bootstrapping")
      compare_pauc(
        data1,
        data2,
        lineup_size = 6,
        n_bootstrap = input$roc_compare_boot,
        label1 = as.character(lvls[1]),
        label2 = as.character(lvls[2])
      )
    })
    list(
      pauc_diff = round(res$pauc_diff, 3),
      z_score = round(res$z_score, 3),
      p_value = format.pval(res$p_value, digits = 3)
    )
  })

  roc_plot <- reactive({
    df <- roc_data()
    validate(
      need(all(c("target_present", "identification", "confidence") %in% names(df)),
           "Data must include target_present, identification, confidence columns.")
    )
    make_roc_gg(make_rocdata(df, lineup_size = 6))
  })

  cac_plot <- reactive({
    df <- roc_data()
    validate(
      need(all(c("target_present", "identification", "confidence") %in% names(df)),
           "Data must include target_present, identification, confidence columns.")
    )
    make_cac_gg(make_cacdata(df, lineup_size = 6, confidence_bins = roc_bins()))
  })

  fullroc_plot <- reactive({
    req(isTRUE(input$roc_full))
    df <- roc_data()
    validate(
      need(all(c("target_present", "identification", "confidence") %in% names(df)),
           "Data must include target_present, identification, confidence columns.")
    )
    plot_fullroc(make_fullroc_data(df, conf_bins = roc_bins(), lineup_size = 6))
  })

  output$roc_plot <- renderPlot({
    roc_plot()
  })

  output$cac_plot <- renderPlot({
    cac_plot()
  })

  output$fullroc_plot <- renderPlot({
    if (!isTRUE(input$roc_full)) return(NULL)
    fullroc_plot()
  })

  output$roc_download_csv <- downloadHandler(
    filename = function() "roc_cac_data.rds",
    content = function(file) {
      df <- roc_data()
      roc_obj <- make_rocdata(df, lineup_size = 6)
      cac_obj <- make_cacdata(df, lineup_size = 6, confidence_bins = roc_bins())
      out <- list(roc_data = roc_obj$roc_data, cac_data = cac_obj$cac_data)
      saveRDS(out, file)
    }
  )

  output$roc_download_png <- downloadHandler(
    filename = function() "roc_plot.png",
    content = function(file) {
      save_plot(roc_plot(), file)
    }
  )

  output$cac_download_png <- downloadHandler(
    filename = function() "cac_plot.png",
    content = function(file) {
      save_plot(cac_plot(), file)
    }
  )

  output$fullroc_download_png <- downloadHandler(
    filename = function() "fullroc_plot.png",
    content = function(file) {
      if (!isTRUE(input$roc_full)) return(NULL)
      save_plot(fullroc_plot(), file)
    }
  )

  eig_data <- reactive({
    if (isTruthy(input$eig_file)) {
      df <- read.csv(input$eig_file$datapath, stringsAsFactors = FALSE)
    } else {
      data(lineup_example)
      df <- lineup_example
    }
    df
  })

  eig_bins <- reactive({
    parse_bins(input$eig_bins)
  })

  eig_obj <- reactive({
    df <- eig_data()
    validate(
      need(all(c("target_present", "identification", "confidence") %in% names(df)),
           "Data must include target_present, identification, confidence columns.")
    )
    make_eig(df, confidence_bins = eig_bins(), show_plot = FALSE)
  })

  ppv_obj <- reactive({
    df <- eig_data()
    validate(
      need(all(c("target_present", "identification", "confidence") %in% names(df)),
           "Data must include target_present, identification, confidence columns.")
    )
    make_ppv_range(df, lineup_size = 6, show_plots = FALSE)
  })

  output$eig_summary <- renderPrint({
    obj <- eig_obj()
    ppv <- ppv_obj()
    list(
      eig = round(obj$eig, 3),
      ppv_nominal = round(ppv$ppv_nominal$overall_ppv, 3),
      ppv_effective = round(ppv$ppv_effective$overall_ppv, 3),
      ppv_none = round(ppv$ppv_none$overall_ppv, 3)
    )
  })

  output$eig_table <- renderDT({
    datatable(round_df(eig_obj()$response_data), options = list(pageLength = 10))
  })

  output$eig_plot <- renderPlot({
    plot_eig(eig_obj())
  })

  output$eig_post_plot <- renderPlot({
    plot_eig_posteriors(eig_obj())
  })

  output$ppv_table <- renderDT({
    datatable(round_df(ppv_obj()$ppv_range_data), options = list(pageLength = 10))
  })

  output$ppv_plot <- renderPlot({
    plot_ppv_range(ppv_obj())
  })

  output$ppv_eff_plot <- renderPlot({
    plot_effective_size_conf(ppv_obj())
  })

  output$eig_download_csv <- downloadHandler(
    filename = function() "eig_response_data.csv",
    content = function(file) {
      write.csv(eig_obj()$response_data, file, row.names = FALSE)
    }
  )

  output$ppv_download_csv <- downloadHandler(
    filename = function() "ppv_range_data.csv",
    content = function(file) {
      write.csv(ppv_obj()$ppv_range_data, file, row.names = FALSE)
    }
  )

  output$eig_download_png <- downloadHandler(
    filename = function() "eig_plot.png",
    content = function(file) {
      save_plot(plot_eig(eig_obj()), file)
    }
  )

  output$ppv_download_png <- downloadHandler(
    filename = function() "ppv_plot.png",
    content = function(file) {
      save_plot(plot_ppv_range(ppv_obj()), file)
    }
  )
}

shinyApp(ui, server)
