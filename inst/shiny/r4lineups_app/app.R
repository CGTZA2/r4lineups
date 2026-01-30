ui <- navbarPage(
  "r4lineups Studio",
  header = tags$head(
    tags$style(HTML(
      "body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
       .panel-heading { font-weight: 600; }
       .help-block { font-size: 12px; color: #555; }
       .download-btn { margin-top: 6px; }
       .section-title { margin-top: 10px; font-weight: 600; }"
    ))
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
        helpText("Enter a lineup vector (positions chosen by mock witnesses)."),
        textAreaInput("bias_vec", "Lineup vector", rows = 5,
                      placeholder = "e.g., 3,2,5,6,1,3,3,5,6,4"),
        numericInput("bias_target", "Target position", value = 3, min = 1),
        numericInput("bias_k", "Nominal lineup size (k)", value = 6, min = 2),
        numericInput("bias_boot", "Bootstrap R", value = 1000, min = 100, step = 100),
        actionButton("bias_example", "Load example (nortje2012$lineup_1)")
      ),
      mainPanel(
        tags$div(class = "section-title", "Summary"),
        verbatimTextOutput("bias_summary"),
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
        helpText("Effective size (Tredoux and Malpass variants)."),
        textAreaInput("esize_vec", "Lineup vector", rows = 5,
                      placeholder = "e.g., 3,2,5,6,1,3,3,5,6,4"),
        numericInput("esize_k", "Nominal lineup size (k)", value = 6, min = 2),
        numericInput("esize_boot", "Bootstrap R", value = 1000, min = 100, step = 100),
        actionButton("esize_example", "Load example (nortje2012$lineup_1)")
      ),
      mainPanel(
        tags$div(class = "section-title", "Summary"),
        verbatimTextOutput("esize_summary"),
        tags$div(class = "section-title", "Plot"),
        plotOutput("esize_plot"),
        downloadButton("esize_download_csv", "Download CSV", class = "download-btn"),
        downloadButton("esize_download_png", "Download Plot", class = "download-btn")
      )
    )
  ),
  tabPanel(
    "Face Similarity",
    sidebarLayout(
      sidebarPanel(
        helpText("Upload target face and foils. Requires Python deps."),
        fileInput("face_target", "Target image", accept = c(".jpg", ".jpeg", ".png")),
        fileInput("face_foils", "Foil images", multiple = TRUE,
                  accept = c(".jpg", ".jpeg", ".png")),
        shinyWidgets::pickerInput("face_model", "Model", choices = available_models(), selected = "ArcFace"),
        shinyWidgets::pickerInput("face_detector", "Detector", choices = available_detectors(), selected = "retinaface"),
        actionButton("face_check", "Check Python deps")
      ),
      mainPanel(
        tags$div(class = "section-title", "Status"),
        verbatimTextOutput("face_status"),
        tags$div(class = "section-title", "Similarity table"),
        DTOutput("face_table"),
        tags$div(class = "section-title", "Plot"),
        plotOutput("face_plot"),
        downloadButton("face_download_csv", "Download CSV", class = "download-btn"),
        downloadButton("face_download_png", "Download Plot", class = "download-btn")
      )
    )
  ),
  tabPanel(
    "ROC & CAC",
    sidebarLayout(
      sidebarPanel(
        helpText("Upload CSV with target_present, identification, confidence."),
        fileInput("roc_file", "Data CSV", accept = c(".csv")),
        textInput("roc_bins", "Confidence bins (optional)", value = "0, 60, 80, 100"),
        shinyWidgets::switchInput("roc_full", "Also compute full ROC", value = TRUE),
        actionButton("roc_example", "Load example (lineup_example)")
      ),
      mainPanel(
        tags$div(class = "section-title", "Data preview"),
        DTOutput("roc_table"),
        tags$div(class = "section-title", "Summary"),
        verbatimTextOutput("roc_summary"),
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
    "EIG & PPV",
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
  })

  observeEvent(input$eig_example, {
    data(lineup_example)
    updateTextInput(session, "eig_bins", value = "0, 60, 80, 100")
  })

  bias_vec <- reactive({
    parse_numeric_vector(input$bias_vec)
  })

  bias_allprop <- reactive({
    vec <- bias_vec()
    validate(need(length(vec) > 0, "Enter a lineup vector."))
    allprop(vec, k = input$bias_k)
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
    vec <- bias_vec()
    validate(need(length(vec) > 0, "Enter a lineup vector."))
    k <- input$bias_k
    target <- input$bias_target
    prop <- lineup_prop_vec(vec, target_pos = target, k = k)
    boot_obj <- boot::boot(vec, lineup_prop_boot, target_pos = target, R = input$bias_boot)
    ci <- boot::boot.ci(boot_obj, type = "perc")$percent[4:5]
    list(
      proportion = prop,
      ci_lower = ci[1],
      ci_upper = ci[2],
      chance = 1 / k
    )
  })

  output$bias_allprop <- renderDT({
    datatable(bias_allprop(), options = list(pageLength = 10))
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

  esize_vec <- reactive({
    parse_numeric_vector(input$esize_vec)
  })

  esize_summary <- reactive({
    vec <- esize_vec()
    validate(need(length(vec) > 0, "Enter a lineup vector."))
    k <- input$esize_k
    tab <- table(vec)
    e_t <- esize_T(tab)
    boot_obj <- boot::boot(tab, esize_T_boot, R = input$esize_boot)
    ci <- boot::boot.ci(boot_obj, type = "perc")$percent[4:5]
    data.frame(
      metric = "Tredoux E'",
      estimate = e_t,
      ci_lower = ci[1],
      ci_upper = ci[2],
      nominal_size = k
    )
  })

  esize_plot <- reactive({
    df <- esize_summary()
    ggplot(df, aes(x = metric, y = estimate)) +
      geom_col(fill = "steelblue") +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
      labs(x = NULL, y = "Effective size", title = "Tredoux Effective Size") +
      theme_minimal(base_size = 12)
  })

  output$esize_summary <- renderPrint({
    as.list(esize_summary()[1, ])
  })

  output$esize_plot <- renderPlot({
    esize_plot()
  })

  output$esize_download_csv <- downloadHandler(
    filename = function() "effective_size_summary.csv",
    content = function(file) {
      write.csv(esize_summary(), file, row.names = FALSE)
    }
  )

  output$esize_download_png <- downloadHandler(
    filename = function() "effective_size_plot.png",
    content = function(file) {
      save_plot(esize_plot(), file)
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

  face_table <- reactive({
    req(input$face_target, input$face_foils)
    target_path <- normalizePath(input$face_target$datapath)
    foil_paths <- normalizePath(input$face_foils$datapath)
    res <- lineup_similarity(
      target_path,
      foil_paths,
      model = input$face_model,
      detector = input$face_detector
    )
    res[, c("foil_name", "distance", "similarity", "verified", "rank")]
  })

  output$face_table <- renderDT({
    datatable(face_table(), options = list(pageLength = 10))
  })

  output$face_plot <- renderPlot({
    req(input$face_target, input$face_foils)
    target_path <- normalizePath(input$face_target$datapath)
    foil_paths <- normalizePath(input$face_foils$datapath)
    res <- lineup_similarity(
      target_path,
      foil_paths,
      model = input$face_model,
      detector = input$face_detector
    )
    plot_lineup_similarity(res)
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
      req(input$face_target, input$face_foils)
      target_path <- normalizePath(input$face_target$datapath)
      foil_paths <- normalizePath(input$face_foils$datapath)
      res <- lineup_similarity(
        target_path,
        foil_paths,
        model = input$face_model,
        detector = input$face_detector
      )
      save_plot(plot_lineup_similarity(res), file)
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

  output$roc_table <- renderDT({
    datatable(head(roc_data(), 10), options = list(pageLength = 10))
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
    roc_summary()
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
      eig = obj$eig,
      ppv_nominal = ppv$ppv_nominal$overall_ppv,
      ppv_effective = ppv$ppv_effective$overall_ppv,
      ppv_none = ppv$ppv_none$overall_ppv
    )
  })

  output$eig_table <- renderDT({
    datatable(eig_obj()$response_data, options = list(pageLength = 10))
  })

  output$eig_plot <- renderPlot({
    plot_eig(eig_obj())
  })

  output$eig_post_plot <- renderPlot({
    plot_eig_posteriors(eig_obj())
  })

  output$ppv_table <- renderDT({
    datatable(ppv_obj()$ppv_range_data, options = list(pageLength = 10))
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
