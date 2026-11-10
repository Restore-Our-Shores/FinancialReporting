app_server <- function(input, output, session) {
  session_root <- tempfile("financialreports_session_")
  dir.create(session_root, recursive = TRUE)
  session$onSessionEnded(function() unlink(session_root, recursive = TRUE))
  quarter_projects_available <- reactiveVal(character())
  quarter_mileage_sheets <- reactiveVal(character())
  quarter_reporting_dates <- reactiveVal(NULL)
  quarter_result <- reactiveVal(NULL)
  quarter_prepared <- reactiveVal(NULL)
  observe({
    updateActionButton(session, "run_quarterly", label = if (isTRUE(input$review_receipts))
      "Continue to receipt review" else "Skip receipt review and generate reports")
  })
  go_to_downloads <- function() updateNavbarPage(session, "report_pages", selected = "downloads")
  receipt_review <- mod_receipt_review_server("receipt_review", result = reactive(quarter_prepared()),
    on_saved = function(result) { quarter_result(result); quarter_prepared(result) }, on_download = go_to_downloads)
  observeEvent(input$return_to_review, updateNavbarPage(session, "report_pages", selected = "receipt_review"))
  observeEvent(input$start_review, updateNavbarPage(session, "report_pages", selected = "receipt_review"))
  observeEvent(input$skip_review, {
    req(quarter_prepared())
    tryCatch({
      result <- withProgress(message = "Generating reports", value = 0.5,
        run_quarterly_pipeline(quarter_prepared()$paths, delivery = "financial_report"))
      record_receipt_review_decision(result, "Review skipped")
      receipt_review$discard()
      quarter_result(result)
      quarter_prepared(result)
      go_to_downloads()
    }, error = function(error) showNotification(conditionMessage(error), type = "error", duration = NULL))
  })

  observeEvent(input$quarter_gl, {
    tryCatch({
      gl <- read_gl_export(uploaded_path(input$quarter_gl))
      projects <- projects_from_gl(input$quarter_gl, gl)
      quarter_projects_available(projects)
      updateSelectizeInput(session, "quarter_projects", choices = projects, selected = projects, server = TRUE)
      dates <- gl_reporting_dates(gl)
      if (!is.null(dates)) {
        quarter_reporting_dates(dates)
      } else {
        quarter_reporting_dates(NULL)
        showNotification("No valid posted dates were found in the GL. Enter the reporting dates manually.", type = "warning")
      }
    }, error = function(error) {
      quarter_projects_available(character())
      quarter_reporting_dates(NULL)
      updateSelectizeInput(session, "quarter_projects", choices = character(), selected = character(), server = TRUE)
      showNotification(conditionMessage(error), type = "error", duration = NULL)
    })
  })

  output$quarter_date_inputs <- renderUI({
    dates <- quarter_reporting_dates()
    dateInput("quarter_start", "Start date", value = if (is.null(dates)) NULL else dates$start,
      format = "yyyy-mm-dd", width = "100%") |>
      tagList(dateInput("quarter_end", "End date", value = if (is.null(dates)) NULL else dates$end,
        format = "yyyy-mm-dd", width = "100%"))
  })


  observeEvent(input$quarter_mileage, {
    tryCatch({
      path <- uploaded_path(input$quarter_mileage)
      quarter_mileage_sheets(if (nzchar(path)) mileage_workbook_sheets(path) else character())
    }, error = function(error) {
      quarter_mileage_sheets(character())
      showNotification(paste("Could not read mileage workbook:", conditionMessage(error)), type = "error", duration = NULL)
    })
  })


  output$quarter_mileage_mapping <- renderUI({
    mileage_mapping_ui(
      "quarter",
      selected_project_values(input$quarter_projects, quarter_projects_available()),
      quarter_mileage_sheets()
    )
  })


  observeEvent(input$run_quarterly, {
    tryCatch({
      if (!length(input$quarter_projects)) stop("Select at least one GL project.")
      selected_projects <- input$quarter_projects
      run_root <- tempfile("run_", tmpdir = session_root)
      dir.create(run_root)
      gl_path <- stage_upload(input$quarter_gl, file.path(run_root, "uploaded_gl"))
      mileage_path <- stage_upload(input$quarter_mileage, file.path(run_root, "uploaded_mileage"))
      receipts <- stage_receipts(input$quarter_receipts, file.path(run_root, "uploaded_receipts"))
      result <- build_reports(
        quarter_id = input$quarter_id,
        period_start = input$quarter_start,
        period_end = input$quarter_end,
        included_projects = projects_setting(selected_projects, quarter_projects_available()),
        general_ledger = gl_path,
        receipt_folder = receipts,
        mileage_log = mileage_path,
        mileage_sheet_map = selected_mileage_sheet_map(input, "quarter", selected_projects),
        mileage_rate = input$quarter_mileage_rate,
        fringe_rate = input$quarter_fringe_rate,
        indirect_rate = input$quarter_indirect_rate,
        root = file.path(run_root, "reports"),
        project_output_root = file.path(run_root, "project_reports"),
        prepare_only = TRUE
      )
      record_receipt_review_decision(result, "Not started")
      quarter_result(NULL)
      quarter_prepared(result)
      if (isTRUE(input$review_receipts)) {
        updateNavbarPage(session, "report_pages", selected = "receipt_review")
        showNotification("Receipts are ready to check. Generate reports when you finish reviewing.", type = "message")
      } else {
        generated <- withProgress(message = "Generating reports", value = 0.5,
          run_quarterly_pipeline(result$paths, delivery = "financial_report"))
        record_receipt_review_decision(generated, "Review skipped")
        receipt_review$discard()
        quarter_result(generated)
        quarter_prepared(generated)
        go_to_downloads()
      }
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "error", duration = NULL)
    })
  })


  output$build_next_steps <- renderUI({
    req(quarter_prepared())
    tagList(actionButton("start_review", "Review expenses and receipts"),
      actionButton("skip_review", "Skip receipt review and generate reports"))
  })
  output$quarter_downloads <- renderUI({
    if (is.null(quarter_result())) return(helpText("Generate a report first."))
    if (receipt_review$dirty()) return(div(class = "warning-note", "Receipt review has draft edits. Apply them or discard them on the Review receipts page before downloading."))
    state <- read_receipt_review(quarter_result())
    table <- review_expense_table(state)
    tagList(p(class = "status-note", paste("Receipt review:", sum(state$lines$reviewed), "of", nrow(state$lines), "lines checked;", sum(table$Check != "Matched"), "lines require receipt attention. Review QC before sharing reports.")),
      downloadButton("download_reports", "Download project reports"),
      downloadButton("download_smartsheet", "Download Smartsheet_Input.xlsx"))
  })
  output$download_reports <- downloadHandler(
    filename = function() paste0(basename(quarter_result()$paths$quarter), "_", report_date_suffix(quarter_result()$paths$settings), ".zip"),
    content = function(file) {
      req(quarter_result(), !receipt_review$dirty())
      export_report_bundle(quarter_result(), file)
    }, contentType = "application/zip"
  )
  output$download_smartsheet <- downloadHandler(
    filename = function() basename(quarter_result()$project_deliverables$smartsheet_summary),
    content = function(file) {
      req(quarter_result(), !receipt_review$dirty())
      if (!file.copy(quarter_result()$project_deliverables$smartsheet_summary, file)) {
        stop("Could not download the summary.")
      }
    }
  )
  output$download_checks <- renderTable({
    req(quarter_result())
    quarter_result()$checks
  })
  output$quarter_checks <- renderTable({
    req(quarter_result())
    quarter_result()$checks
  })

  output$quarter_status <- renderText({
    result <- quarter_result()
    if (is.null(result)) {
      if (is.null(quarter_prepared())) "No project reports have been created in this app session."
      else "Receipts are ready for review. Finish or skip review to generate reports."
    }
    else paste("Reports are ready to download.",
      "Review the reconciliation checks and the bundle's QC files before distributing reports.",
      "Download the bundle before leaving this page; session files are temporary.", sep = "\n")
  })


}
