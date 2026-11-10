app_server <- function(input, output, session) {
  session_root <- tempfile("financialreports_session_")
  dir.create(session_root, recursive = TRUE)
  session$onSessionEnded(function() unlink(session_root, recursive = TRUE))
  quarter_projects_available <- reactiveVal(character())
  quarter_mileage_sheets <- reactiveVal(character())
  quarter_result <- reactiveVal(NULL)

  observeEvent(input$quarter_gl, {
    tryCatch({
      gl <- read_gl_export(uploaded_path(input$quarter_gl))
      projects <- projects_from_gl(input$quarter_gl, gl)
      quarter_projects_available(projects)
      updateSelectizeInput(session, "quarter_projects", choices = projects, selected = projects, server = TRUE)
      dates <- gl_reporting_dates(gl)
      if (!is.null(dates)) {
        updateDateInput(session, "quarter_start", value = dates$start)
        updateDateInput(session, "quarter_end", value = dates$end)
      } else {
        showNotification("No valid posted dates were found in the GL. Enter the reporting dates manually.", type = "warning")
      }
    }, error = function(error) {
      quarter_projects_available(character())
      updateSelectizeInput(session, "quarter_projects", choices = character(), selected = character(), server = TRUE)
      showNotification(conditionMessage(error), type = "error", duration = NULL)
    })
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
      quarter_result(NULL)
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
        project_output_root = file.path(run_root, "project_reports")
      )
      quarter_result(result)
      showNotification("Reports are ready. Download the bundle to retain your reports and QC evidence.", type = "message")
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "error", duration = NULL)
    })
  })


  output$quarter_downloads <- renderUI({
    req(quarter_result())
    tagList(downloadButton("download_reports", "Download reports and audit bundle"),
      downloadButton("download_smartsheet", "Download SmartsheetInput.xlsx"))
  })
  output$download_reports <- downloadHandler(
    filename = function() paste0(basename(quarter_result()$paths$quarter), ".zip"),
    content = function(file) {
      req(quarter_result())
      export_report_bundle(quarter_result(), file)
    }, contentType = "application/zip"
  )
  output$download_smartsheet <- downloadHandler(
    filename = function() "SmartsheetInput.xlsx",
    content = function(file) {
      req(quarter_result())
      if (!file.copy(quarter_result()$project_deliverables$smartsheet_summary, file)) {
        stop("Could not download the summary.")
      }
    }
  )
  output$quarter_checks <- renderTable({
    req(quarter_result())
    quarter_result()$checks
  })

  output$quarter_status <- renderText({
    result <- quarter_result()
    if (is.null(result)) "No project reports have been created in this app session."
    else paste("Reports are ready to download.",
      "Review the reconciliation checks and the bundle's QC files before distributing reports.",
      "Download the bundle before leaving this page; session files are temporary.", sep = "\n")
  })


}
