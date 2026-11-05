library(shiny)
library(shinyFiles)

options(shiny.maxRequestSize = 100 * 1024^2)

project_root <- getOption("financial_reporting.root", normalizePath(getwd(), mustWork = TRUE))
if (!file.exists(file.path(project_root, "R", "quarterly_reporting_core.R"))) {
  project_root <- normalizePath(file.path(getwd(), ".."), mustWork = TRUE)
}
setwd(project_root)
source(file.path("R", "quarterly_reporting_core.R"))

current_quarter_start <- function() {
  month_number <- as.integer(format(Sys.Date(), "%m"))
  as.Date(sprintf("%s-%02d-01", format(Sys.Date(), "%Y"), ((month_number - 1L) %/% 3L) * 3L + 1L))
}

current_quarter_end <- function() {
  seq(current_quarter_start(), by = "3 months", length.out = 2L)[2] - 1
}

current_quarter_id <- function() {
  paste0(format(current_quarter_start(), "%Y"), "_Q", ((as.integer(format(current_quarter_start(), "%m")) - 1L) %/% 3L) + 1L)
}

uploaded_path <- function(upload) {
  if (is.null(upload) || is.null(upload$datapath) || length(upload$datapath) == 0 ||
      is.na(upload$datapath[1]) || !nzchar(upload$datapath[1])) {
    return("")
  }
  as.character(upload$datapath[1])
}

projects_from_gl <- function(upload) {
  path <- uploaded_path(upload)
  if (!nzchar(path) || !file.exists(path)) return(character())
  gl <- read_gl_export(path)
  normalized_names <- normalize_gl_headers(names(gl))
  project_column <- names(gl)[normalized_names == "grant_and_program_name"]
  projects <- trimws(as.character(gl[[project_column[1]]]))
  sort(unique(projects[!is.na(projects) & nzchar(projects)]))
}

projects_setting <- function(selected_projects, available_projects) {
  selected_projects <- trimws(as.character(selected_projects))
  selected_projects <- selected_projects[!is.na(selected_projects) & nzchar(selected_projects)]
  if (length(selected_projects) == 0 || length(available_projects) == 0 ||
      setequal(selected_projects, available_projects)) {
    return("All projects in GL")
  }
  paste(selected_projects, collapse = ";")
}

selected_project_values <- function(selected_projects, available_projects) {
  selected_projects <- trimws(as.character(selected_projects))
  selected_projects <- selected_projects[!is.na(selected_projects) & nzchar(selected_projects)]
  if (length(selected_projects) == 0 && length(available_projects) > 0) available_projects else selected_projects
}

suggest_mileage_sheet <- function(project, sheets) {
  if (length(sheets) == 0) return("")
  project_key <- gsub("[^a-z0-9]", "", tolower(project))
  sheet_keys <- gsub("[^a-z0-9]", "", tolower(sheets))
  contains_project <- vapply(sheet_keys, function(sheet_key) {
    grepl(project_key, sheet_key, fixed = TRUE) || grepl(sheet_key, project_key, fixed = TRUE)
  }, logical(1))
  if (any(contains_project)) return(sheets[which(contains_project)[1]])
  sheets[which.min(utils::adist(project_key, sheet_keys))]
}

mileage_mapping_id <- function(prefix, index) paste0(prefix, "_mileage_sheet_", index)

selected_mileage_sheet_map <- function(input, prefix, projects) {
  if (length(projects) == 0) return(character())
  mapped_sheets <- vapply(seq_along(projects), function(index) {
    value <- input[[mileage_mapping_id(prefix, index)]]
    if (is.null(value) || !nzchar(value)) "" else value
  }, character(1))
  stats::setNames(mapped_sheets[nzchar(mapped_sheets)], projects[nzchar(mapped_sheets)])
}

mileage_mapping_ui <- function(prefix, projects, sheets) {
  if (length(sheets) == 0) return(NULL)
  if (length(projects) == 0) {
    return(div(class = "warning-note", "Select GL projects to match them to mileage worksheets."))
  }
  tagList(
    h4("Match selected projects to mileage worksheets"),
    p("The app suggests matches, but confirm each one before building the report. Leave a project unselected if it has no mileage worksheet."),
    lapply(seq_along(projects), function(index) {
      selectInput(
        mileage_mapping_id(prefix, index),
        label = projects[index],
        choices = c("No mileage worksheet" = "", sheets),
        selected = suggest_mileage_sheet(projects[index], sheets)
      )
    })
  )
}

read_output <- function(paths, file_name) {
  path <- file.path(paths$internal, file_name)
  if (!file.exists(path)) return(data.frame(Message = paste("Output not available:", file_name)))
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

open_folder <- function(path) {
  if (dir.exists(path)) shell.exec(normalizePath(path, mustWork = TRUE))
}

page_help <- tagList(
  h3("How this proof of concept works"),
  p("Use the Quarterly report tab for the complete quarterly reporting cycle. Use the Smartsheet refresh tab whenever the dashboard data needs a newer GL-based refresh."),
  tags$ul(
    tags$li("Both paths keep their files separate and retain copied source inputs."),
    tags$li("Payroll is read directly from the GL. Receipts and mileage remain visible for review until their supporting sources are supplied."),
    tags$li("Receipts can be placed in one folder. Include the amount in each filename; add a date when available so the app can match it to the GL.")
  ),
  h4("Project selection"),
  p("After uploading a General Ledger, choose one or more projects from the dropdown. All projects are selected by default.")
)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f7f9fb; }
    .navbar { margin-bottom: 20px; }
    .well { background: #ffffff; border: 1px solid #d9e2ec; }
    .status-note { padding: 12px; border-left: 4px solid #2d6a8f; background: #edf6fb; margin: 12px 0; }
    .warning-note { padding: 12px; border-left: 4px solid #b7791f; background: #fffaf0; margin: 12px 0; }
    .table { background: #ffffff; }
  "))),
  navbarPage(
    title = "Financial Reporting",
    tabPanel(
      "Setup",
      fluidRow(
        column(
          7,
          wellPanel(
            h3("Required setup"),
            p("Select the local folder that contains the four private metadata files used by this app. These files are not stored in GitHub."),
            shinyFiles::shinyDirButton("metadata_choose", "Select metadata folder", "Choose the folder containing the metadata files"),
            verbatimTextOutput("metadata_path")
          )
        )
      )
    ),
    tabPanel(
      "Quarterly report",
      fluidRow(
        column(
          4,
          wellPanel(
            h3("Report setup"),
            textInput("quarter_id", "Report Name", value = ""),
            h4("Reporting dates"),
            fluidRow(
              column(6, dateInput("quarter_start", "Start date", value = NULL)),
              column(6, dateInput("quarter_end", "End date", value = NULL))
            ),
            fileInput("quarter_gl", "General Ledger", accept = c(".xlsx", ".xls", ".csv")),
            fileInput("quarter_mileage", "Mileage workbook (up to 100 MB)", accept = c(".xlsx", ".xls", ".csv")),
            selectizeInput("quarter_projects", "Projects to include", choices = character(), multiple = TRUE,
              options = list(placeholder = "Upload a General Ledger to choose projects")),
            uiOutput("quarter_mileage_mapping"),
            hr(),
            h4("Supporting inputs"),
            shinyFiles::shinyDirButton("quarter_receipts_choose", "Select receipt folder", "Choose the receipt folder"),
            verbatimTextOutput("quarter_receipts_path"),
            hr(),
            h4("Project deliverables"),
            shinyFiles::shinyDirButton("quarter_output_choose", "Select output folder", "Choose the parent folder for project deliverables"),
            verbatimTextOutput("quarter_output_path"),
            hr(),
            numericInput("quarter_mileage_rate", "Mileage reimbursement rate", value = 0.765, min = 0, step = 0.001),
            numericInput("quarter_fringe_rate", "Payroll fringe rate", value = 0.18, min = 0, step = 0.01),
            numericInput("quarter_indirect_rate", "Indirect rate", value = 0.10, min = 0, step = 0.01),
            actionButton("run_quarterly", "Build quarterly report package", class = "btn-primary"),
            br(), br(),
            actionButton("open_quarterly", "Open quarterly output folder")
          )
        ),
        column(
          8,
          h2("Report review"),
          div(class = "status-note", "The quarterly workflow uses the full input packet and produces an auditable report package. Missing items stay visible instead of becoming zero."),
          verbatimTextOutput("quarter_status"),
          h4("Input readiness"),
          tableOutput("quarter_readiness"),
          h4("Project scope"),
          tableOutput("quarter_scope"),
          h4("Reconciliation checks"),
          tableOutput("quarter_checks"),
          h4("Payroll from GL"),
          tableOutput("quarter_payroll_summary"),
          h4("Expense and receipt QC"),
          tableOutput("quarter_receipt_qc"),
          h4("Category report draft"),
          tableOutput("quarter_category")
        )
      )
    ),
    tabPanel(
      "Smartsheet refresh",
      fluidRow(
        column(
          4,
          wellPanel(
            h3("Frequent dashboard refresh"),
            textInput("smartsheet_id", "Name of refresh", value = paste0(current_quarter_id(), "_smartsheet-current")),
            h4("Reporting dates"),
            fluidRow(
              column(6, dateInput("smartsheet_start", "Start date", value = NULL)),
              column(6, dateInput("smartsheet_end", "End date", value = NULL))
            ),
            fileInput("smartsheet_gl", "Latest General Ledger", accept = c(".xlsx", ".xls", ".csv")),
            fileInput("smartsheet_mileage", "Mileage workbook (optional, up to 100 MB)", accept = c(".xlsx", ".xls", ".csv")),
            selectizeInput("smartsheet_projects", "Projects to include", choices = character(), multiple = TRUE,
              options = list(placeholder = "Upload a General Ledger to choose projects")),
            uiOutput("smartsheet_mileage_mapping"),
            div(class = "warning-note", "Payroll is read directly from the GL. Supply receipts and mileage when those amounts should be included in the draft."),
            shinyFiles::shinyDirButton("smartsheet_receipts_choose", "Select receipt folder", "Choose the receipt folder"),
            verbatimTextOutput("smartsheet_receipts_path"),
            numericInput("smartsheet_mileage_rate", "Mileage reimbursement rate", value = 0.765, min = 0, step = 0.001),
            numericInput("smartsheet_fringe_rate", "Payroll fringe rate", value = 0.18, min = 0, step = 0.01),
            numericInput("smartsheet_indirect_rate", "Indirect rate", value = 0.10, min = 0, step = 0.01),
            actionButton("run_smartsheet", "Build Smartsheet draft", class = "btn-primary"),
            br(), br(),
            actionButton("open_smartsheet", "Open Smartsheet output folder")
          )
        ),
        column(
          8,
          h2("Smartsheet refresh review"),
          div(class = "status-note", "This pathway is independent of quarterly report folders. It keeps a separate run history under smartsheet_refreshes."),
          verbatimTextOutput("smartsheet_status"),
          h4("Input readiness"),
          tableOutput("smartsheet_readiness"),
          h4("Reconciliation checks"),
          tableOutput("smartsheet_checks"),
          h4("Smartsheet draft"),
          tableOutput("smartsheet_draft")
        )
      )
    ),
    tabPanel("Help", page_help)
  )
)

server <- function(input, output, session) {
  quarter_receipts <- reactiveVal("")
  smartsheet_receipts <- reactiveVal("")
  quarter_output_root <- reactiveVal("")
  quarter_projects_available <- reactiveVal(character())
  smartsheet_projects_available <- reactiveVal(character())
  quarter_mileage_sheets <- reactiveVal(character())
  smartsheet_mileage_sheets <- reactiveVal(character())
  quarter_result <- reactiveVal(NULL)
  smartsheet_result <- reactiveVal(NULL)

  metadata_files <- c("ProjectCrosswalk.csv", "AccountCategoryMap.csv", "ProjectReportingRules.csv", "ByProjectCategoryBudgets.xlsx")
  metadata_default <- file.path(project_root, "data", "Metadata")
  metadata_folder <- reactiveVal(if (dir.exists(metadata_default)) normalizePath(metadata_default, mustWork = TRUE) else "")
  folder_roots <- shinyFiles::getVolumes()()

  selected_directory <- function(value) {
    selected <- tryCatch(shinyFiles::parseDirPath(folder_roots, value), error = function(error) character())
    if (length(selected) == 0 || is.na(selected[1]) || !nzchar(selected[1])) return("")
    normalizePath(selected[1], mustWork = FALSE)
  }

  metadata_problem <- function() {
    folder <- metadata_folder()
    if (!nzchar(folder) || !dir.exists(folder)) {
      return("Select the folder containing ProjectCrosswalk.csv, AccountCategoryMap.csv, ProjectReportingRules.csv, and ByProjectCategoryBudgets.xlsx on the Setup tab.")
    }
    missing <- metadata_files[!file.exists(file.path(folder, metadata_files))]
    if (length(missing) > 0) {
      return(paste("The selected metadata folder is missing:", paste(missing, collapse = ", ")))
    }
    NULL
  }

  shinyFiles::shinyDirChoose(input, "metadata_choose", roots = folder_roots, session = session)
  shinyFiles::shinyDirChoose(input, "quarter_receipts_choose", roots = folder_roots, session = session)
  shinyFiles::shinyDirChoose(input, "smartsheet_receipts_choose", roots = folder_roots, session = session)
  shinyFiles::shinyDirChoose(input, "quarter_output_choose", roots = folder_roots, session = session)

  observeEvent(input$metadata_choose, {
    folder <- selected_directory(input$metadata_choose)
    if (nzchar(folder)) metadata_folder(folder)
  })
  observeEvent(input$quarter_receipts_choose, {
    folder <- selected_directory(input$quarter_receipts_choose)
    if (nzchar(folder)) quarter_receipts(folder)
  })
  observeEvent(input$smartsheet_receipts_choose, {
    folder <- selected_directory(input$smartsheet_receipts_choose)
    if (nzchar(folder)) smartsheet_receipts(folder)
  })
  observeEvent(input$quarter_output_choose, {
    folder <- selected_directory(input$quarter_output_choose)
    if (nzchar(folder)) quarter_output_root(folder)
  })

  observeEvent(input$quarter_gl, {
    tryCatch({
      projects <- projects_from_gl(input$quarter_gl)
      quarter_projects_available(projects)
      updateSelectizeInput(session, "quarter_projects", choices = projects, selected = projects, server = TRUE)
    }, error = function(error) {
      quarter_projects_available(character())
      updateSelectizeInput(session, "quarter_projects", choices = character(), selected = character(), server = TRUE)
      showNotification(conditionMessage(error), type = "error", duration = NULL)
    })
  })

  observeEvent(input$smartsheet_gl, {
    tryCatch({
      projects <- projects_from_gl(input$smartsheet_gl)
      smartsheet_projects_available(projects)
      updateSelectizeInput(session, "smartsheet_projects", choices = projects, selected = projects, server = TRUE)
    }, error = function(error) {
      smartsheet_projects_available(character())
      updateSelectizeInput(session, "smartsheet_projects", choices = character(), selected = character(), server = TRUE)
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

  observeEvent(input$smartsheet_mileage, {
    tryCatch({
      path <- uploaded_path(input$smartsheet_mileage)
      smartsheet_mileage_sheets(if (nzchar(path)) mileage_workbook_sheets(path) else character())
    }, error = function(error) {
      smartsheet_mileage_sheets(character())
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

  output$smartsheet_mileage_mapping <- renderUI({
    mileage_mapping_ui(
      "smartsheet",
      selected_project_values(input$smartsheet_projects, smartsheet_projects_available()),
      smartsheet_mileage_sheets()
    )
  })

  output$metadata_path <- renderText({
    problem <- metadata_problem()
    if (is.null(problem)) paste("Metadata folder ready:", metadata_folder()) else problem
  })

  output$quarter_receipts_path <- renderText(if (nzchar(quarter_receipts())) quarter_receipts() else "No receipt folder selected")
  output$smartsheet_receipts_path <- renderText(if (nzchar(smartsheet_receipts())) smartsheet_receipts() else "No receipt folder selected")
  output$quarter_output_path <- renderText(if (nzchar(quarter_output_root())) quarter_output_root() else "No folder selected. Project deliverables will remain inside the retained report folder.")

  observeEvent(input$run_quarterly, {
    tryCatch({
      problem <- metadata_problem()
      if (!is.null(problem)) stop(problem)
      selected_projects <- selected_project_values(input$quarter_projects, quarter_projects_available())
      paths <- prepare_quarter(
        quarter_id = input$quarter_id,
        period_start = input$quarter_start,
        period_end = input$quarter_end,
        included_projects = projects_setting(selected_projects, quarter_projects_available()),
        general_ledger = uploaded_path(input$quarter_gl),
        receipt_folder = quarter_receipts(),
        mileage_log = uploaded_path(input$quarter_mileage),
        mileage_sheet_map = selected_mileage_sheet_map(input, "quarter", selected_projects),
        mileage_rate = input$quarter_mileage_rate,
        fringe_rate = input$quarter_fringe_rate,
        indirect_rate = input$quarter_indirect_rate,
        root = "quarters",
        project_output_root = quarter_output_root(),
        metadata_folder = metadata_folder()
      )
      quarter_result(run_quarterly_pipeline(paths, delivery = "quarterly_report"))
      showNotification("Quarterly report package created for review.", type = "message")
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "error", duration = NULL)
    })
  })

  observeEvent(input$run_smartsheet, {
    tryCatch({
      problem <- metadata_problem()
      if (!is.null(problem)) stop(problem)
      selected_projects <- selected_project_values(input$smartsheet_projects, smartsheet_projects_available())
      paths <- prepare_quarter(
        quarter_id = input$smartsheet_id,
        period_start = input$smartsheet_start,
        period_end = input$smartsheet_end,
        included_projects = projects_setting(selected_projects, smartsheet_projects_available()),
        general_ledger = uploaded_path(input$smartsheet_gl),
        receipt_folder = smartsheet_receipts(),
        mileage_log = uploaded_path(input$smartsheet_mileage),
        mileage_sheet_map = selected_mileage_sheet_map(input, "smartsheet", selected_projects),
        mileage_rate = input$smartsheet_mileage_rate,
        fringe_rate = input$smartsheet_fringe_rate,
        indirect_rate = input$smartsheet_indirect_rate,
        root = "smartsheet_refreshes",
        metadata_folder = metadata_folder()
      )
      smartsheet_result(run_quarterly_pipeline(paths, delivery = "smartsheet_draft"))
      showNotification("Smartsheet draft created for review.", type = "message")
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "error", duration = NULL)
    })
  })

  observeEvent(input$open_quarterly, {
    result <- quarter_result()
    if (is.null(result)) showNotification("Build a quarterly report package first.", type = "warning") else open_folder(result$paths$quarterly)
  })

  observeEvent(input$open_smartsheet, {
    result <- smartsheet_result()
    if (is.null(result)) showNotification("Build a Smartsheet draft first.", type = "warning") else open_folder(result$paths$smartsheet)
  })

  output$quarter_status <- renderText({
    result <- quarter_result()
    if (is.null(result)) "No quarterly report package has been created in this app session."
    else paste(
      "Saved quarterly package:", normalizePath(result$paths$quarterly, mustWork = FALSE),
      "\nProject deliverables:", normalizePath(result$project_deliverables$root, mustWork = FALSE)
    )
  })

  output$smartsheet_status <- renderText({
    result <- smartsheet_result()
    if (is.null(result)) "No Smartsheet refresh has been created in this app session."
    else paste("Saved Smartsheet draft:", normalizePath(result$paths$smartsheet, mustWork = FALSE))
  })

  output$quarter_readiness <- renderTable({ req(quarter_result()); read_output(quarter_result()$paths, "Input_Readiness.csv") }, striped = TRUE, bordered = TRUE)
  output$quarter_scope <- renderTable({ req(quarter_result()); read_output(quarter_result()$paths, "Project_Scope.csv") }, striped = TRUE, bordered = TRUE)
  output$quarter_checks <- renderTable({ req(quarter_result()); read_output(quarter_result()$paths, "Reconciliation_Checks.csv") }, striped = TRUE, bordered = TRUE)
  output$quarter_payroll_summary <- renderTable({ req(quarter_result()); read_output(quarter_result()$paths, "Payroll_GL_Summary.csv") }, striped = TRUE, bordered = TRUE)
  output$quarter_receipt_qc <- renderTable({ req(quarter_result()); read_output(quarter_result()$paths, "Expense_Receipt_QC.csv") }, striped = TRUE, bordered = TRUE)
  output$quarter_category <- renderTable({ req(quarter_result()); read_output(quarter_result()$paths, "Quarterly_Report_Category_Draft.csv") }, striped = TRUE, bordered = TRUE)

  output$smartsheet_readiness <- renderTable({ req(smartsheet_result()); read_output(smartsheet_result()$paths, "Input_Readiness.csv") }, striped = TRUE, bordered = TRUE)
  output$smartsheet_checks <- renderTable({ req(smartsheet_result()); read_output(smartsheet_result()$paths, "Reconciliation_Checks.csv") }, striped = TRUE, bordered = TRUE)
  output$smartsheet_draft <- renderTable({ req(smartsheet_result()); read_output(smartsheet_result()$paths, "Smartsheet_Draft.csv") }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)