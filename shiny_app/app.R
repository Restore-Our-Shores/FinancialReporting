library(shiny)
library(shinyFiles)

options(shiny.maxRequestSize = 100 * 1024^2)

project_root <- getOption("financial_reporting.root", normalizePath(getwd(), mustWork = TRUE))
if (!file.exists(file.path(project_root, "R", "quarterly_reporting_core.R"))) {
  project_root <- normalizePath(file.path(getwd(), ".."), mustWork = TRUE)
}
options(financial_reporting.root = project_root)
source(file.path(project_root, "R", "quarterly_reporting_core.R"))

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

projects_from_gl <- function(upload, gl = NULL) {
  path <- uploaded_path(upload)
  if (!nzchar(path) || !file.exists(path)) return(character())
  if (is.null(gl)) gl <- read_gl_export(path)
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
  if (length(sheets) == 0) return(helpText("Upload a mileage workbook to choose its worksheets. CSV logs use their Project column instead."))
  if (length(projects) == 0) {
    return(div(class = "warning-note", "Select GL projects to match them to mileage worksheets."))
  }
  tagList(
    p("For each GL project below, choose the worksheet that contains its mileage. Worksheet names can differ from GL project names. Confirm each suggested match, or choose No mileage worksheet."),
    lapply(seq_along(projects), function(index) {
      selectInput(
        mileage_mapping_id(prefix, index),
        label = paste("Mileage worksheet for", projects[index]),
        choices = c("No mileage worksheet" = "", sheets),
        selected = suggest_mileage_sheet(projects[index], sheets)
      )
    })
  )
}

open_folder <- function(path) {
  if (dir.exists(path)) shell.exec(normalizePath(path, mustWork = TRUE))
}

page_help <- tagList(
  h3("How this proof of concept works"),
  p("Build project reports and an automatic SmartsheetInput.xlsx summary from the Quarterly report tab. The summary has three rows per project for manual entry into Smartsheet."),
  tags$ul(
    tags$li("Each run retains copied source inputs and writes project reports plus the all-project summary to the selected output folder."),
    tags$li("Payroll is read directly from the GL. Receipts and mileage remain visible for review until their supporting sources are supplied."),
    tags$li("Receipts can be placed in one folder. Include the amount in each filename; add a date when available so the app can match it to the GL.")
  ),
  h4("Project selection"),
  p("After uploading a General Ledger, choose one or more projects from the dropdown. All projects are selected by default.")
)

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
  navbarPage(
    title = div(class = "brand-lockup",
      tags$img(src = "restore-our-shores-logo.jpg", class = "brand-logo", alt = "Restore Our Shores"),
      span(class = "brand-title", "Financial Reporting", tags$small("Project reporting"))
    ),
    windowTitle = "Financial Reporting",
    header = div(class = "app-hero",
      span(class = "eyebrow", "Restore Our Shores"),
      h1("Financial reporting"),
      p(class = "lead", "Bring your ledger, mileage, and receipts together to create project reports."),
      div(class = "workflow",
        span(class = "workflow-step", tags$b("1"), "Choose source files"),
        span(class = "workflow-step", tags$b("2"), "Select projects and dates"),
        span(class = "workflow-step", tags$b("3"), "Build reports")
      )
    ),
    tabPanel(
      "Quarterly report",
      fluidRow(
        column(
          3,
          wellPanel(
            h3("Report inputs"),
            textInput("quarter_id", "Report Name", value = ""),
            h4("Reporting dates"),
            helpText("Dates fill from the imported GL. You can edit either date."),
            dateInput("quarter_start", "Start date", value = NULL, format = "yyyy-mm-dd", width = "100%"),
            dateInput("quarter_end", "End date", value = NULL, format = "yyyy-mm-dd", width = "100%"),
            hr(),
            numericInput("quarter_mileage_rate", "Mileage reimbursement rate", value = 0.765, min = 0, step = 0.001),
            numericInput("quarter_fringe_rate", "Payroll fringe rate", value = 0.18, min = 0, step = 0.01),
            numericInput("quarter_indirect_rate", "Indirect rate", value = 0.10, min = 0, step = 0.01),
            p("Select source files, projects, and folders on the main page.")
          )
        ),
        column(
          9,
          h2("Build project reports"),
          wellPanel(
            h3("1. Choose GL projects"),
            p("Upload your General Ledger, then choose which projects should receive reports. All GL projects are included by default."),
            fileInput("quarter_gl", "General Ledger", accept = c(".xlsx", ".xls", ".csv")),
            selectizeInput("quarter_projects", "Projects to include", choices = character(), multiple = TRUE,
              options = list(placeholder = "Upload a General Ledger to choose projects"))
          ),
          wellPanel(
            h3("2. Match mileage worksheets"),
            fileInput("quarter_mileage", "Mileage workbook (up to 100 MB)", accept = c(".xlsx", ".xls", ".csv")),
            uiOutput("quarter_mileage_mapping")
          ),
          wellPanel(
            h3("3. Choose folders and build"),
            fluidRow(
              column(6,
                h4("Receipts"),
                shinyFiles::shinyDirButton("quarter_receipts_choose", "Select receipts directory", "Choose the directory containing receipt PDFs or images"),
                verbatimTextOutput("quarter_receipts_path")
              ),
              column(6,
                h4("Project report output"),
                shinyFiles::shinyDirButton("quarter_output_choose", "Select output folder", "A folder for each selected project will be created here"),
                verbatimTextOutput("quarter_output_path")
              )
            ),
            actionButton("run_quarterly", "Build project reports", class = "btn-primary"),
            actionButton("open_quarterly", "Open selected output folder")
          ),
          div(class = "status-note", "The selected output folder receives one folder per project plus SmartsheetInput.xlsx, a single worksheet with Expenses, Payroll, and Mileage rows for every included project."),
          verbatimTextOutput("quarter_status")
        )
      )
    ),
    tabPanel("Help", page_help)
  )
)

server <- function(input, output, session) {
  quarter_receipts <- reactiveVal("")
  quarter_output_root <- reactiveVal("")
  quarter_projects_available <- reactiveVal(character())
  quarter_mileage_sheets <- reactiveVal(character())
  quarter_result <- reactiveVal(NULL)

  folder_roots <- shinyFiles::getVolumes()()

  selected_directory <- function(value) {
    selected <- tryCatch(shinyFiles::parseDirPath(folder_roots, value), error = function(error) character())
    if (length(selected) == 0 || is.na(selected[1]) || !nzchar(selected[1])) return("")
    normalizePath(selected[1], mustWork = FALSE)
  }

  shinyFiles::shinyDirChoose(input, "quarter_receipts_choose", roots = folder_roots, session = session)
  shinyFiles::shinyDirChoose(input, "quarter_output_choose", roots = folder_roots, session = session)

  observeEvent(input$quarter_receipts_choose, {
    folder <- selected_directory(input$quarter_receipts_choose)
    if (nzchar(folder)) quarter_receipts(folder)
  })
  observeEvent(input$quarter_output_choose, {
    folder <- selected_directory(input$quarter_output_choose)
    if (nzchar(folder)) quarter_output_root(folder)
  })

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


  output$quarter_receipts_path <- renderText(if (nzchar(quarter_receipts())) quarter_receipts() else "No receipts directory selected")
  output$quarter_output_path <- renderText(if (nzchar(quarter_output_root())) quarter_output_root() else "No output folder selected")

  observeEvent(input$run_quarterly, {
    tryCatch({
      if (!nzchar(quarter_output_root())) stop("Select an output folder before building project reports.")
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
        root = file.path(project_root, "quarters"),
        project_output_root = quarter_output_root()
      )
      quarter_result(run_quarterly_pipeline(paths, delivery = "quarterly_report"))
      showNotification("Project reports and SmartsheetInput.xlsx were created in the selected output folder.", type = "message")
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "error", duration = NULL)
    })
  })


  observeEvent(input$open_quarterly, {
    if (!nzchar(quarter_output_root())) showNotification("Select an output folder first.", type = "warning") else open_folder(quarter_output_root())
  })


  output$quarter_status <- renderText({
    result <- quarter_result()
    if (is.null(result)) "No project reports have been created in this app session."
    else paste(
      "Project report folders:", normalizePath(result$project_deliverables$root, mustWork = FALSE),
      "\nSmartsheet summary:", normalizePath(result$project_deliverables$smartsheet_summary, mustWork = FALSE)
    )
  })


}

shinyApp(ui, server)
