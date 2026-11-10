page_help <- tagList(
  h3("How this proof of concept works"),
  p("Build project reports and an automatic SmartsheetInput.xlsx summary from the Financial reporting tab. The summary has three rows per project for manual entry into Smartsheet."),
  tags$ul(
    tags$li("Each run includes copied source inputs, project reports, QC evidence, and the all-project summary in a downloadable ZIP."),
    tags$li("Payroll is read directly from the GL. Receipts and mileage remain visible for review until their supporting sources are supplied."),
    tags$li("Upload receipt PDFs or images together. Include the amount in each filename; add a date when available so the app can match it to the GL.")
  ),
  h4("Project selection"),
  p("After uploading a General Ledger, choose one or more projects from the dropdown. All projects are selected by default.")
)

app_ui <- function(request) {
  golem::add_resource_path("financialreports", app_sys("app", "www"))
  fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "financialreports/app.css")),
  navbarPage(
    title = div(class = "brand-lockup",
      tags$img(src = "financialreports/restore-our-shores-logo.jpg", class = "brand-logo", alt = "Restore Our Shores"),
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
      "Financial reporting",
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
            p("Select source files and projects on the main page.")
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
            h3("3. Upload receipts and build"),
            fileInput("quarter_receipts", "Receipt PDFs or images (multiple files)",
              multiple = TRUE, accept = c(".pdf", ".png", ".jpg", ".jpeg", ".tif", ".tiff", ".heic")),
            helpText("Keep the amount in each receipt filename, plus a date when available. Upload files from project subfolders together; the GL determines each match."),
            actionButton("run_quarterly", "Build project reports", class = "btn-primary"),
            uiOutput("quarter_downloads")
          ),
          div(class = "status-note", "Download the report bundle to save project workbooks, receipt packets, SmartsheetInput.xlsx, input snapshots, settings, and QC evidence. Files are removed from this session when you disconnect."),
          verbatimTextOutput("quarter_status"),
          tableOutput("quarter_checks")
        )
      )
    ),
    tabPanel("Help", page_help)
  )
)

}
