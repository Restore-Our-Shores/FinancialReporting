page_help <- tagList(
  h3("How this proof of concept works"),
  p("Build project reports and a Smartsheet_Input.xlsx summary from the Financial reporting tab. The summary has one row per project with payroll, expenses, mileage, fringe, and indirect amount."),
  tags$ul(
    tags$li("Each run includes copied source inputs, project reports, QC evidence, and the all-project summary in a downloadable ZIP."),
    tags$li("Check expense lines beside receipt previews before generating reports. Correct matches and save notes, then finish review to generate reports. You may also skip receipt review and generate reports directly. Download the reports afterward."),
    tags$li("Payroll is read directly from the GL. Receipts and mileage remain visible for review until their supporting sources are supplied."),
    tags$li("Choose the folder containing receipt PDFs or images; subfolders are included. Include the amount in each filename; add a date when available so the app can match it to the GL.")
  ),
  h4("Project selection"),
  p("After uploading a General Ledger, choose one or more projects from the dropdown. All projects are selected by default.")
)

app_ui <- function(request) {
  golem::add_resource_path("financialreports", app_sys("app", "www"))
  fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "financialreports/app.css"),
    tags$script(src = "financialreports/receipt-folder.js")),
  navbarPage(
    title = div(class = "brand-lockup",
      tags$img(src = "financialreports/restore-our-shores-logo.jpg", class = "brand-logo", alt = "Restore Our Shores"),
      span(class = "brand-title", "Financial Reporting", tags$small("Project reporting"))
    ),
    id = "report_pages",
    windowTitle = "Financial Reporting",
    header = div(class = "app-hero",
      span(class = "eyebrow", "Restore Our Shores"),
      h1("Financial reporting"),
      p(class = "lead", "Bring your ledger, mileage, and receipts together to create project reports."),
      div(class = "workflow",
        span(class = "workflow-step", tags$b("1"), "Choose source files"),
        span(class = "workflow-step", tags$b("2"), "Select projects and dates"),
        span(class = "workflow-step", tags$b("3"), "Review receipts or skip"),
        span(class = "workflow-step", tags$b("4"), "Generate and download reports")
      )
    ),
    tabPanel(
      "Financial reporting", value = "report_builder",
      fluidRow(
        column(
          3,
          wellPanel(
            h3("Report inputs"),
            textInput("quarter_id", "Report Name", value = ""),
            h4("Reporting dates"),
            helpText("Dates fill from the imported GL. You can edit either date."),
            uiOutput("quarter_date_inputs"),
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
            h3("3. Choose receipts and review options"),
            fileInput("quarter_receipts", "Receipt folder (including subfolders)",
              multiple = TRUE, buttonLabel = "Choose folder…", placeholder = "No receipt folder selected"),
            helpText("Choose the folder where all receipts are stored. PDFs and supported images in subfolders are included. Keep the amount in each filename, plus a date when available. Receipts are optional."),
            checkboxInput("review_receipts", "Check receipts before generating reports", value = TRUE),
            actionButton("run_quarterly", "Continue to receipt review", class = "btn-primary"),
            uiOutput("build_next_steps")
          ),
          div(class = "status-note", "Download the project reports or Smartsheet_Input.xlsx after report generation. Files are removed from this session when you disconnect."),
          verbatimTextOutput("quarter_status"),
          tableOutput("quarter_checks")
        )
      )
    ),
    tabPanel("Review receipts", mod_receipt_review_ui("receipt_review"), value = "receipt_review"),
    tabPanel("Download reports", value = "downloads",
      h2("Download financial reports"),
      p("Download the full bundle to retain workbooks, receipt packets, source snapshots, QC files, and any review edits."),
      uiOutput("quarter_downloads"),
      tableOutput("download_checks"),
      actionButton("return_to_review", "Back to receipt review")),
    tabPanel("Help", page_help)
  )
)

}
