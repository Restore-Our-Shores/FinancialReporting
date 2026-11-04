# Quarterly Financial Reporting Pipeline
#
# This is the internal calculation engine used by the local Shiny app.
# The app supplies isolated quarter settings and writes review-ready files.
# It never treats a missing input as zero.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)
})

settings_path <- getOption("quarterly.settings_path", NA_character_)
if (is.na(settings_path) || !file.exists(settings_path)) {
  stop("RunQuarterlyReport.R is an internal engine. Start the workflow with shiny::runApp(\"shiny_app\", launch.browser = TRUE).")
}
crosswalk_path <- getOption("quarterly.crosswalk_path", file.path("data", "Metadata", "ProjectCrosswalk.csv"))
account_map_path <- getOption("quarterly.account_map_path", file.path("data", "Metadata", "AccountCategoryMap.csv"))
rules_path <- getOption("quarterly.rules_path", file.path("data", "Metadata", "ProjectReportingRules.csv"))
budget_path <- getOption("quarterly.budget_path", file.path("data", "Metadata", "ByProjectCategoryBudgets.xlsx"))

settings <- utils::read.csv(settings_path, check.names = FALSE, stringsAsFactors = FALSE)
setting_value <- function(name) {
  value <- settings$Value[settings$Setting == name]
  if (length(value) == 0 || is.na(value[1]) || !nzchar(trimws(value[1]))) return(NA_character_)
  trimws(value[1])
}
resolve_path <- function(value) {
  if (is.na(value) || !nzchar(value)) return(NA_character_)
  if (grepl("^[A-Za-z]:[/\\\\]", value) || startsWith(value, "/")) return(value)
  file.path(getwd(), value)
}
parse_number_safe <- function(x) {
  x <- trimws(as.character(x))
  negative <- str_detect(x, "^\\(.*\\)$")
  value <- suppressWarnings(as.numeric(str_remove_all(x, "[,$()]")))
  ifelse(negative & !is.na(value), -value, value)
}
parse_gl_date <- function(x) {
  x <- trimws(as.character(x))
  raw_number <- suppressWarnings(as.numeric(x))
  result <- as.Date(rep(NA_real_, length(x)), origin = "1899-12-30")
  is_number <- !is.na(raw_number)
  result[is_number] <- as.Date(raw_number[is_number], origin = "1899-12-30")
  text_date <- !is_number & !is.na(x) & nzchar(x)
  result[text_date] <- suppressWarnings(mdy(x[text_date]))
  result
}
read_optional_table <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  extension <- tolower(tools::file_ext(path))
  if (extension == "csv") return(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE))
  if (extension %in% c("xlsx", "xls")) return(as.data.frame(read_excel(path)))
  stop("Unsupported input type: ", extension, ". Use CSV, XLSX, or XLS.")
}
normal_names <- function(x) str_replace_all(str_to_lower(x), "[^a-z0-9]+", "_")
find_column <- function(data, patterns) {
  names_normal <- normal_names(names(data))
  matches <- which(vapply(names_normal, function(name) any(str_detect(name, patterns)), logical(1)))
  if (length(matches) == 0) return(NA_integer_)
  matches[1]
}
map_project <- function(project, crosswalk) {
  match_index <- match(trimws(as.character(project)), crosswalk[["Source Project Name"]])
  mapped <- crosswalk[["Standard Project Name"]][match_index]
  ifelse(is.na(mapped) | !nzchar(mapped), trimws(as.character(project)), mapped)
}
write_csv <- function(data, name, output_dir) {
  utils::write.csv(data, file.path(output_dir, name), row.names = FALSE, na = "")
}

gl_path <- resolve_path(setting_value("General ledger file"))
receipt_folder <- resolve_path(setting_value("Receipt folder"))
mileage_path <- resolve_path(setting_value("Mileage log file"))
payroll_path <- resolve_path(setting_value("Finance payroll summary file"))
prior_spend_path <- resolve_path(setting_value("Prior spend file"))
output_dir <- resolve_path(setting_value("Output folder"))
payroll_tolerance <- as.numeric(setting_value("Payroll QC tolerance"))
expense_tolerance <- as.numeric(setting_value("Expense receipt QC tolerance"))
period_start <- as.Date(setting_value("Reporting period start"))
period_end <- as.Date(setting_value("Reporting period end"))
period_label <- paste0("Q", ((month(period_start) - 1) %/% 3) + 1, ":", format(period_start, "%y"))
project_scope_value <- setting_value("Included projects")
project_scope <- if (is.na(project_scope_value) || !nzchar(trimws(project_scope_value)) || tolower(trimws(project_scope_value)) %in% c("all", "all projects", "all projects in gl")) {
  character(0)
} else {
  trimws(unlist(strsplit(project_scope_value, ";", fixed = TRUE)))
}

if (is.na(gl_path) || !file.exists(gl_path)) stop("General ledger file is missing. Supply it in the R Markdown parameter dialog.")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

crosswalk <- utils::read.csv(crosswalk_path, check.names = FALSE, stringsAsFactors = FALSE)
account_map <- utils::read.csv(account_map_path, check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character")
rules <- utils::read.csv(rules_path, check.names = FALSE, stringsAsFactors = FALSE)

# 1. Load and standardize the GL. The shared reader finds the transaction
# table even when a Finance export places its headings below report metadata.
gl_raw <- read_gl_export(gl_path)
project_column_candidates <- c("grant_and_program_name", "gl_and_program_name")
project_column_index <- which(normal_names(names(gl_raw)) %in% project_column_candidates)[1]
if (is.na(project_column_index)) stop("The GL needs a Grant and Program name column.")
project_column_name <- names(gl_raw)[project_column_index]

source_total <- gl_raw %>%
  filter(trimws(`Posted dt.`) == "Total") %>%
  transmute(value = parse_number_safe(Amount)) %>%
  pull(value)

standardized_gl <- gl_raw %>%
  mutate(
    source_row = row_number() + 1L,
    transaction_date = parse_gl_date(`Posted dt.`),
    document_date = parse_gl_date(`Doc dt.`),
    debit = coalesce(parse_number_safe(Debit), 0),
    credit = coalesce(parse_number_safe(Credit), 0),
    source_amount = parse_number_safe(Amount),
    gl_account = str_extract(GL, "^[0-9]+"),
    gl_account_description = str_trim(str_remove(GL, "^[0-9]+\\s*-\\s*")),
    project_original = str_trim(.data[[project_column_name]]),
    vendor = `Vendor name`,
    description = `Memo/Description`,
    document_number = Doc,
    transaction_number = `Txn No`,
    journal = JNL
  ) %>%
  filter(!is.na(transaction_date), !is.na(gl_account), nzchar(gl_account)) %>%
  mutate(
    project_standard = map_project(project_original, crosswalk),
    category = account_map[["Reporting Category"]][match(gl_account, account_map[["GL Account"]])],
    include_in_reports = account_map[["Include in Reports"]][match(gl_account, account_map[["GL Account"]])],
    category = coalesce(category, "Unmapped"),
    include_in_reports = coalesce(include_in_reports, "No"),
    net_amount = debit - credit,
    source_difference = net_amount - source_amount,
    source = "gl",
    source_file = basename(gl_path),
    duplicate_key = paste(transaction_date, document_date, document_number, transaction_number, journal, project_original, gl_account, vendor, debit, credit, description, sep = "|")
  ) %>%
  group_by(duplicate_key) %>%
  mutate(duplicate_count = n(), possible_duplicate = duplicate_count > 1L) %>%
  ungroup() %>%
  arrange(transaction_date, project_standard, gl_account, source_row)

if (length(project_scope) > 0) {
  standardized_gl <- standardized_gl %>%
    filter(project_original %in% project_scope | project_standard %in% project_scope)
  if (nrow(standardized_gl) == 0) {
    stop("No GL transactions matched Included projects. Use project names from the Grant and Program name column and separate multiple names with semicolons.")
  }
}
scoped_source_total <- if (length(project_scope) == 0 && length(source_total) > 0 && !is.na(source_total[1])) {
  source_total[1]
} else {
  sum(standardized_gl$source_amount, na.rm = TRUE)
}
project_scope_summary <- tibble(
  scope_setting = if (length(project_scope) == 0) "All projects in GL" else paste(project_scope, collapse = "; "),
  projects_included = length(unique(standardized_gl$project_standard)),
  transactions_included = nrow(standardized_gl),
  source_amount_in_scope = scoped_source_total
)

gl_actuals <- standardized_gl %>%
  filter(include_in_reports == "Yes", category %in% c("Payroll", "Expenses")) %>%
  group_by(project_standard, category) %>%
  summarise(gl_actual = sum(net_amount), .groups = "drop")

# 2. Reconcile a flat receipt folder to the GL. Receipt amounts are read from
# filenames; dates are optional but make the automatic match much stronger.
receipt_extensions <- "pdf|png|jpg|jpeg|tif|tiff|heic"
receipt_files <- if (!is.na(receipt_folder) && dir.exists(receipt_folder)) {
  list.files(receipt_folder, recursive = TRUE, full.names = TRUE, pattern = paste0("\\.(", receipt_extensions, ")$"), ignore.case = TRUE)
} else character(0)
receipt_amount <- function(file_name) {
  matches <- str_match_all(basename(file_name), "\\$([0-9][0-9,]*(?:\\.[0-9]{2})?)")[[1]]
  if (nrow(matches) == 0) return(NA_real_)
  parse_number_safe(matches[nrow(matches), 2])
}
receipt_date_from_filename <- function(file_name) {
  name <- basename(file_name)
  compact <- str_match(name, "(20[0-9]{2})([0-1][0-9])([0-3][0-9])")
  if (!is.na(compact[1, 1])) return(as.Date(paste(compact[1, 2], compact[1, 3], compact[1, 4], sep = "-")))
  separated <- str_match(name, "(20[0-9]{2})[-_. ]([0-1][0-9])[-_. ]([0-3][0-9])")
  if (!is.na(separated[1, 1])) return(as.Date(paste(separated[1, 2], separated[1, 3], separated[1, 4], sep = "-")))
  as.Date(NA)
}
receipt_vendor_key <- function(file_name) {
  value <- tools::file_path_sans_ext(basename(file_name))
  value <- str_remove_all(value, "(20[0-9]{2})[-_. ]?([0-1][0-9])[-_. ]?([0-3][0-9])")
  value <- str_remove_all(value, "\\$[0-9][0-9,]*(?:\\.[0-9]{2})?")
  value <- str_replace_all(value, "[^A-Za-z]+", " ")
  str_squish(str_to_lower(value))
}
receipt_root <- if (!is.na(receipt_folder) && dir.exists(receipt_folder)) normalizePath(receipt_folder) else NA_character_
receipt_inventory <- tibble(
  receipt_file = receipt_files,
  project_folder = if (length(receipt_files) == 0) character(0) else basename(dirname(receipt_files)),
  receipt_amount = if (length(receipt_files) == 0) numeric(0) else vapply(receipt_files, receipt_amount, numeric(1)),
  receipt_date = if (length(receipt_files) == 0) as.Date(character()) else as.Date(vapply(receipt_files, function(path) as.character(receipt_date_from_filename(path)), character(1))),
  receipt_vendor = if (length(receipt_files) == 0) character(0) else vapply(receipt_files, receipt_vendor_key, character(1))
) %>%
  mutate(
    project_hint = if_else(!is.na(receipt_root) & normalizePath(dirname(receipt_file)) != receipt_root, as.character(map_project(project_folder, crosswalk)), NA_character_),
    amount_status = if_else(is.na(receipt_amount), "Amount missing from filename", "Amount found in filename")
  )

expense_lines <- standardized_gl %>%
  filter(include_in_reports == "Yes", category == "Expenses", net_amount > 0) %>%
  mutate(match_date = if_else(!is.na(document_date), document_date, transaction_date))
assigned_receipt <- rep(NA_integer_, nrow(expense_lines))
matched_expense <- rep(NA_integer_, nrow(receipt_inventory))
match_status <- rep("Missing", nrow(receipt_inventory))
for (receipt_index in seq_len(nrow(receipt_inventory))) {
  receipt <- receipt_inventory[receipt_index, ]
  if (is.na(receipt$receipt_amount)) {
    match_status[receipt_index] <- "Review: amount missing from filename"
    next
  }
  candidates <- which(abs(expense_lines$net_amount - receipt$receipt_amount) <= expense_tolerance)
  if (!is.na(receipt$project_hint) && nzchar(receipt$project_hint)) {
    candidates <- candidates[expense_lines$project_standard[candidates] == receipt$project_hint]
  }
  if (!is.na(receipt$receipt_date)) {
    dated <- candidates[as.character(expense_lines$match_date[candidates]) == as.character(receipt$receipt_date)]
    if (length(dated) > 0) candidates <- dated
  }
  if (nzchar(receipt$receipt_vendor) && nchar(receipt$receipt_vendor) >= 3) {
    vendor_text <- str_to_lower(paste(expense_lines$vendor[candidates], expense_lines$description[candidates]))
    vendor_matches <- candidates[str_detect(vendor_text, fixed(receipt$receipt_vendor))]
    if (length(vendor_matches) > 0) candidates <- vendor_matches
  }
  available <- candidates[is.na(assigned_receipt[candidates])]
  if (length(available) == 1) {
    matched_expense[receipt_index] <- available
    assigned_receipt[available] <- receipt_index
    match_status[receipt_index] <- "Matched"
  } else if (length(available) == 0) {
    match_status[receipt_index] <- "Missing"
  } else {
    match_status[receipt_index] <- "Review: multiple GL candidates"
  }
}
receipt_reconciliation <- receipt_inventory %>%
  mutate(
    match_status = match_status,
    project_standard = if_else(is.na(matched_expense), NA_character_, expense_lines$project_standard[matched_expense]),
    transaction_date = if_else(is.na(matched_expense), as.Date(NA), expense_lines$transaction_date[matched_expense]),
    document_date = if_else(is.na(matched_expense), as.Date(NA), expense_lines$document_date[matched_expense]),
    packet_date = if_else(is.na(receipt_date), document_date, receipt_date),
    gl_vendor = if_else(is.na(matched_expense), NA_character_, expense_lines$vendor[matched_expense]),
    gl_description = if_else(is.na(matched_expense), NA_character_, expense_lines$description[matched_expense]),
    gl_amount = if_else(is.na(matched_expense), NA_real_, expense_lines$net_amount[matched_expense]),
    document_number = if_else(is.na(matched_expense), NA_character_, expense_lines$document_number[matched_expense])
  )
receipt_summary <- receipt_reconciliation %>%
  filter(match_status == "Matched") %>%
  group_by(project_standard) %>%
  summarise(receipt_packet_total = sum(receipt_amount), receipt_count = n(), .groups = "drop")

gl_expenses <- gl_actuals %>%
  filter(category == "Expenses") %>%
  transmute(project_standard, gl_expenses = gl_actual)
expense_receipt_qc <- full_join(gl_expenses, receipt_summary, by = "project_standard") %>%
  mutate(
    difference = receipt_packet_total - gl_expenses,
    status = case_when(
      length(receipt_files) == 0 ~ "Receipt packet missing",
      is.na(receipt_packet_total) ~ "No confirmed receipt-to-GL matches",
      is.na(gl_expenses) ~ "Receipt has no matching GL expense",
      abs(difference) <= expense_tolerance ~ "Pass",
      TRUE ~ "Review variance"
    )
  )

# 3. Finance payroll is the payroll-reporting source only after a GL tie-out.
payroll_raw <- tryCatch(read_optional_table(payroll_path), error = function(e) e)
payroll_detail <- tibble(project_standard = character(), payroll_date = character(), staff_name = character(), payroll_amount = numeric(), status = character())
payroll_qc <- gl_actuals %>%
  filter(category == "Payroll") %>%
  transmute(project_standard, gl_payroll = gl_actual)
if (inherits(payroll_raw, "error") || is.null(payroll_raw)) {
  payroll_qc <- payroll_qc %>% mutate(finance_payroll = NA_real_, difference = NA_real_, status = if (inherits(payroll_raw, "error")) payroll_raw$message else "Finance payroll summary missing")
} else {
  project_column <- find_column(payroll_raw, "project|grant|program")
  amount_column <- find_column(payroll_raw, "payroll.*amount|amount.*payroll|payroll")
  if (is.na(project_column) || is.na(amount_column)) {
    payroll_qc <- payroll_qc %>% mutate(finance_payroll = NA_real_, difference = NA_real_, status = "Finance payroll summary needs Project and Payroll Amount columns")
  } else {
    finance_payroll <- payroll_raw %>%
      transmute(project_standard = map_project(.data[[names(payroll_raw)[project_column]]], crosswalk), finance_payroll = parse_number_safe(.data[[names(payroll_raw)[amount_column]]])) %>%
      group_by(project_standard) %>% summarise(finance_payroll = sum(finance_payroll, na.rm = TRUE), .groups = "drop")
    payroll_qc <- full_join(payroll_qc, finance_payroll, by = "project_standard") %>%
      mutate(difference = finance_payroll - gl_payroll, status = case_when(is.na(finance_payroll) ~ "Finance payroll project missing", is.na(gl_payroll) ~ "Finance payroll has no matching GL", abs(difference) <= payroll_tolerance ~ "Pass", TRUE ~ "Review variance"))
    date_column <- find_column(payroll_raw, "pay.*date|date")
    staff_column <- find_column(payroll_raw, "staff|employee|name")
    if (!is.na(date_column) && !is.na(staff_column)) {
      payroll_detail <- payroll_raw %>%
        transmute(
          project_standard = map_project(.data[[names(payroll_raw)[project_column]]], crosswalk),
          payroll_date = as.character(parse_gl_date(.data[[names(payroll_raw)[date_column]]])),
          staff_name = as.character(.data[[names(payroll_raw)[staff_column]]]),
          payroll_amount = parse_number_safe(.data[[names(payroll_raw)[amount_column]]]),
          status = "Available"
        )
    }
  }
}

# 4. Mileage requires Maggie's log and a project rule with an approved rate.
mileage_raw <- tryCatch(read_optional_table(mileage_path), error = function(e) e)
mileage_summary <- tibble(project_standard = character(0), miles = numeric(0), mileage_cost = numeric(0), status = character(0))
if (inherits(mileage_raw, "error") || is.null(mileage_raw)) {
  mileage_summary <- tibble(project_standard = sort(unique(standardized_gl$project_standard)), miles = NA_real_, mileage_cost = NA_real_, status = if (inherits(mileage_raw, "error")) mileage_raw$message else "Mileage log missing")
} else {
  project_column <- find_column(mileage_raw, "^project$|grant.*code|grant|program")
  miles_column <- find_column(mileage_raw, "total.*miles|^miles$")
  if (is.na(project_column) || is.na(miles_column)) {
    mileage_summary <- tibble(project_standard = sort(unique(standardized_gl$project_standard)), miles = NA_real_, mileage_cost = NA_real_, status = "Mileage log needs Project and Miles columns")
  } else {
    mileage_summary <- mileage_raw %>%
      transmute(project_standard = map_project(.data[[names(mileage_raw)[project_column]]], crosswalk), miles = parse_number_safe(.data[[names(mileage_raw)[miles_column]]])) %>%
      group_by(project_standard) %>% summarise(miles = sum(miles, na.rm = TRUE), .groups = "drop") %>%
      left_join(rules %>% transmute(project_standard = `Standard Project Name`, mileage_rate = `Mileage Rate`), by = "project_standard") %>%
      mutate(mileage_cost = if_else(is.na(mileage_rate), NA_real_, miles * mileage_rate), status = if_else(is.na(mileage_rate), "Mileage rate missing", "Pass"))
  }
}

# 5. Contract balances need finance-confirmed prior spend. The existing
# metadata '- Spent' values remain comparison data, not prior-period actuals.
prior_raw <- tryCatch(read_optional_table(prior_spend_path), error = function(e) e)
prior_spend <- tibble(project_standard = character(0), category = character(0), prior_spend = numeric(0))
prior_status <- if (inherits(prior_raw, "error")) prior_raw$message else if (is.null(prior_raw)) "Prior spend file missing" else "Pass"
if (!inherits(prior_raw, "error") && !is.null(prior_raw)) {
  project_column <- find_column(prior_raw, "project|grant|program")
  category_column <- find_column(prior_raw, "^category$")
  amount_column <- find_column(prior_raw, "prior.*spend|spend.*prior")
  if (is.na(project_column) || is.na(category_column) || is.na(amount_column)) {
    prior_status <- "Prior spend file needs Project, Category, and Prior Spend columns"
  } else {
    prior_spend <- prior_raw %>% transmute(project_standard = map_project(.data[[names(prior_raw)[project_column]]], crosswalk), category = .data[[names(prior_raw)[category_column]]], prior_spend = parse_number_safe(.data[[names(prior_raw)[amount_column]]]))
  }
}

budget_metadata <- read_excel(budget_path) %>%
  transmute(project_standard = `Project Name`, item = Item, contracted_amount = as.numeric(Value), category = str_extract(Item, "^(Payroll|Expenses|Mileage)"), item_type = str_extract(Item, "(Budgeted|Spent)$"))
budgets <- budget_metadata %>% filter(item_type == "Budgeted", !is.na(category)) %>% select(project_standard, category, contracted_amount)
projects <- sort(unique(c(standardized_gl$project_standard, budgets$project_standard)))
categories <- c("Payroll", "Expenses", "Mileage")

report_categories <- expand.grid(project_standard = projects, category = categories, stringsAsFactors = FALSE) %>% as_tibble() %>%
  left_join(budgets, by = c("project_standard", "category")) %>%
  left_join(gl_actuals, by = c("project_standard", "category")) %>%
  left_join(payroll_qc %>% select(project_standard, finance_payroll, payroll_status = status), by = "project_standard") %>%
  left_join(expense_receipt_qc %>% select(project_standard, receipt_packet_total, expense_status = status), by = "project_standard") %>%
  left_join(mileage_summary %>% select(project_standard, miles, mileage_cost, mileage_status = status), by = "project_standard") %>%
  left_join(prior_spend, by = c("project_standard", "category")) %>%
  mutate(
    current_quarter_spend = case_when(
      category == "Payroll" & payroll_status == "Pass" ~ finance_payroll,
      category == "Expenses" & expense_status == "Pass" ~ gl_actual,
      category == "Mileage" & mileage_status == "Pass" ~ mileage_cost,
      TRUE ~ NA_real_
    ),
    remaining = if_else(!is.na(contracted_amount) & !is.na(prior_spend) & !is.na(current_quarter_spend), contracted_amount - prior_spend - current_quarter_spend, NA_real_),
    readiness = case_when(
      category == "Payroll" ~ payroll_status,
      category == "Expenses" ~ expense_status,
      category == "Mileage" ~ mileage_status,
      TRUE ~ "Review"
    ),
    prior_spend_status = prior_status
  ) %>% arrange(project_standard, factor(category, levels = categories))

smartsheet_draft <- report_categories %>%
  select(project_standard, category, current_quarter_spend, contracted_amount) %>%
  tidyr::pivot_wider(names_from = category, values_from = c(current_quarter_spend, contracted_amount)) %>%
  transmute(
    Project = project_standard,
    `Reporting Period` = period_label,
    Payroll = current_quarter_spend_Payroll,
    Expenses = current_quarter_spend_Expenses,
    Mileage = current_quarter_spend_Mileage,
    `Spent Amount` = if_else(!is.na(Payroll) & !is.na(Expenses) & !is.na(Mileage), Payroll + Expenses + Mileage, NA_real_),
    `Total Budget` = if_else(!is.na(contracted_amount_Payroll) & !is.na(contracted_amount_Expenses) & !is.na(contracted_amount_Mileage), contracted_amount_Payroll + contracted_amount_Expenses + contracted_amount_Mileage, NA_real_),
    `Percent Spent` = if_else(!is.na(`Spent Amount`) & !is.na(`Total Budget`) & `Total Budget` != 0, `Spent Amount` / `Total Budget`, NA_real_),
    `Report Status` = if_else(!is.na(`Percent Spent`) & prior_status == "Pass", "Ready for review", "Inputs or QC pending")
  )

input_readiness <- tibble(
  input = c("General ledger", "Receipt packet", "Mileage log from Maggie", "Finance payroll summary", "Prior spend history"),
  location = c(gl_path, receipt_folder, mileage_path, payroll_path, prior_spend_path),
  status = c(
    if_else(file.exists(gl_path), "Available", "Missing"),
    if_else(length(receipt_files) > 0, paste(length(receipt_files), "receipt file(s) found"), "Missing"),
    if_else(!is.na(mileage_path) && file.exists(mileage_path), "Available", "Missing"),
    if_else(!is.na(payroll_path) && file.exists(payroll_path), "Available", "Missing"),
    if_else(!is.na(prior_spend_path) && file.exists(prior_spend_path), "Available", "Missing")
  ),
  why_it_matters = c("Source transaction detail and GL controls.", "Expense-packet comparison.", "Mileage amount and mileage category actual.", "Payroll reporting amount and GL cross-check.", "Previous spend and remaining contract balance.")
)

reconciliation_checks <- tibble(
  check = c("GL source total less standardized net for selected projects", "Unmapped project transactions", "Unmapped account transactions", "Possible duplicate transactions", "Payroll QC projects passed", "Expense receipt QC projects passed"),
  value = c(sum(standardized_gl$net_amount) - scoped_source_total, sum(standardized_gl$project_standard == ""), sum(standardized_gl$category == "Unmapped"), sum(standardized_gl$possible_duplicate), sum(payroll_qc$status == "Pass"), sum(expense_receipt_qc$status == "Pass")),
  status = c(if_else(abs(sum(standardized_gl$net_amount) - scoped_source_total) < 0.005, "Pass", "Review"), if_else(any(standardized_gl$project_standard == ""), "Review", "Pass"), if_else(any(standardized_gl$category == "Unmapped"), "Review", "Pass"), if_else(any(standardized_gl$possible_duplicate), "Review", "Pass"), "Review", "Review")
)

write_csv(project_scope_summary, "Project_Scope.csv", output_dir)
write_csv(standardized_gl, "Standardized_GL_Transactions.csv", output_dir)
write_csv(input_readiness, "Input_Readiness.csv", output_dir)
write_csv(receipt_inventory, "Receipt_Inventory.csv", output_dir)
write_csv(receipt_reconciliation, "Receipt_Reconciliation.csv", output_dir)
write_csv(payroll_qc, "Payroll_GL_QC.csv", output_dir)
write_csv(payroll_detail, "Payroll_Detail.csv", output_dir)
write_csv(expense_receipt_qc, "Expense_Receipt_QC.csv", output_dir)
write_csv(mileage_summary, "Mileage_Summary.csv", output_dir)
write_csv(report_categories, "Quarterly_Report_Category_Draft.csv", output_dir)
write_csv(smartsheet_draft, "Smartsheet_Draft.csv", output_dir)
write_csv(reconciliation_checks, "Reconciliation_Checks.csv", output_dir)

writeLines(c(
  "Quarterly Financial Reporting Pipeline — Run Notes",
  paste("Reporting period:", period_label),
  "",
  "Use the files in this output folder in this order:",
  "1. Input_Readiness.csv — confirm every required source is available.",
  "2. Project_Scope.csv â€” confirm all or only the intended Grant and Program name values are included.",
  "3. Payroll_GL_QC.csv — resolve every variance before Payroll feeds the report.",
  "4. Expense_Receipt_QC.csv and Receipt_Inventory.csv — resolve packet variances and filenames with no dollar amount.",
  "5. Mileage_Summary.csv — confirm Maggie's miles and applicable project rate.",
  "6. Quarterly_Report_Category_Draft.csv — review funding, prior spend, current spend, and remaining balance.",
  "7. Smartsheet_Draft.csv — import only rows marked Ready for review."
), file.path(output_dir, "RUN_NOTES.txt"))

cat("Quarterly reporting pipeline complete. Review:", normalizePath(output_dir), "\n")
cat("Do not distribute quarterly reports or update Smartsheet until all relevant QC statuses are Pass.\n")
