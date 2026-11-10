mod_receipt_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "review-heading", h2("Check expenses against receipts"),
      p("Select an expense on the left to view its receipt. Correct a match, add a note, and mark the line reviewed.")),
    uiOutput(ns("review_body"))
  )
}

receipt_preview_file <- function(path, folder) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "pdf") return(list(path = path, type = "application/pdf"))
  if (extension %in% c("png", "jpg", "jpeg")) return(list(path = path, type = if (extension == "png") "image/png" else "image/jpeg"))
  if (!requireNamespace("magick", quietly = TRUE)) stop("Preview conversion is unavailable. Download the original receipt instead.")
  converted <- tempfile("preview_", tmpdir = folder, fileext = ".pdf")
  magick::image_write(magick::image_read(path), path = converted, format = "pdf")
  list(path = converted, type = "application/pdf")
}

mod_receipt_review_server <- function(id, result, on_saved, on_download) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    draft <- reactiveVal(NULL)
    dirty <- reactiveVal(FALSE)
    selected_line <- reactiveVal(NA_integer_)
    preview_dir <- tempfile("receipt_previews_"); dir.create(preview_dir)
    session$onSessionEnded(function() unlink(preview_dir, recursive = TRUE))
    generation <- 0L
    discard <- function() {
      current <- result()
      draft(if (is.null(current)) NULL else read_receipt_review(current))
      dirty(FALSE)
    }
    observeEvent(result(), {
      generation <<- generation + 1L
      discard()
      state <- draft()
      selected_line(if (nrow(state$expenses)) state$expenses$source_row[1] else NA_integer_)
      updateSelectInput(session, "project", choices = c("All projects" = "", sort(unique(state$expenses$project_standard))), selected = "")
    }, ignoreNULL = TRUE, priority = 100)

    output$review_body <- renderUI({
      if (is.null(result())) return(div(class = "status-note", "Choose source files on the Financial reporting page and continue to receipt review."))
      state <- isolate(draft())
      projects <- sort(unique(state$expenses$project_standard))
      receipt_labels <- if (nrow(state$receipts)) paste0("[R", state$receipts$receipt_id, "] ", basename(state$receipts$receipt_file)) else character()
      receipt_choices <- c("No receipt" = "0", stats::setNames(as.character(state$receipts$receipt_id), receipt_labels))
      tagList(
        fluidRow(
          column(8, uiOutput(ns("progress"))),
          column(4, div(class = "review-actions",
            actionButton(ns("apply_edits"), "Finish review and generate reports", class = "btn-primary"),
            actionButton(ns("skip"), "Skip receipt review and generate reports")))
        ),
        div(class = "review-workspace", fluidRow(
          column(6, div(class = "review-panel",
            h3("Expense lines"),
            fluidRow(column(7, selectInput(ns("project"), "Project", choices = c("All projects" = "", projects))),
              column(5, checkboxInput(ns("open_only"), "Only lines needing attention", value = FALSE))),
            DT::DTOutput(ns("expenses")),
            div(class = "review-navigation", actionButton(ns("previous"), "Previous line"), actionButton(ns("next_line"), "Next line")),
            helpText("Search or sort the table, then click a line. Reviewed means you checked the line; reconciliation issues remain visible."))),
          column(6, div(class = "review-panel receipt-panel",
            uiOutput(ns("line_details")),
            selectizeInput(ns("receipt_choice"), "Receipt to view or assign", choices = receipt_choices, selected = as.character(isolate(current_receipt_id()))),
            uiOutput(ns("match_details")), uiOutput(ns("reassign_option")),
            textAreaInput(ns("note"), "Review note (optional)", rows = 2),
            div(class = "review-edit-actions", actionButton(ns("save_match"), "Save match"),
              actionButton(ns("unmatch"), "Remove current match"),
              actionButton(ns("save_note"), "Save note"),
              actionButton(ns("mark_reviewed"), "Mark line reviewed", class = "btn-primary"),
              actionButton(ns("mark_unreviewed"), "Reopen line")),
            downloadLink(ns("download_receipt"), "Download original receipt"),
            uiOutput(ns("receipt_preview"))
          ))
        ))
      )
    })

    table_data <- reactive({
      req(draft())
      table <- review_expense_table(draft())
      if (!is.null(input$project) && nzchar(input$project)) table <- table[table$Project == input$project, , drop = FALSE]
      if (isTRUE(input$open_only)) table <- table[table$Review != "Reviewed" | table$Check != "Matched", , drop = FALSE]
      table
    })
    output$expenses <- DT::renderDT({
      table <- table_data()
      DT::datatable(table, rownames = FALSE, escape = TRUE,
        selection = list(mode = "single", selected = which(table$Line == isolate(selected_line()))),
        options = list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE,
          language = list(emptyTable = "No expense lines in this view.", search = "Find expense:"))) |>
        DT::formatCurrency("Amount", currency = "$", digits = 2)
    }, server = FALSE)
    proxy <- DT::dataTableProxy("expenses", session = session)
    observeEvent(table_data()$Line, {
      ids <- table_data()$Line
      if (!selected_line() %in% ids) selected_line(if (length(ids)) ids[1] else NA_integer_)
    })
    observeEvent(input$expenses_rows_selected, {
      index <- input$expenses_rows_selected
      if (length(index) == 1L && index > 0L && index <= nrow(table_data())) selected_line(table_data()$Line[index])
    })
    current <- reactive({
      req(draft(), !is.na(selected_line()))
      row <- draft()$expenses[draft()$expenses$source_row == selected_line(), , drop = FALSE]
      req(nrow(row) == 1L)
      row
    })
    current_receipt_id <- reactive({
      if (is.null(draft()) || is.na(selected_line())) return(0L)
      assigned <- which(!is.na(draft()$assignments$source_row) & draft()$assignments$source_row == selected_line())
      if (length(assigned)) assigned[1] else 0L
    })
    # Updates happen when changing lines, not when browsing receipt candidates.
    observeEvent(list(selected_line(), draft()$assignments), {
      req(draft(), !is.na(selected_line()))
      receipts <- draft()$receipts
      labels <- if (nrow(receipts)) paste0("[R", receipts$receipt_id, "] ", basename(receipts$receipt_file)) else character()
      choices <- c("No receipt" = "0", stats::setNames(as.character(receipts$receipt_id), labels))
      updateSelectizeInput(session, "receipt_choice", choices = choices, selected = as.character(current_receipt_id()), server = TRUE)
      line <- match(selected_line(), draft()$lines$source_row)
      updateTextAreaInput(session, "note", value = draft()$lines$note[line])
      DT::selectRows(proxy, which(table_data()$Line == selected_line()))
    }, ignoreNULL = FALSE)
    move_line <- function(offset) {
      ids <- table_data()$Line
      ordered <- input$expenses_rows_all
      if (length(ordered)) ids <- ids[ordered]
      if (!length(ids)) return()
      index <- match(selected_line(), ids)
      if (is.na(index)) index <- 1L
      next_index <- max(1L, min(length(ids), index + offset))
      selected_line(ids[next_index])
      DT::selectPage(proxy, ceiling(next_index / 10))
    }
    observeEvent(input$previous, move_line(-1L))
    observeEvent(input$next_line, move_line(1L))
    candidate_id <- reactive({
      value <- suppressWarnings(as.integer(input$receipt_choice))
      if (length(value) != 1L || is.na(value) || is.null(draft()) || !value %in% c(0L, draft()$receipts$receipt_id)) return(0L)
      value
    })
    candidate <- reactive({
      req(draft(), candidate_id() > 0L)
      draft()$receipts[candidate_id(), , drop = FALSE]
    })
    output$line_details <- renderUI({
      if (is.na(selected_line())) return(tagList(h3("No expense line selected"), p("Choose a line from the table or generate reports.")))
      row <- current()
      tagList(h3(paste("Expense line", row$source_row)),
        p(tags$b(row$project_standard), " | ", row$vendor),
        p("Posted: ", row$transaction_date, " | Document date: ", if (is.na(row$document_date)) "Not supplied" else row$document_date),
        p(row$description), p(class = "expense-amount", paste0("GL amount: $", formatC(row$net_amount, digits = 2, format = "f"))))
    })
    output$match_details <- renderUI({
      req(draft(), !is.na(selected_line()))
      if (!candidate_id()) return(div(class = "warning-note", "No receipt selected. You can leave this line unmatched for later review."))
      receipt <- candidate()
      difference <- receipt$receipt_amount - current()$net_amount
      owner <- draft()$assignments$source_row[candidate_id()]
      tagList(
        div(class = if (is.finite(difference) && abs(difference) <= 0.01) "status-note" else "warning-note",
          if (!is.finite(difference)) "Receipt amount is missing from the filename. QC will remain pending."
          else paste0("Receipt: $", formatC(receipt$receipt_amount, digits = 2, format = "f"),
            " | Difference: $", formatC(difference, digits = 2, format = "f"),
            if (abs(difference) > 0.01) " - amount mismatch remains in QC." else " - amounts agree.")),
        if (!is.na(owner) && owner != selected_line()) p(class = "warning-note", paste("This receipt is assigned to expense line", owner, ". Moving it will leave that line unmatched and reopen its review.")),
        if (candidate_id() != current_receipt_id()) helpText("Previewing a different receipt does not change the match. Select Save match to use it.")
      )
    })
    output$reassign_option <- renderUI({
      req(draft())
      owner <- if (candidate_id() > 0L) draft()$assignments$source_row[candidate_id()] else NA_integer_
      if (!is.na(owner) && owner != selected_line()) checkboxInput(ns("allow_reassign"), "Move this receipt from its current expense line", FALSE)
    })
    edit <- function(receipt_id = NULL, reviewed = NULL) {
      tryCatch({
        state <- edit_receipt_review(draft(), selected_line(), receipt_id = receipt_id, reviewed = reviewed,
          note = if (is.null(input$note)) "" else input$note, reassign = isTRUE(input$allow_reassign))
        draft(state); dirty(TRUE)
      }, error = function(error) showNotification(conditionMessage(error), type = "error"))
    }
    observeEvent(input$save_match, edit(receipt_id = candidate_id()))
    observeEvent(input$unmatch, edit(receipt_id = 0L))
    observeEvent(input$save_note, edit())
    observeEvent(input$mark_reviewed, {
      if (candidate_id() != current_receipt_id()) {
        showNotification("Save or discard the candidate receipt selection before marking this line reviewed.", type = "warning")
      } else edit(reviewed = TRUE)
    })
    observeEvent(input$mark_unreviewed, edit(reviewed = FALSE))
    output$progress <- renderUI({
      req(draft())
      table <- review_expense_table(draft())
      tagList(div(class = "review-progress", tags$b(paste(sum(draft()$lines$reviewed), "of", nrow(table), "lines reviewed")),
        span(paste(sum(table$Check != "Matched"), "lines need receipt attention"))),
        p(if (dirty()) "Your saved matches and notes will be applied when you generate reports." else "Check receipts, then generate reports. You can also skip receipt review."))
    })
    observe({
      updateActionButton(session, "skip", label = if (dirty()) "Discard draft edits and generate reports" else "Skip remaining review and generate reports")
    })
    preview <- reactive({
      req(candidate())
      tryCatch({
        file <- receipt_preview_file(candidate()$receipt_file, preview_dir)
        url <- session$registerDataObj(ns(paste0("receipt_", generation, "_", candidate_id())), data = file,
          filterFunc = function(data, req) {
            if (!file.exists(data$path)) return(shiny::httpResponse(404, "text/plain", "Receipt is no longer available."))
            shiny::httpResponse(200, content_type = data$type,
              content = readBin(data$path, what = "raw", n = file.info(data$path)$size),
              headers = list("Cache-Control" = "private, no-store", "X-Content-Type-Options" = "nosniff"))
          })
        list(url = url, type = file$type)
      }, error = function(error) list(error = conditionMessage(error)))
    })
    output$receipt_preview <- renderUI({
      if (!candidate_id()) return(div(class = "receipt-empty", "Select a receipt to preview it here."))
      file <- preview()
      if (!is.null(file$error)) return(div(class = "warning-note", paste("Preview unavailable.", file$error, "Download the original receipt above.")))
      content <- if (identical(file$type, "application/pdf")) {
        tags$iframe(src = file$url, title = basename(candidate()$receipt_file), class = "receipt-preview", height = "760px", width = "100%")
      } else tags$img(src = file$url, alt = basename(candidate()$receipt_file), class = "receipt-image")
      tagList(p(tags$a(href = file$url, target = "_blank", rel = "noopener noreferrer", "Open full-size preview in a new tab")), content)
    })
    output$download_receipt <- downloadHandler(
      filename = function() basename(candidate()$receipt_file),
      content = function(file) { if (!file.copy(candidate()$receipt_file, file)) stop("Could not download receipt.") }
    )
    observeEvent(input$apply_edits, {
      req(result())
      tryCatch({
        updated <- result()
        if (dirty()) {
          updated <- withProgress(message = "Generating reports with reviewed matches", value = 0.5, {
            save_receipt_review(result(), draft(), reviewer = if (is.null(session$user)) "" else session$user)
          })
        } else {
          if (is.null(updated$project_deliverables)) updated <- withProgress(message = "Generating reports", value = 0.5,
            run_quarterly_pipeline(updated$paths, delivery = "financial_report"))
          record_receipt_review_decision(updated, "No review edits applied")
        }
        on_saved(updated)
        dirty(FALSE)
        showNotification("Reports generated. Choose a download on the Download reports page.", type = "message")
        on_download()
      }, error = function(error) showNotification(conditionMessage(error), type = "error", duration = NULL))
    })
    observeEvent(input$skip, {
      req(result())
      tryCatch({
        updated <- result()
        if (is.null(updated$project_deliverables)) updated <- withProgress(message = "Generating reports", value = 0.5,
          run_quarterly_pipeline(updated$paths, delivery = "financial_report"))
        record_receipt_review_decision(updated, "Remaining review skipped")
        discard()
        on_saved(updated)
        on_download()
      }, error = function(error) showNotification(conditionMessage(error), type = "error", duration = NULL))
    })
    list(dirty = reactive(dirty()), discard = discard)
  })
}
