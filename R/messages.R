## Show a warning to inform user that no data was selected
show_warning_no_data_selected <- function() {
    msg <- "Choose a processed file and/or at least one raw data file"
    showModal(modalDialog(title = "trackeRapp message",
                          div(tags$b(msg, class = "warningMessage")),
                          size = "s",
                          footer = tagList(
                              modalButton("Cancel"),
                              actionButton("uploadSampleDataset", "Upload sample dataset"))))
}

## Show a warning when too many sessions have been selected
show_warning_too_many_sessions <- function(nsessions) {
    msg <- paste("You have selected", nsessions, "sessions, which can result in an unstable 'Workouts view'. Do you want to continue with this selection?")
    showModal(modalDialog(title = "trackeRapp message",
                          div(tags$b(msg, class = "warningMessage")),
                          size = "s",
                          footer = tagList(
                              modalButton("No"),
                              actionButton("proceed_modal", "Yes"))))
}
