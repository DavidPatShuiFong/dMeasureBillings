# rintrojs introduction for billings

#' rintrojs steps for billings module
#'
#' @title steps_introduction_df
#' @param element_name the HTML datatable element, including datatable and helpers such as print/copy view
#' @description returns a dataframe of rintrojs steps
#'
#' requires pro-forma steps from DailyMeasure
#'
#' @export
steps_introduction_df <- function(element_name) {
  steps_df <-
    data.frame(element = as.character(NA),
               intro = c(paste(shiny::tags$h4("GPstat! Billings"),
                               shiny::br(),
                               "View billings for chosen clinicians and dates.",
                               shiny::br(), shiny::br(),
                               "If a selected clinician does not have a subscription/license",
                               "then billings will only be shown if at least a week old.")),
               position = "auto",
               stringsAsFactors = FALSE) %>>%
    rbind(DailyMeasure::steps_choose_clinician_date_df()) %>>%
    rbind(data.frame(element = element_name,
                     intro = c(paste(shiny::tags$h4("Billing view"),
                                     shiny::br(),
                                     "List of billings",
                                     "according to currently selected clinicians and dates.",
                                     shiny::br(), shiny::br(),
                                     "'Show all day's billings' will include billings done for patients",
                                     "seen by the selected clinicians during the selected dates (according to",
                                     "the appointment book), but billed by another provider.",
                                     shiny::br(), shiny::br(),
                                     "By default shows : ", shiny::strong("Patient"), "(name),",
                                     shiny::strong("DOB/Age"), " and ",
                                     shiny::strong("Billings"), " list.",
                                     shiny::br(), shiny::br(),
                                     "Also",
                                     "shows Appointment details, status and visit type e.g.", shiny::strong("AppointmentTime"),
                                     "and", shiny::strong("Provider"), " (clinician).")),
                     position = "auto")) %>>%
    rbind(DailyMeasure::steps_datatable_helpers(element_name)) %>>%
    rbind(data.frame(element = element_name,
                     intro = c(paste(shiny::tags$h4("Show all day's billings"),
                                     shiny::br(),
                                     shiny::icon("gear"), shiny::br(),
                                     "Top-right of the table view.",
                                     shiny::br(), shiny::br(),
                                     "Include billings done for patients",
                                     "seen by the selected clinician during the selected dates (according to",
                                     "the appointment book), but billed by another provider.",
                                     shiny::br(), shiny::br(),
                                     "For example, show billings for a patient seen by a clinician who",
                                     "provides services on behalf of another practitioner e.g. a nurse",
                                     shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto"))

  return(steps_df)

}
