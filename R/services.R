#' billings
#'
#' MBS (medicare benefits schedule) item numbers for CDM
#'
#' @name billings
#'
#' @include Billings.R
NULL

.public(
  dMeasureBillings, "appointments_billings",
  data.frame(
    Patient = character(), InternalID = integer(),
    AppointmentDate = as.Date(integer(0), origin = "1970-01-01"),
    AppointmentTime = character(), Provider = character(),
    DOB = as.Date(integer(0), origin = "1970-01-01"),
    Age = numeric(),
    ServiceDate = as.Date(integer(0), origin = "1970-01-01"),
    MBSItem = integer(), Description = character()
  )
)

# appointment list with billings
# collects ALL billings for patients who have displayed appointments
# used by billings view, and CDM billings view
# requires appointments_list

#' List of appointments with billings
#'
#' Filtered by date, and chosen clinicians
#'
#' Billings for patients who have displayed appointments
#'
#' collects ALL billings for patients who have displayed appointments
#' used by CDM billings view
#'
#' @param dMeasureBillings_obj dMeasureBillings R6 object
#' @param date_from start date, inclusive (date object)
#' @param date_to end date, inclusive (date object)
#'  default of date_from and date_to defined by choose_date method of dMeasure
#' @param clinicians (default $clinicians of dMeasure object) list of clinicians to view
#' @param status (default NA) filter by 'status' if not NA
#'  permissible values are 'Booked', 'Completed', 'At billing',
#'  'Waiting', 'With doctor'
#' @param lazy (default FALSE) if lazy=TRUE, then don't re-calculate appointments_filtered to calculate
#'
#' @return list of appointments
#' @export
billed_appointments <- function(dMeasureBillings_obj,
                                date_from = NA, date_to = NA,
                                clinicians = NA,
                                status = NA,
                                lazy = FALSE) {
  dMeasureBillings_obj$billed_appointments(dMeasureBillings_obj, date_from, date_to, clinicians, status, lazy)
}

.public(
  dMeasureBillings, "billed_appointments",
  function(date_from = NA,
             date_to = NA,
             clinicians = NA,
             status = NA,
             lazy = FALSE) {
    if (is.na(date_from)) {
      date_from <- self$dM$date_a
    }
    if (is.na(date_to)) {
      date_to <- self$dM$date_b
    }
    if (all(is.na(clinicians))) {
      clinicians <- self$dM$clinicians
    }

    clinicians <- intersect(
      clinicians,
      self$dM$clinician_list(view_name = "billings")
    )
    # this view is potentially restricted. if 'GlobalBillView' restriction
    # is enabled, reduce the number of clinicians shown, if the authorized
    # user does not have 'GlobalBillView' attribute

    if (is.null(clinicians) || length(clinicians) == 0) {
      clinicians <- c("")
    }

    if (self$dM$emr_db$is_open()) {
      # only if EMR database is open

      if (!lazy) {
        self$dM$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
        # if not 'lazy' evaluation, then re-calculate self$appointments_list
        # (that is automatically done by calling the $list_appointments method)
      }

      intID <- c(self$dM$appointments_list %>>% dplyr::pull(InternalID), -1)

      self$appointments_billings <-
        self$dM$appointments_list %>>%
        dplyr::left_join(self$dM$db$services %>>%
          dplyr::filter(
            InternalID %in% intID,
            ServiceDate <= date_to
          ),
        by = "InternalID", copy = TRUE
        ) %>>%
        dplyr::collect() %>>%
        dplyr::mutate(ServiceDate = as.Date(substr(ServiceDate, 1, 10)))
    }
    return(self$appointments_billings)
  }
)
.reactive_event(
  dMeasureBillings, "appointments_billingsR",
  quote(
    shiny::eventReactive(
      c(self$dM$appointments_listR()), {
        # update if reactive version of $appointments_list
        # is updated
        self$billed_appointments(lazy = TRUE)
        # re-calculates, but don't need to re-calculate
        # $appointments_list
      }
    )
  )
)

.public(
  dMeasureBillings, "services_list",
  data.frame(
    Patient = character(), InternalID = integer(),
    ServiceDate = as.Date(integer(0), origin = "1970-01-01"),
    MBSItem = integer(0),
    Description = character(0),
    Provider = character(0),
    DOB = as.Date(integer(0), origin = "1970-01-01"),
    Age = numeric()
  )
)

#' List of services with date of birth
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasureBillings_obj dMeasureBillings R6 object
#' @param date_from default is $date_a. start date, inclusive (date object)
#' @param date_to default is $date_b end date, inclusive (date object)
#' @param clinicians default is $clinicians. list of clinicians to view
#' @param internalID default is NA (include all). vector of internal IDs to include (filter)
#' @param payerCode default is $payerCode
#'  0 = unknown
#'  1 = private (patient)
#'  2 = Medicare direct billing 'bulk-billing'
#'  3 = Department of Veteran Affairs 'DVA'
#'  4 = WorkCover
#'  5 = private (head of family)
#'  8 = private (other)
#' @param lazy if lazy=TRUE, then don't re-calculate $appointments_filtered to calculate
#'
#' @return list of services
#' @export
list_services <- function(dMeasureBillings_obj,
                          date_from = NA, date_to = NA,
                          clinicians = NA,
                          internalID = NA,
                          payerCode = NA,
                          lazy = FALSE) {
  dMeasureBillings_obj$list_services(date_from, date_to, clinicians, internalID, payerCode, lazy)
}
.public(
  dMeasureBillings, "list_services",
  function(date_from = NA,
    date_to = NA,
    clinicians = NA,
    internalID = NA,
    payerCode = NA,
    lazy = FALSE) {
    if (is.na(date_from)) {
      date_from <- self$dM$date_a
    }
    if (is.na(date_to)) {
      date_to <- self$dM$date_b
    }
    if (all(is.na(clinicians))) {
      clinicians <- self$dM$clinicians
    }
    # no additional clinician filtering based on privileges or user restrictions
    if (length(payerCode) > 0 && is.na(payerCode[[1]])) {
      payerCode <- self$payerCode
    }
    clinicians <- intersect(
      clinicians,
      self$dM$clinician_list(view_name = "billings")
    )
    # this view is potentially restricted. if 'GlobalBillView' restriction
    # is enabled, reduce the number of clinicians shown, if the authorized
    # user does not have 'GlobalBillView' attribute

    if (all(is.na(clinicians)) || length(clinicians) == 0) {
      clinicians <- c("")
    }

    if (self$dM$emr_db$is_open()) {
      # only if EMR database is open

      provider_list <- self$dM$UserFullConfig %>>%
        dplyr::select(UserID, Provider = Fullname) %>>% # just need two fields
        dplyr::filter(Provider %in% clinicians) # only users in clinicians list

      payerCode <- c(payerCode, -1) # can't filter on empty vector

      services_list <- self$dM$db$servicesRaw %>>%
        dplyr::filter(
          ServiceDate >= date_from & ServiceDate <= date_to,
          PayerCode %in% payerCode
        ) %>>%
        dplyr::select(-c(PayerCode)) %>>%
        dplyr::collect()

      invoiceID <- c(services_list %>>% dplyr::pull(InvoiceID), -1)
      userID <- c(provider_list %>>% dplyr::pull(UserID), -1)
      invoices_list <- self$dM$db$invoices %>>%
        dplyr::filter(InvoiceID %in% invoiceID) %>>%
        dplyr::filter(UserID %in% userID) %>>% {
          if (is.na(internalID[[1]])) {
            .
          } else {
            dplyr::filter(., InternalID %in% internalID)
          }
        } %>>% dplyr::collect()

      internalID <- c(invoices_list %>>% dplyr::pull(InternalID), -1)
      # could be a subset of previously supplied list of internalID
      patients_list <- self$dM$db$patients %>>%
        dplyr::select(InternalID, Firstname, Surname, DOB) %>>%
        dplyr::filter(InternalID %in% internalID) %>>% dplyr::collect()

      services_list <- invoices_list %>>%
        # invoices list has been filtered for userid
        # whereas services_list has not yet been filtered for userid
        dplyr::left_join(services_list, by = "InvoiceID") %>>%
        dplyr::left_join(patients_list, by = "InternalID") %>>%
        dplyr::left_join(provider_list, by = "UserID") %>>%
        dplyr::mutate(
          ServiceDate = as.Date(ServiceDate),
          DOB = as.Date(DOB),
          Patient = paste(Firstname, Surname),
          Age = dMeasure::calc_age(DOB, ServiceDate)
        ) %>>%
        dplyr::select(
          Patient, InternalID, ServiceDate, MBSItem, Description, Provider,
          DOB, Age
        )

      self$services_list <- services_list
    }
    return(self$services_list)
  }
)
.reactive_event(
  dMeasureBillings, "services_listR",
  quote(
    shiny::eventReactive(
      c(
        self$dM$date_aR(), self$dM$date_bR(),
        self$dM$cliniciansR(),
        self$payerCodeR()
      ), {
        self$list_services()
      }
    )
  )
)

.public(
  dMeasureBillings, "services_list_allclinicians",
  data.frame(
    Patient = character(), InternalID = integer(),
    ServiceDate = as.Date(integer(0), origin = "1970-01-01"),
    MBSItem = integer(0),
    Description = character(0),
    Provider = character(0),
    DOB = as.Date(integer(0), origin = "1970-01-01"),
    Age = numeric()
  )
)

#' List of services with date of birth
#'
#' Filtered by date. Not filtered by clinicians
#'
#' @param dMeasureBillings_obj dMeasureBillings R6 object
#' @param date_from default is $date_a. start date, inclusive (date object)
#' @param date_to default is $date_b end date, inclusive (date object)
#' @param internalID default is NA (include all). otherwise, a vector of internal IDs to include (filter)
#' @param payerCode default is $payerCode
#'  0 = unknown
#'  1 = private (patient)
#'  2 = Medicare direct billing 'bulk-billing'
#'  3 = Department of Veteran Affairs 'DVA'
#'  4 = WorkCover
#'  5 = private (head of family)
#'  8 = private (other)
#'
#' @return list of services
#' @export
list_services_allclinicians <- function(dMeasureBillings_obj,
                                        date_from = NA, date_to = NA,
                                        internalID = NA,
                                        payerCode = NA) {
  dMeasureBillings_obj$list_services_allclinicians(
    date_from, date_to,
    internalID,
    payerCode
  )
}
.public(
  dMeasureBillings, "list_services_allclinicians",
  function(date_from = NA,
             date_to = NA,
             internalID = NA,
             payerCode = NA) {
    if (is.na(date_from)) {
      date_from <- self$dM$date_a
    }
    if (is.na(date_to)) {
      date_to <- self$dM$date_b
    }
    if (length(payerCode) > 0 && is.na(payerCode[[1]])) {
      payerCode <- self$payerCode
    }
    payerCode <- c(payerCode, -1) # can't filter on empty vector

    if (self$dM$emr_db$is_open()) {
      # only if EMR database is open

      provider_list <- self$dM$UserFullConfig %>>%
        dplyr::select(UserID, Provider = Fullname)

      self$services_list_allclinicians <- self$dM$db$servicesRaw %>>%
        dplyr::filter(
          ServiceDate >= date_from & ServiceDate <= date_to,
          PayerCode %in% payerCode
        ) %>>%
        dplyr::select(-c(PayerCode)) %>>%
        dplyr::inner_join(self$dM$db$invoices %>>% {
          if (is.na(internalID[[1]])) {
            .
          }
          else {
            dplyr::filter(., InternalID %in% internalID)
          }
        },
        by = "InvoiceID"
        ) %>>%
        dplyr::inner_join(self$dM$db$patients %>>%
          dplyr::select(InternalID, Firstname, Surname, DOB) %>>% {
            if (is.na(internalID[[1]])) {
              .
            }
            else {
              dplyr::filter(., InternalID %in% internalID)
            }
          },
        by = "InternalID"
        ) %>>%
        dplyr::collect() %>>%
        dplyr::left_join(provider_list,
          by = "UserID"
        ) %>>%
        dplyr::mutate(
          ServiceDate = as.Date(ServiceDate),
          DOB = as.Date(DOB),
          Patient = paste(Firstname, Surname),
          Age = dMeasure::calc_age(DOB, ServiceDate)
        ) %>>%
        dplyr::select(
          Patient, InternalID, ServiceDate, MBSItem, Description, Provider,
          DOB, Age
        )
    }
    return(self$services_list_allclinicians)
  }
)
#.reactive_event(
#  dMeasureBillings, "services_list_allcliniciansR",
#  quote(
#    shiny::eventReactive(
#      c(self$dM$date_aR(), self$dM$date_bR()), {
#        browser()
#        self$list_services_allclinicians()
#      }
#    )
#  )
#)

#' Billings done on same day as appointments or visit
#'
#' filter to billings which are done on the same day as displayed appointments
#'
#' @param dMeasureBillings_obj dMeasureBillings R6 object
#' @param date_from (default $date_a from dMeasure) start date
#' @param date_to (default $date_b from dMeasure) end date (inclusive)
#' @param clinicians (default $clinicians from dMeasure) list of clinicians to view
#' @param lazy if TRUE, then do not recalculate appointment list. otherwise, re-calculate
#' @param screentag (default FALSE) optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print (default TRUE) optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
#' @export
appointments_billings_sameday <- function(dMeasureBillings_obj, date_from, date_to,
                                          clinicians,
                                          lazy, screentag, screentag_print) {
  dMeasureBillings_obj$appointments_billlings_sameday(
    date_from, date_to, clinicians,
    lazy, screentag, screentag_print
  )
}
.public(
  dMeasureBillings, "appointments_billings_sameday",
  function(date_from = NA, date_to = NA,
             clinicians = NA,
             lazy = FALSE,
             screentag = FALSE,
             screentag_print = TRUE) {
    # as of 2019-Sep-08, this method's functionality has been replaced
    # by $list_billings

    if (is.na(date_from)) {
      date_from <- self$dM$date_a
    }
    if (is.na(date_to)) {
      date_to <- self$dM$date_b
    }
    if (all(is.na(clinicians))) {
      clinicians <- self$dM$clinicians
    }
    # no additional clinician filtering based on privileges or user restrictions

    if (!lazy) {
      self$dM$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
      # if not 'lazy' evaluation, then re-calculate
      # (that is automatically done by calling the above methods)
    }

    billings_sameday <- self$appointments_billings %>>%
      dplyr::filter(ServiceDate == AppointmentDate) %>>%
      # billings done on the same day as displayed appointments
      dplyr::select(
        Patient, InternalID, AppointmentDate, AppointmentTime,
        Provider, MBSItem, Description
      )
    # need to preserve ApppointmentTime and Provider
    # in the case where there are multiple apppointments
    # for the patient in the same time period/day and providers

    if (screentag) {
      # change MBSITEMS into fomantic/semantic tags
      billings_sameday <- billings_sameday %>>%
        dplyr::mutate(
          billingtag =
            dMeasure::semantic_button(MBSItem,
              colour = "green",
              popuphtml = paste0(
                "<h4>", AppointmentDate,
                "</h3><p><font size=\'+0\'>",
                Description, "</p>"
              )
            )
        )
    }

    if (screentag_print) {
      billings_sameday <- billings_sameday %>>%
        dplyr::mutate(billingtag_print = MBSItem)
    }

    return(billings_sameday)
  }
)

#####  payerCode ############################################
# used by $list_billings
# default is 0 to 8 (0:8)
#  0 = unknown
#  1 = private (patient)
#  2 = Medicare direct billing 'bulk-billing'
#  3 = Department of Veteran Affairs 'DVA'
#  4 = WorkCover
#  5 = private (head of family)
#  8 = private (other)
.private(dMeasureBillings, ".payerCode", 0:8)
.active(dMeasureBillings, "payerCode", function(value) {
  # a numeric vector
  # should be an integer
  #  but can only test for 'numeric')
  #  as.integer(c(5,4)) returns FALSE
  if (missing(value)) {
    return(private$.payerCode)
  }
  if (is.numeric(value)) {
    private$.payerCode <- value
    private$set_reactive(self$payerCodeR, value)
  } else {
    warning("$payerCode only accepts numeric vector.")
  }
})
.reactive(dMeasureBillings, "payerCodeR", 0:8)


empty_billings_list <-
  data.frame(
    Patient = character(), InternalID = integer(),
    Date = as.Date(integer(0), origin = "1970-01-01"),
    AppointmentTime = character(0),
    Status = character(0),
    VisitType = character(0),
    Provider = character(0),
    DOB = as.Date(integer(0), origin = "1970-01-01"),
    Age = numeric(0),
    billingtag = character(0),
    billingtag_print = character(0)
  )

.public(
  dMeasureBillings, "billings_list",
  empty_billings_list
)

#' Billings, appointments and visit combined view
#'
#' filter to chosen date range and practitioners
#'
#' billings are aggregated/group to patient/day
#'
#' @param dMeasureBillings_obj dMeasureBillings R6 object
#' @param date_from (default date_a field) start date
#' @param date_to (default date_b field) end date (inclusive)
#' @param clinicians (default clinicians field) list of clinicians to view
#' @param own_billings (default own_billings field) logical TRUE/FALSE
#'
#'  only show billings done by the specified provider on the
#'  contact (visit/appointment/billing) day.
#'
#'  if FALSE, the all billings
#'  done on the patient on that day are shown.
#' @param lazy if TRUE, then do not recalculate appointment list. otherwise, re-calculate
#' @param screentag (default FALSE) optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print (default TRUE) optionally add a 'printable' description of 'action'
#' @param payerCode default is $payerCode
#'  0 = unknown
#'  1 = private (patient)
#'  2 = Medicare direct billing 'bulk-billing'
#'  3 = Department of Veteran Affairs 'DVA'
#'  4 = WorkCover
#'  5 = private (head of family)
#'  8 = private (other)
#'
#' @return list of appointments (with patient details) and attached billings
#'  billings are in list() of MBSItem, Description and Provider
#' @export
list_billings <- function(dMeasureBillings_obj,
                          date_from = NA, date_to = NA,
                          clinicians = NA, own_billings = NA,
                          lazy = FALSE, payerCode = NA) {
  dMeasureBillings_obj$list_billings(
    date_from, date_to, clinicians, own_billings,
    lazy, payerCode
  )
}
.public(dMeasureBillings, "list_billings", function(date_from = NA,
                                                    date_to = NA,
                                                    clinicians = NA,
                                                    own_billings = NA,
                                                    lazy = FALSE,
                                                    payerCode = NA) {
  if (is.na(date_from)) {
    date_from <- self$dM$date_a
  }
  if (is.na(date_to)) {
    date_to <- self$dM$date_b
  }
  if (all(is.na(clinicians))) {
    clinicians <- self$dM$clinicians
  }
  # no additional clinician filtering based on privileges or user restrictions
  if (is.na(own_billings)) {
    own_billings <- self$own_billings
  }
  if (length(payerCode) > 0 && is.na(payerCode[[1]])) {
    payerCode <- self$payerCode
  }

  x <- self$dM$check_subscription(
    clinicians,
    date_from, date_to
  )
  # check subscription, which depends on selected
  # clinicians and selected date range
  if (x$changedate) {
    # if a user without valid subscription for chosen date range
    # then return an empty dataframe
    return(empty_billings_list)
  }

  if (!lazy) {
    # if not lazy OR
    # could be forced by changedate
    self$dM$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
    self$dM$list_visits(date_from, date_to, clinicians, lazy = FALSE)
  }

  df <- self$dM$appointments_list %>>%
    dplyr::rename(Date = AppointmentDate) %>>%
    dplyr::full_join(self$dM$visits_list %>>%
      dplyr::rename(Date = VisitDate),
    by = c("Patient", "InternalID", "Date", "DOB", "Age", "Provider")
    )
  # this join just adds VisitType
  apptID <- df %>>% dplyr::pull(InternalID) %>>% c(-1)
  if (!lazy && own_billings) {
      self$list_services(date_from, date_to, clinicians, payerCode = payerCode, lazy = FALSE)
  }
  if (!own_billings) {
      self$list_services_allclinicians(
        date_from, date_to,
        internalID = apptID, payerCode = payerCode
      )
  }
  if (own_billings) {
    services_list <- self$services_list
  } else {
    services_list <- self$services_list %>>%
      rbind(self$services_list_allclinicians) %>>%
      dplyr::distinct() # remove the duplicates
    # self$services_list might not always be a subset of self$services_list_allclinicians
    # because the second dataframe has been restricted by apptID
    # and it is possible that items in self$services_list do not have an appointment
  }
  df <- df %>>%
    dplyr::full_join(services_list %>>%
        dplyr::rename(
          Date = ServiceDate,
          MBSDescription = Description,
          MBSProvider = Provider
          ),
      by = c("Patient", "InternalID", "Date", "DOB", "Age")) %>>%
    dplyr::group_by(
      Patient, InternalID, Date, AppointmentTime,
      Status, VisitType, Provider, DOB, Age
    ) %>>%
    # gathers services from the same date/time into a single row
    dplyr::summarise(
      MBSItem = I(list(c(MBSItem))),
      MBSDescription = I(list(c(MBSDescription))),
      MBSProvider = I(list(c(MBSProvider)))
    ) %>>%
    # need I(list()) to place list in a data-frame
    dplyr::ungroup() %>>%
    # merges appointments and visit and services lists
    dplyr::select(
      Patient, InternalID, Date, AppointmentTime,
      Status, VisitType, Provider, DOB, Age,
      MBSItem, MBSDescription, MBSProvider
    )

  self$billings_list <- df

  return(self$billings_list)
})
.reactive_event(
  dMeasureBillings, "billings_listR",
  quote(
    shiny::eventReactive(
      c(
        self$dM$appointments_listR(),
        self$services_listR(),
        self$dM$visits_listR(),
        self$own_billingsR()
      ), {
        self$list_billings(lazy = TRUE)
      }
    )
  )
)

#' List whether patients had a recent GPMP (care plan)
#'
#' @param dMeasureBillings_obj dMeasureBillings R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#' @param months_min minimum age in months (inclusive, 'from')
#' @param months_max maximum age in months (exclusive, 'up to')
#'
#' @return a vector of numbers, whih care internalID
#' @export
gpmp_list <- function(dMeasureBillings_obj, appointments = NULL,
  months_min = 0, months_max = 12) {
  dMeasureBillings_obj$list_gpmp(
    appointments, months_min, months_max
  )
}
.public(dMeasureBillings, "gpmp_list", function(
  appointments = NULL,
  months_min = 0, months_max = 12) {

  if (is.null(appointments)) {
    appointments <- self$dM$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }
  clinicians <- self$dM$clinicians
  date_to <- max(appointments$Date, self$dM$date_b)
  # maximum (most recent) of chosen dates, or chosen 'date range'
  date_from <- date_to

  x <- self$dM$check_subscription(
    clinicians,
    date_from, date_to,
    adjust_days = 120
  )
  # check subscription, which depends on selected
  # clinicians and selected date range
  if (x$changedate) {
    # this will happen if dates are changed
    appointments$Date <- min(appointments$Date, x$date_to)
    # earlier of the dates
    warning("A chosen user has no subscription for chosen date range. Empty vector returned")
    return(numeric(0))
  }

  intID <- appointments %>>% dplyr::pull(InternalID) %>>% c(-1)

  intID <- self$dM$db$services %>>%
    dplyr::filter(
      MBSItem == 721, # care plan item number
      InternalID %in% intID
      ) %>>%
    dplyr::select(InternalID, ServiceDate) %>>%
    dplyr::collect() %>>%
    dplyr::left_join(
      appointments,
      by = "InternalID") %>>%
    dplyr::mutate(AgeInMonths = dMeasure::calc_age_months(as.Date(ServiceDate), as.Date(Date))) %>>%
    dplyr::filter(AgeInMonths >= months_min & AgeInMonths < months_max) %>>%
    dplyr::pull(InternalID) %>>%
    unique()

  return(intID)

})


#' tags a billlings list for printing or HTML screen display
#'
#' @param dMeasureBillings_obj dMeasureBillings R6 object
#' @param billings_list dataframe. requires ServiceDate and
#'   lists (MBSItem, MBSDescription, MBSProvider)
#' @param screentag (default FALSE) optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print (default TRUE) optionally add a 'printable' description of 'action'
#'
#' @return tagged billings list
#'  adds billingtag_print for 'print' view
#'  adds billingtag for HTML view using fomantic/semantic buttons
#' @export
tag_billings_list <- function(billings_list, screentag = FALSE, screentag_print = TRUE) {
  browser()
  if (screentag) {
    billings_list <- billings_list %>>%
      # if 'screen' (not print) display
      # and 'own_billings'. if own_billings is FALSE then
      #  the tags will be provided by self$service_list_allclinicians below
      dplyr::mutate(
        billingtag = mapply(
          paste,
          dplyr::if_else(
            is.na(MBSItem),
            # need to sapply because is.null doesn't deal with lists/vectors
            # https://stackoverflow.com/questions/30716377/how-to-detect-null-values-in-a-vector
            list(character(0)),
            c(mapply(function(...) list(dMeasure::semantic_button(...)),
              MBSItem,
              colour = "green"#,
#              popuphtml = paste0(
#                "<h4>", Date,
#                "</h4><p><font size=\'+0\'>",
#                MBSDescription, " (",
#                MBSProvider, ")</p>"
#              )
            ))
          ),
          collapse = "",
          USE.NAMES = FALSE
        )
      )
  }

  if (screentag_print) {
    billings_list <- billings_list %>>%
      dplyr::mutate(
        billingtag_print = mapply(
          paste,
          dplyr::if_else(
            is.na(MBSItem),
            list(character(0)),
            mapply(function(...) list(paste0(...)),
              MBSItem, " [",
              MBSProvider, "]",
              USE.NAMES = FALSE
            )
          ),
          collapse = ", ",
          USE.NAMES = FALSE
        )
      )
  }

  return(billings_list)
}
