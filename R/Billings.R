#' dMeasureBillings - list billings module for dMeasure
#'
#'
#' @name billings
#' @title dMeasureBillings
#'
#' @include r6_helpers.R
#' functions to help create R6 classes
NULL

#' dMeasureBillings class
#' @title dMeasureBillings class
#' @description list services (billings) by clinician providers
#' @export
dMeasureBillings <- R6::R6Class("dMeasureBillings",
  public = list(
    # dM is a dMeasure object
    dM = NULL,
    initialize = function(dMeasure_obj) {
      # dMeasure_obj is a R6 dMeasure object
      self$dM <- dMeasure_obj
      if (length(public_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(public_init_fields$name)) {
          if (public_init_fields$obj[[i]] == "dMeasureBillings") {
            self[[public_init_fields$name[[i]]]] <-
              eval(public_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }
      if (length(private_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(private_init_fields$name)) {
          if (private_init_fields$obj[[i]] == "dMeasureBillings") {
            private[[private_init_fields$name[[i]]]] <-
              eval(private_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }

      if (requireNamespace("shiny", quietly = TRUE)) {
        # set reactive version only if shiny is available
        # note that this is for reading (from programs calling this object) only!
        if (length(reactive_fields$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_fields$name)) {
            if (reactive_fields$obj[[i]] == "dMeasureBillings") {
              self[[reactive_fields$name[[i]]]] <- shiny::reactiveVal(
                eval(reactive_fields$value[[i]]) # could 'quote' the value
              )
            }
          }
        }
        if (length(reactive_event$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_event$name)) {
            if (reactive_event$obj[[i]] == "dMeasureBillings") {
              self[[reactive_event$name[[i]]]] <-
                eval(reactive_event$value[[i]]) # could 'quote' the value
            }
          }
        }
      }
    }
  )
  # this is a 'skeleton' class
  # it is filled in the with the '.public' function
)

##### special reactive functions ##########################


.private(dMeasureBillings, "set_reactive", function(myreactive, value) {
  # reactive (if shiny/reactive environment is available) is set to 'value'
  # myreactive is passed by reference
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(value)
  }
})
.private(dMeasureBillings, "trigger", function(myreactive) {
  # toggles a reactive between (usually) 0 and 1
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(1 - shiny::isolate(myreactive()))
  }
})

###########################################################


#' show only 'own' billings next to provider name
#'
#' @name own_billings
#'
#' @param dMeasureBillings_obj dMeasureBillings R6 object
#'
#' @return current value of own_billings if no parameter provided
#'
#' @export
own_billings <- function(dMeasureBillings_obj, value) {
  if (missing(value)) {
    return(dMeasureBillings_obj$own_billings)
  }
  else {
    dMeasureBillings_obj$own_billings <- value
  }
}
.private(dMeasureBillings, ".own_billings", TRUE)
.active(dMeasureBillings, "own_billings", function(value) {
  if (missing(value)) {
    return(private$.own_billings)
  } else if (is.logical(value)) {
    private$.own_billings <- value
  } else {
    warning("$own_billings only accepts logical values.")
  }

  private$set_reactive(self$own_billingsR, private$.own_billings) # set reactive version
  return(private$.own_billings)
})
.reactive(dMeasureBillings, "own_billingsR", quote(private$.own_billings))
