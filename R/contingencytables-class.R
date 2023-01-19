# Instantiates an object of the "contingencytables_*" classes
newContingencytablesOutput <- function(content, class_name = "single") {
  # Defining valid structure
  valid_classes <- c(
    "contingencytables_singletest",
    "contingencytables_multipletests"
  )
  # Expanding shortened class names
  class_name <- switch(
    class_name,
    "single"   = valid_classes[1],
    "multiple" = valid_classes[2],
    class_name
  )
  valid_content <- switch(
    class_name,
    "contingencytables_singletest"   = c("name", "statistics"),
    "contingencytables_multipletests" = c("statistics", "FUN"),
    stop(
      "Invalid class_name. Please choose between ",
      paste(valid_classes, collapse = ", ")
    )
  )

  # Validating input
  names_match <- all(sort(names(content)) %in% sort(valid_content))
  if (is.null(names(content)) || !names_match) {
    stop(
      "Invalid content. List must contain the following elements: ",
      paste(valid_content, collapse = ", ")
    )
  } else {
    if (class_name == "contingencytables_multipletests") {
      FUN_arguments <- names(formals(content$FUN))
      stopifnot(FUN_arguments == "statistics")
    }
    return(structure(content, class = class_name))
  }
}
