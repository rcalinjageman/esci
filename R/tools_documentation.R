esci_document_object <- function(estimate) {
  tdoc <- NULL
  for (x in 1:length(estimate)) {
    if (class(estimate[[x]]) == "data.frame") {
      tdoc <- paste(
        tdoc,
        "#' - **",
        names(estimate)[[x]],
        "**\n",
        sep = ""
      )
      for (mycol in colnames(estimate[[x]])) {
        tdoc <- paste(
          tdoc,
          "#'     - *",
          mycol,
          "* - \n",
          sep = ""
        )
      }
    }
  }
  cat(tdoc)
}

