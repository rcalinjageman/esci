esci_hello <- function(jaspResults, dataset, options) {
  cat("Hello, world!")

  myres <- as.data.frame(statpsych::ci.stdmean1(alpha = 0.05, m = 50, sd = 10, n = 25, h = 10))

  if(is.null(jaspResults[["tbl_esci_helo"]])) {

     tbl_esci_hello <- createJaspTable(title = "ci.stdmean1")
     tbl_esci_hello$dependOn(c("variables", "mu"))


     tbl_esci_hello$addColumnInfo(name = "Estimate",  title = "Estimate", type = "number")
     tbl_esci_hello$addColumnInfo(name = "SE",  title = "SE", type = "number")
     tbl_esci_hello$addColumnInfo(name = "LL",  title = "LL", type = "number")
     tbl_esci_hello$addColumnInfo(name = "UL",  title = "UL", type = "number")

     tbl_esci_hello$showSpecifiedColumnsOnly <- TRUE

     tbl_esci_hello$setExpectedSize(nrow(myres))

     tbl_esci_hello$addRows(
	list(
		Estimate = myres$Estimate,
		SE = myres$SE,
		LL = myres$LL,
		UL = myres$UL
         )
     )

     txt_esci_hello <- createJaspHtml(
          text = gettextf("The Estimate was %s 95 percent CI %s %s.", myres$Estimate, myres$LL, myres$UL)
     )



  }

  return()
}


esci_describe <- function(jaspResults, dataset, options) {

  ready <- (length(options$variables) > 0)

  if (ready)
    dataset <- esci_describeReadData(dataset, options)

  myres <- NULL

  if (ready) {

    args <- list()

    args$data <- dataset
    args$outcome_variable <- encodeColNames(options$variables[1])
    call <- esci::estimate_magnitude
    myres <- try(do.call(what = call, args = args))

    # myres <- esci::estimate_magnitude(
    #   data = dataset,
    #   outcome_variable = !!encodeColNames(options$variables[1])]
    # )
  }

  if (is.null(jaspResults[["overviewTable"]]))
    esci_describeTableOverview(jaspResults, dataset, options, ready, myres)

  if (is.null(jaspResults[["describePlot"]]))
    esci_describePlotDescribe(jaspResults, dataset, options, ready, myres)

  # myres <- as.data.frame(statpsych::ci.stdmean1(alpha = 0.05, m = 50, sd = 10, n = 25, h = 10))

  # txt_esci_hello <- createJaspHtml(
  #        text = gettextf("The Estimate was %s 95 percent CI %s %s.", myres$Estimate, myres$LL, myres$UL)
  #   )



  return()
}


esci_describeReadData <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns.as.numeric = options$variables))
}



esci_describeTableOverview <- function(jaspResults, dataset, options, ready, myres) {


  overviewTable <- createJaspTable(title = "Overview")

  overviewTable$dependOn(c("variables"))

  overviewTable$addColumnInfo(name = "mean", title = "M", type = "number")
  overviewTable$addColumnInfo(name = "LL", title = "LL", type = "number")
  overviewTable$addColumnInfo(name = "UL", title = "UL", type = "number")
  overviewTable$addColumnInfo(name = "p",          title = "p",          type = "pvalue")

  overviewTable$showSpecifiedColumnsOnly <- TRUE

  jaspResults[["overviewTable"]] <- overviewTable


  if (!ready)
    return()

  esci_describeFillOverview(overviewTable, dataset, options, myres)

  return()
}

esci_describeFillOverview <- function(overviewTable, dataset, options, myres) {
  if (is.null(myres))
    return()

  overviewTable$addRows(
    list(
      M = myres$es_mean$effect_size,
      LL = myres$es_mean$LL,
      UL = myres$es_mean$UL,
      P = 0.40
    )
  )

  return()


}

esci_describePlotDescribe <- function(jaspResults, dataset, options, ready, myres) {

  describePlot <- createJaspPlot(title = "Describe",  width = 160, height = 320)
  describePlot$dependOn(c("variables"))

  jaspResults[["describePlot"]] <- describePlot

  if (!ready)
    return()

  esci_describeFillPlotDescriptives(describePlot, dataset, options, myres)

  return()
}


esci_describeFillPlotDescriptives <- function(describePlot, dataset, options, myres) {
  if (is.null(myres))
    return()

  plot <- esci::plot_describe(myres)

  describePlot$plotObject <- plot

  return()


}

