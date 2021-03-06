
# This file is automatically generated, you probably don't want to edit this

jmvDescriptivesOptions <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvDescriptivesOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            measure1 = NULL,
            plottype = "dotplot",
            marker = "none",
            bins = "12",
            show.mean = FALSE,
            show.median = FALSE,
            show.zlines = FALSE,
            show.s = FALSE,
            ylab = NULL,
            xlab = NULL,
            size = "1",
            ymin = "auto",
            ymax = "auto",
            xmin = "auto",
            xmax = "auto",
            color.regular = "gray",
            color.highlighted = "red", ...) {

            super$initialize(
                package='esci',
                name='jmvDescriptives',
                requiresData=TRUE,
                ...)

            private$..measure1 <- jmvcore::OptionVariable$new(
                "measure1",
                measure1)
            private$..plottype <- jmvcore::OptionList$new(
                "plottype",
                plottype,
                default="dotplot",
                options=list(
                    "dotplot",
                    "histogram"))
            private$..marker <- jmvcore::OptionString$new(
                "marker",
                marker,
                default="none")
            private$..bins <- jmvcore::OptionString$new(
                "bins",
                bins,
                default="12")
            private$..show.mean <- jmvcore::OptionBool$new(
                "show.mean",
                show.mean,
                default=FALSE)
            private$..show.median <- jmvcore::OptionBool$new(
                "show.median",
                show.median,
                default=FALSE)
            private$..show.zlines <- jmvcore::OptionBool$new(
                "show.zlines",
                show.zlines,
                default=FALSE)
            private$..show.s <- jmvcore::OptionBool$new(
                "show.s",
                show.s,
                default=FALSE)
            private$..ylab <- jmvcore::OptionString$new(
                "ylab",
                ylab)
            private$..xlab <- jmvcore::OptionString$new(
                "xlab",
                xlab)
            private$..size <- jmvcore::OptionString$new(
                "size",
                size,
                default="1")
            private$..ymin <- jmvcore::OptionString$new(
                "ymin",
                ymin,
                default="auto")
            private$..ymax <- jmvcore::OptionString$new(
                "ymax",
                ymax,
                default="auto")
            private$..xmin <- jmvcore::OptionString$new(
                "xmin",
                xmin,
                default="auto")
            private$..xmax <- jmvcore::OptionString$new(
                "xmax",
                xmax,
                default="auto")
            private$..color.regular <- jmvcore::OptionString$new(
                "color.regular",
                color.regular,
                default="gray")
            private$..color.highlighted <- jmvcore::OptionString$new(
                "color.highlighted",
                color.highlighted,
                default="red")

            self$.addOption(private$..measure1)
            self$.addOption(private$..plottype)
            self$.addOption(private$..marker)
            self$.addOption(private$..bins)
            self$.addOption(private$..show.mean)
            self$.addOption(private$..show.median)
            self$.addOption(private$..show.zlines)
            self$.addOption(private$..show.s)
            self$.addOption(private$..ylab)
            self$.addOption(private$..xlab)
            self$.addOption(private$..size)
            self$.addOption(private$..ymin)
            self$.addOption(private$..ymax)
            self$.addOption(private$..xmin)
            self$.addOption(private$..xmax)
            self$.addOption(private$..color.regular)
            self$.addOption(private$..color.highlighted)
        }),
    active = list(
        measure1 = function() private$..measure1$value,
        plottype = function() private$..plottype$value,
        marker = function() private$..marker$value,
        bins = function() private$..bins$value,
        show.mean = function() private$..show.mean$value,
        show.median = function() private$..show.median$value,
        show.zlines = function() private$..show.zlines$value,
        show.s = function() private$..show.s$value,
        ylab = function() private$..ylab$value,
        xlab = function() private$..xlab$value,
        size = function() private$..size$value,
        ymin = function() private$..ymin$value,
        ymax = function() private$..ymax$value,
        xmin = function() private$..xmin$value,
        xmax = function() private$..xmax$value,
        color.regular = function() private$..color.regular$value,
        color.highlighted = function() private$..color.highlighted$value),
    private = list(
        ..measure1 = NA,
        ..plottype = NA,
        ..marker = NA,
        ..bins = NA,
        ..show.mean = NA,
        ..show.median = NA,
        ..show.zlines = NA,
        ..show.s = NA,
        ..ylab = NA,
        ..xlab = NA,
        ..size = NA,
        ..ymin = NA,
        ..ymax = NA,
        ..xmin = NA,
        ..xmax = NA,
        ..color.regular = NA,
        ..color.highlighted = NA)
)

jmvDescriptivesResults <- if (requireNamespace('jmvcore')) R6::R6Class(
    inherit = jmvcore::Group,
    active = list(
        text = function() private$.items[["text"]],
        descriptives = function() private$.items[["descriptives"]],
        distribution = function() private$.items[["distribution"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Descriptives")
            self$add(jmvcore::Html$new(
                options=options,
                name="text",
                title="Instructions/Errors",
                visible=FALSE))
            self$add(jmvcore::Table$new(
                options=options,
                name="descriptives",
                title="Descriptive Statistics",
                rows=1,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="m", 
                        `type`="number", 
                        `title`="M", 
                        `superTitle`="Location"),
                    list(
                        `name`="median", 
                        `type`="number", 
                        `title`="Median", 
                        `superTitle`="Location"),
                    list(
                        `name`="s1", 
                        `type`="text", 
                        `title`=""),
                    list(
                        `name`="s", 
                        `type`="number", 
                        `superTitle`="Spread"),
                    list(
                        `name`="min", 
                        `type`="number", 
                        `title`="Minimum", 
                        `superTitle`="Spread"),
                    list(
                        `name`="max", 
                        `type`="number", 
                        `title`="Maximum", 
                        `superTitle`="Spread"),
                    list(
                        `name`="q1", 
                        `type`="number", 
                        `title`="25th percentile", 
                        `superTitle`="Spread"),
                    list(
                        `name`="q3", 
                        `type`="number", 
                        `title`="75th percentile", 
                        `superTitle`="Spread"),
                    list(
                        `name`="s2", 
                        `type`="text", 
                        `title`=""),
                    list(
                        `name`="N", 
                        `type`="integer", 
                        `superTitle`="Counts"),
                    list(
                        `name`="NA_count", 
                        `type`="integer", 
                        `title`="Missing", 
                        `superTitle`="Counts"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="distribution",
                title="Distribution",
                width=400,
                height=300,
                requiresData=TRUE,
                renderFun=".plot"))}))

jmvDescriptivesBase <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvDescriptivesBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = 'esci',
                name = 'jmvDescriptives',
                version = c(1,0,0),
                options = options,
                results = jmvDescriptivesResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE)
        }))

#' Descriptives
#'
#' 
#' @param data .
#' @param measure1 .
#' @param plottype .
#' @param marker .
#' @param bins .
#' @param show.mean .
#' @param show.median .
#' @param show.zlines .
#' @param show.s .
#' @param ylab .
#' @param xlab .
#' @param size .
#' @param ymin .
#' @param ymax .
#' @param xmin .
#' @param xmax .
#' @param color.regular .
#' @param color.highlighted .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$text} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$descriptives} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$distribution} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$descriptives$asDF}
#'
#' \code{as.data.frame(results$descriptives)}
#'
#' @export
jmvDescriptives <- function(
    data,
    measure1,
    plottype = "dotplot",
    marker = "none",
    bins = "12",
    show.mean = FALSE,
    show.median = FALSE,
    show.zlines = FALSE,
    show.s = FALSE,
    ylab,
    xlab,
    size = "1",
    ymin = "auto",
    ymax = "auto",
    xmin = "auto",
    xmax = "auto",
    color.regular = "gray",
    color.highlighted = "red") {

    if ( ! requireNamespace('jmvcore'))
        stop('jmvDescriptives requires jmvcore to be installed (restart may be required)')

    if ( ! missing(measure1)) measure1 <- jmvcore::resolveQuo(jmvcore::enquo(measure1))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(measure1), measure1, NULL))


    options <- jmvDescriptivesOptions$new(
        measure1 = measure1,
        plottype = plottype,
        marker = marker,
        bins = bins,
        show.mean = show.mean,
        show.median = show.median,
        show.zlines = show.zlines,
        show.s = show.s,
        ylab = ylab,
        xlab = xlab,
        size = size,
        ymin = ymin,
        ymax = ymax,
        xmin = xmin,
        xmax = xmax,
        color.regular = color.regular,
        color.highlighted = color.highlighted)

    analysis <- jmvDescriptivesClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}
