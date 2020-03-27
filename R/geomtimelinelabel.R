#' Time line labels of earthquakes
#'
#' \code{geom_timeline_label} return a \code{\link[ggplot2]{layer}}
#'     representing earthquakes annotations to be added after
#'     \code{\link{geom_timeline}}, i.e. a vertical line to each data
#'     point drown by \code{\link{geom_timeline}} with a text annotation
#'     (e.g. the location of the earthquake) attached to each earthquake
#'     line.
#'
#' @details Aesthetics:
#' \code{geom_timeline_label} understands the following aesthetics
#'   (required in bold):
#' \itemize{
#'   \item \strong{x}: (Date) of earthquakes.
#'   \item \strong{label}: (chr) text annotation.
#'   \item y: (factr) stratification. If present multiple time lines
#'            will be plotted for each level of the factor
#'            (e.g. country).
#'   \item colour: of the points.
#'   \item n_max: annotation to drowm.
#'   \item size: of the points (if provided with \code{n_max} too,
#'         the \code{n_max} largest eartquakes will be annotated).
#' }
#'
#' @inheritParams ggplot2::geom_text
#' @param n_max (int) number of earthquakes, where we take the n_max
#'        largest (in \code{size}, if provided) earthquakes
#' @param na.rm (lgl) remove missing data?
#' @param ... further arguments passed to the geom layer
#'
#' @importFrom ggplot2 layer
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#'ggplot(noaa,aes(x=DATE,y=COUNTRY,col=TOTAL_DEATHS,size=EQ_PRIMARY))+
#'geom_timeline_label()+theme_minimal()
#' }
geom_timeline_label <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", show.legend = NA, inherit.aes = TRUE,
  n_max = NULL, ..., na.rm = FALSE
) {
  ggplot2::layer(
    geom = GeomTimelineLabel,
    mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(n_max = n_max, na.rm = na.rm, ...)
  )
}

#' @rdname geom_timeline_label
#' @format NULL
#' @usage NULL
#'
#' @importFrom ggplot2 ggproto Geom draw_key_blank
#' @importFrom dplyr group_by top_n ungroup
#' @importFrom grid gpar linesGrob textGrob gList
geom_timeline_label <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", show.legend = NA, inherit.aes = TRUE,
  n_max = NULL, ..., na.rm = FALSE
) {
  ggplot2::layer(
    geom = GeomTimelineLabel,
    mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(n_max = n_max, na.rm = na.rm, ...)
  )
}




#' @rdname geom_timeline_label
#' @importFrom ggplot2 ggproto Geom draw_key_blank
#' @importFrom dplyr group_by top_n ungroup
#' @importFrom grid gpar linesGrob textGrob gList
GeomTimelineLabel <- ggplot2::ggproto(
  "GeomTimelineLabel", ggplot2::Geom,

  required_aes = c("x", "label"),


  default_aes  = ggplot2::aes(
    y             = 0.25,
    colour        = "black",
    size          = 1,
    alpha         = 0.25,
    shape         = 19,
    linesize      = 0.5,
    linetype      = 1,
    fontsize      = 10,
    stroke        = 1,
    angle         = 60
  ),

  draw_key = ggplot2::draw_key_blank,

  setup_data = function(data, params) {

    if (!("size" %in% colnames(data))) {
      warning(paste(
        "size is not provided.\n",
        "a random sample of points will be used"
      ))
      data$size <- sample.int(nrow(data))
    }



    if (!is.null(params$n_max)) {
      message(paste(params$n_max, "annotation will be drown."))
      data <- data %>%
        dplyr::group_by_("y") %>%
        dplyr::mutate(size_rank = dplyr::row_number(size)) %>%
        dplyr::top_n(params$n_max, size_rank) %>%
        dplyr::ungroup() %>%
        dplyr::select(-size_rank)
      print(data)
    }

    data
  },

  draw_panel = function(data, panel_scales, coord, n_max) {

    coords <- coord$transform(data, panel_scales)

    if (length(unique(coords$y)) == 1) {
      coords$y <-  0.25
    }

    if (!("size" %in% names(coords))) {
      coords$size <- 0.25
    }

    n_grp  <- length(unique(data$y))
    offset <- 0.2 / n_grp

    y_lines <- unique(coords$y)


    lines <- grid::polylineGrob(
      x  = grid::unit(
        rep(c(0, 1), each = length(y_lines)),
        "npc"
      ),
      y  = grid::unit(c(y_lines, y_lines), "npc"),
      id = rep(seq_along(y_lines), 2),
      gp = grid::gpar(
        col = "grey",
        lwd = grid::unit(coords$linesize[1], "mm")
      )
    )

    points <- grid::pointsGrob(
      x    = coords$x,
      y    = coords$y,
      size = grid::unit(coords$size / 5, "char"),
      pch  = coords$shape,
      gp   = grid::gpar(
        col      = coords$colour,
        fill     = coords$fill,
        fontsize = grid::unit(coords$fontsize, "points")
      )
    )

    names <- grid::textGrob(
      x     = grid::unit(coords$x, "npc"),
      y     = grid::unit(coords$y + offset, "npc"),
      label = coords$label,
      just  = c("left", "bottom"),
      rot   = 60,
      gp    = grid::gpar(
        col      = coords$colour,
        fontsize = grid::unit(coords$fontsize, "points")
      ),
      check.overlap = FALSE
    )

    grid::gList(names,lines,points)
  }
)

