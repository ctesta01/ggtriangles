
#' Draw Triangle Legend Key with polygonGrob
#'
#' @export
#'
#' @examples
#' library(grid)
#' grid.newpage()
#'
#' triangle <-
#'   draw_key_triangle(
#'     data = data.frame(
#'       x = .5,
#'       y = .01,
#'       triangle_height = -.5,
#'       triangle_width = .5,
#'       angle = 0,
#'       fill = 'black',
#'       alpha = 1,
#'       colour = 'black',
#'       size = 1
#'     )
#'   )
#'
#' grid.draw(triangle)
draw_key_triangle <- function(data, params, size) {
  idx <- rep(seq_len(nrow(data)), each = 3)
  rep_data <- data[idx, ]

  x_off <- as.vector(outer(
    c(-0.5, 0, 0.5),
    data$triangle_width
  ))

  y_off <- as.vector(outer(
    c(0, 1, 0),
    data$triangle_height
  ))
  y_off <- y_off + .5

  ang <- rep_data$angle * (pi / 180)
  x_new <- x_off * cos(ang) - y_off * sin(ang)
  y_new <- x_off * sin(ang) + y_off * cos(ang)


  # Origin x and y have fixed values
  x <- unit(0.5, "npc") + unit(x_new, "cm")
  y <- unit(0.2, "npc") + unit(y_new, "cm")

  grid::polygonGrob(
    x = x, y = y, id = idx,
    gp = grid::gpar(
      col  = alpha(data$colour, data$alpha),
      fill = alpha(data$fill, data$alpha),
      lwd  = data$size * .pt,
      lty  = data$linetype
    )
  )
}


#' GeomTriangles ggproto - draws triangles with base having midpoint (x,y)
#'
#' @export
GeomTriangles <- ggproto(
  "GeomTriangles", GeomPoint,
  default_aes = aes(
    colour = "black", fill = "black", size = 0.5, linetype = 1,
    alpha = 1, angle = 0, triangle_width = 0.5, triangle_height = 0.5
  ),
  setup_params = function(data, params) {
    return(params)
  },
  draw_panel = function(
    data, panel_params, coord, na.rm = FALSE
  ) {

    # Apply coordinate transform
    df <- coord$transform(data, panel_params)

    # Repeat every row 3x
    idx <- rep(seq_len(nrow(df)), each = 3)
    rep_df <- df[idx, ]

    # Calculate offsets from origin
    x_off <- as.vector(outer(c(-0.5, 0, 0.5), df$triangle_width))
    y_off <- as.vector(outer(c(0, 1, 0), df$triangle_height))

    # Rotate offsets
    ang <- rep_df$angle * (pi / 180)
    x_new <- x_off * cos(ang) - y_off * sin(ang)
    y_new <- x_off * sin(ang) + y_off * cos(ang)

    # Combine offsets with origin
    x <- unit(rep_df$x, "npc") + unit(x_new, "cm")
    y <- unit(rep_df$y, "npc") + unit(y_new, "cm")

    grid::polygonGrob(
      x = x, y = y, id = idx,
      gp = grid::gpar(
        col  = alpha(df$colour, df$alpha),
        fill = alpha(df$fill, df$alpha),
        lwd  = df$size * .pt,
        lty  = df$linetype
      )
    )
  },
  draw_key = draw_key_triangle,
  setup_data = function(data, params) { return(data) }
)


#' geom_triangles plots isosceles triangles with base at (x,y) and argument
#' height, width, angle, fill, color, alpha, linetype, and size supported.
#'
#' @examples
#'
#' # iris dataset example
#' # ====================
#'
#'   ggplot(iris,
#'     aes(
#'       x = Sepal.Length,
#'       y = Sepal.Width,
#'       triangle_height = Petal.Length,
#'       fill = Petal.Length,
#'       color = Petal.Length,
#'       triangle_width = Petal.Width
#'     )
#'   ) +
#'   geom_triangles(alpha = .7, size = 1) +
#'   scale_fill_viridis_c(option = 'A', end = .8) +
#'   scale_color_viridis_c(option = 'A', end = .8) +
#'   scale_triangle_width_continuous(range = c(0.1, 1), n.breaks = 4) +
#'   scale_triangle_height_continuous(range = c(0.1, 1), n.breaks = 3) +
#'   ggtitle(
#'     "Sepal length and width and petal length and width of iris flowers",
#'     "Petal length and width are shown by the height and width of each triangle"
#'   ) +
#'   theme_bw() +
#'   theme(legend.position = 'bottom')
#'
#'
#' # sleep dataset example
#' # =====================
#'
#' sleep_effect_max <- max(abs(datasets::sleep$extra))
#' triangle_height_range <- c(-sleep_effect_max, sleep_effect_max)
#'
#' ggplot(datasets::sleep,
#'        aes(
#'          x = as.numeric(ID),
#'          y = as.numeric(group),
#'          triangle_height = extra
#'        )) +
#'   geom_triangles(alpha = 0.85) +
#'   scale_y_continuous(breaks = c(1, 2)) +
#'   expand_limits(y = c(0.5, 2.5)) +
#'   xlab("Individual") +
#'   ylab("Drug Given") +
#'   scale_triangle_height_continuous(
#'     breaks = c(-sleep_effect_max, 0, sleep_effect_max),
#'     range = c(-.75, .75),
#'     limits = triangle_height_range
#'   ) +
#'   ggtitle("Data show the effects of two soporific drugs administered to a group of 10 people") +
#'   labs(caption = "Data from datasets::sleep",
#'        triangle_height = "Observed change\nin sleep hours") +
#'   theme(
#'     legend.position = 'bottom',
#'     legend.key.height = unit(1.75, 'cm'),
#'     legend.key.width = unit(.75, 'cm')
#'   )
#'
geom_triangles <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  layer(
    stat = "identity", geom = GeomTriangles, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Alter triangle width scaling
#'
#' Using scales::rescale_pal, scale triangle width to appear with the given
#' range.
#'
#' @export
scale_triangle_width_continuous <- function(..., range = c(0.1, 1)) {
  continuous_scale(
    aesthetics = "triangle_width",
    scale_name = "triangle_wscale",
    palette = scales::rescale_pal(range),
    ...
  )
}

#' Alter triangle height scaling
#'
#' Using scales::rescale_pal, scale triangle height to appear with the given
#' range.
#'
#' @export
scale_triangle_height_continuous <- function(..., range = c(0.1, 1)) {
  continuous_scale(
    aesthetics = "triangle_height",
    scale_name = "triangle_hscale",
    palette = scales::rescale_pal(range),
    ...
  )
}
