
draw_key_triangle <- function(data, params, size) {
  idx <- rep(seq_len(nrow(data)), each = 3)
  rep_data <- data[idx, ]

  x_off <- as.vector(outer(
    c(-0.5, 0, 0.5),
    data$width
  ))

  y_off <- as.vector(outer(
    c(0, 1, 0),
    data$height
  ))

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



GeomTriangles <- ggproto(
  "GeomTriangles", GeomPoint,
  default_aes = aes(
    colour = "black", fill = "black", size = 0.5, linetype = 1,
    alpha = 1, angle = 0, width = 0.5, height = 0.5
  ),

  draw_panel = function(
    data, panel_params, coord, na.rm = FALSE
  ) {
    # Apply coordinate transform
    df <- coord$transform(data, panel_params)

    # Repeat every row 3x
    idx <- rep(seq_len(nrow(df)), each = 3)
    rep_df <- df[idx, ]
    # Calculate offsets from origin
    x_off <- as.vector(outer(c(-0.5, 0, 0.5), df$width))
    y_off <- as.vector(outer(c(0, 1, 0), df$height))

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
  draw_key = draw_key_triangle
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
#'       height = Petal.Length,
#'       fill = Petal.Length,
#'       color = Species,
#'       width = Petal.Width
#'     )
#'   ) +
#'   geom_triangles(alpha = .7, size = 1) +
#'   scale_fill_viridis_c(end = .8) +
#'   scale_color_viridis_d(option = 'A', end = .8) +
#'   scale_width_continuous(range = c(0.1, 1), n.breaks = 4) +
#'   scale_height_continuous(range = c(0.1, 1), n.breaks = 3) +
#'   ggtitle(
#'     "Sepal length and width and petal length and width of iris flowers",
#'     "Petal length and width are shown by the height and width of each triangle"
#'   ) +
#'   theme_bw()
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

scale_width_continuous <- function(..., range = c(0.1, 1)) {
  continuous_scale(
    aesthetics = "width",
    scale_name = "wscale",
    palette = scales::rescale_pal(range),
    ...
  )
}

scale_height_continuous <- function(..., range = c(0.1, 1)) {
  continuous_scale(
    aesthetics = "height",
    scale_name = "hscale",
    palette = scales::rescale_pal(range),
    ...
  )
}
