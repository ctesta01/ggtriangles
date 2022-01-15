
#' Draw geom_triangle legend based on its height (z)
#'
#' @export
#'
#' @examples
#'
#' draw_geom_triangles_height_legend()
#'
#' draw_geom_triangles_height_legend(
#' width = c(min(mtcars$cyl),median(mtcars$cyl), max(mtcars$cyl)),
#' labels = c(min(mtcars$cyl),median(mtcars$cyl), max(mtcars$cyl)),
#' ) +
#' ggtitle("Cylinders") +
#' theme(plot.title = element_text(hjust = 0.5))
#'
draw_geom_triangles_height_legend <- function(
  width = 1,
  width_scale = .1,
  height_scale = .1,
  z_values = 1:3,
  n.breaks = 3,
  labels = c("low", "medium", "high"),
  color = 'black',
  fill = 'black'
) {
  ggplot(
    data = data.frame(x = rep(0, times = n.breaks),
                      y = seq(1,n.breaks),
                      z = quantile(z_values, seq(0, 1, length.out = n.breaks)) %>% as.vector(),
                      width = width,
                      label = labels,
                      color = color,
                      fill = fill
    ),
    mapping = aes(x = x, y = y, z = z, label = label, width = width)
  ) +
    geom_triangles(width_scale = width_scale, height_scale = height_scale, color = color, fill = fill) +
    geom_text(mapping = aes(x = x + .5), size = 3) +
    expand_limits(x = c(-.25, 3/4)) +
    theme_void() +
    theme(plot.title = element_text(size = 10, hjust = .5))
}

#' Draw geom_triangle legend based on its width
#'
#' @export
#'
#' @examples
#'
#' draw_geom_triangles_width_legend()
#'
#' draw_geom_triangles_width_legend(
#' width = quantile(mtcars$wt, c(.33, .66, 1)),
#' labels = round(quantile(mtcars$wt, c(.33, .66, 1)), 2)
#' ) +
#' ggtitle("Weight") +
#' theme(plot.title = element_text(hjust = 0.5))
#'

draw_geom_triangles_width_legend <- function(
  width = 1:3,
  width_scale = .1,
  height_scale = .1,
  z_values = 1,
  n.breaks = 3,
  labels = c("low", "medium", "high"),
  color = 'black',
  fill = 'black'
) {
  ggplot(
    data = data.frame(x = rep(0, times = n.breaks),
                      y = seq(1, n.breaks),
                      z = rep(1, n.breaks),
                      width = width,
                      label = labels,
                      color = color,
                      fill = fill
    ),
    mapping = aes(x = x, y = y, z = z, label = label, width = width)
  ) +
    geom_triangles(width_scale = width_scale, height_scale = height_scale, color = color, fill = fill) +
    geom_text(mapping = aes(x = x + .5), size = 3) +
    expand_limits(x = c(-.25, 3/4)) +
    theme_void() +
    theme(plot.title = element_text(size = 10, hjust = .5))
}
