
#' Draw geom_triangle legend based on size
#'
#' @export
#'
#' @examples
#'
#'  vertical = TRUE
#'  width = 1
#'  height_scale = 1
#'  z_values = 1:3/3
#'  n.breaks = 3
#'  labels = round(1:3/3, 1)
#'
#'
#' ggplot(
#'   data = data.frame(x = rep(0, times = n.breaks),
#'                     y = seq(1,n.breaks),
#'                     z = quantile(z_values, seq(0, 1, length.out = n.breaks)) %>% as.vector(),
#'                     label = labels
#'   ),
#'   mapping = aes(x = x, y = y, z = z, label = label)
#' ) +
#' geom_triangles()
#'
#' ggplot(mtcars, aes(x = mpg, y = cyl, z = disp, label = disp)) +
#'   ggtriangles::geom_triangles(height_scale = 0.001)
#'
draw_geom_triangles_size_legend <- function(
  width = 1/15,
  height_scale = 1/15,
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
                      label = labels
    ),
    mapping = aes(x = x, y = y, z = z, label = label)
  ) +
    geom_triangles(width = width, height_scale = height_scale, color = color, fill = fill) +
    geom_text(mapping = aes(x = rep(width + 0.25, times = n.breaks), y = seq(1, n.breaks)+0.25), hjust = 0) +
    expand_limits(x = c(-0.5, width + 1), y = c(.5, n.breaks + .5)) +
    theme_void()
}
