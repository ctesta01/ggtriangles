
#' GeomTriangles ggproto
#'
#' draw_group creates a "triangle_df" which contains in each row the
#' coordinates for a vertex of one of the triangles produced.
#'
#' The simplified version is that given the aes() arguments x, y, z, and
#' optionally width and height_scale, triangles are drawn which have a
#' horizontal base which is specified with midpoint (x,y). The vertices adjacent
#' to the base edge are at points (x-width,y) and (x+width,y).
#'
#' The 3rd and final vertex is positioned at (x,y+z).
#'
#' The complicated version is just taking into account the x,y,z and width
#' values are modified by the x and y coordinate systems and the height_scale
#' aesthetic argument.
#'
#' This system is currently designed to plot triangles which have a flat/horizontal
#' edge with midpoint (x,y), but the system could potentially be extended in future
#' work to adjust the angle of the triangles.
#'
#' @export
GeomTriangles <- ggproto("GeomTriangles", Geom,
                        required_aes = c("x", "y", "z"),

                        default_aes = aes(
                          colour = 'black', fill = "black", size = 0.5,
                          linetype = 1, alpha = 1, width = 1, height_scale = 1
                        ),

                        draw_key = draw_key_polygon,

                        draw_group = function(data, panel_params, coord) {

                          coords <- coord$transform(data, panel_params)

                          triangle_df <-
                            tibble(
                              group = 1:nrow(coords),
                              point1 = lapply(1:nrow(coords), function(i) {with(coords, c(x[[i]] - width[[i]], y[[i]]))}),
                              point2 = lapply(1:nrow(coords), function(i) {with(coords, c(x[[i]] + width[[i]], y[[i]]))}),
                              point3 = lapply(1:nrow(coords), function(i) {with(coords, c(x[[i]], y[[i]] + z[[i]]*height_scale[[i]]))})
                            )

                          triangle_df <- triangle_df %>% tidyr::pivot_longer(
                            cols = c(point1, point2, point3),
                            names_to = 'vertex',
                            values_to = 'coordinates'
                          )

                          triangle_df <- triangle_df %>% rowwise() %>% mutate(
                            x = coordinates[[1]],
                            y = coordinates[[2]])

                          grid::polygonGrob(
                            triangle_df$x, triangle_df$y, id = triangle_df$group,
                            default.units = "native",
                            gp = grid::gpar(
                              col = rep(coords$colour, each = 3),
                              fill = rep(coords$fill, each = 3),
                              alpha = rep(coords$alpha, each = 3)
                            )
                          )
                        }
)


#' Triangle plotter -- plot triangles with base x,y and height z
#'
#' Draw triangles with base at (x,y), specifieid width, and height
#' given by aesthetic argument z.
#'
#' @param x,y the x-y coordinates of the midpoint of the base of the triangle
#' @param z the height for each triangle, subject to multiplication by height_scale
#' @param width the width of the base of the triangle
#' @param height_scale the scaling factor (default: 1) for the triangle's height relative to the y values
#'
#' @seealso GeomTriangle
#'
#' @export
#'
#' @examples
#'
#' # example 1
#' iris %>%
#'   ggplot(aes(x = Sepal.Length, y = Sepal.Width, z = Petal.Length)) +
#'   geom_triangles(width = 0.01, height_scale = 0.005) +
#'   ggtitle("Sepal length, width, and petal length of iris flowers",
#'   "Petal length is shown by the height of each triangle")
#'
#' # example 1.2 -- using variable width
#'  iris %>%
#' ggplot(aes(x = Sepal.Length, y = Sepal.Width, z = Petal.Length, width = Petal.Width*0.01)) +
#'   geom_triangles(height_scale = 0.005) +
#'  ggtitle("Sepal length, width, and petal length of iris flowers",
#'          "Petal length is shown by the height of each triangle, and petal width by the width of each triangle")
#'
#' # example 2
#' mtcars %>%
#' ggplot(aes(x = mpg, y = disp, z = cyl, color = hp, fill = hp)) +
#'   geom_triangles(width = 0.01, height_scale = .007) +
#'   scale_fill_viridis_c() +
#'   scale_color_viridis_c()
#'
#' # example 3
#' ggplot(data.frame(x=1:5, y = 1:5, z = (c(5,-.5,3,1.5,-7)/15)), aes(x=x,y=y,z=z)) +
#'   geom_triangles(width = 0.01, height_scale = 0.1)
#'
#' # example with legend using patchwork
#' library(patchwork)
#'
#' z_values <- c(5,-.5,3,1.5,-7)/15
#'
#' plt <- ggplot(data.frame(x=1:5, y = 1:5, z = z_values), aes(x=x,y=y,z=z)) +
#'     geom_triangles(width = 0.01, height_scale = 0.1)
#'
#' legend <- draw_geom_triangles_size_legend(z_values = z_values, height_scale = .25, width = 0.05)
#'
#' blank_plot <- ggplot() + theme_void()
#'
#' (plt + (blank_plot / legend / blank_plot)) +
#'   plot_layout(ncol = 2, nrow = 1, widths = c(1, .25))
#'
geom_triangles <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  layer(
    geom = GeomTriangles, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
