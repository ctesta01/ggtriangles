
#' Stat ggproto that computes a dataframe of triangle vertices given data
#'
#' Given the specified width and height for the triangles, the compute_group
#' function in this ggproto StatTriangles object create a data.frame (triangle_df
#' in the code) which has rows which each represent a vertex of a triangle,
#' with columns for the group (the id of which triangle the vertex belongs to),
#' x, and y along with other aesthetic parameters like color, alpha, linewidth.
#'
#' The code supports an angle parameter which allows the user to adjust the angle
#' the triangles are directed -- however this feature is still under development
#' and may not work well where the x and y axes of a figure are on significantly
#' different scales.
#'
#'
StatTriangles <- ggproto("StatTriangles", Stat,
  required_aes = c('x', 'y', 'z'),
  compute_group = function(data, scales, params, width = 1, height_scale = .05, width_scale = .05, angle = 0) {

    # specify default width
    if (is.null(data$width)) data$width <- 1

    # for each row of the data, create the 3 points that will make up our
    # triangle based on the z, width, height_scale, and width_scale given.
		triangle_df <-
			tibble::tibble(
				group = 1:nrow(data),
				point1 = lapply(1:nrow(data), function(i) {with(data, c(x[[i]] - width[[i]]/2*width_scale, y[[i]]))}),
				point2 = lapply(1:nrow(data), function(i) {with(data, c(x[[i]] + width[[i]]/2*width_scale, y[[i]]))}),
				point3 = lapply(1:nrow(data), function(i) {with(data, c(x[[i]], y[[i]] + z[[i]]*height_scale))})
			)

		# pivot the data into a long format so that each coordinate pair (e.g. vertex)
		# will be its own row
		triangle_df <- triangle_df %>% tidyr::pivot_longer(
			cols = c(point1, point2, point3),
			names_to = 'vertex',
			values_to = 'coordinates'
		)

		# extract the coordinates -- this must be done rowwise because
		# coordinates is a list where each element is a c(x,y) coordinate pair
		triangle_df <- triangle_df %>% rowwise() %>% mutate(
			x = coordinates[[1]],
			y = coordinates[[2]])

		# save the original x and y so we can perform rotations by the
		# given angle with reference to (orig_x, orig_y) as the fixed point
		# of the rotation transformation
    triangle_df$orig_x <- rep(data$x, each = 3)
    triangle_df$orig_y <- rep(data$y, each = 3)

		# fill in aesthetics to the dataframe
    triangle_df$colour <- rep(data$colour, each = 3)
    triangle_df$size <- rep(data$size, each = 3)
    triangle_df$fill <- rep(data$fill, each = 3)
    triangle_df$linetype <- rep(data$linetype, each = 3)
    triangle_df$alpha <- rep(data$alpha, each = 3)
    triangle_df$angle <- rep(data$angle, each = 3)

    # rotate the data according to the angle by first subtracting out the
    # (orig_x, orig_y) component, applying coordinate rotations, and then
    # adding the (orig_x, orig_y) component back in.
		new_coords <- triangle_df %>% mutate(
      x_diff = x - orig_x,
      y_diff = y - orig_y,
      x_new = x_diff * cos(angle) - y_diff * sin(angle),
      y_new = x_diff * sin(angle) + y_diff * cos(angle),
      x_new = orig_x + x_new,
      y_new = orig_y + y_new
		)

		# overwrite the x,y coordinates with the newly computed coordinates
		triangle_df$x <- new_coords$x_new
		triangle_df$y <- new_coords$y_new

    triangle_df
  }
)


#' render a polygon layer using the StatTriangles ggproto
#'
#' @examples
#' data.frame(x = 1:5) %>%
#'   ggplot(aes(x = x, y = x, z = x, width = 1)) +
#'   stat_triangles()
stat_triangles <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatTriangles, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


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
#'
#' @examples
#'
#' ggplot(mtcars, aes(x = cyl, y = hp, z = hp - mean(hp), width = 1)) +
#'   geom_triangles()
#'
#' ggplot(mtcars, aes(x = cyl, y = hp, z = hp - mean(hp), width = 1)) +
#'   geom_triangles() +
#'   geom_smooth(formula = "y ~ 1", method = 'lm') +
#'   ggtitle("A plot showing amount above and below average and by how much")
#'
GeomTriangles <- ggproto("GeomTriangles", GeomPolygon,
	default_aes = aes(
			color = 'black', fill = "black", size = 0.5, linetype = 1, alpha = 1, angle = 0, width = 1
		),
  draw_key = draw_key_polygon
)

#' Plot triangles with base centered at (x,y) and height z
#'
#' Draw triangles with base at (x,y), specified width, and height
#' given by aesthetic argument z.
#'
#' @param x,y the x-y coordinates of the midpoint of the base of the triangle
#' @param z the height for each triangle, subject to multiplication by height_scale
#' @param width the width of the base of the triangle
#' @param width_scale the width of the base of the triangle
#' @param height_scale the scaling factor (default: 1) for the triangle's height relative to the y values
#'
#' @seealso GeomTriangle
#'
#' @export
#'
#' @examples
#'
#' # nearly simplest possible example
#' data.frame(x = 1:5) %>%
#'   ggplot(aes(x = x, y = x, z = x)) +
#'   geom_triangles()
#'
#' data.frame(x = -1*1:5) %>%
#'   ggplot(aes(x = x, y = x, z = x)) +
#'   geom_triangles()
#'
#' # works with facets
#' data.frame(x = 1:5) %>%
#'   ggplot(aes(x = x, y = x, z = x)) +
#'   geom_triangles() +
#'   facet_wrap(~x)
#'
#' # alpha, width, fill, and color aesthetics are supported
#' data.frame(x = 1:5) %>%
#'   ggplot(aes(x = x, y = x, z = x, fill = x, color = x, width = x, alpha = x)) +
#'   geom_triangles(color = NA, height_scale = 1, width_scale = 1) +
#'   geom_point(color = 'black', alpha = 1) +
#'   scale_y_continuous(breaks = 1:10) +
#'   scale_x_continuous(breaks = seq(.5, to = 7.5, by = .5))
#'
#' # angle is supported
#' data.frame(x = c(0:30)) %>%
#'   ggplot(aes(x = x, y = x, z = x/15+1, fill = x, alpha = x, angle = x/10*pi)) +
#' 	 geom_triangles(color = NA, height_scale = 1, width_scale = 1) +
#'   geom_point(color = 'black', alpha = 1) + scale_fill_viridis_c()
#'
#'
#' # here's an example with the iris dataset making use of height, width, and
#' # color to communicate variables from the dataset
#'
#' iris %>%
#'   ggplot(
#'     aes(
#'       x = Sepal.Length,
#'       y = Sepal.Width,
#'       z = Petal.Length,
#'       fill = Petal.Length,
#'       width = Petal.Width
#'     )
#'   ) +
#'   geom_triangles(color = NA,
#'                  width_scale = .1,
#'                  alpha = .7) +
#'   scale_fill_viridis_c(end = .8) +
#'   ggtitle(
#'     "Sepal length and width and petal length and width of iris flowers",
#'     "Petal length and width are shown by the height and width of each triangle"
#'   ) +
#'   theme_bw()
#'
#' # an example with the mtcars dataset showing mpg, displacement, cylinders,
#' # weight, and horsepower for each vehicle -- makes use of ggrepel::geom_text_repel.
#' #
#' # best viewed as around a 8.5in by 7in landscape figure
#'
#' library(ggrepel)
#'
#' mtcars %>%
#'   tibble::rownames_to_column('name') %>%
#'   ggplot(aes(x = mpg, y = disp, z = cyl, width = wt, color = hp, fill = hp, label = name)) +
#'   geom_triangles(height_scale = 2, width_scale = .5, alpha = .7) +
#'   geom_point(color = 'black', size = 1) +
#'   ggrepel::geom_text_repel(color = 'black', size = 2, nudge_y = -10) +
#'   scale_fill_viridis_c(end = .6) +
#'   scale_color_viridis_c(end = .6) +
#'   xlab("miles per gallon") +
#'   ylab("engine displacement (cu. in.)") +
#'   labs(fill = 'horsepower', color = 'horsepower') +
#'   ggtitle("MPG, Engine Displacement, # of Cylinders, Weight, and Horsepower of Cars from the 1974 Motor Trends Magazine",
#'   "Cylinders shown in height, weight in width, horsepower in color") +
#'   theme_bw() +
#'   theme(plot.title = element_text(size = 10), plot.subtitle = element_text(size = 8))
#'
geom_triangles <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatTriangles, geom = GeomTriangles, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

