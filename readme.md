# ggtriangles

`ggtriangles` provides the `geom_triangles()` function which plots isoceles
triangles with a horizontal base centered at position `(x,y)` with height `z`.

**note:** ggtriangles is still under development.

### an example plot using ggtriangles

![an example of a plot using geom_triangles that shows the results of a sleep study and whether drug 1 or 2 increased participants' sleep hours](img/sleep.png)


### to install

    devtools::install_github("ctesta01/ggtriangles")


### basic usage

    iris %>%
      ggplot(aes(x = Sepal.Length, y = Sepal.Width, z = Petal.Length)) +
      geom_triangles(width = 0.1, height_scale = 0.05)
      
![a scatterplot of triangles with varying heights showing the Sepal Length, Sepal width, and petal length on the x and y axes and by the height of the triangles](img/basic.png)

### to add a legend

For now, I recommend using patchwork and combining your plots with a manually
constructed legend you can create using `geom_triangles` and `geom_text`.

A great addition to this project would be an improved `draw_key` for
`GeomTriangles`, but I have not made that addition yet.

    library(patchwork)

    z_values <- c(5,-.5,3,1.5,-7)/15

    plt <- ggplot(data.frame(x=1:5, y = 1:5, z = z_values), aes(x=x,y=y,z=z)) +
        geom_triangles(width = 0.1)

    legend <- draw_geom_triangles_size_legend(z_values = c(-5,0,5)/15, height_scale = 1.5, width = 0.1)

    blank_plot <- ggplot() + theme_void()

    (plt + (blank_plot / legend / blank_plot)) +
      plot_layout(ncol = 2, nrow = 1, widths = c(1, .25))


![plot showing triangles on the diagonal with a legend showing an upward triangle labeled 'high' a horizontal dash for 'medium' and a down arrow for 'low'](img/basic_with_legend.png)


### motivation

I originally created this because I wanted to be able to render something like
the NYTimes maps of where COVID-19 death rates had increased and decreased since
vaccines were widely made available to adults.

![nyt map of where covid death rates increased since vaccines became available](img/nyt_increased_orig.png)
![nyt map of where covid death rates decreased since vaccines became available](img/nyt_decreased_orig.png)

I spent some time re-creating something similar (though not exactly the same)
as the NYT analysis to demonstrate that functionality and use-case for `ggtriangles`.

![](img/nyt_increased_counties.png)
![](img/nyt_decreased_counties.png)

You can check out how I did it in the `vignettes/recreating_nytimes_viz.Rmd`
document.
