# ggtriangles

`ggtriangles` provides the `geom_triangles()` function which plots isoceles
triangles with a horizontal base centered at position `(x,y)` with height `z`.

**note:** ggtriangles is still under development.

### an example plot using ggtriangles

![an example of a plot using geom_triangles that shows the results of a sleep study and whether drug 1 or 2 increased participants' sleep hours](img/sleep.png)


### to install

    devtools::install_github("ctesta01/ggtriangles")
    
### basic usage

    library(ggtriangles)
    data.frame(x = 1:5) %>%
      ggplot(aes(x = x, y = x, z = x)) +
      geom_triangles()

![a scatterplot of triangles with varying heights](img/simplest_example.png)

### how to get started

Check out the examples available in the help pages by entering `?geom_triangles` into
R after you've loaded the package.


### some more examples

The code for these is available in `?geom_triangles`.

![ggtriangles example with iris dataset using the triangles height and width 
to represent sepal length and width](img/iris_example.png)

![ggtriangles example with mtcars dataset using the triangles height and width 
to represent cylinders and horsepower](img/mtcars_example2.png)

ggtriangles also supports angles as well as transparency!

![ggtriangles also supports angles](img/angles_in_ggtriangles.png)

I think ggtriangles will provide a nice interface both for Rtists who are
looking to incorporate triangles programmatically in their generative art, as
well as data visualization creators who are interested in using triangles'
position, height, width, color, and fill to communicate features of
their data.

![an example of triangles produced by ggtriangles arranged in a spiral where every 
triangle points to the next triangle in the inward direction](img/spiral.png)


### motivation

I originally created this because I wanted to be able to render something like
the NYTimes maps of where COVID-19 death rates had increased and decreased since
vaccines were widely made available to adults.

#### origianl nytimes version:

![nyt map of where covid death rates increased since vaccines became available](img/nyt_increased_orig.png)
![nyt map of where covid death rates decreased since vaccines became available](img/nyt_decreased_orig.png)

These maps were originally featured here: <https://www.nytimes.com/interactive/2021/12/28/us/covid-deaths.html> 

I spent some time re-creating something similar (though not exactly the
same) as the NYTimes analysis and graphic to demonstrate that functionality and
use-case for `ggtriangles`.

#### ggtriangles version:

![](img/nyt_increased_counties.png)
![](img/nyt_decreased_counties.png)

You can check out how I did it in the [`inst/recreate_nytimes_visualization_format.R`](inst/recreate_nytimes_visualization_format.R)
document. Note that these figures are provided purely as examples and not meant
for decision making purposes or scientific usage. 


### feedback

Please feel free to make suggestions and provide feedback on ggtriangles in an
issue on this GitHub repository.
