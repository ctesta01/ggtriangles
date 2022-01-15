# ggtriangles

`ggtriangles` provides the `geom_triangles()` function which plots isoceles
triangles with a horizontal base centered at position `(x,y)` with height `z`.

**note:** ggtriangles is still under development.

### an example plot using ggtriangles

![an example of a plot using geom_triangles that shows the results of a sleep study and whether drug 1 or 2 increased participants' sleep hours](img/sleep.png)


### to install

    devtools::install_github("ctesta01/ggtriangles")
    
### basic usage

    data.frame(x = 1:5) %>%
      ggplot(aes(x = x, y = x, z = x)) +
      geom_triangles()
     
![a scatterplot of triangles with varying heights](img/simplest_example.png)


### some more examples

![ggtriangles example with iris dataset using the triangles height and width 
to represent sepal length and width](img/iris_example.png)

![ggtriangles example with mtcars dataset using the triangles height and width 
to represent cylinders and horsepower](img/mtcars_example2.png)

![ggtriangles also supports angles](img/angles_in_ggtriangles.png)


### motivation

I originally created this because I wanted to be able to render something like
the NYTimes maps of where COVID-19 death rates had increased and decreased since
vaccines were widely made available to adults.

![nyt map of where covid death rates increased since vaccines became available](img/nyt_increased_orig.png)
![nyt map of where covid death rates decreased since vaccines became available](img/nyt_decreased_orig.png)

These maps were originally featured here: <https://www.nytimes.com/interactive/2021/12/28/us/covid-deaths.html> 

I spent some time re-creating something similar (though not exactly the
same) as the NYTimes analysis and graphic to demonstrate that functionality and
use-case for `ggtriangles`.

![](img/nyt_increased_counties.png)
![](img/nyt_decreased_counties.png)

You can check out how I did it in the [`inst/recreate_nytimes_visualization_format.R`](inst/recreate_nytimes_visualization_format.R)
document. Note that these figures are provided purely as examples and not meant
for decision making purposes or scientific usage. 
