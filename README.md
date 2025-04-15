# golfMapsR

Welcome to <strong>golfMapsR</strong>, an R package for working with and visualizing geospatial golf course data. The data consists of detailed golf course polygons (greens, fairways, tee boxes, bunkers, hazards, etc), sourced from Google Earth and OpenStreetMap.

<p align="center">
  <img alt="Light" src="images/maps/erin_hills.png" width="45%">
&nbsp; &nbsp; &nbsp; &nbsp;
  <img alt="Dark" src="https://github.com/abodesy14/golfMapsR/assets/46985185/424b8c59-1971-474d-9a33-3f5feb176203" width="45%">
</p>

## Installation:
You can install golfMapsR from my GitHub:
```r
# install.packages("devtools")
devtools::install_github("abodesy14/golfMapsR")
library(golfMapsR)
```

## Functions:
- ```get_mapped_courses()``` : Start here to get a list of mapped courses. The returned table includes an api_id for each course, which can be used as inputs to the other functions for retrieving or plotting specific courses.
- ```get_polygon_data(api_id)``` : Retrieves polygon level data for a given golf course. This returns an sf object with the type of polygon, square footage of the polygon, the course, hole number, distance to the green from each polygon, etc. The distance calculation is math-based and uses a “as the crow flies” method (straight line distance between centroids). Metrics such as average green size and number of bunkers can be calculated from this data.
- ```plot_course(api_id, hole_num)``` : Plots a full golf course for the api_id supplied and hole numbers supplied. If no argument is entered for hole_num, the whole course will be plotted. Here's an example of how you can plot <strong>Amen Corner</strong> at Augusta: <br> <br>
```plot_course(api_id = "30000000", hole_num = c(11,12,13))```

<p align="left">
  <img src="images/maps/Amen Corner.jpg" width="45%">
</p>


## Shiny App:
You can interact with my Shiny app here to see which courses have been mapped: https://abodesy14.shinyapps.io/golfMapsR/

## Article:
For more backstory on the project, you can view my article here: https://medium.com/towards-data-science/plotting-golf-courses-in-r-with-google-earth-8ee8aa6f6293

## Contributing:
To request a new course mapping, just fill out the Issues Template. If you have any questions or interest in contributing, feel free to make a PR and/or reach out to me at adam.c.beaudet@gmail.com.
