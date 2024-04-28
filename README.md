# golfMapsR

Welcome to the <strong>golfMapsR</strong>, a repository dedicated to mapping golf courses using geographic data. Each golf course in this repo has been traced in Google Earth to capture various course elements such as fairways, greens, water hazards, bunkers, etc. These detailed polygon outlines are saved in KML and geoJSON formats to be read into R and plotted with ggplot2. 

<p align="center">
  <img alt="Light" src="maps/erin_hills.png" width="45%">
&nbsp; &nbsp; &nbsp; &nbsp;
  <img alt="Dark" src="https://github.com/abodesy14/golfMapsR/assets/46985185/424b8c59-1971-474d-9a33-3f5feb176203" width="45%">
</p>


## Usage
To get started, clone this repo and use the <strong>generate_map.R</strong> script. The course mapping database is limited to those found at data/mapped_course_list/mapped_courses.csv. Use the <strong>set_course_name</strong> variable at the beginning of the script to select a course to render, or navigate to https://abodesy14.shinyapps.io/golfMapsR/ to test out the Shiny app.


## Contributing
Use the <strong>Issues</strong> template to request a new course mapping. 

<strong>Disclaimer</strong>: The process of mapping golf courses in its current state can be time consuming. I would love for this repo to build up a database of mapped courses that can be used by golfers and/or programmers of all kinds. I myself don't have the bandwith to scale this repo at large, so if you have any interest in contributing to this project, please feel free to clone/fork and create a PR.

## Future Roadmap
<li>Layer individual shots on top of maps more programatically. Currently, shot coordinates must be known to do so.</li>
<li>Calculate area of polygon types for each course - this opens the door for course to course comparisons, such as: average green size, number of bunkers, average fairway size, amount of water/hazards, etc. To further iterate on this idea, these attributes could be modeled to a golfers performance. </li>
<li>Computer vision to more programatically map courses</li>
<li>Tree polygons - trees aren't currently outlined in this iteration of the project, but will be considered in the future.</li>
<li>Create R package</li>
<li>Shiny App</li>
