
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- ```{r, echo = FALSE} -->

<!-- knitr::opts_chunk$set( -->

<!--   collapse = TRUE, -->

<!--   comment = "#>", -->

<!--   fig.path = "README-" -->

<!-- ) -->

<!-- ``` -->

# The trackeRapp

The goal of the
[**trackeRapp**](https://trackerapp.com) is to
provide an integrated workflow and a web interface for the analysis of sports
data from GPS-enabled tracking devices. The **trackeRapp** is a platform with flexible and extensive
visualisation and analysis tools. It has a user friendly, intuitive and
adjustable interface and it was developed open-source on top of the
[**trackeR**](https://github.com/trackerproject/trackeR) package. The
interface was fully built using the **shiny** package.

## Installation

You can install the **trackeRapp** from CRAN:

``` r
install.packages('trackeRapp')
```

You can also install the **trackeRapp** from github with:

``` r
devtools::install_github("trackerproject/trackeRapp")
```

## Getting started

Please see the [**tour de
trackeRapp**](https://trackerproject.github.io/trackeRapp/) to learn about the **trackeRapp** and all its capabilities, including many tutorial videos and visualisations. 

## Example

A user can run the web interface on their local machine by running the
following commands:

``` r
# Load the package
library(trackeRapp)
# Open the interface in the browser
trackeR_app()
```
