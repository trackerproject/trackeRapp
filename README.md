trackeRapp <img src="README_files/trackeRapp.svg" width="360" align="right">
============================================================================

[**trackeRapp**](https://trackerapp.com) is a
[**shiny**](http://shiny.rstudio.com) interface for the analysis and
tracking of athletic activity data in R.

**trackeRapp** builds on the extensive infrastructure provided by the
[**trackeR**](https://cran.r-project.org/package=trackeR) R package and
offers a user-friendly web-interface that implements an integrated
workflow for the analysis of running, cycling and swimming data from
GPS-enabled tracking devices through flexible and extensive interactive
visualisations and data-analytic tools. **trackeRapp** offers
functionality to import data from raw activity files of popular formats
(tcx, gpx, json and db3), clean them, organised them and finally export
images of processed activity data that can be used for further analysis
not only within *trackeRapp* but also for more advanced modelling in R.

Installation
------------

You can install **trackeRapp** from CRAN by typing

    install.packages("trackeRapp")

You can also install the development version of **trackeRapp** from
github by doing

    # install.packages("devtools")
    devtools::install_github("trackerproject/trackeRapp")

Getting started
---------------

Please see the [**tour de
trackeRapp**](https://trackerproject.github.io/trackeRapp/) pages for
tutorial videos, explanation of the visualisations, and to learn more
about **trackeRapp** and all of its capabilities.

Below are just a few screenshots from the **trackeRapp** web interface

<img src="README_files/evening_sessions.png" alt="Session summaries" width="95%" />
<p class="caption">
Session summaries
</p>

<br>

<img src="README_files/map.png" width="95%" style="display: block; margin: auto;" />

<br>

<img src="README_files/session_summaries.png" width="95%" style="display: block; margin: auto;" />

<br>

<img src="README_files/zones_multiple_sessions.png" width="95%" style="display: block; margin: auto;" />

Video channel
-------------

The YouTube channel at features video tutorials about and the workflow
it provides.

Launching the user-interface
----------------------------

The web-interface can be accessed remotely at
<https://www.trackerapp.com> or on a local machine by running the
following commands within R:

    # Load the package
    library("trackeRapp")
    # Open the interface in the browser
    trackeR_app()

Code of Conduct
---------------

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
