get_coord.trackeRdata <- function(x, keep = 0.1) {
    sessions <- seq_along(x)
    out <- lapply(sessions, function(s) {
        coord <- coredata(x[[s]])[, c("longitude", "latitude")]
        subsample <- seq(1, nrow(coord), length.out = ceiling(keep * nrow(coord)))
        st_multilinestring(list(coord[subsample, ]))
    })
    st_sf(session = sessions, geometry = st_sfc(out))
}

## https://blog.cpsievert.me/2018/03/30/visualizing-geo-spatial-data-with-sf-and-plotly/

if (FALSE)
{

    library(plotly)
    library(sf)
    library(crosstalk)
    library(pryr)

    data(trails, package = 'mapview')

    kantas <- readRDS("inst/extdata/sample.rds")

    kantas <- readRDS("~/Dropbox/Student projects/Robin project/Data//Nikos_kantas_activities.rds")

    kantas_sf <- get_coord.trackeRdata(kantas, keep = 0.05)
    st_crs(kantas_sf) <- 4326

    ## works
    shda <- SharedData$new(kantas_sf)
    bscols(
        plot_mapbox(shda,
                    text = ~ session,
                    hoverinfo = "text"),
        DT::datatable(shda))


    ## works
    plot_mapbox(shda,
                split = ~ session,
                hoverinfo = "split")

    ##

}
