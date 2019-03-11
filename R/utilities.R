get_coord.trackeRdata <- function(x) {
    sessions <- seq_along(x)
    out <- lapply(sessions, function(s) {
        st_linestring(coredata(x[[s]])[, c("longitude", "latitude")])
    })
    st_sf(session = sessions, geometry = st_sfc(out))
}

## https://blog.cpsievert.me/2018/03/30/visualizing-geo-spatial-data-with-sf-and-plotly/
