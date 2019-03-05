library("hexSticker")
library("png")
library("grid")
library("ggplot2")

g2 <- readPNG(system.file("shiny/www/", "running-logo.png", package = "trackeRapp"))
g2 <- rasterGrob(g2, interpolate = TRUE)

gg <- qplot(c(2, 3), c(2, 3.9), geom = "blank") +
  annotation_custom(g2, xmin = 2, xmax = 3, ymin = 2, ymax = 3.7) +
  # geom_text(aes(x = 2.68, y = 2.4, label = "app", vjust = 0, hjust = 0), color = "white", size = 5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = NULL)
sticker(gg,
        package="trackeR",
        p_size = 8, s_x = 1.02, s_y = 1.15, s_width = 1.4, s_height = 1.7,
        p_y = 0.55,
        p_color = rgb(1, 1, 1, 0.7),
        h_color = "#001f3f",
        h_fill = "#0073b7",
        url = 'www.trackerapp.com',
        u_color = 'white',
        u_size = 1,
        filename="~/Downloads/baseplot.png")

