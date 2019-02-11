library("hexSticker")
library("png")
library("grid")
library("ggplot2")

"#FF94B7"
"#FFB32F"
"#B6D000"
"#00E380"
"#00E6E6"
"#28CFFF"
"#F3A2FF"


g2 <- readPNG(system.file("icons", "man-running_1.png", package = "trackeRapp"))
# g2 <- matrix(rgb(g2[,,1], g2[,,2], g2[,,3], g2[,,4] * 1), nrow = dim(g2)[1])
g2[g2 == 0] <-  "#00000000"
g2[g2 == 1] <- "#FFFFFFFF"
g2 <- rasterGrob(g2, interpolate = TRUE)

gg <- qplot(c(2, 3), c(2, 3.9), geom = "blank") +
  annotation_custom(g2, xmin = 2, xmax = 3, ymin = 2, ymax = 3.9) +
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
        p_color = "white",
        h_color = "#00354f",
        h_fill = "#034e72",
        url = 'www.trackerapp.com',
        u_color = 'white',
        u_size = 1,
        filename="~/Downloads/baseplot.png")

