#############################################
# Active fire maps with R
# Milos Popovic 2023/08/22
#############################################

libs <- c(
    "tidyverse", "data.table",
    "ggmap", "gganimate"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libs == F)){
    install.packages(
        libs[!installed_libs]
    )
}

invisible(lapply(libs, library, character.only = T))

# 1. GET AREA
#------------

# 11.456961,36.608032,18.684083,42.024922

xmin <- 11.4
ymin <- 36.6
xmax <- 18.7
ymax <- 42

area_coords <- c(
    xmin, ymin, xmax, ymax
)

area <- paste(
    area_coords,
    sep = ",",
    collapse = ","
)

# 2. FIRE DATA
#-------------

get_fire_data <- function(
    main_url,
    map_key,
    source,
    area,
    day_range,
    date){
        url <- paste(
            main_url,
            map_key,
            source,
            area,
            day_range,
            date,
            sep = "/"
        )

    fire_data <- data.table::fread(url)

    return(fire_data)

    }

main_url <- "https://firms.modaps.eosdis.nasa.gov/api/area/csv"
map_key <- "******************************" # PLEASE SET YOUR OWN MAPKEY
source <- "VIIRS_SNPP_NRT"
day_range <- 10
date <- Sys.Date() - 11

fire_data <- get_fire_data(
    main_url = main_url,
    map_key = map_key,
    source = source,
    area = area,
    day_range = day_range,
    date = date
)

head(fire_data)
nrow(fire_data)

# 3. BACKGROUND
#--------------

bg_layer <- ggmap::get_stamenmap(
    bbox = area_coords,
    zoom = 7,
    maptype = "terrain",
    color = "bw",
    force = T
)

ggmap::ggmap(bg_layer)

# 4. THEME
#----------

theme_for_the_win <- function(){
    theme_void() +
    theme(
        legend.position = "right",
        legend.title = element_text(
            size = 12, color = "grey10"
        ),
        legend.text = element_text(
            size = 11, color = "grey10"
        ),
        plot.title = element_text(
            size = 16, color = "grey10",
            hjust = .5
        ),
        plot.subtitle = element_text(
            face = "bold",
            size = 24, color = "firebrick",
            hjust = .5
        ),
        plot.caption = element_text(
            size = 10, color = "grey30",
            hjust = .5, vjust = 0
        ),
        plot.margin = unit(
            c(t = -2, r = 0, b = -5, l = .1),
            "lines"
        )
    )
}

# 5. MAP
#-------

fire_data$datum <- as.Date(
    fire_data$acq_date
)

p <- ggmap::ggmap(bg_layer) +
    geom_point(
        data = fire_data,
        aes(
            x = longitude,
            y = latitude,
            color = bright_ti5 - 273.15,
            group = datum
        ),
        inherit.aes = F
    ) +
    scale_color_gradientn(
        name = "Brightness\ntemperature\n(°C)",
        colors = rev(hcl.colors(
            6, "Inferno"
        ))
    ) +
    theme_for_the_win() +
    labs(
        title = "Wilfires 11-20 August 2023",
        caption = "Data: NASA FIRMS",
        subtitle = "{as.Date(frame_time)}"
    )
    
# 6. ANIMATE
#-----------

timelapse_map <- p +
gganimate::transition_time(
    time = as.Date(datum)
) +
gganimate::shadow_mark() +
gganimate::enter_fade() +
gganimate::exit_fade() +
gganimate::ease_aes(
    "cubic-in-out",
    interval = .25
)

animated_map <- gganimate::animate(
    timelapse_map,
    nframes = 60,
    duration = 12,
    start_pause = 3,
    end_pause = 30,
    width = 7,
    height = 7,
    units = "in",
    res = 300,
    fps = 15,
    renderer = gifski_renderer(
        loop = T
    )
)

gganimate::anim_save(
    "fire-southern-italy.gif",
    animated_map
)
