remotes::install_github("jnolis/ggirl")
library(ggirl)

contact_email <- "fakeemailforreal@gmail.com"

send_address_1 <- address(name = "Fake Personname",
                          address_line_1 = "250 North Ave",
                          city = "Boston",
                          state = "MA",
                          postal_code = "22222",
                          country = "US")

message_1 <- "Come join us every other Thursday at noon!"

# ------------------------------------------------------------------------------
library(tidyverse)
library(randomcoloR)
library(gganimate)
library(transformr)
library(showtext)
library(colorspace)

# Loading Accenture fonts I found from here: www.whatfontis.com
font_add("rotis", "../fonts/rotis-sans-serif.ttf") # old accenture font
font_add("unko", "../fonts/unkoi8b.ttf") # new accenture font

# Automatically use showtext to render text for future devices
showtext_auto()

# Tell showtext the resolution of the device, only needed for bitmap graphics.
showtext_opts(dpi = 96)

# Add starting coordinates. These are the xy coordinates for the accenture
# greater than signs
accenture_x1 <- c(240, 600, 510, 240)
accenture_y1 <- c(40, 190, 230, 125)
accenture_x2 <- c(240, 600, 600, 240)
accenture_y2 <- c(420, 270, 190, 335)
accenture_color1 <- "green"
accenture_color2 <- "darkgreen"
accenture_group <- 1

# create dataframe with initial values in it.
accenture <- data.frame(
  accenture_x1, 
  accenture_y1,
  accenture_x2, 
  accenture_y2,
  accenture_group,
  accenture_color1,
  accenture_color2
)

# this is a loop to populate the original dataframe from above. In it we take 
# take te most recent values and change the x,y coordinates a little bit so that
# the points all move in a circle
for (i in seq(1, 360, by = 0.5)) {
  loop_df <- accenture %>%
    filter(accenture_group == max(accenture_group)) %>%
    mutate(
      accenture_x1 = accenture_x1 + cos(i) * 325, #cos(angle)*radius
      accenture_y1 = accenture_y1 + sin(i) * 325, #sin(angle)*radius
      accenture_x2 = accenture_x2 + cos(i) * 325, #cos(angle)*radius
      accenture_y2 = accenture_y2 + sin(i) * 325, #sin(angle)*radius
      accenture_group = accenture_group + 1,
      accenture_color1 = randomColor(),
      accenture_color2 = lighten(accenture_color1, amount = 0.15, method = "relative", space = "HCL")
    )
  
  accenture <- accenture %>%
    bind_rows(loop_df)
}

# duplicate the dataset 100 times and then create polygon_alpha to add a bunch 
# more polygons with decreasing alpha. This will give a cool effect.
accenture <- accenture %>%
  uncount(100, .id = "frame") %>%
  filter(accenture_group <= frame) %>%
  arrange(frame, accenture_group) %>%
  group_by(
    frame
  ) %>%
  mutate(
    accenture_x_lag1 = lag(accenture_x1), 
    accenture_y_lag1 = lag(accenture_y1), 
    accenture_x_lag2 = lag(accenture_x2), 
    accenture_y_lag2 = lag(accenture_y2), 
    tail = last(accenture_group) - accenture_group,
    polygon_alpha = pmax(0, (10-tail)/10)
  ) %>%
  ungroup() %>%
  mutate(
    accenture_label_x = sum(max(max(accenture_x1), max(accenture_x2)), min(min(accenture_x1), min(accenture_x2)))/2,
    accenture_label_y = sum(max(max(accenture_y1), max(accenture_y2)), min(min(accenture_y1), min(accenture_y2)))/2
  )

# Plot the logo.
dss_logo <- 
  accenture %>%
  filter(frame == 44) %>%
  ggplot() +
  geom_polygon(aes(x = accenture_x1, y = accenture_y1, fill = accenture_color2, alpha = polygon_alpha)) +
  geom_polygon(aes(x = accenture_x2, y = accenture_y2, fill = accenture_color1, alpha = polygon_alpha)) +
  annotate(
    "text",
    x = unique(accenture$accenture_label_x),
    y = seq(
      unique(accenture$accenture_label_y)+100,
      unique(accenture$accenture_label_y)-100,
      length = 3
    ),
    label = list(
      "Accenture",
      "Federal~Services",
      "Data~Science~Social"
    ),
    family = "unko",
    size = 12,
    color = "#4B636E",
    parse = TRUE
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#FFFFFF"),
    legend.position = "none"
  ) +
  scale_alpha(range = c(0,1)) +
  guides(alpha = F) +
  coord_equal()

# mail postcard ----------------------------------------------------------------
ggpostcard(dss_logo,
           contact_email, 
           messages = message_1,
           send_addresses = send_address_1)
