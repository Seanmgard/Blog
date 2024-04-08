
# Load Packages -----------------------------------------------------------

pacman::p_load(dplyr, ggplot2, readxl, readr, tidyr, ggbeeswarm, ggtext, showtext, ggimage)

# Specify Font ------------------------------------------------------------

#Specify font
font_add_google("Lato", "lato")
showtext_auto()

# Load Data ---------------------------------------------------------------

Beatles_Album_List <- c("Please Please Me (Remastered)", "With The Beatles (Remastered)", "A Hard Day's Night (Remastered)",
                        "Beatles For Sale (Remastered)", "Help! (Remastered)", "Rubber Soul (Remastered)", "Revolver (Remastered)",
                        "Sgt. Pepper's Lonely Hearts Club Band (Remastered)", "Magical Mystery Tour (Remastered)", "The Beatles (Remastered)",
                        "Yellow Submarine (Remastered)", "Abbey Road (Remastered)", "Let It Be (Remastered)")

Rename_Map <- c("Please Please Me (Remastered)" = "Please Please Me", "With The Beatles (Remastered)" = "With The Beatles",
                "A Hard Day's Night (Remastered)" = "Hard Day's Night", "Beatles For Sale (Remastered)" = "Beatles For Sale",
                "Help! (Remastered)" = "Help!", "Rubber Soul (Remastered)" = "Rubber Soul", "Revolver (Remastered)" = "Revolver",
                "Sgt. Pepper's Lonely Hearts Club Band (Remastered)" = "Sgt. Peppers", "Magical Mystery Tour (Remastered)" = "Magical Mystery Tour",
                "The Beatles (Remastered)" = "White Album", "Yellow Submarine (Remastered)" = "Yellow Submarine",
                "Abbey Road (Remastered)" = "Abbey Road", "Let It Be (Remastered)" = "Let It Be")

Beatles_Raw <- read_csv("Projects/Beatles Emotional Score/beatles_spotify.csv")

Beatles <- Beatles_Raw |> 
  select(name, album, release_date, energy, URL)

Beatles_Albums <- Beatles |> 
  filter(album %in% Beatles_Album_List) |> 
  mutate(release_date = as.Date(release_date, format = "%m/%d/%Y")) |> 
  arrange(release_date)

Beatles_Albums$album <- Rename_Map[Beatles_Albums$album]

Beatles_Albums <- Beatles_Albums |> 
  filter(!album %in% c("Magical Mystery Tour", "Yellow Submarine"))

average_energy <- mean(Beatles_Albums$energy, na.rm = TRUE)

# Visualize Data ----------------------------------------------------------

#Title and Subtitle Text
Title <- "Using Machine Learning To Track The Energy of the Beatles"
Subtitle <- "Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic 
tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low. Perceptual 
features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."
Caption <- "Data from Spotify. Chart by Sean Gardner (seanmgard.github.io)"

min_x <- min(Beatles_Albums$energy)

Beatles_Plot <- ggplot(Beatles_Albums, aes(x=energy, y = factor(album, level=c("Let It Be", "Abbey Road", "Yellow Submarine", "White Album",
                                                         "Magical Mystery Tour", "Sgt. Peppers", "Revolver", "Rubber Soul",
                                                         "Help!", "Beatles For Sale", "Hard Day's Night",
                                                         "With The Beatles", "Please Please Me")))) + 
  geom_point(aes(color = album), size = 1.5, alpha = 0.55) +
  geom_image(data = Beatles_Albums, mapping = aes(x=-0.01, y = album, image=URL), size = 0.08) +
  labs(x = "Energy Score", title = Title, subtitle = Subtitle, caption = Caption) +
  geom_vline(xintercept = average_energy, linetype = "dashed", color = "white") +
  annotate("text", x = average_energy + 0.33, y = length(levels(factor(Beatles_Albums$album))) /2, label = "Average Energy Per Song", color = "white", size = 8, family = "lato") +
  geom_curve(aes(x = average_energy + 0.2, y = length(levels(factor(Beatles_Albums$album))) /2, xend = average_energy, yend = length(levels(factor(Beatles_Albums$album)))/2), color = "white", curvature = -0.3, size = 0.05, arrow = arrow(type = "closed", length = unit(0.06, "inches"))) +
  theme_void() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position = 'none',
    axis.title = element_blank(),
    axis.text = element_text(size = 20, colour = "white", family = "lato", hjust = 0),
    axis.text.y = element_blank(),
    text = element_text(size = 24, lineheight = 0.3, colour = "white", family = "lato"),
    plot.background = element_rect(fill = '#1E212B'),
    panel.background = element_blank(), 
    panel.grid=element_blank(),
    plot.title = element_text(color="white", face="bold", family = "lato", size=48, margin=margin(t=10)),
    plot.subtitle = element_text(color= "white", size=24, family = "lato", margin=margin(t = 5, b = 20)),
    plot.caption = element_markdown(colour = "white", hjust = 0, family = "lato", margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 5, r = 20, l = 20))

ggsave(filename="Projects/Beatles Emotional Score/Beatles_Plot.png", height=7, width=6.2,   bg = "white")


# Additional Analysis -----------------------------------------------------

Album_Rankings <- Beatles |> 
  group_by(album) |> 
  summarize(Mean_energy = mean(energy)) |> 
  filter(album %in% Beatles_Album_List)

Top_Songs <- Beatles_Albums |> 
  group_by(album) |> 
  top_n(1, energy) |> 
  ungroup() |> 
  select(-URL)

Bottom_Songs <- Beatles_Albums |> 
  group_by(album) |> 
  top_n(-1, energy) |> 
  ungroup() |> 
  select(-URL) 


# Songwriting Credits -----------------------------------------------------

Songwriters_Raw <- read_excel("Projects/Beatles Emotional Score/Beatles_Songwriter_List.xlsx")

Songwriters <- Songwriters_Raw |> 
  filter(`Main composer` %in% c("Lennon/McCartney", "McCartney", "Lennon", "Harrison", "Starr"))

average_energies <- Songwriters %>%
  group_by(`Main composer`) %>%
  summarise(mean_energy = mean(Energy))


#Title and Subtitle Text
Title_Songwriter <- "The Composers: Songwriting Energy of The Beatles"
Subtitle_Songwriter <- "Energy varies depending on the individual composer of a song. Though Lennon/McCartney songs showed the highest 
level of energy on average, John Lennon actually surpassed Paul McCartney on an individual level. Paul McCartney 
actually had the lowest average energy of any Beatle, though he also showed the most range."
Caption <- "Data from Spotify. Chart by Sean Gardner (seanmgard.github.io)"

Songwriter_Chart <- Songwriters |> 
  ggplot(aes(x = factor(`Main composer`, level = c("Lennon", "McCartney", "Lennon/McCartney", "Harrison", "Starr")), y = Energy)) +
  geom_beeswarm(aes(color = `Main composer`), cex = 2, size =1.5) +
  geom_image(data = Songwriters, mapping = aes(x=`Main composer`, y = 0.01, image=URL), size = 0.08) +
  labs(y = "Energy Score", title = Title_Songwriter, subtitle = Subtitle_Songwriter, caption = Caption) +
  #theme_void() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position = 'none',
    axis.title = element_blank(),
    axis.text = element_text(size = 20, colour = "white", family = "lato", hjust = 0),
    axis.text.x = element_blank(),
    text = element_text(size = 24, lineheight = 0.3, colour = "white", family = "lato"),
    plot.background = element_rect(fill = '#1E212B'),
    panel.background = element_blank(), 
    panel.grid=element_blank(),
    plot.title = element_text(color="white", face="bold", family = "lato", size=48, margin=margin(t=10)),
    plot.subtitle = element_text(color= "white", size=24, family = "lato", margin=margin(t = 5, b = 20)),
    plot.caption = element_markdown(colour = "white", hjust = 0, family = "lato", margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 5, r = 20, l = 20))

Songwriter_Chart

ggsave(filename="Projects/Beatles Emotional Score/Songwriter_Chart.png", height=6, width=6,   bg = "white")

# Correlations ------------------------------------------------------------

Correlation_Data <- Beatles_Raw |> 
  filter(URL != 0) |> 
  select(name, danceability, energy, instrumentalness, loudness, speechiness, tempo, valence) |> 
  pivot_longer(cols = -c(name, energy), names_to = "variable", values_to = "value")

#Title and Subtitle Text
Title_Correlation <- "Correlation Between Spotify Audio Features for Beatles Songs"
Subtitle_Correlation <- "Spotify analyzes the audio of every track on the platform to identify song attributes. The list below highlights the correlation between a 
song's 'energy' and the other features tracked for every song in the Beatles discography."
Caption <- "Data from Spotify. Chart by Sean Gardner (seanmgard.github.io)"

Correlation_Chart <- Correlation_Data |>
  ggplot(aes(x = energy, y = value, color = variable)) +
  geom_point(size = 0.2) +
  geom_smooth(method = lm, se = FALSE, aes(group = variable), size = 0.2) +
  facet_wrap(~ variable, scales = 'free_y') +
  geom_text(data = r_squared, aes(label = sprintf("R² = %.2f", r.squared), x = Inf, y = Inf, group = variable), 
            position = position_nudge(x = -0.5, y = -0.5), hjust = 1, vjust = 1, color = "white", size = 4) +
  labs(y = "Energy Score", title = Title_Correlation, subtitle = Subtitle_Correlation, caption = Caption) +
  theme_void() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position = 'none',
    axis.title = element_blank(),
    axis.text = element_blank(),
    text = element_text(size = 18, lineheight = 0.3, colour = "white", family = "lato"),
    plot.background = element_rect(fill = '#1E212B'),
    panel.background = element_blank(), 
    panel.grid=element_blank(),
    plot.title = element_text(color="white", face="bold", family = "lato", size=36, margin=margin(t=10)),
    plot.subtitle = element_text(color= "white", size=18, family = "lato", margin=margin(t = 5, b = 20)),
    plot.caption = element_markdown(colour = "white", hjust = 0, family = "lato", margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 5, r = 20, l = 20))

Correlation_Chart

ggsave(filename="Projects/Beatles Emotional Score/Correlation_Chart.png", height=4, width=5.5,   bg = "white")
