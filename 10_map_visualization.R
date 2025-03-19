## map 

# Load U.S. states shapefile
states <- tigris::states(cb = TRUE, resolution = "20m", class = "sf")

# Filter for the 50 states (exclude territories)
states <- states[!states$STUSPS %in% c("HI", "AK", "GU", "PR", "VI"), ]
states$state 

# merge 
states <- merge(states, full_indicators, by.x = "STUSPS", by.y = "State")


#### Trying to bucket them to normalize the distribution --- 
states <- states %>%
  mutate(
    Imm_Con_Ind = (Imm_Class_Concrete_2016 - min(Imm_Class_Concrete_2016)) / (max(Imm_Class_Concrete_2016) - min(Imm_Class_Concrete_2016)),
    Imm_Full_Ind = (Imm_Class_Full_2016 - min(Imm_Class_Full_2016)) / (max(Imm_Class_Full_2016) - min(Imm_Class_Full_2016)),
    Imm_Con_Index = case_when(
      Imm_Class_Concrete_2016 <= -5  ~ -1,   # High Anti
      Imm_Class_Concrete_2016 < 0    ~ -0.5, # Low Anti
      Imm_Class_Concrete_2016 < 10   ~ 0.5,  # Low Pro
      Imm_Class_Concrete_2016 >= 10   ~ 1,    # High Pro
    ),
    Imm_Sym_Index = case_when(
      Imm_Class_Full_2016 <= -10 ~ -1,     # High Anti: score <= -10
      Imm_Class_Full_2016 > -10 & Imm_Class_Full_2016 <= 0 ~ -0.5,  # Low Anti: score between -10 and 0
      Imm_Class_Full_2016 > 0 & Imm_Class_Full_2016 < 20 ~ 0.5,   # Low Pro: score between 0 and 20
      Imm_Class_Full_2016 >= 20 ~ 1,        # High Pro: score > 20
    )
    )
  


map_2016 <- ggplot(data = states) +
  geom_sf(aes(fill = Imm_Con_Index)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State, 2012-2016",
    caption = "Data Source: Original Data"
  ) +
  theme(
    axis.text = element_blank(),  # Removes lat/long labels
    axis.ticks = element_blank(), # Removes axis ticks
    panel.grid = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "panel"
  )

map_2016_alt <- ggplot(data = states) +
  geom_sf(aes(fill = Imm_Sym_Index)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State, 2012-2016, with Symbolic Legislation",
    caption = "Data Source: Original Data"
  ) +
  theme(
    axis.text = element_blank(),  # Removes lat/long labels
    axis.ticks = element_blank(), # Removes axis ticks
    panel.grid = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "panel"
  )

ggsave(
  filename = "state_climate_2016_con.png",  # File name with extension
  plot = map_2016,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

ggsave(
  filename = "state_climate_2016_sym.png",  # File name with extension
  plot = map_2016_alt,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)
