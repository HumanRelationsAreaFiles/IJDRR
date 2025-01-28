library(dplyr)
library(tidyr)
library(igraph)
library(scales) 
library(viridis)

# source('hz.initialize.R')

# Minimum spanning trees --------------------------------------------------

## set-up ------------------------------------------------------------------
hz_type <-  d %>% select(c(1,9))

h <- hz_type %>%
  group_by(OWC, H.5.) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  pivot_wider(names_from = H.5., values_from = frequency, values_fill = 0)

h[, 2:29] <- lapply(h[, 2:29], function(x) ifelse(x > 0, 1, 0))

h <- as.numeric(h)

h <- h[,-c(1)]

### Rename hazard type columns-----------------------------------------------------------

h <- h %>%
  rename(
    Drought = Dr,
    Flood = Fl,
    "Livestock/agricultural pests and disease (PD)" = PD,
    "Windstorm" = Wi,
    "Great storm/extreme precipitation" = GsEp,
    "Hurricane" = Hu,
    Thunderstorm = Th,
    Fire = Fi,
    "Severe, early, or long winter" = SEW,
    Earthquake = Ea,
    Snowstorm = SS,
    "Erratic rain" = ER,
    Frost = Fr,
    "Sand/dust storm" = SDS,
    "Rough seas" = RS,
    Tornado = To,
    "Soil/land erosion" = SE,
    "Delayed or early rain" = DRn,
    "Tidal wave" = TW,
    Avalanche =Av,
    "Volcanic activity" = Vo,
    "Glacial advance" = GAC,
    "Saltwater intrusion" = SW,
    Doldrum = Do,
    Landslide = Ls,
    Hail = Ha,
    "Smoke/smog/inversion" = Sm,
    "Extreme temperatures" = ET
  )

### graph objects --------------------------------------------------------------------

thz <- as.data.frame(h[sapply(h, is.numeric)])
# row.names(dcow) <- cow$textid
thz <- thz[rowSums(thz)>0,]
#dcow <- dcow[,which(colSums(dcow) != 0)]
thz <- thz[,which(colSums(thz) > 0 )]



mc <- as.matrix(dist(t(thz), method='binary'))
gmc <- igraph::graph_from_adjacency_matrix(mc, mode = 'undirected', weighted = T, diag = F)
gmc <- igraph::mst(gmc, algorithm = 'prim')
V(gmc)$count <- colSums(thz)
V(gmc)$specific <- names(V(gmc)) 

count_percent <- colSums(thz) / nrow(thz) * 100
count_percent_hz <- as.data.frame(count_percent)


##trees-----------------------------------------------------------------
tree_hz_1 <- 
  ggraph(gmc, 'stress') + 
  geom_node_point(aes(color = count, size = count), alpha = 0.9) +  
  geom_edge_link(alpha = 0.5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +
  theme_graph(base_size = 17) +
  scale_color_viridis_c(option = "D", limits = c(1, 60), direction = -1) +  
  scale_size_continuous(range = c(2, 10), guide = "none") +  
  guides(color = guide_colorbar(barwidth = 10, barheight = 1, title = "Societies with hazard type")) +  # Color legend as a gradient bar
  theme(legend.position = "bottom")  


# adjustment of palette for readability
faded_viridis <- alpha(viridis(256, option = "D", direction = -1), 0.8)

tree_hz_2 <- 
  ggraph(gmc, 'stress') + 
  geom_node_point(aes(color = count, size = count), alpha = 0.8) +  
  geom_edge_link(alpha = 0.5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +
  theme_graph(base_size = 17) +
  scale_color_gradientn(colors = faded_viridis, limits = c(1, 60), name = "Societies with hazard type") +  # Apply the faded and reversed color scale
  scale_size_continuous(range = c(2, 10), guide = "none") +  
  guides(color = guide_colorbar(barwidth = 10, barheight = 1)) +  
  theme(legend.position = "bottom")  


