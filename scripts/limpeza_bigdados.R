
## Pacotes
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)

# 2. Criar objeto com apenas as coordenadas (e a coluna species, por exemplo)
dados_filtrados <- dados_filtrados %>%
  select(species, lon, lat) %>%       # Seleciona colunas de interesse
  distinct()                          # Remove duplicatas, se houver

# 3. Converter para objeto espacial sf (sistema WGS84)
dados_sf <- st_as_sf(dados_limpos, coords = c("lon", "lat"), crs = 4326)
names(dados_sf)

# Carregar o mapa do mundo como sf
mapa_mundial <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#Colocar o nome dos continentes no mapa
continentes <- ne_countries(scale = "medium", returnclass = "sf") |>
  group_by(continent) |>
  summarise(geometry = st_union(geometry)) |>
  st_centroid()
# Extrair coordenadas dos centroides para colunas separadas
coords <- st_coordinates(continentes)
continentes$lon <- coords[, 1]
continentes$lat <- coords[, 2]

# 5. Plotar os pontos espaciais sobre o mapa
ggplot() +
  geom_sf(data = mapa_mundial, fill = "gray90", color = "white") +
  geom_sf(data = dados_sf, aes(color = species), size = 1, alpha = 0.7,show.legend = FALSE) +
 # geom_text(data = continentes,
 #          aes(x = st_coordinates(geometry)[,1],
 #             y = st_coordinates(geometry)[,2],
 #            label = continent),
 #       size = 2, fontface = "bold", color = "gray20")+
  labs(title = "Ocorrência das espécies de Trechaleidae",
       color = "Espécies") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.box = "horizontal")

#Outra opção de mapa
ggplot() +
  geom_sf(data = mapa_mundial, fill = "gray90", color = "white") +
  geom_sf(data = dados_sf, aes(color = species), 
          alpha = 0.8, size = 1, show.legend = FALSE) +
  labs(
    title = "Distribuição atual das espécies de Trechaleidae",
    x = "Longitude", 
    y = "Latitude",
    caption = "Fonte: Elaborado pelo autor."
  ) +
  scale_color_viridis_d(option = "turbo") +
  coord_sf(label_graticule = waiver(), label_axes = waiver()) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = "grey", color = NA),
    legend.text = element_text(color = "white", size = 10),
    legend.title = element_text(color = "white", size = 12),
    legend.key = element_rect(fill = "grey40", color = NA),
    panel.grid = element_line(color = "grey")
  )


# Mapas após limpeza:
library(maps)

# Criar o mapa
mapa_mundial2 <- map_data("world") # Substitua por "usa" ou "brasil" dependendo do seu país

# Mapa com pontos válidos

map_trechaleidae4 <- ggplot() +
  geom_polygon(data = mapa_mundial2, aes(x = long, y = lat, group = group), fill = "lightgrey") +  # Ajuste para o fundo
  geom_sf(data = paises, fill = NA, color = "white") +  # Bordas dos países em branco
  geom_point(data = cleaned_data_final, aes(x = lon, y = lat, color = species), 
             alpha = 0.3, size = 1, show.legend = F) +  # Manter legendas visíveis
  labs(
    title = "Distribuição atual das espécies de Trechaleidae",
    x = "Longitude", 
    y = "Latitude",
    caption = "Fonte: Elaborado pelo autor."
  ) +
  scale_color_viridis_d(option = "turbo") +  # Paleta vibrante
  coord_sf(expand = F)+
  #theme_minimal(base_size = 14) +  # Estética dark, com fontes maiores
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Ajuste do tamanho na legenda
  theme(
    legend.position = "right",  # Posição da legenda
    legend.background = element_rect(fill = "white", color = NA),  # Fundo escuro sem borda
    legend.text = element_text(color = "black", size = 10),  # Texto em branco
    legend.title = element_text(color = "black", size = 12),  # Título da legenda
    legend.key = element_rect(fill = "grey40", color = NA),  # Simplicidade na legenda
    panel.grid = element_line(color = "grey")  # Grade do painel discreta
  )
#Salvar 
# Opção 1:
ggsave(
  filename = "map_trechaleidae3.png",
  plot = map_trechaleidae3,
  device = "png",
  scale = 1,
  bg = "white"
)
#Opção 2:
ggsave(
  filename = "map_trechaleidae4.png",
  plot = map_trechaleidae4,
  device = "png",
  scale = 1,
  bg = "white"
)

## Criar limites da América do Sul 

# Defina os limites de longitude e latitude (PARA CORTAR O MAPA)
limites_lon <- c(-140, -30)  # Defina os limites de longitude (exemplo)
limites_lat <- c(-60, 60)     # Defina os limites de latitude (exemplo)

# limite 2
limites_lon2 <- c(-130, -37)  # Defina os limites de longitude (exemplo)
limites_lat2 <- c(-50, 50)     # Defina os limites de latitude (exemplo)
# Carregar pacotes necessários
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
# Carregar o mapa dos países em formato sf
paises <- ne_countries(scale = "medium", returnclass = "sf")

# Calcular os centróides e extrair coordenadas
paises_centroides <- paises %>%
  st_centroid() %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  ) %>%
  select(country = name, longitude, latitude)

# Mapa da distribuição mostrando só a América do Sul 7

mapa_test<- ggplot() +
  geom_polygon(data = mapa_mundial2, aes(x = long, y = lat, group = group), fill = "lightgrey") +  # Ajuste para o fundo
  geom_sf(data = paises, fill = NA, color = "white") +  # Bordas dos países em branco
  geom_point(data = cleaned_data_final, aes(x = lon, y = lat, color = species), 
             alpha = 0.3, size = 1, show.legend = F) +  # Manter legendas visíveis
  geom_text(data = paises_centroides, 
            aes(x = longitude, y = latitude, label = country),
            size = 2, color = "gray20", check_overlap = TRUE) + ## Adiciona o nome dos países
  labs(
    title = "Distribuição atual das espécies de Trechaleidae",
    x = "Longitude", 
    y = "Latitude",
    caption = "Fonte: Elaborado pelo autor."
  ) +
  scale_color_viridis_d(option = "turbo") +  # Paleta vibrante
  coord_sf(expand = F)+
  #theme_minimal(base_size = 14) +  # Estética dark, com fontes maiores
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Ajuste do tamanho na legenda
  coord_sf(xlim = limites_lon2, ylim = limites_lat2) +
  theme(
    legend.position = "right",  # Posição da legenda
    legend.background = element_rect(fill = "white", color = NA),  # Fundo escuro sem borda
    legend.text = element_text(color = "black", size = 8),  # Texto em branco
    legend.title = element_text(color = "black", size = 12),  # Título da legenda
    legend.key = element_rect(fill = "grey40", color = NA),  # Simplicidade na legenda
    panel.grid = element_line(color = "grey")  # Grade do painel discreta
  )
#Salvar 
# Opção 1:
ggsave(
  filename = "mapa_test.png",
  plot = mapa_test,
  device = "png",
  scale = 1,
  bg = "white"
)
#Opção 2:
ggsave(
  filename = "map_trechaleidae6.png",
  plot = map_trechaleidae_Amer2,
  device = "png",
  scale = 1,
  bg = "white"
)

# Mapa com os pontos no oceano 
ggplot() +
  geom_polygon(data = mapa_mundial, aes(x = long, y = lat, group = group), fill = "lightgrey") +  # Ajuste para o fundo
  geom_sf(data = paises, fill = NA, color = "#ffffff") +  # Bordas dos países em branco
  #geom_point(data = data_excluded_ocean, aes(x = lon, y = lat, color = species), 
           #  alpha = 0.8, size = 1, shape = 23, show.legend = F) +  # Manter legendas visíveis
  #geom_text(data = data_excluded_ocean, aes(x = lon, y = lat, label = "🕷️"), size =3.5) +
  geom_text(data = data_excluded_ocean, aes(x = lon, y = lat, label = "🕷️"), size =2.5) +
  labs(
    title = "Dados excluidos pelo pacote CoordinateCleaner",
    x = "Longitude", 
    y = "Latitude",
    caption = "Fonte: Elaborado pelo autor."
  ) +
  scale_color_viridis_d(option = "turbo") +  # Paleta vibrante
  coord_sf(expand = FALSE)+
  #theme_minimal(base_size = 14) +  # Estética dark, com fontes maiores
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Ajuste do tamanho na legenda
  theme(
    legend.position = "right",  # Posição da legenda
    legend.background = element_rect(fill = "grey", color = NA),  # Fundo escuro sem borda
    legend.text = element_text(color = "white", size = 10),  # Texto em branco
    legend.title = element_text(color = "white", size = 12),  # Título da legenda
    legend.key = element_rect(fill = "grey40", color = NA),  # Simplicidade na legenda
    panel.grid = element_line(color = "grey")  # Grade do painel discreta
  )
# Opção 1:
ggsave(
  filename = "data_excluded_ocean2.png",
  plot = points_ocean,
  device = "png",
  scale = 1,
  bg = "white"
)
#Opção 2:
ggsave(
  filename = "data_excluded_ocean3.png",
  plot = points_ocean_size2_5,
  device = "png",
  scale = 1,
  bg = "white"
)


