library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

# Leitura dos dados
dados <- read_excel("tabelas_dados/dados_brutos_semfiltros.xlsx")

# Remover registros sem coordenadas
dados_clean <- dados %>% filter(!is.na(lon), !is.na(lat))

# Converter para objeto espacial
dados_sf <- st_as_sf(dados_clean, coords = c("lon", "lat"), crs = 4326)

# Carregar mapa base
mapa_mundial <- ne_countries(scale = "medium", returnclass = "sf")

# Plotar
ggplot() +
  geom_sf(data = mapa_mundial, fill = "gray90", color = "white") +
  geom_sf(data = dados_sf, aes(color = species), size = 1, alpha = 0.6,show.legend = F) +
  scale_color_viridis_d(option = "turbo") +
  labs(
    title = "Distribuição geográfica das espécies de Trechaleidae",
    color = "Espécies",
    caption = "Fonte: Dados compilados pelo autor"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


## Gráfico de barras: número de ocorrências por espécies
dados_clean %>%
  count(species, sort = TRUE) %>%
  ggplot(aes(x = reorder(species, n), y = n)) +
  geom_bar(stat = "identity", fill = "#2c7fb8") +
  coord_flip() +
  labs(
    title = "Número de registros por espécie",
    x = "Espécie",
    y = "Número de registros"
  ) +
  theme_minimal(base_size = 12) +  # tamanho base da fonte
  theme(
    axis.text.y = element_text(size = 7),  # diminui tamanho dos nomes das espécies
    plot.title = element_text(size = 14, face = "bold")
  )

## Opção que mostra as 20 espécies com mais registros
dados_clean %>%
  count(species, sort = TRUE) %>%
  slice_max(n, n = 20) %>%  # mostra só as 20 com mais registros
  ggplot(aes(x = reorder(species, n), y = n)) +
  geom_bar(stat = "identity", fill = "#2c7fb8") +
  coord_flip() +
  labs(
    title = "Número de registros por espécie",
    x = "Espécie",
    y = "Número de registros"
  ) +
  theme_minimal(base_size = 12) +  # tamanho base da fonte
  theme(
    axis.text.y = element_text(size = 7),  # diminui tamanho dos nomes das espécies
    plot.title = element_text(size = 14, face = "bold")
  )

## por país
dados_clean %>%
  count(country, sort = TRUE) %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_bar(stat = "identity", fill = "#41b6c4") +
  coord_flip() +
  labs(
    title = "Número de registros por país",
    x = "País",
    y = "Número de registros"
  ) +
  theme_minimal()

## Gráfico de bsrras com espécies agrupadas por gênero
library(dplyr)
library(ggplot2)
library(forcats)

dados %>%
  mutate(genero = sub(" .*", "", species)) %>%  # Extrai o gênero
  count(genero, species, sort = TRUE) %>%
  ggplot(aes(x = fct_reorder(species, n), y = n, fill = genero)) +
  geom_col() +
  coord_flip() +
  labs(x = "Espécies", y = "Número de registros", fill = "Gênero",
       title = "Número de registros por espécie (agrupadas por gênero)") +
  theme_minimal(base_size = 10)

dados_classificados <- dados %>%
  count(species) %>%
  mutate(
    categoria_amostragem = case_when(
      n < 50 ~ "Pouco amostrada",
      n >= 50 & n < 100 ~ "Moderadamente amostrada",
      n >= 100 ~ "Bem amostrada"
    )
  )


library(tidyr)

dados %>%
  count(species, country) %>%
  left_join(dados_classificados, by = "species") %>%
  group_by(country, categoria_amostragem) %>%
  summarise(qtd_especies = n_distinct(species), .groups = "drop") %>%
  ggplot(aes(x = reorder(country, qtd_especies), y = qtd_especies, fill = categoria_amostragem)) +
  geom_col(position = "stack") +
  coord_flip() +
  labs(x = "País", y = "Número de espécies",
       title = "Classificação das espécies por categoria de amostragem e país",
       fill = "Categoria") +
  theme_minimal()


dados %>%
  count(species, country) %>%
  left_join(dados_classificados, by = "species") %>%
  group_by(country, categoria_amostragem) %>%
  summarise(qtd_especies = n_distinct(species), .groups = "drop") %>%
  ggplot(aes(x = reorder(country, qtd_especies), y = qtd_especies, fill = categoria_amostragem)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_manual(
    values = c("Pouco amostrada" = "red4", "Moderadamente amostrada" = "yellow4", "Bem amostrada" = "green4")
  ) +
  labs(x = "País", y = "Número de espécies",
       title = "Classificação das espécies por categoria de amostragem e país",
       fill = "Categoria de Amostragem") 

# Calcular total de espécies por país
totais_por_pais <- dados %>%
  count(species, country) %>%
  group_by(country) %>%
  summarise(total_especies = n_distinct(species), .groups = "drop")

# Gerar o gráfico com anotação
dados %>%
  count(species, country) %>%
  left_join(dados_classificados, by = "species") %>%
  group_by(country, categoria_amostragem) %>%
  summarise(qtd_especies = n_distinct(species), .groups = "drop") %>%
  left_join(totais_por_pais, by = "country") %>%
  ggplot(aes(x = reorder(country, total_especies), y = qtd_especies, fill = categoria_amostragem)) +
  geom_col(position = "stack") +
  geom_text(
    data = . %>%
      group_by(country) %>%
      summarise(total = sum(qtd_especies), .groups = "drop"),
    aes(x = country, y = total + 1, label = total),  # +1 para posicionar acima da barra
    inherit.aes = FALSE,
    size = 3.5
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("Pouco amostrada" = "red4", "Moderadamente amostrada" = "yellow3", "Bem amostrada" = "green3")
  ) +
  labs(x = "País", y = "Número de espécies",
       title = "Classificação das espécies por categoria de amostragem e país",
       fill = "Categoria de Amostragem") +
  theme_minimal()

# Gerar gráfico com rótulo dentro da barra
dados %>%
  count(species, country) %>%
  left_join(dados_classificados, by = "species") %>%
  group_by(country, categoria_amostragem) %>%
  summarise(qtd_especies = n_distinct(species), .groups = "drop") %>%
  left_join(totais_por_pais, by = "country") %>%
  ggplot(aes(x = reorder(country, total_especies), y = qtd_especies, fill = categoria_amostragem)) +
  geom_col(position = "stack") +
  geom_text(
    data = . %>%
      group_by(country) %>%
      summarise(total = sum(qtd_especies), .groups = "drop"),
    aes(x = country, y = total - 2, label = total),  # -2 para posicionar dentro da barra
    inherit.aes = FALSE,
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("Pouco amostrada" = "red4", "Moderadamente amostrada" = "yellow3", "Bem amostrada" = "green3")
  ) +
  labs(x = "País", y = "Número de espécies",
       title = "Classificação das espécies por categoria de amostragem e país",
       fill = "Categoria de Amostragem") +
  theme_minimal()



dados_agg <- dados %>%
  group_by(country) %>%
  summarise(n_species = n_distinct(species),
            n_genera = n_distinct(stringr::word(species, 1)))  # obtém o gênero pela primeira palavra

# Baixar shapefile de países (formato sf)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Unir dados ao mapa (garantir nomes compatíveis!)
world_dados <- left_join(world, dados_agg, by = c("name" = "country"))

ggplot(world_dados) +
  geom_sf(aes(fill = n_species)) +
  scale_fill_viridis_c(option = "plasma", na.value = "gray90", name = "Nº de espécies") +
  labs(title = "Riqueza de espécies da família Trechaleidae por país") +
  theme_minimal()

ggplot(world_dados) +
  geom_sf(aes(fill = n_genera)) +
  scale_fill_viridis_c(option = "plasma", na.value = "gray90", name = "Nº de espécies") +
  labs(title = "Riqueza de espécies da família Trechaleidae por país") +
  theme_minimal()

## Criar coluna com gênero na tabela
library(dplyr)
library(stringr)
library(ggplot2)
dados <- dados %>%
  mutate(genero = word(species, 1))


##Mapa com legenda
ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_sf(data = dados_sf, aes(color = genero), alpha = 0.7, size = 2) +
  scale_color_viridis_d(name = "Gênero") +
  labs(title = "Ocorrências por gênero da família Trechaleidae",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# Vetor com 16 cores distintas
cores_16 <- c(
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a",
  "#66a61e", "#e6ab02", "#a6761d", "#666666",
  "#1f78b4", "#b2df8a", "#fb9a99", "#fdbf6f",
  "#cab2d6", "#ffff99", "#a6cee3", "#ff7f00"
)

# Aplicar no gráfico
ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_sf(data = dados_sf, aes(color = genero), alpha = 0.7, size = 2) +
  scale_color_manual(values = cores_16, name = "Gênero") +
  labs(title = "Ocorrências por gênero da família Trechaleidae",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

library(Polychrome)
library(dplyr)

# Pegue os 16 gêneros únicos no seu dataset
generos_unicos <- unique(dados_sf$genero)

# Gere a paleta com o mesmo número de cores que de gêneros
cores_16 <- createPalette(length(generos_unicos), seedcolors = c("#000000", "#FFFFFF"))

# Atribua os nomes dos gêneros às cores
names(cores_16) <- generos_unicos
ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_sf(data = dados_sf, aes(color = genero), alpha = 0.7, size = 2) +
  scale_color_manual(values = cores_16, name = "Gênero") +
  labs(title = "Ocorrências por gênero da família Trechaleidae",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

