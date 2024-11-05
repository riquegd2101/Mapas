library(sf)
library(ggplot2)
library(readxl)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(mapview) # Para exportar como PNG

df <- read_xlsx("Nova pasta/202410_Acordo.xlsx", sheet = 2)
mg <- st_read("Nova pasta/MG_Municipios_2022.shp")
regi <- read_xlsx("Nova pasta/Municípios e Regiões 2.0 - Copia (2).xlsx")
regi <- regi %>% select(IBGE...1, microrregiao.nome, mesorregiao.nome, `VL COMPLETO`) %>% 
  rename(CD_MUN = IBGE...1)

df$CD_MUN <- as.character(df$CD_MUN)
df_mapa <- merge(mg, df, by = "CD_MUN", all.x = TRUE)
df_mapa <- merge(df_mapa, regi, by = "CD_MUN")
df_mapa <- df_mapa %>% select(-SIGLA_UF, -AREA_KM2, -Município)
df_mapa <- df_mapa %>% filter(!is.na(Atingido) | !is.na(`VL COMPLETO`) | CD_MUN == "3102852")
# Agrupando e unindo os municípios para criar contornos externos para as mesorregiões e microrregiões
mesorregioes <- df_mapa %>%
  group_by(mesorregiao.nome) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

microrregioes <- df_mapa %>%
  group_by(microrregiao.nome) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

##################################################################################################

  mapa_litio <- leaflet() %>%
    # Adicionando todos os municípios com contorno leve
    addPolygons(data = df_mapa, 
                color = "gray95", weight = 0.3, fillOpacity = 0.1, 
                label = "Minas Gerais") %>%
    
    # Adicionando municípios atingidos com cores diferentes e labels
    addPolygons(data = df_mapa,
                fillColor = case_when(
                  df_mapa$`VL COMPLETO` == "Vale do Lítio Ampliado" ~ "#3a6604",
                  df_mapa$`VL COMPLETO` == "Vale do Lítio Reduzido" ~ "#a4ea4f",
                  TRUE ~ "transparent"
                ),
                color = "black", weight = 0.1, fillOpacity = 0.6,
                label = ~paste("Município:", df_mapa$NM_MUN, 
                               " - Mesorregião:", df_mapa$mesorregiao.nome, 
                               " - Microrregião:", df_mapa$microrregiao.nome)) %>%
    
    # Adicionando municípios atingidos com cores diferentes e labels
    addPolygons(data = df_mapa,
                fillColor = case_when(
                  df_mapa$Atingido == "Diretamente atingidos" ~ "red",
                  df_mapa$Atingido == "Demais atingidos" ~ "darkred",
                  TRUE ~ "transparent"
                ),
                color = "black", weight = 0.1, fillOpacity = 0.6,
                label = ~paste("Município:", df_mapa$NM_MUN, 
                               " - Mesorregião:", df_mapa$mesorregiao.nome, 
                               " - Microrregião:", df_mapa$microrregiao.nome)) %>%
    
    # Adicionando contorno das microrregiões com linha pontilhada
    addPolylines(data = microrregioes, 
                 color = "black", weight = 1.5, fillOpacity = 0.4,
                 label = "Microrregiões") %>%
    
    # Adicionando contorno das mesorregiões com linha sólida mais espessa
    addPolylines(data = mesorregioes, 
                 color = "black", weight = 2.5, 
                 label = "Mesorregiões") %>%
    
    # Configurações do mapa
    addTiles() %>%
    addLegend("topright", 
              colors = c("#3a6604", "#a4ea4f", "red", "darkred"), 
              labels = c("Vale do Lítio Ampliado", "Vale do Lítio Reduzido", "Diretamente atingidos", "Demais atingidos"), 
              title = "Atingidos Bacia do Rio Doce",
              opacity = 1) %>%
    setView(lng = -43.9, lat = -19.9, zoom = 7)
  
  saveWidget(mapa_litio, "mapa_riodoce_litio.html", selfcontained = TRUE)
mapa_litio   
