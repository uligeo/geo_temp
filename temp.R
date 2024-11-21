library(terra) # Carga la biblioteca para manejo de datos ráster
library(leaflet) # Carga la biblioteca para visualización de mapas interactivos
library(leaflet.extras) # Complementos para mapas de Leaflet
library(htmltools) # Herramientas para manipulación de contenido HTML
library(RColorBrewer) # Biblioteca para paletas de colores

# Cargar los rásteres
tempr2023 <- rast('LST_Landsat-2.tif') # Temperatura de 2023
tempr2014 <- rast('LST_Landsat2014.tif') # Temperatura de 2014

# Reescalar (agregar) ambos rásteres para mejor rendimiento
tempr2023_resampled <- aggregate(tempr2023, fact = 2) # Reduce la resolución del ráster 2023
tempr2014_resampled <- aggregate(tempr2014, fact = 2) # Reduce la resolución del ráster 2014

# Definir el rango global para la paleta de colores (usando valores mínimo y máximo comunes)
global_min <- 25  # Establecer el valor mínimo de temperatura
global_max <- 40  # Establecer el valor máximo de temperatura

# Ajustar ambos rásteres al rango definido
tempr2023_resampled <- clamp(tempr2023_resampled, lower = global_min, upper = global_max, values = TRUE)
tempr2014_resampled <- clamp(tempr2014_resampled, lower = global_min, upper = global_max, values = TRUE)

# Calcular la diferencia de temperatura entre 2023 y 2014
temperature_difference <- tempr2023_resampled - tempr2014_resampled

# Definir una paleta invertida "Spectral" con tonos rojizos mejorados
original_spectral <- rev(brewer.pal(11, "Spectral")) # Paleta original
red_biased_spectral <- colorRampPalette(c(original_spectral[1], original_spectral[2], original_spectral[5], "red", "darkred")) # Ajuste

# Crear una función de mapeo de colores compartida para ambos rásteres
shared_colors <- colorNumeric(palette = red_biased_spectral(100), 
                               domain = c(global_min, global_max), 
                               na.color = "transparent")

# Paleta para la diferencia de temperatura
diff_palette <- colorNumeric(palette = c("blue", "white", "red"), 
                              domain = c(-10, 10), 
                              na.color = "transparent")

# Estadísticas resumidas
mean_2014 <- mean(values(tempr2014_resampled), na.rm = TRUE) # Media 2014
mean_2023 <- mean(values(tempr2023_resampled), na.rm = TRUE) # Media 2023
mean_diff <- mean(values(temperature_difference), na.rm = TRUE) # Media de diferencias

# Crear contenido personalizado para el popup
popup_content <- HTML(paste0(
  "<h4>Panel de Cambio de Temperatura</h4>",
  "<b>Temperatura media (2014): </b>", round(mean_2014, 2), "°C<br>",
  "<b>Temperatura media (2023): </b>", round(mean_2023, 2), "°C<br>",
  "<b>Diferencia media de temperatura: </b>", round(mean_diff, 2), "°C<br>",
  "<p>Este mapa compara los datos de temperatura de 2014 y 2023, destacando áreas con cambios significativos. Use el control de capas para alternar entre años o ver la capa de diferencia para los cambios.</p>"
))

# Crear el mapa interactivo con Leaflet
map <- leaflet() %>%
  addTiles() %>% # Agregar un mapa base
  # Agregar la capa ráster de 2023
  addRasterImage(tempr2023_resampled, 
                 colors = shared_colors, 
                 opacity = 0.8, 
                 group = "Temperatura 2023") %>%
  addLegend(pal = shared_colors, 
            values = c(global_min, global_max), 
            title = "Temperatura 2023 (°C)",
            group = "Temperatura 2023") %>%
  # Agregar la capa ráster de 2014
  addRasterImage(tempr2014_resampled, 
                 colors = shared_colors, 
                 opacity = 0.8, 
                 group = "Temperatura 2014") %>%
  addLegend(pal = shared_colors, 
            values = c(global_min, global_max), 
            title = "Temperatura 2014 (°C)",
            group = "Temperatura 2014") %>%
  # Agregar la capa de diferencia de temperatura
  addRasterImage(temperature_difference, 
                 colors = diff_palette, 
                 opacity = 0.8, 
                 group = "Diferencia de Temperatura") %>%
  addLegend(pal = diff_palette, 
            values = c(-10, 10), 
            title = "Diferencia de Temperatura (°C)",
            group = "Diferencia de Temperatura") %>%
  # Agregar el contenido del popup
  addPopups(lng = -89.5, lat = 21.0, popup = popup_content) %>%
  # Agregar un control para alternar capas
  addLayersControl(
    overlayGroups = c("Temperatura 2023", "Temperatura 2014", "Diferencia de Temperatura"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Activar opciones de búsqueda y zoom
  addSearchOSM() %>%
  addScaleBar(position = "bottomright") %>%
  addFullscreenControl()

map

library(htmlwidgets)
saveWidget(map, file = "mapa_interactivo.html")
