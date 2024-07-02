# Instalar y cargar igraph si no está instalado
if(!require(igraph)) install.packages("igraph", dependencies=TRUE)
library(igraph)

# Instalar y cargar leaflet si no está instalado
if(!require(leaflet)) install.packages("leaflet", dependencies=TRUE)
library(leaflet)


# Definir las ciudades y sus coordenadas
ciudades <- data.frame(
  Ciudad = c("Anchorage", "Cordova", "Juneau", "Yakutat", "Ketchikan", "Sitka", "Petersburg", "Wrangell"),
  Latitud = c(61.2181, 60.5428, 58.3019, 59.5489, 55.3418, 57.0531, 56.8125, 56.4708),
  Longitud = c(-149.9003, -145.7574, -134.4197, -139.7278, -131.6461, -135.3300, -132.9556, -132.3767)
)

# Crear el mapa interactivo
map <- leaflet(ciudades) %>%
  addTiles() %>%
  setView(lng = -135, lat = 57, zoom = 4) %>%
  addMarkers(
    lat = ~Latitud,
    lng = ~Longitud,
    label = ~Ciudad
  )

# Mostrar el mapa
map

#------------------------------------------------------------------

# Función para calcular la distancia de Haversine
d_haversine <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Radio de la Tierra en kilómetros
  delta_lat <- (lat2 - lat1) * pi / 180
  delta_lon <- (lon2 - lon1) * pi / 180
  a <- sin(delta_lat / 2) * sin(delta_lat / 2) +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) *
    sin(delta_lon / 2) * sin(delta_lon / 2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  dist <- R * c
  return(dist)
}

# Calcular las distancias entre todas las ubicaciones
distancias <- matrix(NA, nrow = nrow(ciudades), ncol = nrow(ciudades))
rownames(distancias) <- ciudades$Ciudad
colnames(distancias) <- ciudades$Ciudad

for (i in 1:nrow(ciudades)) {
  for (j in 1:nrow(ciudades)) {
    distancias[i, j] <- d_haversine(ciudades$Latitud[i], ciudades$Longitud[i], ciudades$Latitud[j], ciudades$Longitud[j])
  }
}

# Ver la matriz de distancias
print(distancias)


# Crear el grafo ponderado utilizando las distancias como pesos de las aristas
grafo <- graph.adjacency(as.matrix(distancias), weighted = TRUE, mode = "undirected")

# Configuración de los nombres de los vértices
V(grafo)$name <- rownames(distancias)

# Obtener los pesos de las aristas
pesos <- E(grafo)$weight


# Normalizar los pesos para el grosor de las aristas
min_peso <- min(pesos)
max_peso <- max(pesos)

# Escalar los pesos a un rango adecuado (por ejemplo, entre 1 y 10)
pesos_normalizados <- 0.5 + 5 * (pesos - min_peso) / (max_peso - min_peso)

# Añadir etiquetas de distancias a las aristas
E(grafo)$label <- round(E(grafo)$weight, 0)

# Dibujar el grafo con etiquetas de distancias y grosores normalizados
plot(grafo, vertex.label = V(grafo)$name, edge.label = E(grafo)$label, edge.width = pesos_normalizados, main = "Ciudades en Alaska")


# Encontrar el MST utilizando el algoritmo de Prim
Mst_prim <- minimum.spanning.tree(grafo, algorithm = "prim")

plot(Mst_prim, layout=layout.auto)



# Calcular la suma de los pesos de las aristas del MST
suma_menor_recorrido <- sum(E(Mst_prim)$weight)

ciudades_recorridas <- V(Mst_prim)$name
aristas_mst <- get.edges(Mst_prim, E(Mst_prim))


# Imprimir la suma del menor recorrido y las ciudades recorridas
cat("El camino con la menor distancia es:", suma_menor_recorrido, "km","\n")
cat("Las ciudades que recorre en orden son:", paste(ciudades_recorridas, collapse = " -> "), "\n")
