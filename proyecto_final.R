# Visualización de Datos II - Proyecto Final en R

# Dataset seleccionado:
# Estadísticas de ProUsuario, 2020 - 2026

# Este conjunto de datos contiene las estadísticas de la cantidad de usuarios atendidos 
# y reclamaciones gestionadas por la Oficina de Servicios y Protección al Usuario (ProUsuario)
# de la Superintendencia de Bancos (SB), generadas en el período 2019 - 2026, en el cual se puede
# encontrar la cantidad de usuarios atendidos, así como las reclamaciones atendidas
# en el período 2020-2026, clasificadas por tipo de decisión y montos instruidos a devolver.

# Requisitos:
# 10 visualizaciones utilizando ggplot. 
# Enfasis en la representacion geometrica y las etiquetas.


# 1- Instalar librerías
install.packages("tidyverse")

# 2- Cargar librerías
library(tidyverse)

# 3- Carga del Dataset
dataProusuario <- read.csv("C:/Users/vanes/Downloads/Visualización de Datos II/Visualización de Datos II/Tareas/Entrega final en R/estadísticas-de-reclamaciones-atendidas-por-prousuario-2020-2026.csv")

# 4- Exploración del dataset
head(dataProusuario)
str(dataProusuario)
colnames(dataProusuario)

# Resumen estadístico
summary(dataProusuario)

# 5- Limpieza del Dataset

# Se renombran las columnas para facilitar el manejo de los datos
dataProusuario <- dataProusuario %>%
  rename(
    anio = Año,
    mes = Mes,
    casos = Casos.recibidos,
    casos_hombres = Casos.recibidos.Hombres,
    casos_mujeres = Casos.recibidos.Mujeres,
    reclamaciones = Reclamaciones,
    completadas = Completadas,
    pendientes = Pendientes,
    favorable = Favorable,
    desfavorable = Desfavorable,
    monto = Monto.instruido.a.devolver.a.favor.del.Usuario,
    monto_hombres = Monto.instruido.a.devolver.a.favor.de.Hombres,
    monto_mujeres = Monto.instruido.a.devolver.a.favor.de.Mujeres
  )

# Convertir a numérico
dataProusuario$reclamaciones <- as.numeric(dataProusuario$reclamaciones)
dataProusuario$casos <- as.numeric(dataProusuario$casos)
dataProusuario$monto <- as.numeric(dataProusuario$monto)

# Revisar datos faltantes Na
colSums(is.na(dataProusuario))

# Convertir fechas
dataProusuario$mes_num <- match(dataProusuario$mes,
                                c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic"))
dataProusuario$fecha <- as.Date(paste(dataProusuario$anio, dataProusuario$mes_num, "01", sep = "-"))

# Ordenar por fecha
dataProusuario <- dataProusuario %>%
  arrange(fecha)

# Verificar dataset final
str(dataProusuario)
summary(dataProusuario)

# Renombramos más columnas
dataProusuario <- dataProusuario %>%
  rename(
    tiempo_resp_recl = Tiempo.de.respuesta..días....Reclamaciones,
    tiempo_resp_recon = Tiempo.de.respuesta..días....Reconsideraciones,
    pct_favorable = X..Favorable,
    pct_desfavorable = X..Desfavorable,
    reclamaciones_acreditadas = Reclamaciones.que.implicaron.acreditación,
    acredit_hombres = Acreditación...Hombres,
    acredit_mujeres = Acreditación...Mujeres
  )

# Convertir mes a factor
dataProusuario$mes <- factor(dataProusuario$mes,
                             levels = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic"))

# Crear variables extra
dataProusuario <- dataProusuario %>%
  mutate(
    ratio_favorable = ifelse(reclamaciones > 0, favorable / reclamaciones, 0),
    total_casos = casos_hombres + casos_mujeres,
    mes_label = paste(mes, anio)
  )
dataProusuario$ratio_favorable <- pmin(dataProusuario$ratio_favorable, 1)

# Redondear porcentajes
dataProusuario <- dataProusuario %>%
  mutate(
    pct_favorable = round(pct_favorable * 100, 1),
    pct_desfavorable = round(pct_desfavorable * 100, 1),
    ratio_favorable = round(ratio_favorable * 100, 1)
  )
dataProusuario$ratio_favorable <- round(dataProusuario$ratio_favorable, 1)

# Verificar dataset final nuevamente
str(dataProusuario)
summary(dataProusuario)

# 4- Visualización de Datos (ggplot2)

# En esta sección se presentan diferentes visualizaciones utilizando ggplot2,
# aplicando la gramática de gráficos: datos, mapeo de variables (aesthetics)
# y representaciones geométricas (geoms) vistas en el video ejemplo.

# 4.1- Evolución de casos
# Se analiza cómo han evolucionado los casos recibidos a lo largo del tiempo

ggplot(dataProusuario, aes(x = fecha, y = casos)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(
    title = "Evolución de Casos Recibidos",
    x = "Fecha",
    y = "Cantidad de casos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.2- Evolución de reclamaciones
# Se analiza la evolución de las reclamaciones en el tiempo

ggplot(dataProusuario, aes(x = fecha, y = reclamaciones)) +
  geom_line(color = "red") +
  geom_point() +
  labs(
    title = "Evolución de Reclamaciones",
    x = "Fecha",
    y = "Cantidad de reclamaciones"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.3- Relación entre casos y reclamaciones
# Se analiza la relación entre los casos recibidos y las reclamaciones

ggplot(dataProusuario, aes(x = casos, y = reclamaciones)) +
  geom_point(color = "purple") +
  labs(
    title = "Relación entre Casos y Reclamaciones",
    x = "Casos recibidos",
    y = "Reclamaciones"
  ) +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")

# 4.4- Casos por sexo
# Comparación de casos recibidos entre hombres y mujeres

ggplot(dataProusuario, aes(x = fecha)) +
  geom_line(aes(y = casos_hombres, color = "Hombres"), linewidth = 1) +
  geom_line(aes(y = casos_mujeres, color = "Mujeres"), linewidth = 1) +
  labs(
    title = "Casos por Sexo",
    x = "Fecha",
    y = "Cantidad",
    color = "Sexo"
  ) +
  scale_color_manual(values = c(
    "Hombres" = "blue",
    "Mujeres" = "deeppink"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.5- Decisiones Favorables vs desfavorables
# Comparación de decisiones tomadas

ggplot(dataProusuario, aes(x = fecha)) +
  geom_line(aes(y = favorable, color = "Favorable")) +
  geom_line(aes(y = desfavorable, color = "Desfavorable")) +
  labs(
    title = "Decisiones Favorables vs Desfavorables",
    x = "Fecha",
    y = "Cantidad",
    color = "Tipo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.6- Montos devueltos a usuarios
# Evolución de los montos en millones de pesos dominicanos (RD$)

ggplot(dataProusuario, aes(x = fecha, y = monto/1000000)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Montos devueltos a usuarios",
    x = "Fecha",
    y = "Monto (Millones RD$)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.7- Distribución de montos (Histograma)
# Histograma de montos en millones de pesos dominicanos (RD$)

ggplot(dataProusuario, aes(x = monto/1000000)) +
  geom_histogram(
    fill = "steelblue",
    color = "black",
    bins = 15
  ) +
  labs(
    title = "Distribución de montos",
    x = "Monto (Millones RD$)",
    y = "Frecuencia"
  ) +
  theme_minimal()

# 4.8- Boxplot de montos
# Dispersión de montos en millones de pesos dominicanos (RD$)

ggplot(dataProusuario, aes(y = monto/1000000)) +
  geom_boxplot(
    fill = "orange",
    color = "black"
  ) +
  labs(
    title = "Distribución de montos (Boxplot)",
    y = "Monto (Millones RD$)"
  ) +
  theme_minimal()

# 4.9- Tiempo de respuesta
# Análisis del tiempo de respuesta en reclamaciones

ggplot(dataProusuario, aes(x = fecha, y = tiempo_resp_recl)) +
  geom_line(color = "brown") +
  labs(
    title = "Tiempo de respuesta de reclamaciones",
    x = "Fecha",
    y = "Días"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.10- Porcentaje de decisiones favorables
# Análisis del porcentaje de decisiones favorables

ggplot(dataProusuario, aes(x = fecha, y = ratio_favorable)) +
  geom_line(color = "darkblue") +
  labs(
    title = "Porcentaje de decisiones favorables",
    x = "Fecha",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Conclusiones

# A partir del análisis exploratorio realizado sobre los datos de ProUsuario
# de la Superintendencia de Bancos de la República Dominicana, fue posible
# identificar tendencias importantes relacionadas con los casos recibidos,
# reclamaciones, decisiones emitidas y montos devueltos a los usuarios.

# Las visualizaciones desarrolladas permitieron interpretar el comportamiento
# de los datos de manera más clara y comprender mejor la evolución de los
# servicios financieros durante el período analizado.

# A continuación, se presentan las conclusiones obtenidas a partir
# de cada una de las visualizaciones realizadas en el análisis.

# 1. Evolución de Casos Recibidos
# Se observa una tendencia creciente en la cantidad de casos recibidos entre 2020 y 2026,
# lo que evidencia un aumento progresivo en el uso de los servicios de ProUsuario.

# 2. Evolución de Reclamaciones
# Las reclamaciones presentan un comportamiento ascendente a lo largo del tiempo,
# indicando una mayor participación de los usuarios en procesos de reclamación financiera.

# 3. Relación entre Casos y Reclamaciones
# Existe una fuerte relación positiva entre los casos recibidos y las reclamaciones,
# lo que sugiere que ambas variables crecen de manera proporcional.

# 4. Casos por Sexo
# Los hombres registran una mayor cantidad de casos en la mayoría de los períodos,
# aunque las mujeres también muestran un crecimiento sostenido durante los años analizados.

# 5. Decisiones Favorables vs Desfavorables
# Las decisiones favorables predominan sobre las desfavorables en la mayor parte del período,
# reflejando una tendencia positiva en la resolución de reclamaciones.

# 6. Montos Devueltos a Usuarios
# Los montos devueltos muestran variaciones importantes entre períodos,
# destacándose algunos meses con valores extraordinariamente altos.

# 7. Distribución de Montos (Histograma)
# La mayor concentración de montos se encuentra entre 5 y 15 millones de pesos,
# aunque existen valores extremos que incrementan la dispersión de los datos.

# 8. Distribución de Montos (Boxplot)
# El boxplot evidencia la presencia de valores atípicos en ciertos períodos,
# indicando meses con devoluciones significativamente superiores al promedio.

# 9. Tiempo de Respuesta de Reclamaciones
# El tiempo de respuesta presentó una reducción importante después de 2021,
# aunque posteriormente se observan aumentos moderados en algunos períodos.

# 10. Porcentaje de Decisiones Favorables
# El porcentaje de decisiones favorables fluctúa durante el período analizado,
# manteniéndose en niveles relativamente altos en gran parte de los años estudiados.


# En conclusión, el uso de ggplot2 permitió representar visualmente la información
# de forma clara y comprensible, facilitando la identificación de patrones,
# tendencias y comportamientos relevantes dentro del conjunto de datos.

# Este análisis demuestra la importancia de la visualización de datos en la
# interpretación de información financiera y en el apoyo a la toma de decisiones.