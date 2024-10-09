

# Leer Datos completos

sizeall2 <- readRDS("tallas_13_24.RDS")


# Prepara data 

# filtro lo requerido por MD: Poblacional y Abril y Mayo
tallapobmar <- sizeall2 %>% 
  filter(rastro=="POBLACIONAL",
         MES %in% c("April","May"))

# Categorizar los datos
tallapobmar$CAT <- as.numeric(as.character(cut(
  x = tallapobmar$sizeE,
  breaks = seq(0, 50, 1),
  labels = seq(0, 49, 1),
  right = FALSE
)))


# Calcular la proporción de cada bin por año
tallaprop <- tallapobmar %>%
  group_by(ANO, MES, CAT) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  arrange(ANO, MES,  CAT)


 # Grafico simple

ggplot(tallaprop,
       aes(x = CAT, 
           y = proportion, 
           fill = as.factor(ANO))) +
  geom_col() +
  facet_grid(ANO ~ MES) +
  labs(x = "CATL", y = "Proporción (%)", color = "Año") +
  theme_few() +
  geom_vline(xintercept = 10.8, col="red")+
  scale_fill_viridis_d(option = "H",
                       name="AÑO")+
  ggtitle("Proporción por Mes y Año")

# guardo data

#saveRDS(tallaprop, "tallas_MD.RData")
#write_csv(tallaprop, "tallas_MD.csv")


## Pedida SIBECORP

# Prepara data 

# filtro lo requerido por MD: Poblacional y Abril y Mayo
tallasib <- sizeall2 %>% 
  filter(rastro=="POBLACIONAL",
         Sampling.point %in% c(2,4,6))

# Categorizar los datos
tallasib$CAT <- as.numeric(as.character(cut(
  x = tallasib$sizeE,
  breaks = seq(0, 50, 1),
  labels = seq(0, 49, 1),
  right = FALSE
)))

# 2. Calcular la estructura en tallas (en %) por mes

# Calcular la estructura en tallas como porcentaje por mes

tallapropsib <- tallasib %>%
  group_by(ANO, MES, CAT) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  arrange(ANO, MES)

write.csv(tallapropsib, "Talla_Sibecorp.csv")
# Grafico simple

plotsib <- ggplot(tallapropsib %>% 
                    drop_na(),
       aes(x = CAT, 
           y = proportion)) +
  geom_col() +
  facet_grid(ANO ~ MES) +
  labs(x = "Bin (mm)", y = "Proportion (%)") +
  theme_few() +
  geom_vline(xintercept = 10.8, col="red")+
  ggtitle("Proporción por Mes y Año")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   size=6))
plotsib



# 1. Calcular ind.m² por clase de talla y para cada mes

# uno los datos

class_indm2 <- size_21_24 %>%
  filter(rastro=="POBLACIONAL",
         Sampling.point %in% c(2,4,6)) %>% 
  group_by(ANO, 
           MES, 
           Sampling.point, 
           ID_codificado_muestreo) %>% 
  mutate(Sampling.point = as.double(Sampling.point))


join_le_de <- right_join(denspobtot2, 
                         class_indm2)







