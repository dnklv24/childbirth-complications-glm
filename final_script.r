library(tidyverse)
library(rio)
library(dplyr)
library(ggplot2)
# install.packages("pscl")
library(pscl)
# install.packages("pROC")
library(pROC)
# install.packages("ResourceSelection")
library(ResourceSelection)
# install.packages("writexl")  
library(writexl)

# Cargar los datos
nacimientos <- import("./Nacimientos 2022.Rdata")
write_xlsx(nacimientos,"./nacimientos_full.xlsx")

# Reemplazar valores de NORMA (1 = complicaciones, 0 = sin complicaciones)
nacimientos$NORMA <- ifelse(nacimientos$NORMA == 2, 1, 0)

# Modelo Logit
modelo_logit_3dimensional <- glm(NORMA ~ EDADM + NUMH, data = nacimientos, family = binomial())

# Validación estadística del modelo
pR2(modelo_logit_3dimensional)

# Predecimos la probabilidad
probabilidades <- predict(modelo_logit_3dimensional, type = "response")

# Curva ROC y AUC
roc_obj <- roc(nacimientos$NORMA, probabilidades)
valor_auc <- auc(roc_obj)

roc_data <- data.frame(
  especificidad = 1 - roc_obj$specificities,
  sensibilidad = roc_obj$sensitivities
)

ggplot(roc_data, aes(x = especificidad, y = sensibilidad)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = "Curva ROC - Modelo Logit",
    subtitle = paste("AUC =", round(valor_auc, 3)),
    x = "1 - Especificidad",
    y = "Sensibilidad"
  ) +
  theme_minimal()

# Test de Hosmer-Lemeshow
hoslem.test(nacimientos$NORMA, fitted(modelo_logit_3dimensional))

summary(modelo_logit_3dimensional)

# Exploración de edad y valores atípicos
min(nacimientos$EDADM) 
max(nacimientos$EDADM)
outliers_edad <- boxplot.stats(nacimientos$EDADM)$out 
unique(outliers_edad)

# Segmentación por edad y exportación
menor20 <- nacimientos %>% filter(EDADM <= 20)
write_xlsx(menor20, "./menor20.xlsx")

entre20y34 <- nacimientos %>% filter(20 < EDADM & EDADM <= 34)
write_xlsx(entre20y34, "./entre20y34.xlsx")

entre35y40 <- nacimientos %>% filter(34 < EDADM & EDADM <= 40)
write_xlsx(entre35y40, "./entre35y40.xlsx")

mayor40 <- nacimientos %>% filter(EDADM > 40)
write_xlsx(mayor40, "./mayor40.xlsx")

# Segmentación de los datos según el número de hijos
min(nacimientos$NUMH) 
max(nacimientos$NUMH)

hijos1a3 <- nacimientos %>% filter(NUMH >= 1 & NUMH <= 3)
hijos3mas <- nacimientos %>% filter(NUMH > 3)

# Calcular porcentaje de complicaciones
menor20_porcentaje <- menor20 %>%
  group_by(EDADM) %>%
  summarise(total_partos = n(),
            complicaciones = sum(NORMA == 1),
            porcentaje_complicaciones = complicaciones / total_partos * 100)

entre20y34_porcentaje <- entre20y34 %>%
  group_by(EDADM) %>%
  summarise(total_partos = n(),
            complicaciones = sum(NORMA == 1),
            porcentaje_complicaciones = complicaciones / total_partos * 100)

entre35y40_porcentaje <- entre35y40 %>%
  group_by(EDADM) %>%
  summarise(total_partos = n(),
            complicaciones = sum(NORMA == 1),
            porcentaje_complicaciones = complicaciones / total_partos * 100)

mayor40_porcentaje <- mayor40 %>%
  group_by(EDADM) %>%
  summarise(total_partos = n(),
            complicaciones = sum(NORMA == 1),
            porcentaje_complicaciones = complicaciones / total_partos * 100)

# Probabilidades según el número de partos previos
probabilidades_numh <- nacimientos %>% 
  filter(NUMH <= 3) %>% 
  group_by(NUMH) %>% 
  summarize(probabilidad = sum(NORMA == 1) / n())

# Gráficos
# Gráfico 1 
ggplot(menor20_porcentaje, aes(x = EDADM, y = porcentaje_complicaciones)) +
  geom_bar(stat = "identity", fill = "pink", alpha = 0.7) +  
  geom_line(aes(group = 1), color = "red", size = 1) +       
  geom_point(color = "red", size = 2) +                      
  scale_x_continuous(breaks = 12:20) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Edad de la madre",
       y = "Porcentaje de complicaciones",
       caption = "Figura 1: Porcentaje de mujeres que sufrieron complicaciones en el parto (menores de 20 años).") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        plot.caption = element_text(hjust=0, face="italic", size=6))

# Gráfico 2
ggplot(entre20y34_porcentaje, aes(x = EDADM, y = porcentaje_complicaciones)) +
  geom_bar(stat = "identity", fill = "violet", alpha = 0.7) +  
  geom_line(aes(group = 1), color = "red", size = 1) +       
  geom_point(color = "red", size = 2) +                      
  scale_x_continuous(breaks = 21:34) +
  scale_y_continuous(limits = c(0,15), breaks = seq(0, 15, 2), labels = function(x) paste0(x, "%")) +
  labs(x = "Edad de la madre",
       y = "Porcentaje de complicaciones",
       caption = "Figura 2: Porcentaje de mujeres que sufrieron complicaciones en el parto (entre 21 y 34 años).") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        plot.caption = element_text(hjust=0, face="italic", size=6))

# Gráfico 3
ggplot(entre35y40_porcentaje, aes(x = EDADM, y = porcentaje_complicaciones)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +  
  geom_line(aes(group = 1), color = "red", size = 1) +       
  geom_point(color