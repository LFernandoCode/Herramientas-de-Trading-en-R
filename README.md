# Herramientas-de-Trading-en-R
Códigos para apoyar el análisis de criptoactivos
# 📊 **Análisis de Mínimos Horarios con R**

Este proyecto utiliza datos históricos  con una frecuencia de una hora para identificar las horas en las que el precio tocó su **mínimo diario** antes de comenzar a subir. Los resultados se presentan en un gráfico de torta que muestra la distribución de estas horas a lo largo de los últimos 20 días.  

## 🚀 **¿Qué hace este script?**

1. Descarga datos de precios desde Binance usando la librería **binancer**.  
2. Procesa los datos para calcular los mínimos diarios y la hora en la que ocurrieron.  
3. Genera un gráfico de torta con **ggplot2**, mostrando la frecuencia con la que cada hora contiene un mínimo.  

## 🛠  librerías en R:  

- `tidyverse`  
- `binancer`  
- `ggplot2`  

