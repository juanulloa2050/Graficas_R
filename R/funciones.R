# library(readxl)
library(dplyr)
library(cowplot)
library(ggplot2)
library(stringr)

crear_pastel <- function(Base_de_Datos, variable, colores = NULL) {
  
  pastel <- Base_de_Datos %>% 
    count({{ variable }}) %>% 
    mutate(pcnt = n / sum(n),
           etiquetas = scales::percent(pcnt),
           midangle = cumsum(pcnt) - pcnt / 2)
  
  pastel_plot <- ggplot(pastel, aes(x = 1, y = pcnt, fill = {{ variable }})) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(x = 1.2, y = pcnt, label = etiquetas),
              position = position_stack(vjust = 0.5),
              color = "white", size = 4,
              angle = pastel$midangle) +
    guides(fill = guide_legend(title = "Categorías")) +
    coord_polar(theta = "y") + 
    theme_void() +
    theme(legend.position = "right")
  
  if (!is.null(colores)) {
    pastel_plot <- pastel_plot + scale_fill_manual(values = colores)
  }
  
  return(pastel_plot)
}


crear_grafico_barras <- function(Base_de_Datos, variable, colores = "#E066FF") {
  
  bar <- ggplot(Base_de_Datos, aes(x = {{ variable }})) +
    geom_bar(stat="count", fill = colores, color='black') +
    theme_cowplot() +
    ylab("Frecuencia") +
    theme(axis.text.x = element_text(vjust = 1, hjust = 0.5)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  
  return(bar)
}

crear_diagrama_caja <- function(Base_de_Datos, variable, colores = "#FF9900") {
  
  boxplot <- ggplot(Base_de_Datos, aes(x = "", y = {{ variable }})) +
    geom_boxplot(fill = colores, color = "black") +
    theme_cowplot() +
    ylab("Valor") +
    xlab("") +
    theme(axis.text.x = element_blank())
  
  return(boxplot)
}

crear_histograma <- function(Base_de_Datos, variable, colores = "#FF5733") {
  
  hist <- ggplot(Base_de_Datos, aes(x = {{ variable }})) +
    geom_histogram(fill = colores, color = "black") +
    theme_cowplot() +
    ylab("Frecuencia") +
    xlab("Valor") +
    theme(axis.text.x = element_text(vjust = 1, hjust = 0.5))
  
  return(hist)
}


