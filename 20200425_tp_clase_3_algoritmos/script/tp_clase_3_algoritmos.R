###############################################################################
#Script: script.Rmd
#Descripción: Ejercicio en R para presentar en el curso de 
# algoritmos - MCD - U. Austral
#Autor: XXXXXX
#Email: XXXXXX
#Fecha creacion: XXXXX
###############################################################################

# 1. Librerías ----
library(plotly)
library(gapminder)
library(tidyverse)
library(lubridate)

# 2. Carga de datos ----
df_base_confirmed <- 
    read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

df_base_m <- 
    read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# Carga data habitantes
# df_p <- read_csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv")
# write_csv(df_p, "20200425_script_clase_3_algoritmos/data/data_pobl_mundial.csv")
df_base_p <- read_csv("20200425_tp_clase_3_algoritmos/data/data_pobl_mundial.csv")

# 3. Procesamiento data df_confirmed ----
df_conf <- 
    df_base_confirmed %>% 
    select(2, seq(5,107,3)) %>% 
    pivot_longer(cols = 2:36,names_to = "fecha") %>% 
    rename("pais" = `Country/Region`) %>% 
    group_by(pais, fecha) %>% 
    summarise(confirmados = sum(value))

df_conf$fecha <- df_conf$fecha %>% as.Date("%m/%d/%y")
df_conf <- df_conf[order(df_conf$fecha),]

# 4. Procesamiento datos de fallecimientos ----
df_mu <- 
    df_base_m %>% 
    select(2, seq(5,107,3)) %>% 
    pivot_longer(cols = 2:36,names_to = "fecha") %>% 
    rename("pais" = `Country/Region`) %>% 
    group_by(pais, fecha) %>% 
    summarise(decesos = sum(value))

df_mu$fecha <- df_mu$fecha %>% as.Date("%m/%d/%y")
df_mu <- df_mu[order(df_mu$fecha),]

# 5. Procesamiento datos de poblacion ----
df_pob <-
    df_base_p %>% filter(Time == 2020, Variant == "Medium") %>% 
    select(2, 9) %>% 
    rename("pais" = Location) %>% 
    mutate(pais = case_when(
        pais =="Bolivia (Plurinational State of)" ~ "Bolivia",
        pais == "Brunei Darussalam" ~ "Brunei",
        pais == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
        pais == "Viet Nam" ~ "Vietnam",
        pais == "United States of America" ~ "US",
        pais == "Republic of Korea" ~ "Korea, South",
        pais == "Congo" ~ "Congo (Brazzaville)",
        pais == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
        pais == "Iran (Islamic Republic of)" ~ "Iran",
        pais == "Lao People's Democratic Republic" ~ "Laos",
        pais == "Republic of Moldova" ~ "Moldova",
        pais == "Russian Federation" ~ "Russia",
        pais == "Syrian Arab Republic" ~ "Siria",
        pais == "China, Taiwan Province of China" ~ "Taiwan*",
        pais == "United Republic of Tanzania" ~ "Tanzania",
        TRUE ~ pais)
    )

# 6. Cruce de tablas ----
df_final <- merge(df_conf, df_mu, by=c("pais","fecha")) 

df_final <- left_join(df_final, df_pob, by = "pais") %>% 
    mutate(conf_por_millon = (confirmados/PopTotal)*1000,
           decesos_por_millon = (decesos/PopTotal)*1000
    )

rm(df_mu, df_pob, df_conf, df_base_confirmed, df_base_m, df_base_p)

# 7. Procesamiento tabla final ----

glimpse(df_final)
# Excluyo zonas sin dato de cantidad de población o no relevantes
df_final <- df_final[complete.cases(df_final$PopTotal),]
# cambio a tipo factor el pais
df_final$pais <- as.factor(df_final$pais)
# Establezco fecha en cantidad de días desde el 21 de marzo de 2020
df_final$fecha <- ymd(df_final$fecha) - ymd("2020-01-21")
# redondeo
df_final$conf_por_millon <- round(df_final$conf_por_millon,0)
df_final$decesos_por_millon <- round(df_final$decesos_por_millon,0)

# 8. Gráfica 1 ----
df_final$pais %>% unique

seleccion_paises <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                      "Peru", "Paraguay","Uruguay", "Ecuador", "China", "US", "Russia",
                      "France", "Italy", "Spain", "South Africa", "Turkey", "Iran", 
                      "Portugal", "Norway", "Nicaragua", "Poland", "Rwanda", "New Zeland",
                      "Switzerland", "Thailand", "United Kingdom")


fig_1 <- 
    df_final %>%
    filter(pais %in% seleccion_paises) %>% 
    plot_ly(
        x = ~ conf_por_millon, 
        y = ~ decesos_por_millon,
        hoverinfo = 'text',
        text = ~ paste("País: ", pais, "<br>Casos por millon: ", conf_por_millon, "<br>Decesos por millon: ", decesos_por_millon ),
        color = ~ pais,
        colors = "Paired",
        type = 'scatter',
        mode = 'markers',
        marker = list(opacity = 0.8, sizemode = "diameter",line = list(width = 2, color = '#FFFFFF')),
        size = ~ PopTotal, 
        sizes = c(30, 60),
        frame = ~ fecha) %>% 
    layout(title = "",
           xaxis = list(title = "Casos confirmados (por millón hab.)"),
           yaxis = list(title = "Casos de muerte (por millón hab.)"),
           paper_bgcolor = 'rgb(255, 229, 229)',
           plot_bgcolor = 'rgb(255, 229, 229)'
    ) %>% 
    animation_slider(
        currentvalue = list(prefix = "Días desde el 21 de Enero: ", font = list(color="red"))
    )

fig_1


# 9. Procesamiento data df_covid19 ----

df_base_confirmed <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

df_base_m <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

df_covid19 <- mutate(df_base_confirmed, Categoria = "Confirmados") %>% 
           bind_rows(mutate(df_base_m, Categoria = "Muertes")) %>% 
           rename("Pais" = `Country/Region`) %>%
           select(2,Categoria,seq(5,107,3)) %>% 
           pivot_longer(3:37, names_to = "Fecha", values_to = "Casos") %>%
           mutate(Fecha = mdy(str_remove(Fecha, "X")))

options(scipen = 20)

paises_a_comparar <- c("Argentina", "Brazil", "Chile","Italy","United Kingdom",
                       "Spain", "France", "US", "China")
# 10. Gráfica 2 ----
fig_2 <- df_covid19 %>% 
  filter(Pais %in% paises_a_comparar) %>% 
  group_by(Categoria, Fecha, Pais) %>% 
  summarise(Casos = sum(Casos)) %>% 
  ggplot() +
  geom_col(aes(x = Fecha, y = Casos, fill = Categoria)) +
  facet_wrap(~Pais, scales = "free") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "COVID19: Evolución de Casos",
       fill = "Situación",
       caption = "\nFuente: The Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)",
       y = NULL, x = NULL) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

fig_2 <- ggplotly(fig_2, tooltip = c("Categoria","Casos")) %>%
  layout(legend = list(orientation = "v",x = 1,y = 1)) %>%
  layout(annotations = list(x = 1.25, y = -0.07, text = "Fuente: The Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)", 
                          showarrow = F, xref='paper', yref='paper', 
                           xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=9, color="grey")))
fig_2

