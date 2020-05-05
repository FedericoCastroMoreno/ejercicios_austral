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
df_base_p <- read_csv("20200425_script_clase_3_algoritmos/data/data_pobl_mundial.csv")

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
df_final$conf_por_millon <- round(df_final$conf_por_millon,3)
df_final$decesos_por_millon <- round(df_final$decesos_por_millon,3)

# 8. Gráfica 1 ----
seleccion_paises <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                      "Peru", "Paraguay", "US", "China")


df_final$pais %>% unique
fig_1 <- df_final %>%
    filter(pais %in% seleccion_paises) %>% 
    plot_ly(
        x = ~ conf_por_millon, 
        y = ~ decesos_por_millon,
        color = ~ pais,
        type = 'scatter',
        mode = 'markers',
        frame = ~ fecha,
        size = ~ PopTotal)

fig_1
