#Leemos los paneles:
 
# Scripts paneles rotativos 5-0-0


paneles <- panel500(n_periods = 60)
f_0izq <- function(x){
  letra <- substr(x, 1,1)
  numchar <- substr(x, 2,3)
  numchar <- stringr::str_pad(numchar, width = 2, side = "left", pad = "0")
  y <- paste0(letra, numchar) 
  y
}
#f_0izq(x = panel[,1])
Paneles <- apply(paneles, FUN = f_0izq, 2)
row.names(Paneles) <- row.names(Paneles)
colnames(Paneles) <- colnames(Paneles)
rm(paneles)



#Definir los paneles de refresco:
  
vctr_paneles_refresco <- c("A09", "B06", "C03", "C11", "D08", "E05", "E13")




# Se generan los paneles de la priemra fila (pivotal) con sus paneles hijos, no se incluyen los paneles de refresco que sustituyen estos paneles:
  
# Generar un dataframe que contenga los paneles pivotales cons sus paneles hijos que no son de refresco
f_identificaPivote <- function(x){
  if(  all(is.na(x)) ) {
    resultado <- length(x) + 1
  } else {
    resultado <- min(which(x == 1 & !is.na(x)))
  }
  resultado
}


df_paneles1raFila_HijosNoRefresco <- rbind(expand.grid(Paneles[1,1], unique(Paneles[,1])), 
                                           expand.grid(Paneles[1,2], unique(Paneles[,2])),
                                           expand.grid(Paneles[1,3], unique(Paneles[,3])),
                                           expand.grid(Paneles[1,4], unique(Paneles[,4])),
                                           expand.grid(Paneles[1,5], unique(Paneles[,5])),
                                           expand.grid(Paneles[1,6], unique(Paneles[,6])),
                                           expand.grid(Paneles[1,7], unique(Paneles[,7])),
                                           expand.grid(Paneles[1,8], unique(Paneles[,8])),
                                           expand.grid(Paneles[1,9], unique(Paneles[,9])),
                                           expand.grid(Paneles[1,10], unique(Paneles[,10])),
                                           expand.grid(Paneles[1,11], unique(Paneles[,11])),
                                           expand.grid(Paneles[1,12], unique(Paneles[,12])),
                                           expand.grid(Paneles[1,13], unique(Paneles[,13])),
                                           expand.grid(Paneles[1,14], unique(Paneles[,14])),
                                           expand.grid(Paneles[1,15], unique(Paneles[,15]))
)
names(df_paneles1raFila_HijosNoRefresco) <- c("panel_pivote", "panel_derivado")
df_paneles1raFila_HijosNoRefresco$panel_pivote <- as.character(df_paneles1raFila_HijosNoRefresco$panel_pivote)

df_paneles1raFila_HijosNoRefresco$panel_derivado  <- as.character(df_paneles1raFila_HijosNoRefresco$panel_derivado )

attr(df_paneles1raFila_HijosNoRefresco, "out.attrs") <- NULL


df_paneles1raFila_HijosNoRefresco$letra_derivada <- substr(df_paneles1raFila_HijosNoRefresco$panel_derivado, 1, 1)
df_paneles1raFila_HijosNoRefresco$numero_derivado <- as.numeric(substr(df_paneles1raFila_HijosNoRefresco$panel_derivado, 2, 3))

df_letraDigitoPanelesRefresco <- data.frame(letra_derivada = substr(vctr_paneles_refresco, 1, 1),
                                            numero_derivado = as.numeric(substr(vctr_paneles_refresco, 2, 3)),
                                            indica_refresco = 1)

df_paneles1raFila_HijosNoRefresco <- left_join(df_paneles1raFila_HijosNoRefresco, df_letraDigitoPanelesRefresco,
                                               by = c("letra_derivada", "numero_derivado"))

df_posicionPanelRefresco <- df_paneles1raFila_HijosNoRefresco %>% group_by(letra_derivada) %>% 
  summarise(posicion_refresco = f_identificaPivote(indica_refresco))

df_paneles1raFila_HijosNoRefresco <- df_paneles1raFila_HijosNoRefresco %>%
  filter(numero_derivado != 1)
df_paneles1raFila_HijosNoRefresco <- df_paneles1raFila_HijosNoRefresco %>% 
  left_join(df_posicionPanelRefresco, by = "letra_derivada")

df_paneles1raFila_HijosNoRefresco <- df_paneles1raFila_HijosNoRefresco %>% 
  filter(numero_derivado  < posicion_refresco)
df_paneles1raFila_HijosNoRefresco <- df_paneles1raFila_HijosNoRefresco %>% 
  select(panel_pivote, panel_derivado)



# Se generan los paneles de refresco pivotales con sus hijos (hasta que eventualmente otro panel de refresco pivotal lo reemplace):
  

df_panelesRefresco_Hijos <- data.frame(panel_pivote = c("A09", "A09", "A09", "B06", "B06", 
                                                        "B06", "B06", "B06", "B06", "B06", "C03", "C03", "C03", "C03", 
                                                        "C03", "C03", "C03", "C11", "C11", "D08", "D08", "D08", "D08", 
                                                        "D08", "E05", "E05", "E05", "E05", "E05", "E05", "E05", "E13"
),
panel_derivado = c("A10", "A11", "A12", "B07", "B08", "B09", 
                   "B10", "B11", "B12", "B13", "C04", "C05", "C06", "C07", "C08", 
                   "C09", "C10", "C12", "C13", "D09", "D10", "D11", "D12", "D13", 
                   "E06", "E07", "E08", "E09", "E10", "E11", "E12", NA))


df_1erospanelesRefresco_Hijos <- data.frame(panel_pivote = c("A09", "A09", "A09", "B06", "B06", 
                                                       "B06", "B06", "B06", "B06", "B06", "C03", "C03", "C03", "C03", 
                                                       "C03", "C03", "C03", "C03", "C03", "C03", "D08", "D08", "D08", 
                                                       "D08", "D08", "E05", "E05", "E05", "E05", "E05", "E05", "E05", 
                                                       "E05"), panel_derivado = c("A10", "A11", "A12", "B07", "B08", 
                                                                                  "B09", "B10", "B11", "B12", "B13", "C04", "C05", "C06", "C07", 
                                                                                  "C08", "C09", "C10", "C11", "C12", "C13", "D09", "D10", "D11", 
                                                                                  "D12", "D13", "E06", "E07", "E08", "E09", "E10", "E11", "E12", 
                                                                                  "E13"))

#Se crea un dataframe con los paneles que son sustituidos (padres) con sus hijos (sean estos o no otros paneles de refrescos):
  

df_panelesSustituidos_Hijos <- data.frame(panel_sustituye = c("A08", "A08", "A08", "A08", 
                                                              "B05", "B05", "B05", "B05", "B05", "B05", "B05", "B05", "C02", 
                                                              "C02", "C02", "C02", "C02", "C02", "C02", "C02", "C02", "C02", 
                                                              "C02", "D07", "D07", "D07", "D07", "D07", "D07", "E04", "E04", 
                                                              "E04", "E04", "E04", "E04", "E04", "E04", "E04"),
                                          panel_refresco = c("A09", 
                                                             "A10", "A11", "A12", "B06", "B07", "B08", "B09", "B10", "B11", 
                                                             "B12", "B13", "C03", "C04", "C05", "C06", "C07", "C08", "C09", 
                                                             "C10", "C11", "C12", "C13", "D08", "D09", "D10", "D11", "D12", 
                                                             "D13", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12", 
                                                             "E13"))

