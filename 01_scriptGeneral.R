
# Librerias
library("openxlsx")
library("Rscience.base")

# Source
source(file = "lib.R")

# Funciones de la libreria Rscience.base
ls("package:Rscience.base")

# Importar archivo Excel
# archivo_excel <- file.choose()
archivo_excel <-  "D:\\bio01\\data-raw\\Rsci_Base001.xlsx"

# Importamos la base de datos
base_original <- read.xlsx(xlsxFile = archivo_excel, sheet = 1)
base_original

# Base Modificada
base_mod <- ControlAndChange(base = base_original, language = "ESP", speak = T)
base_mod[,3] <- as.character(base_mod[,3])
base_mod

# n_resumen()
tabla_n_resumen <- n_resumen(base = base_mod)
tabla_n_resumen

# Medidas de posicion
tabla_posicion <- MedidasPosicion(base = base_mod,  
                                  selected_columns = c(1,3, 5), 
                                  digits = 2)

tabla_posicion

# Medidas de Dispersion
tabla_dispersion <- MedidasDispersion(base = base_mod,  
                                      selected_columns = c(1,3, 5), 
                                      digits = 2)
tabla_dispersion

tabla_resumen <- MedidasResumen(base = base_mod, 
                                selected_columns = c(1,3,5),
                                digits = 2)

tabla_resumen



Graficos.1C(base = base_mod, selected_columns = c(1, 3, 5), only_plot = TRUE)

################################################################################



Graficos.1C(base = base_mod, selected_columns = c(1), only_plot = FALSE)

