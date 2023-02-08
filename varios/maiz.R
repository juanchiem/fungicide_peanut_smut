library(tidyverse)

book <- expand.grid(
  semilla = str_c("sem", 0:1), 
  foliar = str_c("fol", 0:1),
  bloque = as.roman(1:3)) %>% 
  mutate(plot = row_number()) %>%  
  left_join(expand.grid(plot = .$plot, pl = 1:5), by="plot") %>% 
  select(plot, everything()) %>% 
  arrange(plot, semilla, foliar) 

# write.csv("~/Dropbox/Papers/1 In progress/carbon_mani/evaluacion_sanitaria_maiz.csv")

library(googlesheets)
my_sheets <- gs_ls() #%>%  View
View(my_sheets)

#registrar una planilla
maiz <- gs_title("maiz_zinc")
maiz$browser_url

#Inspeccionar 
maiz %>% gs_ws_ls()

#Navegar
maiz %>% gs_browse()

maiz %>% gs_read(ws = "enf")

#Agregar una nueva hoja con la planilla creada a un archivo pre-existente

maiz <- maiz %>% 
  gs_ws_new(ws_title = "eval_enferm", input = book,
            trim = FALSE, verbose = FALSE)

#Crear una nuevo archivo con la planilla creada

gs_new("ensayo_maiz", ws_title = "eval_13_3", 
       input = planilla,
       trim = FALSE, verbose = FALSE) %>% 
  gs_read()

