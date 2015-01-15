#Функция для очистки данных
clear_d <-function(tbl){
  tbl %>%
    mutate(Hillshade = Hillshade_9am+Hillshade_Noon+Hillshade_3pm) %>%
    select(-c(Hillshade_9am:Hillshade_3pm))%>%
    
    gather(Soil_Type,frequency, Soil_Type1:Soil_Type40 )%>%
    filter(frequency==1) %>%
    select(-frequency) %>%
    mutate(Soil_Type=as.integer(substr(Soil_Type,10,20))) %>%
    
    gather(Wilderness_Area,frequency, Wilderness_Area1:Wilderness_Area4 ) %>%
    filter(frequency==1) %>%
    select(-frequency) %>%
    mutate(Wilderness_Area=as.integer(substr(Wilderness_Area,16,20))) %>%
    
#     mutate(Aspect = round(Aspect%%316/20)) %>% 
#     mutate(X_Aspect = as.integer(100*sin(Aspect)),Y_Aspect = as.integer(100*cos(Aspect))) %>%
    arrange(Id) %>%
    select(-Id)
}


# mutate(X_Aspect = as.integer(10*sin(Aspect)),Y_Aspect = as.integer(10*cos(Aspect))) %>%
#   select(-Aspect)  %>%
