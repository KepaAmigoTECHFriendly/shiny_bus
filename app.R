
library(shiny)
library(shinyjs)
library(DT)
library(htmltools)
library(httr)
library(jsonlite)
library(dplyr)
library(shinyalert)
library(shinybusy)

library(leaflet)
library(sf)
library(lubridate)

# ------------------------------------------------------------------------------
# PETICIÓN TOKENs THB    http://plataforma:9090
# ------------------------------------------------------------------------------

cuerpo <- '{"username":"kepa@techfriendly.es","password":"kepatech"}'
post <- httr::POST(url = "http://plataforma:9090/api/auth/login",
                   add_headers("Content-Type"="application/json","Accept"="application/json"),
                   body = cuerpo,
                   verify= FALSE,
                   encode = "json",verbose()
)

resultado_peticion_token <- httr::content(post)
auth_thb <- paste("Bearer",resultado_peticion_token$token)
# ------------------------------------------------------------------------------

# REFERENCIAS
df_referencia_paradas_L_V <- read.csv("REFERENCIA_paradas_bus_plasencia_L_V.csv",sep = ",", stringsAsFactors = FALSE)
df_referencia_paradas_S_D_F <- read.csv("REFERENCIA_paradas_bus_plasencia_S_D_F.csv",sep = ",", stringsAsFactors = FALSE)


# TIEMPOS
# 1 - Número bus
# Get dispositivos plataforma
url_thb <- "http://plataforma:9090/api/tenant/devices?pageSize=10000&page=0"
peticion <- GET(url_thb, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))
df <- jsonlite::fromJSON(rawToChar(peticion$content))
df <- as.data.frame(df)
df_dispositivos_gps <- df[df$data.type == "GPS",] # Filtrado por GPS

ids_gps <- df_dispositivos_gps$data.id$id

numero_bus <- c()
keys <- URLencode(c("Número"))
for(i in 1:length(ids_gps)){

  url_gps <- paste("http://plataforma:9090/api/plugins/telemetry/DEVICE/",ids_gps[i],"/values/attributes?",keys,sep = "")
  peticion <- GET(url_gps, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))
  # Tratamiento datos. De raw a dataframe
  df <- jsonlite::fromJSON(rawToChar(peticion$content))
  numero <- df$value[grep("Número",df$key)]
  numero_bus <- c(numero_bus, numero)
}





#=====================================================
# INTERFAZ DE USUARIO
#=====================================================
ui <- fluidPage(

    tags$style(type = 'text/css',
               '.dataTables_scrollBody {transform:rotateX(180deg);}',
               '.dataTables_scrollBody table {transform:rotateX(180deg);}'
    ),

    # Inicialización shinyjs
    useShinyjs(),
    useShinyalert(),
    withMathJax(),

    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),  # Importación iconos Font awesome

    titlePanel(title=div(style = "display: inline;",
                         a(href="http://plataforma:9090",
                           img(src="img/logo_plasencia.png",style = 'width: 100px; high: 200px; display: inline;')
                         )
    )),


    # MENU
    navbarPage(id ="menu", NULL,

               tabPanel("Referencia paradas",
                        sidebarLayout(
                          sidebarPanel(
                            # 0 - Selección rango temporal
                            radioButtons(
                              inputId = "temporalidad",
                              label = "Selección rango temporal",
                              choices = c("Lunes-Viernes","Sábado, Domingos y festivos")
                            ),
                            br(),
                            tags$hr(),
                            actionButton("boton_guardar_en_BBDD", "Guardar en BBDD"),
                            width=3
                          ),

                          mainPanel(
                            tags$div(id="1",tags$h4(tags$b("Referencia paradas Lunes-Viernes"))),
                            dataTableOutput("referencia_paradas_L_V"),
                            br(),
                            tags$div(id="2",tags$h4(tags$b("Referencia paradas Sábado, Domingos y festivos"))),
                            dataTableOutput("referencia_paradas_S_D_F"),
                            width=9
                          )
                        )
               ),  # Cierre tabPanel "Referencia paradas"


               tabPanel("Cálculo referencia tiempos",
                        sidebarLayout(
                          sidebarPanel(
                            # 1 - Número autobús
                            numericInput(
                              inputId = "numero_bus",
                              label = "Número autobús",
                              value = 10,
                              min = 1,
                              max = 30,
                            ),
                            br(),
                            # 2 - Línea
                            numericInput(
                              inputId = "linea",
                              label = "Línea",
                              value = 1,
                              min = 1,
                              max = 3,
                            ),
                            br(),
                            # 3 - Sentido
                            radioButtons(
                              inputId = "sentido",
                              label = "Selección sentido",
                              choices = c("Subida","Bajada")
                            ),
                            br(),
                            # 4 - Fecha inicio
                            textInput(inputId = "fecha_inicio",
                                      label = "Fecha inicio",
                                      value = "2023-05-05 10:30:00",
                                      placeholder = "2023-05-05 10:30:00"
                            ),
                            br(),
                            # 4 - Fecha fin
                            textInput(
                              inputId = "fecha_fin",
                              label = "Fecha fin",
                              value = "2023-05-05 11:00:00",
                              placeholder = "2023-05-05 11:00:00"
                            ),
                            br(),
                            radioButtons(
                              inputId = "temporalidad_tiempos",
                              label = "Selección rango temporal",
                              choices = c("Lunes-Viernes","Sábado, Domingos y festivos")
                            ),
                            br(),
                            tags$hr(),
                            actionButton("guardar_tiempos", "Guardar tiempos de llegada"),
                            width=3
                          ),

                          mainPanel(
                            leafletOutput("mapa", height = 500),
                            br(),
                            tags$div(id="1",tags$h4(tags$b("Cálculo de matriz de tiempos de llegada"))),
                            dataTableOutput("tiempos_llegada"),
                            width=9
                          )
                        )
               )  # Cierre tabPanel "Cálculo tiempos"

    ) # Cierre navbarPage
)



#==========================================================================
#==========================================================================
# LÓGICA DE SERVIDOR
#==========================================================================
#==========================================================================
server <- function(input, output, session) {

  #====================================================================================================================================================
  # REFERENCIAS

  # ==================================
  # Cambios dinámicos

  observeEvent(input$temporalidad, {
    if(input$temporalidad == "Lunes-Viernes"){
      shinyjs::hide("referencia_paradas_S_D_F")
      shinyjs::hide("2")
      shinyjs::show("referencia_paradas_L_V")
      shinyjs::show("1")
    }else{
      shinyjs::show("referencia_paradas_S_D_F")
      shinyjs::show("2")
      shinyjs::hide("referencia_paradas_L_V")
      shinyjs::hide("1")
    }
  })


  datos <- reactiveValues(df_referencia_L_V = NULL, df_referencia_S_D_F = NULL)

  #Captura datos modificados

  # Referencia paradas L-V
  observeEvent(input$referencia_paradas_L_V_cell_edit, {
    datos$df_referencia_L_V[input$referencia_paradas_L_V_cell_edit$row, (input$referencia_paradas_L_V_cell_edit$col + 1)] <- input$referencia_paradas_L_V_cell_edit$value

  })

  # Referencia paradas S-D-F
  observeEvent(input$referencia_paradas_S_D_F_cell_edit, {

    datos$df_referencia_S_D_F[input$referencia_paradas_S_D_F_cell_edit$row, (input$referencia_paradas_S_D_F_cell_edit$col + 1)] <- input$referencia_paradas_S_D_F_cell_edit$value

  })


  observeEvent(input$boton_guardar_en_BBDD, {
    shinyalert("¿Está seguro de que desea actualizar los datos?", "", showCancelButton=TRUE, showConfirmButton=TRUE, confirmButtonText = "Sí", cancelButtonText = "No", callbackR = confirmacion_subida)
  })

  confirmacion_subida <- function(value) {
    if(value){

      show_modal_spinner(
        spin = "double-bounce",
        color = "#8f1336",
        text = "Actualizando datos. Por favor, espere unos segundos",
        session = session
      )

      # Referencia de L - V
      df_referencia_L_V <-  datos$df_referencia_L_V
      # Escritura CSV en APP SHINY
      write.csv(df_referencia_L_V,"img/REFERENCIA_paradas_bus_plasencia_L_V.csv", row.names = FALSE)
      # Escritura CSV en CARPETA COMPARTIDA para programa tiempos de llegada
      df_csv <- df_referencia_L_V[,c("id","Nombre_plataforma","latitud","longitud","linea_1","linea_2","linea_3","sentido")]
      colnames(df_csv)[2] <- "name"
      # write.csv(df_csv,"/srv/shiny-server/shiny_bus/paradas_bus_plasencia_L_V.csv", row.names = FALSE)

      # Referencia de S - D - F
      df_referencia_S_D_F <-  datos$df_referencia_S_D_F
      # Escritura CSV en APP SHINY
      # write.csv(df_referencia_S_D_F,"/srv/shiny-server/shiny_bus/REFERENCIA_paradas_bus_plasencia_S_D_F.csv", row.names = FALSE)
      # Escritura CSV en CARPETA COMPARTIDA para programa tiempos de llegada
      df_csv <- df_referencia_S_D_F[,c("id","Nombre_plataforma","latitud","longitud","linea_1","linea_2","linea_3","sentido")]
      colnames(df_csv)[2] <- "name"
      # write.csv(df_csv,"/srv/shiny-server/shiny_bus/paradas_bus_plasencia_S_D_F.csv", row.names = FALSE)

      remove_modal_spinner(session = getDefaultReactiveDomain())
      shinyalert("Éxito", "Datos registrados correctamente", type = "success")
    }
  }


  # ==================================
  # Referencias

  referencia_L_V <- reactive({

    df <- df_referencia_paradas_L_V
    return(df)

  })

  referencia_S_D_F <- reactive({

    df <- df_referencia_paradas_S_D_F
    return(df)

  })

  # ==================================
  # Outputs

  # Tabla referencia paradas L-V
  output$referencia_paradas_L_V <- renderDataTable({
    df <- referencia_L_V()

    datos$df_referencia_L_V = df


    datatable(df, editable = TRUE, rownames= FALSE, options = list(searchHighlight = TRUE,pageLength = 30,
                                                                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                   scrollX=TRUE,
                                                                   scrollCollapse=TRUE))
  })

  # Tabla referencia paradas S-D-F
  output$referencia_paradas_S_D_F <- renderDataTable({
    df <- referencia_S_D_F()

    datos$df_referencia_S_D_F = df

    datatable(df, editable = TRUE, rownames= FALSE, options = list(searchHighlight = TRUE,pageLength = 30,
                                                                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                   scrollX=TRUE,
                                                                   scrollCollapse=TRUE))
  })





  #====================================================================================================================================================
  # CÁLCULO TIEMPOS

  observeEvent(input$guardar_tiempos, {
    shinyalert("¿Está seguro de que desea guardar los tiempos calculados?", "", showCancelButton=TRUE, showConfirmButton=TRUE, confirmButtonText = "Sí", cancelButtonText = "No", callbackR = confirmacion_subida_tiempos)
  })


  # ==================================
  # Referencias

  id_gps_filtro <- reactive({

    # Get id_gps en base al número del autobús
    posicion_numero <- match(input$numero_bus, numero_bus)
    id_gps <- ids_gps[posicion_numero]

  })

  datos_bus <- reactive({

    id_dispositivo <- id_gps_filtro()

    fecha_1 <- input$fecha_inicio
    fecha_2 <- input$fecha_fin

    fecha_1 <- format(as.numeric(as.POSIXct(fecha_1))*1000,scientific = F)
    fecha_2 <- format(as.numeric(as.POSIXct(fecha_2))*1000,scientific = F)

    keys <- URLencode(c("lat,lon,spe"))
    url_thb_fechas <- paste("http://plataforma:9090/api/plugins/telemetry/DEVICE/",id_dispositivo,"/values/timeseries?limit=10000&keys=",keys,"&startTs=",fecha_1,"&endTs=",fecha_2,sep = "")
    peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

    # Tratamiento datos. De raw a dataframe
    df <- jsonlite::fromJSON(rawToChar(peticion$content))
    df <- as.data.frame(df)
    df <- df[,-c(3,5)]

    # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
    shiny::validate(
      need(nrow(df) != 0,
           "¡Atención!\nNo existen datos para la combinación de filtros seleccionada\nSeleccione de nuevo los filtros por favor.")
    )

    colnames(df) <- c("ts","lat","lon","spe")
    df$spe <- gsub("*. km/h","",df$spe)
    df$spe <- as.numeric(df$spe)
    #df <- df[-grep(" km/h",df$spe),]
    df <- df[df$lat != "none",]
    df$fecha_time <- as.POSIXct(as.numeric(df$ts)/1000, origin = "1970-01-01")

    df_datos_bus <- df
    df_datos_bus$lat <- as.numeric(df_datos_bus$lat)
    df_datos_bus$lon <- as.numeric(df_datos_bus$lon)
    return(df_datos_bus)

  })

  # MAPA
  output$mapa <- renderLeaflet({

    df_datos_bus <- datos_bus()

    # Ploteo leaflet marquesinas
    leaflet(df_datos_bus) %>%
      addTiles() %>%
      addMarkers(lng=df_datos_bus$lon, lat=df_datos_bus$lat, popup = as.character(df_datos_bus$fecha_time))
  })


  calculo_matriz_tiempos <- reactive({

    show_modal_spinner(
      spin = "double-bounce",
      color = "#8f1336",
      text = "Calculando matriz de tiempos. Por favor, espere unos segundos",
      session = session
    )

    df_datos_bus <- datos_bus()

    if(input$temporalidad_tiempos == "Lunes-Viernes"){
      df_paradas <- read.csv("REFERENCIA_paradas_bus_plasencia_L_V.csv", sep = ",")
    }else{
      df_paradas <- read.csv("REFERENCIA_paradas_bus_plasencia_S_D_F.csv", sep = ",")
    }

    # Linea 1
    df_paradas_linea_1_subida <- df_paradas[df_paradas$linea_1 == 1 & (df_paradas$sentido == 1 | df_paradas$sentido >=2),]
    df_paradas_linea_1_bajada <- df_paradas[df_paradas$linea_1 == 1 & (df_paradas$sentido == 0 | df_paradas$sentido >=2),]

    # Linea 2
    df_paradas_linea_2_subida <- df_paradas[df_paradas$linea_2 == 1 & (df_paradas$sentido == 1 | df_paradas$sentido >=2),]
    df_paradas_linea_2_bajada <- df_paradas[df_paradas$linea_2 == 1 & (df_paradas$sentido == 0 | df_paradas$sentido >=2),]

    # Linea 3
    df_paradas_linea_3_subida <- df_paradas[df_paradas$linea_3 == 1 & (df_paradas$sentido == 1 | df_paradas$sentido >=2),]
    df_paradas_linea_3_bajada <- df_paradas[df_paradas$linea_3 == 1 & (df_paradas$sentido == 0 | df_paradas$sentido >=2),]



    #------------------------------------------------------------------------------
    # GENERACIÓN GEOCERCAS POR LÍNEA Y SENTIDO
    #-----------------------------------------------------------------------------
    # Creación de geocercas
    dato_linea <- input$linea
    sentido <- ifelse(input$sentido == "subida",1,0)
    if(dato_linea == 1){
      df_trabajo_paradas <- df_paradas[df_paradas$linea_1 == 1 & (df_paradas$sentido == as.numeric(sentido) | df_paradas$sentido >=2),]
    }else if(dato_linea == 2){
      df_trabajo_paradas <- df_paradas[df_paradas$linea_2 == 1 & (df_paradas$sentido == as.numeric(sentido) | df_paradas$sentido >=2),]
    }else{
      df_trabajo_paradas <- df_paradas[df_paradas$linea_3 == 1 & (df_paradas$sentido == as.numeric(sentido) | df_paradas$sentido >=2),]
    }

    lat <- df_trabajo_paradas$latitud
    long <- df_trabajo_paradas$longitud

    # Agrupación de puntos en variable stores
    paradas_sfc <- st_sfc(st_multipoint(cbind(long, lat)), crs = 4326)   # Puntos paradas

    ID_GEOCERCA <- c()
    ID_PARADA <- c()
    NOMBRE_PARADA_GEOCERCA <- c()

    # PARADAS, NO HACE FALTA ITERAR
    # Cambio a UTM
    paradas_utm <- st_transform(paradas_sfc, "+proj=utm +zone=29")
    # Generación de geocercas en paradas
    paradas_separadas_id <- st_cast(paradas_utm, "POINT")
    geocercas <- st_buffer(paradas_separadas_id, 70)


    # BUCLE POR CADA UNO DE LOS REGISTROS DE POSICIÓN DEL AUTOBUS
    for(i in 1:nrow(df_datos_bus)){

      # Posición bus
      posicion_bus <- st_sfc(st_point(c(df_datos_bus$lon[i], df_datos_bus$lat[i])), crs = 4326)

      # Cambio a UTM
      #paradas_utm <- st_transform(paradas_sfc, "+proj=utm +zone=29")
      posicion_bus_utm     <- st_transform(posicion_bus, "+proj=utm +zone=29")

      # Generación de geocercas en paradas
      #paradas_separadas_id <- st_cast(paradas_utm, "POINT")
      #geocercas <- st_buffer(paradas_separadas_id, 70)

      # Conversión multipunto a punto de la posición del bus
      columnas_utm_posicion_bus <- st_cast(posicion_bus_utm, "POINT")

      # Comprobación si el bus está sobre una geocerca (dataframe booleane de n filas donde n son las paradas de la línea en un sentido, y 1 columna)
      id_posicion_geocerca <- st_contains(geocercas, columnas_utm_posicion_bus, sparse = FALSE)

      # Si el bus está encima de al menos una geocerca:
      if(any(id_posicion_geocerca[,1])){
        id_geocerca_actual <- match(TRUE,id_posicion_geocerca[,1])  # Get id de la geocerca en la que se encuentra el bus

        # GENERACIÓN DF CON GEOCERCAS
        id_parada <- df_trabajo_paradas$id
        nombre_parada <- df_trabajo_paradas$Nombre_plataforma
        id_geocerca <- 1:length(paradas_separadas_id)
        df_geocercas <- data.frame(id_parada, nombre_parada, id_geocerca, id_posicion_geocerca, geocercas)

        # ID parada donde se encuenta el bus actualmente
        id_parada_detección_bus <- df_geocercas$id_parada[df_geocercas$id_geocerca == id_geocerca_actual]
        nombre_parada_detección_bus <- df_geocercas$nombre_parada[df_geocercas$id_geocerca == id_geocerca_actual]

        # Volcado en arrays
        ID_GEOCERCA <- c(ID_GEOCERCA, id_geocerca_actual) # Volcado id geocerca en array geocercas
        NOMBRE_PARADA_GEOCERCA <- c(NOMBRE_PARADA_GEOCERCA, nombre_parada_detección_bus) # Volcado nombre parada en array geocercas
        ID_PARADA <- c(ID_PARADA, id_parada_detección_bus)
      }else{
        # Volcado en arrays
        ID_GEOCERCA <- c(ID_GEOCERCA, NA) # Volcado id geocerca en array geocercas
        NOMBRE_PARADA_GEOCERCA <- c(NOMBRE_PARADA_GEOCERCA, NA) # Volcado nombre parada en array geocercas
        ID_PARADA <- c(ID_PARADA, NA)
      }
    }

    df_datos_bus$ID_GEOCERCA <- ID_GEOCERCA
    df_datos_bus$ID_PARADA <- ID_PARADA
    df_datos_bus$NOMBRE_PARADA_GEOCERCA <- NOMBRE_PARADA_GEOCERCA

    df_datos_bus_sin_na <- na.omit(df_datos_bus)
    df_datos_sin_paradas_duplicadas <- df_datos_bus_sin_na[!duplicated(df_datos_bus_sin_na$NOMBRE_PARADA_GEOCERCA), ]
    df_datos_sin_paradas_duplicadas <- df_datos_sin_paradas_duplicadas[order(df_datos_sin_paradas_duplicadas$ts, decreasing = FALSE),]  # Orden por ts



    # -----------------------------------------------------------------------------
    # MATRIZ ORIGEN DESTINO
    # -----------------------------------------------------------------------------

    # Generación de nuevas columnas = nombre paradas
    for(i in 1:nrow(df_datos_sin_paradas_duplicadas)){
      df_datos_sin_paradas_duplicadas[, as.character(df_datos_sin_paradas_duplicadas$NOMBRE_PARADA_GEOCERCA[i])] <- replicate(nrow(df_datos_sin_paradas_duplicadas), NA)
    }

    # Calculo tiempos entre paradas
    for(col in 9:(ncol(df_datos_sin_paradas_duplicadas))){
      for(filas in 1:nrow(df_datos_sin_paradas_duplicadas)){
        tiempo <- df_datos_sin_paradas_duplicadas$fecha_time[filas]
        second(tiempo) <- second(tiempo) - 20

        diferencia_tiempo <- as.numeric(difftime(df_datos_sin_paradas_duplicadas$fecha_time[col-8], tiempo, units = "mins"))
        if(diferencia_tiempo < 1 & diferencia_tiempo > 0.5){
          df_datos_sin_paradas_duplicadas[filas,col] <- 1
        }else{
          df_datos_sin_paradas_duplicadas[filas,col] <- floor(as.numeric(difftime(df_datos_sin_paradas_duplicadas$fecha_time[col-8], tiempo, units = "mins")))
        }
      }
    }


    df_matriz_tiempos <- df_datos_sin_paradas_duplicadas[,7:ncol(df_datos_sin_paradas_duplicadas)]

    remove_modal_spinner(session = getDefaultReactiveDomain())

    return(df_matriz_tiempos)

  })




  # Cálculo matriz de tiempos
  output$tiempos_llegada <- renderDataTable({

    df_matriz_tiempos <- calculo_matriz_tiempos()

    datatable(df_matriz_tiempos, editable = TRUE, rownames= FALSE, options = list(searchHighlight = TRUE,pageLength = 30,
                                                                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                   scrollX=TRUE,
                                                                   scrollCollapse=TRUE))
  })



  confirmacion_subida_tiempos <- function(value) {
    if(value){

      show_modal_spinner(
        spin = "double-bounce",
        color = "#8f1336",
        text = "Actualizando datos. Por favor, espere unos segundos",
        session = session
      )

      df_matriz_tiempos <- calculo_matriz_tiempos()
      if(input$temporalidad_tiempos == "Lunes-Viernes"){
        tiempo <- "L_V"
      }else{
        tiempo <- "S_D_F"
      }
      write.csv(df_matriz_tiempos,paste("matriz_tiempos_bajada_L1.csv","matriz_tiempos_",input$sentido,"_L",input$linea,"_",tiempo,".csv",sep = ""), row.names = FALSE)

      remove_modal_spinner(session = getDefaultReactiveDomain())
      shinyalert("Éxito", "Datos registrados correctamente", type = "success")
    }
  }



}



# Run the application
shinyApp(ui = ui, server = server)
