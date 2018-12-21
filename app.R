###GASOLINA

library(shinydashboard)
library(shiny)
library(googlesheets)
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)




ui <- dashboardPage(
  dashboardHeader(title = "Gasolina"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Ingreso Informacion", tabName = "ingreso_informacion", icon = icon("pencil")),
      menuItem("Preview", tabName = "preview", icon = icon("th")),
      menuItem("Graficos", tabName = "graficos", icon = icon("signal")),
      menuItem("Google Sheet",tabName = "google_sheet", icon = icon("list-alt"))
    )
    
  ),
  dashboardBody(
    tabItems(
      ###ingreso_informacion tab content
      tabItem(
        tabName = "ingreso_informacion",
        fluidRow(
          box(
            title = "Controls",
            
            ##DateInput Fecha
            dateInput("fecha_id", "Fecha", value = Sys.Date()),
            
            ##numericInput Kilometraje
            numericInput("kilometraje_id","Kilometraje:", value = 0.0, min = 0),
            
            ##numericInput Monto
            numericInput("monto_id","Monto:", value = 0.0, min = 0),
            
            ##numericInput Litros
            numericInput("litros_id","Litros:", value = 0.0, min = 0),
            
            ##ActionButton Guardar
            actionButton("guardar_id", "Guardar"),
            
            htmlOutput("Text")
            
          )
          
          
        )
      ),
      ###preview tab content
      tabItem(
        tabName = "preview",
        fluidRow(
          box(
            title = "Informacion Almacenda",
            ###TABLA QUE MUESTRA LA INFORMACION DEL WORKSHEET
            dataTableOutput("previewtbl_id"),
            ##ActionButton Guardar
            actionButton("refresh_id", "Refresh")
          )
         
          
        )
       
        
      ),
      ###graficos tab content
      tabItem(
        tabName = "graficos",
        fluidRow(
          box(
            title = "",
            plotOutput("plot_line_monto", width = 400)
          )
        )
      ),
      ###google_sheet tab content
      tabItem(
        tabName = "google_sheet",
        h2("GOOGLE SHEET TAB")
      )
    )
  )
)

server <- function(input, output) {
  
  ####AUTENTICACION EN GOOGLESHEETS
  # token <- gs_auth(cache = TRUE)
  # gs_auth(token = token)
  


   gs_df <- reactive({
     #Example key for my simple spreadsheet on google drive
     options("googlesheets.httr_oauth_cache" = "gs_auth")
     gskey2 <- "1_Aph_YRhnw4DAD7WKWs09vpI1Z9Fvog-c7g180x5dFc"
     
     gs_auth()
     # register the googlesheet
     gs_file <- gs_key(x=gskey2)
     gs_df <- head(gs_read(gs_file))
   })
  
  ###ACCION DEL BOTON DE GUARDADR
  observeEvent(input$guardar_id,{
    
    ###BORRAR MENSAJE DE GUARDADO DESPUES DE INGRESAR UN REGISTRO
    output$Text <- renderPrint({
      if(input$guardar_id == 0) {
        HTML(paste0("<h3>","","</h3>"))
      }
    })
    
    ###IDENTIFICAR GOOGLESHEET
    gasolinaform <- gs_title("GasolinaForm")
    
    ###SE INSERTA LOS VALORES DEL FORM EN EL GOOGLESHEET
    gasolinaform <- gs_add_row(gasolinaform, ws = "data",
                               input = c(as.character(now()),as.character(input$fecha_id), input$kilometraje_id,
                                         input$monto_id, input$litros_id))
    
    ###MENSAJE DESPUES DE GUARDAR LOS REGISTROS
    output$Text <- renderPrint({
      if(input$guardar_id > 0) {
        HTML(paste0("<h3>","Registros guardados!","</h3>"))
      }
    })
    
  })######
  
  ###SE CREA UN DATAFRAME CON EL TAB "DATA" DEL WORKSHEET
  # gs_dataframe <-gs_read(gasolinaform, ws="data")
  # output$previewtbl_id <- renderDataTable(gs_dataframe)
  
  ###ACCION PARA EL BOTON REFRESH DEL TAB PREVIEW
  observeEvent(input$refresh_id,{
    gasolinaform <- gs_title("GasolinaForm")
    gs_dataframe <-gs_read(gasolinaform, ws="data")
    output$previewtbl_id <- renderDataTable(gs_dataframe)
    
  })
  
  ###Graficco Monto por Fecha
  output$plot_line_monto <- renderPlot({
    #Identificacion de Googlesheet
    gasolinaform <- gs_title("GasolinaForm")
    
    #Carga de informacion de googlesheet
    gs_dataframe <-gs_read(gasolinaform, ws="data")
    
    ##Se convierten los campos de texto a fecha y numerico
    gs_dataframe$`Fecha de Carga` <- as.Date(gs_dataframe$`Fecha de Carga`)
    gs_dataframe$`Distancia recorrida` <- as.numeric(gs_dataframe$`Distancia recorrida`)
    gs_dataframe$`Monto de Carga` <- as.numeric(gs_dataframe$`Monto de Carga`)
    gs_dataframe$Litros <- as.numeric(gs_dataframe$Litros)
    
    gs_dataframe$year <- format(gs_dataframe$`Fecha de Carga`,"%Y")
    gs_dataframe$day <- format(gs_dataframe$`Fecha de Carga`,"%d")
    gs_dataframe$month <- format(gs_dataframe$`Fecha de Carga`,"%b")
  
    ##Generacion de grafico
    # p <- plot_ly(gs_dataframe, x = ~`Fecha de Carga`, y = ~`Monto de Carga`, type = 'scatter', mode = 'lines')
    # p
    
    
  })
  
}

shinyApp(ui, server)
