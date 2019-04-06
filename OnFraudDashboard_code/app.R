#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("./funciones.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "Prueba de concepto"),
  
  # Sidebar with a slider input for number of bins
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Presentacion muestra",
      tabName = "dashboard",
      icon = icon("dashboard")
      
    ),
    menuItem("Outliers en contratos unicos", tabName = "ContratosUnicos", icon = icon("th")),
    
    menuItem(
      "Expedientes cercanos a umbral",
      tabName = "ContratosCercanos",
      icon = icon("th")
    ),
    menuItem("Agregacion de expedientes", tabName = "ContratosAgregados",icon = icon("th")),
    menuItem("Principales Resultados", tabName = "Resultados",icon = icon("th"))
    )
  ),
  
  dashboardBody(tabItems(
    tabItem(
      tabName = "dashboard",
      tabTitle="xxx",
      fluidRow(
        valueBoxOutput("valor", width = 3),
        
        valueBoxOutput("suma", width = 3),
        
        valueBoxOutput("proveedores", width = 2),
        
        valueBoxOutput("valormdoedioprov", width = 2),
        
        valueBoxOutput("valormediocont", width = 2)
        
      ),
      fluidRow( box(title='Expedientes',status = "primary", solidHeader = T,width = 12,plotlyOutput(
        "plot1", width = "100%", height = 500
      ))),
      fluidRow(column(8,
                      box( title='Volumen de contratos', status = "primary", solidHeader = T,width = 12,plotlyOutput("pie1", width = "100%"))),
               column(4,
                      box( title='Volumen de la muestra', status = "primary", solidHeader = T,width = 12,tableOutput('tabla1'))))
    ),
    tabItem(
      tabName = "ContratosUnicos",
      column(7,
      fluidRow(
        column(12, box(title='Num contratos por categoria', status = "primary", solidHeader = T,width = 12,plotlyOutput("barplot1", width = "100%")))
      ),
       fluidRow(
         
        column(12,
              box(title='outliers', status = "primary", solidHeader = T,width = 12,  plotlyOutput("box1")))
      )
      ),
      column(5,
            fluidRow( box(title='Expedientes de contrato unico', status = "primary", solidHeader = T,width = 12, dataTableOutput("datatable1"))),
            fluidRow(
              valueBoxOutput("outliersSuministros"),
              valueBoxOutput("outliersServicios")),
            fluidRow(
              valueBoxOutput("outliersObra"),
              valueBoxOutput("outliersPrivados"))
      
            )
      
      
     
      
      
   ),
    tabItem(
      tabName = "ContratosCercanos",
      
      fluidRow(
        valueBoxOutput("porcentajeTotalSS", width = 6),
        valueBoxOutput("porcentajeTotalObras", width = 6)
      ),
      
      fluidRow(
        column(6, box( title='Servicios y suministros', status = "primary", solidHeader = T,width = 12,plotlyOutput("scatter1", width = "100%"))),
        column(6,box( title='Obras', status = "primary", solidHeader = T,width = 12, plotlyOutput("scatter2", width = "100%")))
      ),
    
      fluidRow(
        column(width = 6,box(title = "Servicios y suministros", status = "primary", solidHeader = T,width = 12, dataTableOutput("datatable5"))),
        column(width = 6, box(title = "Obras", status = "primary", solidHeader = T,width = 12,dataTableOutput("datatable6")))
      )
      
    ),
    
    tabItem(
      tabName = "ContratosAgregados",
      
      fluidRow(
        column(6,box(title='Servicios y suministros', status = "primary", solidHeader = T,width = 12,plotlyOutput("scatter3", width = "100%"))),
        column(6,box(title='Obras', status = "primary", solidHeader = T,width = 12,plotlyOutput("scatter4", width = "100%")))
        
      ),
      fluidRow(
        column(8,box(title="Expedientes desagregados", status = "primary", solidHeader = T,width = 12,dataTableOutput("datatable7"))),
        column(4,box(title='Volumen respecto al total', status = "primary", solidHeader = T,width = 12,plotlyOutput("pie2")))
      )
    #  fluidRow(
    #    column(width = 3, dataTableOutput("datatable7")),
    #    column(width = 3, dataTableOutput("datatable8"))
    #  )
      
    ),
   tabItem(
     tabName = "Resultados",
     
    
     fluidRow(
       box(title = 'Ponderacion de resultados por expedientes', width = 12,status = "primary", height = "350",solidHeader = T,dataTableOutput("resultados")),
       #column(8,div(dataTableOutput("resultados")),style = "font-size: 75%; width: 45%"),
       column(4,sliderInput("slider1", "selecciona el umbral!", 
                           min = 1, 
                           max = 800,
                           value = c(1,800),
                           step = 3
       ))
     ),
     fluidRow(
       column(6,box(title='Importe de redflags por centro', status = "primary", solidHeader = T,width = 12, plotlyOutput("barplot5"))),
       column(6,box(title='Importe de posible fraude por centro', status = "primary", solidHeader = T,width = 12, plotlyOutput("barplot6")))
     )
     #  fluidRow(
     #    column(width = 3, dataTableOutput("datatable7")),
     #    column(width = 3, dataTableOutput("datatable8"))
     #  )
     
   )
    
    
  ))
)

# Define server logic required to draw a histogram
server <- function(input, output)
{
  #llamadas a SPARQL
  agregado <- concentracion()
  general <- descriptivo()
  

  nc <- numeroContratos(agregado)
  np <- numeroProveedores(agregado)
  vc <- valorContratos(agregado)
  
  avgCP <- vc / np
  avgCC <- vc / nc
  df <- volumenTipoContratos(general)
  df$medio <- df$suma / df$count

  outserv <- getOutliersServicios(agregado)
  outpriv <- getOutliersPrivados(agregado)
  outsum <- getOutliersSuministros(agregado)
  outoba <- getOutliersObras(agregado)
  df2 <- devolverContratos(agregado)
  
  print("datos cargadis")
  
  servCercanos <- getDFSumServCercanosUmbral(general)
  obrasCercanos <- getDFObrasCercanosUmbral(general)
  
  servCercanos %>% group_by(identifier, description) %>% summarise(cont =
                                                                     n(), suma = sum(value)) %>% filter(suma > 1)
  obrasCercanos %>% group_by(identifier, description) %>% summarise(cont =
                                                                      n(), suma = sum(value)) %>% filter(suma == 1)
  
  avgVCS <- sum(servCercanos$value) / valorContratosSumServ(general)
  avgVCO <- sum(obrasCercanos$value) / valorContratosSumServ(general)
  
  
  dfObrasAgregado<-getDFObrasAgregadas(agregado)
  dfSSAgregado<-getDFSumServAgregados(agregado)
  
  
  tmp1<-dfSSAgregado%>%union(dfObrasAgregado)
  
  tmp12<-general%>%inner_join(tmp1,by=c("identifier","description"))
  tmp12<-select(tmp12,contract,desc,description,identifier,value)
  tmp12$contract<-getExpediente(tmp12$contract)
  
  tmp2<-data.frame(c(sum(tmp1$sumavalor),sum(agregado$sumavalor)))
  colnames(tmp2)<-c("Volumen Agregado S/ Total")
 
  tmp3<-agregado %>%filter(numerodecontrataciones==1 )
  tmp4<-tmp3%>%inner_join(general,by=c("identifier","description",c("sumavalor"="value")))
  tmp4$contract<-getExpediente(tmp4$contract)
  tmp4<-getDFOutliers(tmp4)
  
 output$suma <- renderValueBox({
    valueBox(
      paste0(format(
        nc,
        digits = 2,
        big.mark = '.',
        decimal.mark = ','
      ), "#"),
      "# de Contratos",
      icon = icon("list"),
      color = "yellow"
    )
  })
  output$valor <- renderValueBox({
    valueBox(
      paste0(format(
        vc,
        digits = 2,
        big.mark = '.',
        decimal.mark = ','
      ), "K"),
      "Valor de los contratos",
      icon = icon("eur", lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$proveedores <- renderValueBox({
    valueBox(
      paste0(format(
        np,
        digits = 2,
        big.mark = '.',
        decimal.mark = ','
      ), ""),
      "Numero de proveedores",
      icon = icon("address-card"),
      color = "blue"
    )
  })
  
  output$valormdoedioprov <- renderValueBox({
    valueBox(
      paste0(
        format(
          avgCP,
          digits = 2,
          big.mark = '.',
          decimal.mark = ','
        ),
        ""
      ),
      "Valor medio proveedor",
      icon = icon("calculator"),
      color = "orange"
    )
  })
  
  output$valormediocont <- renderValueBox({
    valueBox(
      format(
        avgCC,
        digits = 2,
        big.mark = '.',
        decimal.mark = ','
      ),
      "Valor medio por contrato",
      icon = icon("calculator"),
      color = "green"
    )
  })
  
  output$outliersSuministros <- renderValueBox({
    valueBox(
      paste0(outsum, ''),
      "# outliers en contratos de suministros",
      icon = icon("new-window", lib = 'glyphicon'),
      color = "blue"
    )
  })
  
  
  output$outliersServicios <-
    renderValueBox({
      valueBox(
        paste0(outserv, ''),
        "# outliers en contratos de servicios",
        icon = icon("new-window", lib = 'glyphicon'),
        color = "green"
      )
    })
  output$outliersObra <-
    renderValueBox({
      valueBox(
        paste0(outoba, ''),
        "# outliers en contratos de obras",
        icon = icon("new-window", lib = 'glyphicon'),
        color = "orange"
      )
    })
  output$outliersPrivados <-
    renderValueBox({
      valueBox(
        paste0(outpriv, ''),
        "# outliers en contratos privados",
        icon = icon("new-window", lib = 'glyphicon'),
        color = "yellow"
      )
    })
  
  
  output$porcentajeTotalSS <-
    renderValueBox({
      valueBox(
        paste0(avgVCS, '%'),
        "Volumen cercano a umbral en Servicios y Suminstros",
        icon = icon("new-window", lib = 'glyphicon'),
        color = "yellow"
      )
    })
  output$porcentajeTotalObras <-
    renderValueBox({
      valueBox(
        paste0(avgVCO, '%'),
        "Volumen cercano a umbral en Servicios y Suminstros",
        icon = icon("new-window", lib = 'glyphicon'),
        color = "yellow"
      )
    })
  ##PLOTLY
  
  
  output$plot1 <- renderPlotly({
    plot_ly(
      x = agregado$identifier,
      y = agregado$sumavalor,
      type = "scattergl",
      mode = 'markers',
      marker = list(
        size = agregado$numerodecontrataciones,
        opacity = 0.5
      )
    ) %>% layout(
      title="Facturacion de proveedores",
      xaxis = list(
        title = "Proveedor",
        size = 5,
        showticklabels = FALSE
      ),
      yaxis = list(title = "Importe")
    )
    # ggplotly(qplot(geom = "point",
    #    x = agregado$identifier,
    #     y = agregado$sumavalor,color=agregado$identifier))
  })
  
  
  output$pie1 <- renderPlotly({
    plot_ly(
      df,
      labels =  ~ description,
      values =  ~ suma,
      type = "pie",
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text'
    )%>%layout(title="Volumen de contratos")
    #  plot_ly(agregado,labels=~description, values=~n,type="pie")
  })
  
  
  output$pie2 <- renderPlotly({
      plot_ly(
             tmp2,
             labels =  tmp2$`Volumen Agregado S/ Total`,
             values =  tmp2$`Volumen Agregado S/ Total`,
             type = "pie",
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text'
         )%>%layout(
           title = "Volumen agregado")
    #  plot_ly(agregado,labels=~description, values=~n,type="pie")
  })
  
  output$box1<-renderPlotly({
    plot_ly(y = tmp3$sumavalor, type = "box",color=tmp3$description) %>%layout(title="Outliers de facturacion en contratos unicos",xaxis= list(showticklabels = FALSE),legend = list(orientation = 'h'))
  })
  
  output$tabla1 <- renderTable(df, width = "50%", striped = TRUE)
  
  # servCercanos<-getDFSumServCercanosUmbral(general)
  #obrasCercanos<-getDFObrasCercanosUmbral(general)
  
  #servCercanos%>%group_by(identifier,description)%>%summarise(cont=n(),suma=sum(value))%>%filter(suma>1)
  #obrasCercanos%>%group_by(identifier,description)%>%summarise(cont=n(),suma=sum(value))%>%filter(suma==1)
  
  output$scatter1 <- renderPlotly({
    plot_ly(
      x = servCercanos$contract,
      y = servCercanos$value,
      type = "scattergl",
      mode = 'markers',
      text=paste("# Contratos",servCercanos$numerodecontrataciones),
      marker = list(size = 10,
                    opacity = 0.5)
    ) %>% layout(
      title="Importe de los contratos",
      xaxis = list(
        title = "Expedientes",
        size = 5,
        showticklabels = FALSE
      ),
      yaxis = list(title = "Importe")
    )
  })
  
  output$scatter2 <- renderPlotly({
    plot_ly(
      x = obrasCercanos$contract,
      y = obrasCercanos$value,
      type = "scattergl",
      mode = 'markers',
      text=paste("# Contratos",obrasCercanos$numerodecontrataciones),
      marker = list(size = 10,
                    opacity = 0.5)
    ) %>% layout(
      title="Importe de los contratos",
      xaxis = list(
        title = "Expedientes",
        size = 5,
        showticklabels = FALSE
      ),
      yaxis = list(title = "Importe")
    )
  })
  p1<-plot_ly(
    x = dfSSAgregado$identifier,
    y = dfSSAgregado$sumavalor,
    type = "scattergl",
    mode = 'markers',
    color=dfSSAgregado$numerodecontrataciones,
    text=paste("# Contratos",dfSSAgregado$numerodecontrataciones),
    marker = list(
      size = dfSSAgregado$numerodecontrataciones,
      opacity = 0.5
    )
  ) %>% layout(
    title="Facturacion agregada del proveedor",
    xaxis = list(
      title = "Proveedor",
      size = 5,
      showticklabels = FALSE
    ),
    yaxis = list(title = "Importe")
  )
  
  p2<- plot_ly(
    x = dfObrasAgregado$identifier,
    y = dfObrasAgregado$sumavalor,
    color=dfObrasAgregado$numerodecontrataciones,
    text=paste("# Contratos",dfObrasAgregado$numerodecontrataciones),
    type = "scattergl",
    mode = 'markers',
    marker = list(
      size = dfObrasAgregado$numerodecontrataciones,
      opacity = 0.5
    )
  ) %>% layout(title="Facturacion agregada del proveedor",
    xaxis = list(
      title = "Proveedor",
      size = 5,
      showticklabels = FALSE
    ),
    yaxis = list(title = "Importe"))
    
  #plotjoin1<-subplot(p1,p2)
  output$scatter3 <- renderPlotly(p1)
  output$scatter4<-renderPlotly(p2)


  output$barplot1 <-
    renderPlotly({
      plot_ly(
        df2,
        y =  ~ contar,
        x =  ~ numerodecontrataciones,
        type = 'bar',
        showscale = FALSE,
        text=paste("Tipo de contrato:",df2$description),
        
        color =  ~ description
      ) %>% layout(xaxis = list(title = "# Contratos por categoria"),
                   yaxis = list(title = "# Unicos", showticklabels = F),size=30)
    })
  output$barpie1 <-
    renderPlotly({
      plot_ly(
        df2[df2$description == 'Contrato de servicios',],
        y =  ~ contar,
        x =  ~ numerodecontrataciones,
        type = 'bar',
        showscale = FALSE,
        color =  ~ numerodecontrataciones
      ) %>% hide_colorbar()
    })
  output$barplot3 <-
    renderPlotly({
      plot_ly(
        df2[df2$description == 'Contrato de obras',],
        y =  ~ contar,
        x =  ~ numerodecontrataciones,
        type = 'bar',
        showscale = FALSE,
        color =  ~ numerodecontrataciones
      ) %>% hide_colorbar()
    })
  output$barplot4 <-
    renderPlotly({
      plot_ly(
        df2[df2$description == 'Contrato privado',],
        y =  ~ contar,
        x =  ~ numerodecontrataciones,
        type = 'bar',
        showscale = FALSE,
        color =  ~ numerodecontrataciones
      ) %>% hide_colorbar()
    })
  
  
  output$datatable1 <-
    renderDataTable(tmp4[,-4],options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
 
  output$datatable5 <-
    renderDataTable(select(servCercanos, identifier, value, contract),options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  output$datatable6 <-
    renderDataTable(select(obrasCercanos, identifier, value, contract),options = list(lengthMenu = c(5, 30, 50), pageLength = 5))

  
  patron1<-unique(tmp4$contract)
  patron2<-unique(c(servCercanos$contract,obrasCercanos$contract))
  patron3<-unique(c(tmp12$contract))
  
  allContracts<-unique(c(patron1,patron2,patron3))
  
  matriz<-matrix(nrow = length(unique(allContracts)),ncol=3)
  matriz[,1]<-c(patron1, rep("NO EXP", length(allContracts) - length(patron1)))
  matriz[,2]<-c(patron2, rep("NO EXP", length(allContracts) - length(patron2)))
  matriz[,3]<-c(patron3, rep("NO EXP", length(allContracts) - length(patron3)))
  
  output$datatable7 <-renderDataTable(tmp12,options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  resultados<-anotar(.jarray(matriz,dispatch = T))
  matrizPonderada<-matrizPonderadaExpedientes(resultados,c(3,2,1))
  matrizPonderada<-matrizPonderada%>%filter(total>3)
  brks <- quantile(matrizPonderada$total, probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
  #output$resultados<-renderDataTable(matrizPonderada,options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  minRowVal <- reactive({
    input$slider1[[1]]        #Retrieve row number that matches selected range on sliderInput
  })
  
  maxRowVal <- reactive({
   input$slider1[[2]]        #Retrieve row number that matches selected range on sliderInput
  })
  
  print(minRowVal)
  print(class(minRowVal))
  observeEvent(input$slider1, {
    print(minRowVal())
    print(maxRowVal())
    print(max(matrizPonderada$total))
    print(min(matrizPonderada$total))
   
    matrizPonderadaDT<-datatable( matrizPonderada%>%filter(total>minRowVal() & total<maxRowVal()),options = list(
      columnDefs = list(list(className = 'dt-center', targets = 5)),
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20)
    ))%>% formatStyle('total', backgroundColor = styleInterval(brks, clrs))
                      
    output$resultados <- DT::renderDataTable({
      matrizPonderadaDT
    })
    
  })
  output$barplot5<-renderPlotly(plot_ly(resultados,x=resultados$autoritydesc,y=resultados$value,type="bar",color=~redflag) %>% layout(xaxis = list(title = "Centro",tickangle=45,tickfont=list(family = "Arial, sans-serif",  size = 7)),margin = list(b = 150, l = 50), yaxis = list(title = "Importe", showticklabels = F,margin = list(b = 150, l = 50)),size=30))
  
  
  output$barplot6<-renderPlotly(plot_ly(resultados,x=resultados$autoritydesc,y=resultados$value,type="bar",color=~fraude) %>% layout(xaxis = list(title = "Centro",tickangle=45,tickfont=list(family = "Arial, sans-serif",  size = 7)),margin = list(b = 150, l = 50), yaxis = list(title = "Importe", showticklabels = F,margin = list(b = 150, l = 50)),size=30))
  
  }

# Run the application
shinyApp(ui = ui, server = server)
