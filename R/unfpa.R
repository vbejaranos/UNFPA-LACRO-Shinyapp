library(shiny)
library(tidyverse)
library(openxlsx)
library(RColorBrewer)
theme_set(theme_minimal())
rm(list = ls())
Pal.1 = Palb.1 = brewer.pal(n = 8, name = "RdPu")
Pal.2 = Palb.2 = brewer.pal(n = 8, name = "PuBu")
Palete = c(Pal.2[3],Pal.2[7],Pal.1[7],Pal.1[3])
Paleteb = c(Palb.2[8],Palb.2[2],Palb.1[8],Palb.1[2])
country = sort(c('Argentina','Brasil','Chile','Colombia','Costa Rica',
                 'Ecuador','Cuba','Guatemala','Mexico',
                 'Peru','Uruguay','Venezuela'))
TCMest = read.xlsx('Tablas.xlsx',sheet = 'TCMest')
TEM_M = read.xlsx('Tablas.xlsx',sheet = 'TEM_M')
Pir = read.xlsx('Tablas.xlsx',sheet = 'Pir')
CFR1 = read.xlsx('Tablas.xlsx',sheet = 'CFR1')
CFR2 = read.xlsx('Tablas.xlsx',sheet = 'CFR2')
TCM_std = read.xlsx('Tablas.xlsx',sheet = 'TCM std')
TCM = read.xlsx('Tablas.xlsx',sheet = 'TCM')
TEM = read.xlsx('Tablas.xlsx',sheet = 'TEM')
Piramide = read.xlsx('Tablas.xlsx',sheet = 'Piramide')
Piramide_mes = read.xlsx('Tablas.xlsx',sheet = 'Piramide mes')
Subnacional = read.xlsx('Tablas.xlsx',sheet = 'Subnacional')
Otras_causas = read.xlsx('Tablas.xlsx',sheet = 'Otras causas')
Exceso = read.xlsx('Tablas.xlsx',sheet = 'Exceso')
Pscore = read.xlsx('Tablas.xlsx',sheet = 'Pscore')
ExcesoCOL = read.xlsx('Tablas.xlsx',sheet = 'ExcesoCOL')
PscoreCOL = read.xlsx('Tablas.xlsx',sheet = 'PscoreCOL')
ExcesoHW = read.xlsx('Tablas.xlsx',sheet = 'ExcesoHW')
Pscore_edad_sexo = read.xlsx('Tablas.xlsx',sheet = 'Pscore edad_sexo')
ExcesoesHW = read.xlsx('Tablas.xlsx',sheet = 'ExcesoesHW')
Exceso_Subnacional = read.xlsx('Tablas.xlsx',sheet = 'Exceso Subnacional')
ExcesoSubCOL = read.xlsx('Tablas.xlsx',sheet = 'ExcesoSubCOL')


ui <- fluidPage(

    # Application title
    #titlePanel("UNFPA-LACRO  ConVERGE"),
    #{
    # add font
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Muli&family=Manrope&family=Exo&family=Quicksand&display=swap"),
    tags$style(type="text/css", "body {padding-top: 20px;font-family: 'Muli', sans-serif;}"),
    # add inline CSS
    tags$style(
        HTML(
            "

  
        i {
            color: #66A0C4;
            }
        i.contacticon {
            color: black;
        
        }

        i.exclamation {
            color: #ef7f1a;
            
        }

        #exclamation {
            display: inline-block;
            border-bottom: 1px dashed #66A0C4;
            padding: 0px 5px 0px 5px;
            margin-left: 5px;
        }

        #exclamation .tooltiptext {
            visibility: hidden;
            
            min-width: 300px;
            background-color: #66A0C4;
            color: #fff;
            text-align:justify;
            text-justify:left;
            border-radius: 6px;
            margin-left: 5px;
            padding: 15px 15px 15px 15px;
            box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
            font-weight: bold;
            /* Position the tooltip */
            position: absolute;
            z-index: 100;
        }

        #exclamation .tooltiptext.referencelevel{
            margin-top: -300px;            
        }

        #exclamation .tooltiptext.clickpointhelp{
            margin-left: -500px;            
        }

        #exclamation:hover .tooltiptext {
            visibility: visible;
            
        }

        i.infoletter {
            color: #66A0C4;
            
        }

        i.infoletter:hover {
            border-color:black;
            color: #ef7f1a;
            text-shadow: 2px 2px 5px black;            
        }
        #infosign{
            display: inline-block;
            border-bottom: 1px dashed #66A0C4;
            padding: 0px 5px 0px 5px;
            margin-left: 5px;
        }

        #infosign .tooltiptext {
            visibility: hidden;
            
            min-width: 300px;
            background-color: #66A0C4;
            color: #fff;
            text-align:justify;
            text-justify:left;
            border-radius: 6px;
            margin-left: 5px;
            padding: 15px 15px 15px 15px;
            box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
            font-weight: bold;
            /* Position the tooltip */
            position: absolute;
            z-index: 100;
        }

        #infosign .tooltiptext.referencelevel{
            margin-top: -300px;            
        }

        #infosign .tooltiptext.clickpointhelp{
            margin-left: -500px;            
        }

        #infosign:hover .tooltiptext {
            visibility: visible;
            
        }
        #variable {
         display: inline;
         margin: 0px;
         }

        i.social {
            color: #66A0C4;    
        }

        i.social:hover {
            color:  #ef7f1a;
            transform: scale(1.5);
        }
        
        i.paintbrush:hover {
            color:  #ef7f1a;
            transform: scale(1.5);
        }
        .control-label {
            display: inline-flex;
        }
        .irs-bar {
            border-top: 1px solid #66A0C4;
            border-bottom: 1px solid #66A0C4;
            background: #66A0C4;
        }
        .irs-from, .irs-to {
            background: #66A0C4;
        }
        a:link {
            color: #66A0C4;
            font-weight:bold;
            background-color: transparent;
            text-decoration: none;
        }
            a:hover {
            color: #ef7f1a;
            background-color: transparent;
            text-decoration: underline;
        }

        a.logo:hover {
            text-decoration: none;
        }
        a.iconlink:hover{
            text-decoration: none;
            transform: scale(1.5);
        }

        .sidebar{
            border-right: 1px solid #66A0C4;
            border-radius:1px;            
        }

        .col-sm-9{
            padding: 0px 3px 0px 3px;
        }

        #shiny-notification-welcome {
             position:fixed;
             top: calc(25%);
             left: calc(25%);
             box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
             color: black;
             max-width: 50%;
             opacity: 0.9;
             }

        #shiny-notification-userguide {
             position:fixed;
             top: calc(2%);
             left: calc(16.66%);
             box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
             color: black;
             max-width: 80vw;
             opacity: 1;
             
             }
             .checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
            
        )
    ),
    #}
    sidebarLayout(
        
        # sidebar
      sidebarPanel(
               # logo
               fluidRow(HTML('<center><a href="https://www.unfpa.org/es/node/23618" target="_blank"><img src="converge.png" style="max-width:65%;"></a></center>')),
               fluidRow(HTML('<center><h5 style="color: #2a4770;text-align:center;font-weight:bold;padding: 0px 5px 0px 5px">Connecting Vital Events Registration and Gender Equality</h5></center>')),
               # social links
               tags$p(style="text-align:center;margin-bottom:0px;", 
                      HTML("<b>Guía de usuario:</b>"),actionLink(style="margin-right:5px;",inputId="userguide", label=HTML(as.character(icon("book", class="social"))))),
               hr(style="background-color: #66A0C4;height: 3px;border-radius: 2px;margin-top:5px;margin-bottom:2px;"),
               # input selectors
               fluidRow(align = "center",
                        selectInput(inputId = "pais",
                                    label = HTML('País'),
                                    choices = country,
                                    selected = "Peru"),
               selectInput(inputId = "year",
                           label=HTML('<span style="color:black;">Año</span>','<span id="infosign">',
                                      as.character(icon("info", class="infoletter")),'<center><span class="tooltiptext"> Es el año de los datos de mortalidad general </span></center> </span>'),
                           choices = c('-' = 'none',2015:2020),
                           selected = 'none')),
               fluidRow(align="center",selectInput(inputId="variableM",
                                                   label=HTML('Mortalidad general','<span id="exclamation">',as.character(icon("exclamation", class="exclamation")),'<span class="tooltiptext"> Las tasas de mortalidad se presentan por 100000 habitantes.</span> </span>'),
                                                   
                                                   choices=c('-' = 'none',
                                                             "Tasa cruda de mortalidad estandarizada" = "TCMest",
                                                             "Tasa específica de mortalidad" = "TEM_M",
                                                             "Piramide de mortalidad" = "Pir"),
                                                   selected = 'none')
               ),
               fluidRow(align="center",selectInput(inputId="variableC",
                                                   label=HTML('Mortalidad COVID19','<span id="exclamation">',as.character(icon("exclamation", class="exclamation")),'<span class="tooltiptext"> Las tasas de mortalidad se presentan por 100000 habitantes.</span> </span>'),
                                                   
                                                   choices=c('-' = 'none',"Tasa de letalidad" = "CFR",
                                                             "Tasa cruda de mortalidad estandarizada" = "TCM std",
                                                             "Tasa cruda de mortalidad" = "TCM",
                                                             "Tasa específica de mortalidad" = "TEM",
                                                             "Piramide de mortalidad" = "Piramide",
                                                             "Piramide de mortalidad por mes" = "Piramide mes",
                                                             "Piramides de mortalidad subnacional" = "Subnacional"),
                                                   selected = 'none')
               ),
               fluidRow(align="center",selectInput(inputId="variableE",
                                                   label=HTML('Exceso de mortalidad'),
                                                   
                                                   choices=c('-' = 'none',
                                                             "Comparación mortalidad COVID19" = "Otras causas",
                                                             "Exceso de mortalidad por edad y sexo" = "ExcesoHW",
                                                             "Exceso de mortalidad global" = "Exceso",
                                                             "Exceso de mortalidad subnacional" = "Exceso Subnacional",
                                                             "P-scores global" = "Pscore",
                                                             "P-scores por edad y sexo" = "Pscorees"),
                                                   selected = 'none')
               ),
               checkboxGroupInput(inputId = "sexo",
                                  label = "Sexo",
                                  choiceNames = list("Mujeres","Hombres"), 
                                  choiceValues = list("MUJERES","HOMBRES"),
                                  inline = TRUE,
                                  selected = c("MUJERES","HOMBRES")
               ),
               # mapas
               fluidRow(align = "center",
                        HTML('<center><h5 style="color: #2a4770;text-align:center;font-weight:bold;padding: 0px 5px 0px 5px">Mapas</h5></center>'),
                        a(href="https://www.arcgis.com/apps/MapSeries/index.html?appid=6d06667778c94217a10ba0d6807de983",icon('globe', class="social"))
               ),
               hr(style="background-color: #66A0C4;height: 3px;border-radius: 2px;margin-top:5px;margin-bottom:2px;"),
               # data links
               HTML('<p style="text-align:center;"><span><b>Datos:</b></span><br/> XLSX <a class=iconlink href="https://github.com/vbejaranos/UNFPA-LACRO-Shinyapp/blob/main/Tablas.xlsx?raw=true" style="margin-right:5px;">',
                    as.character(icon("file-excel-o", class="social")),
                    '</a> Meta <a class=iconlink href="https://github.com/vbejaranos/UNFPA-LACRO-Shinyapp/blob/main/Data%20mapping.xlsx?raw=true" style="margin-right:5px;">', as.character(icon("file-excel-o", class="social")),'</a>
                    </a> Nota <a class=iconlink href="https://github.com/vbejaranos/UNFPA-LACRO-Shinyapp/blob/main/The%20Demographics%20of%20COVID%20-%20LACRO.pdf" style="margin-right:5px;">', as.character(icon("file-pdf-o", class="social")),'</a>
                    </p>'),
               hr(style="background-color: #66A0C4;height: 3px;border-radius: 2px;margin-top:5px;margin-bottom:5px;"),
               #logos
               fluidRow(align = "center",
                        a(href="https://covid19-map.unfpa.org/",img(src="unfpa.png", style="width:30%;", class="logo"),target="_blank", class="logo")
                        
               )
        ),


    #main 
    mainPanel(
           #title info
      fluidRow(align="center", 
               HTML('<h2 style="margin-top:0px;margin-bottom:0px;color: #2a4770;text-align:center;font-weight:bold;"> UNFPA LACRO - CONVERGE </h2>'),
               HTML("<span style='color:green;font-weight:bold;'> Repositorio Público GitHub ", '<a class=iconlink href="https://github.com/vbejaranos/UNFPA-LACRO-Shinyapp" target="_blank">', as.character(icon("github", class="social")), "</a></span><br/>"),
               HTML('<hr style="width=60%;background-color:#66A0C4;height:1px;border-radius:2px;margin: 0px 15px 5px 15px;">')
      ),
           
           
           # figure
           div(
               style = "margin-left: 300px,",

               plotOutput("plot", height = "400px"
                          # brush = brushOpts(
                          #     id = "plot1_brush"
                          # 
                          # )

               )
           )
           
      )
    )
)


server <- function(input, output,session) {
  # user's guide
  observeEvent(input$userguide,{
    showNotification(id="userguide",duration=NA, type="message",
                     ui=fluidRow(style="max-height: 80vh;overflow:auto;padding-left:15px;",
                                 HTML(
                                   "<div style='overflow-y: auto;'>   

        <h2 style='margin-top:0px;'> Guía de usuario </h2>
        
        <div>
        <span>A partir de la recolección de datos de los países de LAC está herramienta permite la visualización de algunos indicadores de mortalidad.</span></div>
        
        <div>
        <span> En la izquierda se encuentran los controles para seleccionar el país, año y tipo de indicador que se desea visualizar. </span>
        <div>
        <span> Basados en el país que se escoja, año e indicador en la derecha se va a mostrar la gráfica correspondiente.</span>
        </div>

        <div style='display:inline-block;padding-top:15px;'> <h6><b> Tener en cuenta</b></h6> </div>
        
        <div>
        <span> Para poder visualizar un nuevo indicador es necesario dejar el anterior en blanco (-) </span>
        <div>
        <span> El año solo hace referencia a los indicadores de mortalidad general </span>
        <div>
        <span> Algunos países no cuentan con todos los indicadores, en caso de error es por falta de datos</span>
        
        </div>
        <div style='display:inline-block;'> <h6><b> Hipervínculos </b></h6> </div>
        
        <div>
        <span> Los logos llevan directamente a las páginas de la iniciativa ConVerge y el tablero COVID por UNFPA</span>
        <div>
        <span> Los mapas llevan a la dirección de ArcGis que dispone características tanto cualitativas como cuantitativas en un mapa de la región de interes.</span>
        <div>
        <span> Los datos permiten la descarga de las tablas, metadatos y notas correspondientes a la información utilizada.</span>
        <div>
        <span> Finalmente el logo de Github lleva al repositorio público de esta herramienta.</span>
        </div>
        
        
        </div>

        </div>

      

        </div>
        ")
                     )
                     
                     
    )
    
  })
  
  observe({

    updateSelectInput(session,"year",
                      choices = c('-' = 'none',unique({TEM_M %>% filter(pais==input$pais)}$year)),
                      selected = 'none')
  }
  )
  
  output$plot <- renderPlot(
    if (input$variableM == 'TCMest'){
      if (input$year == 'none') {
        g = TCMest %>% filter(pais == input$pais) %>% 
          ggplot(aes(x = mes,y = est)) + 
          labs(x = 'Mes',y = 'TCM Estandarizada') +
          geom_line(aes(y = med),linetype = 2,color = 'black',size = 1) +
          scale_x_continuous(breaks = 1:12) + scale_y_continuous(n.breaks = 10) +
          theme(legend.position = 'bottom',axis.text = element_text(size =  16),
                axis.title = element_text(size = 18),
                legend.text = element_text(size = 16),
                legend.title = element_text(size = 16),
                strip.text.x = element_text(size = 18, face = "bold")) + 
          geom_line(aes(col = year),size = 1) + scale_color_discrete('Año')
      }else{
        g = TCMest %>% filter(pais == input$pais,year == input$year) %>% 
          ggplot(aes(x = mes,y = est)) + 
          geom_line(aes(),size = 1) +
          labs(x = 'Mes',y = 'TCM Estandarizada') +
          geom_line(aes(y = med),linetype = 2,color = 'black',size = 1) +
          scale_x_continuous(breaks = 1:12) + scale_y_continuous(n.breaks = 10) +
          theme(legend.position = 'bottom',axis.text = element_text(size =  16),
                axis.title = element_text(size = 18),
                legend.text = element_text(size = 16),
                legend.title = element_text(size = 16),
                strip.text.x = element_text(size = 18, face = "bold"))
      }
      g
      
    }else if (input$variableM == 'TEM_M'){
      if (input$year == 'none') {
        g = TEM_M %>% filter(pais == input$pais,sexo %in% input$sexo) %>% 
          ggplot(aes(x = mes,y = n)) + theme_minimal() +
          theme(axis.text = element_text(size =  20),
                axis.title = element_text(size = 20),
                legend.text = element_text(size = 16),
                legend.title = element_text(size = 16),
                strip.text = element_text(size = 20, face = "bold")) +
          facet_grid(sexo~edad,scales = 'free_y') + 
          scale_y_continuous(n.breaks = 10) +
          geom_line(aes(col = year),size = 1.5) + labs(x = 'Mes',y = 'TEM') +
          scale_color_discrete("Año")
      }else{
        g = TEM_M %>% filter(pais == input$pais,year == input$year,
                             sexo %in% input$sexo) %>% 
          ggplot(aes(x = mes,y = n)) + theme_minimal() +
          theme(axis.text = element_text(size =  20),
                axis.title = element_text(size = 20),
                legend.text = element_text(size = 16),
                legend.title = element_text(size = 16),
                strip.text = element_text(size = 20, face = "bold")) +
          facet_grid(sexo~edad,scales = 'free_y') + 
          scale_y_continuous(n.breaks = 10) +
          geom_line(aes(),size = 1) + labs(x = 'Mes',y = 'TEM')
      }
      g
      
      
    
    }else if(input$variableM == 'Pir'){
        Pir1 = Pir %>% filter(pais == input$pais)
        g = Pir1 %>% ggplot(aes(x = edad, y = POP, fill = Label, color = Label)) + 
          geom_bar(data = subset(Pir1, Label == "MUJERES 2020"), stat = "identity", width = 1.0, alpha = 1)  + 
          geom_bar(data = subset(Pir1, Label == "HOMBRES 2020"), aes(x = edad, y = -1*POP), stat = "identity", width = 1.0, alpha = 1)  + 
          geom_bar(data = subset(Pir1, Label == "MUJERES PROMEDIO"), stat = "identity", width = 1.0, alpha = 0.2)  + 
          geom_bar(data = subset(Pir1, Label == "HOMBRES PROMEDIO"), aes(x = edad, y = -1*POP), stat = "identity", width = 1.0, alpha = 0.2)  +
          coord_flip() +
          labs(y = "TEM", x = "Edad", color = "", fill = "") +
          scale_color_manual(values = c(Pal.2[c(2,8)],Pal.1[c(2,8)])) + 
          scale_fill_manual(values = c(Palb.2[c(8,1)],Palb.1[c(8,1)])) + 
          guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
          scale_y_continuous(n.breaks = 10,labels = function(x){x %>% abs}) +
          theme(legend.position = "bottom", legend.text = element_text(size = 14),
                plot.title = element_text(size = 20, face = "bold"),
                plot.subtitle  = element_text(size = 20), 
                axis.text = element_text(size =  20),
                axis.title = element_text(size = 16))
        g
      
    
    }else if(input$variableC == 'CFR'){
      g = CFR2 %>% filter(pais == input$pais) %>%
      ggplot(aes(x = time,y = cfrcum*100)) + 
        geom_line(aes(),size = 1) + 
        scale_y_continuous(n.breaks = 8) +
        theme(axis.text = element_text(size =  16),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 16),
              legend.text = element_text(size = 16),
              strip.text.x = element_text(size = 16,face = 'bold'),
              legend.key.size = unit(2,'line')) + 
        labs(x = 'Dia',y = '% tasa letalidad COVID19')
      g
    
    }else if(input$variableC == 'TCM std'){
      g = TCM_std %>% filter(pais == input$pais) %>%
        ggplot(aes(x = semana,y = PropEst)) +
        geom_line(aes(linetype = Std),size = 1) + 
        labs(x = 'Semana',y = 'COVID19 TCM estandarizada',linetype = '') +
        scale_linetype_manual(values = c(2,1,3)) + 
        theme(axis.text = element_text(size =  16),
              axis.title = element_text(size = 18),
              legend.position = 'bottom',
              strip.text = element_text(size = 18, face = "bold"),
              legend.text = element_text(size = 16),
              legend.key.size = unit(2,'line'))
      g
      
    
    }else if(input$variableC == 'TCM'){
      g = TCM %>% filter(pais == input$pais) %>%
        ggplot(aes(x = semana,y = Prop)) + theme_minimal() +
        theme(axis.text = element_text(size =  20),
              axis.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.title = element_text(size = 20),
              strip.text = element_text(size = 16,face = 'bold'),
              legend.key.size = unit(1.5,'line')) +
        geom_line(size = 1) + labs(x = 'Semana',y = 'TCM')
      g
    }else if(input$variableC == 'TEM'){
      g = TEM %>% filter(pais == input$pais,sexo %in% input$sexo) %>%
        ggplot(aes(x = semana,y = n)) +
        theme(axis.text = element_text(size =  16),
              axis.title = element_text(size = 20),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 16),
              strip.text = element_text(size = 20, face = "bold")) +
        facet_grid(sexo~edad,scales = 'free_y') + scale_y_continuous(n.breaks = 10) +
        geom_line(size = 1) + labs(x = 'Semana',y = 'TEM')
      g
    }else if(input$variableC == 'Piramide'){
      Piramide1 = Piramide %>% filter(pais == input$pais)
      g = ggplot(data = Piramide1,mapping = aes(x = edad, y = POP, fill = Label, color = Label)) + 
        geom_bar(data = subset(Piramide1, Label == "MUJERES"), stat = "identity", width = 1.0)  +
        geom_bar(data = subset(Piramide1, Label == "HOMBRES"), aes(x = edad, y = -1*POP), stat = "identity", width = 1.0)  + 
        coord_flip()  + labs(y = "TEM", x = "Edad", color = "", fill = "") +
        scale_fill_manual(values = c(Pal.2[7],Pal.1[7])) + 
        scale_color_manual(values = c(Palb.2[3],Palb.1[3])) + 
        scale_y_continuous(n.breaks = 10,labels = function(x){x %>% abs}) +
        theme(legend.position = "bottom", legend.text = element_text(size = 16),
              plot.title = element_text(size = 20, face = "bold"),
              plot.subtitle  = element_text(size = 12), 
              axis.text = element_text(size =  16),
              axis.title = element_text(size = 18),
              strip.text.x = element_text(size = 18, face = "bold")
        )
      g
    }else if(input$variableC == 'Piramide mes'){
      Piramide_mes1 = Piramide_mes %>% filter(pais == input$pais)
      g = ggplot(Piramide_mes1,aes(x = edad, y = POP, fill = Label, color = Label,alpha = mes, group = Pais)) + 
        facet_wrap(Pais~.,scales = 'free_x') +
        geom_bar(data = subset(Piramide_mes1, Label == "MUJERES"), stat = "identity", width = 1)  +
        geom_bar(data = subset(Piramide_mes1, Label == "HOMBRES"), aes(x = edad, y = -1*POP), stat = "identity", width = 1) +
        coord_flip() + labs(y = "TEM", x = "Edad", color = "", fill = "",alpha = "Mes") +
        scale_color_manual(values = c(Pal.2[c(3)],Pal.1[c(3)])) + 
        scale_fill_manual(values = c(Palb.2[c(8)],Palb.1[c(8)])) + 
        guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
        scale_y_continuous(n.breaks = 10,labels = function(x){x %>% abs}) +
        theme(legend.position = "bottom", 
              legend.text = element_text(size = 16),
              legend.title = element_text(face = 'bold',size = 14),
              plot.title = element_text(size = 20, face = "bold"),
              plot.subtitle  = element_text(size = 12), 
              axis.text = element_text(size =  20),
              axis.title = element_text(size = 20),
              strip.text.x = element_text(size = 16, face = "bold"))
      g
    }else if(input$variableC == 'Subnacional'){
      Subnacional1 = Subnacional %>% filter(pais == input$pais)
      g = ggplot(data = Subnacional1,mapping = aes(x = edad, y = POP, fill = Label, color = Label)) + 
        facet_wrap(level~.,scales = 'free_x') +
        geom_bar(data = subset(Subnacional1, Label == "MUJERES"), stat = "identity", width = 1.0)  +
        geom_bar(data = subset(Subnacional1, Label == "HOMBRES"), aes(x = edad, y = -1*POP), stat = "identity", width = 1.0)  + 
        coord_flip()  + labs(y = "TEM", x = "Edad", color = "", fill = "") +
        scale_fill_manual(values = c(Pal.2[7],Pal.1[7])) + 
        scale_color_manual(values = c(Palb.2[3],Palb.1[3])) + 
        scale_y_continuous(labels = function(x){x %>% abs},n.breaks = 5) +
        theme(legend.position = "bottom", legend.text = element_text(size = 16),
              plot.title = element_text(size = 20, face = "bold"),
              plot.subtitle  = element_text(size = 12), 
              axis.text = element_text(size =  16),
              axis.title = element_text(size = 18),
              strip.text.x = element_text(size = 18, face = "bold")
        )
      g
    }else if(input$variableE == 'Otras causas'){
      Otras_causas1 = Otras_causas %>% filter(pais == input$pais)
      g = ggplot(data = Otras_causas1,mapping = aes(x = edad, y = POP, fill = Label,group = causa,color = Label)) + 
        facet_grid(.~causa,scales = 'free_x') +
        geom_bar(data = subset(Otras_causas1, Label == "MUJERES"), stat = "identity", width = 1.0,position = 'dodge')  +
        geom_bar(data = subset(Otras_causas1, Label == "HOMBRES"), aes(x = edad, y = -1*POP), stat = "identity",position = 'dodge', width = 1.0)  + 
        coord_flip()  + labs(y = "TEM", x = "Edad", color = "", fill = "") +
        scale_fill_manual(values = c(Pal.2[7],Pal.1[7])) + 
        scale_color_manual(values = c(Palb.2[3],Palb.1[3])) + 
        scale_y_continuous(n.breaks = 6,labels = function(x){x %>% abs}) + 
        theme(legend.position = "bottom", legend.text = element_text(size = 16),
              plot.title = element_text(size = 20, face = "bold"),
              plot.subtitle  = element_text(size = 16), 
              axis.text = element_text(size =  16),
              axis.title = element_text(size = 16),
              strip.text = element_text(size = 16, face = "bold")
        )
      g
    }else if(input$variableE == 'Exceso'){
      if(input$pais == 'Colombia'){
        g = ggplot(ExcesoCOL,aes(x = semana,y = covid_deaths)) + 
          geom_area(aes(y = non_covid_deaths,fill = 'Otras muertes por\ncausas internas'),alpha = 0.3) + 
          geom_point(aes(col = 'Muertes\nCOVID19 observadas')) +
          geom_line(aes(col = 'Muertes\nCOVID19 observadas')) + theme_minimal() + 
          theme(axis.text = element_text(size =  16),
                strip.text = element_text(size = 16, face = "bold"),
                legend.text = element_text(size = 12),
                axis.title = element_text(size = 16)) +
          geom_line(aes(y = expected_deaths,linetype = 'Muertes esperadas\n (promedio)')) + 
          scale_fill_manual('',values = 'blue') + scale_color_manual('',values = 'red') +
          scale_linetype_manual('',values = 2) + labs(x = 'Semana',y = 'Conteo de muertes')
      }else{
        g = Exceso %>% filter(pais == input$pais) %>% 
          ggplot(aes(x = mes,y = covid_deaths)) + 
          geom_area(aes(y = non_covid_deaths,fill = 'Otras muertes por\ncausas internas'),alpha = 0.3) + 
          geom_point(aes(col = 'Muertes COVID19\n observadas')) +
          geom_line(aes(col = 'Muertes COVID19\n observadas')) + 
          theme(axis.text = element_text(size =  16),
                strip.text = element_text(size = 16, face = "bold"),
                legend.text = element_text(size = 12),
                axis.title = element_text(size = 16)) +
          geom_line(aes(y = expected_deaths,linetype = 'Muertes esperadas\n (promedio)')) + 
          scale_fill_manual('',values = 'blue') + scale_color_manual('',values = 'red') +
          scale_linetype_manual('',values = 2) + labs(x = 'Mes',y = 'Conteo de muertes')
      }
      g
    }else if(input$variableE == 'ExcesoHW'){
      g = ExcesoesHW %>% filter(pais == input$pais,sexo %in% input$sexo) %>% 
        ggplot(aes(x = mes,y = covid_deaths)) + 
        facet_grid(sexo~edad,scales = 'free') +
        geom_area(aes(y = non_covid_deaths,fill = 'Otras muertes por\ncausas internas'),alpha = 0.3) + 
        geom_line(aes(y = promedio, linetype = 'Muertes esperadas\nPromedio')) +
        geom_line(aes(y = HW, linetype = 'Muertes esperadas\nHolt-Winters')) +
        geom_ribbon(aes(ymin = lwr,ymax = upr),alpha = 0.2,col = 'gray') +
        geom_point(aes(col = 'Muertes COVID19\n observadas')) +
        geom_line(aes(col = 'Muertes COVID19\n observadas')) + 
        theme(axis.text = element_text(size =  16),
              strip.text = element_text(size = 16, face = "bold"),
              legend.text = element_text(size = 12),
              axis.title = element_text(size = 16)) +
        scale_fill_manual('',values = 'blue') + 
        scale_color_manual('',values = 'red') +
        scale_linetype_manual('',values = c(2,1)) +
        labs(x = 'Mes',y = 'Conteo de muertes',linetype = 'Muertes esperadas')
      g
    }else if(input$variableE == 'Exceso Subnacional'){
      if(input$pais == 'Colombia'){
        g = ExcesoSubCOL %>% filter(pais == 'Colombia') %>% 
          ggplot(aes(x = semana,y = covid_deaths)) + facet_wrap(level~.,scales = 'free') +
          geom_line(aes(y = promedio, linetype = 'Muertes esperadas\nPromedio')) +
          geom_line(aes(y = HW, linetype = 'Muertes esperadas\nHolt-Winters')) +
          geom_ribbon(aes(ymin = lwr,ymax = upr),alpha = 0.2,col = 'gray') +
          geom_point(aes(col = 'Muertes COVID19\n observadas')) +
          geom_line(aes(col = 'Muertes COVID19\n observadas')) + 
          theme(axis.text = element_text(size =  16),
                strip.text = element_text(size = 16, face = "bold"),
                legend.text = element_text(size = 12),
                axis.title = element_text(size = 16)) +
          scale_fill_manual('',values = 'blue') + 
          scale_color_manual('',values = 'red') +
          scale_linetype_manual('',values = c(2,1)) +
          labs(x = 'Semana',y = 'Conteo de muertes',linetype = '')
        
      }else{
        g = Exceso_Subnacional %>% filter(pais == input$pais) %>% 
          ggplot(aes(x = mes,y = covid_deaths)) + facet_wrap(level~.,scales = 'free') +
          geom_area(aes(y = non_covid_deaths,fill = 'Otras muertes por\ncausas internas'),alpha = 0.3) + 
          geom_line(aes(y = promedio, linetype = 'Muertes esperadas\nPromedio')) +
          geom_line(aes(y = HW, linetype = 'Muertes esperadas\nHolt-Winters')) +
          geom_ribbon(aes(ymin = lwr,ymax = upr),alpha = 0.2,col = 'gray') +
          geom_point(aes(col = 'Muertes COVID19\n observadas')) +
          geom_line(aes(col = 'Muertes COVID19\n observadas')) + 
          theme(axis.text = element_text(size =  16),
                strip.text = element_text(size = 16, face = "bold"),
                legend.text = element_text(size = 12),
                axis.title = element_text(size = 16)) +
          scale_x_continuous(breaks = seq(1,12,2)) +
          scale_fill_manual('',values = 'blue') + 
          scale_color_manual('',values = 'red') +
          scale_linetype_manual('',values = c(2,1)) +
          labs(x = 'Mes',y = 'Conteo de muertes',linetype = 'Muertes esperadas')
        
      }
      g
    }else if(input$variableE == 'Pscore'){
      if(input$pais == 'Colombia'){
        g = PscoreCOL %>%
          ggplot(aes(x = semana,y = pscore)) +
          geom_point() + geom_line(size = 1) + theme_minimal() +
          theme(axis.text = element_text(size =  16),
                strip.text = element_text(size = 16, face = "bold"),
                legend.text = element_text(size = 12),
                axis.title = element_text(size = 16)) +
          labs(x = 'Mes',y = 'P-score')
      }else{
        g = Pscore %>% filter(pais == input$pais) %>%
          ggplot(aes(x = mes,y = pscore)) +
          geom_point() + geom_line(size = 1) + theme_minimal() +
          theme(axis.text = element_text(size =  16),
                strip.text = element_text(size = 16, face = "bold"),
                legend.text = element_text(size = 12),
                axis.title = element_text(size = 16)) +
          labs(x = 'Mes',y = 'P-score')
        
      }
      g
    }else if (input$variableE == 'Pscorees'){
      g = Pscore_edad_sexo %>% filter(pais == input$pais,sexo %in% input$sexo) %>%
        ggplot(aes(x = mes,y = pscore)) +
        geom_point() + geom_line() + theme_minimal() +
        theme(axis.text = element_text(size =  16),
              strip.text = element_text(size = 16, face = "bold"),
              legend.text = element_text(size = 12),
              axis.title = element_text(size = 16)) +
        facet_grid(sexo~edad,scales = 'free_y') +
        labs(x = 'Mes',y = 'Conteo de muertes')
      g
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
    