######################################
# mtha app - ec analysis
# version v3 - correc bug alm 
# steps
# leer directorio - done
# agregar fichero - done
# Agregar datos sup - done
# Filtrar criterias - done
# filtrar alm - done
# generar tabla capitales - done

######################################

library(shiny)
library(dplyr)
library(tidyr)
library(DBI)
library(DT)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(readxl)
library(ECMkt)

options(scipen =9999, digits=2, OutDec=",", encoding = 'ISO-8859-1')


#source("source_mtha_mod.r")
#source("source_mod2.r")


ui <- fluidPage(
  
  titlePanel(div(img(src="marta.jpg",height=50), img(src="logo.png",height=50))),
  
  # Options to select information
  navbarPage("MARTHA- MARKET-ALM",
             tabPanel("HERRAMIENTAS",
             sidebarLayout(
                  sidebarPanel(
                          h3(),
                          fileInput("file1", "Fichero Criterias"),
                          h3(),
                          fileInput("file2", "Fichero ALM"),
                          h3(),
                          actionButton("but1", "Agrupar vectores"),
                          h3(),
                          actionButton("but2", "Exportar file csv"),
                          h3(),
                          actionButton("but3", "Exportar adj csv"),
                          h3(),
                          width=3, style="color: #fff; background-color: #cb3234; border-color: #2e6da4"
                        ),
                        mainPanel(
                        tabsetPanel(  
                          tabPanel("TOOLS",     
                          h3("Nuevos Criterias"),
                          h3(),
                          tableOutput("nuevos_criterias"),
                          h3(),
                          h3("Resumen"),
                          tableOutput("resumen1")
                          
                        ),
                        # Show information
                        tabPanel("ALM-CAPITAL",
                        h3("ALM"),
                        h3(),
                        tableOutput("criterias"),
                        h3("Resumen"),
                        h3(),
                        tableOutput("resumen")
                      )
                      )           
                      )
                            )
          ), tabPanel("COMPARATIVA",
                      sidebarLayout(
                        sidebarPanel(
                          h3(),
                          fileInput("file3", "CSV Mes Actual"),
                          h3(),
                          fileInput("file4", "CSV Mes Anterior"),
                          h3(),
                          fileInput("file5", "Fichero Comentarios"),
                          h3(),
                          width=3, style="color: #fff; background-color: #cb3234; border-color: #2e6da4"
                        ), mainPanel(
                          tabsetPanel(  
                            tabPanel("ALM",
                                h3("Analisis"),
                                h3(),
                                uiOutput("prueba"),
                                h3(),
                                plotOutput("alm_graph1")
                                
                            ),
                            # Show information
                            tabPanel("Trading",
                                     h3("Analisis"),
                                     h3(),
                                     uiOutput("prueba1"),
                                     h3(),
                                     plotOutput("tra_graph1")
                            ),
                            tabPanel("Equity Non Trading",
                                     h3("Analisis"),
                                     h3(),
                                     uiOutput("prueba2"),
                                     h3(),
                                     plotOutput("rv_graph1")
                            ),
                            tabPanel("FX Estructural",
                                     h3("Analisis"),
                                     h3(),
                                     uiOutput("prueba3"),
                                     h3(),
                                     plotOutput("fx_graph1")
                            )
                            
                        ))
    )
)
)
)
server <- function(input, output) {
  
 
  ##################################################################################################
  # HERRAMIENTAS
  ##################################################################################################
  
  
  crit<- reactive({
                  read.csv2(input$file1$datapath, stringsAsFactors =F)
                  })
  alm_a<- reactive({
    read.csv2(input$file2$datapath, stringsAsFactors =F)
  })
   
  # Agregador de ficheros
  
  vectores<- eventReactive(input$but1, {
    vectum<- data.frame()
    vectores<- data.frame()
    filetxt <- data.frame()
    dir<-choose.dir()
    setwd(dir)
    txtfiles <- as.vector(list.files(dir))
    
    #length(txtfiles)
    
    for (i in 1:length(txtfiles)) {
      # output vectores
      filetxt<-read.csv2(txtfiles[i],  header=F, stringsAsFactors = F)
      filetxt<- filetxt[,1:8]
      filetxt<-na.omit(filetxt)
      vectum<- rbind(vectum, as.data.frame(filetxt))
    }
    
    vectores<- as.data.frame(vectum)
    
    
  })
  
  
  totv <- reactive({
    totv<-left_join(as.data.frame(vectores()),crit(), by=c("V4"="V"))
    
  })
  
  alm <- reactive({
    alm <- totv() %>% filter(Tipologia_3=='ALM ')
    
  })
  
  # Pantalla 1
  
  nc<- reactive({
      nc<-totv() %>% group_by(V4,Tipologia_3) %>% filter(is.na(Tipologia_3))
      nc<- unique(nc$V4)
      
  })
  
  output$nuevos_criterias <- renderTable({
    nc()
  })
  
  rm <- reactive({
      rm_<- data.frame()
      cd <- unique(vectores()$V4)
      for (vc in cd) {
        asm <- vectores() %>% filter(V4 %in% vc )
        ad_rm <- as.data.frame(t(c(vc, nrow(asm), mean(as.numeric(as.character(asm$V6))),sd(as.numeric(as.character(asm$V6))), quantile(as.numeric(as.character(asm$V6)),0.01))))
        rm_ <- rbind(rm_,ad_rm)
      }
      colnames(rm_)<- c("Vector", "Filas", "Media", "Desviacion", "Percentil_9990")
      rm<- as.data.frame(rm_)
  })
  
  output$resumen1<- renderTable({
      rm()
  })
  
  # Pantalla 2 
  
 
  
  tot_a <- reactive({
      tot_a<- anti_join(totv(),alm())
      alm_a <- alm() %>% filter(V4 %in% alm_cap_aj()$vector)
      tot_a <- rbind(tot_a,alm_a)
      tot_a <- tot_a[,-c(9:11)]
      
  })    
  
  
  
  # exportar a csv
  
  observeEvent(input$but2, {
    write.csv2(as.data.frame(vectores()), file.choose(), row.names = F)
  })
  
  
  # exportar ajustado
  
  observeEvent(input$but3, {
    write.csv2(as.data.frame(tot_a()), file.choose())
  })





alm_cap_aj <- reactive({
  alm_cap_ <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(alm_cap_) <- c("entity", "mve", "nim", "vector")
  vector<- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(vector) <- c("entity", "mve", "nim", "vector")
  
  for (i in 1:nrow(alm_a())) {
    mve <- alm() %>% filter(V4== as.character(alm_a()[i,3]))
    print(as.character(alm_a()[i,3]))
    nim <- alm() %>% filter(V4== as.character(alm_a()[i,4]))
    print(as.character(alm_a()[i,4]))
    if (nrow(mve)!=0) {
      dist_mve<- fit_skewt(as.numeric(as.character(mve[['V6']])),as.numeric(as.character(alm_a()[i,2])))
      capital_mve<- qst(0.0005, dp= get_param(dist_mve))
    } else {
      capital_mve <-0
    }
      
    if (nrow(nim)!=0) {
      dist_nim<- fit_skewt(as.numeric(as.character(nim[['V6']])),as.numeric(as.character(alm_a()[i,2])))
      capital_nim<- qst(0.0005, dp= get_param(dist_nim))
    } else {
      capital_nim <-0
    }  
      
      if (capital_mve<capital_nim) {
        vcc <- as.character(alm_a()[i,3])
      } else {
        vcc <- as.character(alm_a()[i,4])
      }
      vector <- as.data.frame(t(c(as.character(alm_a()[i,1]), capital_mve, capital_nim, vcc)))
      alm_cap_<- rbind(alm_cap_, vector)
    }
  colnames(alm_cap_) <- c("entity", "mve", "nim", "vector")
  format(alm_cap_, big.mark=".", justify =c("right"))
  alm_cap_aj<- as.data.frame(alm_cap_)
})


 
output$criterias <- renderTable({
  alm_cap_aj()
})

output$resumen<- renderTable({
  tot_a()
})

##################################################################################################
# HERRAMIENTAS
##################################################################################################

mes<- reactive({
  read.csv2(input$file3$datapath, header=F)
})
mes_a<- reactive({
  read.csv2(input$file4$datapath, header=F)
})


agr <- reactive({
          agr <- left_join(mes(),crit(), by=c("V4"="V"))
})

agr_a <- reactive({
  agr_a <- left_join(mes_a(),crit(), by=c("V4"="V"))
})

##################################################################################################
# ALM
##################################################################################################


alm_agr <- reactive({
  alm_agr<- agr() %>% filter(Tipologia_3=='ALM ')
})


alm_agr_a <- reactive({
  alm_agr_a<- agr_a() %>% filter(Tipologia_3=='ALM ')
})

#output$prueba <- renderUI({
#                cd <- unique(alm_agr()$V4)
#                for (vc in cd) {
#                  asm <- alm_agr() %>% filter(V4 %in% vc )
#                  asm_a <- alm_agr_a() %>% filter(V4 %in% vc )
#                  filet<- rbind(asm, asm_a)
#                  output$plot_test<- renderPlot({
#                    h<-ggplot(filet, aes(x=V5, y=V6,colour=V1))+ geom_line()
#                  })
#                  plotOutput("plot_test")
#                }  
#})

output$prueba<- renderUI({ selectInput("buvert", "Unit:", choices=unique(alm_agr()$V4))})



output$alm_graph1 <- renderPlot({
                     asm <- alm_agr() %>% filter(V4 %in% input$buvert )
                     asm_a <- alm_agr_a() %>% filter(V4 %in% input$buvert )
                     filet<- rbind(asm, asm_a)
                     ggplot(filet, aes(x=V5, y=V6,colour=V1,group=2))+ geom_line() + scale_y_discrete(labels = NULL)+ scale_x_discrete(labels = NULL)  +labs(title = input$buvert)
})

##################################################################################################
# TRADING
##################################################################################################


tra_agr <- reactive({
  tra_agr<- agr() %>% filter(Tipologia_3=='Trading')
})


tra_agr_a <- reactive({
  tra_agr_a<- agr_a() %>% filter(Tipologia_3=='Trading')
})

#output$prueba <- renderUI({
#                cd <- unique(alm_agr()$V4)
#                for (vc in cd) {
#                  asm <- alm_agr() %>% filter(V4 %in% vc )
#                  asm_a <- alm_agr_a() %>% filter(V4 %in% vc )
#                  filet<- rbind(asm, asm_a)
#                  output$plot_test<- renderPlot({
#                    h<-ggplot(filet, aes(x=V5, y=V6,colour=V1))+ geom_line()
#                  })
#                  plotOutput("plot_test")
#                }  
#})

output$prueba1<- renderUI({ selectInput("buvert1", "Unit:", choices=unique(tra_agr()$V4))})



output$tra_graph1 <- renderPlot({
  tsm <- tra_agr() %>% filter(V4 %in% input$buvert1 )
  tsm_a <- tra_agr_a() %>% filter(V4 %in% input$buvert1 )
  filet1<- rbind(tsm, tsm_a)
  ggplot(filet1, aes(x=V5, y=V6,colour=V1,group=2))+ geom_line() + scale_y_discrete(labels = NULL)+ scale_x_discrete(labels = NULL)  +labs(title = input$buvert1)
})

##################################################################################################
# RV
##################################################################################################


rv_agr <- reactive({
  rv_agr<- agr() %>% filter(Tipologia_3=='RV')
})


rv_agr_a <- reactive({
  rv_agr_a<- agr_a() %>% filter(Tipologia_3=='RV')
})

#output$prueba <- renderUI({
#                cd <- unique(alm_agr()$V4)
#                for (vc in cd) {
#                  asm <- alm_agr() %>% filter(V4 %in% vc )
#                  asm_a <- alm_agr_a() %>% filter(V4 %in% vc )
#                  filet<- rbind(asm, asm_a)
#                  output$plot_test<- renderPlot({
#                    h<-ggplot(filet, aes(x=V5, y=V6,colour=V1))+ geom_line()
#                  })
#                  plotOutput("plot_test")
#                }  
#})

output$prueba2<- renderUI({ selectInput("buvert2", "Unit:", choices=unique(rv_agr()$V4))})



output$rv_graph1 <- renderPlot({
  rsm <- rv_agr() %>% filter(V4 %in% input$buvert2 )
  rsm_a <- rv_agr_a() %>% filter(V4 %in% input$buvert2 )
  filet2<- rbind(rsm, rsm_a)
  ggplot(filet2, aes(x=V5, y=V6,colour=V1,group=2))+ geom_line() + scale_y_discrete(labels = NULL)+ scale_x_discrete(labels = NULL)  +labs(title = input$buvert2)
})


##################################################################################################
# FX Estructural
##################################################################################################


fx_agr <- reactive({
  fx_agr<- agr() %>% filter(Tipologia_3=='FX Estructural')
})


fx_agr_a <- reactive({
  fx_agr_a<- agr_a() %>% filter(Tipologia_3=='FX Estructural')
})

#output$prueba <- renderUI({
#                cd <- unique(alm_agr()$V4)
#                for (vc in cd) {
#                  asm <- alm_agr() %>% filter(V4 %in% vc )
#                  asm_a <- alm_agr_a() %>% filter(V4 %in% vc )
#                  filet<- rbind(asm, asm_a)
#                  output$plot_test<- renderPlot({
#                    h<-ggplot(filet, aes(x=V5, y=V6,colour=V1))+ geom_line()
#                  })
#                  plotOutput("plot_test")
#                }  
#})

output$prueba3<- renderUI({ selectInput("buvert3", "Unit:", choices=unique(fx_agr()$V4))})



output$fx_graph1 <- renderPlot({
  fxsm <- fx_agr() %>% filter(V4 %in% input$buvert3 )
  fxsm_a <- fx_agr_a() %>% filter(V4 %in% input$buvert3 )
  filet2<- rbind(fxsm, fxsm_a)
  ggplot(filet2, aes(x=V5, y=V6,colour=V1,group=2))+ geom_line() + scale_y_discrete(labels = NULL)+ scale_x_discrete(labels = NULL)  +labs(title = input$buvert3)
})


}

# Run the application 
shinyApp(ui = ui, server = server)
