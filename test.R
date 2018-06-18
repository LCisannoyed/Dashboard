library(shiny)
library(DT)
library(TAM)
library(tidyverse)
library(WrightMap)
library(ggplot2)

# load functions
source("0. functions.R")


ui <- fluidPage(
  
  title = 'Select Table Rows',
  
  h1('Wright map'),
  
  fluidRow(
    column(6, DT::dataTableOutput('itemStats')),
    column(6, plotOutput('itemStatsPlot', height = 500))
  ),

 #   fluidRow(
  #  column(6, DT::dataTableOutput('x1')),
   # column(6, plotOutput('x2', height = 500))
#  ),
  
  hr(),
  
  h1('A Server-side Table'),
  
  fluidRow(
    column(9, DT::dataTableOutput('x3')),
    column(3, verbatimTextOutput('x4'))
  )
  
)

server <- function(input, output, session) {
  
  output$x1 = DT::renderDataTable(cars, server = FALSE)
  
  # highlight selected rows in the scatterplot
  output$x2 = renderPlot({
    s = input$x1_rows_selected
    par(mar = c(4, 4, 1, .1))
    plot(cars)
    if (length(s)) points(cars[s, , drop = FALSE], pch = 19, cex = 2)
  })
  
  # server-side processing
  mtcars2 = mtcars[, 1:8]
  output$x3 = DT::renderDataTable(mtcars2, server = TRUE)
  
  # print the selected indices
  output$x4 = renderPrint({
    s = input$x3_rows_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  
  # load sample Data from TAM
  data(data.cqc01)
  dat <- data.cqc01
  names(dat) <- gsub("BSM","",names(dat))
  
  # start Tam analysis
  mod01 <- TAM::tam(dat, item.elim=FALSE)
  
  itemStats<-cbind(mod01$item,mod01$xsi)  %>%
    select(item , N , M , xsi , se.xsi) %>% 
    mutate(M=round(100*.$M,1),
           xsi= round(.$xsi,3),
           se.xsi = round(.$se.xsi,3))  
  
  names(itemStats) <- c("item","N","facility","threshold","se")
  
  # Ability estimate - Weighted Likelihood Estimate
  Abil <- TAM::tam.wle(mod01)
  ## @knitr wrightmap
  thr <- TAM::tam.threshold(mod01)
  
  # item stats
  output$itemStats <- DT::renderDataTable(
    itemStats,
    options = list(
      pageLength = 30,
      scrollY = '600px',
      scrollX = TRUE,
      paging = FALSE,
      autoWidth = TRUE,
      columnDefs = list(list(
        width = '10px', targets = "_all"
      ))
    ),
    rownames = TRUE,
    selection = 'multiple',
    filter = 'top') 
  
  # highlight selected rows in the scatterplot
  output$itemStatsPlot <- renderPlot({
   s <-  input$itemStats_rows_selected
    par(mar = c(4, 4, 1, .1))
    thrWithBins<-item.side.LC(thr)
    
#    plot(x=thrWithBins$xpoint, y = thrWithBins$ypoint, type="p")
     itemsideplot<-ggplot(thrWithBins, aes(xpoint, ypoint, label=item)) + 
       geom_point(shape = ".")+
       geom_text(aes(label=item),hjust=0, vjust=0,size=8) +
       labs(x = "", y= "") + 
       coord_cartesian(xlim = c(1, max(thrWithBins$xpoint)+1)) +
       theme(
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         panel.background = element_rect(fill="white",colour = "black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
     itemsideplot
     
   if (length(s)>0) {
     highlighted<-thrWithBins[s,]
     itemsideplot+
       geom_text(data=highlighted,aes(label=item),hjust=0, vjust=0 ,colour="red",size=8)
     #geom_point(data=highlighted,size=5,colour="red") #points(thrWithBins[s, , drop = FALSE], pch = 19, cex = 2)
   }else{
     itemsideplot
     }
  })
  
}

shinyApp(ui, server)