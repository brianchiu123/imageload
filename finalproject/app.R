#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(reshape2)
library(tidyverse)
library(corrplot)
library(caret)
library(shiny)
library(shinythemes)
library(gplots)

# Define UI for application that draws a histogram
ui <- fluidPage(
    shinyUI(navbarPage("Final Project",theme = shinytheme("cosmo"),
        navbarMenu("Data Processing", icon = icon("far fa-angry"),
            tabPanel("I",fluid = TRUE,
                sidebarLayout(
                    sidebarPanel(
                        radioButtons(inputId = "dens",
                                    label = "Density",
                                    choices = c("Yes" = 1, "No" = 2),
                                    selected = 2)
                    ),
                    
                    mainPanel(
                       plotOutput("houseprice")
                    )
                )
            ),
            tabPanel("II correlation picture",fluid = TRUE,
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("cor", "correlation type :",
                                         c("Type I" = 1,
                                           "Type II" = 2,
                                           "Type III" = 3)),
                         ),
                         
                         mainPanel(
                             plotOutput("correlation")
                         )
                     )
            ),
            tabPanel("Plot",fluid = TRUE,
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("plot_i", "Select feature :",
                                     c("crim" = 1,
                                       "zn" = 2,
                                       "indus" = 3,
                                       "chas" = 4,
                                       "nox" = 5,
                                       "rm" = 6,
                                       "age" = 7,
                                       "dis" = 8,
                                       "rad" = 9,
                                       "tax" = 10,
                                       "ptratio" = 11,
                                       "black" = 12,
                                       "lstat" = 13)),
                       ),
                       mainPanel(
                         plotOutput("plot")
                       )
                     )
            ),
            tabPanel("Impoertance",fluid = TRUE,
                     sidebarLayout(
                       sidebarPanel(
                       ),
                       
                       mainPanel(
                         uiOutput("imp")
                       )
                     )
            ),
        
        ) 
        
        
)
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    get_upper_tri <- function(CorMat){
        CorMat[upper.tri(CorMat)]<- NA
        return(CorMat)
    }
    
    get_lower_tri <- function(CorMat){
        CorMat[lower.tri(CorMat)]<- NA
        return(CorMat)
    }
    
    reorder <- function(CorMat){
        dd <- as.dist((1-CorMat)/2)
        hc <- hclust(dd)
        CorMar <- CorMat[hc$order, hc$order]
    }
    Boston <- read.csv("train.csv")
    df<-data.frame(Boston)
    df$ID <- NULL
    print(df)
    summary(df)
    
    CorMat <- cor(df[ ,c("crim","zn","chas","nox","rm","age","dis","rad","tax","ptratio","black","lstat","medv")])
  


    
    output$houseprice <- renderPlot({
        library(ggplot2)
        if (input$dens == 2){
            houseprice <- ggplot(df, aes(x=medv)) 
            houseprice <- houseprice +   geom_histogram(colour="black")+ 
                scale_fill_brewer(palette = "Pastel1")+
                labs(y="count", x = "price($1000)")
            
            print(houseprice)
            
        }else{
            housepricedense<-ggplot(df, aes(x=medv)) + 
                geom_histogram(aes(y=..density..), colour="black")+
                geom_density(alpha=.2, fill="#FF6666")+
                scale_fill_brewer(palette = "Pastel1")
            print(housepricedense + labs(y="density", x = "price($1000)"))
        }
        
    })
    output$correlation <- renderPlot({
        CorMat <- reorder(CorMat)
        upper_tri <- get_upper_tri(CorMat)
        lower_tri <- get_lower_tri(CorMat)
        meltNum <- melt(lower_tri, na.rm = T)
        meltColor <- melt(upper_tri, na.rm = T)
        g <- ggplot() +
            labs(x = NULL, y = NULL) +
            geom_tile(data = meltColor, 
                      mapping = aes(Var2, Var1, 
                                    fill = value)) +
            geom_text(data = meltNum,
                      mapping = aes(Var2, Var1,
                                    label = round(value, digit = 2))) +
            scale_x_discrete(position = "top") +
            scale_fill_gradient(low = "white", high = "firebrick4",
                                limit = c(-1,1), name = "Pearson\nCorrelation") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank())
        if (input$cor == 1){
            print(g)
        }
        else if (input$cor == 2){
            print(ggcorrplot::ggcorrplot(cor(df), type = "lower", lab = TRUE))
        }
        else if (input$cor == 3){
            print(ggcorrplot::ggcorrplot(cor(df), lab = TRUE))
        }
        
    })
    output$plot <- renderPlot({
      
      i = as.numeric(input$plot_i)

        fearturescatter <-ggplot(df, aes(x=df[,i], y=df[,14])) +
          geom_point() + 
          labs(y="medv", x = colnames(df)[i])+
          scale_fill_brewer(palette = "Pastel1")
        print(fearturescatter )
      


    })
    output$imp <- renderUI({

      tags$img(src = "https://github.com/1101-datascience/finalproject-finalproject_group11/blob/main/importance.png")
      
    })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
