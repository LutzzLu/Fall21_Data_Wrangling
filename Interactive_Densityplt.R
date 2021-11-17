#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
ui <- basicPage(
    plotOutput("plot1",
               click = "plot_click",
               dblclick = "plot_dblclick",
               hover = "plot_hover",
               brush = "plot_brush"
    ),
    verbatimTextOutput("info")
)

server <- function(input, output) {
    pca=read.csv("pca_dimension_reduced.csv",header=TRUE,sep=",")
    colnames(pca)=c("num","PCA_1","PCA_2","target")
    T=unique(pca$target)
    tar_cols<-c("#de8f6e","#aac0aa","#2a1a1f","#bb0a21","#087e8b")
    names(tar_cols)<-levels(pca$target)
    ggpl1 <- ggplot(pca,aes(x=`PCA_1`,y=`PCA_2`,color=target))+
        geom_point()+ 
        labs(x="PCA_1",y="PCA_2")+
        scale_color_manual(values=tar_cols)+ 
        theme_classic()
    ggpl1
    pca_mean1=c()
    pca_mean2=c()
    pca_var1=c()
    pca_var2=c()
    for(i in 1:5){
        pca_mean1[i]=mean(pca[which(pca$target==T[i]),]$PCA_1)
        pca_mean2[i]=mean(pca[which(pca$target==T[i]),]$PCA_2)
        pca_var1[i]=var(pca[which(pca$target==T[i]),]$PCA_1)
        pca_var2[i]=var(pca[which(pca$target==T[i]),]$PCA_2)
    }
    pcameasure=data.frame(T,pca_mean1,pca_mean2,pca_var1,pca_var2)
    
   
    
    output$plot1 <- renderPlot({plot(density(pca$PCA_1))
                                     lines(density(pca$PCA_2),col="red")})
    
    output$info <- renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
                   " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
        }
        
        paste0(
            "click: ", xy_str(input$plot_click),
            "dblclick: ", xy_str(input$plot_dblclick),
            "hover: ", xy_str(input$plot_hover),
            "brush: ", xy_range_str(input$plot_brush)
        )
    })
}

shinyApp(ui, server)
