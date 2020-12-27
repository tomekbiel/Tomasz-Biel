
shinyServer(function(input, output) {
  i <- reactive({
    if(input$x=='a'){ i<-3 }    
    if(input$x=='b'){ i<-4 }     
    if(input$x=='c'){ i<-5 }     
    if(input$x=='d'){ i<-6 }     
    if(input$x=='e'){ i<-7 }     
    if(input$x=='f'){ i<-8 }     
    if(input$x=='g'){ i<-9 }
    if(input$x=='h'){ i<-10 }   
    if(input$x=='i'){ i<-11 }
    if(input$x=='j'){ i<-12 }
    return(i)
  })
  
  j <- reactive({
    if(input$y=='k'){ j<-3 }    
    if(input$y=='l'){ j<-4 }     
    if(input$y=='m'){ j<-5 }     
    if(input$y=='n'){ j<-6 }     
    if(input$y=='o'){ j<-7 }     
    if(input$y=='p'){ j<-8 }     
    if(input$y=='r'){ j<-9 }
    if(input$y=='s'){ j<-10 }   
    if(input$y=='t'){ j<-11 }
    if(input$y=='u'){ j<-12 }
    return(j)
  })
  
  i1 <- reactive({
    ind[ind$Country==input$selected_country,][,i()]
  })
  
  j1 <- reactive({
    
    ind[ind$Country==input$selected_country,][,j()]
  })
  
  output$myplot<- renderPlot({
    
    pl<-ggplot(data = ind$selected_country ,aes(x = i1(), y = j1(), size = j1(), color = i1() ))+
      labs(title = c('Relation of selected indicators:'), subtitle = paste(colnames(ind)[i()], colnames(ind)[j()],sep = ' & ') , x = colnames(ind)[i()], y = colnames(ind)[j()], caption = 'by_tomasz_biel')+                
      geom_point(position = 'jitter') +
      scale_color_gradient(low="gold", high="red4")+
      scale_shape_discrete(name="Koty czy Ptaki?") + 
      geom_text( aes(label=ind[ind$Country==input$selected_country,][,2]),hjust=-0.2)
    theme_economist()+
      theme(legend.position = "right")
    
    if (input$liniaTrendu) {
      pl <- pl + geom_smooth(se=FALSE, method="lm", size=1, color='red')+
        geom_smooth()
      
    }
    pl
    
  })
  
  output$model <- renderPrint({
    summary(lm(i1() ~ j1(),data= ind$selected_country))
  })
  
  output$myplot2 <- renderPlot({
    
    lin<-lm(i1() ~ j1(),data= ind$selected_country)
    resid <- lin$residuals
    resid <- resid[resid > -5 & resid < 5]
    
    # Plot a histogram of the residuals
    hist(resid, seq(-5, 5, .5), prob=TRUE, col="#bbbbbb",
         xlim=c(-5, 5), ylim=c(0, dnorm(0) * 1.5),
         yaxt="n", bty="n", ylab="", xlab="", main="Distribution of Residuals")
    rug(resid, lwd=2)
    
    # Plot a normal density (the expected residual distribtuion)
    curve(dnorm, col="seagreen", lwd=2, add=TRUE)
    
  })
  
  output$myplot3 <- renderPlot({
    
    lin<-lm(i1() ~ j1(),data= ind$selected_country)
    
    wsp <- lin$coefficients
    print(wsp)
    abs_wsp <- abs(wsp[2])
    
    trends <- replicate(999,{
      j1_sample <- sample(j1())
      wsp <- lm(i1()~j1_sample, ind$selected_country)$coef
      abs(wsp[2])
    })
    
    trends <- c(trends,abs_wsp)
    
    hist(trends, col="grey", main="", las=1, border="white")
    
    abline(v=abs_wsp, col="red", lwd=5)
    
    # p-value
    print(mean(trends))# >= abs_wsp))
    
  })
   
  output$density<-renderPlot({
    
    x12<-na.omit(data.frame(i1(),j1()))
    
    scatterplotMatrix(x12, diagonal='density', pch=1,lwd=1.5, regLine = list(col = "red", lwd = 3), 
                      smooth = list(col.smooth = "green", col.spread = "blue"))
  })
  
  output$density2 <- renderPlot({
    x12<-na.omit(data.frame(i1(), j1()))
    print(x12)
    d<-bkde2D(x12[,1:2], bandwidth = sapply(x12[,1:2],dpik))
    persp(d$x1,d$x2, d$fhat, zlab='Density',
          theta=35,phi=23, xlab = colnames(ind)[i()], ylab = colnames(ind)[j()])
  })

})
