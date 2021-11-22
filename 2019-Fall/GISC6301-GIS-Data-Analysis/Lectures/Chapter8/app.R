library(shiny)
plotAlphaBeta <- function(alpha=0.05, beta=0.01, effect=1, n=16){
  pg <- function(x, xCrit, alpha=T){
    if (alpha) {
      x <- x[x >= xCrit]
      y <- dnorm(x, muH0, sdError)
      x <- c(xCrit, x, x[length(x)], xCrit)
      y <- c(0,y,0,0)
      polygon(x, y, col="grey80")  
    } else {
      x <- x[x < xCrit]
      y <- dnorm(x, muH1, sdError)
      x <- c(x[1], x, x[length(x)], x[1])
      y <- c(0,y,0,0)
      polygon(x, y, col="grey50")  
    }
  } # end::pg
  
  sdError <- 1/sqrt(n)
  muH0 <- 0
  muH1 <- effect
  xCrit <- qnorm(1-alpha, muH0, sdError)
  x <- seq(muH0-1/2, 1+1/2, by=0.001)
  
  yH0 <- dnorm(x,muH0,sdError)
  yH1 <- dnorm(x,muH1,sdError)
  
  plot(x,yH0, type="l",ylim=c(0,max(yH0)), 
       xaxt="n", ylab=expression("Distribuiton of Test Statistic "~~bar(X)), 
       xlab=expression(bar(X)))
  axis(1, seq(muH0-1/2, 1+1/2, by=0.25))
  pg(x, xCrit, alpha=T)
  
  lines(x,yH1)
  pg(x, xCrit, alpha=F)
  abline(v=muH0, lty=5)
  abline(v=muH1, lty=5)
  abline(v=xCrit, lwd=3)
  lines(x,yH0)
} # end::plotAlphaBeta

testPowerUI <- function(id){
  ns <- NS(id)
  valList <- list(
    sliderInput(ns("aSlider"),"Select Alpha Error",0.005,0.10,0.05,step=0.005),
    sliderInput(ns("bSlider"),"Select Beta Error",0.005,0.25,0.01,step=0.005),
    sliderInput(ns("eSlider"),"Select Effect Size",0.05,1.00,0.50,step=0.05),
    sliderInput(ns("sSlider"),"Select Sample Size",16,256,64,step=1),
    tags$h3(textOutput(ns("Num"))),
    plotOutput(ns("AlphaBeta"))
  )
  switch(id,
         Alpha = valList[1] <- NULL,
         Beta = valList[2] <- NULL,
         Effect = valList[3] <- NULL,
         Sample = valList[4] <- NULL)
  tagList(valList)
} # end::testPowerUI

testPower <- function(input, output, session, option){
  library(pwr)
  if(option == "a") {
    beta <- reactive(input$bSlider)
    effect <- reactive(input$eSlider)
    n <- reactive(input$sSlider)
    alpha <- reactive( pwr.t.test(n=n(), d=effect(), sig.level=NULL, power=(1-beta()), 
                                  type="paired", alternative="greater")$sig.level )
    output$Num <- renderText(paste0("Alpha Error: ",round(alpha(),5)))
    output$AlphaBeta <- renderPlot({plotAlphaBeta(alpha=alpha(), beta=beta(), effect=effect(), n=n())})
  } #end::"a"
  
  if(option == "b") {
    alpha <- reactive(input$aSlider)
    effect <- reactive(input$eSlider)
    n <- reactive(input$sSlider)
    beta <- reactive(1-pwr.t.test(n=n(), d=effect(), sig.level=alpha(), power=NULL, 
                                  type="paired", alternative="greater")$power )
    output$Num <- renderText(paste0("Beta Error: ",round(beta(),5)))
    output$AlphaBeta <- renderPlot({plotAlphaBeta(alpha=alpha(), beta=beta(), effect=effect(), n=n())})
  } #end::"b"
  
  if(option == "e") {
    alpha <- reactive(input$aSlider)
    beta <- reactive(input$bSlider)
    n <- reactive(input$sSlider)
    effect <- reactive(pwr.t.test(n=n(), d=NULL, sig.level=alpha(), power=1-beta(), 
                                  type="paired", alternative="greater")$d )
    output$Num <- renderText(paste0("Effect Size: ",round(effect(),2)))
    output$AlphaBeta <- renderPlot({plotAlphaBeta(alpha=alpha(), beta=beta(), effect=effect(), n=n())})    
  } #end::"e"
  
  if(option == "s") {
    alpha <- reactive(input$aSlider)
    effect <- reactive(input$eSlider)
    beta <- reactive(input$bSlider)
    n <- reactive(pwr.t.test(n=NULL, d=effect(), sig.level=alpha(), power=1-beta(), 
                             type="paired", alternative="greater")$n )
    output$Num <- renderText(paste0("Sample Size: ",round(n(),0)))
    output$AlphaBeta <- renderPlot({plotAlphaBeta(alpha=alpha(), beta=beta(), effect=effect(), n=n())})    
  } #end::"s"
 
} # end::testPower

ui <- fluidPage(
  titlePanel("Explore Alpha- and Beta-Errors subject to Effect- and Sample-Sizes"),
  navbarPage(title="Calculate:",
             tabPanel("Alpha Error", testPowerUI("Alpha")),
             tabPanel("Beta Error", testPowerUI("Beta")),
             tabPanel("Effect Size", testPowerUI("Effect")),
             tabPanel("Sample Size", testPowerUI("Sample"))
  )  
) # end::ui

server <- function(input, output){
  callModule(testPower,"Alpha","a")
  callModule(testPower,"Beta","b")
  callModule(testPower,"Effect","e")
  callModule(testPower,"Sample","s")
} # end::server

shinyApp(ui = ui, server = server)