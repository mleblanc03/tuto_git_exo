library(shiny)


ui<-fluidPage(
  
  # Application title
  titlePanel("Integration Monte Carlo"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("tarea", label="Visualtion ",
                   choices = c(
                     "Integration",
                     "Integration multidimensionnelle"
                   ),
                   selected="Integration"
      )
     
    ),
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(
        condition="input.tarea=='Integration'",
        h2("Integration de g sur [a,b]"),
        
        textInput(
          inputId="expresion1", 
          label="g(x)=",
          value="2*x"
        ),
        
        sliderInput("a", "a", min=-1000, max=1000, value=0),
        sliderInput("b", "b", min=-1000, max=1000, value=10),
        numericInput("nsim", "Numero de simulaciones", value=100), 
        sliderInput("alpha","alpha",min=0,max=1,value=0.05),
        
        textOutput("resultat1"),
        
        textOutput("resultat2")
        
      )
    )
  )
)


server<-function(input, output) {
  set.seed(20160815)
  
  fun1 <- reactive({
    texto <- paste("aux <-function(x) {return( ",input$expresion1,")}")
    eval(parse(text=texto))
    aux
  })
  
  x<-reactive({runif(n = input$nsim,min=input$a,max=input$b)})
  g_x<-reactive({sapply(x(),fun1())})
  #Application Monte-Carlo
  
  y<-reactive({sum(sapply(x(),fun1()))*(1/input$nsim)*(input$b-input$a)})
             #[sum((i=1)to nsim) g(Xi)]   con Xi~unif(a,b)                        *(1/nsim)      *(b-a)
  
  
  ecart_type<-reactive({sqrt((1/(input$nsim-1))*sum( (g_x()-y())*(g_x()-y()) ))}) 
  z<-reactive({qnorm(1-((input$alpha)/2),0,1)})
  dif<-reactive({z()*ecart_type()*(1/sqrt(input$nsim))})
 
  text1<-reactive({texto1<-paste("Approximation integrale de " ,input$expresion1," sur [ ",input$a," , ",input$b ," ] = ",y())})
  text2<-reactive({texto2<-paste("Intervalo de confianza de ",(1-(input$alpha))*100," % es [",y()-dif()," , ",y()+dif()," ]")})
  
  output$resultat1 <- renderText({text1()})
  output$resultat2<-renderText({text2()})
  
  

}

shinyApp(ui=ui,server = server)
