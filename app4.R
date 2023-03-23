library(shiny)
library(ggplot2)
library(shinyFeedback)
#library(knitr)
#library(kableExtra)
#library(dplyr)
library(xtable)

expFunc1 <- function(theta,x){
  return(theta*(1-exp(-x/theta)))
}
expFunc2 <- function(theta,k=1){
  return(theta^(k)*factorial(k))
}

#myfunc <- function(){
#  x <- 1
#  if(x==1){
#    string <- paste("hello there",input$policyLimit)
#    return(string)
#  }
#}

js <- "
window.MathJax = {
  loader: {
    load: ['[tex]/colortbl']
  },
  tex: {
    displayMath: [['$$', '$$']],
    packages: {'[+]': ['colortbl']}
  }
};
"

ui <- fluidPage(
  withMathJax(),
  shinyFeedback::useShinyFeedback(),
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution","Distribution",choices=c("exponential","gamma","pareto")),
      
      tabsetPanel(
        id="distParameters",
        type="hidden",
        tabPanel("exponential",fluidRow(column(6,sliderInput("exponentialTheta","Theta",min=1000,max=5000,step = 500,value=1000)))),
        tabPanel("gamma", fluidRow(column(6,sliderInput("gammaTheta","Theta",min=5,max=500,step=5,value=250)),
                                   column(6,sliderInput("gammaAlpha","Alpha",min=1,max=4,step=1,value=2)))),
        tabPanel("pareto",fluidRow(column(6,sliderInput("paretoTheta","Theta",min=5,max=500,step=5,value=250)),
                                   column(6,sliderInput("paretoAlpha","Alpha",min=1,max=4,step=1,value=2))))
      ),
      sliderInput("deductible","Deductible",value=0,min=0,max=10000,step=1000),
      sliderInput("policyLimit","Policy Limit:",value=0,min=0,max=10000,step=1000),
      sliderInput("coinsurance","Coinsurance: (%Insurer Pays)",value=100,min=70,max=100,step=2),
      actionButton("calculate","Calculate!"),
      textOutput("justTesting"),
      textOutput("justTesting2")
    ),
    mainPanel(
      tabsetPanel(
        id="outputs",
        type="pills",
        tabPanel("SolutionTab",
                 htmlOutput("steps1"),
                 uiOutput("solution1"),
                 textOutput("steps2"),
                 uiOutput("solution2")),
        tabPanel("Integral/DerivativeTab",
                 uiOutput("integral_derivative"),
                 uiOutput("expTabular"),
                 uiOutput("integral_derivative2"),
                 uiOutput("expTabular2")),
        tabPanel("FormulaTab",
                 tableOutput("formula"))
      )
    )
  )
)




server <- function(input, output, session) {
  observeEvent(input$distribution,{
    updateTabsetPanel(inputId="distParameters",selected=input$distribution)
  })
  
  
  # calculate button
  observeEvent(input$calculate,{
    
    # outputs selected deductible|policyLimit|coinsurance
    selectedInputs <- paste(ifelse(input$deductible==0,"0","1"),
                            ifelse(input$policyLimit==0,"0","1"),
                            ifelse(input$coinsurance==100,"0","1"))
    selectedInputs <- gsub(" ","",selectedInputs)
    output$justTesting <- renderText(selectedInputs)
    
    if(substr(selectedInputs,1,1)=="0"){
      lowerLimit <- 0
    }else{
      lowerLimit <- input$deductible
    }
    
    if(substr(selectedInputs,2,2)=="1"){
      if(substr(selectedInputs,1,1)=="0" & substr(selectedInputs,3,3)=="0"){
        upperLimit <- input$policyLimit
      }else if(substr(selectedInputs,1,1)=="1" & substr(selectedInputs,3,3)=="0"){
        upperLimit <- input$policyLimit+input$deductible
      }else if(substr(selectedInputs,1,1)=="0" & substr(selectedInputs,3,3)=="1"){
        upperLimit <- round(input$policyLimit/(input$coinsurance/100),0)
      }else if(substr(selectedInputs,1,1)=="1" & substr(selectedInputs,3,3)=="1"){
        upperLimit <- round((input$policyLimit/(input$coinsurance/100))+input$deductible,0)
      }
    }
    
    if(substr(selectedInputs,2,2)=="0"){
      upperLimit <- "\\infty"
    }
    
    #Solution Tab strings-------------------------------------------
    #E(x) steps
    if(substr(selectedInputs,1,1)=="0" & substr(selectedInputs,2,2)=="0" & substr(selectedInputs,3,3)=="0"){
      steps1 <- paste("")
    }
    if(substr(selectedInputs,1,1)=="1" & substr(selectedInputs,2,2)=="0" & substr(selectedInputs,3,3)=="0"){
      steps1 <- paste("With a deductible no payments are made for losses below the deductible, this is the same as the expected value minus a policy limit at the deductible.")
    }
    if(substr(selectedInputs,1,1)=="0" & substr(selectedInputs,2,2)=="1" & substr(selectedInputs,3,3)=="0"){
      steps1 <- paste("With a policy limit the maximum covered loss is equal to the policy limit.")
    }
    if(substr(selectedInputs,1,1)=="0" & substr(selectedInputs,2,2)=="0" & substr(selectedInputs,3,3)=="1"){
      steps1 <- paste("With coinsurance the insurance company only covers a percentage of the losses")
    }
    if(substr(selectedInputs,1,1)=="1" & substr(selectedInputs,2,2)=="1" & substr(selectedInputs,3,3)=="0"){
      steps1 <- paste("With a deductible and policy limit the maximum covered loss is equal to the policy limit + deductible.")
    }
    if(substr(selectedInputs,1,1)=="1" & substr(selectedInputs,2,2)=="0" & substr(selectedInputs,3,3)=="1"){
      steps1 <- paste("With a deductible and coinsurance the insurance company covers a portion of the losses above the deductible.")
    }
    if(substr(selectedInputs,1,1)=="0" & substr(selectedInputs,2,2)=="1" & substr(selectedInputs,3,3)=="1"){
      steps1 <- paste("With a policy limit and coinsurance the maximum covered loss becomes the policy limit/coinsurance.")
    }
    if(substr(selectedInputs,1,1)=="1" & substr(selectedInputs,2,2)=="1" & substr(selectedInputs,3,3)=="1"){
      steps1 <- paste("With a deductible, policy limit, and coinsurance the maximum covered loss becomes the policy limit/coinsurance + deductible.")
    }
    
    
    #E(x) solution
    if(substr(selectedInputs,1,1)=="1"){
      lower <- paste("-E[X\\wedge",lowerLimit,"]")
      lowerMath <- expFunc1(input$exponentialTheta,lowerLimit)
      varianceFormula <- "For\\ a\\ deductible\\ use\\ basic\\ principles\\ (next\\ tab)."
    }else{
      lower <- ""
      lowerMath <- 0
      varianceFormula <- ""
    }
    
    if(substr(selectedInputs,2,2)=="1"){
      upper <- paste("E[X\\wedge",upperLimit,"]")
      upperMath <- expFunc1(input$exponentialTheta,upperLimit)
    }else{
      upper <- "E[X]"
      upperMath <- expFunc2(input$exponentialTheta)
    }
    
    if(substr(selectedInputs,3,3)=="1"){
      coin <- paste(input$coinsurance/100,"*")
      coinLeft <- "("
      coinRight <- ")"
      coinMath <- input$coinsurance/100
    }else{
      coin <- ""
      coinLeft <- ""
      coinRight <- ""
      coinMath <- input$coinsurance/100
    }
    
    # E(X) steps
    
    #steps1 <- paste("E(X)",dedSteps,polSteps,coinSteps,sep="\n")
    #<br/>
    output$steps1 <- renderUI({
      HTML(paste(steps1))
    })
    
    solution1 <- paste("E(X)",
                         "$$",coin,coinLeft,upper,lower,coinRight,"$$",
                         "$$E(X)=",round(coinMath*(upperMath-lowerMath),2),"$$")
    

    output$solution1 <- renderUI(
      withMathJax(solution1)
    )
    
    #output$steps2 <- renderText(steps2)
    
    solution2 <- paste("Var(X)",
                          "$$",varianceFormula,"$$")
    
    output$solution2 <- renderUI(
      withMathJax(solution2)
    )
    
    
    
    
    
    
    
    #basic principle strings--------------------------------------------
    #deductibles
    if(substr(selectedInputs,1,1)=="1"){
      dedString <- paste("(x-",input$deductible,")")
      dedStringSq <- paste("(x-",input$deductible,")^{2}")
      dedString2 <- paste("+\\frac{1}{",input$exponentialTheta,"}[",input$exponentialTheta,"*",lowerLimit,"e^{-x/",input$exponentialTheta,"}]_{",lowerLimit,"}^{",upperLimit,"}")
      dedString2Sq <- paste("+\\frac{1}{",input$exponentialTheta,"}*2*",lowerLimit,"[",input$exponentialTheta,"xe^{-x/",input$exponentialTheta,"}+",input$exponentialTheta,"^{2}e^{-x/",input$exponentialTheta,"}]_{",lowerLimit,"}^{",upperLimit,"}-","\\frac{1}{",input$exponentialTheta,"}*",lowerLimit,"^{2}*[",input$exponentialTheta,"e^{-x/",input$exponentialTheta,"}]_{",lowerLimit,"}^{",upperLimit,"}")
    }else{
      dedString <- "x"
      dedStringSq <- "x^{2}"
      dedString2 <- ""
      dedString2Sq <- ""
    }
    #policy limits
    if(substr(selectedInputs,2,2)=="1"){
      polString <- paste("+[S(",upperLimit,")*",input$policyLimit,"]")
      polStringSq <- paste("+[S(",upperLimit,")*",input$policyLimit,"^{2}]")
    }else{
      polString <- ""
      polStringSq <- ""
    }
    #coinsurance
    if(substr(selectedInputs,3,3)=="1"){
      coinString <- paste(input$coinsurance/100,"*")
    }else{
      coinString <- ""
    }

    #output$justTesting2 <- renderText(coinMath)
    
    
    expIntegral <- c(paste("First Principles:",
                         "$$First\\ Moment$$",
                         "$$",coinString,"\\int_{",lowerLimit,"}^{",upperLimit,"}\\ ",dedString,"*\\frac{1}{",input$exponentialTheta,"}*e^{-x/",input$exponentialTheta,"}dx",polString,"$$",
                         "$$",coinString,"\\left(\\frac{1}{",input$exponentialTheta,"}","[-",input$exponentialTheta,"xe^{-x/",input$exponentialTheta,"}-",input$exponentialTheta,"^{2}e^{-x/",input$exponentialTheta,"}]_{",lowerLimit,"}^{",upperLimit,"}",dedString2,polString,"\\right)$$"),
                         
                     paste("$$Second\\ Moment$$",
                         "$$",coinString,"\\int_{",lowerLimit,"}^{",upperLimit,"}\\ ",dedStringSq,"*\\frac{1}{",input$exponentialTheta,"}*e^{-x/",input$exponentialTheta,"}dx",polStringSq,"$$",
                         "$$",coinString,"\\left(\\frac{1}{",input$exponentialTheta,"}[-x^{2}*",input$exponentialTheta,"e^{-x/",input$exponentialTheta,"}-2x*",input$exponentialTheta,"^{2}e^{-x/",input$exponentialTheta,"}-2*",input$exponentialTheta,"^{3}e^{-x/",input$exponentialTheta,"}]_{",lowerLimit,"}^{",upperLimit,"}",dedString2Sq,polStringSq,"\\right)$$"))

                              
    outputintegral_derivative <- switch(input$distribution,
                                        "exponential"=expIntegral,
                                        "gamma"="gammaintegral",
                                        "pareto"="paretointegral")
    
    output$integral_derivative <- renderUI({
      withMathJax(outputintegral_derivative[1])
    })

  #table 1
    output$expTabular <- renderUI({
      tab <- data.frame(c("+","-","+"),c("x","1","0"),c(paste("e^{-x/",input$exponentialTheta,"}*\\frac{1}{",input$exponentialTheta,"}"),
                                                                  paste("-",input$exponentialTheta,"e^{-x/",input$exponentialTheta,"}*\\frac{1}{",input$exponentialTheta,"}"),
                                                                  paste(input$exponentialTheta,"^{2}e^{-x/",input$exponentialTheta,"}*\\frac{1}{",input$exponentialTheta,"}")))
      colnames(tab) <- c("+/-","derivative", "integral")
      xtab          <- print(
        xtable(tab, align = rep("c", 4)),
        floating                   = FALSE,
        tabular.environment        = "array",
        print.results              = TRUE,
        sanitize.colnames.function = identity,
        sanitize.text.function=identity,
        include.rownames           = FALSE, 
        #add.to.row                 = list(
        #pos     = as.list(-1),
        #command = "\\rowcolor[gray]{0.95}"
        #)
      )
      tagList(
        #withMathJax(),
        HTML(paste0("$$", xtab, "$$")),
        #tags$script(HTML(js)),
        #tags$script(
         # async="", 
          #src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
        #)
      )
    })
    
    output$integral_derivative2 <- renderUI({
      withMathJax(outputintegral_derivative[2])
    })
    
    # table 2
    output$expTabular2 <- renderUI({
      tab <- data.frame(c("+","-","+","-"),
                        c("x^{2}","2x","2","0"),
                        c(paste("e^{-x/",input$exponentialTheta,"}*\\frac{1}{",input$exponentialTheta,"}"),
                                                                  paste("-",input$exponentialTheta,"e^{-x/",input$exponentialTheta,"}*\\frac{1}{",input$exponentialTheta,"}"),
                                                                  paste(input$exponentialTheta,"^{2}e^{-x/",input$exponentialTheta,"}*\\frac{1}{",input$exponentialTheta,"}"),
                                                                  paste("-",input$exponentialTheta,"^{3}e^{-x/",input$exponentialTheta,"}*\\frac{1}{",input$exponentialTheta,"}")))
      colnames(tab) <- c("+/-","derivative", "integral")
      xtab <- print(
        xtable(tab, align = rep("c", 4)),
        floating                   = FALSE,
        tabular.environment        = "array",
        print.results              = TRUE,
        sanitize.colnames.function = identity,
        sanitize.text.function=identity,
        include.rownames           = FALSE 
      )
      tagList(
        #withMathJax(),
        HTML(paste0("$$", xtab, "$$")),
        #tags$script(HTML(js)),
        #tags$script(
          #async="", 
          #src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
        #)
      )
      
    })

#-------------------------------------------------------------------------------------    
    
    outputformula <- switch(input$distribution,
                            "exponential"="exponentialformula",
                            "gamma"="gammaformula",
                            "pareto"="paretoformula")
    
    #output$formula2 <- renderText(outputformula)
    
  })

  #----------------------------------------------------

}

shinyApp(ui, server)








