#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls()) 
set.seed(333) # reproducible
library(directlabels)
library(shiny) 
library(shinyWidgets)
library(shinythemes)  # more funky looking apps
library(DT)
library(shinyalert)
library(Hmisc)
library(reshape)
library(rms)
library(ormPlot)
library(ordinal)
library(ggplot2)
library(tidyverse)
#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
options(max.print=1000000)    
fig.width <- 400
fig.height <- 300
fig.width1 <- 1380
fig.height1 <- 700
fig.width2 <- 1400
fig.height2 <- 300
fig.width3 <- 1400  
fig.height3 <- 600
fig.width4 <- 1380
fig.height4 <- 450
fig.width5 <- 1380
fig.height5 <- 225
fig.width6 <- 400
fig.height6 <- 550
fig.width7 <- 600
fig.widthx <- 593
fig.heightx <- 268
fig.height7 <- 600
fig.width9 <- 1380
fig.height9 <- 500

## convenience functions
p0 <- function(x) {formatC(x, format="f", digits=1)}
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p3 <- function(x) {formatC(x, format="f", digits=3)}
p5 <- function(x) {formatC(x, format="f", digits=5)}
logit <- function(p) log(1/(1/p-1))
expit <- function(x) 1/(1/exp(x) + 1)
inv_logit <- function(logit) exp(logit) / (1 + exp(logit))
is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful
options(width=200)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                # paper
                useShinyalert(),  # Set up shinyalert
                setBackgroundColor(
                    color = c( "#2171B5", "#F7FBFF"), 
                    gradient = "linear",
                    direction = "bottom"
                ),
                
                h2("xxxxxxxxxxxxxxx"), 
                
                h4("xx
         "), 
                
                h3("  "), 
                
                
                sidebarLayout(
                    
                    sidebarPanel( width=3 ,
                                  
                                  tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                                  
                                  
                                  actionButton(inputId='ab1', label="R Shiny ",   icon = icon("th"),   
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/proportional-odds-model2/master/app.R', '_blank')"), 
                                  actionButton(inputId='ab1', label="R code",   icon = icon("th"),   
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/proportional-odds-model2/master/app%20stripped%20code.R', '_blank')"),  
                                  actionButton("resample", "Simulate a new sample"),
                                  br(),  
                                  tags$style(".well {background-color:#b6aebd ;}"), 
                                  
                                  h4("xxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                                  div(
                                      
                                      
 
                                      
                                      
                                      tags$head(
                                          tags$style(HTML('#ab1{background-color:orange}'))
                                      ),
                                      
                                      tags$head(
                                          tags$style(HTML('#resample{background-color:orange}'))
                                      ),
                                      
                                      textInput('K', 
                                                div(h5(tags$span(style="color:blue", "No of covariates"))), "10"),
                                      
                                      tags$hr(),
                                      textInput('pow', 
                                                div(h5(tags$span(style="color:blue", "power %"))), "90"),
                                      
                                      textInput('sigma', 
                                                div(h5(tags$span(style="color:blue", "residual variation"))), "2"),
                                      tags$hr(), 
                                      textInput('theta', 
                                                div(h5(tags$span(style="color:blue", "treatment effect"))), ".4"),
                                      
                                      
                                      # textInput('or2', 
                                      #           div(h5(tags$span(style="color:blue", "yyyyyyyyyyyyyyyy"))), "1"),
                                      
                                      #  textInput('n2y2', 
                                      # #      div(h5("Enter the true correlation (tab 2)")), ".8"),
                                      # div(h5(tags$span(style="color:blue", "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))), "0.8"),
                                      # tags$hr(),
                                      
                                      # div(h5("References:")),  
                                      # tags$a(href = "https://en.wikipedia.org/wiki/Bootstrapping_%28statistics%29", tags$span(style="color:blue", "[1] PRO"),),   
                                      # div(p(" ")),
                                      # tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176345338",  tags$span(style="color:blue", "[2] PO"),),   
                                      # div(p(" ")),
                                      # tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176344552", tags$span(style="color:blue", "[3] Krushke"),),
                                      # div(p(" ")),
                                      # tags$a(href = "https://blogs.sas.com/content/iml/2017/09/20/fishers-transformation-correlation.html", tags$span(style="color:blue", "[4] xxxxxx"),),  
                                      # div(p(" ")),
                                      # tags$a(href = "https://rdrr.io/cran/rms/man/predict.lrm.html", tags$span(style="color:blue", "prediction of model mean"),),  
                                      # div(p(" ")),
                                      # tags$hr()
                                  )
                                  
                                  
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(width=9,
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              navbarPage(       
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                                  tags$style(HTML("
                            .navbar-default .navbar-brand {color: orange;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: #b6aebd;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}
                   ")),
                                  
                                  
                                  tabPanel("1 xxxxxxxxxxx", value=7, 
                                           h4("All covariates are prognostic, standard error of treatment effect (z) smaller if we adjust (right output)"),
                                           
                                           
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      div( verbatimTextOutput("B") )     
                                                     # div(plotOutput("beta",  width=fig.width7, height=fig.height7)),
                                                      
                                               ) ,
                                               
                                               
                                               fluidRow(
                                                   column(width = 5, offset = 0, style='padding:1px;',
                                                          div( verbatimTextOutput("A") )        
                                                        #  div(plotOutput("reg.plotx",  width=fig.width7, height=fig.height7)) 
                                                          
                                                   ))),
                                           h4(paste("Figures 1 & 2. xxxxxxxxxxxxxxx")), 
                                           
                                  ) ,
                                  
                                  tabPanel("2 xxxxxxxxx", value=3, 
                                           h4("All covariates are not prognostic, standard error of treatment effect (z) only slightly larger if we adjust (right output)"),
                                           
                                           
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      div( verbatimTextOutput("D") )     
                                                      # div(plotOutput("beta",  width=fig.width7, height=fig.height7)),
                                                      
                                               ) ,
                                               
                                               
                                               fluidRow(
                                                   column(width = 5, offset = 0, style='padding:1px;',
                                                          div( verbatimTextOutput("C") )        
                                                          #  div(plotOutput("reg.plotx",  width=fig.width7, height=fig.height7)) 
                                                          
                                                   ))),
                                           h4(paste("Figures 1 & 2. xxxxxxxxxxxxxxx")), 
                                           
                                  ) ,
                                  
                                  tabPanel("3 xxxxxxxxxxxxxxxx", value=7, 
                                           
                                           
                                           div(plotOutput("reg.plot", width=fig.width1, height=fig.height1)),
                                           
                                           fluidRow(
                                               column(width = 7, offset = 0, style='padding:1px;',
                                                      #      h4(paste("Figure 3. xxxxxxxxxxxxxxx")), 
                                                      
                                               )),
                                           
                                  ) ,
                                  
                                  
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  
                                  tabPanel("4 xxxxxxx", value=3, 
                                           
                                           h5(paste("Enter xxxxxxxxxxxxxxxxxxx")), 
                                           textInput('rcat2', 
                                                     div(h5(tags$span(style="color:blue",
                                                     ))), "999"),
                                           
                                           
                                         #  div(plotOutput("preds2", width=fig.width1, height=fig.height3)),
                                           
                                           
                                           
                                           fluidRow(
                                               column(width = 7, offset = 0, style='padding:1px;',
                                          #            h4(paste("Figure 4. Plot of the predicted probabilities")), 
                                                      
                                               )),
                                  ),
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  tabPanel("5 xxxxxxxxxxxx", 
                                           h4(paste("xxxxxxxxxxxxxxx")),
                                           
                                           h4("xxxxxxxxxxxxxx"),
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      
                                                    #  div(plotOutput("preds", width=fig.width7, height=fig.height3)),
                                                      
                                                      fluidRow(
                                                          
                                                          textInput('base', 
                                                                div(h5(tags$span(style="color:blue", 
                                                                                    "xxxxxxxxxxxx"))), "1")
                                                          
                                                          
                                                      ),
                                               ) ,
                                               
                                               fluidRow(
                                                   
                                                   
                                                   column(width = 5, offset = 0, style='padding:1px;',
                                                          
                                                       #   div(plotOutput("predicts", width=fig.width7, height=fig.height3)),
                                                          
                                                          fluidRow(
                                                              
                                                              textInput('group', 
                                                                        div(h5(tags$span(style="color:blue", 
                                                                                         "xxxxxxxxxxxxxxx"))), "1"),
                                                              
                                                              textInput('rcat', 
                                                                        div(h5(tags$span(style="color:blue", 
                                                                                         "xxxxxxxxxxxxxxx"))), "999"),
                                                              
                                                          ),
                                                          
                                                   ))),
                                           
                                           
                                           width = 30 )     ,
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  tabPanel("6 xxxx",
                                        #   h4(paste("Table 3 Predicted probabilities, the estimated mean Y (meanY) is calculated by summing values of Y multiplied by the estimated Prob(Y=j)")),
                                           fluidRow(
                                               column(width = 12, offset = 0, style='padding:1px;',
                                                      
                                                   #   div( verbatimTextOutput("reg.summaryp") ),
                                                   #   h4(paste("Table 4 Predicted cummulative probabilities ")),
                                                    #  div( verbatimTextOutput("reg.summaryc") ),
                                               ) ,
                                               
                                           ),
                                           
                                  ),
                                  
                                  tabPanel("7 xxxxxxxxx", value=3, 
                                           h4(" xxxxxxxxxxxxx"),
                                           
                                           
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      
                                                      #div( verbatimTextOutput("reg.summary4") )
                                               ) ,
                                               
                                               fluidRow(
                                                   column(width = 5, offset = 0, style='padding:1px;',
                                                          
                                                     #     div( verbatimTextOutput("reg.summary5")),
                                                      #    div(plotOutput("predictl", width=fig.widthx, height=fig.heightx)),
                                                          
                                                   ))),
                                           h4("xxxxxxxxxxxxxxxxxxxxx"),
                                  ),
                                  
                                  tabPanel("8 xxxxxxxx", value=3, 
                                           
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      h4("xxxxxxxxxxxxxxx"),
                                                      textInput('kints',
                                                           div(h5(tags$span(style="color:blue",
                                                                                ""))), ""), 
                                                      
                                                      #div(plotOutput("PP.plot", width=fig.width7, height=fig.height6)),
                                                      h4("Figure xxxxxxxxxxxxxx"),
                                                      br() , 
                                                      
                                                      h4(""),
                                                      
                                                      h4("Table xxxxxxxxxxxxx"),
                                                      #div( verbatimTextOutput("predz"), width = 2), # 
                                               ),
                                               
                                               fluidRow(
                                                   
                                                   
                                                   h4("xxxxxxxxxxxx"),
                                                   h4("yyyyyyyyyyyy"),
                                                   br(), br(), br() ,  
                                                   
                                                   
                                                   column(width = 5, offset = 0, style='padding:0px;',
                                                          
                                                        #  div(plotOutput("PP.plot2", width=fig.width7, height=fig.height6)),
                                                          h4("Figure 9 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                                                          
                                                   )))
                                           
                                  ) ,
                                  
                                  
                                  tabPanel("9 xxxxxxxxx", value=3, 
                                           
                                           #h5(paste("Checking assumptions")), 
                                           #div(plotOutput("assumption", width=fig.width1, height=fig.height3)),
                                           h4("Figure xxxxxxxxxxxxxx"),
                                           h4( "xxxxxxxxxxxx" ),
                                           h4("Table  xxxxxxxxxxxxxxxxxxx"),
                                         # div( verbatimTextOutput("assump")),  
                                           
                                  ),
                                  
                                  
                                  tabPanel("10 xxxxxxxx", value=3, 
                                           
                                        #   div(plotOutput("ecdfs", width=fig.width1, height=fig.height3)),
                                           h4("Figure 11 xxxxxxxxxxxx"), 
                                           h4("xxxxxxxxxxxxxxxx"), 
                                         #  div(plotOutput("logitseries", width=fig.width1, height=fig.height3)),
                                           
                                           
                                           h4("Figure 12 xxxxxxxxxxxxxxxxxxxxx"),  
                                           
                                           h4("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.")
                                           
                                  ),
                                  
                                  
                                  tabPanel("11 xxxxxxxxxxxx", 
                                           
                                           fluidRow(
                                               column(width = 9, offset = 0, style='padding:1px;',
                                                      h4("Table 9 xxxxxxxxxxxx"),
                                                    div( verbatimTextOutput("dat")),
                                               ),
                                               
                                               column(width = 3, offset = 0, style='padding:1px;',
                                                      h4("xxxx"),
                                                      h4("xxxxxxxxxxxxx
                                                  \n"),
                                                      
                                                      tags$hr(),
                                                      div(h4("References:")),  
                                                      tags$a(href = "https://stats.stackexchange.com/search?q=proportional+odds+model", tags$span(style="color:blue", "[1] xxxxxxxxxxxxx"),),   
                                                      div(p(" ")),
                                                      tags$a(href = "hhttps://en.wikipedia.org/wiki/Ordered_logit",  tags$span(style="color:blue", "[2] xxxxxxxxxxxxxx"),),   
                                                      div(p(" ")),
                                                      #  tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176344552", tags$span(style="color:blue", "[3] Krushke"),),
                                                      #  div(p(" ")),
                                                      tags$a(href = "http://hbiostat.org/doc/rms.pdf", tags$span(style="color:blue", "[3] xxxxxxxxxxxxxx"),),  
                                                      div(p(" ")),
                                                      tags$a(href = "https://rdrr.io/cran/rms/man/predict.lrm.html", tags$span(style="color:blue", "[4] xxxxxxxxxxxxxxxx"),),  
                                                      div(p(" ")),
                                                      tags$a(href = "https://psyarxiv.com/x8swp/", tags$span(style="color:blue", "[5] xxxxxxxxxxxxx"),),  
                                                      div(p(" ")),
                                                      tags$a(href = "https://stats.stackexchange.com/questions/89474/interpretation-of-ordinal-logistic-regression#89485
", tags$span(style="color:blue", "[6] xxxxxxxxxxxxx"),),  
                                                      div(p(" ")),
                                                      tags$hr()
                                                      
                                               )
                                               
                                               
                                           )
                                  )##end
                                  
                                  
                                  
                                  
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   END NEW   
                              )
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                ) 
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- shinyServer(function(input, output   ) {
    
    shinyalert("Welcome! \nExplore xxxxxxxxxxx!",
               "xxxxxxxxxxxxxx", 
               type = "info")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # This is where a new sample is instigated 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    random.sample <- reactive({
        
        foo <- input$resample
        
        K <- as.numeric(unlist(strsplit(input$K,",")))
        
        pow <- as.numeric(unlist(strsplit(input$pow,",")))
        
        sigma <- as.numeric(unlist(strsplit(input$sigma,",")))
        
        theta <- (as.numeric(unlist(strsplit(input$theta,","))))   # user enter odds, need log for the maths
        
        return(list(  
            K=K,  
            pow=pow/100,
            sigma=sigma, 
            theta=theta
        ))
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tab 1 simulate po model data and analyse
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mcmc <- reactive({
        
        sample <- random.sample()
       
        K=sample$K
        pow=sample$pow
        sigma=sample$sigma
        theta=sample$theta        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        Po <- power.t.test( delta =theta, sd=sigma, sig.level=0.05,
                            power=pow, type="two.sample", alternative=c("two.sided"))

        N <-ceiling(Po$n)*2
                                                   # variables
        X <- array(runif(N*K , -1,1), c(N,K))  
        z <- sample(c(0,1), N, replace=T)          # treatment indicator
        a <- 1                                     # intercept
        
        # making up beta coefficients
        # b=1:K 
        # b= rep(1,K)                               
        b <- round(sort(runif(K, 0,5)), digits=2)  # making up some beta coefficients
        
        y <- a+ X %*% b + theta*z + rnorm(N,0, sigma)
        fake <- data.frame(X=X, y=y, z=z)
        # put the data together
        dat <- fake
        
        y <- a +  theta*z + rnorm(N,0, sigma)
        fake2 <- data.frame(X=X, y=y, z=z)
        
        
        
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # confidence interval
        zz <-   lapply(fake[1:K], function(x) 
            t.test(x ~ fake$z, paired = FALSE, na.action = na.pass))
        
        zzz <-   lapply(zz, function(x) 
            x[4]
        )
        
        zzz<- as.data.frame(zzz)
        ci <- t(zzz)
        conf<- as.data.frame(ci)
        
        
        #  Mean diff
        mzz <-   lapply(zz, function(x) 
            (x[5])
        )
        mzz<- as.data.frame(mzz)
        ci <- t(mzz)
        r<- as.data.frame(ci)
        
        doff <- as.vector( r["mean in group 0"] - r["mean in group 1"] )
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
        
        return(list(  dat=dat, conf=conf, doff=doff , K=K, N=N, X=X, fake2=fake2)) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DO THE ANALYSIS~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    analysis <- reactive({
        
       # return(list( ols.=ols., orm.=orm. , kk=kk , P2=P2, k=k, K=K,dat=dat, m=m, f2=f2, f3=f3, P=P, sf1=sf1,d=d )) 
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # beta dist plot 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
    
    output$beta <- renderPlot({        
        
        sample <- random.sample()
        
        
                 
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #  plot 1
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    output$reg.plot <- renderPlot({         
        
        # Get the  data
   
        doff <- mcmc()$doff
        conf <- mcmc()$conf
        K <- mcmc()$K
        
        # plot
        par(mar=c(3,3,3,3), mgp=c(1.5,.5,0), tck=-.01)
        plot(c(0, K+1), range(conf), bty="l", xlab="Covariates", 
             ylab="Estimate Mean difference", xaxs="i",  type="n", 
             main="'Imbalance' estimate mean difference of covariate distribution & 95% confidence interval ")
        #axis(2, seq(-5,5,1))
        # axis(1, seq(1,K,10))
        points(1:K, doff[,1], pch=20)
        abline(0, 0, col="gray")
        for (i in 1:K){
            if (prod(conf[i,c(1,2)]) < 0 ) {
                lines(c(i,i), conf[i,c(1,2)], lwd=.8, col='blue') 
            } else {
                lines(c(i,i), conf[i,c(1,2)], lwd=.8, col='red') 
            }
        }
        
            
 
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tables of predictions
    
    reg1 <- reactive({  
        
        d <- mcmc()$dat
         
        X <- mcmc()$X  #have to bring X through , as model will fail without this
        
        ols2 <- lm(y~X+z,data=d)
        ols1 <- lm(y~z,d)
        
        A<-summary(ols2)
        B<-summary(ols1)
        
        return(list(  A=A, B=B)) 
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$A <- renderPrint({
        
        return(reg1()$A)
    })
    
    output$B <- renderPrint({
        
        return(reg1()$B)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    reg2 <- reactive({  
        
        d <- mcmc()$fake2
        
        X <- mcmc()$X  #have to bring X through , as model will fail without this
        
        ols2 <- lm(y~X+z,data=d)
        ols1 <- lm(y~z,d)
        
        A<-summary(ols2)
        B<-summary(ols1)
        
        return(list(  C=A, D=B)) 
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$C <- renderPrint({
        
        return(reg2()$C)
    })
    
    output$D <- renderPrint({
        
        return(reg2()$D)
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # non cummulative predicted probabilities plot run the analysis again
    # not efficient I know
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$preds2 <- renderPlot({
        
      
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # non cummulative predicted probabilities plot run the analysis again
    # not efficient I know
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$preds <- renderPlot({
        
       
        
    })
    

    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plots of predictions
    
    output$predicts <- renderPlot({   
        
         
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # text 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    output$PP.plot <- renderPlot({   
        
      
        
    }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~baseline plots~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    output$PP.plot2 <- renderPlot({   
        
       
        
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # a plot of coef from series of logistic regression models. checking assumptions
    
    output$logitseries <- renderPlot({   
        
         
        
        
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~assumption plot~~~~~~~~~~~~~~~~~~~~~~~~    
    # on the fly plot harrell's PO assumption plot...
    
    output$assumption <- renderPlot({   
        
       
        
    }) 
    
    
    assump <- reactive({
        
         
        
    })  
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~baseline predictions~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    predz <- reactive({
        
       
    })  
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # text 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    output$textWithNumber <- renderText({ 
      
        
    })
    
    
    output$assump <- renderPrint({
        
      #  return(print(assump()$s, digits=3))
        
    }) 
    
    
    output$textWithNumber1 <- renderText({ 
        
       # A <- analysis()$f2     
        
        
    })
    
    output$dat <- renderPrint({
        
        d <- mcmc()$dat
        
        #d <- plyr::arrange(d, baseline, treatment)
        
        return(print(d, digits=4))
    })
    
    
    output$predz <- renderPrint({
        
        #return(print(predz()$p, digits=4))
    })
    
    output$predt <- renderPrint({
        
        #return(print(predt()$pt, digits=4))
    })
    
    
    output$reg.summary1 <- renderPrint({
        
        #return( (analysis()$f2 ))
        
    })
    
    output$reg.summary3 <- renderPrint({
        
        #return(print(analysis()$sf1, digits=4))
        
    })
    
    output$reg.summary4 <- renderPrint({
        
        #return(print(lmx()$linear, digits=4))
        
    })
    
    output$reg.summary5 <- renderPrint({
        
      #  return(print(lmx()$an, digits=4))
        
    })
    
    output$reg.summaryp <- renderPrint({
        
       # return(print(predictz()$prob, digits=4))
        
    })
    
    output$reg.summaryc <- renderPrint({
        
        #return(print(predictz()$cprob, digits=4))
        
    })
    
    output$reg.summaryci <- renderPrint({
        
        #return(print(predictz()$plotci, digits=4))
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    lmx <- reactive({
        
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$predictl <- renderPlot({   
        
       
        
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$ecdfs <- renderPlot({   
        
        
        
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})

# Run the application 
shinyApp(ui = ui, server = server)