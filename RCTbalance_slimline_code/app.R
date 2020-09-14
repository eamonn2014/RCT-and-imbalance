#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Some notes on the topic of randomisation
# https://twitter.com/ildiazm/status/1303002930723913728
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
# It follows that covariate imbalance, contrary # to what has been claimed by Altman, is just as much of a problem for large Studies as for  small ones
# https://twitter.com/f2harrell/status/1299755896319475712
# https://twitter.com/f2harrell/status/1298640944405807105
# but adjusted estimation does not have to be robust to be a major improvement over unadjusted analysis.  
# Using observed imbalances to find covariates to adjust for is arbitrary and reduces power by maximizing co-linearity with treatment

# https://discourse.datamethods.org/t/should-we-ignore-covariate-imbalance-and-stop-presenting-a-stratified-table-one-for-randomized-trials/547/32
# 'randomisation entitles us to ignore covariates we have not measured.'
# To me the goal of a parallel-group randomized clinical trial is to answer this question: do two patients starting out at the same point 
# (same age, severity of disease, etc.), one on treatment A and one on treatment B, end up with the same expected outcomes? This is fundamentally a completely conditional model.

# https://www.linkedin.com/pulse/stop-obsessing-balance-stephen-senn/
# https://discourse.datamethods.org/t/guidelines-for-covariate-adjustment-in-rcts/2814/2
#' Can you reconcile these two points?
#'   
#' @f2harrell' : One covariate imbalance is likely to be counterbalanced by another in opposite direction.
#' @stephensenn : One covariate imbalance likely coincides with other imbalances in same direction (thus, adjusting for one adjusts for them all)
#' Stephen John Senn #' @stephensenn
#' 22 Aug 2019
#' 1/2 So the first is true(ish) of unobserved covariates. The second  covers the fact that given observed  imbalance/balance in one covariate there may be imbalance/balance in another. So what?!
#

# https://twitter.com/f2harrell/status/1303002649080532995
# Unadjusted estimates are biased in comparison with adjusted estimates from nonlinear models, a fact well studied for decades.  Some do not call this 'bias' but the damage is the same. Literature is summarized in my ANCOVA chapter in http://hbiostat.org/doc/bbr.pdf
# https://discourse.datamethods.org/t/should-we-ignore-covariate-imbalance-and-stop-presenting-a-stratified-table-one-for-randomized-trials/547/2
# scenarios investigated in this app
# y	prognostic		      adj
# y	prognostic		      not adj
# y	unrelated		        adj
# y	unrelated		        not adj
# y	mix prog		        adj
# y	mix prog	         	not adj
# y	correlated prog		  adj
# y	correlated prog		  not adjusted
# n	correlated not prog	adj                <----no
# n	correlated not prog	not adjusted       <----no
# y	imbalances prog		  adj
# y	imbalances prog		  not adjusted
# y	imbalances not prog	adj
# y	imbalances not prog	not adjusted

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  rm(list=ls()) 
  set.seed(333) # reproducible
  library(directlabels)
  library(shiny) 
  library(shinyWidgets)
  library(shinythemes)  # more funky looking apps
  #library(DT)
  library(shinyalert)
  library(Hmisc)
  library(reshape)
  library(rms)
  #library(ormPlot)
  #library(ordinal)
  library(ggplot2)
  library(tidyverse)
  library(Matrix)

  options(max.print=1000000)    
  
  fig.width <- 1200
  fig.height <- 500
  fig.width1 <- 1380
  fig.width8 <- 1380
  fig.height1 <- 700
  fig.width7 <- 700
  fig.height7 <- 500
  fig.width6 <- 680
  ## convenience functions
  p0 <- function(x) {formatC(x, format="f", digits=0)}
  p1 <- function(x) {formatC(x, format="f", digits=1)}
  p2 <- function(x) {formatC(x, format="f", digits=2)}
  p3 <- function(x) {formatC(x, format="f", digits=3)}
  p4 <- function(x) {formatC(x, format="f", digits=4)}
  p5 <- function(x) {formatC(x, format="f", digits=5)}

  logit <- function(p) log(1/(1/p-1))
  expit <- function(x) 1/(1/exp(x) + 1)
  inv_logit <- function(logit) exp(logit) / (1 + exp(logit))
  is.even <- function(x){ x %% 2 == 0 } # function to identify odd maybe useful

  options(width=200)
  options(scipen=999)
  w=4  # line type
  ww=3 # line thickness
  wz=1 

# not used, but could use this for MSE
  calc.mse <- function(obs, pred, rsq = FALSE){
      if(is.vector(obs)) obs <- as.matrix(obs)
      if(is.vector(pred)) pred <- as.matrix(pred)
      
      n <- nrow(obs)
      rss <- colSums((obs - pred)^2, na.rm = TRUE)
      if(rsq == FALSE) rss/n else {
          tss <- diag(var(obs, na.rm = TRUE)) * (n - 1)
          1 - rss/tss
      }
  }

  RR=.37 ## used to limit correlations between variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2 , paper another option to try
                  # paper
                  useShinyalert(),  # Set up shinyalert
                  setBackgroundColor(
                      color = c( "#2171B5", "#F7FBFF"), 
                      gradient = "linear",
                      direction = "bottom"
                  ),
                  
            h2("Covariate adjustment in randomised controlled trials (RCTs) with a continuous response"), 
                
            h4("The main value of randomization in RCTs is that it eliminates selection bias, treatment groups are on average comparable in terms of known
                and unknown patient characteristics [1]. But as Stephen Senn has stated '1) randomised controlled trials don't deliver balance *even* if they are very large and 
                2) valid inference does *not* depend on having balanced groups', facts that do not seem 
                to be common knowledge [2]. As Senn says elsewhere, 'Balance is valuable as a contribution to efficiency. It has nothing to do with validity' [3]. 
                This app looks into these points and investigates a related common misconception concerning RCTs; 
                it is mistakenly thought there is no need to include baseline covariates in the analysis.
                Many RCTs are analysed in a simple manner using only the randomised treatment as the independent variable. 
                When the response outcome is continuous, 
                precision of the treatment effect estimate is improved when adjusting for baseline covariates. 
                Because of randomisation We do not expect covariates to be related to the treatment assignment, 
                but  may be related to the outcome ans so are not considered confounding. 
                Differences between the outcome which can be attributed to differences in the covariates can be removed, 
                resulting in a more precise estimate of treatment effect. This should be considered more often as sample sizes can be reduced.
                
                As Frank Harrell has said, 'unadjusted analysis makes the most severe assumptions of all (that risk factors do not exist)' [4].

                Note: The total effect of covariates has to be bounded. For example the range of human fasting blood glucose levels is approx. 70 to 130 mg/dL 
                and if we were simulating this response and adding similar covariates into a model this will result in a response the variance of which keeps on increasing 
                and soon implausible fasting blood glucose levels will result. 
                In fact a single continuous covariate could be used as a linear predictor or risk score that summarizes the multivariable contribution of a set of predictor variables [5,6]."), 

                h4("With this app Monte Carlo simulation is used to generate an RCT with patients randomised 1:1 to treatment and control with a continuous response, estimating treatment effects whilst examining adjusting and not adjusting for covariates related to the outcome, 
                covariates not related to the outcome, collinear or correlated covariates related to the outcome and imbalanced covariates both of prognostic value and unrelated to the outcome.
                As the variance of the response increases with more covariates in the simulation, it is advisable to limit the number of covariates, the default is 3. 
                As the number of simulations to get smooth curves is high, the application may time out before simulations complete. Therefore take the code and run on your own machine. 
                There are also four tabs presenting example results all using many simulations.
                Note, the prognostic strength of treatment may be small compared with patient characteristics,
                such as age as in the GUSTO-1 trial (though the GUSTO-1 response is binary) [7].
                The limited simulations I have done support adjusting over not adjusting, the mean square error being smaller when adjusting. 
                Note we have ideal data conforming to 
                statistical distributions, no missing data and all covariates are truly linear and continuous.
                "), 
                
                h3("  "), 
         sidebarLayout(
                    
                    sidebarPanel( width=3 ,
                                  
                                  tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),

                                  actionButton(inputId='ab1', label="R Shiny ",   icon = icon("th"),   
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/RCT-and-imbalance/master/RCTbalance_slimline_code/app.R', '_blank')"), 
                                  actionButton(inputId='ab1', label="R code",   icon = icon("th"),   
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/RCT-and-imbalance/master/app%20qc.R', '_blank')"),  
                                  actionButton("resample", "Simulate a new sample"),
                                  br(),  
                                  tags$style(".well {background-color:#b6aebd ;}"), 
                                  
                                  # h4("User inputs"),
                                  div(
                                      
                                      tags$head(
                                          tags$style(HTML('#ab1{background-color:orange}'))
                                      ),
                                      
                                      tags$head(
                                          tags$style(HTML('#resample{background-color:orange}'))
                                      ),
                                      
                                       splitLayout(
                                          
                                          textInput('K', 
                                                    div(h5(tags$span(style="color:blue", "No of covariates (min 2)"))), "3"),
                                          
                                          textInput('Kp', 
                                                    div(h5(tags$span(style="color:blue", "Make covariates X1:Xn prognostic"))), "2")
                                      ),
                                       
                                      textInput('Fact', 
                                                div(h5(tags$span(style="color:blue", "Covariate coefficients. Here a multiplicative factor is selected so that betas are randomly chosen between (-X*treatment effect) and (X*treatment effect)"))), "1"),
                                      
                                      
                                      tags$hr(),
                                      splitLayout(
                                          textInput('theta', 
                                                    div(h5(tags$span(style="color:blue", "Treatment effect"))), ".223"),
                                          
                                          textInput('sigma', 
                                                    div(h5(tags$span(style="color:blue", "Residual variation"))), ".85")
                                      ),
                                      
                                      splitLayout(
                                          
                                          textInput('pow', 
                                                    div(h5(tags$span(style="color:blue", "Power (%)"))), "90"),
                                          textInput('alpha', 
                                                    div(h5(tags$span(style="color:blue", "Alpha level two sided (%)"))), "5")
                                      ),
                                      tags$hr(),
                                      
                                      textInput('simuls', 
                                                div(h5(tags$span(style="color:blue", "Number of simulations (simulation tab only)"))), "99"),
                                      tags$hr(), 
                                      
                                      textInput('covar', 
                                                div(h5(tags$span(style="color:blue", "Covariate distribution 1: uniform(-1,1), 2: normal(0,1)"))), "2"),
                                      
                                      ###https://stackoverflow.com/questions/49616376/r-shiny-radiobuttons-how-to-change-the-colors-of-some-of-the-choices
                                      
                                       radioButtons(
                                          inputId = "dist",
                                          label =  div(h5(tags$span(style="color:blue","Plot choices for simulation tab 2, select to present :"))),
                                          choiceNames = list(
                                              HTML("<font color='blue'>All scenarios</font>"), 
                                              tags$span(style = "color:blue", "Covariates all of prognostic value only"), 
                                              tags$span(style = "color:blue", "Covariates all of no prognostic value only"), 
                                              tags$span(style = "color:blue", "Mix of prognostic and non prognostic covariates only"),
                                              tags$span(style = "color:blue", "Correlated covariates all of prognostic value only"),
                                              tags$span(style = "color:blue", "Imbalanced covariates all of prognostic value only"),
                                              tags$span(style = "color:blue", "Imbalanced covariates of no prognostic value only")
                                              
                                          ),
                                          choiceValues = c("All", "d1", "d3", "d5",  "d7", "d9", "d11")
                                      )
                                      
                                   ),
                                  
              ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(width=9, #eight=4,
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
                                   
                                  tabPanel( "1 Simulation - to adjust or not to adjust",
                                            
                                            h4(htmlOutput("textWithNumber1a") ),
                                            fluidRow(
                                                column(width = 6, offset = 0, style='padding:1px;',
                                                       
                                                       div(plotOutput("reg.plotx",  width=fig.width8, height=fig.height7)),
                                                       div(plotOutput("reg.ploty",  width=fig.width8, height=fig.height7)),
                                                ) ,
                                                
                                                
                                                fluidRow(
                                                    column(width = 6, offset = 0, style='padding:1px;',
                                                           
                                                    ))),#
                                            
                                            h4(paste("Here we perform simulations investigating the treatment effect estimate and associated standard error when there are
                                           covariates that are prognostic, covariates unrelated to the outcome, a mix of prognostic and covariates unrelated to the outcome, correlated covariates
                                           and imbalance prognostic covariates and imbalanced covariates of no prognostic value. For each scenario we adjust and also do not adjust for the covariates.
                                           The default number of simulations
                                           is set at a lowly 99 so that results appear quickly. It is advisable to increase this number.
                                                    The top panel shows the distribution of the treatment effect estimates, the lower panel the associated standard error estimates. The true value is shown
                                                    by the grey vertical lines. The same covariates are used for investigations of covariates with prognostic value,
                                                    covariates unrelated to the outcome, a mix of prognostic and covariates unrelated to the outcome.
                                                    For imbalanced and correlated investigations covariates will be unique. Correlations are capped at +/- 0.37.
                                                    In the case of the imbalanced scenario, an imbalance is induced for all covariates by way of the treatment arm being derived from a Normal(0.3, 1) and the control arm
                                                    from a Normal(0, 1) distribution. We can also investigate scenarios of imbalanced covariates derived from a uniform distribution Uniform(-1,1) in control
                                                    and Uniform(-0.8,1.2) in the treatment arm.

                                                 ")),
                                            
                                            h4(paste("Table 2 Summary, sorted by smallest mean squared error (MSE) estimate")),
                                           
                                            div( verbatimTextOutput("zz") )  ,
                                            h4(htmlOutput("textWithNumber99",) ),
                                            div( verbatimTextOutput("mse.target") )  ,
                                            h4(paste("Here are the true coefficients of the covariates used in the simulation: ")),
                                            div( verbatimTextOutput("betas") )  ,
                                            
                                         
                                            width = 30 )     ,
                                  
                                  
                                  tabPanel( "2 Example A results, default setting",
                                            
                                            h4(paste("Figure 3 Treatment effect estimates, betas -0.01,  0.02,  0.13, default settings 100,000 simulations (radio buttons do not work here)")),
                                            img(src='estimates100K.png', align = "right"),
                                            h4(paste("Figure 4 Standard error estimates")),
                                            img(src='se100K.png', align = "right"),
                                            h4(paste("Table 3 Summary, sorted by smallest mean squared error (MSE) estimate")),
                                            img(src='summary100K.png', align = "center"),
                                            
                                              )     ,
                                  
                                  
                                  tabPanel( "3 Example B results",
                                            
                                            h4(paste("Figure 5 Treatment effect estimates, betas -.99,  -0.4,  0.99 (no larger than 2 x trt effect); trt effect 1; residual variation 3; 50,000 simulations (radio buttons do not work here)")),
                                            img(src='trtesr2.png', align = "right"),
                                            h4(paste("Figure 6 Standard error estimates")),
                                            img(src='se.estimates2.png', align = "right"),
                                            h4(paste("Table 4 Summary, sorted by smallest mean squared error (MSE) estimate")),
                                            img(src='summary of results2.png', align = "center"),
                                            
                                  )     ,
                                  
                                  
                                  tabPanel( "4 Example C results",
                                            
                                            h4(paste("Figure 7 Treatment effect estimates, betas -.58,  0,  0.5 (no larger than 0.75 x trt effect); trt effect 1; residual variation 2; 50,000 simulations (radio buttons do not work here)")),
                                            img(src='trtesr3.png', align = "right"),
                                            h4(paste("Figure 8 Standard error estimates")),
                                            img(src='se.estimates3.png', align = "right"),
                                            h4(paste("Table 5 Summary, sorted by smallest mean squared error (MSE) estimate")),
                                            img(src='summary of results3.png', align = "center"),
                                            
                                  )     ,
                                  
                                  tabPanel( "5 Example D results",
                                            
                                            h4(paste("Figure 8 Treatment effect estimates, betas  -0.87,  0.62,  0.82 (no larger than 4 x trt effect); trt effect .223; residual variation 0.2; 10,000 simulations (radio buttons do not work here)")),
                                            img(src='estimate4.png', align = "right"),
                                            h4(paste("Figure 8 Standard error estimates")),
                                            img(src='se4.png', align = "right"),
                                            h4(paste("Table 6 Summary, sorted by smallest mean squared error (MSE) estimate")),
                                            img(src='summary4.png', align = "center"),
                                            
                                  )     ,
                                  
                                  tabPanel("6 Notes & references", value=3, 
                                           
                                           h4("First, a power calculation function in R for a ttest, using the random error, treatment effect, alpha and power is used to determine the sample size.") ,
                                           
                                           h4("Tab 1, presents the results of simulation where we investigate (i) adjusting for true prognostic covariates (i.a) ignoring them in the analysis. We investigate (ii)
                                       adjusting for covariates unrelated to the outcome (ii.a) ignoring them in the analysis. We investigate (iii) adjusting for covariates some unrelated and some related to the outcome (iii.a) 
                                       ignoring them in the analysis. We investigate (iv) adjusting for covariates related to the outcome which are correlated with each other (iv.a) ignoring them in the analysis (v) 
                                       adjusting for covariates of prognostic value that are imbalanced (v.a) ignoring them in the analysis and finally (vi) adjusting for imbalanced covariates of no prognostic value 
                                       (vi.a) ignoring them in the analysis. Plots of treatment effect estimates and standard error estimates are presented as well as a summary of simulations from which we can draw conclusions.") ,
                                           
                                                                
                                           
                                           h4("
                                          The first user input is the number of covarites to study. 
                                          The next input determines how many of the first n covariates are related to the outcome when investigating a
                                          a mix of prognostic and non prognostic covariates. 
                                          The next input determines the range over which the covariate beta coefficients are randomly selected, 
                                          using +/- multiples of the true treatment effect. The treatment effect and random error are determined 
                                          in the next two input boxes respectively. Power and the alpha level two sided can be selected using the next two boxes. 
                                          A power calculation for a ttest is executed resulting in a sample size used in the app. The next input box is used to 
                                          determine the number of simulations used in tab 1. The covarite distribution can be altered by selecting between the 
                                          two stated distributions, this option does work for the imbalanced investigations. The bottom of the user input section 
                                          allows control over which simulated scenario results are presented graphically. 
                                          We can also simulate a new sample or check the code behind the app by hitting the orange buttons.
                                          "),
                                           h4("
                                          The next four tabs present results of pre-run simulations.
                                          "),
                                           
                                           
                                           column(width = 12, offset = 0, style='padding:1px;',
                                                  
                                                  tags$hr(),
                                                  div(h4("References:")),  
                                                  tags$a(href = "https://www.bmj.com/content/bmj/340/bmj.c869.full.pdf", tags$span(style="color:blue", "[1] CONSORT 2010 Explanation and Elaboration: updated guidelines for reporting parallel group randomised trials"),),   
                                                  div(p(" ")),
                                                  tags$a(href = "https://www.linkedin.com/pulse/stop-obsessing-balance-stephen-senn/", tags$span(style="color:blue", "[2] Stephen Senn, Stop obsessing about balance"),),   
                                                  div(p(" ")),
                                                  tags$a(href = "https://discourse.datamethods.org/t/should-we-ignore-covariate-imbalance-and-stop-presenting-a-stratified-table-one-for-randomized-trials/547/32", tags$span(style="color:blue", "[3] Stephen Senn, point 4, Should we ignore covariate imbalance and stop presenting a stratified table one for randomized trials"),),  
                                                  div(p(" ")),
                                                  tags$a(href = "https://twitter.com/f2harrell/status/1298640944405807105",  tags$span(style="color:blue", "[4]  Frank Harrell, twitter, 'unadjusted analysis makes the most severe assumptions of all (that risk factors do not exist)'."),),   
                                                  div(p(" ")),
                                                  tags$a(href = "https://statistics.fas.harvard.edu/files/statistics/files/21_stephen_senn.pdf", tags$span(style="color:blue", "[5] Randomisation isn’t perfect but doing better is harder than you think "),),   
                                                  div(p(" ")),
                                                  tags$a(href = "https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.8570", tags$span(style="color:blue", "[6] Graphical calibration curves and the integrated calibration index (ICI) for survival models, Statistics in Medicine. 2020;1–29 "),),  
                                                  div(p(" ")),
                                                  tags$a(href = "https://www.sciencedirect.com/science/article/abs/pii/S0002870300900012?via%3Dihub", tags$span(style="color:blue", "[7] Steyerberg, E. W., Bossuyt, P. M. M., & Lee, K. L. (2000). Clinical trials in acute myocardial infarction: Should we adjust for baseline characteristics? American Heart Journal, 139(5), 745–751. doi:10.1016/s0002-8703(00)90001-2"),),   
                                                  div(p(" ")),
                                                  tags$a(href = "https://twitter.com/f2harrell/status/1299755896319475712", tags$span(style="color:blue", "[8] Frank Harrell, twitter, Adjusted analysis"),),   
                                                  div(p(" ")),
                                                  tags$a(href = "https://discourse.datamethods.org/t/guidelines-for-covariate-adjustment-in-rcts/2814/2", tags$span(style="color:blue", "[9] Frank Harrell, Guidelines for covariate adjustment in rcts"),),  
                                                  div(p(" ")),
                                                  tags$a(href = "https://www.fharrell.com/post/covadj/", tags$span(style="color:blue", "[10] E.Steyerberg explains some of the advantages of conditioning on covariates"),),  
                                                  div(p(" ")),
                                               
                                                  tags$hr()
                                           ) 
                                           
                                  )
                                  
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   END NEW   
                              )
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                ) 
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- shinyServer(function(input, output   ) {
    
    shinyalert("Welcome! \nAdjusting for covariates in continuous response RCT!",
               "Best to do it!", 
               type = "info")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # This is where a new sample is instigated and inputs converted to numeric
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    random.sample <- reactive({
        
        foo <- input$resample
        
        K <- as.numeric(unlist(strsplit(input$K,",")))
        
        Kp <- as.numeric(unlist(strsplit(input$Kp,",")))
        
        pow <- as.numeric(unlist(strsplit(input$pow,",")))
        
        sigma <- as.numeric(unlist(strsplit(input$sigma,",")))
        
        alpha <- as.numeric(unlist(strsplit(input$alpha,",")))
        
        theta <- (as.numeric(unlist(strsplit(input$theta,","))))    
        
        simuls <- (as.numeric(unlist(strsplit(input$simuls,","))))    
        
        covar <- (as.numeric(unlist(strsplit(input$covar,","))))   
        
        Fact <- (as.numeric(unlist(strsplit(input$Fact,","))))
        
        return(list(  
            K=K,  
            Kp=Kp,  
            pow=pow/100,
            sigma=sigma, 
            alpha=alpha/100, 
            theta=theta,
            simuls=simuls,
            covar=covar,
            Fact=Fact
        ))
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tab 1 simulate data (covariates and response)  
    # create  response with prognostic covariate
    # create covariates that are not prognostic
    # create a mix of above 2
    # alos look at the difference of the covariates across arms
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mcmc <- reactive({

        sample <- random.sample()

        K=sample$K
        Kp=sample$Kp
        pow=sample$pow
        sigma=sample$sigma
        theta=sample$theta
        alpha=sample$alpha
        covar=sample$covar
        Fact=sample$Fact
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #power the examples
        Po <- power.t.test( delta =theta, sd=sigma, sig.level=alpha,
                            power=pow, type="two.sample", alternative=c("two.sided"))

        MM <- N <-ceiling(Po$n)*2  # total will always be even

        bigN <- MM  
        N1=MM/2
        N2=N1
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # For the se of diff, n1=n2 , a simulation may have different numbers allocated to treatments 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        return(list(  
  
                      Na=N1,
                      Nb=N2,
                      N=N, 
                      bigN=bigN,
                      sigma=sigma
                      
                      ))
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # SIMULATION CODE STARTS HERE
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # here is code to simulate scenarios prognostic covariates, covariates unrelated to y, mix of pro and unrelated to y covariates, 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    simul <- reactive({
        
        sample <- random.sample()
        # need to rename to avoid recursive issues
        K1=sample$K
        Kp=sample$Kp
        pow=sample$pow
        sigma1=sample$sigma
        theta1=sample$theta        
        alpha=sample$alpha  
        covar=sample$covar
        Fact=sample$Fact
        
        simuls=sample$simuls
        
        N1 <- mcmc()$N # 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        # making up some beta coefficients, fixed for all simulations as it is outside loop
        b1 <- round(sort(runif(K1, -theta1*Fact,theta1*Fact)), digits=2) 
        
        
        simfun <- function(N=N1, K=K1, a=1, sigma=sigma1, theta=theta1, b=b1) {
            
            # we can select this, does not seem to have a big impact
            if (covar==1) {  
                X <- array(runif(N*K , -1,1), c(N,K))     # initially covars were uniform dist
            } else {
                X <- array(rnorm(N*K, 0, 1), c(N,K))  
            }
            
            z <- sample(c(0,1), N, replace=T)                           # treatment indicators
            y <-  a+ X %*% b + theta*z + rnorm(N,0, sigma)              # linear predictor
            y2 <- a+           theta*z + rnorm(N,0, sigma)              # linear predictor
            y3 <- a+ X[,1:Kp] %*% b[1:Kp] + theta*z + rnorm(N,0, sigma) # linear predictor
            
            data.frame(X=X, y=y, z=z, y2=y2, y3=y3)
            
        }
        
        #https://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame
        
        # function to run the analyses
        statfun <- function(d) {
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~all prognostic
            zz <- lm(y~.-y2-y3, data=d)     ## adjusting for prognostic X, y2 is not included by use of the '-'
            f <-  summary(zz)
            
            zz1 <- lm(y~z, data=d)          ## not adjusting for prognostic X, only trt. indicator included
            f1 <-  summary(zz1)
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~mix
            zz2 <- lm(y2~.-y-y3, data=d)    ## adjusting for X which are not prognostic
            f2 <-  summary(zz2)
            
            zz3 <- lm(y2~z, data=d)         ## not adjusting for X which are not prognostic
            f3 <-  summary(zz3)
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#some prognostic
            zz4 <- lm(y3~.-y-y2, data=d)    ## adjusting some X  are prognostic
            f4 <-  summary(zz4)
            
            zz5 <- lm(y3~z, data=d)         ## not adjusting some X  are prognostic
            f5 <-  summary(zz5)
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~collect estimates
            
            cbind(
                
                #f$coefficients [,1]["z"],
                coef(f)["z", "Estimate"],
                coef(f)["z", "Std. Error"],
                
                coef(f1)["z", "Estimate"],
                coef(f1)["z", "Std. Error"],
                
                coef(f2)["z", "Estimate"],
                coef(f2)["z", "Std. Error"],
                
                coef(f3)["z", "Estimate"],
                coef(f3)["z", "Std. Error"], #8
                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~new
                coef(f4)["z", "Estimate"],
                coef(f4)["z", "Std. Error"],
                
                coef(f5)["z", "Estimate"],
                coef(f5)["z", "Std. Error"],
                
                # collect p values
                coef(f)["z", "Pr(>|t|)"]  < alpha,  #13
                coef(f1)["z", "Pr(>|t|)"] < alpha,
                coef(f2)["z", "Pr(>|t|)"] < alpha,
                coef(f3)["z", "Pr(>|t|)"] < alpha,
                coef(f4)["z", "Pr(>|t|)"] < alpha,
                coef(f5)["z", "Pr(>|t|)"] < alpha  , #18
                ## mse
                mean((d$y-predict(zz))^2),   #19
                mean((d$y-predict(zz1))^2),
                mean((d$y2-predict(zz2))^2),
                mean((d$y2-predict(zz3))^2),
                mean((d$y3-predict(zz4))^2),
                mean((d$y3-predict(zz5))^2),   #24
                
                mean(quantile( (d$y-predict(zz))^2, .025)), #25
                mean(quantile( (d$y-predict(zz))^2, .975)), 
                mean(quantile( (d$y-predict(zz1))^2, .025)), 
                mean(quantile( (d$y-predict(zz1))^2, .975)),
                mean(quantile( (d$y2-predict(zz2))^2, .025)), #29
                mean(quantile( (d$y2-predict(zz2))^2, .975)), 
                mean(quantile( (d$y2-predict(zz3))^2, .025)), 
                mean(quantile( (d$y2-predict(zz3))^2, .975)),
                mean(quantile( (d$y3-predict(zz4))^2, .025)), 
                mean(quantile( (d$y3-predict(zz4))^2, .975)), 
                mean(quantile( (d$y3-predict(zz5))^2, .025)), 
                mean(quantile( (d$y3-predict(zz5))^2, .975)),   #36
                
                f$sigma,  #37
                f1$sigma,
                f2$sigma,
                f3$sigma,
                f4$sigma,
                f5$sigma ,   #42
                
                f$adj.r.squared,  #43
                f1$adj.r.squared,
                f2$adj.r.squared,
                f3$adj.r.squared,
                f4$adj.r.squared,
                f5$adj.r.squared    #48
                
            )
            
        }
        
        library(plyr)
        res <- raply(simuls, statfun(simfun())) # run the model many times
        # summarize
        result <- apply(res,2,mean)
        q1.result <- apply(res,2, quantile, probs=c(0.025), na.rm=TRUE)
        q2.result <- apply(res,2, quantile, probs=c(0.975), na.rm=TRUE)
        # collect
        return(list(  
            
            res=res,
            result=result ,
            q1.result=q1.result,
            q2.result=q2.result,
            b1=b1
            
        )) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$sim1 <- renderPrint({
        
        return(simul()$result)
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # simulation code 2 do the same as the first simulation code, but this time correlated covariates are created
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    simul2 <- reactive({
        
        sample <- random.sample()
        # need to rename to avoid recursive issues
        K1=sample$K
        Kp=sample$Kp
        pow=sample$pow
        sigma1=sample$sigma
        theta1=sample$theta        
        alpha=sample$alpha  
        Fact=sample$Fact
        
        simuls=sample$simuls
        
        b1=simul()$b1  #cal in same beta coefficient as first simulation
        
        N1 <- mcmc()$N # 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # simulate models many times collect estimate and SE
        
        simfun2<- function(N=N1, K=K1, a=1, sigma=sigma1, theta=theta1, b=b1) {
            
            x <- Matrix(runif(K*K,-RR,RR), K)   # create a correlation matrix randomly , wont allow very high correlations
            
            A <- forceSymmetric(x)
            
            diag(A) <- 1
            
            M <- A
            
            M <- nearPD(M, conv.tol = 1e-7)$mat # default
            # Cholesky decomposition
            L = chol(M)
            nvars = dim(L)[1]
            
            # Random variables that follow an M correlation matrix
            r = t(L) %*% matrix(rnorm(nvars*N, 0,1), nrow=nvars, ncol=N)  #2,2
            r = t(r)
            
            r <- as.matrix(r)#
            rdata <- as.data.frame(r)
            XX<- as.matrix(rdata)
            z <- sample(c(0,1), N, replace=T)                # treatment indicator
            y <- a+ XX %*% b + theta*z + rnorm(N,0, sigma)   # betas created earlier
            data.frame(X=XX, y=y, z=z)
            
        }
        
        #https://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame
        
        statfun2 <- function(d) {
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            zz <- lm(y~., data=d)       ## adjusting for all X and z
            f <-  summary(zz)
            
            zz1 <- lm(y~z, data=d)      ## ignoring covariates
            f1 <-  summary(zz1)
            
            cbind(
                
                coef(f)["z", "Estimate"],
                coef(f)["z", "Std. Error"],
                coef(f1)["z", "Estimate"],
                coef(f1)["z", "Std. Error"], 
                coef(f)["z", "Pr(>|t|)"]  < alpha,
                coef(f1)["z", "Pr(>|t|)"]  < alpha,
                
                ## mse
                mean((d$y-predict(zz))^2),
                mean((d$y-predict(zz1))^2),
                mean(quantile( (d$y-predict(zz))^2, .025)), 
                mean(quantile( (d$y-predict(zz))^2, .975)), 
                mean(quantile( (d$y-predict(zz1))^2, .025)), 
                mean(quantile( (d$y-predict(zz1))^2, .975)),
                
                f$sigma,  #13
                f1$sigma,
                
                f$adj.r.squared,  #44
                f1$adj.r.squared  #45
                
            )
            
        }
        
        
        library(plyr)
        res <- raply(simuls, statfun2(simfun2())) # run the model many times
        result <- apply(res,2,mean)
        q1.result <- apply(res,2, quantile, probs=c(0.025), na.rm=TRUE)
        q2.result <- apply(res,2, quantile, probs=c(0.975), na.rm=TRUE)
        
        return(list(  
            
            res=res,
            result=result,  # means
            q1.result=q1.result,
            q2.result=q2.result,
            betas=b1
            
        )) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    })
   
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # do the same as the first simulation code, but this time imbalanced covariates are created
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    simul3 <- reactive({
        
        sample <- random.sample()
        # need to rename to avoid recursive issues
        K1=sample$K
        Kp=sample$Kp
        pow=sample$pow
        covar=sample$covar
        sigma1=sample$sigma
        theta1=sample$theta        
        alpha=sample$alpha  
        Fact=sample$Fact
        
        simuls=sample$simuls
        
        b1=simul()$b1  #cal in same beta coefficient as first simulation
        
        N1 <- mcmc()$N # total
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # simulate models many times collect estimate and SE
        
        simfun3<- function(N=N1, K=K1, a=1, sigma=sigma1, theta=theta1, b=b1) {
            
            MM = N
            N2=MM/2
            N1=N2 
            
            if (covar==1) {  
                X1 <- array(runif(N1*K , -1,1), c(N1,K))  
                X2 <- array(runif(N2*K , -.8,1.2), c(N2,K))   ##imbalance compared to above
                XY <- X <- rbind(X1,X2)
            } else {
                X1 <- array(rnorm(N1*K, 0,  1), c(N1,K))  
                X2 <- array(rnorm(N2*K, .3, 1), c(N2,K))   ##imbalance compared to above
                XY <- X <- rbind(X1,X2)
            }
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            z <- rep(0:1, c(N1,N2))  #assign 1 so we maintain shift in arms
            
            a <- 1                                     # intercept
            
            # b coefficient are generated earlier 
            y <- a+ X %*% b + theta*z + rnorm(MM,0, sigma)  # note I use M here
   
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # create y covariates not associated with y
            y2 <- a +  theta*z + rnorm(MM,0, sigma)    # note I use M here
   
            data.frame(X=XY, y=y, z=z, y2=y2)
            
        }
        
        #https://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame
        
        statfun3 <- function(d) {
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            zz <- lm(y~.-y2, data=d)     ## adjusting for prog imbalance X, y2 is not included by use of the '-'
            f <-  summary(zz)
            
            zz1 <- lm(y~z, data=d)          ## not adjusting for imbalance prog X, only trt. indicator included
            f1 <-  summary(zz1)
            
            zz2 <- lm(y2~.-y, data=d)    ## adjusting for X which are not prog but imbalanced
            f2 <-  summary(zz2)
            
            zz3 <- lm(y2~z, data=d)         ## not adjusting for X which are not prog but imbalanced
            f3 <-  summary(zz3)
            
            
            cbind(
                
                coef(f)["z", "Estimate"],
                coef(f)["z", "Std. Error"],
                
                coef(f1)["z", "Estimate"],
                coef(f1)["z", "Std. Error"],
                
                coef(f2)["z", "Estimate"],
                coef(f2)["z", "Std. Error"],
                
                coef(f3)["z", "Estimate"],
                coef(f3)["z", "Std. Error"], #8
                
                # collect p values for power
                coef(f)["z", "Pr(>|t|)"]  < alpha,  #9
                coef(f1)["z", "Pr(>|t|)"] < alpha,
                coef(f2)["z", "Pr(>|t|)"] < alpha,
                coef(f3)["z", "Pr(>|t|)"] < alpha,
                
                ## mse
                mean((d$y-predict(zz))^2),   #14
                mean((d$y-predict(zz1))^2),
                mean((d$y2-predict(zz2))^2),
                mean((d$y2-predict(zz3))^2),
                
                
                mean(quantile( (d$y-predict(zz))^2, .025)), #19
                mean(quantile( (d$y-predict(zz))^2, .975)), 
                mean(quantile( (d$y-predict(zz1))^2, .025)), 
                mean(quantile( (d$y-predict(zz1))^2, .975)),
                mean(quantile( (d$y2-predict(zz2))^2, .025)), #23
                mean(quantile( (d$y2-predict(zz2))^2, .975)), 
                mean(quantile( (d$y2-predict(zz3))^2, .025)), 
                mean(quantile( (d$y2-predict(zz3))^2, .975)),
                
                f$sigma,  #27
                f1$sigma,
                f2$sigma,
                f3$sigma, #30
                
                f$adj.r.squared,  #31
                f1$adj.r.squared,
                f2$adj.r.squared,
                f3$adj.r.squared #34
                
            )
            
        }
        
        library(plyr)
        res <- raply(simuls, statfun3(simfun3())) # run the model many times
        # summarize
        result <- apply(res,2,mean)
        q1.result <- apply(res,2, quantile, probs=c(0.025), na.rm=TRUE)
        q2.result <- apply(res,2, quantile, probs=c(0.975), na.rm=TRUE)
        # collect
        return(list(  
            
            res=res,
            result=result ,
            q1.result=q1.result,
            q2.result=q2.result,
            b1=b1
            
        )) 
        
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # simulation plots
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # collect simulation trt effect estimates from simulation and plot!
    
    output$reg.plotx <- renderPlot({         #means
        
        # Get the  data
        
        res <- simul()$res
        res2 <- simul2()$res
        res3 <- simul3()$res
        
        sample <- random.sample()
        theta1=sample$theta     
        
        d1 <-  density(res[,1] )
        d2 <-  density(res[,3] )
        d3 <-  density(res[,5] )
        d4 <-  density(res[,7] )
        d5 <-  density(res[,9] )
        d6 <-  density(res[,11] )
        d7 <-  density(res2[,1] )
        d8 <-  density(res2[,3] )
        d9 <-   density(res3[,1] )
        d10 <-  density(res3[,3] )
        d11 <-  density(res3[,5] )
        d12 <-  density(res3[,7] )
        
        dz <- max(c(d1$y, d2$y, d3$y, d4$y, d5$y, d6$y, d7$y, d8$y  , d9$y, d10$y, d11$y, d12$y  ))
        dx <- range(c(d1$x,d2$x,  d3$x, d4$x, d5$x, d6$x, d7$x, d8$x   , d9$x, d10$x, d11$x, d12$x  ))
        
        if (input$dist %in% "All") {
            
            plot((d1), xlim = dx, main=paste0("Density of treatment estimates, truth= ",p3(theta1),""), ylim=c(0,dz),lty=wz, lwd=ww,
                 xlab="Treatment effect",  
                 ylab="Density")                           
            lines( (d2), col = "black", lty=w, lwd=ww)  
            lines( (d3), col = "red", lty=wz, lwd=ww)    
            lines( (d4), col = "red", lty=w, lwd=ww)          
            lines( (d5), col = "blue", lty=wz, lwd=ww)       
            lines( (d6), col = "blue", lty=w, lwd=ww)       
            lines( (d7), col = "purple", lty=wz, lwd=ww)       
            lines( (d8), col = "purple", lty=w, lwd=ww)       
            
            lines( (d9), col = "green", lty=wz, lwd=ww)       
            lines( (d10), col = "green", lty=w, lwd=ww)       
            lines( (d11), col = "grey", lty=wz, lwd=ww)       
            lines( (d12), col = "grey", lty=w, lwd=ww)  
            
        }
        
        else if (input$dist %in% "d1") {  #remove
            
            
            plot((d1), xlim = dx, main=paste0("Density of treatment estimates, truth= ",p3(theta1),""), ylim=c(0,dz),lty=wz, lwd=ww,
                 xlab="Treatment effect",  
                 ylab="Density")  
            lines( (d2), col = "black", lty=w, lwd=ww)  
            
        }
        
        else if (input$dist %in% "d3") {  #remove
            
            
            plot((d3), xlim = dx, main=paste0("Density of treatment estimates, truth= ",p3(theta1),""), ylim=c(0,dz),lty=wz, lwd=ww,col="red",
                 xlab="Treatment effect",  
                 ylab="Density")               
            lines( (d4), col = "red", lty=w, lwd=ww)          
            
        }
        
        else if (input$dist %in% "d5") {
            
            plot((d5), xlim = dx, main=paste0("Density of treatment estimates, truth= ",p3(theta1),""), ylim=c(0,dz),lty=wz, lwd=ww, col="blue",
                 xlab="Treatment effect",  
                 ylab="Density")                    
            
            lines( (d6), col = "blue", lty=w, lwd=ww)       
            
        }
        
        else if (input$dist %in% "d7") {
            
            plot((d7), xlim = dx, main=paste0("Density of treatment estimates, truth= ",p3(theta1),""), ylim=c(0,dz),lty=wz, lwd=ww, col="purple",
                 xlab="Treatment effect", 
                 ylab="Density") 
            
            lines( (d8), col = "purple", lty=w, lwd=ww)     
            
        }
        else if (input$dist %in% "d9") {
            
            plot((d9), xlim = dx, main=paste0("Density of treatment estimates, truth= ",p3(theta1),""), ylim=c(0,dz),lty=wz, lwd=ww, col="green",
                 xlab="Treatment effect", 
                 ylab="Density")  
            
            lines( (d10), col = "green", lty=w, lwd=ww)     
            
        }
        
        else if (input$dist %in% "d11") {
            
            plot((d11), xlim = dx, main=paste0("Density of treatment estimates, truth= ",p3(theta1),""), ylim=c(0,dz),lty=wz, lwd=ww, col="grey",
                 xlab="Treatment effect",  
                 ylab="Density")  
            
            lines( (d12), col = "grey", lty=w, lwd=ww)     
            
        }
        
        
        abline(v = theta1, col = "darkgrey")                
        legend("topright",       # Add legend to density
               legend = c(" adj. for true prognostic covariates", 
                          " not adj. for true prognostic covariates" ,
                          " adj. for covariates unrelated to outcome", 
                          " not adj. for covariates unrelated to outcome",
                          " adj. for mix of prognostic and unrelated to outcome", 
                          " not adj. mix of prognostic and unrelated to outcome", 
                          " adj. for correlated prognostic covariates", 
                          " not adj. for correlated prognostic covariates",
                          " adj. for imbalanced prognostic covariates", 
                          " not adj. for imbalanced prognostic covariates", 
                          " adj. for imbalanced covariates unrelated to outcome", 
                          " not adj. imbalanced covariates unrelated to outcome"
                          
               ),
               col = c("black", "black","red","red","blue", "blue", "purple", "purple", "green", "green", "grey", "grey"),
               lty = c(wz, w,wz,w,wz,w,wz,w,wz,w,wz,w)  ,lwd=ww
               , bty = "n", cex=1)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # SIMUALTION PLOT
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # collect simulation trt effect standard error estimates from simulation and plot!
    
    output$reg.ploty <- renderPlot({         #standard errors
        
        # Get the  data
        
        res <- simul()$res
        res2 <- simul2()$res
        res3 <- simul3()$res
        
        sample <- random.sample()
        sigma1=sample$sigma
        N1 <- mcmc()$N # 
        
        n1 <- mcmc()$Na
        n2 <- mcmc()$Nb
        
        d1 <-  density(res[,2] )
        d2 <-  density(res[,4] )
        d3 <-  density(res[,6] )
        d4 <-  density(res[,8] )
        d5 <-  density(res[,10] )
        d6 <-  density(res[,12] )
        d7 <-  density(res2[,2] )
        d8 <-  density(res2[,4] )
        
        d9 <-   density(res3[,2] )
        d10 <-  density(res3[,4] )
        d11 <-  density(res3[,6] )
        d12 <-  density(res3[,8] )
        
        # we may have imbalance in numbers, otherwise the se will not be exactly correct and this maybe seen in plot
        se. <-  sqrt( sigma1^2/n1 + sigma1^2/n2 )   #ditto
        
        dz <- max(c(d1$y, d2$y, d3$y, d4$y, d5$y, d6$y, d7$y, d8$y  , d9$y, d10$y, d11$y, d12$y    ))
        dx <- range(c(d1$x,d2$x,  d3$x, d4$x, d5$x, d6$x, d7$x, d8$x   , d9$x, d10$x, d11$x, d12$x    ))
        
        if (input$dist %in% "All") {
            
            plot( (d1), xlim = c(dx), main=paste0("Density of treatment standard error estimates, truth= ",p4(se.),""), ylim=c(0,dz),lty=wz, lwd=ww,
                  xlab="Standard error",  
                  ylab="Density")  
            lines( (d2), col = "black", lty=w, lwd=ww)  
            lines( (d3), col = "red", lty=wz, lwd=ww)    
            lines( (d4), col = "red", lty=w, lwd=ww)          
            lines( (d5), col = "blue", lty=wz, lwd=ww)       
            lines( (d6), col = "blue", lty=w, lwd=ww)       
            lines( (d7), col = "purple", lty=wz, lwd=ww)       
            lines( (d8), col = "purple", lty=w, lwd=ww)       
            
            lines( (d9), col = "green", lty=wz, lwd=ww)       
            lines( (d10), col = "green", lty=w, lwd=ww)       
            lines( (d11), col = "grey", lty=wz, lwd=ww)       
            lines( (d12), col = "grey", lty=w, lwd=ww)  
            
        }
        
        
        else if (input$dist %in% "d1") {  
            
            plot((d1), xlim = dx, main=paste0("Density of treatment standard error estimates, truth= ",p3(se.),""), ylim=c(0,dz),lty=wz, lwd=ww,
                 xlab="Treatment effect",  
                 ylab="Density") 
            lines( (d2), col = "black", lty=w, lwd=ww)  
            
        }
        
        else if (input$dist %in% "d3") {  
            
            
            plot((d3), xlim = dx, main=paste0("Density of treatment standard error estimates, truth= ",p3(se.),""), ylim=c(0,dz),lty=wz, lwd=ww,col="red",
                 xlab="Treatment effect", 
                 ylab="Density")  
            lines( (d4), col = "red", lty=w, lwd=ww)          
            
        }
        
        else if (input$dist %in% "d5") {
            
            plot((d5), xlim = dx, main=paste0("Density of treatment standard error estimates, truth= ",p3(se.),""), ylim=c(0,dz),lty=wz, lwd=ww, col="blue",
                 xlab="Treatment effect",  
                 ylab="Density")  
            
            lines( (d6), col = "blue", lty=w, lwd=ww)       
            
        }
        
        else if (input$dist %in% "d7") {
            
            plot((d7), xlim = dx, main=paste0("Density of treatment standard error estimates, truth= ",p3(se.),""), ylim=c(0,dz),lty=wz, lwd=ww, col="purple",
                 xlab="Treatment effect",  
                 ylab="Density") 
            
            lines( (d8), col = "purple", lty=w, lwd=ww)     
            
        }
        
        else if (input$dist %in% "d9") {
            
            plot((d9), xlim = dx, main=paste0("Density of treatment standard error estimates, truth= ",p3(se.),""), ylim=c(0,dz),lty=wz, lwd=ww, col="green",
                 xlab="Treatment effect",  
                 ylab="Density")  
            
            lines( (d10), col = "green", lty=w, lwd=ww)     
            
        }
        
        else if (input$dist %in% "d11") {
            
            plot((d11), xlim = dx, main=paste0("Density of treatment standard error estimates, truth= ",p3(se.),""), ylim=c(0,dz),lty=wz, lwd=ww, col="grey",
                 xlab="Treatment effect",  
                 ylab="Density")  
            
            lines( (d12), col = "grey", lty=w, lwd=ww)     
            
        }
        
        abline(v = se., col = "darkgrey")   
        legend("topright",           # Add legend to density
               legend = c(" adj. for true prognostic covariates", 
                          " not adj. for true prognostic covariates" ,
                          " adj. for covariates unrelated to outcome", 
                          " not adj. for covariates unrelated to outcome",
                          " adj. for mix of prognostic and unrelated to outcome", 
                          " not adj. mix of prognostic and unrelated to outcome", 
                          " adj. for correlated prognostic covariates", 
                          " not adj. for correlated prognostic covariates",
                          " adj. for imbalanced prognostic covariates", 
                          " not adj. for imbalanced prognostic covariates", 
                          " adj. for imbalanced covariates unrelated to outcome", 
                          " not adj. imbalanced covariates unrelated to outcome"
                          
               ),
               col = c("black", "black","red","red","blue", "blue", "purple", "purple", "green", "green", "grey", "grey"),
               lty = c(wz, w,wz,w,wz,w,wz,w,wz,w,wz,w) ,lwd=ww
               , bty = "n", cex=1)
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$textWithNumber99 <- renderText({ 
        
        HTML(
            "Mean squared error (MSE: accuracy and precision) combines bias and
                    variance as (bias*bias+variance). It represents the total variation around the
                    true value, rather than the average estimated value. MSE gives an overall sense of the quality of the
                    estimator. As the MSE can be written as the sum of the variance of the estimator and the squared bias of the estimator, 
                    this implies that in the case of unbiased estimators, the MSE and variance are equivalent. So compare the calculated MSE to the 
                    true sigma squared 
                    on the left input and printed here:"
        )
        
    })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # table for simulation summary
    table.sim <- reactive({
        
        res <- simul()$res  
        result <- simul()$result  
        result2 <- simul2()$result  
        result3 <- simul3()$result  
        
        q1.result <- simul()$q1.result  
        q2.result <- simul()$q2.result  
        
        q1.result2 <- simul2()$q1.result  
        q2.result2 <- simul2()$q2.result  
        
        q1.result3 <- simul3()$q1.result  
        q2.result3 <- simul3()$q2.result  

        zz <- rbind(
          (c( p4(result[1])   ,     p2(q1.result[1])  ,  p2(q2.result[1])   , p4(result[2] ) ,  p4(result[13] ) ,  p4(result[19] ) ,      p4(result[37] )    ,  p4(result[43] )         )) ,
          (c( p4(result[3])   ,     p2(q1.result[3]) ,   p2(q2.result[3])   , p4(result[4] ) ,  p4(result[14] ) ,  p4(result[20] ) ,      p4(result[38] )    ,  p4(result[44] )        )) ,
          (c( p4(result[5])   ,     p2(q1.result[5]) ,   p2(q2.result[5])   , p4(result[6] ) ,  p4(result[15] ) ,  p4(result[21] ) ,      p4(result[39] )    ,  p4(result[45] )        )) ,
          (c( p4(result[7])   ,     p2(q1.result[7]) ,   p2(q2.result[7])   , p4(result[8] ) ,  p4(result[16] ) ,  p4(result[22] ) ,      p4(result[40] )    ,  p4(result[46] )         )) ,
          (c( p4(result[9])   ,     p2(q1.result[9]) ,   p2(q2.result[9])   , p4(result[10] ) , p4(result[17] ) ,  p4(result[23] ) ,      p4(result[41] )    ,  p4(result[47] )           )) ,
          (c( p4(result[11])  ,     p2(q1.result[11]) ,  p2(q2.result[11])  , p4(result[12] ) , p4(result[18] ) ,  p4(result[24] ) ,      p4(result[42] )    ,  p4(result[48] )         )) ,
          (c( p4(result2[1])  ,     p2(q1.result2[1]),   p2(q2.result2[1])  , p4(result2[2] ) , p4(result2[5] ) ,  p4(result2[7] ) ,      p4(result2[13] )   ,  p4(result2[15] )         )) ,
          (c( p4(result2[3])  ,     p2(q1.result2[3])  , p2(q2.result2[3])  , p4(result2[4] ) , p4(result2[6] ) ,  p4(result2[8] ) ,      p4(result2[14] )   ,  p4(result2[16] )          )),
          (c( p4(result3[1])  ,     p2(q1.result3[1])  , p2(q2.result3[1])   , p4(result3[2] ) ,  p4(result3[9] ) ,  p4(result3[13] ) , p4(result3[25] )   ,  p4(result3[29] )         )) ,
          (c( p4(result3[3])  ,     p2(q1.result3[3]) ,  p2(q2.result3[3])   , p4(result3[4] ) ,  p4(result3[10] ) ,  p4(result3[14] ) ,p4(result3[26] )   ,  p4(result3[30] )         )) ,
          (c( p4(result3[5])  ,     p2(q1.result3[5]) ,  p2(q2.result3[5])   , p4(result3[6] ) ,  p4(result3[11] ) ,  p4(result3[15] ) , p4(result3[27] )  ,  p4(result3[31] )         )) ,
          (c( p4(result3[7])  ,     p2(q1.result3[7]) ,  p2(q2.result3[7])   , p4(result3[8] ) ,  p4(result3[12] ) ,  p4(result3[16] ) , p4(result3[28] )  ,  p4(result3[32]  )         )) 
        ) 
        
        zz <- as.data.frame(zz)
        
        colnames(zz) <- c("Mean  ", "Lower 95%CI", "Upper 95%CI", "Stand.error", "Power ","B" , "sigma","R2")
        
        zz <- data.frame(lapply(zz, function(x) as.numeric(as.character(x))))
        zz <- as.data.frame(zz)
        rownames(zz)<- c(
            " adj. for true prognostic covariates", 
            " not adj. for true prognostic covariates" ,
            " adj. for non prognostic covariates", 
            " not adj. for non prognostic covariates",
            " adj. for some non prognostic covariates", 
            " not adj. when some prognostic covariates", 
            " adj. for correlated prognostic covariates", 
            " not adj. for correlated prognostic covariates",
            " adj. for imbalanced prognostic covariates",
            " not adj. for imbalanced prognostic covariates",  
            " adj. for non prognostic imbalanced covariates",
            " not adj. for non prognostic imbalanced covariates"
        )
        zz <- zz[order(zz$B),]
        
        colnames(zz) <- c("Mean  ", "Lower 95%CI", "Upper 95%CI", "Std.error", "Power ","MSE" , "sigma","R2")
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        return(list(  
            
            zz=zz
            
        )) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    })
    
    
    output$zz <- renderPrint({
      
      d <- table.sim()$zz
      
      return(d)
    })
    ##~~~~~~~~~~~~~
  
    
    output$textWithNumber1a <- renderText({ 
        
        placebo <- mcmc()$placebo
        treated <- mcmc()$treated
        bigN <- mcmc()$bigN
        
        HTML(paste0(  #tags$hr(),
            "Figure 1 Simulation results. Randomised 1:1, we have  "  
            
            ,tags$span(style="color:red",  bigN  ),
            " total patients randomised 1:1 for each simulation. The true covariate coefficients are fixed at the same values for all simulations
                      and are selected randomly between +/- multiples of the treatment effect, as dictated by the input on left. The true covariate coefficients are printed at the bottom."
            
        ))    
        
    })
    
 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # text 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    output$betas <- renderPrint({
        
        d <- simul2()$betas
        return(print(d))
        
    })
  
 
    
    output$mse.target <- renderPrint({
      
      d <- mcmc()$sigma
      
      return(print(d^2)) 
      
    })
 
}) 

# Run the application 
shinyApp(ui = ui, server = server)