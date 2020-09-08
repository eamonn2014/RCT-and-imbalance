#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#  It follows that covariate imbalance, contrary
 #to what has been claimed by Altman, is just as much of a problem for large Studies as for
# small ones
#https://twitter.com/f2harrell/status/1299755896319475712
#https://twitter.com/f2harrell/status/1298640944405807105
#'ut adjusted estimation does not have to be robust to be a major improvement over unadjusted analysis.  Unadjusted analysis makes the most severe assumptions of all (that risk factors do not exist). '
#Using observed imbalances to find covariates to adjust for is arbitrary and reduces power by maximizing co-linearity with treatment

#https://discourse.datamethods.org/t/should-we-ignore-covariate-imbalance-and-stop-presenting-a-stratified-table-one-for-randomized-trials/547/32
#'randomisation entitles us to ignore covariates we have not measured.'
# To me the goal of a parallel-group randomized clinical trial is to answer this question: do two patients starting out at the same point 
# (same age, severity of disease, etc.), one on treatment A and one on treatment B, end up with the same expected outcomes? This is fundamentally a completely conditional model.


##https://discourse.datamethods.org/t/guidelines-for-covariate-adjustment-in-rcts/2814/2
#' Can you reconcile these two points?
#'   
#'   @f2harrell
#' : One covariate imbalance is likely to be counterbalanced by another in opposite direction.
#' 
#' @stephensenn
#' : One covariate imbalance likely coincides with other imbalances in same direction (thus, adjusting for one adjusts for them all)
#' Stephen John Senn
#' @stephensenn
#' ·
#' 22 Aug 2019
#' 1/2 So the first is true(ish) of unobserved covariates. The second  covers the fact that given observed  imbalance/balance in one covariate there may be imbalance/balance in another. So what?!
#
# Stop obsessing about balance
# 1) RCTs don't deliver balance even if they are very large 2) valid inference does not depend on having balanced groups
#https://twitter.com/f2harrell/status/1303002649080532995
#Unadjusted estimates are biased in comparison with adjusted estimates from nonlinear models, a fact well studied for decades.  Some do not call this 'bias' but the damage is the same. Literature is summarized in my ANCOVA chapter in http://hbiostat.org/doc/bbr.pdf


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#https://discourse.datamethods.org/t/should-we-ignore-covariate-imbalance-and-stop-presenting-a-stratified-table-one-for-randomized-trials/547/2
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
 
fig.width <- 1200
fig.height <- 500
fig.width1 <- 1380
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
is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful

options(width=200)
options(scipen=999)
w=1  # line type
ww=3 # line thickness

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
ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                # paper
                useShinyalert(),  # Set up shinyalert
                setBackgroundColor(
                    color = c( "#2171B5", "#F7FBFF"), 
                    gradient = "linear",
                    direction = "bottom"
                ),
                
                h2("Covariate adjustment in randomised controlled trials (RCTs) with a continuous response"), 
                
           
                h4("Stephen Senn's truisims '1) randomised controlled trials don't deliver balance even if they are very large 2) valid inference does not depend on having balanced groups' do not seem 
                to be common knowledge [1]. As Senn says elsewhere, 'Balance is valuable as a contribution to efficiency. It has nothing to do with validity' [3]. We will look into these points and investigate a related common misconception concerning RCTs; it is mistakenly thought there is no need to include baseline covariates in the analysis.
                Many RCTs are analysed in a simple manner using only the randomised treatment as the independent variable. When the response outcome is continuous, 
                precision of the treatment effect estimate is improved when adjusting for baseline covariates. We do not expect covariates to be related to the treatment assignment because of randomisation, but they 
may be related to the outcome, they are therefore not considered to be confounding. However, differences between the outcome which can be 
attributed to differences in the covariates can be removed, this results in a more precise estimate of treatment effect.
This should be considered more often as sample sizes can be reduced. As Frank Harrell has said, 'unadjusted analysis makes the most severe assumptions of all (that risk factors do not exist)'.
In short, not adjusting is permissable ONLY when there are no prognostic covariates.  How can that be known with certainty? 
              Power is therefore compromised in the unadjusted analyses when there are measured prognostic covariates availalable to include in the model. 
We simulate a 1:1 RCT with a continuous response, estimating treatment effects whilst examining adjustment of covariates related to the outcome, 
covariates not related to the outcome and collinear or correlated covariates. 

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
                                  
                                 # h4("User inputs"),
                                  div(
                            
                                      tags$head(
                                          tags$style(HTML('#ab1{background-color:orange}'))
                                      ),
                                      
                                      tags$head(
                                          tags$style(HTML('#resample{background-color:orange}'))
                                      ),
                                      
                                      textInput('K', 
                                                div(h5(tags$span(style="color:blue", "No of covariates"))), "3"),
                                      
                                      textInput('Kp', 
                                                div(h5(tags$span(style="color:blue", "Make covariates X1 to Xn prognostic (tab 5 only)"))), "2"),
                                      
                                      textInput('Fact', 
                                                div(h5(tags$span(style="color:blue", "Covariate coefficients. Here a multiplicative factor is selected so that betas are randomly chosen between (-X*treatment effect) and (X*treatment effect)"))), "1"),
                                      
                                      tags$hr(),
                                      textInput('theta', 
                                                div(h5(tags$span(style="color:blue", "Treatment effect"))), ".223"),
                                      
                                      textInput('sigma', 
                                                div(h5(tags$span(style="color:blue", "Residual variation"))), ".85"),
                                      tags$hr(), 
                                     
                                      
                                      textInput('pow', 
                                                div(h5(tags$span(style="color:blue", "Power (%)"))), "90"),
                                      textInput('alpha', 
                                                 div(h5(tags$span(style="color:blue", "Alpha level two sided (%)"))), "5"),
                                      tags$hr(),
                                      textInput('simuls', 
                                                div(h5(tags$span(style="color:blue", "Number of simulations (simulation tab only)"))), "99"),
                                      tags$hr(), 
                                      
                                      textInput('covar', 
                                                div(h5(tags$span(style="color:blue", "Covariate distribution 1: uniform(-1,1), 2: normal(0,1)"))), "2"),
                                      
                                   
                                  )
                                  
                                  
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
                                  tabPanel("1 Power calculation", value=7,
                                           
                                           
                                           fluidRow(
                                             column(width = 6, offset = 0, style='padding:1px;',
                                                    div( verbatimTextOutput("Power") ),
                                                    h4(paste("Table 1 Result of power calculation based on inputs")),
                                                    h4(paste("The first step is to perform a sample size calculation, based on the user inputs.")),
                                                    h4(htmlOutput("textWithNumber3") ),
                                                  #  h4(htmlOutput("textWithNumber1b") ),
                                             ) ,


                                             fluidRow(
                                               column(width = 5, offset = 0, style='padding:1px;',
                                                   
                                               ))),
                                          
                                  ) ,
                                  tabPanel( "2 Simulation - to adjust or not to adjust",
                                           # h4(paste("Figure 1 Simulation results")) , 
                                            
                                            h4(htmlOutput("textWithNumber1a") ),        
                                            fluidRow(
                                              column(width = 6, offset = 0, style='padding:1px;',
                                                    
                                                     div(plotOutput("reg.plotx",  width=fig.width7, height=fig.height7)),
                                                   #  div(plotOutput("reg.plotxx",  width=fig.width7, height=fig.height7)),
                                              ) ,
                                              
                                              
                                              fluidRow(
                                                column(width = 5, offset = 0, style='padding:1px;',
                                                      
                                                       div(plotOutput("reg.ploty",  width=fig.width7, height=fig.height7)),
                                                    #   div(plotOutput("reg.plotyy",  width=fig.width7, height=fig.height7)),
                                                ))),#
                                           
                                           h4(paste("Here we perform simulations investigating the treatment effect estimate and associated standard error when there are
                                           covariates that are prognostic, covariates unrelated to the outcome, a mix of prognostic and covariates unrelated to the outcome and finally 
                                           when there are correlated covariates. For each scenario we adjust and also do not adjust for the covariates.
                                           The default number of simulations 
                                           is set at a lowly 99 so that results appear quickly. It is advisable to increase this number.
                                                    The left panel shows the distribution of the treatment effect estimates, the right panel the associated standard error estimates. The true value is shown
                                                    by the grey vertical lines. The same covariates are used for all investigations except for the correlated covartiate investigation. 
                                                    Correlations are capped at +/- 0.37")),
                                           
                                            h4(paste("Table 2 Summary, sorted by smallest mean squared error (MSE) estimate")),
                                            div( verbatimTextOutput("zz") )  ,
                                           h4(htmlOutput("textWithNumber99",) ),
                                           h4(paste("Here are the true coefficients of the covariates used in the simulation: ")),
                                           div( verbatimTextOutput("betas") )  ,
                                            
                                            
                                            width = 30 )     ,
                                  
                                  tabPanel("3 Measured covariates prognostic", value=7, 
                                         
                                           #h4("It appears the standard error of the treatment effect (variable z) is smaller if we adjust"),
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                    
                                                      h4("Table 3 Linear model, one realisation ignoring prognostic covariates (treatment is variable z)"),
                                                      
                                                        div( verbatimTextOutput("B") )     ,
                                                      
                                                      
                                                      h4("Figure 3 Outcome v fitted linear predictor seperately for control and treated groups, multivariable model"),
                                                      div(plotOutput("diag",  width=fig.width6, height=fig.height7)),
                                                      h4("Figure 3a Outcome v fitted linear predictor seperately for control and treated groups, bivariate model (note intercept and coef)"),
                                                      div(plotOutput("diagu",  width=fig.width6, height=fig.height7)),
                                                      h4("Figure 3b residual v linear predictor seperately for control and treated groups, multivariable model"),
                                                      div(plotOutput("residual1",  width=fig.width6, height=fig.height7)),
                                               ) ,
                                               
                                               
                                               fluidRow(
                                                   column(width = 6, offset = 0, style='padding:1px;',
                                                          h4("Table 4 Linear model, adjusting for prognostic covarites"),
                                                          
                                                          div( verbatimTextOutput("A") )   ,     
                                                          h4("Table 5 Correlations between covariates"),
                                                          
                                                          div( verbatimTextOutput("R1") ),
                                                          h4("  True betas of covariates"),
                                                          div( verbatimTextOutput("betaz") ) 
                                                   ))),
                                           
                                         

                                  ) ,
                                  
                                  tabPanel("3a Measured covariates non prognostic", value=3, 
                                           
                                           
                                           
                                           fluidRow(
                                               column(width = 6, offset = 0, style='padding:1px;',
                                                      h4("Table 6 Linear model, one realisation ignoring covariates that are not prognostic (treatment is variable z)"),
                                                      div( verbatimTextOutput("D") )     ,
                                                   
                                                      h4("Figure 4 Outcome v fitted linear predictor seperately for control and treated groups, multivariable model"),
                                                      div(plotOutput("diag1",  width=fig.width6, height=fig.height7)),
                                                      h4("Figure 4a Outcome v fitted linear predictor seperately for control and treated groups, bivariate model (note intercept and coef)"),
                                                      div(plotOutput("diag1u",  width=fig.width6, height=fig.height7)),
                                                    h4("Figure 4b residual v linear predictor seperately for control and treated groups, multivariable model"),
                                                    div(plotOutput("residual2",  width=fig.width6, height=fig.height7)),
                                               ) ,
                                               
                                               
                                               fluidRow(
                                                   column(width = 5, offset = 0, style='padding:1px;',
                                                          h4("Table 7 Linear model, adjusting for all covariates that are not prognostic"),
                                                          div( verbatimTextOutput("C") )  ,      
                                                          h4("Table 8 Correlations between covariates"),
                                                          div( verbatimTextOutput("R2") )
                                                   ))),

                                  ) ,
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  tabPanel( "3b Measured covariates mix of non prog and prognostic", 
                                     
                                            
                                           h4("First X1:Xn covariates only are prognostic, the remainder are not"),
                                           
                                           
                                           fluidRow(
                                             column(width = 6, offset = 0, style='padding:1px;',
                                                    h4("Table 9 Linear model, one realisation ignoring all covariates some of which are prognostic (treatment is variable z)"),
                                                    div( verbatimTextOutput("E") )     ,
                                                    
                                                    h4("Figure 5 Outcome v fitted linear predictor seperately for control and treated groups, multivariable model"),
                                                    div(plotOutput("diag3",  width=fig.width6, height=fig.height7)),  
                                                    
                                                    h4("Figure 5a Outcome v fitted linear predictor seperately for control and treated groups, bivariate model (note intercept and coef)"),
                                                    div(plotOutput("diag3u",  width=fig.width6, height=fig.height7)),
                                                    h4("Figure 5b residual v linear predictor seperately for control and treated groups, multivariable model"),
                                                    div(plotOutput("residual3",  width=fig.width6, height=fig.height7)),
                                                    
                                             ) ,
                                             
                                             
                                             fluidRow(
                                               column(width = 5, offset = 0, style='padding:1px;',
                                                      h4("Table 10 Linear model, adjusting for all covariates some of which are prognostic"),
                                                      div( verbatimTextOutput("F") ) ,       
                                                      h4("Table 11 Correlations between covariates"),
                                                      div( verbatimTextOutput("R3") )
                                               ))),
                                           
                                           
                                           width = 30 )     ,
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  tabPanel( "3c Measured correlated covariates",

                                            fluidRow(
                                              column(width = 6, offset = 0, style='padding:1px;',
                                                     
                                                     h4("Table 12 Linear model, one realisation ignoring all prognostic covariates that have some correlation with each other (treatment is variable z)"),
                                                     div( verbatimTextOutput("G") ),
                                                     
                                                     h4("Figure 6 Outcome v fitted linear predictor seperately for control and treated groups, multivariable model"),
                                                     div(plotOutput("diag4",  width=fig.width6, height=fig.height7)),
                                                     
                                                     h4("Figure 6a Outcome v fitted linear predictor seperately for control and treated groups, bivariate model (note intercept and coef)"),
                                                     div(plotOutput("diag4u",  width=fig.width6, height=fig.height7)),
                                                     h4("Figure 6b residual v linear predictor seperately for control and treated groups, multivariable model"),
                                                     div(plotOutput("residual4",  width=fig.width6, height=fig.height7)),
 
                                              ) ,

                                              fluidRow(
                                                column(width = 5, offset = 0, style='padding:1px;',
                                                       h4("Table 13 Linear model, adjusting for all prognostic covariates that have some correlation with each other"),
                                                       div( verbatimTextOutput("H") ),
                                                       h4("Table 14 Correlations between covariates"),
                                                        div( verbatimTextOutput("R4") ),
                                                       h4("  True betas of covariates"),
                                                       div( verbatimTextOutput("betazz") ) 
                                                ))),
 
                                            width = 30 )     ,

                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  
                                  tabPanel("3d Observations based on one realisation", value=3, 
                                  
                                           h4(htmlOutput("textWithNumber1") ),
                                           
                                         h4("Table 15 Observations only from one realisation"),
                                          
                                       
                                         div( verbatimTextOutput("summary1") )  ,
                                           
                                         h4("[1 v 2] We see adjusting for known measured prognostic covariates results in a more precise estimate. "),
                                         h4("[3 v 4] We see adjusting for measured non prognostic covariates we do not lose much precision."),
                                         h4("[5 v 6] We see adjusting for measured covariates results in a more precise estimate."),
                                         h4("[5 v 6] We see adjusting for measured correlated covariates results in a more precise estimate than if we don't adjust for them."),
                                           fluidRow(
                                               column(width = 7, offset = 0, style='padding:1px;',
                                          
                                               )),
                                  ),
                                  
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  tabPanel("3e Observed covariate balance", value=7, 
                                           h4("Figure 7 Difference in baseline covariates across arms. Larger sample sizes does not mean better covariate balance. Precision improves so smaller differences are picked up."),
                                           
                                           div(plotOutput("reg.plot", width=fig.width1, height=fig.height1)),
                                           
                                   
                                           
                                           
                                           # covariates <- 10
                                           # A <- rnorm(covariates, 0, sqrt(4/50))    # differences
                                           # mean(abs(A) > (1.96*sqrt(4/50)))
                                           
                                           
                                           
                                           fluidRow(
                                             column(width = 7, offset = 0, style='padding:1px;',

                                             )),
                                           
                                  ) ,
                                  
                                  tabPanel("4 Balance does not get better when clinical trials get larger", 
                                           h4("Figure 8 Investigating covariate balance, enter desired values in boxes."),
                                           fluidRow(
                                             column(3,
                                                    textInput('NN', 
                                                              div(h5(tags$span(style="color:blue", "Sample size in each group (1:1 randomisation)"))), "50")),
                                             
                                             column(3,
                                                    textInput('sdsd', 
                                                              div(h5(tags$span(style="color:blue", "Residual variation"))), "1")),
                                             
                                             
                                             column(3,
                                                    textInput('alpha2', 
                                                              div(h5(tags$span(style="color:blue", "Two sided alpha level"))), "0.05")),
                                           ),
                                           
                                           
                                           div(plotOutput("norm.plot", width=fig.width, height=fig.height)),
                                           
                                           h4("'The reason is that in parallel group trials, 
                                  other things being equal, the standard error is inversely proportional to the square root of the sample size. 
                                  The larger the sample size the narrower the confidence interval. 
                                  Thus the (reduced) allowance for uncertainty about the distribution of prognostic factors has already consumed the benefit of 
                                  having a larger sample size by posting a narrower confidence interval.
                                     There is no further benefit to attribute to it.' [1]"),
                                           h4(" "),
                                           h4("Imagine a trial recruiting 100 patients randomised 1:1 and the standard deviation of the covarites is 1. The standard error of the 
                                              difference is therefore the square root of (4/100) = 0.2. So we can simulate the difference for any number of measured covariates and or sample size.
                                              But we don't have to, the proportion of 'significant differences' is simply found using the normal distribution.
                                              So it does not matter how many covariates we have measured. On average 5% of the covariates will be picked up as 'significant' at the 5% level (randomisation assures the null is true).
                                             ")
                                           
                                           
                                  ),

                                  tabPanel("5 Data used tabs 3,4 & 5", value=3, 
                                           
                                           h4("Data and Response for prognostic, non prognostic and mix of prognostic and non prognostic variables"), 
                                           div( verbatimTextOutput("ddd")),
                                     
                                  ),
                                  
                                  tabPanel("6 Correlated data tab 6", 
                                           
                                           fluidRow(
                                               column(width = 9, offset = 0, style='padding:1px;',
                                              
                                                    div( verbatimTextOutput("fake4")),
                                               ),
                                              
                                           )
                                  ),
                                  
                                  
                             
                                  
                                  tabPanel("check", value=3, 
                                           
                                          # h4("Data and Response for prognostic, non prognostic and mix of prognostic and non prognostic variables"), 
                                           div( verbatimTextOutput("ttests2")),
                                           
                                  ),
                
                                  tabPanel("7 Notes", value=3, 
                                           
                                           ## could do correlated covariates not related to the outcome?
                                           h4("The first tab, shows the standard power calculation function in R for a ttest, using the random error, treatment effect, alpha and power to determine the sample size.  
                                            The next tab, presents the results of simulation where we investigate (i) adjusting for true prognostic covariates (ia) ignoring them in the analysis. 
                                            We investigate (ii) adjusting for covariates unrelated to the outcome (iia) ignoring them in the analysis.
                                            We investigate (iii) adjusting for covariates some unrelated and some related to the outcome (iiia) ignoring them in the analysis.
                                           We investigate (iv) adjusting for covariates related to the outcome and are correlated with each other (iva) ignoring them in the analysis.
                                           Plots of treatment effect estimates and standard error estimates are presented as well as a summary of simulations from which we can draw conclusions.
                                           The third tab presents one realisation from scenario (i),the fourth tab presents one realisation from scenario (ii), the fifth tab presents one realisation from scenario (iii)
                                              the sixth tab presents one realisation from scenario (iv), the seventh tab presents a summary of the previous 4 tabs. Tabs 3 to 6 also present the correlation matrix from the multivariable
                                              model and diagnostic plots. Tab 8 presents a plot of the difference in each covariate across the trial arms. The next two tabs present the data used in tabs 3,4 and 5 and then the correlated data."),

                                           
                                           column(width = 3, offset = 0, style='padding:1px;',
                                                  
                                                  tags$hr(),
                                                  div(h4("References:")),  
                                                  
                                                  tags$a(href = "https://www.linkedin.com/pulse/stop-obsessing-balance-stephen-senn/", tags$span(style="color:blue", "[1] Stephen Senn"),),   
                                                  div(p(" ")),
                                                  tags$a(href = "https://twitter.com/f2harrell/status/1299755896319475712", tags$span(style="color:blue", "[1] Frank Harrell twitter"),),   
                                                  div(p(" ")),
                                                  tags$a(href = "https://twitter.com/f2harrell/status/1298640944405807105",  tags$span(style="color:blue", "[2]  Frank Harrell twitter"),),   
                                                  div(p(" ")),
                                                  tags$a(href = "https://discourse.datamethods.org/t/should-we-ignore-covariate-imbalance-and-stop-presenting-a-stratified-table-one-for-randomized-trials/547/32", tags$span(style="color:blue", "[3] Data discourse, see Senn's points"),),  
                                                  div(p(" ")),
                                                  tags$a(href = "https://discourse.datamethods.org/t/guidelines-for-covariate-adjustment-in-rcts/2814/2", tags$span(style="color:blue", "[4] Data discourse"),),  
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
    
    shinyalert("Welcome! \nAdjusting for covariates in RCTs!",
               "Best to do it!", 
               type = "info")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # This is where a new sample is instigated and inouts made numeric
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

        N <-ceiling(Po$n)*2
        
        # allow covariates to have different distibution
        if (covar==1) {  
        X <- array(runif(N*K , -1,1), c(N,K))     # initially covars were uniform dist
        } else {
        X <- array(rnorm(N*K, 0, 1), c(N,K))  
        }
        
        z <- sample(c(0,1), N, replace=T)          # treatment indicator
        a <- 1                                     # intercept
                            
        b <- round(sort(runif(K, -theta*Fact,theta*Fact)), digits=2)  # create beta coefficients, multiple of true trt effect

        #prognostic covariates
        y <- a+ X %*% b + theta*z + rnorm(N,0, sigma)
        fake <- data.frame(X=X, y=y, z=z)
        dat <- fake

        # covariates not related to outcome
        y <- a +  theta*z + rnorm(N,0, sigma)
        fake2 <- data.frame(X=X, y=y, z=z)
        
        #mix of prog. and non prognostic covariates
        y <- a+ X[,1:Kp] %*% b[1:Kp] + theta*z + rnorm(N,0, sigma)
        fake3 <- data.frame(X=X, y=y, z=z)

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #create a dataset to print in app
        ddd <- cbind(fake[,c(1:K)], prog.response=fake$y, notprog.response= fake2$y, mixprog.response=fake3$y)
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # let's get confidence interval of the diff of covariates across arms
        # function
        l <- zz <-   lapply(fake[1:K], function(x) 
            t.test(x ~ fake$z, paired = FALSE, na.action = na.pass, var.equal = TRUE))
        
        dfx <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T))  # dataset to export ttests
        names(dfx) <- c("t stat","df","p-value","lower 95%CI","upper 95%CI","mean placebo", "mean treated","null","Std err" ,"alt","method","dataset")
    
        # check covariate distribution
        zzz <-   lapply(zz, function(x)    ## execute the function
            x[4]
        )
        
        zzz<- as.data.frame(zzz)
        ci <- t(zzz)
        conf<- as.data.frame(ci)
        
        #  run the function again, and pull out the means
        mzz <-   lapply(zz, function(x) 
            (x[5])
        )
        
        mzz<- as.data.frame(mzz)
        ci <- t(mzz)
        r<- as.data.frame(ci)
        
        doff <- as.vector( r["mean in group 0"] - r["mean in group 1"] )
        
        ## counts in arms
        placebo <- as.vector(table(z)) [1]
        treated <- as.vector(table(z)) [2]
        bigN <- placebo + treated
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ################################################################
        ###now create covariate that are correlation wit h each other
        ################################################################
        library(Matrix)
         
        nobs <- N
        
        ##RR is defined prior to app code
        x <- Matrix(runif(K*K,-RR,RR), K)   # create a correlation matrix randomly , we won't allow very high correlations
  
        A <- forceSymmetric(x)
        
        diag(A) <- 1 # make diagonals 1
        
        #isSymmetric(A)
      
        M <- A
        
        # note you cannot just make a matrix of random values as covariance (corr matrix) 
        # here we ensure the matrix is a proper covariance matrix
        #https://r.789695.n4.nabble.com/how-do-I-make-a-correlation-matrix-positive-definite-td3006440.html
        M <- nearPD(M, conv.tol = 1e-7)$mat # default
        # Cholesky decomposition
        L = chol(M)
        nvars = dim(L)[1]
        
        # R chol function produces an upper triangular version of L
        # so we have to transpose it.
        # Just to be sure we can have a look at t(L) and the
        # product of the Cholesky decomposition by itself
        
        t(L)
        
        t(L) %*% L
        
        # Random variables that follow an M correlation matrix
        r = t(L) %*% matrix(rnorm(nvars*nobs, 0,1), nrow=nvars, ncol=nobs) #22
        r = t(r)
        
        r <- as.matrix(r)
        rdata <- as.data.frame(r)
        XX<- as.matrix(rdata)
        # rdata
        # x
        # splom(rdata)
        # cor(rdata)
        
        # #######################################
        # apply(rdata,2, sd)
        # apply(rdata,2, mean)
        # 
        # response is created
        y <- a+ XX %*% b + theta*z + rnorm(N,0, sigma)
        fake4 <- data.frame(X=rdata, y=y, z=z)
   
        
        return(list(  dat=dat, conf=conf, doff=doff , K=K, N=N, X=X, fake2=fake2, fake3=fake3,
                      
                      placebo=placebo, treated=treated, bigN=bigN, fake4=fake4, XX=XX, Po=Po, ddd=ddd,
                      
                      betas=b,  dfx=dfx, #df=df,
                      
                      zz=zz)) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    })
    
    output$ttests   <- renderPrint({      # not used  
      
      return(mcmc()$zz)
      
    })
    
    output$ttests2   <- renderPrint({        
      
      d <- mcmc()$dfx
      
      return(print(d, digits=4))
      
      
    })
    
    output$betazz  <- output$betaz <- renderPrint({        
      
      return(mcmc()$betas)
      
    })
    
    output$Power <- renderPrint({        
      
      return(mcmc()$Po)

    })
    
    
    output$ddd <- renderPrint({        
      
      return(mcmc()$ddd)
      
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # diagnostic plots
    
    output$diag <- renderPlot({         
      
      sample <- random.sample()
    
      fit <- reg1()$fit  # call in multivariable regression model
      d <- mcmc()$dat    # call in the data
      y <- d$y           # response
      z <- d$z           # trt indicator
      y_hat=predict(fit) # predictions
      
      # code taken from regression and other stories book
      par(mfrow=c(1,2))
      for(i in 0:1) {
        
        plot( range(y_hat, y), range(y_hat, y), type='n' , main=paste("z =", i, "N=", length(y[z==i])  ),  xlab="Linear predictor", ylab="Outcome, y")
        points(y_hat[z==i], y[z==i], pch=20+i)
        abline(0,1)
        
      }
      
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # do the same for bivariate analysis
    output$diagu <- renderPlot({         
      
      sample <- random.sample()
      
      fit <- reg1()$fit0
      d <- mcmc()$dat
      y <- d$y
      z <- d$z
      y_hat=predict(fit)
      
      par(mfrow=c(1,2))
      for(i in 0:1) {
        
        plot( range(y_hat, y), range(y_hat, y), type='n' , main=paste("z =", i, "N=", length(y[z==i])  ),  xlab="Linear predictor", ylab="Outcome, y")
        points(y_hat[z==i], y[z==i], pch=20+i)
        abline(0,1)
        
      }
      
      
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## only on multivariable do this residual v prediction
    output$residual1 <- renderPlot({         
      
      sample <- random.sample()
      
      fit <- reg1()$fit
      d <- mcmc()$dat
      y <- d$y
      z <- d$z
      y_hat=predict(fit)
    
    r <- y - y_hat
    par(mfrow=c(1,2), mar=c(3,3,2,2), mgp=c(1.7,.5,0), tck=-.01)
    par(mfrow=c(1,2))
    for (i in 0:1){
      plot(range(y_hat), range(r), type="n", xlab=expression(paste("Linear predictor, ", hat(y))),
           ylab="Residual, r", main=paste("z =", i), bty="l")
      points(y_hat[z==i], r[z==i], pch=20+i)
      abline(0, 0)
    }
    
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~do the same for non prognostic covariates modelling
    output$diag1 <- renderPlot({         
      
      sample <- random.sample()
    
      fit <- reg2()$fit
      d <- mcmc()$fake2
      y <- d$y
      y_hat=predict(fit)
      z <- d$z
      
      par(mfrow=c(1,2))
      for(i in 0:1) {
        
        plot( range(y_hat, y), range(y_hat, y), type='n' ,main=paste("z =", i, "N=", length(y[z==i])  ),  xlab="Linear predictor", ylab="Outcome, y")
        points(y_hat[z==i], y[z==i], pch=20+i)
        abline(0,1)
        
      }
      
      
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$diag1u <- renderPlot({         
      
      sample <- random.sample()
      
      fit <- reg2()$fit0
      d <- mcmc()$fake2
      y <- d$y
      y_hat=predict(fit)
      z <- d$z
      
      par(mfrow=c(1,2))
      for(i in 0:1) {
        
        plot( range(y_hat, y), range(y_hat, y), type='n' , main=paste("z =", i, "N=", length(y[z==i])  ),  xlab="Linear predictor", ylab="Outcome, y")
        points(y_hat[z==i], y[z==i], pch=20+i)
        abline(0,1)
        
      }
      
      
    })
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## only on multivariable do this 
    output$residual2 <- renderPlot({         
      
      sample <- random.sample()
      
      fit <- reg2()$fit
      d <- mcmc()$fake2
      y <- d$y
      z <- d$z
      y_hat=predict(fit)
      
      r <- y - y_hat
      par(mfrow=c(1,2), mar=c(3,3,2,2), mgp=c(1.7,.5,0), tck=-.01)
      par(mfrow=c(1,2))
      for (i in 0:1){
        plot(range(y_hat), range(r), type="n", xlab=expression(paste("Linear predictor, ", hat(y))),
             ylab="Residual, r", main=paste("z =", i), bty="l")
        points(y_hat[z==i], r[z==i], pch=20+i)
        abline(0, 0)
      }
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$diag3 <- renderPlot({         
      
      sample <- random.sample()
      
      fit <- reg3()$fit
      d <- mcmc()$fake3
      y <- d$y
      y_hat=predict(fit)
      z <- d$z
      
      par(mfrow=c(1,2))
      for(i in 0:1) {
        
        plot( range(y_hat, y), range(y_hat, y), type='n' , main=paste("z =", i, "N=", length(y[z==i])  ),  xlab="Linear predictor", ylab="Outcome, y")
        points(y_hat[z==i], y[z==i], pch=20+i)
        abline(0,1)
        
      }
      
      
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$diag3u <- renderPlot({         
      
      sample <- random.sample()
      
      fit <- reg3()$fit0
      d <- mcmc()$fake3
      y <- d$y
      y_hat=predict(fit)
      z <- d$z
      
      par(mfrow=c(1,2))
      for(i in 0:1) {
        
        plot( range(y_hat, y), range(y_hat, y), type='n' , main=paste("z =", i, "N=", length(y[z==i])  ),  xlab="Linear predictor", ylab="Outcome, y")
        points(y_hat[z==i], y[z==i], pch=20+i)
        abline(0,1)
        
      }
      
      
    })
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## only on multivariable do this 
    output$residual3 <- renderPlot({         
      
      sample <- random.sample()
      
      fit <- reg3()$fit
      d <- mcmc()$fake3
      y <- d$y
      z <- d$z
      y_hat=predict(fit)
      
      r <- y - y_hat
      par(mfrow=c(1,2), mar=c(3,3,2,2), mgp=c(1.7,.5,0), tck=-.01)
      par(mfrow=c(1,2))
      for (i in 0:1){
        plot(range(y_hat), range(r), type="n", xlab=expression(paste("Linear predictor, ", hat(y))),
             ylab="Residual, r", main=paste("z =", i), bty="l")
        points(y_hat[z==i], r[z==i], pch=20+i)
        abline(0, 0)
      }
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$diag4u <- renderPlot({         
      
      sample <- random.sample()
      
      fit <- reg4()$fit0
      d <- mcmc()$fake4
      y <- d$y
      y_hat=predict(fit)
      z <- d$z
      
      par(mfrow=c(1,2))
      for(i in 0:1) {
        
        plot( range(y_hat, y), range(y_hat, y), type='n' , main=paste("z =", i),  xlab="Linear predictor", ylab="Outcome, y")
        points(y_hat[z==i], y[z==i], pch=20+i)
        abline(0,1)
        
      }
      
      
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$diag4 <- renderPlot({         
      
      sample <- random.sample()
      
      fit <- reg4()$fit
      d <- mcmc()$fake4
      y <- d$y
      y_hat=predict(fit)
      z <- d$z
      
      par(mfrow=c(1,2))
      for(i in 0:1) {
        
        plot( range(y_hat, y), range(y_hat, y), type='n' , main=paste("z =", i),  xlab="Linear predictor", ylab="Outcome, y")
        points(y_hat[z==i], y[z==i], pch=20+i)
        abline(0,1)
        
      }
      
      
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## only on multivariable do this 
    output$residual4 <- renderPlot({         
      
      sample <- random.sample()
      
      fit <- reg4()$fit
      d <- mcmc()$fake4
      y <- d$y
      z <- d$z
      y_hat=predict(fit)
      
      r <- y - y_hat
      par(mfrow=c(1,2), mar=c(3,3,2,2), mgp=c(1.7,.5,0), tck=-.01)
      par(mfrow=c(1,2))
      for (i in 0:1){
        plot(range(y_hat), range(r), type="n", xlab=expression(paste("Linear predictor, ", hat(y))),
             ylab="Residual, r", main=paste("z =", i), bty="l")
        points(y_hat[z==i], r[z==i], pch=20+i)
        abline(0, 0)
      }
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #  plot  of the difference in covarites across arms
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    output$reg.plot <- renderPlot({         
        
      # as in frank harrell's covariate imbalance code. he used .3 cutoff for difference in mean when se =.2
      # se*1.5
      
      sample <- random.sample()
      sigma1=sample$sigma
      N1 <- mcmc()$N # 
      
      se. <-  sqrt((2*(sigma1^2 + sigma1^2)) /N1)  
      se. <-  sqrt((sigma1^2+sigma1^2) / (N1/2) )  #ditto
      
        placebo <- mcmc()$placebo
        treated <- mcmc()$treated
        bigN <- mcmc()$bigN
   
        doff <- mcmc()$doff
        conf <- mcmc()$conf
        K <- mcmc()$K
        
        #dff <- mcmc()$df
        
        # plot
        par(mar=c(4,3,3,3), mgp=c(1.5,.5,0), tck=-.01)
        plot(c(0, K+1), range(conf), bty="l", xlab="Covariates", 
             ylab="Estimate Mean difference", xaxs="i",  type="n",    sub="Note each CI is computed using a t-test using the observed data for each covariate comparison. The horizontal lines use the true SE (are not based on the t dist) and are guides only.",
             main=paste0("'Imbalance' treatment arm estimate of mean difference of each covariate distribution & 95% confidence interval
             placebo=",placebo,", treated=",treated,", total=",bigN,", we show +/- standard error of difference ",p3(se.)," and 1.96 X standard error of difference ",p3(se.*qnorm(.975)),""))
        #axis(2, seq(-5,5,1))
        # axis(1, seq(1,K,10))
        points(1:K, doff[,1], pch=20)
        abline(0, 0, col="pink", lty=w, lwd=.5)
        abline(se., 0, col="pink" , lty=w, lwd=.5)
        abline(-se., 0, col="pink" , lty=w, lwd=.5)
        #abline(qt(.975, df=dff)*se., 0, col="pink" , lty=w, lwd=.5)
        #abline(qt(.975, df=dff)*-se., 0, col="pink" , lty=w, lwd=.5)
        abline(2*se., 0, col="pink" , lty=w, lwd=.5)
        abline(2*-se., 0, col="pink" , lty=w, lwd=.5)
        
        for (i in 1:K){
            if (prod(conf[i,c(1,2)]) < 0 ) {
                lines(c(i,i), conf[i,c(1,2)], lwd=.8, col='blue') 
            } else {
                lines(c(i,i), conf[i,c(1,2)], lwd=.8, col='red') 
            }
        }

    })
    
    # here we run the individual regressions to present , prognostic covariates
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    reg1 <- reactive({  
        
        d <- mcmc()$dat
         
        X <- mcmc()$X  #have to bring X through , as model will fail without this
        
        ols2 <- lm(y~X+z,data=d)
        ols1 <- lm(y~z,d)
        
        A<-summary(ols2)
        B<-summary(ols1)
        xx <- cov2cor(vcov(ols2))
        R <- round(xx,2)
        
        #get stats so we can compare together
        x<- A
        stat1 <- t(cbind(c(x$coefficients["z",], sigma=x$sigma, r2= x$adj.r.squared)))
        x<- B
        stat2 <- t(cbind(c(x$coefficients["z",], sigma=x$sigma, r2= x$adj.r.squared)))
        
        return(list(  A=A, B=B, stat1=stat1, stat2=stat2, R=R, fit=ols2, fit0=ols1)) 
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$A <- renderPrint({
        
        return(reg1()$A)
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$B <- renderPrint({
        
        return(reg1()$B)
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$R1 <- renderPrint({
      
      return(reg1()$R)
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # here we run the individual regressions to present , non prognostic covariates
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    reg2 <- reactive({  
        
        d <- mcmc()$fake2
        
        X <- mcmc()$X  #have to bring X through , as model will fail without this
        
        ols2 <- lm(y~X+z,data=d)
        ols1 <- lm(y~z,d)
        
        A<-summary(ols2)
        B<-summary(ols1)
        xx <- cov2cor(vcov(ols2))
        R <- round(xx,2)
        
        #get stats so we can compare together
        x<- A
        stat3 <- t(cbind(c(x$coefficients["z",], sigma=x$sigma, r2= x$adj.r.squared)))
        x<- B
        stat4 <- t(cbind(c(x$coefficients["z",], sigma=x$sigma, r2= x$adj.r.squared)))

        return(list(  C=A, D=B,   stat4=stat4, stat3=stat3, R=R, fit=ols2, fit0=ols1)) 
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$C <- renderPrint({
        
        return(reg2()$C)
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$D <- renderPrint({
        
        return(reg2()$D)
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$R2 <- renderPrint({
      
      return(reg2()$R)
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # here we run the individual regressions to present , mix of prognostic and non prog covariates
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    reg3<- reactive({  
      
      d <- mcmc()$fake3
      
      X <- mcmc()$X  #have to bring X through , as model will fail without this
      
      ols2 <- lm(y~X+z,data=d)
      ols1 <- lm(y~z,d)
      
      A<-summary(ols2)
      B<-summary(ols1)
      
      xx <- cov2cor(vcov(ols2))
      R <- round(xx,2)
      
      #get stats so we can compare together
      x<- A
      stat5 <- t(cbind(c(x$coefficients["z",], sigma=x$sigma, r2= x$adj.r.squared)))
      x<- B
      stat6 <- t(cbind(c(x$coefficients["z",], sigma=x$sigma, r2= x$adj.r.squared)))
      
      return(list(  A=A, B=B,   stat5=stat5, stat6=stat6, R=R, fit=ols2, fit0=ols1)) 
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$F <- renderPrint({
      
      return(reg3()$A)
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$E <- renderPrint({
      
      return(reg3()$B)
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$R3 <- renderPrint({
      
      return(reg3()$R)
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # here we run the individual regressions to present , prognostic correlated covariates
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    reg4<- reactive({  
      
      d <- mcmc()$fake4
      
      X <- mcmc()$XX  #have to bring X through , as model will fail without this
      
      ols2 <- lm(y~X+z,data=d)
      ols1 <- lm(y~z,d)
      
      
      xx <- cov2cor(vcov(ols2))
      R <- round(xx,6)
      
      A<-summary(ols2)
      B<-summary(ols1)
      
      #get stats so we can compare together
      x<- A
      stat5 <- t(cbind(c(x$coefficients["z",], sigma=x$sigma, r2= x$adj.r.squared)))
      x<- B
      stat6 <- t(cbind(c(x$coefficients["z",], sigma=x$sigma, r2= x$adj.r.squared)))
      
      return(list(  A=A, B=B,   stat5=stat5, stat6=stat6, R=R, fit=ols2, fit0=ols1)) 
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$H <- renderPrint({
      
      return(reg4()$A)
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$G <- renderPrint({
      
      return(reg4()$B)
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$R4 <- renderPrint({
      
      return(reg4()$R)
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # create a table to summarise individual realisations
    output$summary1 <- renderPrint({
        
        stat1 <- reg1()$stat1 # ignoring true prog 
        stat2 <- reg1()$stat2 # adjusting for true prog
        
        stat3 <- reg2()$stat3  # ignoring non prog
        stat4 <- reg2()$stat4  # adj for non prog
        
        
        stat5 <- reg3()$stat5  # ignoring mix of prog and non prog
        stat6 <- reg3()$stat6  # adj  mix of prog and non prog
        
        stat7 <- reg4()$stat5  # ignoring correlated prog covariates
        stat8 <- reg4()$stat6  # adj for correlated prog covariates
        
        # placebo <- mcmc()$placebo
        # treated <- mcmc()$treated
        # bigN <- mcmcm()$bigN
        
        d <- rbind(stat1, stat2, stat3, stat4, stat5, stat6, stat7, stat8)
        
        d<- data.frame(d)
        
        colnames(d) <- c("Estimate","Standard error","t-value","P-value","sigma", "Adj.R2")
        
        rownames(d) <- c(
                         "[1] Multivariable adjusting for measured true prognostic covariates",
                         "[2] Bivariate no adjustment, measured prognostic covariates ignored",
                        
                         "[3] Multivariable adjusting for measured covariates unrelated to outcome",
                         "[4] Bivariate no adjustment, measured covariates unrelated to outcome ignored",
                         
                         "[5] Multivariable adjusting for measured prognostic and covariates unrelated to outcome",
                         "[6] Bivariate no adjustment, measured covariates ignored",
                         
                         "[7] Multivariable adjusting for measured prognostic covariates that are correlated",
                         "[8] Bivariate no adjustment, measured correlated covariates ignored"
                         )
        
        return(print(d, digits=4))
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
    # here is code to simulate scenarios prognostic covariates, covariates unrelated to y, mix of pro and unrelated to y covariates, 
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
        
        # X <- array(runif(N*K , -1,1), c(N,K))          # array of variables
        # # Simulate a difference in two means with data SD of ?
        # X <- array(rnorm(N*K, 0, 1), c(N,K))  
        # 
        # we can select this, does not seem to have a big inpact
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
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
    # do the same as the first simulation code, but this time correlated covariates are created
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
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #  plot 2 not using
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    # output$reg.plot2 <- renderPlot({         
    #   
    #   # Get the  data
    #   
    #   res <- simul()$res
    #   result <- simul()$result
    #   
    #   
    #   hist(res[,1], nclass=50,   main=paste0("Distribution of estimates of treatment effect ",p3(result[1]), ""), xlab='Treatment effect')
    #   
    # })
    # 
    # 
    # output$reg.plot3 <- renderPlot({         
    #   
    #   # Get the  data
    #   res <- simul()$res
    #   result <- simul()$result
    #   
    #   hist(res[,2], nclass=50,   main=paste0("Distribution of estimates of treatment effect se ",p3(result[2]), ""), xlab='Treatment effect se')
    #   
    # })
    # 
    # 
    # 
    # output$reg.plot4 <- renderPlot({         
    #   
    #   # Get the  data
    #   
    #   res <- simul()$res
    #   result <- simul()$result
    #   
    #   
    #   hist(res[,3], nclass=50,   main=paste0("Distribution of estimates of treatment effect ",p3(result[3]), ""), xlab='Treatment effect')
    #   
    # })
    # 
    # 
    # output$reg.plot5 <- renderPlot({         
    #   
    #   # Get the  data
    #   res <- simul()$res
    #   result <- simul()$result
    #   
    #   hist(res[,4], nclass=50,   main=paste0("Distribution of estimates of treatment effect se ",p3(result[4]), ""), xlab='Treatment effect se')
    #   
    # })
    # 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # output$reg.plotx <- renderPlot({         
    #   
    #   # Get the  data
    #   
    #   res <- simul()$res
    #   result <- simul()$result
    #   
    #   
    #   sample <- random.sample()
    #   theta1=sample$theta     
    #   
    #   
    #   d1 <-  (res[,1] )
    #   d2 <-  (res[,3] )
    # 
    #   plot(density(d1), xlim = c(- 3, 3), main="Kernel Density of treatment effect estimates",
    #        xlab="Treatment effect", #Change the x-axis label
    #        ylab="Density") #y-axis label)                   # Plot density of x
    #   lines(density(d2), col = "red")                                                      # Overlay density  
    #   abline(v = theta1, col = "red")                  
    #  
    #   legend("topleft",                                  # Add legend to density
    #          legend = c("Density adj", "Density not adj" ),
    #          col = c("black", "red"),
    #          lty = 1)
    # })
    # 
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # 
    # output$reg.ploty <- renderPlot({         
    #   
    #   # Get the  data
    #   
    #   res <- simul()$res
    #   result <- simul()$result
    #   
    #   d1 <-  (res[,2] )
    #   d2 <-  (res[,4] )
    #   
    #   plot(density(d1), xlim = c(0, 2), main="Kernel Density of standard error of trt effect",
    #        xlab="Standard error", #Change the x-axis label
    #        ylab="Density") #y-axis label)                  # Plot density of x
    #   lines(density(d2), col = "red")                                                      # Overlay density  
    #   
    #   
    #   legend("topright",                                  # Add legend to density
    #          legend = c("Density adj", "Density not adj" ),
    #          col = c("black", "red"),
    #          lty = 1)
    # })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # output$textWithNumber4 <- renderText({ 
    #   
    #   result <- simul()$result  # means
    #   
    #   HTML(paste0(  tags$hr(),
    #                 "Mean and se adjusting  "  
    #                 , tags$span(style="color:red",  p3(result[1]))  ,
    #                 " ; "  
    #                 , tags$span(style="color:red",  p3(result[3] )) ,
    #                 " and ignoring in analysis "
    #                 , tags$span(style="color:red",  p3(result[2]  )),
    #                 " ; "
    #                 , tags$span(style="color:red",  p3(result[4] )) ,
    #                 
    #                 br(), br(),
    #                 br(), br(),
    #                 tags$hr()
    # 
    #   ))    
    #   
    # })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # collect simulation trt effect estimates from simulation and plot!
    output$reg.plotx <- renderPlot({         #means
      
      # Get the  data
      
      res <- simul()$res
      res2 <- simul2()$res

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

      dz <- max(c(d1$y, d2$y, d3$y, d4$y, d5$y, d6$y, d7$y, d8$y))
      dx <- range(c(d1$x,d2$x,  d3$x, d4$x, d5$x, d6$x, d7$x, d8$x))

      
      plot((d1), xlim = dx, main=paste0("Density of treatment estimates, truth= ",p3(theta1),""), ylim=c(0,dz),lty=w, lwd=ww,
           xlab="Treatment effect", #Change the x-axis label
           ylab="Density") #y-axis label)                   # Plot density of x
      lines( (d2), col = "red", lty=w, lwd=ww)  
      lines( (d3), col = "blue", lty=w, lwd=ww)    
      lines( (d4), col = "green", lty=w, lwd=ww)          
      lines( (d5), col = "grey", lty=w, lwd=ww)       
      lines( (d6), col = "pink", lty=w, lwd=ww)       
      lines( (d7), col = "yellow", lty=w, lwd=ww)       
      lines( (d8), col = "purple", lty=w, lwd=ww)       
      
      abline(v = theta1, col = "darkgrey")                
      legend("topright",                                  # Add legend to density
             legend = c(" adj. for true prognostic covariates", 
                        " not adj. for true prognostic covariates" ,
                        " adj. for covariates unrelated to outcome", 
                        " not adj. for covariates unrelated to outcome",
                        " adj. for mix of prognostic and unrelated to outcome", 
                        " not adj. mix of prognostic and unrelated to outcome", 
                        " adj. for correlated prognostic covariates", 
                        " not adj. for correlated prognostic covariates"
                        
             ),
             col = c("black", "red","blue","green","grey", "pink", "yellow", "purple"),
             lty = w, lwd=ww, bty = "n", cex=.75)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # collect simulation trt effect standard error estimates from simulation and plot!
    
    output$reg.ploty <- renderPlot({         #standard errors
      
      # Get the  data
      
      res <- simul()$res
      res2 <- simul2()$res

      sample <- random.sample()
      sigma1=sample$sigma
      N1 <- mcmc()$N # 
      
      n1 <- mcmc()$placebo
      n2 <- mcmc()$treated
      
      d1 <-  density(res[,2] )
      d2 <-  density(res[,4] )
      d3 <-  density(res[,6] )
      d4 <-  density(res[,8] )
      d5 <-  density(res[,10] )
      d6 <-  density(res[,12] )
      d7 <-  density(res2[,2] )
      d8 <-  density(res2[,4] )
      
      #se. <-  sqrt((2*(sigma1^2 + sigma1^2)) /N1)  
      #se. <-  sqrt((sigma1^2+sigma1^2) / (N1/2) )  #ditto
      
      # we may have imbalance in numbers, otherwise the se will not be exactly correct and this maybe seen in plot
      se. <-  sqrt( sigma1^2/n1 + sigma1^2/n2 )   #ditto
      
      
      
      dz <- max(c(d1$y, d2$y, d3$y, d4$y, d5$y, d6$y, d7$y, d8$y))
      dx <- range(c(d1$x,d2$x,  d3$x, d4$x, d5$x, d6$x, d7$x, d8$x))
      
      plot( (d1), xlim = c(dx), main=paste0("Density of treatment standard error estimates, truth= ",p4(se.),""), ylim=c(0,dz),lty=w, lwd=ww,
            xlab="Standard error", #Change the x-axis label
            ylab="Density") #y-axis label)                   # Plot density of x
      lines( (d2), col = "red", lty=w, lwd=ww)  
      lines( (d3), col = "blue", lty=w, lwd=ww)    
      lines( (d4), col = "green", lty=w, lwd=ww)          
      lines( (d5), col = "grey", lty=w, lwd=ww)       
      lines( (d6), col = "pink", lty=w, lwd=ww)       
      lines( (d7), col = "yellow", lty=w, lwd=ww)       
      lines( (d8), col = "purple", lty=w, lwd=ww)     
      
      abline(v = se., col = "darkgrey")   
      # legend("topright",                                  # Add legend to density
      #        legend = c(" adj. for true prognostic covariates", 
      #                   " not adj. for true prognostic covariates" ,
      #                   " adj. for covariates unrelated to outcome", 
      #                   " not adj. for covariates unrelated to outcome",
      #                   " adj. for mix of prognostic and unrelated to outcome", 
      #                   " not adj. mix of prognostic and unrelated to outcome", 
      #                   " adj. for correlated prognostic covariates", 
      #                   " not adj. for correlated prognostic covariates"
      #                   
      #        ),
      #        col = c("black", "red","blue","green","grey", "pink", "yellow", "purple"),
      #        lty = w, lwd=ww, bty = "n", cex=.75)
    })
  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # this code writes coloured text to app, nice idea, lots of code, but not very clear, the simple table is better
    # output$textWithNumber2 <- renderText({ 
    #   
    #   res <- simul()$res  # means
    #   result <- simul()$result  # means
    #   result2 <- simul2()$result  # means
    #   
    #   q1.result <- simul()$q1.result  # means
    #   q2.result <- simul()$q2.result  # means
    #   
    #   q1.result2 <- simul2()$q1.result  # means
    #   q2.result2 <- simul2()$q2.result  # means
    #   
    #   adjusting <- "adjusting"
    #   ignoring <- "ignoring"
    #   
    #   HTML(paste0(  tags$hr(),
    #                  
    #                 "Mean and se ",
    #                 tags$span(style="color:green",   adjusting)  ,
    #                 " for true prognostic covariates (black lines) " 
    #                 
    #                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                 , tags$span(style="color:blue",  p3(result[1]))  ,
    #                 
    #                                      " 95%CI ("
    #                  , tags$span(style="color:blue",  p2(q1.result[1]))  ,
    #                  ", "
    #                  , tags$span(style="color:blue",  p2(q2.result[1]))  ,
    #                  " )",
    #                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                 
    #                  
    #                 " ; "  
    #                 , tags$span(style="color:red",  p3(result[2] )) ,
    #                 " power "
    #                 , tags$span(style="color:green",  p3(result[13] )) ,
    #                 " MSE "
    #                 , tags$span(style="color:purple",  p2(result[19] )) ,
    #                 " 95%CI ("
    #                 , tags$span(style="color:purple",  p2(result[25] )) ,
    #                 ", "
    #                 , tags$span(style="color:purple",  p2(result[26] )) ,
    #                 " )",
    #                 
    #                 
    #               br(), br(),
    #              
    #                   
    #                 
    #                 " Mean and se ",
    #                 tags$span(style="color:red",   ignoring) , 
    #                 " true prognostic in analysis (red lines) "
    #                 , tags$span(style="color:blue",  p3(result[3]  )),
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #               " 95%CI ("
    #               , tags$span(style="color:blue",  p2(q1.result[3]))  ,
    #               ", "
    #               , tags$span(style="color:blue",  p2(q2.result[3]))  ,
    #               " )",
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #               
    #                 " ; "
    #                 , tags$span(style="color:red",  p3(result[4] )) ,
    #                 " power "
    #                 , tags$span(style="color:green",  p3(result[14] )) ,
    #                 " MSE "
    #                 , tags$span(style="color:purple",  p2(result[20] )) ,
    #         
    #               " 95%CI ("
    #               , tags$span(style="color:purple",  p2(result[27] )) ,
    #               ", "
    #               , tags$span(style="color:purple",  p2(result[28] )) ,
    #               " )",
    #               
    #               
    #               
    #               
    #                 br(), br(),
    #                 "Mean and se ",
    #                 tags$span(style="color:green",   adjusting)  ,
    #                 " for non prognostic covariates (blue lines) "  
    #                 , tags$span(style="color:blue",  p3(result[5]))  ,
    #            
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #               " 95%CI ("
    #               , tags$span(style="color:blue",  p2(q1.result[5]))  ,
    #               ", "
    #               , tags$span(style="color:blue",  p2(q2.result[5]))  ,
    #               " )",
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #               
    #                 " ; "  
    #                 , tags$span(style="color:red",  p3(result[6] )) ,
    #                 " power "
    #                 , tags$span(style="color:green",  p3(result[15] )) ,
    #                 " MSE "
    #                 , tags$span(style="color:purple",  p2(result[21] )) ,
    #               " 95%CI ("
    #               , tags$span(style="color:purple",  p2(result[29] )) ,
    #               ", "
    #               , tags$span(style="color:purple",  p2(result[30] )) ,
    #               " )",
    #                 br(), br(),
    #                 
    #                 " Mean and se ",
    #                 tags$span(style="color:red",   ignoring) , 
    #                 "  non prognostic covariates (green lines) "
    #               
    #               
    #                 , tags$span(style="color:blue",  p3(result[7]  )),
    #               
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #               " 95%CI ("
    #               , tags$span(style="color:blue",  p2(q1.result[7]))  ,
    #               ", "
    #               , tags$span(style="color:blue",  p2(q2.result[7]))  ,
    #               " )",
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #               " ; "
    #               
    #                 , tags$span(style="color:red",  p3(result[8] )) ,
    #                 " power "
    #                 , tags$span(style="color:green",  p3(result[16] )) ,
    #                 " MSE "
    #                 , tags$span(style="color:purple",  p2(result[22] )) ,
    #               " 95%CI ("
    #               , tags$span(style="color:purple",  p2(result[31] )) ,
    #               ", "
    #               , tags$span(style="color:purple",  p2(result[32] )) ,
    #               " )",
    #                 br(), br(),
    #                 
    #                 "Mean and se ",
    #                 tags$span(style="color:green",   adjusting)  ,
    #                 " for mix of prognostic and non prognostic covariates (grey lines) "  
    #                 , tags$span(style="color:blue",  p3(result[9]))  ,
    #               
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #               " 95%CI ("
    #               , tags$span(style="color:blue",  p2(q1.result[9]))  ,
    #               ", "
    #               , tags$span(style="color:blue",  p2(q2.result[9]))  ,
    #               " )",
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #                 " ; "  
    #                 , tags$span(style="color:red",  p3(result[10] )) ,
    #                 " power "
    #                 , tags$span(style="color:green",  p3(result[17] )) ,
    #                 " MSE "
    #                 , tags$span(style="color:purple",  p2(result[23] )) ,
    #               " 95%CI ("
    #               , tags$span(style="color:purple",  p2(result[33] )) ,
    #               ", "
    #               , tags$span(style="color:purple",  p2(result[34] )) ,
    #               " )",
    #                 br(), br(),
    #                 
    #                 " Mean and se ",
    #                 tags$span(style="color:red",   ignoring) , 
    #                 " mix of prognostic and non prognostic covariates (pink lines) "
    #                 , tags$span(style="color:blue",  p3(result[11]  )),
    #                 " ; ",
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #               " 95%CI ("
    #               , tags$span(style="color:blue",  p2(q1.result[11]))  ,
    #               ", "
    #               , tags$span(style="color:blue",  p2(q2.result[11]))  ,
    #               " )",
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               " ; "  
    #                 , tags$span(style="color:red",  p3(result[12] )) ,
    #                 " power "
    #                 , tags$span(style="color:green",  p3(result[18] )) ,
    #                 " MSE "
    #                 , tags$span(style="color:purple",  p2(result[24] )) ,
    #               " 95%CI ("
    #               , tags$span(style="color:purple",  p2(result[35] )) ,
    #               ", "
    #               , tags$span(style="color:purple",  p2(result[36] )) ,
    #               " )",
    #                 br(), br(),
    #                 "Mean and se ",
    #                 tags$span(style="color:green",   adjusting)  ,
    #                 " adjusting for true prognostic correlated covariates (yellow lines) "  
    #                 , tags$span(style="color:blue",  p3(result2[1]))  ,
    #               
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #               " 95%CI ("
    #               , tags$span(style="color:blue",  p2(q1.result2[1]))  ,
    #               ", "
    #               , tags$span(style="color:blue",  p2(q2.result2[1]))  ,
    #               " )",
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #                 " ; "  
    #                 , tags$span(style="color:red",  p3(result2[2] )) ,
    #                 " power "
    #                 , tags$span(style="color:green",  p3(result2[5] )) ,
    #                 " MSE "
    #                 , tags$span(style="color:purple",  p2(result2[7] )) ,
    #               " 95%CI ("
    #               , tags$span(style="color:purple",  p2(result2[9] )) ,
    #               ", "
    #               , tags$span(style="color:purple",  p2(result2[10] )) ,
    #               " )",
    #                 br(), br(),
    #                 
    #                 " Mean and se ",
    #                 tags$span(style="color:red",   ignoring) , 
    #                 "  true prognostic correlated covariates (purple lines) "
    #                 , tags$span(style="color:blue",  p3(result2[3]  )),
    #               
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #               " 95%CI ("
    #               , tags$span(style="color:blue",  p2(q1.result2[3]))  ,
    #               ", "
    #               , tags$span(style="color:blue",  p2(q2.result2[3]))  ,
    #               " )",
    #               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               
    #               
    #                 " ; "
    #                 , tags$span(style="color:red",  p3(result2[4] )) ,
    #                 " power "
    #                 , tags$span(style="color:green",  p3(result2[6] )) ,
    #                 " MSE "
    #                 , tags$span(style="color:purple",  p2(result2[8] )) ,
    #               " 95%CI ("
    #               , tags$span(style="color:purple",  p2(result2[11] )) ,
    #               ", "
    #               , tags$span(style="color:purple",  p2(result2[12] )) ,
    #               " )",
    #                 
    #                 br(), br()
    #                 # "Mean squared error (MSE: accuracy and precision) combines bias and
    #                 # variance as (bias*bias+variance). It represents the total variation around the
    #                 # true value, rather than the average estimated value. MSE gives an overall sense of the quality of the
    #                 # estimator. As the MSE can be written as the sum of the variance of the estimator and the squared bias of the estimator, 
    #                 # this implies that in the case of unbiased estimators, the MSE and variance are equivalent. So compare the calculated MSE to the 
    #                 # true sigma squared 
    #                 # on the left input."
    #   ))    
    #   
    # })   
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       
    output$textWithNumber99 <- renderText({ 
      
      HTML(
                    "Mean squared error (MSE: accuracy and precision) combines bias and
                    variance as (bias*bias+variance). It represents the total variation around the
                    true value, rather than the average estimated value. MSE gives an overall sense of the quality of the
                    estimator. As the MSE can be written as the sum of the variance of the estimator and the squared bias of the estimator, 
                    this implies that in the case of unbiased estimators, the MSE and variance are equivalent. So compare the calculated MSE to the 
                    true sigma squared 
                    on the left input."
      )
      
    })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # table for simulation summary
    table.sim <- reactive({
        
      res <- simul()$res  
      result <- simul()$result  
      result2 <- simul2()$result  
      
      q1.result <- simul()$q1.result  
      q2.result <- simul()$q2.result  
      
      q1.result2 <- simul2()$q1.result  
      q2.result2 <- simul2()$q2.result  
    
      zz <- rbind(
         (c( p3(result[1])  ,     p2(q1.result[1])  ,  p2(q2.result[1])   , p3(result[2] ) ,  p2(result[13] ) ,  p2(result[19] ) ,   p2(result[25] ),   p2(result[26] )  ,   p2(result[37] )    ,   p2(result[43] )         )) ,
               (c( p3(result[3])  ,     p2(q1.result[3]) ,   p2(q2.result[3])   , p3(result[4] ) ,  p2(result[14] ) ,  p2(result[20] ) ,   p2(result[27] ) ,  p2(result[28] )   , p2(result[38] )      ,    p2(result[44] )        )) ,
                     (c( p3(result[5])  ,     p2(q1.result[5]) ,   p2(q2.result[5])   , p3(result[6] ) ,  p2(result[15] ) ,  p2(result[21] ) ,   p2(result[29] ) ,  p2(result[30] )  , p2(result[39] )      ,     p2(result[45] )        )) ,
                           (c( p3(result[7])  ,     p2(q1.result[7]) ,   p2(q2.result[7])   , p3(result[8] ) ,  p2(result[16] ) ,  p2(result[22] ) ,   p2(result[31] ) ,  p2(result[32] ) , p2(result[40] )      ,     p2(result[46] )         )) ,
                                 (c( p3(result[9])  ,     p2(q1.result[9]) ,   p2(q2.result[9])   , p3(result[10] ) , p2(result[17] ) ,  p2(result[23] ) ,   p2(result[33] ) ,  p2(result[34] ) , p2(result[41] )      ,   p2(result[47] )             )) ,
                                       (c( p3(result[11])  ,    p2(q1.result[11]) ,  p2(q2.result[11])  , p3(result[12] ) , p2(result[18] ) ,  p2(result[24] ) ,   p2(result[35] ) ,  p2(result[36] ) , p2(result[42] )      ,     p2(result[48] )         )) ,
                                             (c( p3(result2[1]),      p2(q1.result2[1]),   p2(q2.result2[1])  , p3(result2[2] ) , p2(result2[5] ) ,  p2(result2[7] ) ,   p2(result2[9] ) , p2(result2[10] ) ,  p2(result2[13] )      ,  p2(result2[15] )         )) ,
                                                   (c( p3(result2[3]),      p2(q1.result2[3])  , p2(q2.result2[3])  , p3(result2[4] ) , p2(result2[6] ) ,  p2(result2[8] ) ,   p2(result2[11] ) , p2(result2[12] ) , p2(result2[14] )      ,p2(result2[16] )          ))
    ) 
      
      zz <- as.data.frame(zz)
      
      colnames(zz) <- c("Mean  ", "Lower 95%CI", "Upper 95%CI", "Stand.error", "Power ","B", "MSE Low 95%CI", "MSE Upp 95%CI", "sigma","R2")
    
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
        " not adj. for correlated prognostic covariates")
      zz <- zz[order(zz$B),]
      
      colnames(zz) <- c("Mean  ", "Lower 95%CI", "Upper 95%CI", "Stand.error", "Power ","  MSE ", "MSE Low 95%CI", "MSE Upp 95%CI", "sigma", "Adj.R2")
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
      return(list(  
        
       zz=zz
        
      )) 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    })
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$norm.plot <- renderPlot({ 
 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #Select true population parameters and probability of errors
      mu1 <- 0  
      sd1 <-  as.numeric(unlist(strsplit(input$sdsd,",")))
      n1 <- n2 <- as.numeric(unlist(strsplit(input$NN,",")))
      alpha = as.numeric(unlist(strsplit(input$alpha2,","))) # "Alpha, Type I assertion",
      
      # --------------------------------------------------------------------------
      crit1 <- qnorm(1-as.numeric(alpha/2))
      
      x <- seq(mu1-6*sd1, mu1+6*sd1, 0.1)
      
      se1 <- sqrt((sd1^2/n1) + (sd1^2/n2))
      
     # crit <- mu1 + crit1 * se1
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # plotting limits
      upper <- mu1+6*se1
      lower <- mu1-6*se1
      gap=0.001
      # z of distibution alpha tresholds
      crit <-  mu1 + crit1 * se1  # how many ses above mean
      crit2 <- mu1 - crit1 * se1  # how many ses below mean
      # ranges for polygons, remember this is not N(0,1)
      xx <-    seq( crit,  upper,  by=gap)
      xxxx <-  seq( lower, crit2,  by=gap)
      xxxxx <- seq( crit2, crit,   by=gap)
      # co-ordinates for placement of text
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #  the distribution of null true effect 0 (null), 
      curve(dnorm(x, mean =mu1, sd= se1), xlim=c(lower, upper),
            bty="n",yaxt="n",lwd=2,  #xaxt="n", 
            col='red',
            ylab='',xlab='Distribution of differences under the null', 
            main=paste0("Sampling distribution of the null, mean difference between the two groups of covariates. 
       We have = ",p0(n1)," in each group and alpha two-sided = ",alpha,". Under the null, diff = ",mu1 ,", SE = ",p4(se1),", critical value = +/- ", p4(crit),""
            )) 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # type 1 error upper
      polygon(x=c(crit,                                 xx, upper),
              y=c(0,    dnorm(mean=mu1, sd=se1 ,        xx),    0),
              col="red")
      
      # null distribution
      polygon(x=c(crit2,                               xxxxx,  crit),
              y=c(0,   dnorm(mean=mu1, sd=se1 ,        xxxxx),    0),
              col=rgb(1,0,0,alpha=0.3))
      
      # type 1 error lower
      polygon(x=c(lower,                               xxxx, crit2),
              y=c(0,   dnorm(mean=mu1, sd=se1 ,        xxxx),    0),
              col="red")
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # the value 97.5 when we have standard error
      pnorm(crit, 0, se1)
      #  the value at .975
      qnorm(.975,0,se1) 
      
    }) 
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$textWithNumber1 <- output$textWithNumber1 <- renderText({ 
      
      placebo <- mcmc()$placebo
      treated <- mcmc()$treated
      bigN <- mcmc()$bigN
      
      HTML(paste0(  tags$hr(),
                    "Randomised 1:1 we have  "  
                    , tags$span(style="color:red",   placebo)  ,
                    " placebo patients and  "  
                    , tags$span(style="color:red",  treated ) ,
                    " treated patients, so in total "
                    , tags$span(style="color:red",  bigN  ),
                    " patients. Using only one realisation we make some observations. To be more certain of findings requires numerous simulations, see simulation tab.", 
                    tags$hr()
      ))    
      
    })  
    
    output$textWithNumber1a <- renderText({ 
      
      placebo <- mcmc()$placebo
      treated <- mcmc()$treated
      bigN <- mcmc()$bigN
      
      HTML(paste0(  #tags$hr(),
                    "Figure 1 Simulation results. Randomised 1:1, we have  "  
                    
                    ,tags$span(style="color:red",  bigN  ),
                    " total patients randomised 1:1 for each simulation. The true covariate coefficients are fixed at the same values for all simulations
                      and are selected randomly between +/- multiples of the treatment effect, as dictated by the input on left. The true covariate coefficients are printed at the bottom."
                  #  tags$hr()
                  
      ))    
      
    })
    
    output$textWithNumber1b <-   output$textWithNumber1 <- renderText({ 
      
      placebo <- mcmc()$placebo
      treated <- mcmc()$treated
      bigN <- mcmc()$bigN
      
      HTML(paste0(  tags$hr(),
                    "Randomised 1:1 we have  "  
                    , tags$span(style="color:red",   placebo)  ,
                    " placebo patients and  "  
                    , tags$span(style="color:red",  treated ) ,
                    " treated patients, so in total "
                    , tags$span(style="color:red",  bigN  ),
                    " patients. Using only one realisation we make some observations.", 
                    tags$hr()
                    
      ))    
      
    })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$textWithNumber3 <- renderText({ 
      
      power <- mcmc()$bigN  # means
     
      HTML(paste0(  #tags$hr(),
                    
                    "Based on the study design dictated by user inputs we will randomise ",
                    tags$span(style="color:green",   power)  ,
                    " patients in a 1:1 fashion" ,
                                        br(), br()

      ))    
      
    })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # output$reg.plotxx <- renderPlot({         
    #   
    #   # Get the  data
    #   res <- simul()$res
    #   res2 <- simul2()$res
    # 
    #   sample <- random.sample()
    #   theta1=sample$theta     
    #   
    #   d1 <-  density(res[,1] )
    #   d2 <-  density(res[,3] )
    #   d3 <-  density(res[,5] )
    #   d4 <-  density(res[,7] )
    #   d5 <-  density(res[,9] )
    #   d6 <-  density(res[,11] )
    #   d7 <-  density(res2[,1] )
    #   d8 <-  density(res2[,3] )
    #   
    #   dz <- max(c(d1$y, d2$y, d3$y, d4$y, d5$y, d6$y, d7$y, d8$y))
    #   dx <- range(c(d1$x,d2$x,  d3$x, d4$x, d5$x, d6$x, d7$x, d8$x))
    #   dx <- range(c(d1$x,  d3$x, d4$x, d5$x))
    # 
    #   plot((d1), xlim = dx, main=paste0("Density of treatment estimates (zoomed in), truth= ",p3(theta1),""), ylim=c(0,dz),lty=w, lwd=ww,
    #         xlab="Treatment effect", #Change the x-axis label
    #         ylab="Density") #y-axis label)                   # Plot density of x
    #   lines( (d2), col = "red", lty=w, lwd=ww)  
    #   lines( (d3), col = "blue", lty=w, lwd=ww)    
    #   lines( (d4), col = "green", lty=w, lwd=ww)          
    #   lines( (d5), col = "grey", lty=w, lwd=ww)       
    #   lines( (d6), col = "pink", lty=w, lwd=ww)       
    #   lines( (d7), col = "yellow", lty=w, lwd=ww)       
    #   lines( (d8), col = "purple", lty=w, lwd=ww)       
    # 
    #   abline(v = theta1, col = "darkgrey")                  
    #   
    # })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # output$reg.plotyy <- renderPlot({         
    #   
    #   res <- simul()$res
    # 
    #   res2 <- simul2()$res
    #  
    #   sample <- random.sample()
    #   sigma1=sample$sigma
    #   N1 <- mcmc()$N # 
    #   
    #   se. <-  sqrt((2*(sigma1^2 + sigma1^2)) /N1)  
    #   se. <-  sqrt((sigma1^2+sigma1^2) / (N1/2) )  #ditto
    #   
    #   d1 <-  density(res[,2] )
    #   d2 <-  density(res[,4] )
    #   d3 <-  density(res[,6] )
    #   d4 <-  density(res[,8] )
    #   d5 <-  density(res[,10] )
    #   d6 <-  density(res[,12] )
    #   d7 <-  density(res2[,2] )
    #   d8 <-  density(res2[,4] )
    #   
    #   dz <- max(c(d1$y, d2$y, d3$y, d4$y, d5$y, d6$y, d7$y, d8$y)) 
    #   dx <- range(c(d1$x,  d3$x, d4$x, d5$x)) #ignore some of the distributions
    #   
    #   plot( (d1), xlim = c(dx), main=paste0("Density of treatment standard error estimates (zoomed in), truth= ",p4(se.),""), ylim=c(0,dz),lty=w, lwd=ww,
    #         xlab="Standard error", #Change the x-axis label
    #         ylab="Density") #y-axis label)                   # Plot density of x
    #   lines( (d2), col = "red", lty=w, lwd=ww)  
    #   lines( (d3), col = "blue", lty=w, lwd=ww)    
    #   lines( (d4), col = "green", lty=w, lwd=ww)          
    #   lines( (d5), col = "grey", lty=w, lwd=ww)       
    #   lines( (d6), col = "pink", lty=w, lwd=ww)       
    #   lines( (d7), col = "yellow", lty=w, lwd=ww)       
    #   lines( (d8), col = "purple", lty=w, lwd=ww)     
    #   
    #   abline(v = se., col = "darkgrey")          
      
      # legend("topright",                                  # Add legend to density
      #        legend = c(" adj. for true prognostic covariates", 
      #                   " not adj. for true prognostic covariates" ,
      #                   " adj. for covariates unrelated to outcome", 
      #                   " not adj. for covariates unrelated to outcome",
      #                   " adj. for mix of prognostic and unrelated to outcome", 
      #                   " not adj. mix of prognostic and unrelated to outcome", 
      #                   " adj. for correlated prognostic covariates", 
      #                   " not adj. for correlated prognostic covariates"
      #                   
      #        ),
      #        col = c("black", "red","blue","green","grey", "pink", "yellow", "purple"),
      #        lty = w, lwd=ww, bty = "n", cex=.75)
   
    #})

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # text 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
     output$betas <- renderPrint({
      
      d <- simul2()$betas
      return(print(d))
      
    })
     ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
    output$dat <- renderPrint({
        
        d <- mcmc()$dat
        
        return(print(d, digits=4))
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$zz <- renderPrint({
      
      d <- table.sim()$zz
      
     # d <- plyr::arrange(d,  (d[,7])) 
      
      return(d)
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$fake2 <- renderPrint({
      
      d <- mcmc()$fake2
      
       
      return(print(d, digits=4))
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$fake3 <- renderPrint({
      
      d <- mcmc()$fake3
      
      return(print(d, digits=4))
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$fake4 <- renderPrint({
      
      d <- mcmc()$fake4
      
       
      return(print(d, digits=4))
    })
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})
 
# Run the application 
shinyApp(ui = ui, server = server)