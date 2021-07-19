# Loading packages ----
library(shiny)
library(shinythemes)
library(tidyverse)
library(fastGraph)
library(ggridges)
library(ggplot2)

# Shiny user interface ----
ui <- fluidPage(
                theme = "bootstrap.min.css",
                includeCSS("www/bootstrap.min.css"),
                navbarPage(
                           title = "Assurance in Clinical Trial Design", collapsible = TRUE, inverse = TRUE, theme = tags$head(tags$link(rel="stylesheet", type="text/css", href="bootstrap.min.css")),
                           tabPanel(
                                    title = "Introduction",
                                    withMathJax(),
                                    tags$style(HTML(".tabbable > .nav > li > a
                                              {background-color: FloralWhite;  color:black}")),
                                    
                                    fluidPage(
                                              tags$h3("What is assurance?"),
                                              img(src = "clinical_trial.jpg", height = 140, width = 300),
                                              p("In context of a clinical trial, the assurance is defined as the")
                                              )
                                    ),
                                
                           tabPanel(
                                    title = "Theory",
                                    fluidPage(
                                              tags$style(HTML(".tabbable > .nav > li > a                  
                                              {background-color: FloralWhite;  color:black}")),
                                              
                                              tabsetPanel(
            
                                                          )
                                              )
                                    ),
                           tabPanel(
                                    title = "R Computation",
                                    fluidPage(
                                            
                                              tabsetPanel(
                                                          tabPanel(
                                                                   title = "Elicitation of survival rate \\(S_1(t_0)\\)",
                                                                   br(),
                                                                   fluidRow(
                                                                            p("Here we elicit a distribution for the unknown quantity \\(S_1(t_0)\\), this is the survival rate of the control group in the clinical trial at time \\(t = t_0 \\).
                                                                               In this elicitation we assume that the prior probability distribution for \\(S_1(t_0)\\) is a beta distribution with shape parameters
                                                                               dependent upon the beliefs and judgements proposed by the expert of the clinical trial."),

                                                                            p("The application seen below is an elicitation tool which derives a prior probability distribution for the unknown
                                                                                quantity \\(S_1(t_0)\\), the expert enteres a vector of numeric values (\\(v_1\\), \\(v_2\\), ..., \\(v_n\\)) along with a vector of probabilities 
                                                                                (\\(p_1\\), \\(p_2\\), ..., \\(p_n\\)) such that they believe \\(P[S_1(t_0) \\leq v_i] = p_i\\) for \\(1 \\leq i \\leq n\\).
                                                                                "),
                                                                            ),
                                                                   fluidRow(
                                                                             column(
                                                                               width = 4,
                                                                               textInput(inputId = "surv_times", label = "Survival Proportion Values", value = "0.7, 0.8, 0.9")
                                                                             ),
                                                                             column(
                                                                               width = 4,
                                                                               textInput(inputId = "cumulative_probs_beta", label = "Cumulative Probabilities", value = "0.25, 0.5, 0.75")
                                                                             ),
                                                                             column(
                                                                               width = 4,
                                                                               numericInput(inputId = "at_time", label = "At time:", value = 1)
                                                                             )
                                                                             ),
                                                                   fluidRow(
                                                                           column(
                                                                             width = 6,
                                                                             plotOutput("pdf_st")
                                                                             ),
                                                                            column(
                                                                              width = 6,
                                                                              plotOutput("cdf_st")
                                                                            )
                                                                           
                                                                         )
                                                                   ),
                                                          
                                                          tabPanel(
                                                                   title = "Elicitation of treatment effect \\(\\rho\\)", dataTableOutput("iris_type"),
                                                                   br(),
                                                                   fluidRow(
                                                                             p("Here we elicit a distribution for the treatment effect difference \\(\\rho\\),
                                                                                that is the difference in the survival time
                                                                                between the control and experimental group. A positive observation for \\(\\rho\\) would
                                                                                indicate a difference in favor of the treatment whereas a negative observation
                                                                                would indicate a difference in favor of the standard treatment recieved by the control group.
                                                                                It is assumed that this unknown value follows a normal distribution with mean and
                                                                                varience dependent upon the beliefs and judgements proposed by the expert
                                                                                of the clinical trial."),
                                                                                
                                                                              p("The application seen below is an elicitation tool which derives a prior probability distribution for the unknown
                                                                                value \\(\\rho\\), the expert enteres a vector of numeric values (\\(v_1\\), \\(v_2\\), ..., \\(v_n\\)) along with a vector of probabilities 
                                                                                (\\(p_1\\), \\(p_2\\), ..., \\(p_n\\)) such that they believe \\(P[\\rho \\leq v_i] = p_i\\) for \\(1 \\leq i \\leq n\\).
                                                                                "),
                                                                             
                                                                             column(
                                                                               width = 4,
                                                                               textInput(inputId = "treat_effect", label = "Treatment Effect", value = "0, 0.1, 0.2")
                                                                             ),
                                                                             column(
                                                                               width = 4,
                                                                               textInput(inputId = "cumulative_probs", label = "Cumulative Probabilities", value = "0.25, 0.5, 0.75")
                                                                             ),
                                                                             column(
                                                                               width = 4,
                                                                               textInput(inputId = "domain", label = "Plausable Range", value = "-1.5, 1.5")     
                                                                             )
                                                                             ),
                                                                   fluidRow(
                                                                            column(
                                                                                   width = 6,
                                                                                   plotOutput("pdf_rho")
                                                                                   ),
                                                                            column(
                                                                                   width = 6,
                                                                                   plotOutput("cdf_rho")
                                                                                   )
                                                                            )
                                                                   ),
                                                          
                                                          tabPanel(
                                                                   title = "Implied survival distributions",
                                                                   br(),
                                                                   fluidRow(
                                                                     
                                                                     p("This tab displays feedback on the survival proportions in each of the treatment groups at some particular time in the trial. 
                                                                       The survival feedback visualised below is a direct consequence of the 
                                                                       beliefs and judgements the user elicited about the observable quantities in the previous tabs. The user is 
                                                                       required to enter a time point into the trial e.g. 2 years, then using the simulated data the user can judge 
                                                                       whether the treatment is effective after the specified time since the trial started. If the user elicited the 
                                                                       treatment to have a positive effect on the experimental group then they should observe that the population mean
                                                                       within the control group is lower than the experimental group, that is the points on the scatter plot are more abundant above the 
                                                                       line of \\(y = x \\) than below.")
                                                                     
                                                                   ),
                                                                   
                                                                   fluidRow(
                                                                     column(
                                                                       width = 4,
                                                                       numericInput(inputId = "at_time_t1", label = "Enter the time point of interest:", value = 2)
                                                                     )
                                                                   ),
                                                                  
                                                                   
                                                                   fluidRow(
                                                                     column(
                                                                       width = 6,
                                                                       plotOutput("surv_distributions")
                                                                     ),
                                                                     column(
                                                                       width = 6,
                                                                       plotOutput("surv_boxplot")
                                                                     )
                                                                     ),
                                                                   fluidRow(
                                                                     column(width = 6),
                                                                     column(
                                                                       width = 6,
                                                                       verbatimTextOutput("anova_output")
                                                                     )
                                                                   )
                                                                   ),
                                                          
                                                          tabPanel(
                                                                   title = "Assurance calculation",
                                                                   br(),
                                                                   fluidRow(
                                                                     p("Here, using all the infomation gathered in the previous tabs, the assurance probability is calculated.
                                                                       Using Monte Carlo simulation methods, many trials can be run a sufficiently large number of times until 
                                                                       the mean of the probability that the null hypothesis will be successfully rejected, with data favouring the experimental 
                                                                       treatment converges towards the assurance. The two-sided hypothesis test within each trial is conducted at a 5% significance level.")
                                                                   ),
                                                                   p("The user can modify the number of patients in each arm of the trial to obtain a higher assurance however still constricted to the prior probability. 
                                                                     They are also able specify the length of the limited recruitment period and the time of the trial, both of which cause perturbations in assurance of the trial."
                                                                   ),
                                                                   fluidRow(
                                                                     column(
                                                                       width = 3,
                                                                       numericInput(inputId = "N_control", label = "Number of patients in control group", value = 50)
                                                                     ),
                                                                     column(
                                                                       width = 3,
                                                                       numericInput(inputId = "N_treatment", label = "Number of patients in experimental group", value = 50)
                                                                     ),
                                                                     
                                                                       column(
                                                                         width = 3,
                                                                         numericInput(inputId = "recruitment_time", label = "Recruitment Time", value = 1)
                                                                       ),
                                                                       column(
                                                                         width = 3,
                                                                         numericInput(inputId = "trial_time", label = "Trial Time", value = 10)
                                                                       )
                                                                     ),
                                                                   fluidRow(
                                                                       column(
                                                                         width = 4,
                                                                         verbatimTextOutput("assurance")
                                                                       )
                                                                     ),
                                                                   fluidRow(
                                                                     column(
                                                                       width = 1
                                                                     ),
                                                                     column(
                                                                       width = 10,
                                                                       align = "center",
                                                                       plotOutput("assurance_plot")
                                                                     ),
                                                                     column(
                                                                       width = 1
                                                                     )
                                                                   )
                                                                      
                                                                   
                                                                   )
                                                          )
                                             )
                                  ),
                           tabPanel(
                                    title = "About",
                                    fluidPage(
                                      p("This app implements the methodology seen in Ren & oakley, S.R.&.J.E.O. 2013. Assurance calculations for planning clinical trials with time-to-event outcomes. [Online]. [15 June 2021]. Available from: ", tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4280895/", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4280895/", target="_blank"), "." 
                                        ),
                                      p("This was the final result of the SURE (Summer Undergraduate Research Experience) produced by Bradley M Howard under supervision and aid of Professor Jeremy E Oakley", "(see", tags$a(href="http://www.jeremy-oakley.staff.shef.ac.uk/", "http://www.jeremy-oakley.staff.shef.ac.uk/", target="_blank"), ")", "at the University of sheffield.
                                        ")
                                              )
                                    )
                           )
                )

# Shiny server ----
server <- function(input, output){
  
  
  # Useful functions
  string_to_num_vector <- function(string){
    numeric_vec <- as.numeric(unlist(strsplit(string, ",")))
    return(numeric_vec)
  } 
  
  lambdas <- function(surv_rate, t0 = at_time()){
    lambda <- -log(surv_rate)/t0
    return(lambda)
  }
  
  prob_outcome <- function(lambda_vec){
    p <- 1 - (exp(-lambda_vec*(T.() - R.())) - exp(-lambda_vec*T.()))/(lambda_vec*R.())
  }
  
  # Constructs a prior probability distribution for the treatment effect rho ----

  # Fit distribution to elicited judgement for rho ----
  
  # Reactive variables associated with rho
  pRho <-  reactive({
    string_to_num_vector(input$cumulative_probs)
    })
  
  vRho <- reactive({
    string_to_num_vector(input$treat_effect)
    })
  
  domain <- reactive({
    string_to_num_vector(input$domain)
  })
  
  distributionRho <- reactive({
    SHELF::fitdist(vals = vRho(),
                   probs = pRho(),
                   lower = domain()[1],
                   upper = domain()[2])
  })
  
  normalParams <- reactive({
    params <- distributionRho()$Normal
    mu <- params[1,1]
    sigma <- params[1,2]
    c(mu, sigma)
  }) 
  
  # Plots the probability density function for the random variable rho
  output$pdf_rho <- renderPlot ({
    curve(dnorm(x, mean = normalParams()[1], sd = normalParams()[2]), from = domain()[1], to = domain()[2], 
          main = sprintf("Normal Density Function (mean = %.2f, sd = %.2f)", normalParams()[1], normalParams()[2]), 
          xlab = "Treatment Effect", ylab = "Probability Density", lwd = 2)
    
    cord.x <- c(domain()[1],seq(domain()[1],vRho()[1],0.005),vRho()[1]) 
    cord.y <- c(0,dnorm(seq(domain()[1],vRho()[1],0.005), mean = normalParams()[1], sd = normalParams()[2])-1/10^3,0)
    polygon(cord.x, cord.y, col="skyblue")
    
    last <- tail(vRho(), n=1)
    
    cord.x <- c(last,seq(last,domain()[2],0.005),domain()[2]) 
    cord.y <- c(0,dnorm(seq(last,domain()[2],0.005), mean = normalParams()[1], sd = normalParams()[2])-1/10^3,0)
    polygon(cord.x, cord.y, col="skyblue")
    
    })
  
  
  # Plots the cumulative distribution function for the random variable rho
  output$cdf_rho <- renderPlot ({
      curve(pnorm(x, mean = normalParams()[1], sd = normalParams()[2]), 
            from = domain()[1], to = domain()[2], 
            main = "Cumulative Distribution Function", 
            xlab = "Treatment Effect", ylab = "Probability Density", lwd = 2)
      
      points(vRho(),pnorm(vRho(),mean = normalParams()[1], sd = normalParams()[2]), pch = 16, 
             col = "black", cex = 1.5)
    })
    
    
    
  # Constructs a prior probability distribution for the treatment effect S_t(0) ----
  
  # Fit distribution to elicited judgement for S_t(0) ----
  
  # Reactive variables associated with S_t(0)
  pS_t0 <- reactive({
    string_to_num_vector(input$cumulative_probs_beta)})
  
  vS_t0 <- reactive({
    string_to_num_vector(input$surv_times)})
  
  at_time <- reactive({
    input$at_time
  })
  
  distributionS_t_0 <- reactive({
    SHELF::fitdist(vals = vS_t0(),
                   probs = pS_t0(), lower = 0, upper = 1)
  })
  
  betaParams <- reactive({
    params <- distributionS_t_0()$Beta
    alpha <- params[1,1]
    beta <- params[1,2]
    c(alpha, beta)
  }) 
  
  
    # Constructs a prior probability density function for the survival times s(t)
    output$pdf_st <- renderPlot({
      curve(dbeta(x, shape1 = betaParams()[1], shape2 = betaParams()[2]), from = -0.5, to = 1.5, 
            main = sprintf("Beta Density Function (alpha = %.2f, beta = %.2f)", betaParams()[1], betaParams()[2]), 
            xlab = "Proportion of patients surviving", ylab = "Probability Density", lwd = 2)
      
      cord.x <- c(0,seq(0,vS_t0()[1],0.005),vS_t0()[1]) 
      cord.y <- c(0,dbeta(seq(0,vS_t0()[1],0.005), shape1 = betaParams()[1], shape2 = betaParams()[2])-1/10^3,0)
      polygon(cord.x, cord.y, col="lightcoral")
      
      last <- tail(vS_t0(), n=1)
      
      cord.x <- c(last,seq(last,1,0.005),1) 
      cord.y <- c(0,dbeta(seq(last,1,0.005), shape = betaParams()[1], shape2 = betaParams()[2])-1/10^3,0)
      polygon(cord.x, cord.y, col="lightcoral")
      
    })
    
    
    # Constructs a prior cumulative distribution function for the survival times s(t)
    output$cdf_st <- renderPlot ({
      curve(pbeta(x, shape1 = betaParams()[1], shape2 = betaParams()[2]), from = -0.5, to = 1.5, 
            main = "Cumulative Distribution Function", 
            xlab = "Treatment Effect", ylab = "Probability Density", lwd = 2)
      
      points(vS_t0(),pbeta(vS_t0(), shape1 = betaParams()[1], shape2 = betaParams()[2]), pch = 16, 
             col = "black", cex = 1.5)
    })
    
    # Sample lambda parameters for control and treatment groups ----
    
    lambdaSample <- reactive({

      nS <- 10000

      nS<- 10000

      S1_t0 <- SHELF::sampleFit(distributionS_t_0(), n = nS)[, "beta"]
      rho <- SHELF::sampleFit(distributionRho(), n = nS)[, "normal"]
      S2_t0 <- S1_t0 + rho
      check <- S2_t0 > 0 & S2_t0 < 1
      lambda_control <- lambdas(S1_t0[check])
      lambda_treat <- lambdas(S2_t0[check])
      data.frame(lambda_control, lambda_treat)
    })
    
    
    # Survival Feedback ---
    at_time_t1 <- reactive({
      input$at_time_t1
    })
    
    short_lambdaSample <- reactive({
      rows <- dim(lambdaSample())[1]
      lambdaSample()[seq(1, rows, 10),]
    })
    
    short_survProp <- reactive({
      lambda1 <- short_lambdaSample()[, 1] # Control lambdas
      lambda2 <- short_lambdaSample()[, 2] # Treatment lambdas
      
      S1t1 <- exp(-at_time_t1() * lambda1)
      S2t1 <- exp(-at_time_t1() * lambda2)
      
      data.frame(S1t1, S2t1)
    })
    
    output$surv_distributions <- renderPlot ({
      
      S1t1 <- short_survProp()[, 1]
      S2t1 <- short_survProp()[, 2]

      plot(S1t1, S2t1, main = sprintf("Scatter plot displaying survival proportions \n in control group vs treatment group at time t = %.1f", at_time_t1()),
           xlab = "Survival proportions in control group", ylab = "Survival proportions in treatment group")
      abline(v = quantile(S1t1, probs = c(0.025, 0.5, 0.975)),
             lty = 2)
      abline(h = quantile(S2t1, probs = c(0.025, 0.5, 0.975)),
             lty = 2)
      abline(0,1)
    })
    
    
    output$surv_boxplot <- renderPlot({
      boxplot(short_survProp(), 
              main = sprintf("Boxplot displaying how survival proportions \n differ with treatment group at time t = %.1f", at_time_t1()),
              names = c("Control", "Treatment"), xlab = ("Group in clinical trial"), ylab = "Survival proportion")
    })
    
    
    output$anova_output <- renderPrint({
      short_rows <- dim(short_survProp())[1] 
      
      str_control <- rep("control", short_rows)
      str_treat <- rep("treat", short_rows)
      
      surv_prop_control <- short_survProp()[,1]
      surv_prop_treat <- short_survProp()[,2]
      
      type_of_treat <- as.factor(c(str_control, str_treat))
      surv_proportions <- c(surv_prop_control,surv_prop_treat)
      
      anova_data <- data.frame(surv_proportions, type_of_treat)
      
      lmReduced <- lm(surv_proportions~1, anova_data)
      lmFull <- lm(surv_proportions~type_of_treat, anova_data)
      
      print(anova(lmReduced, lmFull))
    })
    
    
    # Assurance Calculation ----
    assurance_calc <- function(theta, n1 = trial_size()[1], n2 = trial_size()[2], p1e, p2e, alpha = 0.05){
      test_statistic <- theta/sqrt(1/(n1*p1e) + 1/(n2*p2e))
      normal_obs <- test_statistic - qnorm(1-alpha/2)
      probability <- pnorm(normal_obs)
      assurance <- mean(probability)
      return(assurance)
    }
    
    R. <- reactive({
      input$recruitment_time
    })
    
    T. <- reactive({
      input$trial_time
    })
    
    trial_size <- reactive({
      c(input$N_control, input$N_treatment)
    })
    
    output$assurance <- renderPrint({
      
      thetas <- log(lambdaSample()[,2]/lambdaSample()[,1])
      
      P1e <- prob_outcome(lambdaSample()[,1])
      P2e <- prob_outcome(lambdaSample()[,2])
      
      assurance <- assurance_calc(theta = thetas, p1e = P1e, p2e = P2e)
      
      print(sprintf("Assurance of your clinical trial: %.4f", assurance))
    })
    
    output$assurance_plot <- renderPlot({
      thetas <- log(lambdaSample()[,2]/lambdaSample()[,1])
      
      P1e <- prob_outcome(lambdaSample()[,1])
      P2e <- prob_outcome(lambdaSample()[,2])
      
      sizes <- c(25, 50, 100, 250, 500, 1000, 2000, 4000)
      
      assurances <- vector()
      
      for(N in sizes){
        assurance <- assurance_calc(theta = thetas, n1 = N, n2 = N, p1e = P1e, p2e = P2e)
        assurances <- append(assurances, assurance)
      }
      
      final <- tail(assurances, 1)
      
      plot(sizes, assurances, ylim = c(0,final + 0.3), 
           main = "Horizontal asymptote depicting the assurance converging to the prior probability that the treatment is effective", 
           xlab = "Number of patients per arm", ylab = "Probability",
           cex = 1.5, pch = 20)
      
      if(trial_size()[1] == trial_size()[2]){
        your_assurance <- assurance_calc(thetas, n1 = trial_size()[1], n2 = trial_size()[2], p1e = P1e, p2e = P2e)
        points(c(trial_size()[1]), c(your_assurance), 
               col = "red", cex = 1.5, pch = 20)
        segments(x0 = trial_size()[1], y0 = -10, x1 = trial_size()[1], y1 = your_assurance, col = "red", lty = 2)
        segments(x0 = -1000, y0 = your_assurance, x1 = trial_size()[1], y1 = your_assurance, col = "red", lty = 2)
        text(trial_size()[1], your_assurance, sprintf("Your Assurance = %.4f", your_assurance), srt = 0, pos = 4)
      }
      
      abline(h = final, col = "blue", lty = 2)
      text(3000, final, "Assurance converging to prior", srt = 0, pos = 3)
    })
}

# Run shiny app ----
shinyApp(ui = ui, server = server,
         options = list(launch.browser = TRUE))
