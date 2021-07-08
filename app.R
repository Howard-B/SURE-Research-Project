# Loading packages ---------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(fastGraph)
library(ggridges)

# Shiny user interface ---------------------------
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
                                                                   fluidRow(
                                                                            p(".",style="color:white"),
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
                                                                               textInput(inputId = "surv_times", label = "Survival Time Values", value = "0.2, 0.3, 0.4")
                                                                             ),
                                                                             column(
                                                                               width = 4,
                                                                               textInput(inputId = "cumulative_probs_beta", label = "Cumulative Probabilities", value = "0.25, 0.5, 0.75")
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
                                                                   fluidRow(
                                                                             p(".", style="color:white"),
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
                                                                               textInput(inputId = "treat_effect", label = "Treatment Effect", value = "0, 0.5, 1")
                                                                             ),
                                                                             column(
                                                                               width = 4,
                                                                               textInput(inputId = "cumulative_probs", label = "Cumulative Probabilities", value = "0.25, 0.5, 0.75")
                                                                             ),
                                                                             column(
                                                                               width = 4,
                                                                               textInput(inputId = "domain", label = "Domain", value = "-4, 4")     
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
                                                                   fluidRow(
                                                                     
                                                                     p("The distribution for the survival time is an exponential distribution."),
                                                                         
                                                                     column(
                                                                       width = 6,
                                                                       plotOutput("surv_distributions")
                                                                     ),
                                                                     column(
                                                                       width = 6,
                                                                       plotOutput("exp_plot")
                                                                     )
                                                                     )
                                                                   ),
                                                          
                                                          tabPanel(
                                                                   title = "Assurance calculation",
                                                                   )
                                                          )
                                             )
                                  ),
                           tabPanel(
                                    title = "About",
                                    fluidPage(
                                      
                                              )
                                    )
                           )
                )

# Shiny server ---------------------------
server <- function(input, output) {
  
  string_to_num_vector <- function(string) {
    numeric_vec <- as.numeric(unlist(strsplit(string, ",")))
    return(numeric_vec)
  } 
  
  lambdas <- function(surv_rate, t0 = 1) {
    lambda <- -log(surv_rate)/t0
    return (lambda)
  }
  
  # Constructs a prior probability distribution for the treatment effect rho ----
  output$pdf_rho <- renderPlot ({
    p <- string_to_num_vector(input$cumulative_probs)
    v <- string_to_num_vector(input$treat_effect)
    
    params <- SHELF::fitdist(vals = v, probs = p)$Normal
    mu <- params[1,1]
    sig <- params[1,2]
    
    domain <- string_to_num_vector(input$domain)
    
    curve(dnorm(x, mean = mu, sd = sig), from = domain[1], to = domain[2], 
          main = sprintf("Normal Density Function (mean = %.2f, sd = %.2f)", mu, sig), 
          xlab = "Treatment Effect", ylab = "Probability Density", lwd = 2)
    
    cord.x <- c(domain[1],seq(domain[1],v[1],0.005),v[1]) 
    cord.y <- c(0,dnorm(seq(domain[1],v[1],0.005), mean = mu, sd = sig)-1/10^3,0)
    polygon(cord.x, cord.y, col="skyblue")
    
    last <- tail(v, n=1)
    
    cord.x <- c(last,seq(last,domain[2],0.005),domain[2]) 
    cord.y <- c(0,dnorm(seq(last,domain[2],0.005), mean = mu, sd = sig)-1/10^3,0)
    polygon(cord.x, cord.y, col="skyblue")
    
    })
  
  
  
  # Fit distribution to elicited judgements for rho ----
  
  
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
  
  output$cdf_rho <- renderPlot ({
     
      params <- distributionRho()$Normal
      mu <- params[1,1]
      sig <- params[1,2]
      
     
      curve(pnorm(x, mean = mu, sd = sig), 
            from = domain()[1], to = domain()[2], 
            main = "Cumulative Distribution Function", 
            xlab = "Treatment Effect", ylab = "Probability Density", lwd = 2)
      
      points(vRho(),pnorm(vRho(),mean=mu,sd=sig), pch = 16, 
             col = "black", cex = 1.5)
    })
    
    
    
  
  # Fit distribution to elicited judgements for S_t(0) ----
    
  pS_t0 <-  reactive({
    string_to_num_vector(input$cumulative_probs_beta)})
  
  vS_t0 <- reactive({
    string_to_num_vector(input$surv_times)})
  
  
    distributionS_t_0 <- reactive({
      SHELF::fitdist(vals = vS_t0(),
                     probs = pS_t0(), lower = 0, upper = 1)
    })
    
    # Constructs a prior probability distribution for the survival times s(t)
    output$pdf_st <- renderPlot({
      
      params <- distributionS_t_0()$Beta
      alpha <- params[1,1]
      beta <- params[1,2]
      
      curve(dbeta(x, shape1 = alpha, shape2 = beta), from = -0.5, to = 1.5, 
            main = sprintf("Beta Density Function (alpha = %.2f, beta = %.2f)", alpha, beta), 
            xlab = "Survival Time", ylab = "Probability Density", lwd = 2)
      
      cord.x <- c(0,seq(0,vS_t0()[1],0.005),vS_t0()[1]) 
      cord.y <- c(0,dbeta(seq(0,vS_t0()[1],0.005), shape1 = alpha, shape2 = beta)-1/10^3,0)
      polygon(cord.x, cord.y, col="lightcoral")
      
      last <- tail(vS_t0(), n=1)
      
      cord.x <- c(last,seq(last,1,0.005),1) 
      cord.y <- c(0,dbeta(seq(last,1,0.005), shape = alpha, shape2 = beta)-1/10^3,0)
      polygon(cord.x, cord.y, col="lightcoral")
      
    })
    
    
    
    output$cdf_st <- renderPlot ({
      
      params <- distributionS_t_0()$Beta
      alpha <- params[1,1]
      beta <- params[1,2]
      
      curve(pbeta(x, shape1 = alpha, shape2 = beta), from = -0.5, to = 1.5, 
            main = "Cumulative Distribution Function", 
            xlab = "Treatment Effect", ylab = "Probability Density", lwd = 2)
      
      points(vS_t0(),pbeta(vS_t0(),shape1=alpha,shape2=beta), pch = 16, 
             col = "black", cex = 1.5)
      
    })
    
    lambda_control <- reactive({
      
    })
    
    # Sample lambda parameters for control and treatment groups ----
    
    lambdaSample <- reactive({
      nS<- 10000
      S1_t0 <- SHELF::sampleFit(distributionS_t_0(), n = nS)[, "beta"]
      rho <- SHELF::sampleFit(distributionRho(), n = nS)[, "normal"]
      S2_t0 <- S1_t0 + rho
      check <- S2_t0 > 0 & S2_t0 < 1
      lambda_control <- lambdas(S1_t0[check])
      lambda_treat <- lambdas(S2_t0[check])
      data.frame(lambda_control, lambda_treat)
    })
    
    
    
    output$surv_distributions <- renderPlot ({
      
      # Obtaining a distribution for rho
      p <- string_to_num_vector(input$cumulative_probs)
      v <- string_to_num_vector(input$treat_effect)
      
      params <- SHELF::fitdist(vals = v, probs = p)$Normal
      mu <- params[1,1]
      sig <- params[1,2]
      
      # Obtaining a distribution for s_1(t_0)
      p <- string_to_num_vector(input$cumulative_probs_beta)
      v <- string_to_num_vector(input$surv_times)
      
      params <- SHELF::fitdist(vals = v, probs = p, lower = 0, upper = 1)$Beta
      alpha <- params[1,1]
      beta <- params[1,2]
      
      # Obtaining values for rho and survival rate
      s2.t0 <- 1
      while (s2.t0 >= 1 | s2.t0 <= 0) {
        rho <- rnorm(n = 1, mean = mu, sd = sig)
        s1.t0 <- rbeta(n = 1, shape1 = alpha, shape2 = beta)
        s2.t0 <- rho + s1.t0  
      }
      
      # Plotting the survival distributions
      lambda_control <<- lambdas(s1.t0)
      lambda_treat <<- lambdas(s2.t0)
      
      reactive(lambda_control)
      reactive(lambda_treat)
      
      length <- 1000
      obs <- c(rexp(n = length, rate = lambda_control), rexp(n = length, rate = lambda_treat))
      group <- c(rep("Control", length), rep("Experimental", length))
      
      exponential <- data.frame(obs, group)
      
      ggplot(exponential, aes(x = obs, y = group, group = group)) + 
        geom_density_ridges(aes(fill = group), alpha = 0.7) +
        scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + ggtitle("Density plot of survival times") +
        xlab("Survival Time") + ylab("Treatment Group")
      
    })
    
    output$exp_plot <- renderPlot ({

      curve(dexp(x, rate = lambda_control), from = 0, to = 5, 
            main = sprintf("Exponential Density Fn (Cont rate = %.2f, Treat rate = %.2f)", lambda_control, lambda_treat), 
            xlab = "Survival Time", ylab = "Probability Density", lwd = 2, col = "#00AFBB")
      
      curve(dexp(x, rate = lambda_treat), from = 0, to = 5, 
            lwd = 2, add = TRUE, col = "#E7B800")
    })
    
}

# Run shiny app ---------------------------
shinyApp(ui = ui, server = server,
         options = list(launch.browser = TRUE))
