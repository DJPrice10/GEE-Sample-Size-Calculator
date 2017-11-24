###################################################################
# R code to accompany "Accounting for Twin Births in Sample Size 
#   Calculations for Randomised Trials", by L Yelland et al.
# 
# This file contains the Shiny app for the sample size calculator.
# 
# Author: David J Price (david.j.price@alumni.adelaide.edu.au)
# Updated: 2017-11-24 
###################################################################


library(shiny)

# USER INTERFACE
ui <- fluidPage(
  navbarPage("GEE Sample Size Calculator",
             tabPanel("Information",
                      titlePanel("Information"),
                      br(),
                      "This calculator can be used to calculate the target sample size for two-arm, parallel group, 
                      randomised controlled trials including both singletons and twins or twins only when the outcome of 
                      interest is a continuous or binary outcome that is measured on the infant. The sample size is calculated 
                      assuming the analysis will be performed using generalised estimating equations (GEEs) with an independence 
                      or exchangeable working correlation structure, and that twins will be randomised to the same treatment group 
                      (cluster randomisation), independently of each other (individual randomisation), or to opposite treatment groups 
                      (opposite randomisation). No allowance is made for loss to follow up. The calculator should be used with caution 
                      for the exchangeable working correlation structure with individual or opposite randomisation.",
                      br(),
                      br(),
                      br(),
                      "For full details, including the assumptions and limitations of the calculations, refer to:",
                      br(),
                      "Yelland LN, Sullivan TR, Price DJ, Lee KJ.",a("Sample size calculations for randomised trials including both 
                      independent and paired data.",href="https://www.ncbi.nlm.nih.gov/pubmed/28074483"),
                      br(),
                      "Statistics in Medicine, 2017; 36:1227-39. DOI: 10.1002/sim.7201",
                      br(),
                      br(),
                      "Created by: David Price",
                      br(),
                      "Last updated: 24th November, 2017",
                      br(),
                      br(),
                      "Please email any questions or comments to:",
                      br(),
                      "Lisa Yelland (",a("lisa.yelland@adelaide.edu.au", href="mailto:lisa.yelland@adelaide.edu.au"),"),",
                      br(),
                      "David J. Price (",a("david.j.price@alumni.adelaide.edu.au", href="mailto:david.j.price@alumni.adelaide.edu.au"),").",
                      br(),
                      br(),
                      "If the calculator is not working on the Shinyapps website, R code for local implementation of the calculator is available on ",a("this GitHub repository.",href="https://github.com/DJPrice10/GEE-Sample-Size-Calculator")
             ),
             
             tabPanel("Calculator",
                      
                      # Give the page a title
                      titlePanel("GEE Sample Size Calculator"),
                      helpText("Sample size calculator for randomised trials including twins."),
                      # *Input() functions,
                      # *Output() functions
                      sidebarPanel(h3("User Inputs"),
                                   helpText("If output is NaN, check that all inputs are feasible values."),
                                   br(),
                                   radioButtons(inputId = "outcome_type", label = "Type of Outcome", choices = c("Continuous","Binary"), inline = TRUE),
                                   helpText("Select outcome of interest."),
                                   numericInput(inputId = "Indep_N_per_group", label = "Number of infants per group", value = 42, min = 1, max = Inf, width=validateCssUnit("50%")),
                                   uiOutput("Indep_N_per_group"),
                                   helpText("Enter the number of infants required per group for your trial obtained using any 
                                            standard sample size calculator that assumes the outcomes of all infants are independent. 
                                            If you have calculated the total sample size (N), the number of infants per group is N/2."),
                                   numericInput(inputId = "pc_mothers_w_twins", label = "% Mothers with Twins", min = 0, max = 100, value = 2, step=0.1, width=validateCssUnit("50%")),
                                   numericInput(inputId = "pc_inf_twins", label = "% Infants that are Twins", min = 0, max = 100, value = 100*((2*2)/(2+100)), step=0.1, width=validateCssUnit("50%")),
                                   uiOutput("pc_inf_twins"),
                                   helpText("Enter the percentage of mothers you expect will have a twin birth OR the percentage of infants you expect will be a twin in your trial. 
                                            The other percentage will be calculated automatically."),
                                   # sliderInput(inputId = "ICC", label = "Intraclass Correlation Coefficient (ICC)", value = 0.5, min = 0, max = 1)
                                   numericInput(inputId = "ICC", label = "Intracluster Correlation Coefficient (ICC)", value = 0.5, min=-1, max=1, step = 0.1, width=validateCssUnit("50%")),
                                   helpText("Enter your best estimate of the correlation between outcomes of infants from the same birth for your trial."),
                                   radioButtons(inputId = "corr_struct", label = "GEE Working Correlation Structure", choices = list("Independence", "Exchangeable"), inline = TRUE),
                                   helpText("Specify whether data from your trial will be analysed assuming an independence (recommended) or exchangeable working correlation structure."),
                                   radioButtons(inputId = "rand_method", label = "Randomisation", choices = list("Cluster", "Individual", "Opposite"), inline = TRUE),
                                   helpText("Specify whether twins from the same birth will be randomised to the same treatment group (cluster), independently of each other 
                                            (individual) or to opposite treatment groups (opposite) in your trial."),
                                   conditionalPanel(condition = "input.outcome_type=='Binary'",
                                                    radioButtons(inputId = "link_fn", label = "Link Function", choices = list("Logistic","Log Binomial"), inline = TRUE),
                                                    helpText("Specify whether data from your trial will be analysed using a logistic model (to estimate the odds ratio) or a log binomial model (to estimate the relative risk)."),
                                                    numericInput(inputId = "trt_grp_prev",label = "Intervention group outcome prevalence (%)", value = 5, min = 0, max = 100, step = 1, width=validateCssUnit("50%")),
                                                    helpText("Enter the expected prevalence of the outcome in the intervention group for your trial as a percentage."),
                                                    numericInput(inputId = "cont_grp_prev",label = "Control group outcome prevalence (%)", value = 10, min = 0, max = 100, step = 1, width=validateCssUnit("50%")),
                                                    helpText("Enter the expected prevalence of the outcome in the control group for your trial as a percentage.")
                                   ),
                                   width=6
                                   ),
                      mainPanel(br(),
                                # h3("Results"),
                                br(),
                                dataTableOutput("results"), width=6
                      )
                      )
             )
)




# SERVER
server <- function(input, output, session) {
  
  # If fractional N, round up. 
  observeEvent(input$Indep_N_per_group,{
    updateNumericInput(session = session, inputId = "Indep_N_per_group", value = abs(ceiling(input$Indep_N_per_group)))
  })
  
  # Update percentage of twins and percentage of mothers with twins based on each other
  observeEvent(input$pc_mothers_w_twins,  {
    # updateSliderInput(session = session, inputId = "pc_inf_twins", value = 100*((input$pc_inf_twins*2)/(input$pc_inf_twins+100)))
    updateNumericInput(session = session, inputId = "pc_inf_twins", value = 100*((input$pc_mothers_w_twins*2)/(input$pc_mothers_w_twins+100)))
  })
  
  # when pc_inf_twins change, update pc_mothers_w_twins
  observeEvent(input$pc_inf_twins,  {
    # updateSliderInput(session = session, inputId = "pc_mothers_w_twins", value = 100*((input$pc_mothers_w_twins/2)/(100-(input$pc_mothers_w_twins/2))))
    updateNumericInput(session = session, inputId = "pc_mothers_w_twins", value = 100*((input$pc_inf_twins/2)/(100-(input$pc_inf_twins/2))))
  })
  
  
  
  # Evaluate Design Effect for selected options, and output table with values
  results.data.table <- reactive({
    
    ifelse(input$outcome_type=="Continuous", {
      indep.de.values <- switch(as.character(input$rand_method),
                                Cluster = 1+(input$ICC*input$pc_inf_twins/100),
                                Individual = 1,
                                Opposite = 1-(input$ICC*input$pc_inf_twins/100)
      )
      exch.de.values <- switch(as.character(input$rand_method),
                               Cluster = (1-input$ICC^2)/(1-input$ICC^2*(100-input$pc_inf_twins)/100 - input$ICC*(input$pc_inf_twins/100)),
                               Individual = (1-input$ICC^2)/(1-(input$ICC^2*(100-input$pc_inf_twins)/100)),
                               Opposite = (1-input$ICC^2)/(1-(input$ICC^2*(100- input$pc_inf_twins)/100)+(input$ICC*input$pc_inf_twins/100))
      )
    }, 
    { ifelse(input$link_fn=="Logistic",
             {pi_term_1 <- (sqrt((input$trt_grp_prev/100)*(input$cont_grp_prev/100)*(1-(input$trt_grp_prev/100))*(1-(input$cont_grp_prev/100))))/(((input$trt_grp_prev/100)*(1-(input$trt_grp_prev/100)))+((input$cont_grp_prev/100)*(1-(input$cont_grp_prev/100))))
             
             indep.de.values <- switch(as.character(input$rand_method),   # Logistic values
                                       Cluster = 1+(input$ICC*input$pc_inf_twins/100),
                                       Individual = 1+(input$ICC*input$pc_inf_twins/100)*(0.5-pi_term_1),
                                       Opposite = 1-(input$ICC*2*pi_term_1*input$pc_inf_twins/100)
             )
             exch.de.values <- switch(as.character(input$rand_method),
                                      Cluster = (1+input$ICC)/(1+(input$ICC*(100-input$pc_inf_twins)/100)),
                                      Individual = ((1-input$ICC^2)/(1-(input$ICC^2*(100-input$pc_inf_twins)/100)))*((1-(input$ICC^2*(100-input$pc_inf_twins)/100)-((input$ICC*input$pc_inf_twins/100)*(0.5+pi_term_1)))/(1-(input$ICC^2*(100-input$pc_inf_twins)/100)-(input$ICC*input$pc_inf_twins/100))),
                                      Opposite = ((1-input$ICC)/(1-(input$ICC*(100-input$pc_inf_twins)/100)))*((1-(input$ICC^2*(100-input$pc_inf_twins)/100)-(input$ICC*2*pi_term_1*input$pc_inf_twins/100))/(1-(input$ICC^2*(100-input$pc_inf_twins)/100)-(input$ICC*input$pc_inf_twins/100)))
             )},
             {pi_term_2 <- (sqrt((input$trt_grp_prev/100)*(input$cont_grp_prev/100)*(1-(input$trt_grp_prev/100))*(1-(input$cont_grp_prev/100))))/(((input$trt_grp_prev/100)*(1-(input$cont_grp_prev/100)))+((input$cont_grp_prev/100)*(1-(input$trt_grp_prev/100))))
             indep.de.values <- switch(as.character(input$rand_method),  # Log Binomial values
                                       Cluster = 1+(input$ICC*input$pc_inf_twins/100),
                                       Individual = 1+((input$ICC*input$pc_inf_twins/100)*(0.5-pi_term_2)),
                                       Opposite = 1-(input$ICC*2*pi_term_2*input$pc_inf_twins/100)
             )
             exch.de.values <- switch(as.character(input$rand_method),
                                      Cluster = (1+input$ICC)/(1+(input$ICC*(100-input$pc_inf_twins)/100)),
                                      Individual = ((1-input$ICC^2)/(1-(input$ICC^2*(100-input$pc_inf_twins)/100)))*((1-(input$ICC^2*(100-input$pc_inf_twins)/100)-((input$ICC*input$pc_inf_twins/100)*(0.5+pi_term_2)))/(1-(input$ICC^2*(100-input$pc_inf_twins)/100)-(input$ICC*input$pc_inf_twins/100))),
                                      Opposite = ((1-input$ICC)/(1-(input$ICC*(100-input$pc_inf_twins)/100)))*((1-(input$ICC^2*(100-input$pc_inf_twins)/100)-(input$ICC*2*pi_term_2*input$pc_inf_twins/100))/(1-(input$ICC^2*(100-input$pc_inf_twins)/100)-(input$ICC*input$pc_inf_twins/100)))
             )}
    )
    })
    
    # Check that inputs are reasonable values
    if((input$Indep_N_per_group<1) | (input$pc_inf_twins<0 | input$pc_inf_twins>100) | (input$ICC< -1 | input$ICC >1) | (input$trt_grp_prev<0 | input$trt_grp_prev>100) | (input$cont_grp_prev<0 | input$cont_grp_prev>100) ){
      indep.de.values <- NaN
      exch.de.values <- NaN
    }
    
    
    if(input$rand_method=="Cluster"){
      
      # Row names for output
      row.names <- c("Design Effect", "Number Infants per Group", "Number Mothers per Group", "Number Infants Total", "Number Mothers Total")
      
    switch(as.character(input$corr_struct),
           Independence = data.frame("Result"=row.names, "Value"=c(sprintf('%.4f',indep.de.values), 
                                                                         ceiling(indep.de.values*input$Indep_N_per_group),
                                                                         ceiling(ceiling(indep.de.values*input$Indep_N_per_group)/(1 + (input$pc_mothers_w_twins/100))),
                                                                         2*ceiling(indep.de.values*input$Indep_N_per_group),
                                                                         2*ceiling(ceiling(indep.de.values*input$Indep_N_per_group)/(1 + (input$pc_mothers_w_twins/100))))),
           Exchangeable = data.frame("Result"=row.names, "Value"=c(sprintf('%.4f',exch.de.values), 
                                                                         ceiling(exch.de.values*input$Indep_N_per_group),
                                                                         ceiling(ceiling(exch.de.values*input$Indep_N_per_group)/(1 + (input$pc_mothers_w_twins/100))),
                                                                         2*ceiling(exch.de.values*input$Indep_N_per_group),
                                                                         2*ceiling(ceiling(exch.de.values*input$Indep_N_per_group)/(1 + (input$pc_mothers_w_twins/100)))))
    )
    } else{
      # Row names for output
      row.names <- c("Design Effect", "Number Infants per Group", "Number Infants Total", "Number Mothers Total")
      
      switch(as.character(input$corr_struct),
             Independence = data.frame("Result"=row.names, "Value"=c(sprintf('%.4f',indep.de.values), 
                                                                           ceiling(indep.de.values*input$Indep_N_per_group),
                                                                           2*ceiling(indep.de.values*input$Indep_N_per_group),
                                                                           ceiling(2*ceiling(indep.de.values*input$Indep_N_per_group)/(1 + (input$pc_mothers_w_twins/100))))),
             Exchangeable = data.frame("Result"=row.names, "Value"=c(sprintf('%.4f',exch.de.values), 
                                                                           ceiling(exch.de.values*input$Indep_N_per_group),
                                                                           2*ceiling(exch.de.values*input$Indep_N_per_group),
                                                                           ceiling(2*ceiling(exch.de.values*input$Indep_N_per_group)/(1 + (input$pc_mothers_w_twins/100)))))
      )
    }
    
  })
  # Output data table results without any data table options (page no., row number, etc..)
  output$results <- renderDataTable({results.data.table()}, options = list(lengthChange= F, paging = F, searching = F, ordering= F, info=F))
  
  
}

shinyApp(ui = ui, server = server)
