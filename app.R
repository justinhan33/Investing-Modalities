library(shiny)
library(knitr)
library(ggplot2)
library(tidyverse)
knitr::opts_chunk$set(echo = TRUE, error = TRUE)


## Future Value Function
#' @title Future value function
#' @description Calculates how much money there will be after investing some initial amount for some specified time and rate
#' @param amount initial investment amount (numeric)
#' @param rate annual rate of return (numeric)
#' @param years number of years (numeric)
#' @return future value (what you'll get)
future_value <- function(amount, rate, years){
  if(amount<0 | rate<0 | years<0){
    stop("amount, rate, and years can not be negative")
  }
  fv <- amount*(1+rate)^years
  return(fv)
}

## Future Value of Annuity
#' @title Future value of annuity function
#' @description Calculates how much money there will be after depositing some fixed amount after each year for some specified number of years and rate
#' @param contrib contributed amount or how much you deposit at the end of each year (numeric)
#' @param rate annual rate of return (numeric)
#' @param years number of years (numeric)
#' @return future value of annuity (what you'll get)
annuity <- function(contrib, rate, years){
  if(contrib<0 | rate<0 | years<0){
    stop("contrib, rate, and years can not be negative")
  }
  fva <- contrib*((((1+rate)^(years))-1)/rate)
  return(fva)
}

## Future Value of Growing Annuity
#' @title Future value of growing annuity function
#' @description Calculates how much money there will be after depositing some growing amount after each year for some specified number of years and rate
#' @param contrib contributed amount or how much you deposit at the end of the first year (numeric)
#' @param rate annual rate of return (numeric)
#' @param growth annual growth rate (numeric)
#' @param years number of years (numeric)
#' @return future value of growing annuity (what you'll get)
growing_annuity <- function(contrib, rate, growth, years){
  if(contrib<0 | rate<0 | growth<0 | years<0){
    stop("contrib, rate, growth, and years can not be negative")
  }
  fvga <- contrib*((((1+rate)^(years))-((1+growth)^(years)))/(rate-growth))
  return(fvga)
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Savings and Investing Modalities"),
  fluidRow(
    column(4,
           sliderInput("initial",
                       "Initial Amount",
                       min = 0,
                       max = 100000,
                       step = 500,
                       value = 1000,
                       pre = "$", 
                       sep = ","),
           sliderInput("annual",
                       "Annual Contribution",
                       min = 0,
                       max = 50000,
                       step = 500,
                       value = 2000, 
                       pre = "$", 
                       sep = ",")
    ),
    column(4,
           sliderInput("return",
                       "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 5),
           sliderInput("growth",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 2)
    ),
    column(4,
           sliderInput("years",
                       "Years",
                       min = 0,
                       max = 50,
                       step = 1,
                       value = 20),
           selectInput("facet",
                       "Facet?",
                       choices = c("Yes", "No"),
                       selected = "No")
    )
  ), 
  hr(), 
  titlePanel(h4("Timelines")),
  fluidRow(plotOutput('modalities')),
  titlePanel(h4("Balances")),
  fluidRow(verbatimTextOutput('table')
  )
)

# Define server logic
server <- function(input, output) {
  
  unfacetted <- reactive({ 
    year <- c(0:input$years)
    amount <- input$initial
    contrib <- input$annual
    rate <- (input$return)/100
    growth <- (input$growth)/100
    
    no_contrib <- c()
    fixed_contrib <- c()
    growing_contrib <- c()
    
    # mode 1
    for(i in year){
      no_contrib[i+1] <- future_value(amount, rate, years = i)
    }
    
    # mode 2
    for(i in year){
      fixed_contrib[i+1] <- future_value(amount, rate, years = i) + annuity(contrib, rate, years = i)
    }
    
    # mode 3
    for(i in year){
      growing_contrib[i+1] <- future_value(amount, rate, years = i) + growing_annuity(contrib, rate, growth, years = i)
    }
    
    modalities <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    
    modalities_tidy <- gather(modalities, key="measure", value="value", c("no_contrib", "fixed_contrib", "growing_contrib")) 
    
    modalities_tidy$measure <- factor(modalities_tidy$measure, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))
    
    unfacetted_plot <- ggplot(modalities_tidy, aes(x=year, y=value, color = as.factor(measure))) +
      geom_line() + 
      geom_point() +
      scale_color_manual(name="variable", values=c("no_contrib" ="red", "fixed_contrib"="green", "growing_contrib"="blue")) +
      labs(title = "Three modes of investing")
    
    return(unfacetted_plot)
  })
  
  facetted <- reactive({
    year <- c(0:input$years)
    amount <- input$initial
    contrib <- input$annual
    rate <- (input$return)/100
    growth <- (input$growth)/100
    
    no_contrib <- c()
    fixed_contrib <- c()
    growing_contrib <- c()
    
    # mode 1
    for(i in year){
      no_contrib[i+1] <- future_value(amount, rate, years = i)
    }
    
    # mode 2
    for(i in year){
      fixed_contrib[i+1] <- future_value(amount, rate, years = i) + annuity(contrib, rate, years = i)
    }
    
    # mode 3
    for(i in year){
      growing_contrib[i+1] <- future_value(amount, rate, years = i) + growing_annuity(contrib, rate, growth, years = i)
    }
    
    modalities <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    
    modalities_long <- gather(modalities, key="measure", value="value", c("no_contrib", "fixed_contrib", "growing_contrib")) 
    
    modalities_long$measure <- factor(modalities_long$measure, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))
    
    facetted_plot <- ggplot(modalities_long, aes(x=year, y=value, color = as.factor(measure))) +
      geom_area(color = FALSE, aes(fill = as.factor(measure)), data = modalities_long, alpha = 0.3) +
      geom_line() + 
      geom_point() +
      scale_fill_manual(name="variable", values=c("no_contrib" ="red", "fixed_contrib"="green", "growing_contrib"="blue")) +
      scale_color_manual(name="variable", values=c("no_contrib" ="red", "fixed_contrib"="green", "growing_contrib"="blue")) +
      facet_wrap(~measure) + labs(title = "Three modes of investing")
    
    return(facetted_plot)
  })
  
  balance_table <- reactive({ 
    year <- c(0:input$years)
    amount <- input$initial
    contrib <- input$annual
    rate <- (input$return)/100
    growth <- (input$growth)/100
    
    no_contrib <- c()
    fixed_contrib <- c()
    growing_contrib <- c()
    
    # mode 1
    for(i in year){
      no_contrib[i+1] <- future_value(amount, rate, years = i)
    }
    
    # mode 2
    for(i in year){
      fixed_contrib[i+1] <- future_value(amount, rate, years = i) + annuity(contrib, rate, years = i)
    }
    
    # mode 3
    for(i in year){
      growing_contrib[i+1] <- future_value(amount, rate, years = i) + growing_annuity(contrib, rate, growth, years = i)
    }
    
    modalities <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    
    return(modalities)
  })
  
  output$modalities <- renderPlot({
    if(input$facet == "No"){
      unfacetted()
    }
    else{
      facetted()
    }
  })
  
  output$table <- renderPrint({balance_table()})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
