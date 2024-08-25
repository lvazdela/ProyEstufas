#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(flextable)
load('R/dfestufasTodasVisitas.Rda')
load('R/menunumericas.Rda')


# REG AND CORR FUNCTIONS --------------------------------------------------

f_lm2 <- function(data = dfestufasTodasVisitas,varx = 'usoeco_porc_hsem',
                  vary = 'pm25', color = NULL, faceta = NULL){
  #browser()
  df <- data
  x <- unlist(df[, varx])
  y = unlist(df[, vary])
  
  
  if(is.null(color) & is.null(faceta)){
    reg1 <- summary(lm(  y ~ x))$coefficients
    reg2 = NULL
    reg3 = NULL
    reg4 = NULL
  }
  
  if(!is.null(color) & is.null(faceta)){
    color2 <- unlist(df[, color])
    color2 <- factor(color2)
    levColor <- levels(color2)
    reg1 <- summary(lm(  y ~ x,
                         subset = df[, color] == levColor[1] ))$coefficients
    reg2 <- summary(lm(  y ~ x,
                         subset = df[, color] == levColor[2]))$coefficients
    reg3 = NULL
    reg4 = NULL
  }
  
  if(is.null(color) & !is.null(faceta)){
    faceta2 <- unlist(df[, faceta])
    faceta2 <- factor(faceta2)
    levFaceta <- levels(faceta2)
    reg1 <- summary(lm(  y ~ x,
                         subset = df[, faceta] == levFaceta[1] ))$coefficients
    reg2 <- summary(lm(  y ~ x,
                         subset = df[, faceta] == levFaceta[2] ))$coefficients
    reg3 = NULL
    reg4 = NULL
  }
  
  if(!is.null(color) & !is.null(faceta)){
    color2 <- unlist(df[, color])
    color2 <- factor(color2)
    levColor <- levels(color2)
    
    faceta2 <- unlist(df[, faceta])
    faceta2 <- factor(faceta2)
    levFaceta <- levels(faceta2)
    
    reg1 <- summary(lm(  y ~ x,
                         subset = df[, color] == levColor[1] &
                           df[, faceta] == levFaceta[1]))$coefficients
    reg2 <- summary(lm(  y ~ x,
                         subset = df[, color] == levColor[2] &
                           df[, faceta] == levFaceta[1]))$coefficients
    reg3 <- summary(lm(  y ~ x,
                         subset = df[, color] == levColor[1] &
                           df[, faceta] == levFaceta[2]))$coefficients
    reg4 <- summary(lm(  y ~ x,
                         subset = df[, color] == levColor[2] &
                           df[, faceta] == levFaceta[2]))$coefficients
  }
  
  
  reg1 <- round(reg1, 2)
  
  if(!is.null(reg2)){
    reg2 <- round(reg2, 2)
  }
  if(!is.null(reg3)){
    reg3 <- round(reg3, 2)
  }
  if(!is.null(reg4)){
    reg4 <- round(reg4, 2)
  }
  
  return(list(reg1 =reg1, reg2 = reg2, reg3 = reg3, reg4 = reg4))
}

f_corr <- function(data = dfestufasTodasVisitas,varx = 'usoeco_porc_hsem',
                   vary = 'pm25', color = NULL, faceta = NULL, metodo = 'pearson'){
  #browser()
  df <- data
  x <- unlist(df[, varx])
  y = unlist(df[, vary])
  
  
  if(is.null(color) & is.null(faceta)){
    corr1 <- cor.test(~x+y, method = metodo )
    
    dfcorr <- data.frame(Estimate = round(corr1$estimate,2),
                         ci95 =ifelse(metodo == 'pearson', paste0(round(corr1$conf.int[1],2), '-', round(corr1$conf.int[2], 2)), ''),
                         variables = paste0(varx, ' and ', vary),
                         p = round(corr1$p.value, 2),
                         filters = 'No')
    return(dfcorr)
  }
  
  if(!is.null(color) & is.null(faceta)){
    color2 <- unlist(df[, color])
    color2 <- factor(color2)
    levColor <- levels(color2)
    corr1 <- cor.test(  ~x+y,
                        subset = df[, color] == levColor[1],
                        method = metodo)
    corr2 <- cor.test(  ~x+y,
                        subset = df[, color] == levColor[2],
                        method = metodo)
    
    df1 <- data.frame(Estimate = round(corr1$estimate,2),
                      ci95 = ifelse(metodo == 'pearson', paste0(round(corr1$conf.int[1],2), '-', round(corr1$conf.int[2], 2)), ''),
                      variables = paste0(varx, ' and ', vary),
                      p = round(corr1$p.value, 2),
                      filters = paste0(color, '-',levColor[1]))
    df2 <- data.frame(Estimate = round(corr2$estimate,2),
                      ci95 = ifelse(metodo == 'pearson', paste0(round(corr2$conf.int[1],2), '-', round(corr2$conf.int[2], 2)), ''),
                      variables = paste0(varx, ' and ', vary),
                      p = round(corr2$p.value, 2),
                      filters = paste0(color, '-',levColor[2]))
    dfcorr <- bind_rows(df1,df2)
    return(dfcorr)
  }
  
  if(is.null(color) & !is.null(faceta)){
    faceta2 <- unlist(df[, faceta])
    faceta2 <- factor(faceta2)
    levFaceta <- levels(faceta2)
    corr1 <- cor.test(  ~x+y,
                        subset = df[, faceta] == levFaceta[1],
                        method = metodo)
    corr2 <- cor.test(  ~x+y,
                        subset = df[, faceta] == levFaceta[2],
                        method = metodo)
    
    df1 <- data.frame(Estimate = round(corr1$estimate,2),
                      ci95 = ifelse(metodo == 'pearson', paste0(round(corr1$conf.int[1],2), '-', round(corr1$conf.int[2], 2)), ''),
                      variables = paste0(varx, ' and ', vary),
                      p = round(corr1$p.value, 2),
                      filters = paste0(faceta, '-',levFaceta[1]))
    df2 <- data.frame(Estimate = round(corr2$estimate,2),
                      ci95 = ifelse(metodo == 'pearson', paste0(round(corr2$conf.int[1],2), '-', round(corr2$conf.int[2], 2)), ''),
                      variables = paste0(varx, ' and ', vary),
                      p = round(corr2$p.value, 2),
                      filters = paste0(faceta, '-',levFaceta[2]))
    dfcorr <- bind_rows(df1, df2)
    
    return(dfcorr) 
    
    
  }
  
  if(!is.null(color) & !is.null(faceta)){
    color2 <- unlist(df[, color])
    color2 <- factor(color2)
    levColor <- levels(color2)
    
    faceta2 <- unlist(df[, faceta])
    faceta2 <- factor(faceta2)
    levFaceta <- levels(faceta2)
    
    corr1 <- cor.test(  ~x+y,
                        subset = df[, color] == levColor[1] &
                          df[, faceta] == levFaceta[1],
                        method = metodo)
    corr2 <- cor.test(  ~x+y,
                        subset = df[, color] == levColor[2] &
                          df[, faceta] == levFaceta[1],
                        method = metodo)
    corr3 <- cor.test(  ~x+y,
                        subset = df[, color] == levColor[1] &
                          df[, faceta] == levFaceta[2],
                        method = metodo)
    corr4 <- cor.test(  ~x+y,
                        subset = df[, color] == levColor[2] &
                          df[, faceta] == levFaceta[2],
                        method = metodo)
    
    df1 <- data.frame(Estimate = round(corr1$estimate,2),
                      ci95 = ifelse(metodo == 'pearson', paste0(round(corr1$conf.int[1],2), '-', round(corr1$conf.int[2], 2)), ''),
                      variables = paste0(varx, ' and ', vary),
                      p = round(corr1$p.value, 2),
                      filters = paste0(paste0(color,'-',levColor[1]),' ', paste0(faceta,'-',levFaceta[1])))
    
    df2 <- data.frame(Estimate = round(corr2$estimate,2),
                      ci95 = ifelse(metodo == 'pearson', paste0(round(corr2$conf.int[1],2), '-', round(corr2$conf.int[2], 2)), ''),
                      variables = paste0(varx, ' and ', vary),
                      p = round(corr2$p.value, 2),
                      filters = paste0(paste0(color,'-',levColor[2]), ' ', paste0(faceta,'-',levFaceta[1])))
    df3 <- data.frame(Estimate = round(corr3$estimate,2),
                      ci95 = ifelse(metodo == 'pearson', paste0(round(corr3$conf.int[1],2), '-', round(corr3$conf.int[2], 2)), ''),
                      variables = paste0(varx, ' and ', vary),
                      p = round(corr3$p.value, 2),
                      filters = paste0(paste0(color,'-',levColor[1]),' ', paste0(faceta,'-',levFaceta[2])))
    df4 <- data.frame(Estimate = round(corr4$estimate,2),
                      ci95 = ifelse(metodo == 'pearson', paste0(round(corr4$conf.int[1],2), '-', round(corr4$conf.int[2], 2)), ''),
                      variables = paste0(varx, ' and ', vary),
                      p = round(corr4$p.value, 2),
                      filters = paste0(paste0(color,'-',levColor[2]),' ', paste0(faceta,'-',levFaceta[2])))
    dfcorr <- bind_rows(df1, df2,df3,df4)
    return(dfcorr)
    
  }
}

# UI  ---------------------------------------------------------------------


ui <- fluidPage(

    # Application title
    titlePanel("Linear regressions for the Stove Project"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          wellPanel(
            h4('Filters:'),
            radioButtons(inputId = 'ecostove',
                         label = 'Only Ecostove group',
                         choices = c('Yes' = '1',
                                     'No'  = 'all')),
            conditionalPanel(
              condition = "input.ecostove == 'all'",
              selectInput(inputId = "color",
                          label = "Group by:",
                          choices = c('Stove in use' = 'estufaenuso',
                                      'Group' = 'grupo',
                                      'Visit' = 'visit',
                                      'No grouping' = 'nada')),
              
              selectInput(inputId = "facet",
                          label = "Facet by:",
                          choices = c('Stove in use' = 'estufaenuso',
                                      'Group' = 'grupo',
                                      'Visit' = 'visit',
                                      'No faceting' = 'nada'),
                          selected = 'visit'),
              
            ),
            conditionalPanel(
              condition = "input.ecostove == '1'",
              selectInput(inputId = "color",
                          label = "Group by:",
                          choices = c('Stove in use' = 'estufaenuso',
                                      'Visit' = 'visit',
                                      'No grouping' = 'nada')),
              
              selectInput(inputId = "facet",
                          label = "Facet by:",
                          choices = c('Stove in use' = 'estufaenuso',
                                      'Visit' = 'visit',
                                      'No faceting' = 'nada'),
                          selected = 'visit')
            )
                        
 
          ),
          wellPanel(
            h4('Variables:'),
            selectInput(inputId = 'varx',
                        label = 'X variable:',
                        choices = menuNumericas,
                        selected = 'usoeco_porc_hsem'),
            selectInput(inputId = 'vary',
                        label = 'Y variable:',
                        choices = menuNumericas,
                        selected = 'pm25')
          ),
          wellPanel(
            h4('Correlation Method'),
            radioButtons(inputId = 'corrmet',
                         label = NULL,
                         choices = c('Kendall' = 'kendall',
                                     'Pearson' = 'pearson',
                                     'Spearman' = 'spearman'))
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            plotOutput("regPlot")
          ),
          fluidRow(
            column(6,
                   h3('Regression results'),
                   uiOutput(outputId = 'regresion')),
            column(6,
                   h3('Correlation results'),
                   uiOutput(outputId = 'correlation')) 
          )
        )
    )
)


# SERVER FUNCTION ---------------------------------------------------------


server <- function(input, output) {

  dfreactive <- reactive({
    df <- dfestufasTodasVisitas
    if(input$ecostove == '1'){
      df <- filter(df, grupo == 'Ecostove')
    }
    df <- df |>
      filter(!is.na(estufaenuso)) |>
      mutate(estufaenuso = factor(estufaenuso, labels = c('No', 'Yes')),
             visit = factor(visit, labels = c('First visit', 'Second visit')))
   df   
  })
  
  ggreactive <- reactive({

    if(input$color != 'nada' & input$facet != 'nada'){
      dfreactive() %>%      
        ggplot(aes_string(x = input$varx, y = input$vary, color = input$color)) +
        theme(text = element_text(size = 20)) +
        geom_point() +
        geom_smooth( se = FALSE, method = 'lm', lty = 'dotdash') +
        #scale_color_manual(values = c('red', 'blue')) +
        facet_wrap(~ get(input$facet), ncol = 2) #, scales = 'free'
    } else {
      if(input$color != 'nada' & input$facet == 'nada'){
        dfreactive() |>
          ggplot(aes_string(x = input$varx, y = input$vary, color = input$color)) +
          theme(text = element_text(size = 20)) +
          geom_point() +
          geom_smooth( se = FALSE, method = 'lm', lty = 'dotdash')
        #scale_color_manual(values = c('red', 'blue'))
      } else{
          if(input$color == 'nada' & input$facet != 'nada'){
            dfreactive() |>
              ggplot(aes_string(x = input$varx, y = input$vary)) +
              theme(text = element_text(size = 20)) +
              geom_point() +
              geom_smooth( se = FALSE, method = 'lm', lty = 'dotdash') +
              facet_wrap(~ get(input$facet), ncol = 2) #, scales = 'free'
          } else{
            if(input$color == 'nada' & input$facet == 'nada'){
              dfreactive() |>
                ggplot(aes_string(x = input$varx, y = input$vary)) +
                theme(text = element_text(size = 20)) +
                geom_point() +
                geom_smooth( se = FALSE, method = 'lm', lty = 'dotdash')
                }
            } 
          }
      }
  })
  
  output$regPlot <- renderPlot({
    ggreactive()
    })
  
  reac_pasa <- reactive({
    #browser()
    dfvisita2 <- dfreactive()[which(dfreactive()$visit == 'Second visit'), ]
    Y <- unlist(dfvisita2[, input$vary])
    novisitaY <- sum(is.na(Y)) == length(Y)
    X <- unlist(dfvisita2[, input$varx])
    novisitaX <- sum(is.na(X)) == length(X)
    if(novisitaX | novisitaY){
      showModal(modalDialog(
        title = 'ATENCIÓN',
        'Variable was not measured in second visit, do not group or facet on visit'
      )) 
    }
  })

  dfregreactive <- reactive({
    #browser()
    if(input$color != 'nada'){
      col <- unlist(dfreactive()[, input$color])
      levcol <- levels(col)}
    
    if(input$facet != 'nada'){
      fac <- unlist(dfreactive()[, input$facet])
      levfac <- levels(fac)}
    
    
    if(input$color != 'nada' & input$facet != 'nada'){
      resreg <- f_lm2(data = dfreactive(), varx = input$varx, vary = input$vary,
                      color = input$color, faceta = input$facet)
      resreg1 <- as.data.frame(resreg$reg1)
      resreg2 <- as.data.frame(resreg$reg2)
      resreg3 <- as.data.frame(resreg$reg3)
      resreg4 <- as.data.frame(resreg$reg4)
      df <- bind_rows(resreg1, resreg2, resreg3, resreg4)
      
      df <- df |>
        mutate(variable = rep(c('intercept', 'x'), 4),
               filter = c(paste0(paste0(input$color,'-',levcol[1]), paste0('-',levfac[1])),'',
                          paste0(paste0(input$color,'-',levcol[2]), paste0('-',levfac[1])),'',
                          paste0(paste0(input$color,'-',levcol[1]), paste0('-',levfac[2])),'',
                          paste0(paste0(input$color,'-',levcol[2]), paste0('-',levfac[2])),'')) |>
        select(5, 1:4,6)

    } else {
      if(input$color != 'nada' & input$facet == 'nada'){
        resreg <- f_lm2(data = dfreactive(), varx = input$varx, vary = input$vary,
                        color = input$color)
        resreg1 <- as.data.frame(resreg$reg1)
        resreg2 <- as.data.frame(resreg$reg2)
        df <- bind_rows(resreg1, resreg2)
        
        df <- df |>
          mutate(variable = rep(c('intercept', 'x'), 2),
                 filter = c(paste0(paste0(input$color,'-',levcol[1]),''),'',
                            paste0(paste0(input$color,'-',levcol[2]), ''),'')) |>
          select(5, 1:4,6)
        
      } else{
        if(input$color == 'nada' & input$facet != 'nada'){
          resreg <- f_lm2(data = dfreactive(), varx = input$varx, vary = input$vary,
                          faceta = input$facet)
          resreg1 <- as.data.frame(resreg$reg1)
          resreg2 <- as.data.frame(resreg$reg2)
          df <- bind_rows(resreg1, resreg2)
          
          df <- df |>
          mutate(variable = rep(c('intercept', 'x'), 2),
                 filter = c(levfac[1],'',
                            levfac[2],'')) |>
            select(5, 1:4,6)
          
        } else{
          if(input$color == 'nada' & input$facet == 'nada'){
            resreg <- f_lm2(data = dfreactive(), varx = input$varx, vary = input$vary)
            resreg1 <- as.data.frame(resreg$reg1)

            df <- resreg1
            df <- df |>
              mutate(variable = c('intercept', 'x')) |>
              select(5, 1:4)
            
          }
        } 
      }
    }
  })

  output$regresion <- renderUI({
      #reac_pasa()
      dfflex <- dfregreactive() |>
        rename( p = `Pr(>|t|)`) |>
        flextable() |>
        color(j = 5, i = ~ variable == 'x' & p < 0.05, color = 'red')
      htmltools_value(dfflex)
  })

  
  dfcorrreactive <- reactive({
   # browser()
    
    if(input$color != 'nada' & input$facet != 'nada'){
      dfcorr <- f_corr(dfreactive(), input$varx, input$vary, color = input$color, faceta = input$facet, metodo = input$corrmet )
      
    } else {
      if(input$color != 'nada' & input$facet == 'nada'){
        dfcorr <- f_corr(dfreactive(), input$varx, input$vary, color = input$color, metodo = input$corrmet )
        
      } else{
        if(input$color == 'nada' & input$facet != 'nada'){
          dfcorr <- f_corr(dfreactive(), input$varx, input$vary, faceta = input$facet, metodo = input$corrmet )
          
        } else{
          if(input$color == 'nada' & input$facet == 'nada'){
            dfcorr <- f_corr(dfreactive(), input$varx, input$vary, metodo = input$corrmet )
          }
        } 
      }
    }
  }) 

  output$correlation <- renderUI({
      #reac_pasa()
      dfflex <- dfcorrreactive() |>
        flextable() |>
        color(j = 4, i = ~ p < 0.05, color = 'red')
      
      htmltools_value(dfflex)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
