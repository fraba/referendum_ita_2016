
setwd('~/public_git/referendum_ita_2016')
load('data/polls.RData')
load('data/expected_results.Rdata')

library(shiny)
library(ggplot2)

script1 <- "$('tbody tr td:nth-child(6)').each(function() {

              var cellValue = $(this).text();

              if (cellValue > 0.1) {
                $(this).css('background-color', '#ef8a62');
              }
              else if (cellValue >= -0.1 && cellValue <= 0.1) {
                $(this).css('background-color', '#f7f7f7');
              }
              else if (cellValue < -0.1) {
                $(this).css('background-color', '#67a9cf');
              }
            })"

script2 <- "$('tbody tr td:nth-child(7)').each(function() {

            var cellValue = $(this).text();
            
            if (cellValue > 0.05) {
            $(this).css('background-color', '#ef8a62');
            }
            else if (cellValue >= -0.05 && cellValue <= 0.05) {
            $(this).css('background-color', '#f7f7f7');
            }
            else if (cellValue < -0.05) {
            $(this).css('background-color', '#67a9cf');
            }
            })"

script3 <- "$('tbody tr td:nth-child(4)').each(function() {

            var cellValue = $(this).text();

            if (cellValue > 50) {
            $(this).css('background-color', '#ef8a62');
            }
            else if (cellValue == 50) {
            $(this).css('background-color', '#f7f7f7');
            }
            else if (cellValue < 50) {
            $(this).css('background-color', '#67a9cf');
            }
            })"

# edit table
polls$error <- NULL
retMarginError <- function(a, n) {
  if (is.na(a) | is.na(n)) return(NA)
  (1.96*sqrt(a*(1-a)/n))*100
}
polls$yes_wtout_undecided_error <- mapply(retMarginError, polls$yes_wtout_undecided/100, polls$sample)
polls$no_wtout_undecided_error <- mapply(retMarginError, polls$no_wtout_undecided/100, polls$sample)
polls$undecided_error <- mapply(retMarginError, polls$undecided/100, polls$sample)
polls$affluence_error <- mapply(retMarginError, polls$affluence/100, polls$sample)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  tags$head(tags$style("
    #expected_yes{color: red; font-size: 25px; font-weight: bold; text-align: center;}
    #updated{font-size: 80%; font-style: italic; text-align: center;}
    table{margin-left:auto; margin-right:auto;}
    "
  ),
  tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });'))
  ),
  
  # Application title
  tags$h2("Referendum costituzionale 2016", style = 'text-align: center;'),
  
  sidebarLayout(
    sidebarPanel(
      textOutput('expected_yes'),
      tags$hr(),
      textOutput('updated'),
      tags$hr(),
      sliderInput("date_range1", 
                  NA, as.Date("2016-01-31"), as.Date('2016-12-04'),
                  value = c(as.Date("2016-01-31"), as.Date('2016-12-04')), 
                  step = 1, sep = ""),
      tags$hr(),
      tags$p("Each dot represents a poll. Polls are pooled by weightning their sample size 
             (larger sample, higher precision) and their distance in time (closer to present, higher precision).",
             style = "font-size: 85%;"),
      tags$div(
        HTML(paste0("Data aggregated by <a href='https://it.wikipedia.org/wiki/Sondaggi_sul_referendum_costituzionale_del_2016_in_Italia' target='_blank'>Wikipedia</a>", 
                    " as they are published by the <a href='http://www.sondaggipoliticoelettorali.it/' target = '_blank'>Italian Government</a>",
                    " (usually after a 7 days embargo).")),
        style = "font-size: 75%; font-style: italic;"),
      width = 3),
    mainPanel(
      plotOutput("ts_pool_polls"), width = 9
    )
  ),
  tags$hr(),
  fluidRow(column(12, 
                  tags$p('Trends by pollster (for polls conducted in the last 30 days)', 
                         style = 'font-weight: bold; text-align: center;'),
                  tableOutput("trend_polster"),
                  tags$p('sd: standard deviation; 
                         trend 1: weighted average (closer to present, higher weight);
                         trend 2: coefficient of linear regression.', style = 'font-size: 70%;'))),
  tags$hr(),
  fluidRow(tags$p("All polls", style = 'font-weight: bold; text-align: center;'),
           column(4, 
                  selectInput("var", 
                              "Variable", 
                              choices = c("Vote YES" = 'yes_wtout_undecided', 
                                          "Vote NO" = 'no_wtout_undecided', 
                                          "Undecided" = 'undecided',
                                          "Affluence" = 'affluence'),
                              selected = 'Yes'),
                  sliderInput("date_range2", 
                              "Date", min(polls$date), max(polls$date),
                              value = range(polls$date), step = 1, sep = ""),
                  selectInput("pollster",
                              "Pollster", 
                              choices = c("All", unique(polls$pollster)),
                              multiple = FALSE,
                              selected = 'All'),
                  tags$p("Each dot represents a poll. Whiskers represent the margin of error based on the sample size of each poll. 
                         The blue line represents a local regression and might be interpreted as an unweighted trend.", 
                         style = "font-size: 85%;")),
           column(8, plotOutput("ts_polls"))),
  tags$hr(),
  fluidRow(tags$div(id='footer',
                    style='font-size: 70%; margin: auto; width: 90%; padding: 10px 10px 10px 10px; text-align: center;',
                    HTML(paste0(
                      "Design: Francesco Bailo (<a href='https://twitter.com/FrBailo' target='_blank'>@FrBailo</a>) ",
                      "| Code: <a href='https://github.com/fraba/referendum_ita_2016' target='_blank'>GitHub</a> ",
                      "| Powered by: <a href='http://www.R-project.org/' target='_blank'>R</a> and <a href='http://shiny.rstudio.com/' target='_blank'>Shiny</a> ",
                      "| R packages: <a href='http://ggplot2.org' target='_blank'>ggplot2</a> ",
                      "| Hosted by: <a href='https://nectar.org.au/research-cloud/'>Nectar Cloud</a> ",
                      "| Version: 0.1 "))))
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  session$onFlushed(function() {
    session$sendCustomMessage(type='jsCode', list(value = script1))
    session$sendCustomMessage(type='jsCode', list(value = script2))
    session$sendCustomMessage(type='jsCode', list(value = script3))
  })
  
  output$ts_pool_polls <- renderPlot({
    
    ggplot(polls, aes(x=date, y=yes_wtout_undecided)) +
      geom_line(data=expected_results, aes(x=date, y=expected_result)) +
      geom_point(alpha = 0.3) +
      geom_hline(yintercept = 50, linetype = 'dashed') +
      geom_vline(xintercept = as.numeric(as.Date('2016-12-04'))) +
      scale_y_continuous(breaks = seq(0, 100, 5)) +
      scale_x_date(date_labels = "%b %Y") +
      coord_cartesian(xlim = c(input$date_range1[1], input$date_range1[2])) + 
      labs(x=NULL, y="%") + 
      theme_bw() +
      theme(legend.position="bottom")
    
  })
  
  output$trend_polster <- renderTable({
    
    trend1 <- function(x) {
      i = 0.05
      d <- diff(x, na.rm = T)
      if (length(d) == 0) {
        return(NA)
      } else {
        return(weighted.mean(d, 1:length(d)))
      }
    }
    
    trend2 <- function(date, value) {
      df <- data.frame(x = as.numeric(date), y = value)
      mod <- lm(value ~ date, data = df)
      mod$coef[2]
    }
    
    library(reshape2)
    library(dplyr)
    trend_polster_df <- 
      melt(polls[,c('date','pollster','yes_wtout_undecided')], 
           id.vars = c('date','pollster'))
    trend_polster_df <- 
      subset(trend_polster_df, date >= Sys.Date() - 30) %>%
      dplyr::group_by(pollster) %>%
      dplyr::summarize(polls = n(),
                       mean = round(mean(value, na.rm = T), 2),
                       sd = round(sd(value, na.rm = T), 2),
                       `trend 1` = round(trend1(value), 2),
                       `trend 2` = round(trend2(date, value), 2))
    trend_polster_df <- trend_polster_df[trend_polster_df$polls > 1, ]
  })
  
  output$updated <- renderText({paste0('Last poll: ', format(max(polls$date), "%a, %e %B %Y"))})
  
  output$expected_yes <- 
    renderText({
      paste0('Yes: ',
             round(expected_results[which.max(expected_results$date),]$expected_result,2),
             "%")})
  
  react_polls <- reactive({
    selected_var <- input$var
    df <- polls[,c('date', selected_var, paste0(selected_var, "_error"), 'pollster')]
    colnames(df) <- c('date', 'var', 'error', 'pollster')
    if (input$pollster != "All") {
      df <- df[polls$pollster == input$pollster,]
    } 
    df <- df[df$date >= input$date_range2[1] & df$date <= input$date_range2[2],]
    df
  })
  
  
  output$ts_polls <- renderPlot({
    
    limits <- aes(ymax = var + error, ymin = var - error)
    
    ggplot(react_polls(), aes(x=date, y=var)) +
      geom_errorbar(limits, width=0, colour = 'grey') +
      geom_smooth(se = FALSE) +
      geom_point() +
      geom_hline(yintercept = 50) +
      scale_y_continuous(breaks = seq(0, 100, 5)) +
      scale_x_date(date_labels = "%b %Y") +
      labs(x=NULL, y="%") + 
      theme_bw() +
      theme(legend.position="bottom")
    
  })
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)

# ggplot(expected_results, aes(x=date, y=expected_result)) + 
#   geom_line() +
#   scale_x_date(limits = as.Date(c("2016-01-31", Sys.Date())))

