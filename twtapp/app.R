# Load
setwd('~/public_git/referendum_ita_2016')
load("data/twt_analysis.RData")

ts_plot_max_hight <-
  max(ts_hour$value, na.rm = T) * 1.5

# TS variables
vars <- as.character(unique(ts_hour$variable))
base_names <- c("tweets/hour", "users/hour", "hashtags/hour", 
                "YES hashtags/hour", "NO hashtags/hour",
                "YES users/hour", "NO users/hour")
names <- base_names
n_rollwins <- length(vars) / length(base_names) - 1
width_rollwins <- c(1,2,4,8,16,32,64)
for (i in n_rollwins) {
  if (i == 1) {
    names <- c(names, paste0(base_names, " (MA ", width_rollwins[i], " day)")) 
  } else {
    names <- c(names, paste0(base_names, " (MA ", width_rollwins[i], " days)")) 
  }
}

names(vars) <- names

ts_hour$label <- names(vars)[match(ts_hour$variable, vars)]

library(RColorBrewer)
base_colors <- c('#8dd3c7', '#bebada', '#fdb462', '#ef8a62', '#67a9cf', '#ef8a62', '#67a9cf')
for (i in n_rollwins) {
  myColors <- c(base_colors, base_colors)
}

names(myColors) <- names(vars)
library(ggplot2)
colScale <- scale_colour_manual(name = "label", values = myColors)

# Fun
df <- freq_mentions_yes
printMentions <- function(df, n = 20) {
  df <- df[1:n,]
  html <- c("<table id = 'mention_tbl'>", 
            "<tr>", 
            rep("<th></th>", 3), "<th>Mentions</th>", 
            "</tr>")
  for (i in 1:nrow(df)) {
    html <- c(html, "<tr>")
    html <- c(html, paste0("<td><img src='", df$image[i], "'></td>"))
    html <- c(html, paste0("<td><a href = 'http://twitter.com/",df$screen_name[i],"'>@", df$screen_name[i], "</a></td>"))
    html <- c(html, paste0("<td>", df$name[i],"</td>"))
    html <- c(html, paste0("<td>",df$freq[i],"</td>"))
    html <- c(html, "</tr>")
  }
  html <- c(html, "</table>")
  return(html)
  # return(paste(html, collapse = ""))
}

library(shiny)
library(leaflet)

# To embed a tweet
tweet_script <- 
  '

  window.onload = (function(){

	var tweets = jQuery(".tweet");
	
  jQuery(tweets).each( function( t, tweet ) { 

  var id = jQuery(this).attr("id");

  twttr.widgets.createTweet(
  id, tweet, 
  {
    conversation : "none",    // or all
    cards        : "hidden",  // or visible 
    linkColor    : "#cc0000", // default is blue
    theme        : "light"    // or dark
  });

  });

  });

  '


ui <- shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("

                    .container {
                      display: flex;
                      width: 95%;
                      margin: 0 auto; 
                    }
                    .container > div {
                    margin:10px 5px 5px 5px;
                    width: 100px;
                    text-align: center;
                    font-weight: bold;
                    }

                    #tweet {
                      width: 400px !important;
                    }
                   
                    #tweet iframe {
                      border: none !important;
                      box-shadow: none !important;
                    }

                    #mention_tbl {
                      text-align: center;
                      border-collapse: separate;
                      border-spacing: 5px 5px;
                    }

                    

                    ")),
    tags$script(src="http://platform.twitter.com/widgets.js"),
    tags$script(tweet_script)
  ),
  
  fluidRow(
    tags$h2("Referendum costituzionale 2016: on Twitter", style = 'text-align: center;'),
    tags$br(),
    column(4,
           tags$div(
             class = "container",
             div(id ='yes_users',
                 textOutput('yes_users'),
                 p("YES"),
                 style = 'background-color: #ef8a62'),
             div(id ='no_users',
                 textOutput('no_users'),
                 p("NO"),
                 style = 'background-color: #67a9cf'),
             div(id ='yes_no_users',
                 textOutput('yes_no_users'),
                 p("Users"),
                 style = 'background-color: #f7f7f7')),
           tags$div(
             class = "container",
             div(id ='yes_hashtags',
                 textOutput('yes_hashtags'),
                 p("YES"),
                 style = 'background-color: #ef8a62'),
             div(id ='no_hashtags',
                 textOutput('no_hashtags'),
                 p("NO"),
                 style = 'background-color: #67a9cf'),
             div(id ='yes_no_hashtags',
                 textOutput('yes_no_hashtags'),
                 p("Hashtags"),
                 style = 'background-color: #f7f7f7')),
           tags$br(),
           tags$p("This is not a survey and is not necessarily representative of trends in the Italian electorate.",
                  style = "font-weight: bold; font-size: 70%;"), 
           tags$hr(),
           tags$div(
             class = "container",
             div(id ='n_users',
                 textOutput('n_users'),
                 p("Users collected"),
                 style = 'background-color: #f7f7f7; font-weight: normal; font-size: 85%;'),
             div(id ='last_tweet',
                 p("Last updated"),
                 textOutput('last_tweet'),
                 style = 'background-color: #f7f7f7; font-weight: normal; font-size: 75%; font-style: italic;'),
             div(id ='n_tweets',
                 textOutput('n_tweets'),
                 p("Tweets collected"),
                 style = 'background-color: #f7f7f7; font-weight: normal; font-size: 85%;')),
           tags$br(),
           tags$p("Tweets collected via the Twitter streaming API based on the following search terms: iovotono', 'iovotosi', 'referendumcostituzionale', 'iodicosi', 'iodicono'.", style = "font-style: italic; font-size: 70%;") 
    ),
    column(8,
           fluidRow(
             column(3,
                    div(style="height: 30px; font-size: 70%",
                        selectInput("vars_ts", NA, choices = vars, selected = c('yes_user', 'no_user'),  
                                    multiple = TRUE, selectize = TRUE))),
             column(1),
             column(8, 
                    sliderInput("date_range1", width = "100%",
                                NA, as.Date(min(ts_hour$hour)), as.Date('2016-12-04'),
                                value = c(as.Date(Sys.time())-30, as.Date(Sys.time())), 
                                step = 1, sep = ""))),
           plotOutput("plot_ts", height = '200px'),
           tags$p("MA: Moving average", style = "font-style: italic; font-size: 70%; text-align: center;")
    )      
  ),
  hr(),
  fluidRow(
    column(6, 
           leafletOutput("map_activity")
    ),
    column(6,
           leafletOutput("map_yes_no")
    )  
  ),
  hr(),
  h3("Most retweeted in the last 5 days", style = 'text-align: center'),
  fluidRow(
    column(6, 
           tags$div(
             h4("YES field", style = 'text-align: center'),
             tags$div(class="tweet", id=top_5_retweets_yes[1]),
             tags$div(class="tweet", id=top_5_retweets_yes[2]),
             tags$div(class="tweet", id=top_5_retweets_yes[3]),
             tags$div(class="tweet", id=top_5_retweets_yes[4]),
             tags$div(class="tweet", id=top_5_retweets_yes[5]),
             style = "background-color: #ef8a62; padding: 5px 5px 5px 5px; text-align: center;"
           )
    ),
    column(6,
           tags$div(
             h4("NO field", style = 'text-align: center'),
             tags$div(class="tweet", id=top_5_retweets_no[1]),
             tags$div(class="tweet", id=top_5_retweets_no[2]),
             tags$div(class="tweet", id=top_5_retweets_no[3]),
             tags$div(class="tweet", id=top_5_retweets_no[4]),
             tags$div(class="tweet", id=top_5_retweets_no[5]),
             style = "background-color: #67a9cf; padding: 5px 5px 5px 5px;"
           )
    )),
  hr(),
  h3("Most shared link in the last 5 days", style = 'text-align: center'),
  fluidRow(
    column(6, 
           tags$div(
             h4("YES field", style = 'text-align: center'),
             dataTableOutput('resources_yes_datatable'),
             style = "background-color: #ef8a62; padding: 5px 5px 5px 5px; text-align: center;"
           )
    ),
    column(6,
           tags$div(
             h4("NO field", style = 'text-align: center'),
             dataTableOutput('resources_no_datatable'),
             style = "background-color: #67a9cf; padding: 5px 5px 5px 5px;"
           )
    )),
  hr(),
  h3("Most metioned users", style = 'text-align: center'),
  fluidRow(
    column(6, 
           tags$div(
             h4("YES field", style = 'text-align: center'),
             HTML(printMentions(freq_mentions_yes)),
             style = "background-color: #ef8a62; padding: 5px 5px 5px 5px; text-align: center;"
           )
    ),
    column(6,
           tags$div(
             h4("NO field", style = 'text-align: center'),
             HTML(printMentions(freq_mentions_no)),
             style = "background-color: #67a9cf; padding: 5px 5px 5px 5px;"
           )
    )),
  hr(),
  h3("Most metioned hashtags", style = 'text-align: center'),
  fluidRow(
    column(6, 
           tags$div(
             h4("YES field", style = 'text-align: center'),
             dataTableOutput('hashtags_yes_datatable'),
             style = "background-color: #ef8a62; padding: 5px 5px 5px 5px; text-align: center;"
           )
    ),
    column(6,
           tags$div(
             h4("NO field", style = 'text-align: center'),
             dataTableOutput('hashtags_no_datatable'),
             style = "background-color: #67a9cf; padding: 5px 5px 5px 5px;"
           )
    )),
  hr(),
  h3("Most linked resources", style = 'text-align: center'),
  fluidRow(
    column(6, 
           tags$div(
             h4("YES field", style = 'text-align: center'),
             dataTableOutput('urls_yes_datatable'),
             style = "background-color: #ef8a62; padding: 5px 5px 5px 5px; text-align: center;"
           )
    ),
    column(6,
           tags$div(
             h4("NO field", style = 'text-align: center'),
             dataTableOutput('urls_no_datatable'),
             style = "background-color: #67a9cf; padding: 5px 5px 5px 5px;"
           )
    )),
  tags$hr(),
  fluidRow(tags$div(id='footer',
                    style='font-size: 70%; margin: auto; width: 90%; padding: 10px 10px 10px 10px; text-align: center;',
                    HTML(paste0(
                      "Design: Francesco Bailo (<a href='https://twitter.com/FrBailo' target='_blank'>@FrBailo</a>) ",
                      "| Code: <a href='https://github.com/fraba/referendum_ita_2016' target='_blank'>GitHub</a> ",
                      "| Powered by: <a href='http://www.R-project.org/' target='_blank'>R</a> and <a href='http://shiny.rstudio.com/' target='_blank'>Shiny</a> ",
                      "| R packages: <a href='http://ggplot2.org' target='_blank'>ggplot2</a>, ",
                                    "<a href='https://CRAN.R-project.org/package=RColorBrewer' target='_blank'>RColorBrewer</a>, ",
                                    "<a href='https://CRAN.R-project.org/package=viridis' target='_blank'>viridis</a>, ",
                                    "<a href='https://CRAN.R-project.org/package=leaflet' target='_blank'>leaflet</a>, ",
                                    "<a href='https://CRAN.R-project.org/package=twitteR' target='_blank'>twitteR</a>",
                      "| Hosted by: <a href='https://nectar.org.au/research-cloud/'>Nectar Cloud</a> ",
                      "| Version: 0.1 "))))
))

server <- function(input, output) {
  
  format_num <- function(x) format(round(x,digits=2), big.mark=",", scientific=FALSE)
  
  ts_data <- reactive({
    subset(ts_hour, variable %in% input$vars_ts)
  })
  
  output$yes_no_users <- renderText({
    format_num(twt_analysis_sum_stats$users$yes_no)
  })
  
  output$yes_users <- renderText({
    paste0(round(twt_analysis_sum_stats$users$yes / twt_analysis_sum_stats$users$yes_no * 100, 2), 
           "%")
  })
  
  output$no_users <- renderText({
    paste0(round(twt_analysis_sum_stats$users$no / twt_analysis_sum_stats$users$yes_no * 100, 2), 
           "%")
  })
  
  output$yes_no_hashtags <- renderText({
    format_num(twt_analysis_sum_stats$hashtags$yes_no)
  })
  
  output$yes_hashtags <- renderText({
    paste0(round(twt_analysis_sum_stats$hashtags$yes / twt_analysis_sum_stats$hashtags$yes_no * 100, 2), 
           "%")
  })
  
  output$no_hashtags <- renderText({
    paste0(round(twt_analysis_sum_stats$hashtags$no / twt_analysis_sum_stats$hashtags$yes_no * 100, 2), 
           "%")
  })
  
  output$n_users <- renderText({
    format_num(twt_analysis_sum_stats$n_users)
  })
  
  output$n_tweets <- renderText({
    format_num(twt_analysis_sum_stats$n_tweets)
  })
  
  output$last_tweet <- renderText({
    format(as.POSIXct(max(ts_hour$hour), tz='Europe/Rome'), "%a, %e %b %Y at %I%p")
  })
  
  
  
  output$plot_ts <- renderPlot({
    ggplot(ts_data(), aes(x=as.POSIXct(hour, tz = 'Europe/Rome'), y=value, colour=label)) +
      geom_line(size=1) +
      scale_x_datetime() +
      coord_cartesian(xlim = as.POSIXct(c(input$date_range1[1], input$date_range1[2]), tz = 'Europe/Rome'),
                      ylim = c(0, ts_plot_max_hight)) +
      labs(x = NULL, y = NULL) +
      theme_bw() +
      theme(legend.position = c(0, 1), 
            legend.justification = c(0, 1),
            legend.title=element_blank()) + 
      colScale
  })
  
  output$map_activity <- renderLeaflet({
    
    # Users by region
    require(leaflet)
    require(viridis)
    italy_adm1_all_users$users_over_electorate <- 
      italy_adm1_all_users$n_users / italy_adm1_all_users$Numero.elettori
    require(leaflet)
    require(viridis)
    pal <- colorNumeric(
      palette = viridis(12),
      domain = italy_adm1_yes_no$users_over_electorate
    )
    format_num <- function(x) format(round(x,digits=2), big.mark=",", scientific=FALSE)
    leaflet(italy_adm1_all_users) %>%
      setView(lng = 12.5674, lat = 42, zoom = 5.2) %>%
      addPolygons(stroke = TRUE,  color = 'black', weight = 2, smoothFactor = 0.2, fillOpacity = 1,
                  fillColor = ~pal(users_over_electorate),
                  popup = ~paste0("<b>", NOME, "</b><br>Users: ", 
                                  format_num(n_users), "<br>",
                                  "Electorate: ", format_num(Numero.elettori))) %>%
      addLegend("topright", values = ~users_over_electorate,
                title = "Twitter activity", 
                colors = rev(viridis(8)),
                labels = c("high", rep("", 6), "low"), 
                opacity = 1
      )
    
  })
  
  output$hashtags_no_datatable = renderDataTable({
    freq_hashtags_no
  })
  
  output$hashtags_yes_datatable = renderDataTable({
    freq_hashtags_yes
  })
  
  output$urls_no_datatable = renderDataTable({
    freq_urls_no
  })
  
  output$urls_yes_datatable = renderDataTable({
    freq_urls_yes
  })
  
  output$resources_no_datatable = renderDataTable({
    freq_resources_no$expanded_url <-
      paste0("<a href='", freq_resources_no$expanded_url, "' target = '_blank' >&#128279;<a/>")
    freq_resources_no
  }, escape=FALSE)
  
  output$resources_yes_datatable = renderDataTable({
    freq_resources_yes$expanded_url <-
      paste0("<a href='", freq_resources_yes$expanded_url, "' target = '_blank' >&#128279;<a/>")
    freq_resources_yes
  }, escape=FALSE)
  
  output$map_yes_no <- renderLeaflet({
    
    require(leaflet)
    require(RColorBrewer)
    pal <- colorNumeric(
      palette = rev(brewer.pal(7, 'RdBu')),
      domain = c(0,1)
    )
    format_num <- function(x) format(round(x,digits=2), big.mark=",", scientific=FALSE)
    leaflet(italy_adm1_yes_no) %>%
      setView(lng = 12.5674, lat = 42, zoom = 5.2) %>%
      addPolygons(stroke = TRUE,  color = 'black', weight = 2, smoothFactor = 0.2, fillOpacity = 1,
                  fillColor = ~pal(mean_yes),
                  popup = ~paste0("<b>", NOME, "</b><br>Users: ", 
                                  format_num(n_users), "<br>",
                                  "Yes (%): ", format_num(mean_yes*100))) %>%
      addLegend("topright", values = ~mean_yes,
                title = "Yes tweets", 
                colors = brewer.pal(7, 'RdBu'),
                labels = c("100%", "", "", "50%", "", "", "0%"), 
                opacity = 1
      )
    
    
    
  })
  
}

shinyApp(ui, server)