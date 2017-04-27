rm(list = ls())
cat("\014") 

packageList = c('ggplot2', 'shiny', 'plotly', 'GGally')
for (i in 1:length(packageList)) {
  if(! is.element(packageList[i],installed.packages()[,1])) {
    install.packages(packageList[i])
  }
}

library(shiny)
library(plotly)
library(ggplot2)
library(GGally)
library(dplyr)

df <- read.csv("dataset_Facebook.csv", sep=";", header=TRUE)
df[is.na(df)] <- 0 
df <- df %>% rename(month=Post.Month, 
                    weekday=Post.Weekday,
                    total_likes=Page.total.likes,
                    total_impressions=Lifetime.Post.Total.Impressions,
                    total_reach = Lifetime.Post.Total.Reach,
                    engaged_users = Lifetime.Engaged.Users,
                    cosumers = Lifetime.Post.Consumers,
                    consumptions = Lifetime.Post.Consumptions)
df$if_weekend = as.factor(df$weekday >= 6)

df_agg <- df %>% 
  group_by(Type, weekday) %>%
  summarise(sum_reach=sum(total_reach), sum_impressions=sum(total_impressions),
            engaged_users=sum(engaged_users), consumers=sum(cosumers),
            consumptions=sum(consumptions)) %>%
  mutate(conversion_rate = consumers / sum_reach, 
         consumption_rate = consumptions / sum_reach)


ui <- navbarPage("Facebook Metrics",
                 tabPanel("Bubble Plot",
                          fluidPage(
                            headerPanel('Total likes vs total impressions'),
                            sidebarPanel(
                              selectInput(inputId='type', label='Type', choices=c(levels(df$Type)), selected="Link")
                            ),
                            mainPanel(
                              plotOutput("bubble_plot")
                            )
                          )),
                 tabPanel("Scatter Plot",
                          plotlyOutput("scatter_plot")),
                 tabPanel("Parallel Coordinates Plot",
                          plotlyOutput("pc_plot"))
)


server <- function(input, output) {

  # Bubble plot
  selectedData <- reactive({
    subset(df, df$Type == input$type)
  })
  
  output$bubble_plot <- renderPlot({
    plt <- ggplot(data = selectedData(), aes(total_likes, total_impressions)) +
      geom_point(aes(colour = if_weekend), alpha = 0.5, size = 3) +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
    return (plt)
  })

  # Scatter plot
  output$scatter_plot <- renderPlotly({
    plt <- ggpairs(df_agg, c(8, 9), mapping=ggplot2::aes(color = Type))
    ggplotly(plt)
  })
  
  # Parallel Coordinates Plot
  output$pc_plot <- renderPlotly({
    plt <- ggparcoord(df, 8:11, groupColumn= "Type", scale = 'uniminmax') + 
      theme(axis.title = element_blank())
    ggplotly(plt)
  })
}

shinyApp(ui = ui, server = server)