# Application for the final assigment on developing products course

library(shiny)
library(datasets)
library(dplyr)
library(ggplot2)

df<-morley
df$Expt<-as.factor(df$Expt)

# 
ui <- navbarPage(title = "Speed of Light",
                 theme = "bootswatch-cerulean.css",
                 tabPanel("graphs", 
                          sidebarLayout(
                              sidebarPanel(
                                  h2("Inputs"),
                                  hr(),
                                  h3("Selection of samples"),
                                  numericInput("qt_inf", 
                                               "Inferior quantile", 
                                               value = 0.05, 
                                               min = 0, 
                                               max = 1,
                                               step = 0.01),
                                  numericInput("qt_sup", 
                                               "Superior quantile", 
                                               value = 0.95, 
                                               min = 0, 
                                               max = 1,
                                               step = 0.01),
                                  hr(),
                                  h3("Graphics Generation"),
                                  p("Press button to generate the graphics, after that you can manipulate some characteristics"),
                                  actionButton("gen_bp", 
                                               "Generate Graphs"),
                                  sliderInput("bin_qty",
                                              "Bins Quantity",
                                              min = 1,
                                              max = 15,
                                              step = 1,
                                              value = 6),
                                  selectInput("sel_col",
                                              "Prob Density Selector",
                                              c("by experiment" = "expt", "with all data" = "all")),
                                  hr(),
                                  h3("Use"),
                                  tags$li("Change superior and inferior quantile limits to select data (see Data Selection Graph)"),
                                  tags$li("Click on the", em("Generate Graphics Button")),
                                  tags$li("Interact with the available inputs to see effects on the generated graphs")
                              ),
                              mainPanel(
                                  splitLayout(
                                      plotOutput("points"),
                                      plotOutput("boxplot")
                                  ),
                                  splitLayout(
                                      plotOutput("histogram"),
                                      plotOutput("density")
                                  )
                              )
                          )
                          ),
                 tabPanel("documentation",
                          h3("Documentation"),
                          p("Using the dataset", em("morley"), " in the R library", em("datasets"), 
                            "this application generates 4 graphics to show some functionalities 
                            of shiny applications."),
                          p("The dataset takes data from 5 experiments with 20 samples each
                            (measurements) of the speed of light, as showed in the following table:"),
                          dataTableOutput("data")
                          )
)


server <- function(input, output) {
    df1<-reactiveValues(dat=tibble(Expt=0, Run=0, Speed=0))
    output$points <- renderPlot({
        ggplot(data = df, aes(x=as.numeric(rownames(df)), y=Speed))+
            geom_point(aes(color=Expt))+
            geom_abline(slope=0, intercept = mean(df$Speed), color ="red")+
            geom_abline(slope=0, intercept = quantile(df$Speed, input$qt_inf), color ="blue")+
            geom_abline(slope=0, intercept = quantile(df$Speed, input$qt_sup), color ="blue")+
            ggtitle("Data selection")+
            xlab("Overall Experiment Sample")
    })
    observeEvent(input$gen_bp, {
        df1$dat<-filter(df, Speed<quantile(df$Speed, input$qt_sup) & Speed>quantile(df$Speed, input$qt_inf))
    })
    output$boxplot <- renderPlot({
        ggplot(data = df1$dat, aes(y=Speed))+
            geom_boxplot(aes(color = Expt))+
            ggtitle("Speed of Light, by experiments")
    })
    output$histogram <-renderPlot({
        ggplot(data = df1$dat, aes(x=Speed))+
            geom_histogram(bins=input$bin_qty)
    })
    output$density <-renderPlot({
        if(input$sel_col == "all"){
            ggplot(data = df1$dat, aes(x=Speed))+
                geom_density(color="black")
        } else {
            ggplot(data = df1$dat, aes(x=Speed))+
                geom_density(aes(color = Expt))
        }
    })
    output$data <- renderDataTable(df, 
                                   options = list(
                                       pageLength =15
                                   )
                                   )
}

# Run the application 
shinyApp(ui = ui, server = server)
