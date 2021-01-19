library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(readxl)

#Import Data
library(tidyr)

species_monthly <- read_excel("Pokecenter_admits_report.xlsx", 
                              sheet = "Current Year Admit by Mo and Sp", 
                              skip = 15, n_max = 46)
total_monthly <- read_excel("Pokecenter_admits_report.xlsx", 
                            sheet = "Current Year Admit by Mo and Sp", 
                            skip = 63)
species_yearly <- read_excel("Pokecenter_admits_report.xlsx", 
                             sheet = "Disposition Table 1975-2019",
                             n_max=131)


# Fix column names for years
colnames(species_yearly) <- format(as.Date(as.numeric(colnames(species_yearly)), origin="1900-01-01"), "%Y")

colnames(species_yearly)[1:2] <- c("Species","Result")
species_yearly <- species_yearly[,!is.na(colnames(species_yearly))]
species_yearly <- species_yearly %>% fill(Species, .direction="down")
species_yearly[,-c(1,2)] <- sapply(species_yearly[,-c(1,2)], as.numeric)
species_yearly$Result <- ifelse(species_yearly$Result %in% c("transferred",
                                                             "transferred (out live/ dead)", 
                                                             "transferred (out)",
                                                             "transferred OUT (live/dead)"),
                                "transferred out",
                                species_yearly$Result)
species_yearly$Result <- ifelse(species_yearly$Result %in% c("transferred (out-LIVE)",
                                                             "transferred (OUT-LIVE)", 
                                                             "transferred out (LIVE only)"),
                                "transferred out (live)",
                                species_yearly$Result)
species_yearly$Result <- ifelse(species_yearly$Result %in% c("transferred (out-DEAD"),
                                "transferred out (dead)",
                                species_yearly$Result)
species_yearly$Result <- ifelse(species_yearly$Result %in% c("relocated/reunited"),
                                "relocated",
                                species_yearly$Result)
species_yearly$Result <- ifelse(species_yearly$Result %in% c("received IN (live/dead)"),
                                "transferred in",
                                species_yearly$Result)

species_yearly_reshaped <- species_yearly %>% 
  pivot_longer(colnames(species_yearly)[3:ncol(species_yearly)], names_to = "Year", values_to = "Count") %>%
  pivot_wider(names_from = Result, values_from = Count)
species_yearly_reshaped['Survived'] <- species_yearly_reshaped$released +
                                          species_yearly_reshaped$placed
species_yearly_reshaped['Died'] <- species_yearly_reshaped$DIT +
  species_yearly_reshaped$euthanized

species_yearly_reshaped['Survival Rate'] <- species_yearly_reshaped$Survived /
  (species_yearly_reshaped$Survived + species_yearly_reshaped$Died)
total_yearly <- species_yearly_reshaped %>% group_by(Year) %>% summarize(
  Species="All",
  Survived=sum(Survived, na.rm=T),
                                                         Died=sum(Died, na.rm=T),
                                                         Survival=round(100*Survived/(Survived+Died)),
                                                         Admit=sum(Admit, na.rm=T))
#ggplot(species_yearly_reshaped, aes(x=Year, y=`Live Admits`, group=Species, col=Species)) +
#  geom_line()
#ggplot(bind_rows(species_yearly_reshaped,
#            total_yearly), aes(y=Admit, group=Species, col=Species)) +
# geom_boxplot()

#ggplot(total_yearly, aes(x=Admit)) +
#  geom_boxplot()

# Species monthly
clean_monthly <- function(species_monthly){
  colnames(species_monthly)[1:2] <- c("Species","Result")
  species_monthly <- species_monthly %>% fill(Species, .direction="down")
  species_monthly_reshaped <- species_monthly[,1:13] %>% 
    pivot_longer(colnames(species_monthly)[3:13], names_to = "Month", values_to = "Count") %>%
    pivot_wider(names_from = Result, values_from = Count)
  species_monthly_reshaped$Month <- factor(species_monthly_reshaped$Month,
                                           levels = unique(species_monthly_reshaped$Month),
                                           ordered=T)
  return(species_monthly_reshaped)
}

species_monthly_reshaped <- clean_monthly(species_monthly)
my_col = sym("Total Admits")
#ggplot(species_monthly_reshaped, aes(x=Month, y=!!my_col, col=Species)) +
#  geom_point() +
#  ggtitle("Survival rate with loess regression")

species_monthly_reshaped['Percent Live Admits Rehabbed'] <- pmax(0, pmin(100, 100*species_monthly_reshaped$`Rehab Releases` / species_monthly_reshaped$`Live Admits`))
species_monthly_reshaped['Dead Dispo (Exclude Carcasses)'] <- species_monthly_reshaped$`Dead Dispo` - (species_monthly_reshaped$`Total Admits` - species_monthly_reshaped$`Live Admits`)
species_monthly_reshaped['Percent Live Admits Dead Disposed'] <- pmax(0, pmin(100, 100*species_monthly_reshaped$`Dead Dispo (Exclude Carcasses)` / species_monthly_reshaped$`Live Admits`))


# Total monthly
total_monthly_reshaped <- clean_monthly(total_monthly)

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

species_choices <- c("Pidgey" = "Pidgey",
"Weedle" = "Weedle",
"Beedrill" = "Beedrill",
"Clefairy" = "Clefairy",
"Pikachu" = "Pikachu",
"Rattata" = "Rattata",
"Eevee" = "Eevee",
"Ponyta" = "Ponyta",
"Bulbasaur" = "Bulbasaur",
"Charmander" = "Charmander")

# Define UI
ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("Pokecenter Rescues & Outcomes", theme = shinytheme("cerulean"),
             tabPanel("Yearly results", fluid = TRUE, icon = icon("calendar-week"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Show me..."),
                          #shinythemes::themeSelector(),
                          fluidRow(
                                          # Select which Species to plot
                                          checkboxGroupInput(inputId = "SpeciesFinder",
                                                             label = "Select Species:",
                                                             choices = species_choices,
                                                             selected = c("Pidgey","Weedle","Beedrill","Rattata","Pikachu","Clefairy","Ponyta","Eevee","Bulbasaur","Charmander")),
                          # Set Time Range
                          sliderInput(inputId = "Year",
                                      label = "Select Years:",
                                      min = 1995,
                                      max = 2019,
                                      value = c(1995,2019),
                                      width = "220px",
                                      sep=""),
                          selectInput(
                            inputId = "yearlyColumn",
                            label = "Value to plot: ",
                            choices = colnames(species_yearly_reshaped[3:ncol(species_yearly_reshaped)]),
                            selected = colnames(species_yearly_reshaped[3]),
                            multiple = FALSE,
                            selectize = TRUE
                          ),
                          selectInput(
                            inputId = "yearlyPlotType",
                            label = "Type of plot to draw: ",
                            choices = c("Scatterplot","Lineplot"),
                            selected = "Scatterplot"
                          )
                        )),
                        mainPanel(
                          withSpinner(plotOutput(outputId = "scatterplotFinder", click = "click_plotFinder"
                          )),
                          hr(),
                          fluidRow(
                            withSpinner(dataTableOutput(outputId = "yearSpeciesTable"))))
                      )
             ),
             tabPanel("Monthly results for 2020", fluid = TRUE, icon = icon("calendar-alt"),
                        tags$style(button_color_css),
                        # Sidebar layout with a input and output definitions
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel("Show me..."),
                            #shinythemes::themeSelector(),
                            fluidRow(
                              # Select which Species to plot
                              checkboxGroupInput(inputId = "SpeciesFinderMonthly",
                                                 label = "Select Species:",
                                                 choices = species_choices,
                                                 selected = c("Pidgey","Weedle","Beedrill","Rattata","Pikachu","Clefairy","Ponyta","Eevee","Bulbasaur","Charmander")),
                              # Set Time Range
                              checkboxGroupInput(inputId = "Month",
                                                 label = "Select Months:",
                                                 choices = c("January","February","March","April","May","June","July","August","September","October","November"),
                                                 selected = c("January","February","March","April","May","June","July","August","September","October","November")),
                              selectInput(
                                inputId = "monthlyColumn",
                                label = "Value to plot: ",
                                choices = colnames(species_monthly_reshaped[3:ncol(species_monthly_reshaped)]),
                                selected = colnames(species_monthly_reshaped[3]),
                                multiple = FALSE,
                                selectize = TRUE
                              ),
                              selectInput(
                                inputId = "monthlyPlotType",
                                label = "Type of plot to draw: ",
                                choices = c("Scatterplot","Lineplot"),
                                selected = "Lineplot"
                              ),
                              h6(p("For purposes of this app, some liberties were taken. 'Dead Dispo' was assumed to be only carcasses, and % calculations were capped at 0-100% (values outside of this possible due to data reflecting 'results this month', not 'results for pokemon admitted this month')"))
                            )),
                          mainPanel(
                            withSpinner(plotOutput(outputId = "scatterplotFinderMonthly", click = "click_plotFinderMonthly"
                            )),
                            hr(),
                            fluidRow(
                              withSpinner(dataTableOutput(outputId = "monthSpeciesTable"))))
                        )
             ),
             tabPanel("Yearly survival rate", fluid = TRUE, icon = icon("otter"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Show me..."),
                          #shinythemes::themeSelector(),
                          fluidRow(
                            # Select which Species to plot
                            checkboxGroupInput(inputId = "survivalSpeciesFinder",
                                               label = "Select Species:",
                                               choices = c("Pidgey","Weedle","Beedrill"),
                                               selected = c("Pidgey","Weedle","Beedrill")),
                            # Set Time Range
                            sliderInput(inputId = "survivalYear",
                                        label = "Select Years:",
                                        min = 1995,
                                        max = 2019,
                                        value = c(1995,2019),
                                        width = "220px",
                                        sep=""),
                          )),
                        mainPanel(
                          withSpinner(plotOutput(outputId = "survivalChangePlot", click = "click_plotFinder"
                          )),
                          hr(),
                      )
                      )),
             tabPanel("Exceptionally busy years", fluid = TRUE, icon = icon("calculator"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Show me..."),
                          #shinythemes::themeSelector(),
                          fluidRow(
                            # Select which Species to plot
                            # Select which Species to plot
                            checkboxGroupInput(inputId = "busySpeciesFinder",
                                               label = "Select Species:",
                                               choices = species_choices,
                                               selected = c("Pidgey","Weedle","Beedrill","Rattata","Pikachu","Clefairy","Ponyta","Eevee","Bulbasaur","Charmander"))
                            )),
                        mainPanel(
                          withSpinner(
                            plotOutput(outputId = "totalBusyPlot", click = "click_plotFinder")
                            ),
                          br(),
                          withSpinner(
                            textOutput(outputId = "totalBusyText")
                          ),
                          br(),
                          hr(),
                          withSpinner(
                            plotOutput(outputId = "speciesBusyPlot", click = "click_plotFinder")
                          ),
                          br(),
                          withSpinner(
                            textOutput(outputId = "speciesBusyText")
                          ),
                          br(),
                          withSpinner(
                            textOutput(outputId = "speciesBusyThoughtsText")
                          ),
                          br(),
                          br()
                          
                        )
                      )),
                        tabPanel("About", fluid = TRUE, icon = icon("info-circle"),
                                 fluidRow(
                                   column(6,
                                          #br(),
                                          h4(p("About the Project")),
                                          h5(p("This is a simple shiny app created to explore some fictional data from a Pokecenter.")),
                                   ),
                                   column(6,
                                          h4(p("About the Author")),
                                          h5(p("Lian is a data scientist."),
                                          HTML('<img src="pidgey.png", height="200px"'),
                                          br()
                                   )
                                 ),
                                 br(),
                                 hr(),
                                 fluidRow(
                                   column(6, 
                                         h5("Sources:"),
                                         h6(
                                           p("Data made up for the purpose of this demo app.")),
                                         h5("Built with",
                                            img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                            "by",
                                            img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                                            ".")
                                         ),
                                 )
                        )
             )
  )
)

# Define server
server <- function(input, output, session) {
  
  output$scatterplotFinder <- renderPlot({
    input$Year
    input$SpeciesFinder
    input$yearlyPlotType
    input$yearlyColumn
    isolate({
      if(input$yearlyPlotType=="Scatterplot"){
        geom_info <- geom_jitter(width=0, height=diff(range(species_yearly_reshaped[!is.na(species_yearly_reshaped[,input$yearlyColumn]),input$yearlyColumn]))/100)
      } else if(input$yearlyPlotType=="Lineplot"){
        geom_info <- geom_line()
      }
      ggplot(subset(species_yearly_reshaped, Species %in% input$SpeciesFinder&
                    Year >= input$Year[1] &
                    Year <= input$Year[2]), aes(x=Year, y=!!sym(input$yearlyColumn), group=Species, col=Species)) +
        geom_info + 
        theme_light() +
        theme(axis.text.x = element_text(angle = 90))
    })
  })
  output$yearSpeciesTable <- DT::renderDataTable({
    input$Year
    input$SpeciesFinder
    isolate({
    DT::datatable(subset(species_yearly_reshaped, Species %in% input$SpeciesFinder&
                    Year >= input$Year[1] &
                    Year <= input$Year[2]))
    
    })
  })
  output$scatterplotFinderMonthly <- renderPlot({
    input$Month
    input$SpeciesFinderMonthly
    input$monthlyColumn
    input$monthlyPlotType
    isolate({
      if(input$monthlyPlotType=="Scatterplot"){
        geom_info <- geom_jitter(width=0, height=diff(range(species_monthly_reshaped[!is.na(species_monthly_reshaped[,input$monthlyColumn]),input$monthlyColumn]))/100)
      } else if(input$monthlyPlotType=="Lineplot"){
        geom_info <- geom_line()
      }
      ggplot(subset(species_monthly_reshaped, Species %in% input$SpeciesFinderMonthly&
                      Month %in% input$Month), aes(x=Month, y=!!sym(input$monthlyColumn), col=Species, group=Species)) +
         geom_info +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90))
    })
  })
  output$monthSpeciesTable <- DT::renderDataTable({
    input$Month
    input$SpeciesFinderMonthly
    isolate({
      DT::datatable(subset(species_monthly_reshaped, Species %in% input$SpeciesFinderMonthly&
                             Month %in% input$Month))
      
    })
  })
  output$survivalChangePlot <- renderPlot({
    input$survivalYear
    input$survivalSpeciesFinder
    isolate({
        geom_info <- geom_point()
        geom_info2 <- geom_smooth()
        ggplot(subset(species_yearly_reshaped, Species %in% input$survivalSpeciesFinder&
                        Year >= input$survivalYear[1] &
                        Year <= input$survivalYear[2]), aes(x=Year, y=!!sym("Survival Rate"), group=Species, col=Species)) +
          geom_info +
          geom_info2 +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90)) +
          ggtitle("Survival rate with loess regression")
    })
  })
  output$totalBusyPlot <- renderPlot({
    isolate({
      ggplot(total_yearly, aes(y=Admit, group=Species, col=Species)) +
      geom_boxplot() +
      theme_light() +
      ggtitle("Boxplot of total admits (all species)") + 
        theme(axis.title.x = element_blank(), axis.text.x = element_blank())
    })
    })
  output$speciesBusyPlot <- renderPlot({
    input$busySpeciesFinder
    isolate({
      ggplot(subset(species_yearly_reshaped, Species %in% input$busySpeciesFinder), 
             aes(y=Admit, group=Species, col=Species)) +
        geom_boxplot() +
      theme_light() +
        ggtitle("Boxplots of admits by species") + 
        theme(axis.title.x = element_blank(), axis.text.x = element_blank())
    })
  })
  output$totalBusyText <- renderText({paste0("Using the interquartile range rule for detecting outliers, 
    two years are determined to have been exceptionally busy years for the center as a whole: 2004 and 2008, with 1,851 and 1,706 admitted animals respectively."
    )})
  output$speciesBusyText <- renderText({
    paste0(
      "Looking at a total level masks particularly busy years on a species level. The center's busyness is largely driven by the most common species, Pidgeys.
      Unsurprisingly, the same two years were extremely busy for Pidgeys: both 2004 and 2008, with 1,381 and 1,370 respectively.
      Pikachus had one outlier year, in 2004 (with 107 admits - typical Pikachu volume is much lower than typical Pidgey volume).
      Technically (in addition to 2004) every year since 2016 has been an outlier for Clefairys - it appears the number of Clefairys admitted increased from a handful to 30-50 a year starting 2016.
      Similarly Eevees saw a big jump from ~30 a year (mostly transferred out) to 70-80 a year (and treated in house) starting 2017. This seems like a change in reporting though - starting in 2017, carcasses appear to be tracked whereas previously they were not.
      With low typical numbers, a small spike can be an outlier. Charmanders, in fact, had an outlier year in 2013 with 1 animal admitted as opposed to the typical 0.
      "
    )})
  output$speciesBusyThoughtsText <- renderText({
    paste0(
      "It seems perhaps there could be value in being able to predict particularly busy years by species (if 2004 and 2008 were a surprise). 
      In order to tackle this, I would talk to the experts to understand what is known about variables leading to particularly busy years. I would try to find leading indicators (e.g. Pokemon academy enrollment?) to see how far in advance I would have been able to predict those years would be busy. 
      Then I would continue grabbing those leading indicators and continuously aim to predict expected future admits to give the pokecenter enough leadtime to e.g. find extra employees."
    )})
  #session$onSessionEnded(stopApp)
}
# Run the application
shinyApp(ui = ui, server = server)