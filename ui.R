library(shinydashboard)

### Info for the 'about' panel #####################################################################
about_string = HTML("<p>This web app was designed to examine data on power plants in the United
                    States. It contains data regarding the location, fuel type, capacity, and
                    generated energy of the individual plants.
                    <br><br>
                    For convenience of plotting, only the lower 48 states are considered.
                    <br>
                    The data is a subset of the Global Power Plant Database, a collection of data on
                    approximately 30,000 power plants across the globe.  It is from version 1.2.0 of
                    the database, released 2019-06-12 available
                    <a href='http://datasets.wri.org/dataset/globalpowerplantdatabase'>here</a></p>")
about_tab = tabItem("about",fluidRow(box(about_string, width = 8)))
####################################################################################################


### UI layout for the exploration tab ##############################################################
input_options = c("Capacity (MW)", "Primary Fuel", "Energy Generated in 2013 (GWh)",
    "Energy Generated in 2014 (GWh)", "Energy Generated in 2015 (GWh)",
    "Energy Generated in 2016 (GWh)", "Energy Generated in 2017 (GWh)"
)
expl_controls = box(width = 4,
    selectizeInput("expl_variable", "Select variable to examine: ", choices = input_options),
    HTML("<br>"),
    downloadButton("downloadPlot", label = "Download Current Plot"),
    HTML("<br>"),
    downloadButton("downloadData", label = "Download Current Data")
)

expl_plot = box(title = uiOutput("expl_plot_title"), width = 8,
                plotOutput("expl_plot"),
                uiOutput("expl_plot_note"),
                HTML("<br>"),
                DT::dataTableOutput("dataSummary"))

exploration_tab = tabItem("exploration", fluidRow(expl_controls, expl_plot))
####################################################################################################

dashboardPage(
    title="US Power Plant Dashboard",
    skin="green",
    dashboardHeader(title = "US Power Plants"),
    dashboardSidebar(
        menuItem("About This Data", tabName = "about", icon=icon("info")),
        menuItem("Data Exploration", tabName = "exploration", icon=icon("book-open")),
        menuItem("Clustering", tabName = "clustering", icon=icon("asterisk")),
        menuItem("Predict Fuel Type", tabName = "model_fuel", icon=icon("sun")),
        menuItem("Predict Generated Amount", tabName = "model_generated", icon=icon("battery-three-quarters")),
        HTML("<br>"),
        downloadButton("downloadAllData", "Download Data Set")
    ),
    dashboardBody(
        tabItems(
            about_tab, exploration_tab
        )
    )
)