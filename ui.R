library(shinydashboard)
library(plotly)

### Info for the 'about' panel #####################################################################
about_string = HTML("<p>This web app was designed to examine data on power plants in the United
                    States. It contains data regarding the location, fuel type, capacity, and
                    generated energy of the individual plants. For convenience of plotting, only the
					lower 48 states are considered.
                    <br>
                    The data is a subset of the Global Power Plant Database, a collection of data on
                    approximately 30,000 power plants across the globe.  It is available
                    <a href='http://datasets.wri.org/dataset/globalpowerplantdatabase'>here</a>.</p>
					<br>
					<i>Data version 1.2.0, released 2019-06-12</i>")
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


### UI layout for clustering tab ###################################################################

# Note: checkboxGroupInput outputs a character vector of the selected choices.
cluster_selector = box(width = 4,
                       checkboxGroupInput("cluster_vars", "Clustering variables:",
                                          c("Latitude"="latitude", "Longitude"="longitude",
                                            "Capacity (MW)"="capacity_mw",
                                            "Commissioning Year"="commissioning_year",
                                            "Energy Generated in 2013 (GWh)"="generation_gwh_2013",
                                            "Energy Generated in 2014 (GWh)"="generation_gwh_2014",
                                            "Energy Generated in 2015 (GWh)"="generation_gwh_2015",
                                            "Energy Generated in 2016 (GWh)"="generation_gwh_2016",
                                            "Energy Generated in 2017 (GWh)"="generation_gwh_2017")),
                       sliderInput("num_clusters", label="Number of clusters",
                                   min=2, max=10, value=2, step = 1))

cluster_plot = box(width = 8,
                   title = "Clustered Data Plot",
                   plotlyOutput("clusterPlot"),
                   uiOutput("clusterNACount"))

clustering_tab = tabItem("clustering", fluidRow(cluster_selector, cluster_plot))
####################################################################################################

### UI for primary fuel prediction #################################################################
fuel_selector = box(width = 6,
                    HTML("These are the parameters for a random forest model. It is attempting to
                         predict the primary fuel type for the power plant using the selected
                         variables and parameter values.<br>"),
                    checkboxGroupInput("rf_vars", "Clustering variables:",
                                       c("Latitude"="latitude", "Longitude"="longitude",
                                         "Capacity (MW)"="capacity_mw",
                                         "Commissioning Year"="commissioning_year",
                                         "Energy Generated in 2017 (GWh)"="generation_gwh_2017")),
                    sliderInput("num_trees", "Number of Trees",
                                min = 10, max = 200, value = 10, step = 10),
                    sliderInput("mtry_slider", "Number of Variables Sampled at Each Split (mtry)",
                                min = 1, max = 1, value = 1, step = 1),
                    actionButton("train_rf", "Train"))

fuel_predictor = box(width = 6, title = "Prediction",
                     numericInput("rf_latitude", "Latitude:", 35, min = 20, max = 55),
                     numericInput("rf_longitude", "Longitude:", -80, min = -100, max = -60),
                     numericInput("rf_capacity", "Capacity (MW):", 0, min = 0),
                     numericInput("rf_year", "Year Commissioned:", 2000, min = 1900, max = 2019),
                     numericInput("rf_gen2017", "2017 Generation: ", 0),
                     HTML("<br>"),
                     actionButton("pred_rf", "Predict"),
                     HTML("<br>Predicted primary fuel type:"),
                     uiOutput("rf_pred"))

primary_fuel_tab = tabItem("model_fuel", fluidRow(fuel_selector, fuel_predictor))
####################################################################################################

### UI for 2017 generation prediction ##############################################################
capacity_selector = box(width = 6,
                        HTML("These are the parameters for a linear regression model. It is
                             attempting to predict the capacity of the power plant using the
                             variables selected.<br><br>"),
						uiOutput("GWh_math"),
                        checkboxGroupInput("lr_vars", "Clustering variables:",
                                           c("Latitude"="latitude", "Longitude"="longitude",
                                             "Primary Fuel"="primary_fuel",
                                             "Capacity (MW)"="capacity_mw",
                                             "Commissioning Year"="commissioning_year")),
                        actionButton("train_lr", "Train"))

capacity_predictor = box(width = 6, title = "Prediction",
						 numericInput("lr_latitude", "Latitude:", 35, min = 20, max = 55),
						 numericInput("lr_longitude", "Longitude:", -80, min = -100, max = -60),
						 selectizeInput("lr_fuel", "Primary Fuel Type: ",
						                choices = c("Biomass", "Coal", "Gas", "Geothermal",
										            "Hydro", "Nuclear", "Oil", "Other", "Solar",
													"Waste", "Wind")),
						 numericInput("lr_capacity", "Capacity (MW):", 0, min = 0),
						 numericInput("lr_year", "Year Commissioned:", 2000, min = 1900, max = 2019),
						 HTML("<br>"),
						 actionButton("pred_lr", "Predict"),
						 HTML("<br>Predicted generation in 2017:"),
						 uiOutput("lr_pred"))

capacity_tab = tabItem("model_generated", fluidRow(capacity_selector, capacity_predictor))
####################################################################################################

dashboardPage(
    title="US Power Plant Dashboard",
    skin="green",
    dashboardHeader(title = "US Power Plants"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About This Data", tabName = "about", icon=icon("info")),
            menuItem("Data Exploration", tabName = "exploration", icon=icon("book-open")),
            menuItem("Clustering", tabName = "clustering", icon=icon("asterisk")),
            menuItem("Predict Fuel Type", tabName = "model_fuel", icon=icon("sun")),
            menuItem("Predict Generated Amount", tabName = "model_generated", icon=icon("battery-three-quarters"))
        ),
        HTML("<br>"),
        downloadButton("downloadAllData", "Download Data Set")
    ),
    dashboardBody(
        tabItems(
            about_tab, exploration_tab, clustering_tab, primary_fuel_tab, capacity_tab
        )
    )
)