library(shiny)
library(shinydashboard)
library(tidyverse)

server <- function(input, output, session) {
    
    # load the data
    usa = readRDS("usa_data.rds")
    
    output$expl_plot_title = renderUI(paste("Plot of", input$expl_variable))
    
    map_var_name = function(){
        return(switch(input$expl_variable, "Capacity (MW)"="capacity_mw",
                      "Primary Fuel"="primary_fuel",
                      "Energy Generated in 2013 (GWh)"="generation_gwh_2013",
                      "Energy Generated in 2014 (GWh)"="generation_gwh_2014",
                      "Energy Generated in 2015 (GWh)"="generation_gwh_2015",
                      "Energy Generated in 2016 (GWh)"="generation_gwh_2016",
                      "Energy Generated in 2017 (GWh)"="generation_gwh_2017"))
    }
    
    exploratory_plot = function(){
        if(input$expl_variable == "Primary Fuel"){
            ggplot(data = usa) +
                geom_point(aes(x = longitude, y = latitude, color = primary_fuel), alpha = 0.5)
        }else{
            plot_var = map_var_name()
            ggplot(data = usa) +
                geom_point(aes(x = longitude, y = latitude, color = !!ensym(plot_var)), alpha = 0.5) + 
                scale_color_gradient(trans = "log", breaks = c(0.001,0.01,0.1,1,10,100,1000,10000))
        }
    }
    
    output$expl_plot = renderPlot({exploratory_plot()})
    
    output$expl_plot_note = renderUI(ifelse(input$expl_variable == "Primary Fuel", "",
                                "Note: Locations with missing data or values 0 or less are colored gray"))
    
    output$downloadPlot = downloadHandler(
        filename = function(){paste0(input$expl_variable, ".png")},
        content = function(file){ggsave(file, exploratory_plot(), device = "png")}
    )
    
    output$downloadData = downloadHandler(
        filename = function(){paste(map_var_name(),"data.csv")},
        content = function(file){write.csv(usa[,c("latitude", "longitude", map_var_name())],
                                           file, row.names = FALSE)}
    )
    
    output$downloadAllData = downloadHandler(
        filename = function(){"all_data.csv"},
        content = function(file){write.csv(usa, file)}
    )
    
    stat_df = function(){
        x = usa[[map_var_name()]]
        stat_names = c("Min", "25% Quartile", "Median", "75% Quartile", "Max", "Mean", "% Missing")
        stats = round(c(quantile(x, probs = c(0,0.25,0.5,0.75,1), na.rm = TRUE),
                      mean(x, na.rm = TRUE), 100*mean(is.na(x))), 2)
        return(data.frame(Value=stats, row.names = stat_names))
    }
    
    output$dataSummary = DT::renderDataTable({
        if(input$expl_variable == "Primary Fuel"){
            data.frame(EnergyType = names(table(usa$primary_fuel)),
                       Counts = as.vector(table(usa$primary_fuel)))
        } else {
            stat_df()
        }
    })
    
    subset_data = function(){
        if(length(input$cluster_vars > 0)){
            usa_subset = usa[,input$cluster_vars]
            keep_rows = complete.cases(usa_subset)
            usa_subset = usa_subset[keep_rows,]
            latlong = usa[keep_rows, c("latitude", "longitude")]
            return(list(latlong,usa_subset))
        } else {
            return(list(NULL, data.frame(latitude=NULL, longitude=NULL)))
        }
    }
    
    cluster_data = function(df){
        if(length(input$cluster_vars > 0)){
            clust_data = sapply(df, function(x){(x-mean(x))/sd(x)})
            clustering = hclust(dist(clust_data))
            return(clustering)
        } else {
            return(NULL)
        }
    }
    
    output$clusterPlot = renderPlot({
        if(length(input$cluster_vars > 0)){
            dfs = subset_data()
            latlong = dfs[[1]]
            clustering = cluster_data(dfs[[2]])
            clusters = cutree(clustering, k = input$num_clusters)
            qplot(x = latlong$longitude, y = latlong$latitude, color = as.factor(clusters))
        } else {
            ggplot()
        }
    })
    
    output$clusterNACount = renderUI({
        if(length(input$cluster_vars > 0)){
            df = subset_data()[[2]]
            ifelse(nrow(usa) - nrow(df) > 0,
                   paste("Note:", nrow(usa) - nrow(df), "records are excluded due to NAs."),
                   "")
        } else {
            ""
        }
    })
    
    observe({updateSliderInput(session, "mtry_slider", max = max(1, length(input$rf_vars)))})
    
    #rf_model = eventReactive({
    #    y = usa$primary_fuel
    #})
    
}