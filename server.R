library(shiny)
library(shinydashboard)
library(tidyverse)
library(randomForest)

server <- function(input, output, session) {
    
	###GENERAL FUNCTIONS############################################################################
    # load the data
    usa = readRDS("usa_data.rds")
    
	# map inputs from usesr controls to the corresponding column names 
    map_var_name = function(){
        return(switch(input$expl_variable, "Capacity (MW)"="capacity_mw",
                      "Primary Fuel"="primary_fuel",
                      "Energy Generated in 2013 (GWh)"="generation_gwh_2013",
                      "Energy Generated in 2014 (GWh)"="generation_gwh_2014",
                      "Energy Generated in 2015 (GWh)"="generation_gwh_2015",
                      "Energy Generated in 2016 (GWh)"="generation_gwh_2016",
                      "Energy Generated in 2017 (GWh)"="generation_gwh_2017"))
    }
    ################################################################################################
	
	###EXPLORATORY TAB FUNCTIONS####################################################################
	output$expl_plot_title = renderUI(paste("Plot of", input$expl_variable))
    
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
    ################################################################################################
	
	###CLUSTERING TAB FUNCTIONS#####################################################################
	# extract just the data needed for clustering
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
    
	# perform the hierarchical clustering and return the cluster numbers for each data point.
    cluster_data = function(df){
        if(length(input$cluster_vars > 0)){
            clust_data = sapply(df, function(x){(x-mean(x))/sd(x)})
            clustering = hclust(dist(clust_data))
            return(clustering)
        } else {
            return(NULL)
        }
    }
    
	# make the clustering plot
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
    
	# give a note to the user about how many NAs (incomplete cases) there are
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
    ################################################################################################
	
	###FUEL TYPE (RANDOM FOREST) FUNCTIONS##########################################################
	# make the mtry slider control's max value change as appropriate
    observe({updateSliderInput(session, "mtry_slider", max = max(1, length(input$rf_vars)))})
    
	# generate the random forest model on a click of the train button; use only complete cases since
	# RFs can't handle NAs
    rf_model = eventReactive(input$train_rf, {
        if(length(input$rf_vars) > 0){
            x = usa[,c(input$rf_vars, "primary_fuel")]
            x = x[complete.cases(x),]
            y = x$primary_fuel
            x = x %>% select(-primary_fuel)
            return(randomForest(x = x, y = as.factor(y),
                                mtry = input$mtry_slider, ntree=input$num_trees))
        }
    })
    
	# make the prediction; have to assemble the prediction data frame on the fly, but don't use all
	# of the columns
    rf_prediction = eventReactive(input$pred_rf, {
        m = rf_model()
        pred_df = data.frame(latitude=input$rf_latitude,
                             longitude=input$rf_longitude,
                             capacity_mw=input$rf_capacity,
                             commissioning_year=input$rf_year,
                             generation_gwh_2017=input$rf_gen2017)
        pred_df = pred_df[,input$rf_vars]
        pred = predict(m, pred_df)
        return(as.character(pred))
    })
	
    output$rf_pred = renderUI(rf_prediction())
	################################################################################################
	
	###2017 gen (LINEAR REGRESSION) FUNCTIONS#######################################################
	
	output$GWh_math = renderUI(
		withMathJax(helpText("Note: GWh are a unit of energy where $$1 GWh = 1GW * 1hour$$"))
	)
	
	#Generate the linear regression model; only trigger it when the train button is pressed to
	#avoid unnecessary overhead; discard incomplete columns
	lr_model = eventReactive(input$train_lr, {
        if(length(input$lr_vars) > 0){
            df = usa[,c(input$lr_vars, "generation_gwh_2017")]
            df = df[complete.cases(x),]
            return(lm(generation_gwh_2017 ~ ., data = df))
        }
    })
	
	#Make predictions for the linear regression model
	lr_prediction = eventReactive(input$pred_lr, {
		m = lr_model()
		pred_df = data.frame(latitude=input$lr_latitude,
                             longitude=input$lr_longitude,
                             capacity_mw=input$lr_capacity,
                             commissioning_year=input$lr_year,
                             primary_fuel=input$lr_fuel)
		pred = predict(m, pred_df)
		return(pred)
	})
	output$lr_pred = renderUI(paste(lr_prediction(), "GWh"))
	################################################################################################
}