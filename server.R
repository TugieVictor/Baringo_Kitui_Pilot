function(input, output, session){
  
  # Reactive dataset 
  Data_1 <- reactive({
    df <- Baringo_County %>%
      filter(Subcounty %in% input$Sub_county) 
    
  })
  
  Data_2 <- reactive({
    df <- B_Subcounty %>%
      filter(Subcounty %in% input$Sub_county) 
    
  })

  # Data table Output
  output$dataT <- renderDataTable(Baringo_Data)
  
  
  # # Rendering the box header  
  # output$head1 <- renderText(
  #   paste("5 states with high rate of", input$var2, "Arrests")
  # )
  # 
  # # Rendering the box header 
  # output$head2 <- renderText(
  #   paste("5 states with low rate of", input$var2, "Arrests")
  # )
  # 
  # 
  # # Rendering table with 5 states with high arrests for specific crime type
  # output$top5 <- renderTable({
  #   
  #   my_data %>% 
  #     select(State, input$var2) %>% 
  #     arrange(desc(get(input$var2))) %>% 
  #     head(5)
  #   
  # })
  # 
  # # Rendering table with 5 states with low arrests for specific crime type
  # output$low5 <- renderTable({
  #   
  #   my_data %>% 
  #     select(State, input$var2) %>% 
  #     arrange(get(input$var2)) %>% 
  #     head(5)
  #   
  #   
  # })
  
  
  # # For Structure output
  # output$structure <- renderPrint({
  #   my_data %>% 
  #     str()
  # })
  # 
  # 
  # # For Summary Output
  # output$summary <- renderPrint({
  #   my_data %>% 
  #     summary()
  # })
  
  # # For histogram - distribution charts
  # output$histplot <- renderPlotly({
  #   p1 = my_data %>% 
  #     plot_ly() %>% 
  #     add_histogram(x=~get(input$var1)) %>% 
  #     layout(xaxis = list(title = paste(input$var1)))
  #   
  #   
  #   p2 = my_data %>%
  #     plot_ly() %>%
  #     add_boxplot(x=~get(input$var1)) %>% 
  #     layout(yaxis = list(showticklabels = F))
  #   
  #   # stacking the plots on top of each other
  #   subplot(p2, p1, nrows = 2, shareX = TRUE) %>%
  #     hide_legend() %>% 
  #     layout(title = "Distribution chart - Histogram and Boxplot",
  #            yaxis = list(title="Frequency"))
  # })
  # 
  
  
  output$population <- renderValueBox({
    Data_1 <- Data_1() %>% 
      filter(Site_Data == "Population (KNBS 2019)") %>% 
      filter(Location == "Total")
    valueBox(paste0(format(sum(Data_1$amount),big.mark=',')), 
                             paste0("Total population in ", input$Sub_county, " selected wards"),
                             color = "green", width = 4, icon = icon("users"))
  })
  
  
  output$household <- renderValueBox({
    Data_1 <- Data_1() %>% 
      filter(Site_Data == "No.of Households (KNBS 2019)") %>% 
      filter(Location == "Total")
    valueBox(paste0(format(sum(Data_1$amount),big.mark=',')), 
                             paste0("Total number of households in ", input$Sub_county, " selected wards"),
                             color = "red", width = 4, icon = icon("home"))
  })
  
  
  output$targeted <- renderValueBox({
    Data_1 <- Data_1() %>% 
      filter(Site_Data == "Target Temasek Households") %>% 
      filter(Location == "Total")
    valueBox(paste0(format(sum(Data_1$amount),big.mark=',')), 
                             paste0("Total targeted households in ", input$Sub_county, " selected wards"),
                             color = "aqua", width = 4, icon = icon("store"))
  })
  
  
  
  output$barChart <- renderPlotly({
    validate(
      need(nrow(Data_1()) > 0, 'There is data for the selected Sub-county since it is not included in the Temasek Pilot Project. 
           To visualize project data, please select one of the following:
           1. Baringo South, 
           2. Mogotio, or 
           3. Baringo North')
    )
    
    Data_1 <- Data_1() %>%
      filter(Sublocation != "Total") %>% 
      filter(Site_Data %in% people_data)
    Inst_Costs <- ggplot(Data_1, aes(Sublocation, amount, fill = Site_Data)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("red", "blue", "green")) +
      theme_bw() +
      scale_y_continuous(labels = comma) +
      geom_text(aes(label = amount),
                position = position_dodge(width = 1),size = 4) +
      facet_wrap(Wards~ ., 
                 scales = "free_x") +
      labs (y = "Counts", title = paste(input$Sub_county,"Selected wards data")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
   
    Inst_Costs %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) 
      # ggplotly(Inst_Costs) %>% style(text = format(Data_1$amount, big.mark = ","), textposition = "top")
  }) 
  
  output$land1 <- renderPlotly({
    validate(
      need(nrow(Data_1()) > 0, 'There is data for the selected Sub-county since it is not included in the Temasek Pilot Project. 
           To visualize project data, please select one of the following:
           1. Baringo South, 
           2. Mogotio, or 
           3. Baringo North')
    )
    
    Data_1 <- Data_1() %>%
      filter(Sublocation != "Total") %>% 
      filter(Site_Data == "Land size (Sq.km)")
    Inst_Costs <- ggplot(Data_1, aes(Sublocation, amount, fill = Site_Data)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("#6E2C00", "blue", "green")) +
      theme_bw() +
      geom_text(aes(label = amount),
                position = position_dodge(width = 1),
                size = 4, 
                color = "red") +
      facet_wrap(~Wards, 
                 scales = "free_x") +
      labs (y = "Area in (Sq.km)", title = "Land size in Sq.km by sublocation") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    Inst_Costs %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text ="")))%>% 
      style(text = format(Data_1$amount, big.mark = ","), textposition = "top")
  }) 
  
    output$lineChart <- renderPlotly({
      validate(
        need(nrow(Data_1()) > 0, 'There is data for the selected Sub-county since it is not included in the Temasek Pilot Project. 
           To visualize project data, please select one of the following:
           1. Baringo South, 
           2. Mogotio, or 
           3. Baringo North')
      )
      
      Data_1 <- Data_1() %>%
        filter(Sublocation != "Total")%>% 
        filter(Site_Data %in% people_data)
      Inst_Costs <- ggplot(Data_1, aes(Sublocation, amount, group = Site_Data)) +
        geom_line(aes(color = Site_Data)) +
        geom_point(aes(col = Site_Data), size =1) +
        scale_color_manual(values = c("red", "steelblue", "green")) +
        theme_bw() +
        geom_text(aes(label = amount),
                  position = position_dodge(width = 1), size = 4) +
        facet_wrap(Wards~ ., 
                   scales = "free_x",
                   ncol = 2, 
                   strip.position = "left") +
        labs (y = "Counts", title = paste(input$Sub_county,"Selected wards data")) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.text.x = element_text(angle = 45, hjust = 1),
              strip.background = element_rect(
                color="black", fill="#F4FC00", size=1.5, linetype="solid"))
      
      Inst_Costs %>%
        ggplotly %>%
        layout(legend = list(orientation = "h",
                             x = 0.1,
                             y = -0.2,
                             title = list(text =""))) %>% 
        style(text = format(Data_1$amount, big.mark = ","), textposition = "top")

    }) 
    
    
    output$popdens1 <- renderPlotly({
      validate(
        need(nrow(Data_1()) > 0, 'There is data for the selected Sub-county since it is not included in the Temasek Pilot Project. 
           To visualize project data, please select one of the following:
           1. Baringo South, 
           2. Mogotio, or 
           3. Baringo North')
      )
      
      Data_1 <- Data_1() %>%
        filter(Sublocation != "Total")%>% 
        filter(Site_Data == "Population Density (KNBS 2019)")
      Inst_Costs <- ggplot(Data_1, aes(Sublocation, amount, group = Site_Data)) +
        geom_line(color = "blue", size = 1) +
        geom_point(col = "#6E2C00", size = 2) +
        theme_bw() +
        geom_text(aes(label = amount),
                  position = position_dodge(width = 1), size = 4) +
        facet_wrap(Wards~ ., 
                   scales = "free_x",
                   ncol = 2, 
                   strip.position = "left") +
        labs (y = "Population Density", title = "Population Density by location") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.text.x = element_text(angle = 45, hjust = 1),
              strip.background = element_rect(
                color="black", fill="#F4FC00", size=1.5, linetype="solid"))
      
      Inst_Costs %>%
        ggplotly %>%
        layout(legend = list(orientation = "h",
                             x = 0.1,
                             y = -0.2,
                             title = list(text ="")))%>% 
        style(text = format(Data_1$amount, big.mark = ","), textposition = "right")
      
    }) 
    
    output$hhdens1 <- renderPlotly({
      validate(
        need(nrow(Data_1()) > 0, 'There is data for the selected Sub-county since it is not included in the Temasek Pilot Project. 
           To visualize project data, please select one of the following:
           1. Baringo South, 
           2. Mogotio, or 
           3. Baringo North')
      )
      
      Data_1 <- Data_1() %>%
        filter(Sublocation != "Total")%>% 
        filter(Site_Data == "Households Density")
      Inst_Costs <- ggplot(Data_1, aes(Sublocation, amount, group = Site_Data)) +
        geom_line(color = "red", size = 1) +
        geom_point(col = "#6E2C00", size = 2) +
        scale_color_manual(values = c("green", "red", "steelblue")) +
        theme_bw() +
        geom_text(aes(label = amount),
                  position = position_dodge(width = 1), size = 4) +
        facet_wrap(Wards~ ., 
                   scales = "free_x",
                   ncol = 2, 
                   strip.position = "left") +
        labs (y = "Households density", title = "Households density by sublocation") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.text.x = element_text(angle = 45, hjust = 1),
              strip.background = element_rect(
                color="black", fill="#F4FC00", size=1.5, linetype="solid"))
      
      Inst_Costs %>%
        ggplotly %>%
        layout(legend = list(orientation = "h",
                             x = 0.1,
                             y = -0.2,
                             title = list(text ="")))%>% 
        style(text = format(Data_1$amount, big.mark = ","), textposition = "right")
      
    }) 
    
    
    
    output$pop <- renderPlotly({
      B_Subcounty$Subcounty <- factor(B_Subcounty$Subcounty, 
                                      levels = unique(B_Subcounty$Subcounty
                                                      [order(B_Subcounty$Numbers, 
                                                             decreasing = T)]))
      B_Subcounty <- B_Subcounty %>% 
        filter(Site_Data == "Population")
      Inst_Costs <- ggplot(B_Subcounty, aes(Subcounty, Numbers)) +
        geom_col(fill = "#FC5000") +
        theme_bw() +
        scale_y_continuous(labels = comma) +
        geom_text(aes(label = Numbers), size = 4) +
        labs (y = "Number of people", title = "Population by Subcounty") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(Inst_Costs) %>%
        style(text = format(B_Subcounty$Numbers, big.mark = ","), textposition = "top")
        
    }) 
    
    
    output$land <- renderPlotly({
      B_Subcounty$Subcounty <- factor(B_Subcounty$Subcounty, 
                                      levels = unique(B_Subcounty$Subcounty
                                                      [order(B_Subcounty$Numbers, 
                                                             decreasing = T)]))
      B_Subcounty <- B_Subcounty %>% 
        filter(Site_Data == "Land Area (Sq.Km)")
      Inst_Costs <- ggplot(B_Subcounty, aes(Subcounty, Numbers)) +
        geom_col(fill = "#26CF00") +
        theme_bw() +
        scale_y_continuous(labels = comma) +
        geom_text(aes(label = Numbers), size = 4) +
        labs (y = "Area in Sq.km", title = "Land size (Sq.Km) by Subcounty") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(Inst_Costs) %>%
        style(text = format(B_Subcounty$Numbers, big.mark = ","), textposition = "top")
      
    }) 
    
    
    output$avghld <- renderPlotly({
      B_Subcounty$Subcounty <- factor(B_Subcounty$Subcounty, 
                                      levels = unique(B_Subcounty$Subcounty
                                                      [order(B_Subcounty$Numbers, 
                                                             decreasing = T)]))
      B_Subcounty <- B_Subcounty %>% 
        filter(Site_Data == "Average Household Size")
      Inst_Costs <- ggplot(B_Subcounty, aes(Subcounty, Numbers)) +
      geom_col(fill = "#0C077F") +
        theme_bw() +
        geom_text(aes(label = Numbers), size = 4, color = "red") +
        labs (y = "Average Household Size", title = "Average Household Size by Subcounty") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(Inst_Costs) %>%
        style(text = format(B_Subcounty$Numbers, big.mark = ","), textposition = "top")
      
      # Inst_Costs <- ggplot(B_Subcounty, aes(Subcounty, Numbers, group = 1)) +
      #   geom_line(color = "#26CF00", size = 1) +
      #   geom_point(col = "#FC5000", size = 2) +
      #   theme_bw() +
      #   labs (y = "Average Household Size", title = "Average Household Size by Subcounty") +
      #   theme(plot.title = element_text(hjust = 0.5), 
      #         axis.text.x = element_text(angle = 45, hjust = 1))
      
    }) 
    
    output$popdens <- renderPlotly({
      B_Subcounty$Subcounty <- factor(B_Subcounty$Subcounty, 
                                      levels = unique(B_Subcounty$Subcounty
                                                      [order(B_Subcounty$Numbers, 
                                                             decreasing = T)]))
      B_Subcounty <- B_Subcounty %>% 
        filter(Site_Data == "Population Density")
      Inst_Costs <- ggplot(B_Subcounty, aes(Subcounty, Numbers, group = 1)) +
        geom_line(color = "#FC5000", size = 1) +
        geom_point(col = "#26CF00", size = 2) +
        theme_bw() +
        geom_text(aes(label = Numbers), size = 4) +
        labs (y = "Population density", title = "Population density by Subcounty") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(Inst_Costs) %>%
        style(text = format(B_Subcounty$Numbers, big.mark = ","), textposition = "top")
      
    }) 
    
    
    output$hhdens2 <- renderPlotly({
      B_Subcounty$Subcounty <- factor(B_Subcounty$Subcounty, 
                                      levels = unique(B_Subcounty$Subcounty
                                                      [order(B_Subcounty$Numbers, 
                                                             decreasing = T)]))
      B_Subcounty <- B_Subcounty %>% 
        filter(Site_Data == "Household Density")
      Inst_Costs <- ggplot(B_Subcounty, aes(Subcounty, Numbers, group = 1)) +
        geom_line(color = "#0C077F", size = 1) +
        geom_point(col = "#26CF00", size = 2) +
        theme_bw() +
        geom_text(aes(label = Numbers), size = 4) +
        labs (y = "Household density", title = "Household density by Subcounty") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(Inst_Costs) %>%
        style(text = format(B_Subcounty$Numbers, big.mark = ","), textposition = "top")
      
    }) 
    
    
    # output$Bs <- renderText(
    #   # "Criteria for selection",
    #   # "In Marigat",
    #   "Criteria for selection,
    #   --------------------------------------------------------------
    #   In Marigat:
    #   1. Estimated 50% of the households in every sublocation selected.
    #   2. Marigat- Physical presence in terms of offices by Self Help Africa (SHA)
    #   3. Do not have a lot of invasive species- Ilchamus excluded
    #   4. 50% - Approx. HH to benefit from the project
    #   --------------------------------------------------------------
    #   In Mochongoi:
    #   1. Sparsely populated area have large farms forming community conservancies – Irong, Kiborgoch,(lower Mochongoi) which forms targets for the project. Each ward has at least a conservancy,
    #   2. 100% households are selected as project beneficiaries due to low population and large land sizes. The HH are also within the conservancies"
    # )
    # 
    # 
    # output$mogotio <- renderText(
    #   # "Criteria for selection",
    #   # "In Marigat",
    #   "Criteria for selection
    #   --------------------------------------------------------------
    #   In Mogotio:
    #   1. Potential private lands for tree growing
    #   2. Areas receives fairly good rainfall and suitable environment for tree growing
    #   3. Potential areas for reseeding 
    #   4. 53% of HH to benefit from the project
    #   --------------------------------------------------------------
    #   In Emining Ward:
    #   1. Potential areas for FMNR, pasture management, reseeding and tree growing,
    #   2. 51% of the total HH to benefit from the project"
    # )
    # 
    # output$BN <- renderText(
    #   # "Criteria for selection",
    #   # "In Marigat",
    #   "Criteria for selection
    #   --------------------------------------------------------------
    #   In Kipsaraman:
    #   1. 60%HH to benefit from the project. Highly populated. Small land sizes, potential for tree growing. Most lands have potential for tree growing
    #   --------------------------------------------------------------
    #   In Kabartonjo:
    #   1. 62%HH to benefit from the project. Highly populated. Small land sizes, potential for tree growing. Most lands have potential for tree growing"
    # )
    
  # ### Bar Charts - State wise trend
  # output$bar <- renderPlotly({
  #   my_data %>% 
  #     plot_ly() %>% 
  #     add_bars(x=~State, y=~get(input$var2)) %>% 
  #     layout(title = paste("Statewise Arrests for", input$var2),
  #            xaxis = list(title = "State"),
  #            yaxis = list(title = paste(input$var2, "Arrests per 100,000 residents") ))
  # })
  # 
  # 
  # ### Scatter Charts 
  # output$scatter <- renderPlotly({
  #   p = my_data %>% 
  #     ggplot(aes(x=get(input$var3), y=get(input$var4))) +
  #     geom_point() +
  #     geom_smooth(method=get(input$fit)) +
  #     labs(title = paste("Relation b/w", input$var3 , "and" , input$var4),
  #          x = input$var3,
  #          y = input$var4) +
  #     theme(  plot.title = element_textbox_simple(size=10,
  #                                                 halign=0.5))
  #   
  #   
  #   # applied ggplot to make it interactive
  #   ggplotly(p)
  #   
  # })
  # 
  # 
  # ## Correlation plot
  # output$cor <- renderPlotly({
  #   my_df <- my_data %>% 
  #     select(-State)
  #   
  #   # Compute a correlation matrix
  #   corr <- round(cor(my_df), 1)
  #   
  #   # Compute a matrix of correlation p-values
  #   p.mat <- cor_pmat(my_df)
  #   
  #   corr.plot <- ggcorrplot(
  #     corr, 
  #     hc.order = TRUE, 
  #     lab= TRUE,
  #     outline.col = "white",
  #     p.mat = p.mat
  #   )
  #   
  #   ggplotly(corr.plot)
  #   
  # })
  # 
  # 
  # # Choropleth map
  # output$map_plot <- renderPlot({
  #   new_join %>% 
  #     ggplot(aes(x=long, y=lat,fill=get(input$crimetype) , group = group)) +
  #     geom_polygon(color="black", size=0.4) +
  #     scale_fill_gradient(low="#73A5C6", high="#001B3A", name = paste(input$crimetype, "Arrest rate")) +
  #     theme_void() +
  #     labs(title = paste("Choropleth map of", input$crimetype , " Arrests per 100,000 residents by state in 1973")) +
  #     theme(
  #       plot.title = element_textbox_simple(face="bold", 
  #                                           size=18,
  #                                           halign=0.5),
  #       
  #       legend.position = c(0.2, 0.1),
  #       legend.direction = "horizontal"
  #       
  #     ) +
  #     geom_text(aes(x=x, y=y, label=abb), size = 4, color="white")
  #   
  #   
  #   
  # })
  
  
  
}
