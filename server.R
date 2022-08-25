function(input, output, session){
  
  datasetOutput <- eventReactive(input$update, {
    switch(input$Subcounty,
           "Baringo" = Baringo_Data,
           "Kitui" = Kitui_Data)
  }, ignoreNULL = FALSE)
  
  
  # Reactive dataset 
  Data_1 <- reactive({
    df <- Baringo_County %>%
      filter(Subcounty %in% input$Sub_county) 
    
  })
  
  Data_2 <- reactive({
    df <- B_Subcounty %>%
      filter(Subcounty %in% input$Sub_county) 
    
  })
  
  
  Data_3 <- reactive({
    df <- Kitui_County %>% 
      filter(Subcounty %in% input$KituiSubcounty)
  })
  
  # Data table Output
  output$dataT <- renderDataTable(datasetOutput())
  
  # Download data table
  output$downloadData <- downloadHandler(
    filename = function(){"Data.csv"},
    content = function(fname){
      write.csv(datasetOutput(), fname)
    }
  )
  
  # Baringo County
  output$population <- renderValueBox({
    Data_1 <- Data_1() %>% 
      filter(Site_Data == "Population (KNBS, 2019)") %>% 
      filter(Location == "Total")
    valueBox(paste0(format(sum(Data_1$amount),big.mark=',')), 
             paste0("Total population in ", input$Sub_county, " selected wards"),
             color = "green", width = 4, icon = icon("users"))
  })
  
  # Baringo County
  output$household <- renderValueBox({
    Data_1 <- Data_1() %>% 
      filter(Site_Data == "No.of Households (KNBS, 2019)") %>% 
      filter(Location == "Total")
    valueBox(paste0(format(sum(Data_1$amount),big.mark=',')), 
             paste0("Total number of households in ", input$Sub_county, " selected wards"),
             color = "red", width = 4, icon = icon("home"))
  })
  
  # Baringo County
  output$targeted <- renderValueBox({
    Data_1 <- Data_1() %>% 
      filter(Site_Data == "Targeted Households") %>% 
      filter(Location == "Total")
    valueBox(paste0(format(sum(Data_1$amount),big.mark=',')), 
             paste0("Total targeted households in ", input$Sub_county, " selected wards"),
             color = "aqua", width = 4, icon = icon("user"))
  })
  
  
  # Baringo County
  output$barChart <- renderPlotly({
    validate(
      need(nrow(Data_1()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
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
                position = position_dodge(width = 1),size = 3) +
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
  
  
  # Baringo County
  output$land1 <- renderPlotly({
    validate(
      need(nrow(Data_1()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
           To visualize project data, please select one of the following:
           1. Baringo South, 
           2. Mogotio, or 
           3. Baringo North')
    )
    
    Data_1 <- Data_1() %>%
      filter(Sublocation != "Total") %>% 
      filter(Site_Data == "Land size (Sq.km) (KNBS, 2019)")
    Inst_Costs <- ggplot(Data_1, aes(Sublocation, amount, fill = Site_Data)) +
      geom_col(position = "dodge", width = .3) +
      scale_fill_manual(values = c("#6E2C00", "blue", "green")) +
      theme_bw() +
      # geom_text(aes(label = amount),
      #           position = position_dodge(width = 1),
      #           size = 4, 
      #           color = "red") +
      # facet_wrap(~Wards, 
      #            scales = "free_x") +
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
  
  
  # Baringo County
  output$lineChart <- renderPlotly({
    validate(
      need(nrow(Data_1()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
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
                position = position_dodge(width = 1), size = 3) +
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
  
  
  # Baringo County
  output$popdens1 <- renderPlotly({
    validate(
      need(nrow(Data_1()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
           To visualize project data, please select one of the following:
           1. Baringo South, 
           2. Mogotio, or 
           3. Baringo North')
    )
    
    Data_1 <- Data_1() %>%
      filter(Sublocation != "Total") %>% 
      filter(Site_Data == "Population density (KNBS, 2019)")
    Inst_Costs <- ggplot(Data_1, aes(Sublocation, amount, fill = Site_Data)) +
      geom_col(position = "dodge", width = .3) +
      scale_fill_manual(values = c("blue", "blue", "green")) +
      theme_bw() +
      # geom_text(aes(label = amount),
      #           position = position_dodge(width = 1),
      #           size = 4, 
      #           color = "red") +
      # facet_wrap(~Wards, 
      #            scales = "free_x") +
      labs (y = "Population Density", title = "Population Density by sublocation") +
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
  
  
  
  
  # output$popdens1 <- renderPlotly({
  #   validate(
  #     need(nrow(Data_1()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
  #          To visualize project data, please select one of the following:
  #          1. Baringo South, 
  #          2. Mogotio, or 
  #          3. Baringo North')
  #   )
  #   
  #   Data_1 <- Data_1() %>%
  #     filter(Sublocation != "Total")%>% 
  #     filter(Site_Data == "Population density (KNBS, 2019)")
  #   Inst_Costs <- ggplot(Data_1, aes(Sublocation, amount, group = Site_Data)) +
  #     geom_line(color = "blue", size = 1) +
  #     geom_point(col = "#6E2C00", size = 2) +
  #     theme_bw() +
  #     geom_text(aes(label = amount),
  #               position = position_dodge(width = 1), size = 4) +
  #     # facet_wrap(Wards~ ., 
  #     #            scales = "free_x",
  #     #            ncol = 2, 
  #     #            strip.position = "left") +
  #     labs (y = "Population Density", title = "Population Density by sublocation") +
  #     theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
  #           axis.text.x = element_text(angle = 45, hjust = 1),
  #           strip.background = element_rect(
  #             color="black", fill="#F4FC00", size=1.5, linetype="solid"))
  #   
  #   Inst_Costs %>%
  #     ggplotly %>%
  #     layout(legend = list(orientation = "h",
  #                          x = 0.1,
  #                          y = -0.2,
  #                          title = list(text ="")))%>% 
  #     style(text = format(Data_1$amount, big.mark = ","), textposition = "right")
  #   
  # }) 
  
  
  # Baringo County
  output$hhdens1 <- renderPlotly({
    validate(
      need(nrow(Data_1()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
           To visualize project data, please select one of the following:
           1. Baringo South, 
           2. Mogotio, or 
           3. Baringo North')
    )
    
    Data_1 <- Data_1() %>%
      filter(Sublocation != "Total") %>% 
      filter(Site_Data == "Households density")
    Inst_Costs <- ggplot(Data_1, aes(Sublocation, amount, fill = Site_Data)) +
      geom_col(position = "dodge", width = .3) +
      scale_fill_manual(values = c("red", "blue", "green")) +
      theme_bw() +
      # geom_text(aes(label = amount),
      #           position = position_dodge(width = 1),
      #           size = 4, 
      #           color = "red") +
      # facet_wrap(~Wards, 
      #            scales = "free_x") +
      labs (y = "Households density", title = "Households density by sublocation") +
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
  
  
  
  # output$hhdens1 <- renderPlotly({
  #   validate(
  #     need(nrow(Data_1()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
  #          To visualize project data, please select one of the following:
  #          1. Baringo South, 
  #          2. Mogotio, or 
  #          3. Baringo North')
  #   )
  #   
  #   Data_1 <- Data_1() %>%
  #     filter(Sublocation != "Total")%>% 
  #     filter(Site_Data == "Households density")
  #   Inst_Costs <- ggplot(Data_1, aes(Sublocation, amount, group = Site_Data)) +
  #     geom_line(color = "red", size = 1) +
  #     geom_point(col = "#6E2C00", size = 2) +
  #     scale_color_manual(values = c("green", "red", "steelblue")) +
  #     theme_bw() +
  #     geom_text(aes(label = amount),
  #               position = position_dodge(width = 1), size = 4) +
  #     # facet_wrap(Wards~ ., 
  #     #            scales = "free_x",
  #     #            ncol = 2, 
  #     #            strip.position = "left") +
  #     labs (y = "Households density", title = "Households density by sublocation") +
  #     theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
  #           axis.text.x = element_text(angle = 45, hjust = 1),
  #           strip.background = element_rect(
  #             color="black", fill="#F4FC00", size=1.5, linetype="solid"))
  #   
  #   Inst_Costs %>%
  #     ggplotly %>%
  #     layout(legend = list(orientation = "h",
  #                          x = 0.1,
  #                          y = -0.2,
  #                          title = list(text ="")))%>% 
  #     style(text = format(Data_1$amount, big.mark = ","), textposition = "right")
  #   
  # }) 
  
  
  # Baringo County
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
      # geom_text(aes(label = Numbers), size = 3) +
      labs (y = "Number of people", title = "Population by Subcounty") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(Inst_Costs) %>%
      style(text = format(B_Subcounty$Numbers, big.mark = ","), textposition = "top")
    
  }) 
  
  # Baringo County
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
      # geom_text(aes(label = Numbers), size = 3) +
      labs (y = "Area in Sq.km", title = "Land size (Sq.Km) by Subcounty") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(Inst_Costs) %>%
      style(text = format(B_Subcounty$Numbers, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Baringo County
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
      # geom_text(aes(label = Numbers), size = 3, color = "red") +
      labs (y = "Average Household Size", title = "Average Household Size by Subcounty") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(Inst_Costs) %>%
      style(text = format(B_Subcounty$Numbers, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Baringo County
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
  
  # Baringo County
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
  
  
  # Kitui County
  output$KituiPop <- renderValueBox({
    Data_3 <- Data_3() %>% 
      filter(Site_Data == "Population (KNBS, 2019)") %>% 
      filter(Location == "Total")
    
    valueBox(
      paste0(format(sum(Data_3$amount), big.mark = ",")),
      paste0("Total population in ", input$KituiSubcounty, " selected wards"),
      color = "green", width = 4, icon = icon("users")
    )
    
  })
  
  # Kitui County
  output$KituiHH <- renderValueBox({
    Data_3 <- Data_3() %>% 
      filter(Site_Data == "No.of Households (KNBS, 2019)") %>% 
      filter(Location == "Total")
    
    valueBox(
      paste0(format(sum(Data_3$amount), big.mark = ",")),
      paste0("Total number of households in ", input$KituiSubcounty, " selected wards"),
      color = "red", width = 4, icon = icon("home")
    )
    
  })
  
  
  # Kitui County
  
  output$kituiTargetedHH <- renderValueBox({
    Data_3 <- Data_3() %>% 
      filter(Site_Data == "Targeted Households") %>% 
      filter(Location == "Total")
    valueBox(paste0(format(sum(Data_3$amount),big.mark=',')), 
             paste0("Total targeted households in ", input$KituiSubcounty, " selected wards"),
             color = "aqua", width = 4, icon = icon("user"))
  })
  
  
  # output$kituiTargetedHH <- renderValueBox({
  #   Data_3 <- Data_3() %>% 
  #     filter(Site_Data == "Land size (Sq.km)") %>% 
  #     filter(Location == "Total")
  #   
  #   valueBox(
  #     paste0(format(sum(Data_3$amount), big.mark = ",")),
  #     paste0("Total land size in Sq.km in ", input$KituiSubcounty, " selected wards"),
  #     color = "aqua", width = 4, icon = icon("store")
  #   )
  #   
  # })
  # 
  
  # Kitui County
  output$Kituibarchart <- renderPlotly({
    validate(
      need(nrow(Data_3()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
           To visualize project data, please select one of the following:
           1. Kitui Rural, or 
           2. Kitui West')
    )
    
    Data_3 <- Data_3() %>%
      filter(Sublocation != "Total") %>% 
      filter(Site_Data %in% people_data)
    Inst_Costs <- ggplot(Data_3, aes(Sublocation, amount, fill = Site_Data)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("red", "blue", "green")) +
      theme_bw() +
      scale_y_continuous(labels = comma) +
      geom_text(aes(label = amount),
                position = position_dodge(width = 1),size = 3) +
      facet_wrap(Wards~ ., 
                 scales = "free_x") +
      labs (y = "Counts", title = paste(input$KituiSubcounty," selected wards data")) +
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
  
  
  # Kitui County
  output$Kituilinechart <- renderPlotly({
    validate(
      need(nrow(Data_3()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
           To visualize project data, please select one of the following:
           1. Kitui Rural, or 
           2. Kitui West')
    )
    
    Data_3 <- Data_3() %>%
      filter(Sublocation != "Total")%>% 
      filter(Site_Data %in% people_data)
    Inst_Costs <- ggplot(Data_3, aes(Sublocation, amount, group = Site_Data)) +
      geom_line(aes(color = Site_Data)) +
      geom_point(aes(col = Site_Data), size =1) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      geom_text(aes(label = amount),
                position = position_dodge(width = 1), size = 3) +
      facet_wrap(Wards~ ., 
                 scales = "free_x",
                 ncol = 2, 
                 strip.position = "left") +
      labs (y = "Counts", title = paste(input$KituiSubcounty," selected wards data")) +
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
      style(text = format(Data_3$amount, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Kitui county
  
  output$kituiLandSize <- renderPlotly({
    validate(
      need(nrow(Data_3()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
           To visualize project data, please select one of the following:
           1. Kitui Rural, or 
           2. Kitui West')
    )
    
    Data_3 <- Data_3() %>%
      filter(Sublocation != "Total") %>% 
      filter(Site_Data == "Land size (Sq.km) (KNBS, 2019)")
    Inst_Costs <- ggplot(Data_3, aes(Sublocation, amount, fill = Site_Data)) +
      geom_col(position = "dodge", width = .3) +
      scale_fill_manual(values = c("#6E2C00", "blue", "green")) +
      theme_bw() +
      # geom_text(aes(label = amount),
      #           position = position_dodge(width = 1),
      #           size = 4, 
      #           color = "red") +
      # facet_wrap(~Wards, 
      #            scales = "free_x") +
      labs (y = "Area in (Sq.km)", title = "Land size in Sq.km by sublocation") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    Inst_Costs %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text ="")))%>% 
      style(text = format(Data_3$amount, big.mark = ","), textposition = "top")
  }) 
  
  
  # 
  # output$kituiLandSize <- renderPlotly({
  #   validate(
  #     need(nrow(Data_3()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
  #          To visualize project data, please select one of the following:
  #          1. Kitui Rural, or 
  #          2. Kitui West')
  #   )
  #   
  #   Data_3 <- Data_3() %>%
  #     filter(Sublocation != "Total") %>% 
  #     filter(Site_Data == "Land size (Sq.km)")
  #   Inst_Costs <- ggplot(Data_3, aes(Sublocation, amount, fill = Site_Data)) +
  #     geom_col(position = "dodge") +
  #     scale_fill_manual(values = c("#6E2C00", "blue", "green")) +
  #     theme_bw() +
  #     facet_wrap(~Wards, 
  #                scales = "free_x") +
  #     labs (y = "Area in (Sq.km)", title = "Land size in Sq.km by sublocation") +
  #     theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
  #           axis.text.x = element_text(angle = 45, hjust = 1))
  #   
  #   Inst_Costs %>%
  #     ggplotly %>%
  #     layout(legend = list(orientation = "h",
  #                          x = 0.1,
  #                          y = -0.2,
  #                          title = list(text ="")))%>% 
  #     style(text = format(Data_3$amount, big.mark = ","), textposition = "top")
  # }) 
  
  
  # Kitui County
  output$kituiPopDensity <- renderPlotly({
    validate(
      need(nrow(Data_3()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
           To visualize project data, please select one of the following:
           1. Kitui Rural, or 
           2. Kitui West')
    )
    
    Data_3 <- Data_3() %>%
      filter(Sublocation != "Total") %>% 
      filter(Site_Data == "Population Density (KNBS, 2019)")
    Inst_Costs <- ggplot(Data_3, aes(Sublocation, amount, fill = Site_Data)) +
      geom_col(position = "dodge", width = .3) +
      scale_fill_manual(values = c("blue", "blue", "green")) +
      theme_bw() +
      # geom_text(aes(label = amount),
      #           position = position_dodge(width = 1),
      #           size = 4, 
      #           color = "red") +
      # facet_wrap(~Wards, 
      #            scales = "free_x") +
      labs (y = "Population Density", title = "Population Density by sublocation") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    Inst_Costs %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text ="")))%>% 
      style(text = format(Data_3$amount, big.mark = ","), textposition = "right")
  }) 
  
  
  
  
  
  
  # output$kituiPopDensity <- renderPlotly({
  #   validate(
  #     need(nrow(Data_3()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
  #          To visualize project data, please select one of the following:
  #          1. Kitui Rural, or 
  #          2. Kitui West')
  #   )
  #   
  #   Data_3 <- Data_3() %>%
  #     filter(Sublocation != "Total")%>% 
  #     filter(Site_Data == "Population Density (KNBS, 2019)")
  #   Inst_Costs <- ggplot(Data_3, aes(Sublocation, amount, group = Site_Data)) +
  #     geom_line(color = "blue", size = 1, show.legend = F) +
  #     geom_point(col = "#6E2C00", size = 2) +
  #     theme_bw() +
  #     geom_text(aes(label = amount),
  #               position = position_dodge(width = 1), size = 4) +
  #     facet_wrap(Wards~ ., 
  #                scales = "free_x",
  #                ncol = 2, 
  #                strip.position = "left") +
  #     labs (y = "Population Density", title = "Population Density by sublocation") +
  #     theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
  #           axis.text.x = element_text(angle = 45, hjust = 1),
  #           strip.background = element_rect(
  #             color="black", fill="#F4FC00", size=1.5, linetype="solid"))
  #   
  #   Inst_Costs %>%
  #     ggplotly %>%
  #     layout(legend = list(orientation = "h",
  #                          x = 0.1,
  #                          y = -0.2,
  #                          title = list(text ="")))%>% 
  #     style(text = format(Data_3$amount, big.mark = ","), textposition = "right")
  #   
  # }) 
  
  
  # Kitui County
  output$KituiHHdensity <- renderPlotly({
    # validate(
    #   need(nrow(Data_3()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
    #        To visualize project data, please select one of the following:
    #        1. Kitui Rural, or 
    #        2. Kitui West')
    # )
    
    Data_3 <- Data_3() %>%
      filter(Sublocation != "Total") %>% 
      filter(Site_Data == "Households density")
    Inst_Costs <- ggplot(Data_3, aes(Sublocation, amount, fill = Site_Data)) +
      geom_col(position = "dodge", width = .3) +
      scale_fill_manual(values = c("red", "blue", "green")) +
      theme_bw() +
      # geom_text(aes(label = amount),
      #           position = position_dodge(width = 1),
      #           size = 4, 
      #           color = "red") +
      # facet_wrap(~Wards, 
      #            scales = "free_x") +
      labs (y = "Households density", title = "Households density by sublocation") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    Inst_Costs %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text ="")))%>% 
      style(text = format(Data_3$amount, big.mark = ","), textposition = "top")
  }) 
  
  
  
  
  # output$KituiHHdensity <- renderPlotly({
  #   validate(
  #     need(nrow(Data_3()) > 0, 'There is no data for the selected Sub-county since it is not included in the Restoring Trees and Livelihoods Pilot Project. 
  #          To visualize project data, please select one of the following:
  #          1. Kitui Rural, or 
  #          2. Kitui West')
  #   )
  #   
  #   Data_3 <- Data_3() %>%
  #     filter(Sublocation != "Total")%>% 
  #     filter(Site_Data == "Households Density")
  #   Inst_Costs <- ggplot(Data_3, aes(Sublocation, amount, fill = Site_Data, group = Site_Data)) +
  #     geom_line(color = "red", size = 1, show.legend = F) +
  #     geom_point(col = "#6E2C00", size = 2) +
  #     theme_bw() +
  #     geom_text(aes(label = amount),
  #               position = position_dodge(width = 1), size = 4) +
  #     facet_wrap(Wards~ ., 
  #                scales = "free_x",
  #                ncol = 2, 
  #                strip.position = "left") +
  #     labs (y = "Households density", title = "Households density by sublocation") +
  #     theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
  #           axis.text.x = element_text(angle = 45, hjust = 1),
  #           strip.background = element_rect(
  #             color="black", fill="#F4FC00", size=1.5, linetype="solid"))
  #   
  #   Inst_Costs %>%
  #     ggplotly %>%
  #     layout(legend = list(orientation = "h",
  #                          x = 0.1,
  #                          y = -0.2,
  #                          title = list(text ="")))%>% 
  #     style(text = format(Data_3$amount, big.mark = ","), textposition = "right")
  #   
  # }) 
  
  
  # Kitui County
  output$Kituipop <- renderPlotly({
    K_Subcounty$Subcounty <- factor(K_Subcounty$Subcounty, 
                                    levels = unique(K_Subcounty$Subcounty
                                                    [order(K_Subcounty$Numbers, 
                                                           decreasing = T)]))
    K_Subcounty <- K_Subcounty %>% 
      filter(Site_Data == "Population")
    Inst_Costs <- ggplot(K_Subcounty, aes(Subcounty, Numbers)) +
      geom_col(fill = "#FC5000") +
      theme_bw() +
      scale_y_continuous(labels = comma) +
      # geom_text(aes(label = Numbers), size = 3) +
      labs (y = "Number of people", title = "Population by Subcounty") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(Inst_Costs) %>%
      style(text = format(K_Subcounty$Numbers, big.mark = ","), textposition = "top")
    
  }) 
  
  
  
  # Kitui County
  output$Kituiland <- renderPlotly({
    K_Subcounty$Subcounty <- factor(K_Subcounty$Subcounty, 
                                    levels = unique(K_Subcounty$Subcounty
                                                    [order(K_Subcounty$Numbers, 
                                                           decreasing = T)]))
    K_Subcounty <- K_Subcounty %>% 
      filter(Site_Data == "Land Area (Sq.Km)")
    Inst_Costs <- ggplot(K_Subcounty, aes(Subcounty, Numbers)) +
      geom_col(fill = "#26CF00") +
      theme_bw() +
      scale_y_continuous(labels = comma) +
      # geom_text(aes(label = Numbers), size = 3) +
      labs (y = "Area in Sq.km", title = "Land size (Sq.Km) by Subcounty") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(Inst_Costs) %>%
      style(text = format(K_Subcounty$Numbers, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Kitui County
  output$Kituiavghld <- renderPlotly({
    K_Subcounty$Subcounty <- factor(K_Subcounty$Subcounty, 
                                    levels = unique(K_Subcounty$Subcounty
                                                    [order(K_Subcounty$Numbers, 
                                                           decreasing = T)]))
    K_Subcounty <- K_Subcounty %>% 
      filter(Site_Data == "Average Household Size")
    Inst_Costs <- ggplot(K_Subcounty, aes(Subcounty, Numbers)) +
      geom_col(fill = "#0C077F") +
      theme_bw() +
      # geom_text(aes(label = Numbers), size = 3, color = "red") +
      labs (y = "Average Household Size", title = "Average Household Size by Subcounty") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(Inst_Costs) %>%
      style(text = format(K_Subcounty$Numbers, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Kitui County
  output$Kituipopdens <- renderPlotly({
    K_Subcounty$Subcounty <- factor(K_Subcounty$Subcounty, 
                                    levels = unique(K_Subcounty$Subcounty
                                                    [order(K_Subcounty$Numbers, 
                                                           decreasing = T)]))
    K_Subcounty <- K_Subcounty %>% 
      filter(Site_Data == "Population Density")
    Inst_Costs <- ggplot(K_Subcounty, aes(Subcounty, Numbers, group = 1)) +
      geom_line(color = "#FC5000", size = 1) +
      geom_point(col = "#26CF00", size = 2) +
      theme_bw() +
      geom_text(aes(label = Numbers), size = 4) +
      labs (y = "Population density", title = "Population density by Subcounty") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(Inst_Costs) %>%
      style(text = format(K_Subcounty$Numbers, big.mark = ","), textposition = "top")
    
  }) 
  
  # Kitui County
  output$Kituihhdens2 <- renderPlotly({
    K_Subcounty$Subcounty <- factor(K_Subcounty$Subcounty, 
                                    levels = unique(K_Subcounty$Subcounty
                                                    [order(K_Subcounty$Numbers, 
                                                           decreasing = T)]))
    K_Subcounty <- K_Subcounty %>% 
      filter(Site_Data == "Household Density")
    Inst_Costs <- ggplot(K_Subcounty, aes(Subcounty, Numbers, group = 1)) +
      geom_line(color = "#0C077F", size = 1) +
      geom_point(col = "#26CF00", size = 2) +
      theme_bw() +
      geom_text(aes(label = Numbers), size = 4) +
      labs (y = "Household density", title = "Household density by Subcounty") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(Inst_Costs) %>%
      style(text = format(K_Subcounty$Numbers, big.mark = ","), textposition = "top")
    
  }) 
  
}
