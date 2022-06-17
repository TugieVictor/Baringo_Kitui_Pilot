## Shiny UI component for the Dashboard

ui <- dashboardPage(
  
 # tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
  
  dashboardHeader(title="Temasek Pilot Baringo Sites Data", titleWidth = 650,
                  tags$li(class="dropdown",tags$a(href="https://www.worldagroforestry.org/", 
                                                  icon("chart-pie"), "CIFOR-ICRAF Website", 
                                                  target="_blank"))
                  # tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/abhinav-agrawal-pmp%C2%AE-safe%C2%AE-5-agilist-csm%C2%AE-5720309" ,icon("linkedin"), "My Profile", target="_blank")),
                  # tags$li(class="dropdown",tags$a(href="https://github.com/aagarw30/R-Shiny-Dashboards/tree/main/USArrestDashboard", icon("github"), "Source Code", target="_blank"))
  ),
  
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar", 
                
                style = "position: fixed;overflow: visible; color: #FFF; width: 220px; white-space: nowrap;",
                
                menuItem(
                  "Instructions & Introduction", 
                  tabName = "Intro",
                  icon = icon("info")
                ),
                
                # menuItem(
                #   "Dataset", 
                #   tabName = "data", 
                #   icon = icon("database")
                #   ),
                
                menuItem(
                  "Temasek Pilot Dashboard", 
                  tabName = "viz", 
                  icon=icon("chart-line")
                  ),
                
                # Conditional Panel for conditional widget appearance
                # Filter should appear only for the visualization menu and selected tabs within it
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'Sublocation'", h3("Temasek Sites"),
                                 selectInput("Sub_county",
                                              "Select Sub-county",
                                               unique(B_Subcounty$Subcounty),
                                              selected = "Baringo North")
                                 ),
                conditionalPanel("input.sidebar == 'viz'  && input.t2 == 'Sublocation'", h3("Change Chart Type"),
                                 selectInput("Chart",
                                             "Select Chart Type",
                                             choices = c("Line chart",
                                                         "Bar chart"),
                                             selected = "Bar chart")
                                 ),
                
                div(class = "sticky_footer", p("Developer: Victor & Dr. Brian"))
                # conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends' ",
                #                  selectInput(inputId = "var2" , label ="Select the Arrest type" , choices = ""))
                # conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", 
                #                  selectInput(inputId = "var3" , label ="Select the X variable" , choices = c1, selected = "Rape")),
                # conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ",
                #                  selectInput(inputId = "var4" , label ="Select the Y variable" , choices = c1, selected = "Assault")),
                # menuItem("Choropleth Map", tabName = "map", icon=icon("map"))
                # 
    )
  ),
  
  
  dashboardBody( 
    tags$script(
      HTML("$('body').addClass('fixed');")
      ),
                 
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 20px;
                              }
                              ')),
              tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
              ),
    
    
    fluidRow(
      tabItems(
      ## First tab item
        tabItem(tabName = "Intro", 
                tabBox(id="t1", width = 12, 
                       tabPanel("About", icon=icon("address-card"),
                                
                                fluidRow(
                                  column(width = 6, style = "background-color: #FF3E00; color: white",
                                    h2(HTML("<strong>Overview</strong>")),
                                  h3(tags$p("This dashboard is for the TEMASEK PILOT PROJECT (Baringo) as a 
                                            guide for site level biophysical baseline preliminary information.")),
                                  h2(HTML("<strong>How to navigate the dashboard</strong>")),
                                  h4(tags$p("The dashboard has two parts; the sidebar on the left (dark area), and the main panel (white area)")),
                                  h4(tags$p("•	To visualize the dataset used in the dashboard graphics, click on the 'Data' Tab above.")),
                                  h4(tags$p("•	To visualize the dashboard, click on the 'Temasek Pilot Dashboard' Tab on the sidebar.")),
                                  tags$br()
                                  ),
                                  column(width = 6, style = "background-color: white",
                                         h2(HTML("<strong>About Temasek Pilot in Baringo</strong>")),
                                         h3(tags$p("Baringo county in Kenya has six sub-counties, i.e.,
                                                    Baringo Central, Baringo North, Baringo South, Eldama Ravine, Mogotio, and Tiaty.")),
                                         h4(HTML("<strong>Temasek pilot selected sites in Baringo</strong>")),
                                         h4(tags$p("Six wards in three subcounties in Baringo have been selected for the 
                                                   TEMASEK PILOT PROJECT. These are:")),
                                         h4(tags$p("•	Baringo North : (Kipsaraman ward and Kabartonjo ward)")),
                                         h4(tags$p("•	Baringo South : (Marigat ward and Mochongoi ward), and ")),
                                         h4(tags$p("•	Mogotio : (Mogotio ward and Emining ward)")),
                                         tags$br(),
                                         tags$br()
                                  )
                                ),
                                
                                tags$br(), 
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12, align = "center", style = "background-color: #17202A; color: white; border: 4px solid #909497; border-radius: 25px",
                                             h2("Temasek Pilot Baringo County Maps"))
                                      )
                                ),
                                
                                fluidRow(
                                  column(width = 6, 
                                         tags$img(src="baringo_s2_NDVI_plot_1 (1).png", width = "100%" , 
                                                  height = 550),
                                         tags$br(), 
                                         tags$a("Map of Baringo county Temasek Pilot wards"), 
                                         align = "center"
                                         ),
                                  column(width = 6, style = 'border-left: 1px solid',
                                         tags$img(src="Baringo_NDVI_plot2.png", width = "100%" , 
                                                  height = 550),
                                         tags$br(), 
                                         tags$a("Vegetation map of Baringo county Temasek Pilot wards"), 
                                         align = "center"
                                  )
                                ), # end of fluidrow
                                
                                tags$br(), 
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12, align = "center", style = "background-color: #17202A; color: white; border: 4px solid #909497; border-radius: 25px",
                                             h2("Temasek Pilot Baringo County Conservancies"))
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12,
                                        tags$img(src="Conservancy1.png", width = "100%", height = 550)
                                        )
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12,
                                             tags$img(src="Conservancy2.png", width = "100%", height = 550)
                                      )
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12,
                                             tags$img(src="Conservancy3.png", width = "100%", height = 550)
                                      )
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12,
                                             tags$img(src="Conservancy4.png", width = "100%", height = 550)
                                      )
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12,
                                             tags$img(src="Conservancy5.png", width = "100%", height = 550)
                                      )
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12,
                                             tags$img(src="Conservancy6.png", width = "100%", height = 550)
                                      )
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12,
                                             tags$img(src="Conservancy7.png", width = "100%", height = 550)
                                      )
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12,
                                             tags$img(src="Conservancy8.png", width = "100%", height = 550)
                                      )
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12,
                                             tags$img(src="Conservancy9.png", width = "100%", height = 550)
                                      )
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12,
                                             tags$img(src="Conservancy10.png", width = "100%", height = 550)
                                      )
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12,
                                             tags$img(src="Conservancy11.png", width = "100%", height = 550)
                                      )
                                  )
                                )
                                
                       ), #end of tabpanel
                       tabPanel(title = "Data", icon = icon("table"),
                                fluidRow(
                                   column(width = 12,
                                      dataTableOutput("dataT", width = "auto", height = "auto")
                                      )
                                  )
                                ) 
                       # tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted")),
                       # tabPanel("Summary Stats", verbatimTextOutput("summary"), icon=icon("chart-pie"))
                )
                
        ),
     
      # Second Tab Item
      tabItem(tabName = "viz", 
              tabBox(id="t2",  width=12, 
                     tabPanel("Temasek Pilot Sublocations", value="Sublocation",
                              # fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                              #          tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                              #          
                              # ),
                              # withSpinner(plotlyOutput("bar"))
                              # withSpinner(plotlyOutput("lineChart", height = "650px")))
                              
                              fluidRow(
                                shinydashboard::valueBoxOutput("population"),
                                shinydashboard::valueBoxOutput("household"),
                                shinydashboard::valueBoxOutput("targeted")
                              ),
                              
                              conditionalPanel(
                                condition = "input.Sub_county == 'Mogotio'",
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12, align = "center", style = "background-color: #FF3E00;",
                                             h2("Criteria for site selection")
                                             ),
                                      column(width = 6,
                                             tags$p(h4("Emining Ward:"), 
                                                    tags$br(), "1. Potential areas for FMNR, pasture management, reseeding and tree growing",
                                                    tags$br(), "2. 51% of the total HH to benefit from the project"
                                             )
                                      ),
                                      column(width = 6,
                                             # h3("Criteria for selection"),
                                             tags$p(h4("Mogotio Ward:"), 
                                                    tags$br(), "1. Potential private lands for tree growing",
                                                    tags$br(), "2. Areas receives fairly good rainfall and suitable environment for tree growing",
                                                    tags$br(), "3. Potential areas for reseeding",
                                                    tags$br(), "4. 53% of HH to benefit from the project"
                                             )
                                      )
                                  )
                                )
                              ),
                              conditionalPanel(
                                condition = "input.Sub_county == 'Baringo North'",
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12, align = "center", style = "background-color: #FF3E00;",
                                             h2("Criteria for site selection")),
                                      column(width = 6,
                                             tags$p(h4("Kabartonjo Ward:"), 
                                                    tags$br(), "1. 62%HH to benefit from the project. Highly populated. Small land sizes, potential for tree growing. Most lands have potential for tree growing"
                                             )
                                      ),
                                      column(width = 6,
                                             # h3("Criteria for selection"),
                                             tags$p(h4("Kipsaraman Ward:"), 
                                                    tags$br(), "1. 60%HH to benefit from the project. Highly populated. Small land sizes, potential for tree growing. Most lands have potential for tree growing"
                                             )
                                      )
                                  )
                                )
                              ),
                              conditionalPanel(
                                condition = "input.Sub_county == 'Baringo South'",
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12, align = "center", style = "background-color: #FF3E00;",
                                             h2("Criteria for site selection")),
                                      column(width = 6,
                                             tags$p(h4("Marigat Ward:"), 
                                                    tags$br(), "1. Estimated 50% of the households in every sublocation selected.",
                                                    tags$br(), "2. Marigat- Physical presence in terms of offices by Self Help Africa (SHA)",
                                                    tags$br(), "3. Do not have a lot of invasive species- Ilchamus excluded",
                                                    tags$br(), "4. 50% - Approx. HH to benefit from the project"
                                             )
                                      ),
                                      column(width = 6,
                                             # h3("Criteria for selection"),
                                             tags$p(h4("Mochongoi Ward:"), 
                                                    tags$br(), "1. Sparsely populated area have large farms forming community conservancies – Irong, Kiborgoch,(lower Mochongoi) which forms targets for the project. Each ward has at least a conservancy",
                                                    tags$br(), "2. 100% households are selected as project beneficiaries due to low population and large land sizes. The HH are also within the conservancies"
                                             )
                                      )
                                  )
                                )
                              ),
                              
                              
                              conditionalPanel(
                                condition = "input.Chart=='Bar chart'",
                                withSpinner(plotlyOutput("barChart", height = "650px"))
                              ),
                              
                              conditionalPanel(
                                condition = "input.Chart=='Line chart'",
                                withSpinner(plotlyOutput("lineChart", height = "650px"))
                              ),
                              
                              fluidRow(
                                box(width = 12,
                                    column(width = 12, align = "center", style = "background-color: #17202A; color: white; border: 4px solid #909497; border-radius: 25px",
                                           h2("Additional Temasek Sites' Information"))
                                )
                              ),
                              
                              withSpinner(plotlyOutput("land1", height = "650px")),
                              
                              withSpinner(plotlyOutput("popdens1", height = "650px")),
                              
                              withSpinner(plotlyOutput("hhdens1", height = "650px")),
                     
                     #    conditionalPanel(
                     #      condition = "input.Sub_county == 'Baringo North'",
                     #      verbatimTextOutput("BN")
                     #    ),
                     # conditionalPanel(
                     #   condition = "input.Sub_county == 'Baringo South'",
                     #   verbatimTextOutput("Bs")
                     # ),
                     # conditionalPanel(
                     #   condition = "input.Sub_county == 'Mogotio'",
                     #   verbatimTextOutput("mogotio")
                     # ),
                     
                     ),
                     tabPanel("Baringo County Data", value="distro",
                                       # selectInput("var", "Select the variable", choices = unique(B_Subcounty$Subcounty)),
                              fluidRow(
                                box(width = 12,
                                    column(width = 12, align = "center", style = "background-color: #17202A; color: white; border: 4px solid #909497; border-radius: 25px",
                                           h2("Kenya National Bureau Of Statisticts Data"))
                                )
                              ),
                              
                              
                              fluidRow(
                                         column(width = 4,
                                         withSpinner(plotlyOutput("pop", height = "650px"))
                                         ),
                                         column(width = 4,
                                           withSpinner(plotlyOutput("land", height = "650px")),
                                         ),
                                         column(width = 4,
                                           withSpinner(plotlyOutput("avghld", height = "650px"))
                                          )
                              ),
                              
                              
                              fluidRow(
                                box(width = 12,
                                    column(width = 12, align = "center", style = "background-color: #17202A; color: white; border: 4px solid #909497; border-radius: 25px",
                                           h2("Inhouse Calculated Data"))
                                )
                              ),
                              
                              
                              fluidRow(
                                column(width = 6,
                                       withSpinner(plotlyOutput("popdens", height = "650px"))),
                                column(width = 6,
                                       withSpinner(plotlyOutput("hhdens2", height = "650px"))
                                )
                              )
                      )
              ),
                     # tabPanel("Distribution", value="distro",
                     #          # selectInput("var", "Select the variable", choices = unique(B_Subcounty$Subcounty)),
                     #          withSpinner(plotlyOutput("histplot", height = "350px"))),
              #        tabPanel("Correlation Matrix", id="corr" , withSpinner(plotlyOutput("cor"))),
              #        tabPanel("Relationship among Arrest types & Urban Population", 
              #                 radioButtons(inputId ="fit" , label = "Select smooth method" , choices = c("loess", "lm"), selected = "lm" , inline = TRUE), 
              #                 withSpinner(plotlyOutput("scatter")), value="relation"),
              #        side = "left"
              # ),
              
      )
      
      
      # Third Tab Item
      # tabItem(
      #   tabName = "map",
      #   box(      selectInput("crimetype","Select Arrest Type", choices = "", selected="Rape", width = 250),
      #             withSpinner(plotOutput("map_plot")), width = 12)
      #   
      #   
      #   
      # )
      # 
    )
    )
  ),
  
  footer = dashboardFooter(
    left = tags$a(href="https://www.knbs.or.ke/?s=2019+Kenya+Population+and+Housing+Census+", icon("cogs"), "Go to Data source: KNBS ", target="_blank"),
    right = tags$a(href="https://1drv.ms/w/s!Ag-4_r8pZseykUViy0dnOlI8i59i?e=h9szZ1", icon("cogs"), "Go to Data source: Temasek", target="_blank")
  )
)


# dashboardFooter(href, src, label = "", width = "100%",
#                 height = "160px", italic = TRUE, bold = TRUE,
#                 style = "text-align:center; align: center; padding: 0px; margin: 0px;")



# tags$footer("My footer", align = "center", style = "
#               position:absolute;
#               bottom:0;
#               width:100%;
#               height:50px;   /* Height of the footer */
#               color: white;
#               padding: 10px;
#               background-color: black;
#               z-index: 1000;")