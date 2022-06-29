
ui <- dashboardPage(
  dashboardHeader(title="Restoring Trees and Livelihoods Pilot: Baringo & Kitui", titleWidth = 650,
                  tags$li(class="dropdown",tags$a(href="https://www.worldagroforestry.org/", 
                                                  icon("fas fa-caret-up"), "Center for International Forestry Research(CIFOR) - International Centre for Research in Agroforestry(ICRAF) Website", 
                                                  target="_blank"))  
  ),
  
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar", 
                
                style = "position: fixed;overflow: visible; color: #FFF; width: 220px; white-space: nowrap;",
                
                menuItem(
                  "Instructions & Introduction", 
                  tabName = "Intro",
                  icon = icon("info")
                ),
                
                
                menuItem(
                  "Pilot Dashboard", 
                  tabName = "viz", 
                  icon=icon("chart-line")
                ),
                
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'Sublocation'", 
                                 h4("Baringo Sites"),
                                 selectInput("Sub_county",
                                             "Select Sub-county",
                                             unique(B_Subcounty$Subcounty),
                                             selected = "Baringo North")
                ),
                
                
                conditionalPanel("input.sidebar == 'viz'  && input.t2 == 'Sublocation'", 
                                 h4("Change Chart Type"),
                                 selectInput("Chart",
                                             "Select Chart Type",
                                             choices = c("Line chart",
                                                         "Bar chart"),
                                             selected = "Bar chart")
                ),
                
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'KituiSublocations'", 
                                 h4("Kitui Sites"),
                                 selectInput("KituiSubcounty",
                                             "Select Sub-county",
                                             unique(Kitui_County$Subcounty),
                                             selected = "Kitui Rural Sub-county")
                ),
                
                conditionalPanel("input.sidebar == 'viz'  && input.t2 == 'KituiSublocations'", 
                                 h4("Change Chart Type"),
                                 selectInput("KituiChart",
                                             "Select Chart Type",
                                             choices = c("Line chart",
                                                         "Bar chart"),
                                             selected = "Line chart")
                ),
                
                div(class = "sticky_footer", sidebarUserPanel("Developers:",
                                                              subtitle = "Victor Mutugi, Dr. Brian Chiputwa"
                ))
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
                       tabPanel("About Baringo", icon=icon("address-card"),
                                
                                fluidRow(
                                  column(width = 6, style = "background-color: #FF3E00; color: white",
                                         h2(HTML("<strong>Overview</strong>")),
                                         h4(tags$p("This dashboard is for the RESTORING TREES and LIVELIHOODS PROJECT (Baringo) as a 
                                            guide for site level biophysical baseline preliminary information.")),
                                         h4(HTML("<strong>How to navigate the dashboard</strong>"), style = "background-color: black;", align = "center"),
                                         h4(tags$p("The dashboard has two parts; the sidebar on the left (dark area), and the main panel (white area)")),
                                         h4(tags$p("•	To visualize the dataset used in the dashboard graphics, click on the 'Data' Tab above.")),
                                         h4(tags$p("•	To visualize the dashboard, click on the 'Temasek Pilot Dashboard' Tab on the sidebar.")),
                                         tags$br()
                                  ),
                                  column(width = 6, style = "background-color: white",
                                         h2(HTML("<strong>About Restoring Trees and Livelihoods Pilot: Baringo</strong>")),
                                         h4(tags$p("Baringo county in Kenya has six sub-counties, i.e.,
                                                    Baringo Central, Baringo North, Baringo South, Eldama Ravine, Mogotio, and Tiaty.")),
                                         h4(HTML("<strong>Baringo selected sites for Restoring Trees and Livelihoods Pilot</strong>")),
                                         h4(tags$p("Six wards in three subcounties in Baringo have been selected for the Project. These are:")),
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
                                             h2("Baringo County Maps"))
                                  )
                                ),
                                
                                fluidRow(
                                  column(width = 6, 
                                         tags$img(src="baringo_s2_NDVI_plot_1 (1).png", width = "100%" , 
                                                  height = 550),
                                         tags$br(), 
                                         tags$a("Map of Baringo county selected wards"), 
                                         align = "center"
                                  ),
                                  column(width = 6, style = 'border-left: 1px solid',
                                         tags$img(src="Baringo_NDVI_plot2.png", width = "100%" , 
                                                  height = 550),
                                         tags$br(), 
                                         tags$a("Vegetation map of Baringo county selected wards"), 
                                         align = "center"
                                  )
                                ), 
                                
                                tags$br(), 
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12, align = "center", style = "background-color: #17202A; color: white; border: 4px solid #909497; border-radius: 25px",
                                             h2("Baringo County Conservancies"))
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
                                
                       ),
                       
                       #Kitui County
                       tabPanel( title = "About Kitui", icon = icon("address-card"),
                                 fluidRow(
                                   column(width = 6, style = "background-color: #FF3E00; color: white",
                                          h2(HTML("<strong>Overview</strong>")),
                                          h4(tags$p("This dashboard is for the RESTORING TREES and LIVELIHOODS PROJECT (Kitui) as a 
                                            guide for site level biophysical baseline preliminary information.")),
                                          h4(HTML("<strong>How to navigate the dashboard</strong>"), style = "background-color: black;", align = "center"),
                                          h4(tags$p("The dashboard has two parts; the sidebar on the left (dark area), and the main panel (white area)")),
                                          h4(tags$p("•	To visualize the dataset used in the dashboard graphics, click on the 'Data' Tab above.")),
                                          h4(tags$p("•	To visualize the dashboard, click on the 'Pilot Dashboard' Tab on the sidebar.")),
                                          tags$br()
                                   ),
                                   column(width = 6, style = "background-color: white;",
                                          h2(HTML("<strong>About Restoring Trees and Livelihoods Pilot: Kitui</strong>")),
                                          h4(tags$p("Kitui county in Kenya has eight sub-counties, i.e.,
                                                    Kitui Central, Kitui East, Kitui Rural, Kitui South, Kitui West, Mwingi East, Mwingi North, and Mwingi West.")),
                                          h4(HTML("<strong>Kitui selected sites for Restoring Trees and Livelihoods Pilot</strong>")),
                                          h4(tags$p("Eight wards in two subcounties in Kitui have been selected for the Project. These are:")),
                                          h4(tags$p("•	Kitui Rural : (Yatta - Kwa Vonza ward, Kanyangi ward, Mbitini ward and Kisasi ward)")),
                                          h4(tags$p("•	Kitui West : (Mutonguni ward, Kauwi ward, Matinyani ward and Kwa mutonga ward), and ")),
                                          tags$br(),
                                          tags$br()
                                   )
                                 )
                       ),
                       
                       tabPanel(title = "Data", icon = icon("table"),
                                fluidRow(
                                  column(width = 12,
                                         h4("Subcounty"),
                                         selectInput("Subcounty",
                                                     "Select Subcounty",
                                                     choices = c("Baringo",
                                                                 "Kitui"),
                                                     selected = "Kitui"),
                                         
                                         actionButton("update", "Update View"),
                                         
                                         downloadButton("downloadData", "Download data table"),
                                         dataTableOutput("dataT", width = "auto", height = "auto")
                                  )
                                )
                       ) 
                )
                
        ),
        
        # Second Tab Item
        tabItem(tabName = "viz", 
                tabBox(id="t2",  width=12,
                       
                       # Baringo County
                       tabPanel("Baringo Pilot Sublocations", value="Sublocation",
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
                                             h2("Additional Sites' Information"))
                                  )
                                ),
                                
                                withSpinner(plotlyOutput("land1", height = "650px")),
                                
                                withSpinner(plotlyOutput("popdens1", height = "650px")),
                                
                                withSpinner(plotlyOutput("hhdens1", height = "650px")),
                                
                       ),
                       
                       
                       tabPanel("Baringo County Data", value="distro",
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
                       ),
                       
                       
                       # Kitui County
                       tabPanel("Kitu Pilot Sublocations", value = "KituiSublocations", height = "auto",
                                fluidRow(
                                  valueBoxOutput("KituiPop"),
                                  valueBoxOutput("KituiHH"),
                                  valueBoxOutput("kituiTargetedHH")
                                ),
                                
                                conditionalPanel(
                                  condition = "input.KituiChart == 'Bar chart'",
                                  withSpinner(plotlyOutput("Kituibarchart", height = "790px"))
                                ),
                                
                                conditionalPanel(
                                  condition = "input.KituiChart == 'Line chart'",
                                  withSpinner(plotlyOutput("Kituilinechart", height = "790px"))
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12, align = "center", style = "background-color: #17202A; color: white; border: 4px solid #909497; border-radius: 25px",
                                             h2("Additional Sites' Information"))
                                  )
                                ),
                                
                                withSpinner(plotlyOutput("kituiLandSize", height = "790px")),
                                fluidRow(box(width = 12,
                                             column(width = 12)
                                )
                                ),
                                withSpinner(plotlyOutput("kituiPopDensity", height = "790px")),
                                tags$br(),
                                withSpinner(plotlyOutput("KituiHHdensity", height = "790px")),
                                tags$br()
                       ),
                       
                       tabPanel("Kitui County Data", value = "KituiCounty",
                                fluidRow(
                                  box(width = 12,
                                      column(width = 12, align = "center", style = "background-color: #17202A; color: white; border: 4px solid #909497; border-radius: 25px",
                                             h2("Kenya National Bureau Of Statisticts Data"))
                                  )
                                ),
                                
                                
                                fluidRow(
                                  column(width = 4,
                                         withSpinner(plotlyOutput("Kituipop", height = "650px"))
                                  ),
                                  column(width = 4,
                                         withSpinner(plotlyOutput("Kituiland", height = "650px")),
                                  ),
                                  column(width = 4,
                                         withSpinner(plotlyOutput("Kituiavghld", height = "650px"))
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
                                         withSpinner(plotlyOutput("Kituipopdens", height = "650px"))
                                  ),
                                  column(width = 6,
                                         withSpinner(plotlyOutput("Kituihhdens2", height = "650px"))
                                  )
                                )
                       )
                ),     
        )
      )
    )
  ),
  
  footer = dashboardFooter(
    left = tags$a(href="https://www.knbs.or.ke/?s=2019+Kenya+Population+and+Housing+Census+", icon("chart-line"), "Go to KNBS: data source", target="_blank"),
    right = tags$a(href="https://1drv.ms/w/s!Ag-4_r8pZseykUViy0dnOlI8i59i?e=h9szZ1", icon("chart-line"), "Go to Project: data source", target="_blank")
  )
)