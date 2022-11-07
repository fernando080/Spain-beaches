path = "env.R"
source(path, encoding = "UTF-8")

ui <- fluidPage(tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                              background-color: #9c4242 !important;
                              } "))),
                tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                                 color: #268BD2; !important
                                }")
                ),
                theme=shinythemes::shinytheme(theme = "superhero"),
                includeCSS("www/styles.css"),
                
                # titlePanel 
                titlePanel(strong("Visualization of the Information, Storyboard",
                                  style="color: #268BD2;font-weight: bold;"),
                           windowTitle = "Visualization of the Information, Storyboard"),
                
                # navlistPanel
                navlistPanel(widths=c(2,10),
                             # tabPanel: Información
                             tabPanel("Home",icon = icon("home"),
                                      markdown('## <span style="color:white">**Shiny Storyboard, Beaches of Spain**</span>'),
                                      
                                      fluidRow(column(width = 4),
                                               column(width = 4, 
                                                      div(class="center",
                                                          img(src="https://img.icons8.com/bubbles/2x/4a90e2/beach.png", height = '250px', width = '250px')
                                                      )),
                                               column(width = 4)                                               
                                               ),
                                      br(),
                                      
                                      leafletOutput("mymap"),
                                      br(),br(),
                                      hr(),
                                      
                                      h3("Objetive:", class="estiloh3"),
                                      div(HTML(var_texto)),
                                      br(),
                                      
                                      h3("Author:", class="estiloh3"),
                                      div(HTML(var_texto_auth)),
                                      br(),
                                      
                                      h3("Summary of the Task:", class="estiloh3"),
                                      div(HTML(var_texto_mitfm)),
                                      
                                      br(),br(),br()
                             ),
                             # tabPanel: Story Data -----
                             tabPanel("Beaches Storyboard",icon = icon("umbrella-beach"),
                                      markdown('## <span style="color:white">**Beaches Storyboard**</span>'),
                                      
                                      br(),
                                      
                                      tabsetPanel(
                                          # tabPanel: Hospitals and Sea Sport Centers ----
                                          tabPanel("Hospitals and Sea Sport Centers", icon = icon("dove"),
                                                   br(),
                                                   
                                                   # Output: Hopital per province and CCAA image
                                                   fluidRow(column(width = 9, plotOutput("Hospi")),
                                                            column(width = 3, 
                                                                   div(HTML('<i class="fas fa-thumbs-up" style = "color:#25FC17;"></i> <span class = "imp-text">Melilla, Cueta, Barcelona, Malaga</span>')), 
                                                                   div(HTML('<i class="fas fa-thumbs-down" style = "color:#EF0808;"></i> <span class = "imp-text">Granada, Canarias, Tarragona, Bizkaia, Murcia</span>')), 
                                                                   br(),
                                                                   div(HTML((var_text_hosp))))),
                                                   
                                                   br(),br(),
                                                   
                                                   # Output: Dep image
                                                   fluidRow(column(width = 9, plotOutput("Dep")),
                                                            column(width = 3, 
                                                                   div(HTML('<i class="fas fa-thumbs-up" style = "color:#25FC17;"></i> <span class = "imp-text">Short distances in general</span>')), 
                                                                   br(),
                                                                   div(HTML((var_text_dep))))),
                                                   
                                                   br(),br(),br()
                                          ),
                                          
                                          # tabPanel: Parking, Urbanization and Occupation ----
                                          tabPanel("Parking, Urbanization and Occupation", icon = icon("city"),
                                                   br(),
                                                   
                                                   h3("Parking:", icon = icon("square-parking") ,class="estiloh3"),
                                                   
                                                   br(),
                                                   
                                                   fluidRow(
                                                       column(width = 6, 
                                                              div(style="width: 100% ; height: 1000px",
                                                                  plotOutput("apar1"))
                                                              ),
                                                       column(width = 6, 
                                                              div(style="width: 100% ; height: 1000px",
                                                                  plotOutput("apar2"))
                                                       )
                                                   ),
                                                   
                                                   br(),
                                                   
                                                   fluidRow(
                                                     column(width = 6, 
                                                            div(HTML('<i class="fas fa-thumbs-up" style = "color:#25FC17;"></i> <span class = "imp-text">Good overall coverage</span>')), 
                                                            div(HTML('<i class="fas fa-thumbs-up" style = "color:#25FC17;"></i> <span class = "imp-text">Good results in Andalucia, Murcia and Basque Countr</span>')),
                                                            div(HTML('<i class="fas fa-thumbs-down" style = "color:#EF0808;"></i> <span class = "imp-text">Poor Results in Galicia, Islas Canarias and Islas Baleares</span>'))
                                                     ),
                                                     column(width = 6, 
                                                            div(HTML('<i class="fas fa-thumbs-up" style = "color:#25FC17;"></i> <span class = "imp-text">Good rate of guarded parkings in the Basque Country or Huelva</span>')), 
                                                            div(HTML('<i class="fas fa-thumbs-down" style = "color:#EF0808;"></i> <span class = "imp-text">A lot of information is missing</span>')), 
                                                            div(HTML('<i class="fas fa-thumbs-down" style = "color:#EF0808;"></i> <span class = "imp-text">They are not guarded car parks in general</span>'))
                                                     )
                                                   ),
                                                   
                                                   br(),
                
                                                   markdown(var_text_apar),
                                                   
                                                   br(),br(),
                                                   
                                                   h3("Urbanization:", class="estiloh3"),
                                                   
                                                   br(),
                                                   
                                                   div(style="width: 100% ; height: 1000px",
                                                       plotOutput("urb1a")),
                                                   
                                                   br(),
                                                   
                                                   div(HTML(var_text_urb_posi)),
                                                   
                                                   
                                                   br(),
                                                   div(HTML('<i class="fas fa-circle" style = "color:#CA75DB;"></i> <span class = "interesting-hint">Extreme cases of high occupacy in Bizkaia, Melilla, Barcelona, Malaga</span>')),
                                                   div(HTML('<i class="fas fa-circle" style = "color:#CA75DB;"></i> <span class = "interesting-hint">Lower degree of occupation in Islas Canarias, A Coruna, Cantabria, Almeria</span>')),
                                                   
                                                   br(),br(),
                                                   
                                                   h3("Occupation:", class="estiloh3"),
                                                   
                                                   br(),
                                                   
                                                   div(style="width: 100% ; height: 1000px",
                                                       plotOutput("ocu1a")),
                                                   
                                                   br(),

                                                   div(HTML(var_text_ocu_posi)),
                                                   
                                                   br(),
                                                   
                                                   div(HTML('<i class="fas fa-circle" style = "color:#CA75DB;"></i> <span class = "interesting-hint">Greater degree of <span class = "imp-word">urbanization</span> on the beaches of the Mediterranean area</span>')),
                                                   div(HTML('<i class="fas fa-circle" style = "color:#CA75DB;"></i> <span class = "interesting-hint">Greater degree of <span class = "imp-word">isolation</span> from the beaches of northern Spain and the islands</span>')),
                                                   div(HTML('<i class="fas fa-circle" style = "color:#CA75DB;"></i> <span class = "interesting-hint">Special cases in Ceuta and Melilla, due to they are small sections of land have a higher degree of urbanization</span>')),
                                                   
                                                   br(),

                                                   markdown(var_text_ocuurb),
                                                   
                                                   br(),br(),br()
                                          ),
                                          
                                          # tabPanel: Width and Length ----
                                          tabPanel("Width and Length", icon = icon("ruler-combined"),
                                                   br(),
                                                   
                                                   h3("Width:", class="estiloh3"),
                                                   
                                                   br(),
                                                   
                                                   div(style="width: 100% ; height: 500px",
                                                       plotOutput("width")),
                                                   
                                                   br(),
                                                   
                                                   div(HTML('<i class="fas fa-thumbs-down" style = "color:#EF0808;"></i> <span class = "imp-text">The beaches belonging to the northern of Spain present a large number of atypical observations</span>')),
                                                   
                                                   br(),
                                                   
                                                   div(HTML('<i class="fas fa-circle" style = "color:#CA75DB;"></i> <span class = "interesting-hint">Greater degree of <span class = "imp-word">urbanization</span> on the beaches of the Mediterranean area</span>')),
                                                   
                                                   br(),br(),
                                                   
                                                   h3("Length:", class="estiloh3"),
                                                   
                                                   br(),
                                                   
                                                   div(style="width: 100% ; height: 500px",
                                                       plotOutput("len")),
                                                   
                                                   br(),
                                                   
                                                   div(HTML('<i class="fas fa-thumbs-up" style = "color:#25FC17;"></i> <span class = "imp-text">Less disproportionate distribution of outliers</span>')), 
                                                   div(HTML('<i class="fas fa-thumbs-up" style = "color:#25FC17;"></i> <span class = "imp-text">Not much difference is observed between the different communities and provinces</span>')),
                                                   div(HTML('<i class="fas fa-thumbs-down" style = "color:#EF0808;"></i> <span class = "imp-text">Atypical results are presented in Andalusia since the exterior graph is not properly observed</span>')), 
                                                   
                                                   br(),
                                                   
                                                   h3("Particular Case, Andalucia:", class="estiloh3"),
                                                   
                                                   br(),
                                                   
                                                   div(style="width: 100% ; height: 500px",
                                                       plotOutput("partcase")),
                                                   
                                                   br(),
                                                   
                                                   div(HTML(var_text_partcase)),
                                                   
                                                   br(),
                                                   
                                                   div(style="width: 100% ; height: 500px",
                                                       plotOutput("partcase2")),
                                                   
                                                   br(),
                                                   
                                                   div(HTML(var_text_partcase2)),
                                                   
                                                   br(),br(),br()
                                          )
                                      )
                             ),
                             
                             
                             tabPanel("Documentation",icon = icon("book"),
                                      
                                      # Markdown documentation
                                      markdown(doc)
                             )
                             
                )
)