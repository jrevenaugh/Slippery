ui <- shinyUI(navbarPage("Slider - Earthquake Simulation",
                         useShinyjs(),
                         theme = shinytheme("united"),
                         tabPanel("Parameters",

                                  # Application title
                                  titlePanel("Fault Segment Distribution"),

                                  # This tabpanel has two columns: the first houses the user interface,
                                  # station, component and phase selection.  The second shows seismograms.
                                  column(4, wellPanel(
                                    # Choose fault segment distribution
                                    selectInput("fault_dist", "Distribution",
                                                choices = list( "Equal" = 1,
                                                                "Uniform" = 2,
                                                                "Gaussian" = 3,
                                                                "Exponential" = 4 ),
                                                selected = 3),

                                    # Slider input for number of segments
                                    sliderInput("n_segments", "Number of Fault Segments",
                                                min = 20,
                                                max = 500,
                                                step = 20,
                                                value = 200),

                                    # Slider for segment interaction factor
                                    sliderInput("kexp", "Interaction Scalar",
                                                min = 1,
                                                max = 4,
                                                step = 0.2,
                                                value = 2),

                                    # Slider for static friction factor
                                    sliderInput("static_base", "Static Friction",
                                                min = 25,
                                                max = 150,
                                                step = 25,
                                                value = 75),

                                    # Text input for random seed
                                    textInput("random_seed",
                                              "Random Seed",
                                              value = "12345678",
                                              width = "50%"),

                                    hr(),

                                    # Action button to perform calculation
                                    actionButton("runIt", label = "Run Simulation")

                                  )),

                                  # The second column houses the plot(s) of the data that was selected.  These plots are defined in the server.R file.
                                  column(6,
                                         plotOutput("plot1", height = 400 )
                                  )
                         ),

                         tabPanel("Offset",
                                  titlePanel("Cumulative Offset vs Time"),
                                  column(12,
                                         plotOutput("plot2", height = 600, width = 1000 )
                                  )
                         ),

                         tabPanel("Quakes",
                                  titlePanel("Rupture Time and Slip"),
                                  column(12,
                                         plotOutput("plot3", height = 600, width = 1000 )
                                  )
                         ),

                         tabPanel("Stats",
                                  titlePanel("Simulation Statistics"),
                                  column(3,
                                         tableOutput("table" )
                                  ),
                                  column(6,
                                         plotOutput("logn", height = 500, width = 500 )
                                  )
                         ),

                         tabPanel("More Information",   # Contact Information.
                                  "Any questions or comments can be sent to:", br(),
                                  "Justin Revenaugh:", br(),
                                  "University of Minnesota", br(),
                                  "Earth Sciences", br(),
                                  "Minneapolis, MN 55455 USA", br(),
                                  a("justinr@umn.edu", href="mailto:justinr@umn.edu"), br(),
                                  a("See the code", href="https://github.com/jrevenaugh/EQ_Locator"), br()
                         )
) )
