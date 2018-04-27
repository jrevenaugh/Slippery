ui <- shinyUI(
  navbarPage("Slippery - Earthquake Simulation",
    useShinyjs(),
    theme = shinytheme("united"),
    tabPanel("Parameters",

      # Set model parameters----------------------------------------------------
      titlePanel("Fault Segment Distribution"),
      column(4,
        wellPanel(
          # Choose fault segment distribution
          selectInput(inputId = "fault_dist",
                      label = "Distribution",
                      choices = list("Equal" = 1,
                                     "Uniform" = 2,
                                     "Gaussian" = 3,
                                     "Exponential" = 4),
                      selected = 3
          ),

          # Slider input for number of segments
          sliderInput(inputId = "n_segments",
                      label = "Number of Fault Segments",
                      min = 20, max = 500, step = 20,
                      value = 200
          ),

          # Slider for segment interaction factor
          sliderInput(inputId = "kexp",
                      label = "Interaction Scalar",
                      min = 1, max = 4, step = 0.2,
                      value = 2
          ),

          # Slider for static friction factor
          sliderInput(inputId = "static_base",
                      label = "Static Friction",
                      min = 25, max = 150, step = 25,
                      value = 75
          ),

          # Text input for random seed
          textInput(inputId = "random_seed",
                    label = "Random Seed",
                    value = "12345678",
                    width = "50%"
          ),

          hr(),

          # Action button to perform calculation
          actionButton(inputId = "runIt",
                       label = "Run Simulation"
          )

        )
      ),

      column(6,
        plotOutput(outputId = "plot1",
                   height = 400
        )
      )
    ),

    # Visualize cumulative offset of blocks ------------------------------------
    tabPanel("Offset",
      titlePanel("Cumulative Offset vs Time"),
      column(12,
        plotOutput(outputId = "plot2",
                   height = 600, width = 1000
        )
      )
    ),

    # Visualize quakes ---------------------------------------------------------
    tabPanel("Quakes",
      titlePanel("Rupture Time and Slip"),
      column(12,
        plotOutput(outputId = "plot3",
                   height = 600, width = 1000
        )
      )
    ),

    # Statistics and Gutenberg/Richter Relation --------------------------------
    tabPanel("Stats",
      titlePanel("Simulation Statistics"),
      column(3,
        tableOutput("table")),
      column(6,
        plotOutput(outputId = "logn",
                   height = 500, width = 500
        )
      )
    ),

    tabPanel(
      "More Information",
      "Any questions or comments can be sent to:",
      br(),
      "Justin Revenaugh:",
      br(),
      "University of Minnesota",
      br(),
      "Earth Sciences",
      br(),
      "Minneapolis, MN 55455 USA",
      br(),
      a("justinr@umn.edu", href = "mailto:justinr@umn.edu"),
      br(),
      a("See the code", href = "https://github.com/jrevenaugh/Slippery"),
      br()
    )
  )
)
