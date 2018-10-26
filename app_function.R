### Shiny app: Eliciting Prior Opinion on External Biases
## Run a Shiny app which allows for the elicitation of expert opinion
## regarding external biases as defined in Wadsworth et al. (Under review)

# The 'OUT_dir' argument specifies the directory where output files should be saved.
# elicit_func() will run the Shiny app.
# - Once the elicitation is completed, the final elicitation plots and
#   final elicited bias prior parameter values will be saved to OUT_dir.

elicit_func <- function(OUT_dir="") {
    pixels = "41px"
    shinyApp(
      ui = fluidPage(titlePanel("Eliciting opinion on Dose-Response relationships with topiramate"),
                     bootstrapPage(tabsetPanel(
                       tabPanel("Outline",
                                br(),
                                fluidRow(
                                  column(2,
                                         br(),
                                         br(),
                                         br(),
                                         textInput("expert","Please enter your name here.")
                                  ),
                                  column(9,
                                         h3(HTML(text=c('<span style="color:blue">Objective: We wish to elicit your opinion on how similar the dose-response relationship in children (4-10 years) in a new study will be to the dose-response relationships in adolescents (11-17 years) and adults that have been observed in a previous study.</span>
                                                        <br/>
                                                        <br/>
                                                        <span style="color:red">Old study:</span> Explored the efficacy of adjunctive topiramate in patients with partial seizures or primary generalised tonic-clonic seizures aged 11-85.
                                                        <br/>
                                                        <br/>
                                                        <span style="color:red">New study:</span> Exploring the efficacy of adjunctive topiramate in patients with partial seizures or primary generalised tonic-clonic seizures aged 4-10 years.
                                                        <br/>
                                                        <br/>
                                                        The response here is percent change in seizure frequency from baseline.')))
                                         )
                                         )
                                         ),
                       tabPanel("Existing data",
                                checkboxInput("check_AdultData","Tick to show adult data"),
                                checkboxInput("check_AdolescentData","Tick to show adolescent data"),
                                h4("This plot presents the line of best fit for adults and adolescents, found from existing data.
                                   The line of best fit represents our best estimate of what the average response on a dose is."),
                                plotOutput("ExistingPlot")
                                ),
                       tabPanel("Dose-response in children",
                                fluidRow(
                                  column(8,
                                         conditionalPanel( condition = "input.Tick_AvgResp == false",
                                                           br(),
                                                           h4("Imagine you have treated a large number of paediatric patients (say 100). Can you give us your opinion about
                                                              their average response?"),
                                                           h4("Pick the shape of the line of best fit for children which most closely matches your belief."),
                                                           plotOutput("ShapePlot1")),
                                         conditionalPanel( condition = "input.Tick_AvgResp == true",
                                                           h4("This plot illustrates your best guess on the line of best fit for the dose-response relationship in children."),
                                                           plotOutput("ERPlot"))),
                                  column(4,
                                         br(),
                                         br(),
                                         checkboxInput("Tick_AvgResp","Elicit expert opinion on the shape of the average response."),
                                         conditionalPanel( condition = "input.Tick_AvgResp == false",
                                                           radioButtons("ShapeAvgResp", "Select the shape that best matches your belief",
                                                                        c("A", "B","C","D","E"))
                                         ),
                                         conditionalPanel( condition = "input.Tick_AvgResp == true",
                                                           h4("Given the existing data, adjust the sliders to give your best guess at the average response amongst children
                                                              aged 2-11 years on placebo and a high dose of the test treatment."),
                                                           uiOutput("sliderAvgPlac"),
                                                           uiOutput("sliderAvgMax")
                                                           ))
                                  )
                       ),
                       tabPanel("Uncertainty regarding dose-response in children",
                                fluidRow(
                                  column(8,
                                         conditionalPanel( condition = "input.Tick_PCtiles == false",
                                                           h4("These plots present the options for the shape which most closely matches your belief regarding your opinion on where the line of best fit for the dose-response relationship in children will lie when we model data from the new study."),
                                                           br(),
                                                           plotOutput("ShapePlot2")),
                                         conditionalPanel( condition = "input.Tick_PCtiles == true",
                                                           h4("This plot illustrates your opinion on where the line of best fit for the dose-response relationship in children will lie when we model data from the new study."),
                                                           plotOutput("ERPlot2"))),
                                  column(4,
                                         br(),
                                         br(),
                                         checkboxInput("Tick_PCtiles","Elicit expert opinion on the shape of 5th and 95th percentiles."),
                                         conditionalPanel( condition = "input.Tick_PCtiles == false",
                                                           radioButtons("ShapePCtiles", "Select the shape that best matches your belief",
                                                                        c("A", "B","C","D"))
                                         ),
                                         conditionalPanel( condition = "input.Tick_PCtiles == true",
                                                           br(),
                                                           uiOutput("sliderDose0"),
                                                           uiOutput("sliderDosemid"),
                                                           uiOutput("sliderDosehigh")))
                                )
                       ),
                       tabPanel("Opinion on placebo",
                                h4("Imagine you have treated 100 paediatric patients, can you give us your opinion regarding
                                   their average percent seizure frequency change from baseline, when on placebo. Please
                                   fill in the text boxes under the histogram below to mark your belief. Every text box  takes a number representing the % chance and matches with the
                                   corresponding interval it is below."),
                                h4("The more stongly you believe the average response
                                   would lie in a particular interval, the more % chance you should add. If you believe it
                                   impossible that the average response would lie in a particular interval, then add no % chance
                                   to that interval. If you are certain the average response would lie in a certain interval,
                                   100% should be in that interval."),
                                h4(div(textOutput("Meantext1"), style = "color:red")),
                                h4(div(textOutput("Uncerttext1"), style = "color:green")),

                                br(),
                                fluidRow(
                                  column(3,
                                         checkboxInput("Tick_DensHist0","Show density")
                                  ),
                                  column(5,
                                         div(h3(textOutput("text1")), style = "color:red")
                                  )
                                ),
                                plotOutput("HistPlot0", click = "plot_click1"),
                                br(),
                                br(),
                                br(),
                                br(),
                                div(style="display:inline-block",class="outer",width="100%",
                                    div(style="display:inline-block",h5("_________")),
                                    div(style="display:inline-block",textInput("n100_n95_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n95_n90_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n90_n85_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n85_n80_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n80_n75_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n75_n70_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n70_n65_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n65_n60_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n60_n55_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n55_n50_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n50_n45_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n45_n40_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n40_n35_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n35_n30_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n30_n25_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n25_n20_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n20_n15_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n15_n10_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n10_n05_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n05_n00_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n00_p05_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p05_p10_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p10_p15_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p15_p20_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p20_p25_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p25_p30_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p30_p35_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p35_p40_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p40_p45_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p45_p50_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p50_p55_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p55_p60_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p60_p65_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p65_p70_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p70_p75_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p75_p80_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p80_p85_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p85_p90_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p90_p95_0", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p95_p100_0", label = "", value = 0,width=pixels))
                                )
                                ),
                       tabPanel("Opinion on moderate dose",
                                h4("Imagine you have treated 100 paediatric patients, can you give us your opinion regarding
                                   their average percent seizure frequency change from baseline, when on a moderate dose of
                                   8 mg/kg/day. Please
                                   fill in the text boxes under the histogram below to mark your belief. Every text box  takes a number representing the % chance and matches with the
                                   corresponding interval it is below."),
                                h4("The more stongly you believe the average response
                                   would lie in a particular interval, the more % chance you should add. If you believe it
                                   impossible that the average response would lie in a particular interval, then add no % chance
                                   to that interval. If you are certain the average response would lie in a certain interval,
                                   100% should be in that interval."),
                                h4(div(textOutput("Meantext2"), style = "color:red")),
                                h4(div(textOutput("Uncerttext2"), style = "color:green")),
                                br(),
                                fluidRow(
                                  column(3,
                                         checkboxInput("Tick_DensHistMid","Show density")
                                  ),
                                  column(5,
                                         div(h3(textOutput("text2")), style = "color:red")
                                  )
                                ),
                                plotOutput("HistPlotMid", click = "plot_click2"),
                                br(),
                                br(),
                                br(),
                                br(),
                                div(style="display:inline-block",class="outer",width="100%",
                                    div(style="display:inline-block",h5("_________")),
                                    div(style="display:inline-block",textInput("n100_n95_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n95_n90_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n90_n85_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n85_n80_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n80_n75_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n75_n70_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n70_n65_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n65_n60_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n60_n55_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n55_n50_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n50_n45_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n45_n40_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n40_n35_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n35_n30_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n30_n25_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n25_n20_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n20_n15_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n15_n10_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n10_n05_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n05_n00_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n00_p05_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p05_p10_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p10_p15_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p15_p20_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p20_p25_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p25_p30_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p30_p35_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p35_p40_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p40_p45_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p45_p50_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p50_p55_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p55_p60_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p60_p65_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p65_p70_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p70_p75_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p75_p80_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p80_p85_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p85_p90_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p90_p95_Mid", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p95_p100_Mid", label = "", value = 0,width=pixels))
                                )
                                ),
                       tabPanel("Opinion on high dose",
                                h4("Imagine you have treated 100 paediatric patients, can you give us your opinion regarding
                                   their average percent seizure frequency change from baseline, when on a high dose of
                                   16 mg/kg/day. Please
                                   fill in the text boxes under the histogram below to mark your belief. Every text box  takes a number representing the % chance and matches with the
                                   corresponding interval it is below."),
                                h4("The more stongly you believe the average response
                                   would lie in a particular interval, the more % chance you should add. If you believe it
                                   impossible that the average response would lie in a particular interval, then add no % chance
                                   to that interval. If you are certain the average response would lie in a certain interval,
                                   100% should be in that interval."),
                                h4(div(textOutput("Meantext3"), style = "color:red")),
                                h4(div(textOutput("Uncerttext3"), style = "color:green")),
                                br(),
                                fluidRow(
                                  column(3,
                                         checkboxInput("Tick_DensHistHigh","Show density")
                                  ),
                                  column(5,
                                         div(h3(textOutput("text3")), style = "color:red")
                                  )
                                ),
                                plotOutput("HistPlotHigh", click = "plot_click3"),
                                br(),
                                br(),
                                br(),
                                br(),
                                div(style="display:inline-block",class="outer",width="100%",
                                    div(style="display:inline-block",h5("_________")),
                                    div(style="display:inline-block",textInput("n100_n95_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n95_n90_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n90_n85_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n85_n80_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n80_n75_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n75_n70_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n70_n65_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n65_n60_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n60_n55_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n55_n50_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n50_n45_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n45_n40_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n40_n35_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n35_n30_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n30_n25_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n25_n20_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n20_n15_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n15_n10_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n10_n05_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n05_n00_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("n00_p05_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p05_p10_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p10_p15_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p15_p20_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p20_p25_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p25_p30_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p30_p35_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p35_p40_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p40_p45_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p45_p50_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p50_p55_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p55_p60_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p60_p65_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p65_p70_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p70_p75_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p75_p80_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p80_p85_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p85_p90_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p90_p95_High", label = "", value = 0,width=pixels)),
                                    div(style="display:inline-block",textInput("p95_p100_High", label = "", value = 0,width=pixels))
                                )
                                ),
                       tabPanel("Uncertainty regarding dose-response in children",
                                fluidRow(
                                  column(3,
                                         checkboxInput("Tick_ShowUncert","Tick to show uncertainty")
                                  ),
                                  column(5,
                                         conditionalPanel(condition = 'input.Tick_ShowUncert == true',
                                                          checkboxInput("Tick_ShapeUncertShow","Show uncertainty from earlier shape choice"))
                                  )
                                ),
                                fluidRow(
                                  column(3,
                                         checkboxInput("Tick_HistShow","Show histograms.")
                                  ),
                                  column(3,
                                         conditionalPanel(condition = 'input.Tick_HistShow == true',
                                                          checkboxInput("Tick_DensShow","Show densities."))
                                  )
                                ),
                                h4("This plot illustrates your opinion on where the line of best fit for the dose-response relationship in children will lie when we model data from the new study."),
                                plotOutput("ERPlot3")
                       ),
                       tabPanel("Comparison of opinion and existing data",
                                fluidRow(
                                  column(3,
                                         checkboxInput("Tick_LogFinal","Show the log plot.")
                                  ),
                                  column(3,
                                         checkboxInput("check_ChildrenData","Tick to show simulated data for younger children based on expert opinion")
                                  )
                                ),                             conditionalPanel( condition = "input.Tick_LogFinal == false",
                                                                                 h4("This plot compares fitted dose-response relationships in adults and adolescents fitted to historical data compared with your opinion on the dose-response relationship in children in the new study."),
                                                                                 br(),
                                                                                 plotOutput("FinalPlot")
                                ),
                                conditionalPanel( condition = "input.Tick_LogFinal == true",
                                                  h4("This plot compares fitted dose-response relationships in adults and adolescents fitted to historical data compared with your opinion on the dose-response relationship in children in the new study, on the log scale."),
                                                  br(),
                                                  plotOutput("FinalLogPlot"))
                       )
                                ))
                       ),
      server = function(input, output,session) {

        # Model specifications
        alpha  <- 4.4469    #adult placebo effect (from Girgis et al. paper)
        betap  <- -0.0627   #adult concentration effect (from Girgis et al. paper)
        gamma1 <- 0.0361    #difference between adult and paediatric placebo effect (from Girgis et al. paper)
        gamma2 <- 0.0048    #difference between adult and paediatric concentration effect (from Girgis et al. paper)
        dose <- 0:16        #dose range
        dose2 <- dose^2
        dnum1 = 1           #index in 'dose' of the first dose we elicit expert opinion about (here dose = 0)
        dnum2 = 9           #index in 'dose' of the second dose we elicit expert opinion about (here dose = 8)
        dnum3 = 17          #index in 'dose' of the third dose we elicit expert opinion about (here dose = 16)
        dnums = c(dnum1,dnum2,dnum3)
        dose.optim <- dose[dnums]

        #PD is log(% change from baseline + 110)
        PD_age0 <- alpha + betap*dose                         #adults
        PD_age1 <- alpha + betap*dose + gamma1 + gamma2*dose  #adolescents

        #Define the 'line of best fit' shapes that will be inititially displayed
        bias_meanshape1 <- (PD_age0+PD_age1)/2 + (PD_age0+PD_age1)/45
        bias_meanshape2 <-  PD_age1
        bias_meanshape3 <- (PD_age0+PD_age1)/2
        bias_meanshape4 <-  PD_age0
        bias_meanshape5 <- (PD_age0+PD_age1)/2 - (PD_age0+PD_age1)/45

        #Define histogram bins
        binwidth = 5
        bins <- seq(-100,100,by=binwidth)
        bins1 <- seq(-100,95,by=binwidth)
        bins2 <- seq(-95,100,by=binwidth)
        center_bin <- seq(-97.5,97.5,by=binwidth)

        #Define functions for transforming between % change and log(% change) scales
        back_trans <- function(x){
          exp(x)-110
        }
        forw_trans <- function(x){
          log(x+110)
        }

        #Define function for finding bias prior parameters from elicited opinion, for reactive plots
        inits1 <- c(0,0,0)
        ofn_q95 <- function(x,q95,ChosenAvgLine){
          ex1 = exp(x[1])
          ex2 = x[2]
          ex3 = exp(x[3])
          bias_var <- ex1^2 + (dose.optim^2)*(ex2^2 + ex3^2) + 2*dose.optim*ex1*ex2
          bias.q95 <- qnorm(0.95,ChosenAvgLine[dnums],sqrt(bias_var))
          y = sum(abs(bias.q95 - q95))
          return(y)
        }

        #Define the line of best fit for children
        mean_line <- reactive({
          bias_mean <- c(AvgInt(),AvgSlo())
          ChosenAvgLine <- PD_age1 + bias_mean[1] + dose*bias_mean[2]
          return(ChosenAvgLine)
        })

        # Define reactive values for starting points for Placebo and Max dose, based on choice of mean line
        reactiveAvgPlac <- reactive({
          if(input$ShapeAvgResp == "A"){
            ChoiceAvgInt <- back_trans(bias_meanshape1[dnum1])
          }else if(input$ShapeAvgResp == "B"){
            ChoiceAvgInt <- back_trans(bias_meanshape2[dnum1])
          }else if(input$ShapeAvgResp == "C"){
            ChoiceAvgInt <- back_trans(bias_meanshape3[dnum1])
          }else if(input$ShapeAvgResp == "D"){
            ChoiceAvgInt <- back_trans(bias_meanshape4[dnum1])
          }else if(input$ShapeAvgResp == "E"){
            ChoiceAvgInt <- back_trans(bias_meanshape5[dnum1])
          }
        })

        reactiveAvgMax <- reactive({
          if(input$ShapeAvgResp == "A"){
            ChoiceAvgMax <- back_trans(bias_meanshape1[dnum3])
          }else if(input$ShapeAvgResp == "B"){
            ChoiceAvgMax <- back_trans(bias_meanshape2[dnum3])
          }else if(input$ShapeAvgResp == "C"){
            ChoiceAvgMax <- back_trans(bias_meanshape3[dnum3])
          }else if(input$ShapeAvgResp == "D"){
            ChoiceAvgMax <- back_trans(bias_meanshape4[dnum3])
          }else if(input$ShapeAvgResp == "E"){
            ChoiceAvgMax <- back_trans(bias_meanshape5[dnum3])
          }
        })

        # Define reactive values for starting points for percentiles
        reactiveRespByDose <- reactive({
          ChosenAvgLine <- mean_line()
          if(input$ShapePCtiles == "A"){
            qmfit <- c(ChosenAvgLine[dnum1] + 0.126, ChosenAvgLine[dnum2] + 0.3676, ChosenAvgLine[dnum3] + 1.055)
            osol <- optim(par=inits1, ofn_q95, q95 = qmfit,ChosenAvgLine=mean_line())
            bias_var <- exp(osol$par[1])^2 + (dose^2)*(osol$par[2]^2 + exp(osol$par[3])^2) + 2*dose*exp(osol$par[1])*osol$par[2]
            ChoiceRespByDose <- c(qnorm(0.95,ChosenAvgLine,sqrt(bias_var)),qnorm(0.05,ChosenAvgLine,sqrt(bias_var)))
            return(ChoiceRespByDose)
          }else if(input$ShapePCtiles == "B"){
            qmfit <- c(ChosenAvgLine[dnum1] + 0.2, ChosenAvgLine[dnum2] + 0.2, ChosenAvgLine[dnum3] + 0.2)
            osol <- optim(par=inits1, ofn_q95, q95 = qmfit,ChosenAvgLine=mean_line())
            bias_var <- exp(osol$par[1])^2 + (dose^2)*(osol$par[2]^2 + exp(osol$par[3])^2) + 2*dose*exp(osol$par[1])*osol$par[2]
            ChoiceRespByDose <- c(qnorm(0.95,ChosenAvgLine,sqrt(bias_var)),qnorm(0.05,ChosenAvgLine,sqrt(bias_var)))
            return(ChoiceRespByDose)
          }else if(input$ShapePCtiles == "C"){
            qmfit <- c(ChosenAvgLine[dnum1] + 0.7, ChosenAvgLine[dnum2] + 0.35, ChosenAvgLine[dnum3] + 0.3)
            osol <- optim(par=inits1, ofn_q95, q95 = qmfit,ChosenAvgLine=mean_line())
            bias_var <- exp(osol$par[1])^2 + (dose^2)*(osol$par[2]^2 + exp(osol$par[3])^2) + 2*dose*exp(osol$par[1])*osol$par[2]
            ChoiceRespByDose <- c(qnorm(0.95,ChosenAvgLine,sqrt(bias_var)),qnorm(0.05,ChosenAvgLine,sqrt(bias_var)))
            return(ChoiceRespByDose)
          }else if(input$ShapePCtiles == "D"){
            qmfit <- c(ChosenAvgLine[dnum1] + 0.7, ChosenAvgLine[dnum2] + 0.35, ChosenAvgLine[dnum3] + 1.2)
            osol <- optim(par=inits1, ofn_q95, q95 = qmfit,ChosenAvgLine=mean_line())
            bias_var <- exp(osol$par[1])^2 + (dose^2)*(osol$par[2]^2 + exp(osol$par[3])^2) + 2*dose*exp(osol$par[1])*osol$par[2]
            ChoiceRespByDose <- c(qnorm(0.95,ChosenAvgLine,sqrt(bias_var)),qnorm(0.05,ChosenAvgLine,sqrt(bias_var)))
            return(ChoiceRespByDose)
          }
        })

        #Define reactive optim
        reactiveOptim <- reactive({
          ChosenAvgLine <- mean_line()
          Chosenqmfit <- forw_trans(c(input$EliDose0,input$EliDosemid,input$EliDosehigh))
          Chosenosol <- optim(par=inits1, ofn_q95, q95 = Chosenqmfit, ChosenAvgLine=mean_line(), control=list(maxit = 1000))
          return(Chosenosol)
        })

        #Define reactive values for final bias covariance matrix
        reactiveFinalCov <- reactive({
          Chosenosol <- reactiveOptim()
          Choicevar1 <- exp(Chosenosol$par[1])^2
          Choicevar2 <- Chosenosol$par[2]^2 + exp(Chosenosol$par[3])^2
          Choicecov <- exp(Chosenosol$par[1])*Chosenosol$par[2]
          return(c(Choicevar1,Choicevar2,Choicecov,Chosenosol$par))
        })

        #Define the sliders
        output$sliderAvgPlac <- renderUI({
          sliderInput("AvgPlac", "Average response on placebo:",
                      min=-100, max=100, value=reactiveAvgPlac(), round=-1, step=0.1)
        })

        output$sliderAvgMax <- renderUI({
          sliderInput("AvgMax", "Maximum effect of topiramate (average response on the highest dose):",
                      min=-100, max=100, value=reactiveAvgMax(), round=-1, step=0.1)
        })

        AvgInt <- reactive({
          forw_trans(input$AvgPlac) - PD_age1[1]
        })

        AvgSlo <- reactive({          #this is: (slope for elicited children) - (slope for adolescent data)
          (forw_trans(input$AvgMax) - forw_trans(input$AvgPlac))/dose[dnum3]  - (PD_age1[2]-PD_age1[1])/(dose[2]-dose[1])
        })

        output$sliderDose0 <- renderUI({
          sliderInput("EliDose0", "Given the existing data, state a value which you are 95% sure the average response
                      amongst younger children on placebo will lie below:",
                      min=round(back_trans(PD_age1[dnum1] + AvgInt() + dose[dnum1]*AvgSlo())), max=100, value=back_trans(reactiveRespByDose()[dnum1]), round=-1, step=0.1)
        })

        output$sliderDosemid <- renderUI({
          sliderInput("EliDosemid", "Given the existing data, state a value which you are 95% sure the average response
                      amongst younger children on a moderate dose of the test treatment will lie below:",
                      min=round(back_trans(PD_age1[dnum2] + AvgInt() + dose[dnum2]*AvgSlo())), max=100, value=back_trans(reactiveRespByDose()[dnum2]), round=-1, step=0.1)
        })

        output$sliderDosehigh <- renderUI({
          sliderInput("EliDosehigh", "Given the existing data, state a value which you are 95% sure the average response
                      amongst younger children on a high dose of the test treatment will lie below:",
                      min=round(back_trans(PD_age1[dnum3] + AvgInt() + dose[dnum3]*AvgSlo())), max=100, value=back_trans(reactiveRespByDose()[dnum3]), round=-1, step=0.1)
        })

        #define the reactive plots
        output$ExistingPlot <- renderPlot({
          par(mar=c(15,5,1.1,2.1), xpd=TRUE)
          plot(dose,back_trans(PD_age0),ylim=c(-100,0),lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',
               xaxp = c(0,16,2), xlab='Dose (mg/kg/day)',ylab='% change from baseline in seizure frequency')
          lines(dose,back_trans(PD_age1),type='l',col='blue',lty=1,lwd=2)
          legend(-0.47,-117,cex=1.5,c("Adult line of best fit", "Adolescent line of best fit","Adult data","Adolescent data"),
                 col=c(1,'blue',1,'blue'), lty=c(1,1,NA,NA),pch=c(NA,NA,15,16),lwd=2)
          par(mar=c(15,5,1.1,2.1), xpd=F)
          abline(h=seq(-100,0,by=20), v=c(0,8,16), col="gray", lty=3)
          if(input$check_AdolescentData == TRUE){
            set.seed(123)
            index_Adoles <- sample(1:length(PD_age1),size=170,replace = T)
            sampAdoles <- PD_age1[index_Adoles]
            AdolesData <- sampAdoles + rnorm(170,0,sqrt(0.03))
            WhichAdolesDose <- dose[index_Adoles]
            points(WhichAdolesDose,back_trans(AdolesData),pch=9,col='blue',cex=1)
          }
          if(input$check_AdultData == TRUE){
            set.seed(100)
            index_Adult <- sample(1:length(PD_age0),size=170,replace = T)
            sampAdult <- PD_age0[index_Adult]
            AdultData <- sampAdult + rnorm(170,0,sqrt(0.03))
            WhichAdultDose <- dose[index_Adult]
            points(WhichAdultDose,back_trans(AdultData),pch=15,cex=1)
          }
        }, height = 820, width = 1200)

        ERPlot_func <- function(){
          par(mar=c(15,5,1.1,2.1), xpd=TRUE)
          plot(dose,back_trans(PD_age0),ylim=c(-100,max(c(0,back_trans(PD_age1 + AvgInt() + dose*AvgSlo())))),lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',
               xaxp = c(0,16,2), xlab='Dose (mg/kg/day)',ylab='% change from baseline in seizure frequency')
          lines(dose,back_trans(PD_age1),type='l',col='blue',lty=1,lwd=2)
          legend(-0.47,-117,cex=1.5,col=c(1,'blue','red'),lty=c(1,1,1),lwd=2,
                 c("Adult line of best fit","Adolescent line of best fit",
                   "Your best guess at the dose-response relationship in children"))
          par(mar=c(15,5,1.1,2.1), xpd=F)
          abline(h=seq(-100,max(c(0,back_trans(PD_age1 + AvgInt() + dose*AvgSlo()))),by=20),
                 v=c(0,8,16), col="gray", lty=3)
          lines(dose,back_trans(PD_age1 + AvgInt() + dose*AvgSlo()),type='l',col='red',lty=1,lwd=2)
        }

        ERPlot_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_meanPlot.pdf",sep=''),pointsize=10,paper="a4")
          ERPlot_func()
          dev.off()
        })

        output$ERPlot <- renderPlot({
          ERPlot_func()
          ERPlot_plot()
        }, height = 820, width = 1200)

        ERPlot2_func <- function(){
          ChosenAvgLine <- mean_line()
          Chosenosol <- reactiveOptim()
          Chosenbias_var <- exp(Chosenosol$par[1])^2 + (dose^2)*(Chosenosol$par[2]^2 + exp(Chosenosol$par[3])^2) + 2*dose*exp(Chosenosol$par[1])*Chosenosol$par[2]
          ChoicePlotqmfit95 <- qnorm(0.95,ChosenAvgLine,sqrt(Chosenbias_var))
          ChoicePlotqmfit05 <- qnorm(0.05,ChosenAvgLine,sqrt(Chosenbias_var))
          par(mar=c(15,5,1.1,2.1), xpd=TRUE)
          plot(dose,back_trans(ChosenAvgLine),
               ylim=c(-100,max(c(back_trans(ChosenAvgLine),back_trans(ChoicePlotqmfit95),
                                 input$EliDose0,input$EliDosemid,input$EliDosehigh))),
               lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',col='red', xaxp = c(0,16,2),
               xlab='Dose (mg/kg/day)',ylab='% change from baseline in seizure frequency')
          legend(-0.47,-125,cex=1.5, lty=c(1,2,NA),lwd=2, pch=c(NA,NA,16),
                 c("Your best guess at the dose-response relationship in children",
                   "Lines indicating your 90% credibility interval around the line of best fit",
                   "Values you gave that you were 95% sure the true average response would lie below"),col=c('red','darkgreen','darkblue'))
          par(mar=c(15,5,1.1,2.1), xpd=F)
          abline(h=seq(-100,max(c(back_trans(ChosenAvgLine),back_trans(ChoicePlotqmfit95),
                                  input$EliDose0,input$EliDosemid,input$EliDosehigh)),by=20), v=c(0,8,16), col="gray", lty=3)
          lines(dose,back_trans(ChoicePlotqmfit95),type='l',col='darkgreen',lty=2,lwd=2)
          lines(dose,back_trans(ChoicePlotqmfit05),type='l',col='darkgreen',lty=2,lwd=2)
          points(dose[dnum1],input$EliDose0,cex=2.5,col="darkblue",pch=16)
          points(dose[dnum2],input$EliDosemid,cex=2.5,col="darkblue",pch=16)
          points(dose[dnum3],input$EliDosehigh,cex=2.5,col="darkblue",pch=16)

        }

        ERPlot2_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_uncertPlot.pdf",sep=''),pointsize=10,paper="a4")
          ERPlot2_func()
          dev.off()
        })

        output$ERPlot2 <- renderPlot({
          ERPlot2_func()
          ERPlot2_plot()
        }, height = 820, width = 1200)

        output$ShapePlot1 <- renderPlot({
          par(mfrow=c(3,2),oma = c(0, 0, 2, 0))
          #Mean shape 1
          plot(dose,back_trans(PD_age0),ylim=c(-100,0),lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',
               xlab='Dose (mg/kg/day)',ylab='% change',main='A',cex.main=3)
          lines(dose,back_trans(PD_age1),type='l',col='blue',lty=1,lwd=2)
          lines(dose,back_trans(bias_meanshape1),type='l',col='red',lty=2,lwd=2)
          #Mean shape 2
          plot(dose,back_trans(PD_age0),ylim=c(-100,0),lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',
               xlab='Dose (mg/kg/day)',ylab='% change',main='B',cex.main=3)
          lines(dose,back_trans(PD_age1),type='l',col='blue',lty=1,lwd=2)
          lines(dose,back_trans(bias_meanshape2),type='l',col='red',lty=2,lwd=2)
          #Mean shape 3
          plot(dose,back_trans(PD_age0),ylim=c(-100,0),lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',
               xlab='Dose (mg/kg/day)',ylab='% change',main='C',cex.main=3)
          lines(dose,back_trans(PD_age1),type='l',col='blue',lty=1,lwd=2)
          lines(dose,back_trans(bias_meanshape3),type='l',col='red',lty=2,lwd=2)
          #Mean shape 4
          plot(dose,back_trans(PD_age0),ylim=c(-100,0),lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',
               xlab='Dose (mg/kg/day)',ylab='% change',main='D',cex.main=3)
          lines(dose,back_trans(PD_age1),type='l',col='blue',lty=1,lwd=2)
          lines(dose,back_trans(bias_meanshape4),type='l',col='red',lty=2,lwd=2)
          #Mean shape 5
          plot(dose,back_trans(PD_age0),ylim=c(-100,0),lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',
               xlab='Dose (mg/kg/day)',ylab='% change',main='E',cex.main=3)
          lines(dose,back_trans(PD_age1),type='l',col='blue',lty=1,lwd=2)
          lines(dose,back_trans(bias_meanshape5),type='l',col='red',lty=2,lwd=2)
          #Legend
          plot(1, type="n", axes=F, xlab="", ylab="")
          legend("topleft",c('Adults','Adolescents','Your best guess at the dose-response relationship in children'),cex=2,col=c(1,'blue','red'),lty=c(1,1,2))
        }, height = 900, width = 1200)

        ShapePlot2_func <- function(){
          ChosenAvgLine <- mean_line()
          #Choice A
          qmfitA <- c(ChosenAvgLine[dnum1] + 0.126, ChosenAvgLine[dnum2] + 0.3676, ChosenAvgLine[dnum3] + 1.055)
          osolA <- optim(par=inits1, ofn_q95, q95 = qmfitA, ChosenAvgLine=mean_line())
          bias_varA <- exp(osolA$par[1])^2 + (dose^2)*(osolA$par[2]^2 + exp(osolA$par[3])^2) + 2*dose*exp(osolA$par[1])*osolA$par[2]
          ChoiceqmfitA95 <- qnorm(0.95,ChosenAvgLine,sqrt(bias_varA))
          ChoiceqmfitA05 <- qnorm(0.05,ChosenAvgLine,sqrt(bias_varA))
          #Choice B
          qmfitB <- c(ChosenAvgLine[dnum1] + 0.2, ChosenAvgLine[dnum2] + 0.2, ChosenAvgLine[dnum3] + 0.2)
          osolB <- optim(par=inits1, ofn_q95, q95 = qmfitB, ChosenAvgLine=mean_line())
          bias_varB <- exp(osolB$par[1])^2 + (dose^2)*(osolB$par[2]^2 + exp(osolB$par[3])^2) + 2*dose*exp(osolB$par[1])*osolB$par[2]
          ChoiceqmfitB95 <- qnorm(0.95,ChosenAvgLine,sqrt(bias_varB))
          ChoiceqmfitB05 <- qnorm(0.05,ChosenAvgLine,sqrt(bias_varB))
          #Choice C
          qmfitC <- c(ChosenAvgLine[dnum1] + 0.7, ChosenAvgLine[dnum2] + 0.35, ChosenAvgLine[dnum3] + 0.3)
          osolC <- optim(par=inits1, ofn_q95, q95 = qmfitC, ChosenAvgLine=mean_line())
          bias_varC <- exp(osolC$par[1])^2 + (dose^2)*(osolC$par[2]^2 + exp(osolC$par[3])^2) + 2*dose*exp(osolC$par[1])*osolC$par[2]
          ChoiceqmfitC95 <- qnorm(0.95,ChosenAvgLine,sqrt(bias_varC))
          ChoiceqmfitC05 <- qnorm(0.05,ChosenAvgLine,sqrt(bias_varC))
          #Choice D
          qmfitD <- c(ChosenAvgLine[dnum1] + 0.7, ChosenAvgLine[dnum2] + 0.35, ChosenAvgLine[dnum3] + 1.2)
          osolD <- optim(par=inits1, ofn_q95, q95 = qmfitD, ChosenAvgLine=mean_line())
          bias_varD <- exp(osolD$par[1])^2 + (dose^2)*(osolD$par[2]^2 + exp(osolD$par[3])^2) + 2*dose*exp(osolD$par[1])*osolD$par[2]
          ChoiceqmfitD95 <- qnorm(0.95,ChosenAvgLine,sqrt(bias_varD))
          ChoiceqmfitD05 <- qnorm(0.05,ChosenAvgLine,sqrt(bias_varD))
          par(mfrow=c(3,2),oma = c(0, 0, 2, 0))
          #Uncertainty shape A
          plot(dose,back_trans(ChosenAvgLine),type='l',col='red',lty=1,lwd=2,main='A',cex.main=3,
               ylim=c(-100,80),cex.lab=2,cex.axis=1.8,xlab='Dose (mg/kg/day)',ylab='% change')
          lines(dose,back_trans(ChoiceqmfitA95),type='l',col='darkgreen',lty=2,lwd=2)
          lines(dose,back_trans(ChoiceqmfitA05),type='l',col='darkgreen',lty=2,lwd=2)
          #Uncertainty shape B
          plot(dose,back_trans(ChosenAvgLine),type='l',col='red',lty=1,lwd=2,main='B',cex.main=3,
               ylim=c(-100,80),cex.lab=2,cex.axis=1.8,xlab='Dose (mg/kg/day)',ylab='% change')
          lines(dose,back_trans(ChoiceqmfitB95),type='l',col='darkgreen',lty=2,lwd=2)
          lines(dose,back_trans(ChoiceqmfitB05),type='l',col='darkgreen',lty=2,lwd=2)
          #Uncertainty shape C
          plot(dose,back_trans(ChosenAvgLine),type='l',col='red',lty=1,lwd=2,main='C',cex.main=3,
               ylim=c(-100,80),cex.lab=2,cex.axis=1.8,xlab='Dose (mg/kg/day)',ylab='% change')
          lines(dose,back_trans(ChoiceqmfitC95),type='l',col='darkgreen',lty=2,lwd=2)
          lines(dose,back_trans(ChoiceqmfitC05),type='l',col='darkgreen',lty=2,lwd=2)
          #Uncertainty shape D
          plot(dose,back_trans(ChosenAvgLine),type='l',col='red',lty=1,lwd=2,main='D',cex.main=3,
               ylim=c(-100,80),cex.lab=2,cex.axis=1.8,xlab='Dose (mg/kg/day)',ylab='% change')
          lines(dose,back_trans(ChoiceqmfitD95),type='l',col='darkgreen',lty=2,lwd=2)
          lines(dose,back_trans(ChoiceqmfitD05),type='l',col='darkgreen',lty=2,lwd=2)
          #Legend
          plot(1, type="n", axes=F, xlab="", ylab="")
          legend("topleft",cex=2,col=c('red','darkgreen'),lty=c(1,2),
                 c('Your best guess at the dose-response relationship in children',
                   'Lines indicating your 90% credibility interval around the line of best fit'))
        }

        ShapePlot2_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_uncertShapePlot.pdf",sep=''),pointsize=10,paper="a4")
          ShapePlot2_func()
          dev.off()
        })

        output$ShapePlot2 <- renderPlot({
          ShapePlot2_func()
          ShapePlot2_plot()
        }, height = 900, width = 1200)

        ChoicePlotqmfit05_reactive <- reactive({
          ChosenAvgLine <- mean_line()
          Chosenosol <- reactiveOptim()
          Chosenbias_var <- exp(Chosenosol$par[1])^2 + (dose^2)*(Chosenosol$par[2]^2 + exp(Chosenosol$par[3])^2) + 2*dose*exp(Chosenosol$par[1])*Chosenosol$par[2]
          ChoicePlotqmfit05 <- qnorm(0.05,ChosenAvgLine,sqrt(Chosenbias_var))
        })


        ChoicePlotqmfit95_reactive <- reactive({
          ChosenAvgLine <- mean_line()
          Chosenosol <- reactiveOptim()
          Chosenbias_var <- exp(Chosenosol$par[1])^2 + (dose^2)*(Chosenosol$par[2]^2 + exp(Chosenosol$par[3])^2) + 2*dose*exp(Chosenosol$par[1])*Chosenosol$par[2]
          ChoicePlotqmfit95 <- qnorm(0.95,ChosenAvgLine,sqrt(Chosenbias_var))
        })


        ############# HISTOGRAMS #########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##########

        # Histogram and percentiles for dose 0

        FreqHistInput0 <- reactive({
          FreqHistInput1 <- c(input$n100_n95_0,input$n95_n90_0,input$n90_n85_0,input$n85_n80_0,input$n80_n75_0,input$n75_n70_0,
                              input$n70_n65_0,input$n65_n60_0,input$n60_n55_0,input$n55_n50_0,input$n50_n45_0,input$n45_n40_0,
                              input$n40_n35_0,input$n35_n30_0,input$n30_n25_0,input$n25_n20_0,input$n20_n15_0,input$n15_n10_0,
                              input$n10_n05_0,input$n05_n00_0,input$n00_p05_0,input$p05_p10_0,input$p10_p15_0,input$p15_p20_0,
                              input$p20_p25_0,input$p25_p30_0,input$p30_p35_0,input$p35_p40_0,input$p40_p45_0,input$p45_p50_0,
                              input$p50_p55_0,input$p55_p60_0,input$p60_p65_0,input$p65_p70_0,input$p70_p75_0,input$p75_p80_0,
                              input$p80_p85_0,input$p85_p90_0,input$p90_p95_0,input$p95_p100_0)
          return(FreqHistInput1)
        })

        HistInput0 <- reactive({
          HistInput1 <- NULL
          for(i in 1:length(FreqHistInput0())){
            HistInput1 <- c(HistInput1,rep(center_bin[i],FreqHistInput0()[i]))
          }
          return(HistInput1)
        })

        HistPlot0_func <- function(){
          ChosenAvgLine <- mean_line()
          Chosenosol <- reactiveOptim()
          Chosenbias_var <- exp(Chosenosol$par[1])^2 + (dose^2)*(Chosenosol$par[2]^2 + exp(Chosenosol$par[3])^2) + 2*dose*exp(Chosenosol$par[1])*Chosenosol$par[2]
          ChoicePlotqmfit95 <- qnorm(0.95,ChosenAvgLine,sqrt(Chosenbias_var))
          ChoicePlotqmfit05 <- qnorm(0.05,ChosenAvgLine,sqrt(Chosenbias_var))

          par(mar=c(11,5,1.1,2.1), xpd=TRUE)
          hist(HistInput0(),breaks=bins,ylim=c(0,max(50, max(freqBins1()))),axes=F,col="lightgrey",main='',
               xlab='% change in seizure frequency from baseline',ylab='% chance of average response',xaxs='i')
          axis(2,at=seq(0,max(50, max(freqBins1())),5))
          axis(1, at=seq(-100,100,binwidth))
          arrows(-1, -16, -100, -16, lwd=2)
          arrows(1, -16, 100, -16, lwd=2)
          text(-35, -19, labels = "Improvement in condition", cex=1.5, col="blue")
          text(35, -19, labels = "Deterioration in condition", cex=1.5, col="red")
          par(mar=c(9,5,1.1,2.1), xpd=F)
          abline(h=(1:max(50, max(freqBins1())))*5, v=seq(-100,100,binwidth), col="gray", lty=3)
          abline(v=round(back_trans(mean_line()[dnum1]),digits=1),lty=1,col='red',lwd=2)
          abline(v=round(back_trans(ChoicePlotqmfit95[dnum1]),digits=1),lty=1,col='green',lwd=2)
          abline(v=round(back_trans(ChoicePlotqmfit05[dnum1]),digits=1),lty=1,col='green',lwd=2)
        }

        HistPlot0_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_hist0Plot.pdf",sep=''),pointsize=10,paper="a4")
          HistPlot0_func()
          dev.off()
        })

        HistDensPlot0_func <- function(){
          par(mar=c(11,5,1.1,2.1), xpd=TRUE)
          Dens1 <- density(rep(HistInput0(),5),adjust=1.8)
          hist(rep(HistInput0(),5),breaks=bins,col="lightgrey",main='',ylim=c(0,max(Dens1$y)),
               xlab='% change in seizure frequency from baseline',ylab='Density',freq=F,xaxs='i')
          lines(Dens1)
          axis(1, at=seq(-100,100,binwidth))
          par(mar=c(9,5,1.1,2.1), xpd=F)
          abline(v=round(back_trans(mean_line()[dnum1]),digits=1),lty=1,col='red',lwd=2)
        }

        HistDensPlot0_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_histDens0Plot.pdf",sep=''),pointsize=10,paper="a4")
          HistDensPlot0_func()
          dev.off()
        })

        output$HistPlot0 <- renderPlot({
          if(input$Tick_DensHist0==FALSE){
            HistPlot0_func()
            HistPlot0_plot()
          }else if(input$Tick_DensHist0==TRUE){
            HistDensPlot0_func()
            HistDensPlot0_plot()
          }
        }, height = 600, width = 1900)

        pctiles1 <- reactive({
          v1_choice <- HistInput0()
          freq1 <- NULL
          for(i in 1:length(bins1)){
            freq1[i] <- sum(v1_choice > bins1[i] & v1_choice < bins2[i])
          }
          numin05_1 <- 0.05*sum(freq1)
          numin25_1 <- 0.25*sum(freq1)
          numin75_1 <- 0.75*sum(freq1)
          numin95_1 <- 0.95*sum(freq1)
          low_05_1 <- bins1[min(which(cumsum(freq1/sum(freq1))>0.05))]
          low_25_1 <- bins1[min(which(cumsum(freq1/sum(freq1))>0.25))]
          low_75_1 <- bins1[min(which(cumsum(freq1/sum(freq1))>0.75))]
          low_95_1 <- bins1[min(which(cumsum(freq1/sum(freq1))>0.95))]
          sum_upto05_1 <- sum(freq1[c(which(cumsum(freq1/sum(freq1))<0.05))])
          sum_upto25_1 <- sum(freq1[c(which(cumsum(freq1/sum(freq1))<0.25))])
          sum_upto75_1 <- sum(freq1[c(which(cumsum(freq1/sum(freq1))<0.75))])
          sum_upto95_1 <- sum(freq1[c(which(cumsum(freq1/sum(freq1))<0.95))])
          bins_freq1 <- NULL
          for(i in 1:length(freq1)){
            bins_freq1 <- c(bins_freq1,rep(center_bin[i],freq1[i]))
          }
          pctiles_1 <- c(low_05_1 + (numin05_1 - sum_upto05_1)*binwidth/freq1[min(which(cumsum(freq1/sum(freq1))>0.05))],
                         low_25_1 + (numin25_1 - sum_upto25_1)*binwidth/freq1[min(which(cumsum(freq1/sum(freq1))>0.25))],
                         low_75_1 + (numin75_1 - sum_upto75_1)*binwidth/freq1[min(which(cumsum(freq1/sum(freq1))>0.75))],
                         low_95_1 + (numin95_1 - sum_upto95_1)*binwidth/freq1[min(which(cumsum(freq1/sum(freq1))>0.95))])

          return(pctiles_1)
        })

        freqBins1 <- reactive({
          v1_choice <- HistInput0()
          freq1 <- NULL
          for(i in 1:length(bins1)){
            freq1[i] <- sum(v1_choice > bins1[i] & v1_choice < bins2[i])
          }
          return(freq1)
        })

        output$Meantext1 <- renderText({
          paste("Your best guess, as shown by the red line, at the average response in children on placebo is ",
                round(back_trans(mean_line()[dnum1]),digits=1),".",sep='')
        })

        output$Uncerttext1 <- renderText({
          paste("Your best guesses, as shown by the green lines, at the 5th and 95th percentiles of the average response in children on placebo are ",
                round(back_trans(ChoicePlotqmfit05_reactive()[dnum1]),digits=1)," and ",round(back_trans(ChoicePlotqmfit95_reactive()[dnum1]),digits=1),". You should assign 90% chance between these lines.",sep='')
        })

        # Histogram and percentiles for moderate dose

        FreqHistInputMid <- reactive({
          FreqHistInput2 <- c(input$n100_n95_Mid,input$n95_n90_Mid,input$n90_n85_Mid,input$n85_n80_Mid,input$n80_n75_Mid,input$n75_n70_Mid,
                              input$n70_n65_Mid,input$n65_n60_Mid,input$n60_n55_Mid,input$n55_n50_Mid,input$n50_n45_Mid,input$n45_n40_Mid,
                              input$n40_n35_Mid,input$n35_n30_Mid,input$n30_n25_Mid,input$n25_n20_Mid,input$n20_n15_Mid,input$n15_n10_Mid,
                              input$n10_n05_Mid,input$n05_n00_Mid,input$n00_p05_Mid,input$p05_p10_Mid,input$p10_p15_Mid,input$p15_p20_Mid,
                              input$p20_p25_Mid,input$p25_p30_Mid,input$p30_p35_Mid,input$p35_p40_Mid,input$p40_p45_Mid,input$p45_p50_Mid,
                              input$p50_p55_Mid,input$p55_p60_Mid,input$p60_p65_Mid,input$p65_p70_Mid,input$p70_p75_Mid,input$p75_p80_Mid,
                              input$p80_p85_Mid,input$p85_p90_Mid,input$p90_p95_Mid,input$p95_p100_Mid)
          return(FreqHistInput2)
        })

        HistInputMid <- reactive({
          HistInput2 <- NULL
          for(i in 1:length(FreqHistInputMid())){
            HistInput2 <- c(HistInput2,rep(center_bin[i],FreqHistInputMid()[i]))
          }
          return(HistInput2)
        })

        HistPlotMid_func <- function(){
          ChosenAvgLine <- mean_line()
          Chosenosol <- reactiveOptim()
          Chosenbias_var <- exp(Chosenosol$par[1])^2 + (dose^2)*(Chosenosol$par[2]^2 + exp(Chosenosol$par[3])^2) + 2*dose*exp(Chosenosol$par[1])*Chosenosol$par[2]
          ChoicePlotqmfit95 <- qnorm(0.95,ChosenAvgLine,sqrt(Chosenbias_var))
          ChoicePlotqmfit05 <- qnorm(0.05,ChosenAvgLine,sqrt(Chosenbias_var))

          par(mar=c(11,5,1.1,2.1), xpd=TRUE)
          hist(HistInputMid(),breaks=bins,ylim=c(0,max(50, max(freqBins2()))),axes=F,col="lightgrey",main='',
               xlab='% change in seizure frequency from baseline',ylab='% chance of average response',xaxs='i')
          axis(2,at=seq(0,max(50, max(freqBins2())),5))
          axis(1, at=seq(-100,100,binwidth))
          arrows(-1, -16, -100, -16, lwd=2)
          arrows(1, -16, 100, -16, lwd=2)
          text(-35, -19, labels = "Improvement in condition", cex=1.5, col="blue")
          text(35, -19, labels = "Deterioration in condition", cex=1.5, col="red")
          par(mar=c(9,5,1.1,2.1), xpd=F)
          abline(h=(1:max(50, max(freqBins2())))*5, v=seq(-100,100,binwidth), col="gray", lty=3)
          abline(v=round(back_trans(mean_line()[dnum2]),digits=1),lty=1,col='red',lwd=2)
          abline(v=round(back_trans(ChoicePlotqmfit95[dnum2]),digits=1),lty=1,col='green',lwd=2)
          abline(v=round(back_trans(ChoicePlotqmfit05[dnum2]),digits=1),lty=1,col='green',lwd=2)
        }

        HistPlotMid_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_histMidPlot.pdf",sep=''),pointsize=10,paper="a4")
          HistPlotMid_func()
          dev.off()
        })

        HistDensPlotMid_func <- function(){
          par(mar=c(11,5,1.1,2.1), xpd=TRUE)
          Dens2 <- density(rep(HistInputMid(),5),adjust=1.8)
          hist(rep(HistInputMid(),5),breaks=bins,col="lightgrey",main='',ylim=c(0,max(Dens2$y)),
               xlab='% change in seizure frequency from baseline',ylab='Density',freq=F,xaxs='i')
          lines(Dens2)
          axis(1, at=seq(-100,100,binwidth))
          par(mar=c(9,5,1.1,2.1), xpd=F)
          abline(v=round(back_trans(mean_line()[dnum2]),digits=1),lty=1,col='red',lwd=2)
        }

        HistDensPlotMid_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_histDensMidPlot.pdf",sep=''),pointsize=10,paper="a4")
          HistDensPlotMid_func()
          dev.off()
        })

        output$HistPlotMid <- renderPlot({
          if(input$Tick_DensHistMid==FALSE){
            HistPlotMid_func()
            HistPlotMid_plot()
          }else if(input$Tick_DensHistMid==TRUE){
            HistDensPlotMid_func()
            HistDensPlotMid_plot()
          }
        }, height = 600, width = 1900)

        pctiles2 <- reactive({
          v2_choice <- HistInputMid()
          freq2 <- NULL
          for(i in 1:length(bins1)){
            freq2[i] <- sum(v2_choice > bins1[i] & v2_choice < bins2[i])
          }
          numin05_2 <- 0.05*sum(freq2)
          numin25_2 <- 0.25*sum(freq2)
          numin75_2 <- 0.75*sum(freq2)
          numin95_2 <- 0.95*sum(freq2)
          low_05_2 <- bins1[min(which(cumsum(freq2/sum(freq2))>0.05))]
          low_25_2 <- bins1[min(which(cumsum(freq2/sum(freq2))>0.25))]
          low_75_2 <- bins1[min(which(cumsum(freq2/sum(freq2))>0.75))]
          low_95_2 <- bins1[min(which(cumsum(freq2/sum(freq2))>0.95))]
          sum_upto05_2 <- sum(freq2[c(which(cumsum(freq2/sum(freq2))<0.05))])
          sum_upto25_2 <- sum(freq2[c(which(cumsum(freq2/sum(freq2))<0.25))])
          sum_upto75_2 <- sum(freq2[c(which(cumsum(freq2/sum(freq2))<0.75))])
          sum_upto95_2 <- sum(freq2[c(which(cumsum(freq2/sum(freq2))<0.95))])
          bins_freq2 <- NULL
          for(i in 1:length(freq2)){
            bins_freq2 <- c(bins_freq2,rep(center_bin[i],freq2[i]))
          }
          pctiles_2 <- c(low_05_2 + (numin05_2 - sum_upto05_2)*binwidth/freq2[min(which(cumsum(freq2/sum(freq2))>0.05))],
                         low_25_2 + (numin25_2 - sum_upto25_2)*binwidth/freq2[min(which(cumsum(freq2/sum(freq2))>0.25))],
                         low_75_2 + (numin75_2 - sum_upto75_2)*binwidth/freq2[min(which(cumsum(freq2/sum(freq2))>0.75))],
                         low_95_2 + (numin95_2 - sum_upto95_2)*binwidth/freq2[min(which(cumsum(freq2/sum(freq2))>0.95))])
          return(pctiles_2)
        })

        freqBins2 <- reactive({
          v2_choice <- HistInputMid()
          freq2 <- NULL
          for(i in 1:length(bins1)){
            freq2[i] <- sum(v2_choice > bins1[i] & v2_choice < bins2[i])
          }
          return(freq2)
        })

        output$Meantext2 <- renderText({
          paste("Your best guess, as shown by the red line, at the average response in children on 8 mg/kg/day is ",
                round(back_trans(mean_line()[dnum2]),digits=1),".",sep='')
        })

        output$Uncerttext2 <- renderText({
          paste("Your best guesses, as shown by the green lines, at the 5th and 95th percentiles of the average response in children on 8 mg/kg/day are ",
                round(back_trans(ChoicePlotqmfit05_reactive()[dnum2]),digits=1)," and ",round(back_trans(ChoicePlotqmfit95_reactive()[dnum2]),digits=1),". You should assign 90% chance between these lines.",sep='')
        })

        # Histogram and percentiles for high dose

        FreqHistInputHigh <- reactive({
          FreqHistInput3 <- c(input$n100_n95_High,input$n95_n90_High,input$n90_n85_High,input$n85_n80_High,input$n80_n75_High,input$n75_n70_High,
                              input$n70_n65_High,input$n65_n60_High,input$n60_n55_High,input$n55_n50_High,input$n50_n45_High,input$n45_n40_High,
                              input$n40_n35_High,input$n35_n30_High,input$n30_n25_High,input$n25_n20_High,input$n20_n15_High,input$n15_n10_High,
                              input$n10_n05_High,input$n05_n00_High,input$n00_p05_High,input$p05_p10_High,input$p10_p15_High,input$p15_p20_High,
                              input$p20_p25_High,input$p25_p30_High,input$p30_p35_High,input$p35_p40_High,input$p40_p45_High,input$p45_p50_High,
                              input$p50_p55_High,input$p55_p60_High,input$p60_p65_High,input$p65_p70_High,input$p70_p75_High,input$p75_p80_High,
                              input$p80_p85_High,input$p85_p90_High,input$p90_p95_High,input$p95_p100_High)
          return(FreqHistInput3)
        })

        HistInputHigh <- reactive({
          HistInput3 <- NULL
          for(i in 1:length(FreqHistInputHigh())){
            HistInput3 <- c(HistInput3,rep(center_bin[i],FreqHistInputHigh()[i]))
          }
          return(HistInput3)
        })

        HistPlotHigh_func <- function(){
          ChosenAvgLine <- mean_line()
          Chosenosol <- reactiveOptim()
          Chosenbias_var <- exp(Chosenosol$par[1])^2 + (dose^2)*(Chosenosol$par[2]^2 + exp(Chosenosol$par[3])^2) + 2*dose*exp(Chosenosol$par[1])*Chosenosol$par[2]
          ChoicePlotqmfit95 <- qnorm(0.95,ChosenAvgLine,sqrt(Chosenbias_var))
          ChoicePlotqmfit05 <- qnorm(0.05,ChosenAvgLine,sqrt(Chosenbias_var))

          par(mar=c(11,5,1.1,2.1), xpd=TRUE)
          hist(HistInputHigh(),breaks=bins,ylim=c(0,max(50, max(freqBins3()))),axes=F,col="lightgrey",main='',
               xlab='% change in seizure frequency from baseline',ylab='% chance of average response',xaxs='i')
          axis(2,at=seq(0,max(50, max(freqBins3())),5))
          axis(1, at=seq(-100,100,binwidth))
          arrows(-1, -16, -100, -16, lwd=2)
          arrows(1, -16, 100, -16, lwd=2)
          text(-35, -19, labels = "Improvement in condition", cex=1.5, col="blue")
          text(35, -19, labels = "Deterioration in condition", cex=1.5, col="red")
          par(mar=c(9,5,1.1,2.1), xpd=F)
          abline(h=(1:max(50, max(freqBins3())))*5, v=seq(-100,100,binwidth), col="gray", lty=3)
          abline(v=round(back_trans(mean_line()[dnum3]),digits=1),lty=1,col='red',lwd=2)
          abline(v=round(back_trans(ChoicePlotqmfit95[dnum3]),digits=1),lty=1,col='green',lwd=2)
          abline(v=round(back_trans(ChoicePlotqmfit05[dnum3]),digits=1),lty=1,col='green',lwd=2)
        }

        HistPlotHigh_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_histHighPlot.pdf",sep=''),pointsize=10,paper="a4")
          HistPlotHigh_func()
          dev.off()
        })

        HistDensPlotHigh_func <- function(){
          par(mar=c(11,5,1.1,2.1), xpd=TRUE)
          Dens3 <- density(rep(HistInputHigh(),5),adjust=1.8)
          hist(rep(HistInputHigh(),5),breaks=bins,col="lightgrey",main='',ylim=c(0,max(Dens3$y)),
               xlab='% change in seizure frequency from baseline',ylab='Density',freq=F,xaxs='i')
          lines(Dens3)
          axis(1, at=seq(-100,100,binwidth))
          par(mar=c(9,5,1.1,2.1), xpd=F)
          abline(v=round(back_trans(mean_line()[dnum3]),digits=1),lty=1,col='red',lwd=2)
        }

        HistDensPlotHigh_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_histDensHighPlot.pdf",sep=''),pointsize=10,paper="a4")
          HistDensPlotHigh_func()
          dev.off()
        })

        output$HistPlotHigh <- renderPlot({
          if(input$Tick_DensHistHigh==FALSE){
            HistPlotHigh_func()
            HistPlotHigh_plot()
          }else if(input$Tick_DensHistHigh==TRUE){
            HistDensPlotHigh_func()
            HistDensPlotHigh_plot()
          }
        }, height = 600, width = 1900)

        pctiles3 <- reactive({
          v3_choice <- HistInputHigh()
          freq3 <- NULL
          for(i in 1:length(bins1)){
            freq3[i] <- sum(v3_choice > bins1[i] & v3_choice < bins2[i])
          }
          numin05_3 <- 0.05*sum(freq3)
          numin25_3 <- 0.25*sum(freq3)
          numin75_3 <- 0.75*sum(freq3)
          numin95_3 <- 0.95*sum(freq3)
          low_05_3 <- bins1[min(which(cumsum(freq3/sum(freq3))>0.05))]
          low_25_3 <- bins1[min(which(cumsum(freq3/sum(freq3))>0.25))]
          low_75_3 <- bins1[min(which(cumsum(freq3/sum(freq3))>0.75))]
          low_95_3 <- bins1[min(which(cumsum(freq3/sum(freq3))>0.95))]
          sum_upto05_3 <- sum(freq3[c(which(cumsum(freq3/sum(freq3))<0.05))])
          sum_upto25_3 <- sum(freq3[c(which(cumsum(freq3/sum(freq3))<0.25))])
          sum_upto75_3 <- sum(freq3[c(which(cumsum(freq3/sum(freq3))<0.75))])
          sum_upto95_3 <- sum(freq3[c(which(cumsum(freq3/sum(freq3))<0.95))])
          bins_freq3 <- NULL
          for(i in 1:length(freq3)){
            bins_freq3 <- c(bins_freq3,rep(center_bin[i],freq3[i]))
          }
          pctiles_3 <- c(low_05_3 + (numin05_3 - sum_upto05_3)*binwidth/freq3[min(which(cumsum(freq3/sum(freq3))>0.05))],
                         low_25_3 + (numin25_3 - sum_upto25_3)*binwidth/freq3[min(which(cumsum(freq3/sum(freq3))>0.25))],
                         low_75_3 + (numin75_3 - sum_upto75_3)*binwidth/freq3[min(which(cumsum(freq3/sum(freq3))>0.75))],
                         low_95_3 + (numin95_3 - sum_upto95_3)*binwidth/freq3[min(which(cumsum(freq3/sum(freq3))>0.95))])
          return(pctiles_3)
        })

        freqBins3 <- reactive({
          v3_choice <- HistInputHigh()
          freq3 <- NULL
          for(i in 1:length(bins1)){
            freq3[i] <- sum(v3_choice > bins1[i] & v3_choice < bins2[i])
          }
          return(freq3)
        })

        output$Meantext3 <- renderText({
          paste("Your best guess, as shown by the red line, at the average response in children on 16 mg/kg/day is ",
                round(back_trans(mean_line()[dnum3]),digits=1),".",sep='')
        })


        output$Uncerttext3 <- renderText({
          paste("Your best guesses, as shown by the green lines, at the 5th and 95th percentiles of the average response in children on 16 mg/kg/day are ",
                round(back_trans(ChoicePlotqmfit05_reactive()[dnum3]),digits=1)," and ",round(back_trans(ChoicePlotqmfit95_reactive()[dnum3]),digits=1),". You should assign 90% chance between these lines.",sep='')
        })

        ###########################
        output$text1 <- renderText({
          paste("You have placed ", length(HistInput0()), "%.", sep='')
        })


        output$text2 <- renderText({
          paste("You have placed ", length(HistInputMid()), "%.", sep='')
        })


        output$text3 <- renderText({
          paste("You have placed ", length(HistInputHigh()), "%.", sep='')
        })
        ############################

        ############# END HISTOGRAMS #######~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##########


        #Define reactive optim
        reactiveOptim2 <- reactive({
          dose.optim2 <- rep(dose[dnums],each=4)
          ChosenAvgLine <- mean_line()
          Match_pctiles <- rep(c(0.05,0.25,0.75,0.95),3)
          ofn_pctiles <- function(x,Exp_pctiles){
            ex1 = exp(x[1])
            ex2 = x[2]
            ex3 = exp(x[3])
            bias_var <- ex1^2 + (dose.optim2^2)*(ex2^2 + ex3^2) + 2*dose.optim2*ex1*ex2
            bias.Exp_pctiles <- qnorm(Match_pctiles,ChosenAvgLine[dnums],sqrt(bias_var))
            y = sum(abs(bias.Exp_pctiles - Exp_pctiles))
            return(y)
          }
          Chosenqmfit <- forw_trans(c(pctiles1(),pctiles2(),pctiles3()))
          Chosenosol <- optim(par=inits1, ofn_pctiles, Exp_pctiles = Chosenqmfit, control=list(maxit = 1000))
          return(Chosenosol)
        })

        #Define reactive values for final bias covariance matrix
        reactiveFinalCov2 <- reactive({
          Chosenosol <- reactiveOptim2()
          Choicevar1 <- exp(Chosenosol$par[1])^2
          Choicevar2 <- Chosenosol$par[2]^2 + exp(Chosenosol$par[3])^2
          Choicecov <- exp(Chosenosol$par[1])*Chosenosol$par[2]
          return(c(Choicevar1,Choicevar2,Choicecov,Chosenosol$par))
        })

        ERPlot3_func <- function(){
          ChosenAvgLine <- mean_line()
          Chosenosol <- reactiveOptim2()
          Chosenbias_var <- exp(Chosenosol$par[1])^2 + (dose^2)*(Chosenosol$par[2]^2 + exp(Chosenosol$par[3])^2) + 2*dose*exp(Chosenosol$par[1])*Chosenosol$par[2]
          ChoicePlotqmfit95 <- qnorm(0.95,ChosenAvgLine,sqrt(Chosenbias_var))
          ChoicePlotqmfit05 <- qnorm(0.05,ChosenAvgLine,sqrt(Chosenbias_var))
          ## To add on uncertainty lines from shape choice
          ChosenosolShapes <- reactiveOptim()
          Chosenbias_varShapes <- exp(ChosenosolShapes$par[1])^2 + (dose^2)*(ChosenosolShapes$par[2]^2 + exp(ChosenosolShapes$par[3])^2) + 2*dose*exp(ChosenosolShapes$par[1])*ChosenosolShapes$par[2]
          ChoicePlotqmfit95Shapes <- qnorm(0.95,ChosenAvgLine,sqrt(Chosenbias_varShapes))
          ChoicePlotqmfit05Shapes <- qnorm(0.05,ChosenAvgLine,sqrt(Chosenbias_varShapes))
          ####################
          if(input$Tick_HistShow == FALSE & input$Tick_ShowUncert == FALSE){
            par(mar=c(15,5,1.1,2.1), xpd=TRUE)
            plot(dose,back_trans(ChosenAvgLine),
                 ylim=c(-100,100), lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',col='red', xaxp = c(0,16,2),
                 xlab='Dose (mg/kg/day)',ylab='% change from baseline in seizure frequency',xlim=c(0,18))
            legend(-0.47,-135,cex=1.5, lty=1,lwd=2,"Your best guess at the dose-response relationship in children",col='red')
            par(mar=c(15,5,1.1,2.1), xpd=F)
            abline(h=seq(-100,100,by=25), v=c(0,8,16), col="gray", lty=3)
          }else if(input$Tick_HistShow == FALSE & input$Tick_ShowUncert == TRUE){
            par(mar=c(15,5,1.1,2.1), xpd=TRUE)
            plot(dose,back_trans(ChosenAvgLine), ylim=c(-100,100), lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',col='red',
                 xaxp = c(0,16,2), xlab='Dose (mg/kg/day)',ylab='% change from baseline in seizure frequency',xlim=c(0,18))
            legend(-0.47,-135,cex=1.5, lty=c(1,2),lwd=2,c("Your best guess at the dose-response relationship in children",
                                                          "Lines indicating your 90% credibility interval around the line of best fit"),col=c('red','darkgreen'))
            par(mar=c(15,5,1.1,2.1), xpd=F)
            abline(h=seq(-100,100,by=25),  v=c(0,8,16), col="gray", lty=3)
            lines(dose,back_trans(ChoicePlotqmfit95),type='l',col='darkgreen',lty=2,lwd=2)
            lines(dose,back_trans(ChoicePlotqmfit05),type='l',col='darkgreen',lty=2,lwd=2)
            if(input$Tick_ShapeUncertShow == TRUE){
              lines(dose,back_trans(ChoicePlotqmfit95Shapes),type='l',col='green',lty=2,lwd=2)
              lines(dose,back_trans(ChoicePlotqmfit05Shapes),type='l',col='green',lty=2,lwd=2)
            }else{}
          }else if(input$Tick_HistShow == TRUE & input$Tick_ShowUncert == FALSE){
            par(mar=c(15,5,1.1,2.1), xpd=TRUE)
            plot(dose,back_trans(ChosenAvgLine),
                 ylim=c(-100,100), lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',col='red', xaxp = c(0,16,2),
                 xlab='Dose (mg/kg/day)',ylab='% change from baseline in seizure frequency',xlim=c(0,18))
            bins1 <- freqBins1()
            bins1[which(bins1==0)] <-NA
            bins2 <- freqBins2()
            bins2[which(bins2==0)] <-NA
            bins3 <- freqBins3()
            bins3[which(bins3==0)] <-NA
            par(new=TRUE,fig=c(0.093, 0.203, 0, 1),mar=c(15,0,1.1,2.1))
            barplot(bins1 , axes=FALSE, space=0, horiz=TRUE)
            if(input$Tick_DensShow == TRUE){
              Dens1 <- density(HistInput0(),adjust=1.8)
              lines(length(HistInput0())*binwidth*Dens1$y,(Dens1$x+100)/binwidth)
            }
            par(new=TRUE,fig=c(0.47, 0.58, 0, 1),mar=c(15,0,1.1,2.1))
            barplot(bins2, axes=FALSE, space=0, horiz=TRUE)
            if(input$Tick_DensShow == TRUE){
              Dens2 <- density(HistInputMid(),adjust=1.8)
              lines(length(HistInputMid())*binwidth*Dens2$y,(Dens2$x+100)/binwidth)
            }
            par(new=TRUE,fig=c(0.847, 0.957, 0, 1),mar=c(15,0,1.1,2.1))
            barplot(bins3 , axes=FALSE, space=0, horiz=TRUE)
            if(input$Tick_DensShow == TRUE){
              Dens3 <- density(HistInputHigh(),adjust=1.8)
              lines(length(HistInputHigh())*binwidth*Dens3$y,(Dens3$x+100)/binwidth)
            }
            par(mar=c(15,5,1.1,2.1), new=TRUE, par(bg=NA),fig=c(0, 1, 0, 1))
            plot(dose,back_trans(ChosenAvgLine),ylim=c(-100,100), lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',
                 col='red', xaxp = c(0,16,2), xlab='',ylab='',xlim=c(0,18))
            legend(-0.47,-135,cex=1.5, lty=1,lwd=2,"Your best guess at the dose-response relationship in children",col='red')
            par(mar=c(15,5,1.1,2.1), xpd=F)
            abline(h=seq(-100,100,by=25), v=c(0,8,16), col="gray", lty=3)
          }else if(input$Tick_HistShow == TRUE & input$Tick_ShowUncert == TRUE){
            par(mar=c(15,5,1.1,2.1), xpd=TRUE)
            plot(dose,back_trans(ChosenAvgLine), ylim=c(-100,100), lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',col='red',
                 xaxp = c(0,16,2), xlab='Dose (mg/kg/day)',ylab='% change from baseline in seizure frequency',xlim=c(0,18))
            legend(-0.47,-135,cex=1.5, lty=c(1,2),lwd=2,col=c('red','darkgreen'),
                   c("Your best guess at the dose-response relationship in children",
                     "Lines indicating your 90% credibility interval around the line of best fit"))
            bins1 <- freqBins1()
            bins1[which(bins1==0)] <-NA
            bins2 <- freqBins2()
            bins2[which(bins2==0)] <-NA
            bins3 <- freqBins3()
            bins3[which(bins3==0)] <-NA
            par(new=TRUE,fig=c(0.093, 0.203, 0, 1),mar=c(15,0,1.1,2.1))
            barplot(bins1 , axes=FALSE, space=0, horiz=TRUE)
            if(input$Tick_DensShow == TRUE){
              Dens1 <- density(HistInput0(),adjust=1.8)
              lines(length(HistInput0())*binwidth*Dens1$y,(Dens1$x+100)/binwidth)
            }
            par(new=TRUE,fig=c(0.47, 0.58, 0, 1),mar=c(15,0,1.1,2.1))
            barplot(bins2, axes=FALSE, space=0, horiz=TRUE)
            if(input$Tick_DensShow == TRUE){
              Dens2 <- density(HistInputMid(),adjust=1.8)
              lines(length(HistInputMid())*binwidth*Dens2$y,(Dens2$x+100)/binwidth)
            }
            par(new=TRUE,fig=c(0.847, 0.957, 0, 1),mar=c(15,0,1.1,2.1))
            barplot(bins3 , axes=FALSE, space=0, horiz=TRUE)
            if(input$Tick_DensShow == TRUE){
              Dens3 <- density(HistInputHigh(),adjust=1.8)
              lines(length(HistInputHigh())*binwidth*Dens3$y,(Dens3$x+100)/binwidth)
            }
            par(mar=c(15,5,1.1,2.1), new=TRUE, par(bg=NA),fig=c(0, 1, 0, 1))
            plot(dose,back_trans(ChosenAvgLine),ylim=c(-100,100), lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',
                 col='red', xaxp = c(0,16,2), xlab='',ylab='',xlim=c(0,18))
            par(mar=c(15,5,1.1,2.1), xpd=F)
            abline(h=seq(-100,100,by=25), v=c(0,8,16), col="gray", lty=3)
            lines(dose,back_trans(ChoicePlotqmfit95),type='l',col='darkgreen',lty=2,lwd=2)
            lines(dose,back_trans(ChoicePlotqmfit05),type='l',col='darkgreen',lty=2,lwd=2)
            if(input$Tick_ShapeUncertShow == TRUE){
              lines(dose,back_trans(ChoicePlotqmfit95Shapes),type='l',col='green',lty=2,lwd=2)
              lines(dose,back_trans(ChoicePlotqmfit05Shapes),type='l',col='green',lty=2,lwd=2)
            }else{}
          }
        }

        ERPlot3_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_uncertPlot2.pdf",sep=''),pointsize=10,paper="a4")
          ERPlot3_func()
          dev.off()
        })

        output$ERPlot3 <- renderPlot({
          ERPlot3_func()
          ERPlot3_plot()
        }, height = 820, width = 1200)


        FinalPlot_func <- function(){
          ChosenAvgLine <- mean_line()
          Chosenosol <- reactiveOptim2()
          Chosenbias_var <- exp(Chosenosol$par[1])^2 + (dose^2)*(Chosenosol$par[2]^2 + exp(Chosenosol$par[3])^2) + 2*dose*exp(Chosenosol$par[1])*Chosenosol$par[2]
          ChoicePlotqmfit95 <- qnorm(0.95,ChosenAvgLine,sqrt(Chosenbias_var))
          ChoicePlotqmfit05 <- qnorm(0.05,ChosenAvgLine,sqrt(Chosenbias_var))
          par(mar=c(15,5,1.1,2.1), xpd=TRUE)
          plot(dose,back_trans(PD_age0),lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',xaxp = c(0,16,2),xlab='Dose (mg/kg/day)',
               ylim=c(-100,max(c(0,back_trans(ChosenAvgLine),back_trans(ChoicePlotqmfit95)))),
               ylab='% change from baseline in seizure frequency')
          lines(dose,back_trans(PD_age1),type='l',col='blue',lty=1,lwd=2)
          lines(dose,back_trans(ChosenAvgLine),type='l',col='red',lty=1,lwd=2)
          legend(-0.47,-120,cex=1.5,c("Adult line of best fit", "Adolescent line of best fit",
                                      "Your best guess at the dose-response relationship in children",
                                      "Lines indicating your 90% credibility interval around the line of best fit"),
                 col=c(1,'blue','red','darkgreen'),
                 lty=c(1,1,1,2),lwd=2) #3
          par(mar=c(15,5,1.1,2.1), xpd=F)
          abline(h=seq(-100,max(c(0,back_trans(ChosenAvgLine),back_trans(ChoicePlotqmfit95))),by=20),
                 v=c(0,8,16), col="gray", lty=3)
          lines(dose,back_trans(ChoicePlotqmfit95),type='l',col='darkgreen',lty=2,lwd=2)
          lines(dose,back_trans(ChoicePlotqmfit05),type='l',col='darkgreen',lty=2,lwd=2)
          if(input$check_ChildrenData == TRUE){
            set.seed(101)
            ChildData <- NULL
            WhichChildDose <- NULL
            for(i in 1:170){
              sampPlaceNum <- sample(1:length(dose),size=1)
              ChildData[i] <- ChosenAvgLine[sampPlaceNum] + rnorm(1,0,sqrt(Chosenbias_var[sampPlaceNum]))
              WhichChildDose[i] <- dose[sampPlaceNum]
            }
            points(WhichChildDose,back_trans(ChildData),pch=16,col='red',cex=0.8)
          }
        }

        FinalPlot_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_finalPlot.pdf",sep=''),pointsize=10,paper="a4")
          FinalPlot_func()
          dev.off()
        })

        output$FinalPlot <- renderPlot({
          FinalPlot_func()
          FinalPlot_plot()
          save_output()
        }, height = 820, width = 1200)

        FinalLogPlot_func <- function(){
          ChosenAvgLine <- mean_line()
          Chosenosol <- reactiveOptim2()
          Chosenbias_var <- exp(Chosenosol$par[1])^2 + (dose^2)*(Chosenosol$par[2]^2 + exp(Chosenosol$par[3])^2) + 2*dose*exp(Chosenosol$par[1])*Chosenosol$par[2]
          ChoicePlotqmfit95 <- qnorm(0.95,ChosenAvgLine,sqrt(Chosenbias_var))
          ChoicePlotqmfit05 <- qnorm(0.05,ChosenAvgLine,sqrt(Chosenbias_var))
          par(mar=c(15,5,1.1,2.1), xpd=TRUE)
          plot(dose,PD_age0,lty=1,lwd=2,cex.lab=2,cex.axis=1.8,type='l',xaxp = c(0,16,2), xlab='Dose (mg/kg/day)',
               ylim=c(2.3,max(c(4.7,ChosenAvgLine,ChoicePlotqmfit95))),
               ylab='Log % change from baseline in seizure frequency')
          lines(dose,PD_age1,type='l',col='blue',lty=1,lwd=2)
          lines(dose,ChosenAvgLine,type='l',col='red',lty=1,lwd=2)
          legend(-0.47,1.8,cex=1.5,c("Adult line of best fit", "Adolescent line of best fit",
                                     "Your best guess at the dose-response relationship in children",
                                     "Lines indicating your 90% credibility interval around the line of best fit"),
                 col=c(1,'blue','red','darkgreen'),
                 lty=c(1,1,1,2),lwd=2) #3
          par(mar=c(15,5,1.1,2.1), xpd=F)
          abline(h=seq(2.3,max(c(4.7,ChosenAvgLine,ChoicePlotqmfit95)),by=20),
                 v=c(0,8,16), col="gray", lty=3)
          lines(dose,ChoicePlotqmfit95,type='l',col='darkgreen',lty=2,lwd=2)
          lines(dose,ChoicePlotqmfit05,type='l',col='darkgreen',lty=2,lwd=2)
          if(input$check_ChildrenData == TRUE){
            set.seed(101)
            ChildData <- NULL
            WhichChildDose <- NULL
            for(i in 1:170){
              sampPlaceNum <- sample(1:length(dose),size=1)
              ChildData[i] <- ChosenAvgLine[sampPlaceNum] + rnorm(1,0,sqrt(Chosenbias_var[sampPlaceNum]))
              WhichChildDose[i] <- dose[sampPlaceNum]
            }
            points(WhichChildDose,ChildData,pch=16,col='red',cex=0.8)
          }
        }

        FinalLogPlot_plot <- reactive({
          pdf(paste(OUT_dir,input$expert,"_LogfinalPlot.pdf",sep=''),pointsize=10,paper="a4")
          FinalLogPlot_func()
          dev.off()
        })

        output$FinalLogPlot <- renderPlot({
          FinalLogPlot_func()
          FinalLogPlot_plot()
        }, height = 820, width = 1200)

        save_output <- reactive({
          write.table(data.frame(names=c("bias_mean1","bias_mean2","bias_var1","bias_var2","bias_cov","loga","b","logc"),
                                 x=c(AvgInt(),AvgSlo(),reactiveFinalCov2())),
                      file=paste(OUT_dir,input$expert,"_output.txt",sep=''))
          write.table(data.frame(low_bin=bins1,upp_bin=bins2, freq = freqBins1()),file=paste(input$expert,"_bins_freq1.txt",sep=''))
          write.table(data.frame(low_bin=bins1,upp_bin=bins2, freq = freqBins2()),file=paste(input$expert,"_bins_freq2.txt",sep=''))
          write.table(data.frame(low_bin=bins1,upp_bin=bins2, freq = freqBins3()),file=paste(input$expert,"_bins_freq3.txt",sep=''))
        })
      }
        )

}

