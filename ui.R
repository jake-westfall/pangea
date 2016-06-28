library(shiny)

shinyUI(fluidPage(title="PANGEA: Power ANalysis for GEneral Anova designs",
  h1("PANGEA (v0.2):"),
  h2("Power ANalysis for GEneral Anova designs"),
  p("PANGEA is the first power analysis program for general ANOVA designs (e.g., Winer, Brown, & Michels, 1991). PANGEA can handle designs with any number of factors, each with any number of levels; any factor can be treated as fixed or random; and any valid pattern of nesting or crossing of the factors is allowed. See the drop-down menu below for specific examples of designs covered by PANGEA."),
  p("Download the",a("PANGEA working paper", href="http://jakewestfall.org/publications/pangea.pdf"),"for all the technical info on how PANGEA works. Get the source code",a("here",href="https://github.com/jake-westfall/pangea"),"."),
  helpText("Note: when sharing the link to this app, please use the stable redirecting page at",
           a("jakewestfall.org/pangea/,", href="http://jakewestfall.org/pangea/"),
           "as the app's current URL is possibly subject to change."),
  
# step 1 ------------------------------------------------------------------

  h3("Step 1: Specify the design"),
  p("Use the buttons below to load an example design, or use the controls to specify your own design."),
  selectInput("example", label=NULL, 
    choices = c("Custom design",
                "1. 2-group between subjects (participants = replicates)",
                "2. 2-group between subjects (participants = explicit factor)",
                "3. 2*3 mixed (within*between)",
                "4. 3-level design: Pupils (replicates)-in-Classrooms-in-Schools)",
                "5. Participants crossed w/ random Stimuli-in-Treatments (Clark, 1973)",
                "6. Participants crossed w/ random Stimuli, counter-balanced (Kenny & Smith, 1980)")),

  p(strong("How many factors?"),
    "Use the buttons below to specify the",em("number of factors")," in the design. The algorithm underlying PANGEA can technically handle any number of factors. The user interface is currently set up to allow up to 12 factors. Practically speaking, the app runs slowly when analyzing designs with more than 8 factors."),
  fluidRow(
    column(2, actionButton("addFac", "Add factor")),
    column(2, actionButton("delFac", "Remove factor"))
  ),
  p(),

  div(strong("What are they called?"),
      "Only the first letter of each name will be used by the rest of the app, so make sure each factor has a unique first letter! Remember that it very often makes sense to represent the Participants as an explicit factor in the design, and in fact is necessary if there is at least one within-subject-varying predictor. As an example, the default inputs describe a simple two-color",a("Stroop task", href="http://en.wikipedia.org/wiki/Stroop_effect")," where the InkColor*WordColor (I*W) interaction represents the Stroop effect; for other examples, see the drop-down menu above."),
  do.call(fluidRow, lapply(1:12, function(x){
    column(1, htmlOutput(paste0("get_f",x,"Lab")))
  })),
  p(),

  div(strong("Which are random?"),
      "For each factor, check the corresponding box if it is a",em("random"),"factor. The fixed vs. random distinction is not always completely unambiguous, but some useful discussion of what it means can be found",a("HERE",href="http://andrewgelman.com/2005/01/25/why_i_dont_use/"),"or",a("HERE",href="http://stats.stackexchange.com/questions/111905/what-is-the-upside-of-treating-a-factor-as-random-in-a-mixed-model"),". Also see the References at the bottom of this page."),
  do.call(fluidRow, lapply(1:12, function(x){
    column(1, htmlOutput(paste0("get_f",x,"Rand")))
  })),
  p(),
  
  div(strong("Which are nested in which (if any)?"),
      "For each factor, enter the names of the other factors that are",em("nested in")," it. Factors are assumed to be crossed by default unless nesting is explicitly indicated. For example, if the factor in question is Treatments (T), and both the School (S) and Pupil (P) factors are nested in Treatments, then in the box below labeled \"T\" you should enter:",em("SP"),"(or, equivalently, ",em("PS"),"). If all the boxes are left blank, then all factors in the design are fully crossed (as in, e.g., a fully within-subjects design). If this is confusing, studying the example designs from the drop-down menu above may help."),
  do.call(fluidRow, lapply(1:12, function(x){
    column(1, htmlOutput(paste0("get_f",x,"Nest")))
  })),
  p(),

  p(strong("How many levels of each",em("fixed"),"factor?"),
    "We'll worry about the random factors in the next step."),
  do.call(fluidRow, lapply(1:12, function(x){
    column(1, htmlOutput(paste0("get_f",x,"FixNum")))
  })),

  conditionalPanel(condition = "input.f1FixNum > 2",
                   p(), htmlOutput("f1FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f1FixContrast",x)))
                   }))),
  conditionalPanel(condition = "input.f2FixNum > 2",
                   p(), htmlOutput("f2FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f2FixContrast",x)))
                   }))),
  conditionalPanel(condition = "input.f3FixNum > 2",
                   p(), htmlOutput("f3FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f3FixContrast",x)))
                   }))),
  conditionalPanel(condition = "input.f4FixNum > 2",
                   p(), htmlOutput("f4FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f4FixContrast",x)))
                   }))),
  conditionalPanel(condition = "input.f5FixNum > 2",
                   p(), htmlOutput("f5FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f5FixContrast",x)))
                   }))),
  conditionalPanel(condition = "input.f6FixNum > 2",
                   p(), htmlOutput("f6FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f6FixContrast",x)))
                   }))),
  conditionalPanel(condition = "input.f7FixNum > 2",
                   p(), htmlOutput("f7FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f7FixContrast",x)))
                   }))),
  conditionalPanel(condition = "input.f8FixNum > 2",
                   p(), htmlOutput("f8FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f8FixContrast",x)))
                   }))),
  conditionalPanel(condition = "input.f9FixNum > 2",
                   p(), htmlOutput("f9FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f9FixContrast",x)))
                   }))),
  conditionalPanel(condition = "input.f10FixNum > 2",
                   p(), htmlOutput("f10FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f10FixContrast",x)))
                   }))),
  conditionalPanel(condition = "input.f11FixNum > 2",
                   p(), htmlOutput("f11FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f11FixContrast",x)))
                   }))),
  conditionalPanel(condition = "input.f12FixNum > 2",
                   p(), htmlOutput("f12FixContrastPrompt"),
                   do.call(fluidRow, lapply(1:12, function(x){
                     column(1, htmlOutput(paste0("get_f12FixContrast",x)))
                   }))),
  p(),

  fluidRow(
    column(4, htmlOutput("get_which"))
  ),
  p(),

  p("When you are finished specifying the design, hit the \"Submit Design\" button below to begin entering the experimental parameters."),
  htmlOutput("submitAndEMS"),

  htmlOutput("step1ErrorPrompt"),
  htmlOutput("step1ErrorBox"),

  htmlOutput("EMSoutput"),

# step 2 ------------------------------------------------------------------

  h3("Step 2: Enter the experimental parameters"),
  htmlOutput("solveInstrs"),
  htmlOutput("step2info"),
  p(),

  fluidRow(
    column(2, htmlOutput("get_ES")),
    column(6, htmlOutput("get_numReps"))
  ),
  p(),
  do.call(fluidRow, lapply(1:12, function(x){
    column(1, htmlOutput(paste0("get_f",x,"RandNum")))
  })),
  p(),
  do.call(fluidRow, lapply(1:12, function(x){
    column(1, htmlOutput(paste0("get_f",x,"Var")))
  })),

  p(),
  htmlOutput("solve"),

  htmlOutput("step2ErrorPrompt"),
  htmlOutput("step2ErrorBox"),

# step 3 ------------------------------------------------------------------

  h3("Step 3: Profit!"),

  verbatimTextOutput("ans"),
  p("The noncentrality parameter is for a noncentral t distribution. It can be interpreted as the expected value of the t-statistic under the alternative hypothesis (this is technically not exactly true, but very close to true, and easier to remember). Degrees of freedom are computed using the", a("Welch-Satterthwaite equation,",href="http://en.wikipedia.org/wiki/Welch%E2%80%93Satterthwaite_equation"),"and so will often be a non-integer value when there are multiple random factors."),
  
  h3("References and Contact info"),
  p("Clark, H. H. (1973). The language-as-fixed-effect fallacy: A critique of language statistics in psychological research.",em("Journal of verbal learning and verbal behavior, 12"),"(4), 335-359.",
    a("[Link to Article PDF]", href="http://acs.ist.psu.edu/papers/clark73.pdf")),
  p("Judd, C. M., Westfall, J., & Kenny, D. A. (2012). Treating stimuli as a random factor in social psychology: A new and comprehensive solution to a pervasive but largely ignored problem.",em("Journal of Personality and Social Psychology, 103"),"(1), 54-69.",
    a("[Link to Article PDF]", href="http://jakewestfall.org/publications/JWK.pdf")),
  p("Judd, C. M., Westfall, J., & Kenny, D. A. (2016). Experiments with more than one random factor: Designs, analytic models, and statistical power.",em("Annual Review of Psychology."),
    a("[Link to Article PDF]", href="http://jakewestfall.org/publications/JWK_AnnRev.pdf")),
  p("Kenny, D. A., & Smith, E. R. (1980).  Note on the analysis of designs in which subjects receive each stimulus only once.",em("Journal of Experimental Social Psychology, 16,"),"497-507.",
    a("[Link to Article PDF]", href="http://jakewestfall.org/KennySmithEMS.pdf")),
  p("Richard, F. D., Bond Jr, C. F., & Stokes-Zoota, J. J. (2003). One hundred years of social psychology quantitatively described.",em("Review of General Psychology, 7"),"(4), 331-363.",
    a("[Link to Article PDF]", href="http://www2.psych.ubc.ca/~schaller/Psyc591Readings/RichardBondStokes-Zoota2003.pdf")),
  p("Westfall, J., Judd, C. M., & Kenny, D. A. (2015). Replicating studies in which samples of participants respond to samples of stimuli.",em("Perspectives on Psychological Science, 10"),"(3), 390-399.",
    a("[Link to Article PDF]", href="http://jakewestfall.org/publications/WJK2015.pdf")),
  p("Westfall, J., Kenny, D. A., & Judd, C. M. (2014). Statistical power and optimal design in experiments in which samples of participants respond to samples of stimuli.",em("Journal of Experimental Psychology: General, 143"),"(5), 2020-2045.",
    a("[Link to Article PDF]", href="http://jakewestfall.org/publications/crossed_power_JEPG.pdf")),
  p("Winer, B. J., Brown, D. R., & Michels, K. M. (1991). Statistical principles in experimental design (3rd edition). McGraw-Hill, New York."),
  p("Contact me with comments/questions/suggestions/bug reports: jake.westfall@utexas.edu"),  
  p(a("[Back to JakeWestfall.org]", href="http://jakewestfall.org"))

)) # end call to shinyUI()