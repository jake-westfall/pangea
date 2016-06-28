library(shiny)

shinyServer(function(input, output, session) {

  # declare global variables
  ems <- NA
  facs <- NA
  rands <- NA
  nests <- NA
  randArg <- NA
  fixedFacs <- NA
  fixedSimples <- NA
  num <- NA
  denom <- NA
  varLabs <- NA
  defaultLabs <- c("Participants", "InkColor", "WordColor", LETTERS[1:10][-9])
  defaultRands <- rep.int(c(TRUE, FALSE), c(1, 11))
  fixNumLabs <- rep(NA, 12)
  step2clear <- TRUE
  fixContrastPrompts <- rep(NA, 12)
  fixContrastMat <- matrix(NA, nrow=12, ncol=12)
  labs <- rep(FALSE, 12)
  randLabs <- rep(NA, 12)
  nestLabs <- rep(NA, 12)
  
  # define error messages
  intError <- "Number of fixed factor levels must be an integer greater than 1."
  allRandError <- "There must be at least 1 fixed factor in the design."
  dupError <- "At least two factor names have duplicate first letters."
  randIntError <- "Number of random factor levels must be an integer greater than 1."
  repsError <- "Number of replicates must be an integer greater than 0."
  ESerror <- "Effect size (Cohen's d) must be a real number."
  sumVPC <- "Variance Partitioning Coefficients (VPCs) must be numbers between 0 and 1, and their sum must be less than or equal to 1 -- see \"Variance component information.\""
  
  # define REACTIVE global variables
  # start off with 3 factors (2-color Stroop task)
  values <- reactiveValues(numFacs = 3, numReps = 1, defaults=NA,
    solve=0, step1errors=character(0), step2errors=character(0))
  
# step 1: specify design --------------------------------------------------

  # initialize "add factor" and "remove factor" buttons
  # in separate reactive blocks
  observe({
    numFacs <- isolate(values$numFacs)
    if(input$addFac > 0){
      if(numFacs < 12) values$numFacs <- numFacs + 1
    }
  })
  observe({
    numFacs <- isolate(values$numFacs)
    if(input$delFac > 0){
      if(numFacs > 1) values$numFacs <- numFacs - 1
    }
  })

# write Labels row --------------------------------------------------------

  observe({
# t1 <- Sys.time()
    lapply(seq(values$numFacs), function(x){
      lab <- isolate(input[[paste0("f",x,"Lab")]])
      if(!labs[x]){
        output[[paste0("get_f",x,"Lab")]] <- renderUI({
          textInputMini(paste0("f",x,"Lab"), label = NULL,
                        value=ifelse(is.null(lab), defaultLabs[x], lab))
        })
        labs[x] <<- TRUE
      }
    })
    lapply(setdiff(1:12, seq(values$numFacs)), function(x){
      if(labs[x]){
        output[[paste0("get_f",x,"Lab")]] <- renderUI(div())
        labs[x] <<- FALSE
      }
    })
# print(paste("labels =", round(c(difftime(Sys.time(), t1)*1000)), "ms"))
  })
  
# write Random row --------------------------------------------------------

  observe({
# t1 <- Sys.time()
    lapply(seq(values$numFacs), function(x){
      lab <- input[[paste0("f",x,"Lab")]]
      if(is.na(randLabs[x]) || randLabs[x] != lab){
        output[[paste0("get_f",x,"Rand")]] <- renderUI({
          checkboxInput(paste0("f",x,"Rand"),
                        label = ifelse(length(lab) > 0, toupper(substr(lab,1,1)), ""),
                        value=ifelse(!is.null(lab),
                                     isolate(input[[paste0("f",x,"Rand")]]),
                                     defaultRands[x]))
        })
        randLabs[x] <<- ifelse(length(lab) > 0, toupper(substr(lab,1,1)), "")
      }
    })
    lapply(setdiff(1:12, seq(values$numFacs)), function(x){
      if(!is.na(randLabs[x])){
        output[[paste0("get_f",x,"Rand")]] <- renderUI(div())
        randLabs[x] <<- NA
      }
    })
# print(paste("random =", round(c(difftime(Sys.time(), t1)*1000)), "ms"))
  })
  
# write Nested row --------------------------------------------------------

  observe({
# t1 <- Sys.time()
    lapply(seq(values$numFacs), function(x){
      lab <- input[[paste0("f",x,"Lab")]]
      if(is.na(nestLabs[x]) || nestLabs[x] != lab){
        output[[paste0("get_f",x,"Nest")]] <- renderUI({
          textInputMini(paste0("f",x,"Nest"),
                        label = ifelse(length(lab) > 0, toupper(substr(lab,1,1)), ""),
                        value=ifelse(!is.null(lab), isolate(input[[paste0("f",x,"Nest")]]), ""))
        })
        nestLabs[x] <<- ifelse(length(lab) > 0, toupper(substr(lab,1,1)), "")
      }
    })
    lapply(setdiff(1:12, seq(values$numFacs)), function(x){
      lab <- input[[paste0("f",x,"Lab")]]
      if(!is.na(nestLabs[x])){
        output[[paste0("get_f",x,"Nest")]] <- renderUI(div())
        nestLabs[x] <<- NA
      }
    })
# print(paste("nested =", round(c(difftime(Sys.time(), t1)*1000)), "ms"))
  })

# compute EMSs and store in "ems" object ----------------------------------

  observe({
# t1 <- Sys.time()
# t2 <- Sys.time()
    # depends on: add/remove factor, label/rand/nest inputs
    
    facs <<- toupper(substr(lapply(seq(as.numeric(values$numFacs)), function(x){
      tolower(input[[paste0("f",x,"Lab")]])
    }),1,1))  
    rands <<- sapply(seq(as.numeric(values$numFacs)), function(x){
      input[[paste0("f",x,"Rand")]]
    })
    nests <<- sapply(seq(as.numeric(values$numFacs)), function(x){
      tolower(input[[paste0("f",x,"Nest")]])
    })
# print(paste("ems.assignGlobals =", round(c(difftime(Sys.time(), t2)*1000)), "ms"))
    if(!any(sapply(rands, is.null)) && any(facs %in% LETTERS)){
      # throw error if duplicate factor names
      if(length(unique(facs)) < length(facs) &&
           !dupError %in% isolate(values$step1errors)){
        values$step1errors <- c(isolate(values$step1errors), dupError)
      }
      # remove error if not duplicate factor names
      if(length(unique(facs)) == length(facs) &&
           dupError %in% isolate(values$step1errors)){
        ind <- match(dupError, isolate(values$step1errors))
        values$step1errors <- isolate(values$step1errors)[-ind]
      }
      
# t2 <- Sys.time()
      # grab input and format arguments for EMS() function
      live <- which(facs %in% LETTERS)
      repChar <- tolower(tail(setdiff(LETTERS, facs[live]), 1))
      formArg <- paste(repChar,"~",paste(facs[live], collapse="*"),
                       collapse="")
      randArg <<- paste(facs[facs %in% LETTERS & rands], collapse="")
      nestArg <- unlist(lapply(live, function(x){
        arg <- toupper(unlist(strsplit(nests[x], split="")))
        arg <- paste0(facs[x], "/", arg[arg %in% facs[live]])
        arg[nchar(arg) > 2]
      }))
      
      # call EMS() function
      if(length(nestArg) > 0){
        ems <<- EMS(as.formula(formArg), random=randArg,
                   nested=nestArg)
      } else{
        ems <<- EMS(as.formula(formArg), random=randArg,
                   nested=NULL)
      }
# print(paste("ems.EMSfunction =", round(c(difftime(Sys.time(), t2)*1000)), "ms"))    
      
      # fill in "which effect" power choices if there are fixed facs
      # otherwise clear those UIs and throw an error
      if(!all(rands)){
# t2 <- Sys.time()
        fixedFacs <<- head(rownames(ems), -1)
        if(randArg != ""){
          ind <- sapply(unlist(strsplit(randArg, split="")), function(x){
            !grepl(x, fixedFacs, fixed=TRUE)
          })
          fixedFacs <<- fixedFacs[apply(ind, 1, all)]
        }
        
        output$get_which <- renderUI({
          selectInput("which", choices=fixedFacs,
            label="Which fixed effect do you want to compute the statistical power to detect?")
        })
# print(paste("ems.getWhich =", round(c(difftime(Sys.time(), t2)*1000)), "ms"))    

# get numbers of fixed factor levels --------------------------------------

# t2 <- Sys.time()
        # select only simple effects, not interactions
        fixedSimples <<- fixedFacs[!grepl("*", fixedFacs, fixed=TRUE)]
        
        # throw error if input is not an integer >= 2
        nonNumerics <- sapply(seq(length(fixedSimples)), function(x){
          fixnum <- input[[paste0("f",x,"FixNum")]]
          suppressWarnings(return(is.na(as.numeric(fixnum)) ||
                                  as.numeric(fixnum) %% 1 > 0 ||
                                  as.numeric(fixnum) < 2))
        })
        # ...but only if no arguments are NULL
        if(!any(sapply(nonNumerics, is.na))){
          if(any(nonNumerics) && !intError %in% isolate(values$step1errors)){
            values$step1errors <- c(isolate(values$step1errors), intError)
          } else if(!any(nonNumerics) && intError %in% isolate(values$step1errors)){
            ind <- match(intError, isolate(values$step1errors))
            values$step1errors <- isolate(values$step1errors)[-ind]
          }
        }
        
        # proceed only if there is no intError
        if(!intError %in% isolate(values$step1errors)){
          # write input boxes for each simple effect
# t3 <- Sys.time()
          lapply(seq(length(fixedSimples)), function(x){
            fixnum <- isolate(as.numeric(input[[paste0("f",x,"FixNum")]]))
            containers <- facs[grep(fixedSimples[x], toupper(nests))]
            newLab <- ifelse(length(containers)==0,
              paste0(fixedSimples[x]," levels"),
              paste0(fixedSimples[x]," levels (per ",paste(containers, sep="*"),")"))
            if(is.na(fixNumLabs[x]) || newLab != fixNumLabs[x]){
              output[[paste0("get_f",x,"FixNum")]] <- renderUI({
                textInputMini(paste0("f",x,"FixNum"), label = newLab,
                              value=ifelse(length(fixnum) > 0, fixnum, 2))
              })
              fixNumLabs[x] <<- newLab
            }
          })
# print(paste("ems.getFixNum.writeActive =", round(c(difftime(Sys.time(), t3)*1000)), "ms"))
# t3 <- Sys.time()
          # clear the remainder of the 12 UIs
          lapply(setdiff(1:12, seq(length(fixedSimples))), function(x){
            if(!is.na(fixNumLabs[x])){
              output[[paste0("f",x,"FixContrastPrompt")]] <- renderUI(div())
              output[[paste0("get_f",x,"FixNum")]] <- renderUI(div())
              fixNumLabs[x] <<- NA
            }
          })
# print(paste("ems.getFixNum.writeNonActive =", round(c(difftime(Sys.time(), t3)*1000)), "ms"))    
        } # end if no intError in step1errors
        
        # clear no-fixed-factors error if it currently exists
        if(allRandError %in% isolate(values$step1errors)){
          ind <- match(allRandError, isolate(values$step1errors))
          values$step1errors <- isolate(values$step1errors)[-ind]
        }
# print(paste("ems.getFixNum =", round(c(difftime(Sys.time(), t2)*1000)), "ms"))    
      } else { # i.e., if all(rands)
        # clear UIs that depend on at least 1 fixed factor
        output$get_which <- renderUI(div())
        lapply(1:12, function(x){
          output[[paste0("get_f",x,"FixNum")]] <- renderUI(div())
          output[[paste0("f",x,"FixContrastPrompt")]] <- renderUI(div())
          lapply(1:12, function(y){
            output[[paste0("get_f",x,"FixContrast",y)]] <- renderUI(div())
          })
        })
        
        # throw error
        if(!allRandError %in% isolate(values$step1errors)){
          values$step1errors <- c(isolate(values$step1errors), allRandError)
        }
      } # end else, i.e., if all(rands)
    } # end if any facs in letters
# print(paste("ems =", round(c(difftime(Sys.time(), t1)*1000)), "ms"))
  }) # end observe

# get contrasts for fixed factors having >2 levels ------------------------

  # only if they are specified in input$which
  observe({
    t1 <- Sys.time()
    if(!intError %in% values$step1errors){
      lapply(1:12, function(f){
        fixnum <- as.numeric(input[[paste0("f",f,"FixNum")]])
        # errors from previous reactive block not registered yet
        # so manually check for bad fixnum input
        if(length(fixnum) > 0 && !is.na(fixnum) &&
             !is.null(input$which) && !is.na(fixedSimples[f])){
          # check if current factor interacts with selected effect
          if(any(grepl(fixedSimples[f], unlist(strsplit(input$which, split="*", fixed=TRUE))))){
            if(fixnum > 2){
              if(is.na(fixContrastPrompts[f])){
                output[[paste0("f",f,"FixContrastPrompt")]] <- renderUI({
                  div("Contrast code values to apply to the levels of ",
                      strong(fixedSimples[f]),"(must sum to 0!). Note that you",em("only"),"need to specify the single code being tested; you do",em("not"),"need to specify the full set of contrast codes.")
                })
                fixContrastPrompts[f] <<- fixedSimples[f]
              }
              lapply(seq(fixnum), function(x){
                if(is.na(fixContrastMat[f,x])){
                  output[[paste0("get_f",f,"FixContrast",x)]] <- renderUI({
                    textInputMini(paste0("f",f,"FixContrast",x),
                                  label = NULL, value=1*(x==1) + -1*(x==2))
                  })
                }
                fixContrastMat[f,x] <<- 1*(x==1) + -1*(x==2)
              })
              lapply(setdiff(1:12, seq(fixnum)), function(x){
                if(!is.na(fixContrastMat[f,x])){
                  output[[paste0("get_f",f,"FixContrast",x)]] <- renderUI(div())
                }
                fixContrastMat[f,x] <<- NA
              })
            }
          } else { # i.e., if fixed factor NOT in input$which
            if(!is.na(fixContrastPrompts[f])){
              output[[paste0("f",f,"FixContrastPrompt")]] <- renderUI(div())
              fixContrastPrompts[f] <<- NA
            }
            lapply(1:12, function(x){
              if(!is.na(fixContrastMat[f,x])){
                output[[paste0("get_f",f,"FixContrast",x)]] <- renderUI(div())
                fixContrastMat[f,x] <<- NA
              }
            })
          } # end if fixed factor NOT in input$which
          # clear contrasts left over from previous examples
        } else if(length(fixnum) > 0 && !is.na(fixnum) &&
                    !is.null(input$which) && is.na(fixedSimples[f])){
          if(!is.na(fixContrastPrompts[f])){
            output[[paste0("f",f,"FixContrastPrompt")]] <- renderUI(div())
            fixContrastPrompts[f] <<- NA
          }
          lapply(1:12, function(x){
            if(!is.na(fixContrastMat[f,x])){
              output[[paste0("get_f",f,"FixContrast",x)]] <- renderUI(div())
              fixContrastMat[f,x] <<- NA
            }
          })
        }
      }) # end lapply
    } # end if no intError in step1errors
# print(paste("getFixContrast =", round(c(difftime(Sys.time(), t1)*1000)), "ms"))
  }) # end observe

# drop-down menu examples -------------------------------------------------

  observe({
# t1 <- Sys.time()
    # clear input$randNums somehow
    # or make default randNum values not depend just on not being NULL
    
    if(substr(input$example,1,1)=="1"){
      updateNumericInput(session, "f2FixNum", value=2)
      values$numFacs <- 1
      updateTextInput(session, "f1Lab", value="Group")
      updateCheckboxInput(session, "f1Rand", value=FALSE)
      updateTextInput(session, "f1Nest", value="")
      updateSelectInput(session, "which", choices="G")
      updateTextInput(session, "f1FixNum", value=2)
    } else if(substr(input$example,1,1)=="2"){
      values$numFacs <- 2
      updateTextInput(session, "f1Lab", value="Group")
      updateTextInput(session, "f2Lab", value="Participant")
      updateCheckboxInput(session, "f1Rand", value=FALSE)
      updateCheckboxInput(session, "f2Rand", value=TRUE)
      updateTextInput(session, "f1Nest", value="P")
      updateTextInput(session, "f2Nest", value="")
      updateSelectInput(session, "which", choices="G")
      updateTextInput(session, "f1FixNum", value=2)
    } else if(substr(input$example,1,1)=="3"){
      values$numFacs <- 3
      updateTextInput(session, "f1Lab", value="Within")
      updateTextInput(session, "f2Lab", value="Between")
      updateTextInput(session, "f3Lab", value="Participant")
      updateCheckboxInput(session, "f1Rand", value=FALSE)
      updateCheckboxInput(session, "f2Rand", value=FALSE)
      updateCheckboxInput(session, "f3Rand", value=TRUE)
      updateTextInput(session, "f1Nest", value="")
      updateTextInput(session, "f2Nest", value="P")
      updateTextInput(session, "f3Nest", value="")
      updateSelectInput(session, "which", choices=c("W","B","W*B"))
      updateTextInput(session, "f1FixNum", value=2)
      updateTextInput(session, "f2FixNum", value=3)
    } else if(substr(input$example,1,1)=="4"){
      values$numFacs <- 3
      updateTextInput(session, "f1Lab", value="Classes")
      updateTextInput(session, "f2Lab", value="Schools")
      updateTextInput(session, "f3Lab", value="Treatments")
      updateCheckboxInput(session, "f1Rand", value=TRUE)
      updateCheckboxInput(session, "f2Rand", value=TRUE)
      updateCheckboxInput(session, "f3Rand", value=FALSE)
      updateTextInput(session, "f1Nest", value="")
      updateTextInput(session, "f2Nest", value="C")
      updateTextInput(session, "f3Nest", value="C")
      updateSelectInput(session, "which", choices="T")
      updateTextInput(session, "f1FixNum", value=2)
    } else if(substr(input$example,1,1)=="5"){
      values$numFacs <- 3
      updateTextInput(session, "f1Lab", value="Participants")
      updateTextInput(session, "f2Lab", value="Stimuli")
      updateTextInput(session, "f3Lab", value="Treatments")
      updateCheckboxInput(session, "f1Rand", value=TRUE)
      updateCheckboxInput(session, "f2Rand", value=TRUE)
      updateCheckboxInput(session, "f3Rand", value=FALSE)
      updateTextInput(session, "f1Nest", value="")
      updateTextInput(session, "f2Nest", value="")
      updateTextInput(session, "f3Nest", value="S")
      updateSelectInput(session, "which", choices="T")
      updateTextInput(session, "f1FixNum", value=2)
    } else if(substr(input$example,1,1)=="6"){
      values$numFacs <- 4
      updateTextInput(session, "f1Lab", value="Participants")
      updateTextInput(session, "f2Lab", value="Groups")
      updateTextInput(session, "f3Lab", value="Stimuli")
      updateTextInput(session, "f4Lab", value="Blocks")
      updateCheckboxInput(session, "f1Rand", value=TRUE)
      updateCheckboxInput(session, "f2Rand", value=FALSE)
      updateCheckboxInput(session, "f3Rand", value=TRUE)
      updateCheckboxInput(session, "f4Rand", value=FALSE)
      updateTextInput(session, "f1Nest", value="")
      updateTextInput(session, "f2Nest", value="P")
      updateTextInput(session, "f3Nest", value="")
      updateTextInput(session, "f4Nest", value="S")
      updateSelectInput(session, "which", choices=c("G","B","G*B"))
      updateTextInput(session, "f1FixNum", value=2)
      updateTextInput(session, "f2FixNum", value=2)
    }
# print(paste("examples =", round(c(difftime(Sys.time(), t1)*1000)), "ms"))
  })

# check for errors in step 1 ----------------------------------------------

  # only display 'submit' button if no errors found
  observe({
# t1 <- Sys.time()
    if(length(values$step1errors) > 0){
      output$submitAndEMS <- renderUI(div())
      output$step1ErrorPrompt <- renderUI({
        h4("The following errors must be corrected before you can continue:",
           style = "color:red")
      })
      output$step1ErrorBox <- renderUI({
        lapply(values$step1errors, function(x){
          div(x, style = "color:red")
        })
      })
      step2clear <<- TRUE
    } else {
      output$submitAndEMS <- renderUI({
        fluidRow(
          column(2, actionButton("submit", strong("Submit Design"))),
          column(10, checkboxInput("showEMS", value=FALSE,
            label="Advanced: Show expected mean square equations for this design?"))
        )
      })
      output$step1ErrorPrompt <- renderUI(div())
      output$step1ErrorBox <- renderUI(div())
    }
# print(paste("step1errors =", round(c(difftime(Sys.time(), t1)*1000)), "ms"))
  })

# step 2: parameters ------------------------------------------------------

  # write instructions and Solve button, get most basic input
  observe({
# t1 <- Sys.time()
    if(length(input$submit) > 0 && input$submit > 0 &&
         length(values$step1errors)==0){
      # show EMS table if requested
      if(isolate(input$showEMS)){
        output$EMSoutput <- renderUI({
          list(
            h4("Table of expected mean square equations"),
            p("Each row of the table represents one of the expected mean square equations for the specified design. Each mean square equation consists of a sum of",em("variance components"),"multiplied by the numbers of levels of some of the factors in the experiment."),
            p("The columns represent the variance components that go into each mean square equation, and the entries in each cell of the table indicate the numbers of factor levels that are multiplied by the corresponding variance component in the corresponding mean square equation. If a cell is blank, it is implicitly equal to 0."),
            p("Some more information about expected mean squares can be found at the following links:",
              a("[1]", href="http://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_introanova_a0000000086.htm"),",",
              a("[2]", href="http://www.math.unb.ca/~rolf/Courses/07w/3373/notes-ems.pdf"),",",
              a("[3]", href="http://www.plantsciences.ucdavis.edu/agr205/Lectures/2011_Transp/T10_MixModels.pdf")),
            fluidRow(
              column(1, div()),
              column(11, div("Columns = Variance components"))
            ),
            fluidRow(
              column(1, div("Rows = Mean squares")),
              column(11, tableOutput("EMStable"))
            ),
            p("The variables representing the numbers of factor levels are the lower-case counterparts of the factor labels (which are printed in upper-case), and represent the number of levels of the corresponding factor. For example, the variable 'a' represents the number of levels of the A factor. The # symbol denotes the number of",em("replicates"),"in the design -- see the \"Replicates information\" section under Step 2."),
            p("Importantly, the numbers of factor levels are",em("per each level of any containing factors"),"(i.e., factors that the current factor is nested in). For example, if the factor A is nested in the factor B, then the variable 'a' represents the number of levels of A per each level of B. But if the factor A is not nested in any other factors, then 'a' just represents the total number of levels of A. See the input box labels under Step 2 for some guidance for the random factors.")
          )
        })
        output$EMStable <- renderTable(ems)
      } else {
        output$EMSoutput <- renderUI(div())
      }
      
      # print instructions and button
      step2clear <<- FALSE
      output$solveInstrs <- renderUI({
        fluidRow(
          column(5, p("Enter the assumed parameters of the study and then click the",strong("Compute Power"),"button at the bottom of this step.")),
          column(2, actionButton("display2info", label=strong("Show/hide more information")))
          # p("To compute power estimates, enter an X for the variable you wish to solve for, then click the",strong("Solve for X"),"button.")
        )
      })
      
      # effect size info
      output$step2info <- renderUI({
        conditionalPanel(condition = "(input.display2info % 2) == 1",
          p(strong("Effect size information."),"Effect sizes here are on the Cohen's d (standardized mean difference) scale. See the figure below for typical values of Cohen's d based on meta-analytic data. For contrasts consisting of more than 2 levels, PANGEA uses a generalized version of Cohen's d that takes into account the reduction in predictor variance relative to a simple two-group-comparison contrast."),
          img(src="http://jakewestfall.org/pangea/d_dist.png"),
          p(strong("Replicates information."),"One of the boxes asks for \"# replicates\", or the number of",em("replicates"),"in the study. In traditional ANOVA terminology, replicates refers to the (constant) number of observations in each of the",em("lowest-level"),"cells of the design; lowest-level in the sense that it refers to the crossing of all fixed",em("AND"),"random factors, including e.g. subjects. For example, in a simple pre-test/post-test style design where we measure each subject twice before a treatment and twice after the treatment, the number of replicates would be 2, since there are 2 observations in each Subject*Treatment cell."),
          p(strong("Variance component information."),"For most designs involving at least one random factor, PANGEA will ask you to specify the variances of some of the random effects in the design; for example, var(e) for the error variance, or var(S) for the variance of the subject means, if the symbol \"S\" represents the Subject factor in your design. These variance components are entered in a standardized form, specifically, as the",em("proportion of the total random variance"),"due to that effect (i.e., as Variance Partitioning Coefficients or VPCs; see Westfall, Kenny, & Judd, 2014). As such, the variances should be between 0 and 1, and should not sum to more than 1. We have attempted to give these variance components sensible default values that respect the so-called",em("hierarchical ordering principle"),"; again, see Westfall et al. (2014), or also the online discussion",a("HERE", href="http://stats.stackexchange.com/questions/72819/relative-variances-of-higher-order-vs-lower-order-random-terms-in-mixed-models"),". The upshot is that you should be able to get basically reasonable power analysis results without messing around too much with the variance components.")
          # p(strong("Standardized vs. unstandardized input."),"As the information above indicates, PANGEA is set up by default to accept",em("standardized"),"input for the experimental parameters; i.e., Cohen's d for the effect to be tested, VPCs for the variances. However, PANGEA can also accept",em("unstandardized"),"parameter input, where the effect is given as a simple mean difference (or, in more general terms, the regression coefficent for the contrast of interest), and the variance components are simply given as variances")
        )
      })
      
# get parameters that are always there (power, ES, reps) ------------------

      output$get_power <- renderUI({
        #textInputMini("power", label="Power", value="X")
      })
      output$get_ES <- renderUI({
        textInputMini("ES", label="Effect size (d)", value=.45)
      })
      output$get_numReps <- renderUI({
        numReps <- isolate(values$numReps)
        textInputMini("numReps",
          label=paste0("Replicates (observations per ",
            paste(facs[nests==""],collapse="*"),")"),
          value=ifelse(length(numReps) > 0, numReps, 1))
      })
#       output$solve <- renderUI({actionButton("solve", strong("Compute Power"))})
#       # create dependence on step2errors
#       values$step2errors      

# get numbers of levels for random factors --------------------------------

      # ONLY if there is at least 1 random factor
      if(any(unlist(rands))){
        lapply(seq(nchar(randArg)), function(x){
          containers <- tolower(unlist(strsplit(randArg, split="")))[x]
          containers <- facs[grep(containers, nests, fixed=TRUE)]
          if(length(containers)==0) containers <- ""
          lab <- unlist(strsplit(randArg, split=""))[x]
          randnum <- isolate(input[[paste0("f",x,"RandNum")]])
          output[[paste0("get_f",x,"RandNum")]] <- renderUI({
            textInputMini(paste0("f",x,"RandNum"),
              label = ifelse(containers[1]=="",
                paste0("Number of ",lab,"'s"),
                paste0("Number of ",lab,"'s (per ",paste(unlist(strsplit(containers,split="")),collapse="*"),")")),
              value=ifelse(is.null(randnum), 30, randnum))
          })
        })
        lapply(setdiff(1:12, seq(nchar(randArg))), function(x){
          output[[paste0("get_f",x,"RandNum")]] <- renderUI(div())
        })
      } # end if any rands
    } # end if submit > 0
# print(paste("step2constant =", round(c(difftime(Sys.time(), t1)*1000)), "ms"))
  }) # end observe

# get other step 2 inputs -------------------------------------------------

  observe({
# t1 <- Sys.time()
    if(length(input$submit) > 0 && input$submit > 0 &&
         length(isolate(values$step1errors))==0 &&
         isolate(input$which) %in% rownames(ems)){
      if(!is.null(input$numReps)) values$numReps <- input$numReps
      num <<- isolate(input$which)
      denom <<- ems[num, -c(which(ems[num,]==""),
                            match(num, colnames(ems)))]
      if(length(denom)==1){
        denom <<- c(error="1")
      }
      
# get random effect variances (as VPCs) -----------------------------------
      # ONLY if there is at least 1 random factor
      if(any(unlist(rands))){
        ranefs <- setdiff(head(rownames(ems), -1), fixedFacs)
        defaults <- nchar(ranefs) %/% 2 + 1
        defaults <- sum(range(defaults)) - defaults
        defaults <- c(defaults, max(defaults)+1)
        defaults <- defaults/sum(defaults)
        names(defaults) <- paste0("var(", c(ranefs, "error"), ")")
        varLabs <<- paste0("var(", names(denom), ")")
        try(if(as.numeric(values$numReps)==1 & any(denom=="#")){
          defaults <- c(defaults, defaults["var(error)"]+defaults[paste0("var(",names(denom)[denom=="#"],")")])
          varLabs <<- c(paste(varLabs[1],
                              paste0("var(",names(denom)[denom=="#"],")"),
                              sep=" + "),
                        varLabs[-c(1,which(varLabs==paste0("var(",names(denom)[denom=="#"],")")))])
          names(defaults) <- c(head(names(defaults),-1), varLabs[1])
        }, silent=FALSE)
        # write input boxes
        howmany <- seq(varLabs)
        if(any(defaults==1)) howmany <- numeric(0)
        values$defaults <- defaults
        lapply(howmany, function(x){
          output[[paste0("get_f",x,"Var")]] <- renderUI({
            textInputMini(paste0("f",x,"Var"), label = varLabs[x],
              value=round(defaults[varLabs[x]], digits=3))
          })
        })
        lapply(setdiff(1:12, howmany), function(x){
          output[[paste0("get_f",x,"Var")]] <- renderUI(div())
        })
      } # end if any rands
    } # end if submit > 0
# print(paste("step2varying =", round(c(difftime(Sys.time(), t1)*1000)), "ms"))
  }) # end observe

# check for errors in step 2 ----------------------------------------------
  observe({
    # check that random levels > 1
    lapply(1:12, function(x) input[[paste0("f",x,"RandNum")]])
    if(any(unlist(rands))){
      # check conditions
      nonNumerics <- sapply(seq(nchar(randArg)), function(x){
        randnum <- input[[paste0("f",x,"RandNum")]]
        suppressWarnings(return(is.na(as.numeric(randnum)) ||
                                  as.numeric(randnum) %% 1 > 0 ||
                                  as.numeric(randnum) < 2))
      })
      # throw error, but only if no arguments are NULL
      if(!any(sapply(nonNumerics, is.na))){
        if(any(nonNumerics) && !randIntError %in% isolate(values$step2errors)){
          values$step2errors <- c(isolate(values$step2errors), randIntError)
        } else if(!any(nonNumerics) && randIntError %in% isolate(values$step2errors)){
          ind <- match(randIntError, isolate(values$step2errors))
          values$step2errors <- isolate(values$step2errors)[-ind]
        }
      } # end if no arguments are null
    } # end if any rands
    
    # check that replicates > 0
    badReps <- is.na(as.numeric(values$numReps)) ||
      as.numeric(values$numReps) %% 1 > 0 || as.numeric(values$numReps) < 1
    if(badReps && !repsError %in% isolate(values$step2errors)){
      values$step2errors <- c(isolate(values$step2errors), repsError)
    } else if(!badReps && repsError %in% isolate(values$step2errors)){
      ind <- match(repsError, isolate(values$step2errors))
      values$step2errors <- isolate(values$step2errors)[-ind]
    }
    
    # check that effect size is numeric
    if(length(input$ES) > 0){
      badES <- suppressWarnings(is.na(as.numeric(input$ES)))
      if(badES && !ESerror %in% isolate(values$step2errors)){
        values$step2errors <- c(isolate(values$step2errors), ESerror)
      } else if(!badES && ESerror %in% isolate(values$step2errors)){
        ind <- match(ESerror, isolate(values$step2errors))
        values$step2errors <- isolate(values$step2errors)[-ind]
      }
    }
    
    # check VPCs
    lapply(1:12, function(x) input[[paste0("f",x,"Var")]])
    if(any(unlist(rands))){
      # check conditions
      checks <- sapply(seq(varLabs), function(x){
        vpc <- input[[paste0("f",x,"Var")]]
        suppressWarnings(return(is.na(as.numeric(vpc)) ||
                                  as.numeric(vpc) < 0 ||
                                  as.numeric(vpc) > 1))
      })
      # throw error, but only if no arguments are NULL
      if(!any(sapply(checks, is.na))){
        # only check sum if previous checks all passed
        badSum <- FALSE
        if(!any(checks)){
          vpcs <- sapply(seq(varLabs), function(x){
            as.numeric(input[[paste0("f",x,"Var")]])
          })
          badSum <- sum(vpcs, na.rm=TRUE) > 1.01 # allow a little tolerance
        }
        if((any(checks) || badSum) && !sumVPC %in% isolate(values$step2errors)){
          values$step2errors <- c(isolate(values$step2errors), sumVPC)
        } else if(!(any(checks) || badSum) && sumVPC %in% isolate(values$step2errors)){
          ind <- match(sumVPC, isolate(values$step2errors))
          values$step2errors <- isolate(values$step2errors)[-ind]
        }
      } # end if no arguments are null
    } # end if any rands
  }) # end observe

# clear step 2 if step 1 modified -----------------------------------------

  observe({
# t1 <- Sys.time()
    # create dependencies on all step 1 input
    values$numFacs
    lapply(1:12, function(x) input[[paste0("f",x,"Lab")]])
    lapply(1:12, function(x) input[[paste0("f",x,"Rand")]])
    lapply(1:12, function(x) input[[paste0("f",x,"Nest")]])
    lapply(1:12, function(x) input[[paste0("f",x,"FixNum")]])
    lapply(1:12, function(x) input[[paste0("f",x,"FixContrast")]])
    input$which
    values$step1errors
    
    # if step 2 not already clear...
    if(!step2clear){
      # clear step 2 UIs
      output$EMSoutput <- renderUI(div())
      output$solveInstrs <- renderUI(div())
      output$solve <- renderUI(div())
      output$step2info <- renderUI(div())
      output$get_power <- renderUI(div())
      output$get_ES <- renderUI(div())
      output$get_numReps <- renderUI(div())
      lapply(1:12, function(x){
        output[[paste0("get_f",x,"RandNum")]] <- renderUI(div())
      })
      lapply(1:12, function(x){
        output[[paste0("get_f",x,"Var")]] <- renderUI(div())
      })
      output$step2ErrorPrompt <- renderUI(div())
      output$step2ErrorBox <- renderUI(div())
      
      # set global flag saying it's clear
      step2clear <<- TRUE
    }
# print(paste("clearStep2 =", round(c(difftime(Sys.time(), t1)*1000)), "ms"))
  })

# only display 'solve' button if no errors found --------------------------

  observe({
# t1 <- Sys.time()
    # make dependent on submit button
    if(length(input$submit) > 0 && input$submit > 0){
      if(length(values$step2errors) > 0){
        output$solve <- renderUI(div())
        output$step2ErrorPrompt <- renderUI({
          h4("The following errors must be corrected before you can continue:",
             style = "color:red")
        })
        output$step2ErrorBox <- renderUI({
          lapply(values$step2errors, function(x){
            div(x, style = "color:red")
          })
        })
      } else if(!is.null(input$numReps)){
        output$solve <- renderUI({actionButton("solve", strong("Compute Power"))})
        output$step2ErrorPrompt <- renderUI(div())
        output$step2ErrorBox <- renderUI(div())
      }
    }
# print(paste("displaySubmit =", round(c(difftime(Sys.time(), t1)*1000)), "ms"))
  })

# step 3: the answer ------------------------------------------------------

  output$ans <- renderPrint({
    # make answer dependent on Solve button
    if(length(input$solve) > 0 && input$solve > 0){
      # get params from step 2
      numFacs <- isolate(values$numFacs)
      pow <- isolate(input$power)
      ES <- as.numeric(isolate(input$ES))
      numReps <- as.numeric(isolate(input$numReps))

      # compute NCP
      numLevs <- rep(NA, numFacs)
      numLevs[!rands] <- as.numeric(sapply(seq(sum(!rands)), function(x){
        isolate(as.numeric(input[[paste0("f",x,"FixNum")]]))
      }))
      if(any(unlist(rands))){
        numLevs[rands] <- as.numeric(sapply(seq(sum(rands)), function(x){
          isolate(input[[paste0("f",x,"RandNum")]])
        }))
      }
      for(x in seq(numFacs)){
        assign(attr(ems, "terms")[-1][x], numLevs[x])
      }
      denomLevs <- strsplit(denom, split="")
      denomLevs <- unlist(lapply(denomLevs, paste, collapse="*"))
      repChar <- attr(ems, "terms")[1]
      denomLevs <- gsub("#", "numReps", denomLevs)
      denomLevs <- sapply(denomLevs, function(x) eval(parse(text=x)))
      if(numReps==1 & any(denom=="#")){
        denomLevs <- denomLevs[-1]
      }
      contrasts <- lapply(unlist(strsplit(num, split="*", fixed=TRUE)), function(x){
        if(numLevs[match(x, facs)]==2){
          con <- c(-1,1)
        } else {
          con <- as.numeric(sapply(seq(numLevs[match(x, facs)]), function(y){
            isolate(input[[paste0("f",match(x, facs[!rands]),"FixContrast",y)]])
          }))
        }
        return(con)
      })
      names(contrasts) <- unlist(strsplit(num, split="*", fixed=TRUE))
      numContrast <- Reduce(kronecker, contrasts)
      designFac <- var(numContrast)*(length(numContrast)-1)/length(numContrast)
      # add sum of squared contrasts to relevant coefficients in EMS line
      denomLevs <- sapply(seq(denomLevs), function(x){
        ind <- match(unlist(strsplit(names(denomLevs[x]), split="*", fixed=TRUE)), names(contrasts))
        if(any(!is.na(ind))){
          return(denomLevs[x] * Reduce(prod, lapply(contrasts[ind[!is.na(ind)]], function(y) sum(y^2))))
        } else {
          return(denomLevs[x])
        }
      })
      if(any(unlist(rands)) & !any(isolate(values$defaults==1))){
        vars <- as.numeric(sapply(seq(varLabs), function(x){
          isolate(input[[paste0("f",x,"Var")]])
        }))
        denomLevLabs <- strsplit(varLabs, split=" + ", fixed=TRUE)
        denomLevLabs <- sapply(denomLevLabs, function(x) tail(x,1))
        denomLevLabs <- sapply(denomLevLabs, function(x) substr(x, 5, nchar(x)-1))
        denomValues <- as.vector(t(denomLevs[denomLevLabs]) %*% vars)
        ncp <- ES*sqrt(designFac)*sqrt(prod(numReps,numLevs))/
          diff(range(numContrast))/sqrt(denomValues)
      } else {
        vars <- 1
        ncp <- ES*sqrt(designFac)*sqrt(prod(numReps,numLevs))/
          diff(range(numContrast))
      }
      
# get DFs for mean squares ------------------------------------------------

      DFeqs <- sapply(seq(numFacs), function(x){
        tolower(isolate(input[[paste0("f",x,"Nest")]]))
      })
      DFeqs <- sapply(seq(DFeqs), function(x){
        containers <- tolower(facs[grepl(tolower(facs[x]), DFeqs, fixed=TRUE)])
        if(length(containers)==0) containers <- ""
        paste(paste(tolower(rownames(ems)[x]),
                    paste(unlist(strsplit(containers, split="")), sep="*"),
                    sep=ifelse(containers=="","","*")),
              "-",ifelse(containers=="","1",
                         paste(unlist(strsplit(containers, split="")), sep="*")))
      })
      DFsimples <- sapply(DFeqs, function(x) eval(parse(text=tolower(x))))
      names(DFsimples) <- rownames(ems)[seq(numFacs)]
      DFints <- rownames(ems)[-c(seq(numFacs),length(rownames(ems)))]
      DFints <- sapply(strsplit(DFints, split="*", fixed=TRUE), function(x){
        prod(DFsimples[x])
      })
      names(DFints) <- rownames(ems)[-c(seq(numFacs),length(rownames(ems)))]
      DFs <- c(DFsimples, unlist(DFints),
               error=prod(numLevs, numReps-1))

      # get DFs using Satterthwaite
      mat <- t(ems[,-match(num, colnames(ems))] != "")
      mode(mat) <- "numeric"
      beta <- solve(mat[,-match(num, colnames(mat))]) %*% mat[,num]
      if(is.null(rownames(beta))) rownames(beta) <- "error"
      MSeqs <- ems[rownames(beta)[as.vector(beta) != 0], names(denom), drop=FALSE]
      if(numReps==1 & any(denom=="#")){
        MSeqs <- MSeqs[,-match(names(denom)[denom=="#"], colnames(MSeqs)), drop=FALSE]
      }
      MSlevs <- strsplit(MSeqs, split="")
      MSlevs <- unlist(lapply(MSlevs, paste, collapse="*"))
      MSlevs <- gsub("#", "numReps", MSlevs)
      MSlevs <- sapply(MSlevs, function(x) eval(parse(text=x)))
      MSlevs <- sapply(MSlevs, function(x) ifelse(is.null(x),0,x))
      MSlevs <- array(MSlevs, dim=dim(MSeqs))
      if(all(dim(MSlevs) > 0)) MS <- MSlevs %*% vars else MS <- rbind(1)
      beta <- beta[beta != 0,,drop=FALSE]
      denomDF <- as.vector((t(beta) %*% MS)^2 /
        sum((beta^2 * MS^2)/unlist(cbind(DFs[rownames(beta)]))))
      
# solve for selected parameter --------------------------------------------

      pow <- pt(qt(.975, df=denomDF), df=denomDF, ncp=ncp, lower.tail=FALSE) +
        pt(qt(.025, df=denomDF), df=denomDF, ncp=ncp, lower.tail=TRUE)

      samps <- sapply(seq(numFacs), function(x){
        tolower(isolate(input[[paste0("f",x,"Nest")]]))
      })
      samps <- sapply(seq(samps), function(x){
        containers <- facs[grepl(tolower(facs[x]), samps, fixed=TRUE)]
        if(length(containers)==0) containers <- ""
        paste(tolower(rownames(ems)[x]),
              paste(unlist(strsplit(containers, split="")), collapse="*"),
              sep=ifelse(containers=="","","*"))
      })
      samps <- sapply(samps, function(x) eval(parse(text=tolower(x))))
      names(samps) <- tolower(facs)
      terms <- c("numReps", attr(ems, "terms")[-1])
      nobs <- eval(parse(text=paste(terms, collapse="*")))
      randFacNames <- sapply(which(rands), function(x){
        return(isolate(input[[paste0("f",x,"Lab")]]))
      })
      names(samps)[rands] <- randFacNames
      samps <- c(samps, replicates=numReps, total_observations=nobs)
      sources <- c(head(rownames(ems),-1), "error")
      if(length(randFacNames) > 0){
        totalSamps <- samps[c("total_observations",randFacNames,"replicates")]
      } else {
        totalSamps <- samps[c("total_observations","replicates")]
      }

      return(list(total_sample_sizes=totalSamps,
                  sources_of_variation=noquote(sources),
                  power_results=c(noncentrality_parameter=round(ncp,2),
                    degrees_of_freedom=round(denomDF,2), power=round(pow,3))))      
    } # end if solve > 0
  }) # end assignment to 'ans'

}) # end call to shinyServer()
