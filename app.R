library(shiny)
library(shinydashboard)
library(shinyRadioMatrix)
library(shinyjs)
library(V8)
library(dplyr)
library(tidyr)
# library(ggplot2)

# fix chronic conditions 

source("customradiomatrix.R") # use this instead of shiny radio matrix package, still need to install shinradio matrix though for the css and js files

# Setup -------------------------------------------------------------------
options(warn=-1) # surpress warnings
options(warn=0) # turn it back on

q_ccs <- tibble(
    ccs = c(
      "...have osteoarthritis in the knee?",
      "...have osteoarthritis in the hip?",
      "...have osteoarthritis in one or both hands?",
      "...have rheumatoid arthritis?",
      "...have any other type of arthritis?",
      "...have/had any of the following: emphysema, chronic bronchitis, chronic obstructive pulmonary disease (COPD),\nor chronic changes in lungs due to smoking?",
      '...have high blood pressure or hypertension?',
      '...have diabetes, borderline diabetes or that your blood sugar is high?',
      '...have heart disease (including congestive heart failure or CHF)?',
      '...have angina (or chest pain due to heart disease)?',
      '...have had a heart attack or myocardial infarction?',
      '...have peripheral vascular disease or poor circulation in your limbs?',
      '...have experienced a stroke or CVA (cerebrovascular accident)?',
      '...have experienced a mini-stroke or TIA (transient ischemic attack)?',
      '...have a memory problem?'
    ),
    ids = c(
      "ostknee",
      "osthip",
      "osthand",
      "rheu",
      "otherarth",
      "copd",
      'hbp',
      'diabetes',
      'chf',
      'angina',
      'heartattk',
      'pvd',
      'stroke',
      'ministroke',
      'memory'
    )
  ) %>%
    mutate(blank = "",
           NS = "NS")

q_ccs2 <- tibble(
    ccs = c(
      "...have dementia or Alzheimer’s disease?",
      "...had Parkinsonism or Parkinson’s disease?",
      '...have intestinal or stomach ulcers?',
      '...have a bowel disorder such as Crohn’s Disease, ulcerative colitis, or Irritable Bowel Syndrome?',
      '...experience bowel incontinence?',
      '...experience urinary incontinence?',
      '...have cataracts?',
      '...have glaucoma?',
      '...have macular degeneration?',
      '...had cancer?',
      '...have osteoporosis, sometimes called low bone mineral density, or thin, brittle or weak bones?',
      '...have back problems, excluding fibromyalgia and arthritis?',
      '...have an UNDER-active thyroid gland (sometimes called hypothyroidism or myxedema)?',
      '...have an OVER-active thyroid gland (sometimes called hyperthyroidism or Graves’ disease)?',
      '...have kidney disease or kidney failure?'
    ),
    ids = c(
      "alzh",
      "parkinsons",
      'stomach',
      'colitis',
      'bowelinc',
      'uriinc',
      'cataracts',
      'glaucoma',
      'md',
      'cancer',
      'osteoporosis',
      'backproblems',
      'hypothyroidism',
      'hyperthyroidism',
      'kidneydis'
    )
  ) %>%
    mutate(blank = "",
           NS = "NS")

# Choices -----------------------------------------------------------------

yn1 <- c("None Selected" = "NS", "Yes" = 0, "No" = "NA")

yn2 <- c("None Selected" = "NS", "Yes" = 0.5, "No" = "NA")

yn3 <- c("None Selected" = "NS", "Yes" = 1, "No" = 0.5)

diff1 <- c("None Selected" = "NS", "No" = 0, "Yes" = "NA", "Unable to do" = 1, "Don't do on doctor's orders" = 1)

diff2 <- c("None Selected" = "NS", "A little difficult" = 0.25, "Somewhat difficult" = 0.5, "Very difficult" = 0.75)

# UI ---------------------------------------------------------------------

ui <- tagList(
  dashboardPage(
    title = "FI Calculator",
  dashboardHeader(
    disable = F,
    title = "Fit-To-Frail Index Calculator",
    titleWidth = "100%"
    ),
  
  dashboardSidebar(
    disable = T,
    collapsed = T,
    width = 0,
    sidebarMenu(
      id = "tabs",
      # style = "position:fixed;width:220px;",
      
      menuItem("Demographics", tabName = "demo", icon = icon("atom")),
      menuItem("BADL", tabName = "badl", icon = icon("atom")),
      menuItem("IADL", tabName = "iadl", icon = icon("atom")),
      menuItem("Mobility", tabName = "mobility", icon = icon("atom")),
      menuItem("Other", tabName = "other", icon = icon("atom")),
      menuItem("Chronic Condition", tabName = "cc", icon = icon("atom")),
      menuItem("Results", tabName = "results", icon = icon("atom")),
      menuItem("Validation", tabName = "validation", icon = icon("atom"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head( # add css here
      tags$link(rel = "stylesheet", type = "text/css", href = "custom_fi.css"),
      tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
        .navbar {
          height: 10;
          min-height: 0 !important;
        }
      '))
    ),
    tabItems(
          

# Demo --------------------------------------------------------------------

tabItem(
  tabName = "demo",
  fluidRow(
    box(
      width = 12,
      # height = 500,
      # h3("Introduction"),
      p("Please complete the survey to calculate your frailty index score."),
        
    ),
    
    
    box(
      width=12,
      numericInput(inputId = "age_years", 
                   label = h5("What is your age?"), 
                   min = 18, 
                   max = 123,
                   step = 1, 
                   value = "",
                   width = '100%')
    ),
    
    box(
      width=12,
      radioButtons(
        inputId = "ori_ask",
        label = h5("What is your current gender identity?"),
        choices = c("Male" = 1,
                    "Female" = 2,
                    "Transgender Man/Transman" = 3,
                    "Transgender Woman/Transwoman" = 4,
                    "Genderqueer" = 5,
                    "Other" = 6),
        selected = character(0)
      ),
      radioButtons(
        inputId = "sex_ask",
        label = h5("What was your sex at birth?"),
        choices = c("Male" = 1,
                    "Female" = 2),
        selected = character(0)
      )
    ),
    
    
    conditionalPanel(
      condition = '(input.age_years > 17 & input.age_years < 124) & (input.sex_ask == "1" | input.sex_ask == "2") &
      (input.ori_ask == "1" | input.ori_ask == "2" | input.ori_ask == "3" | input.ori_ask == "4" | input.ori_ask == "5" | input.ori_ask == "6")',
      box(
        width = 12,
        column(
          width = 12,
          align = "center",
          actionButton('tomobility', 'Continue', width = '120px')
        )
      )
    ),
    conditionalPanel(
      condition = 'input.age_years < 18 & (input.sex_ask == "1" | input.sex_ask == "2")',
      box(
        width = 12,
        column(
          width = 12,
          align = "center",
          h5("The frailty index was not designed for people under 18 years of age.")
        )
      )
    )
  )
),

# Mobility --------------------------------------------------------------
tabItem(
  tabName = "mobility",
  fluidRow(
    fixedPanel(
      bottom = '1.5px',
      right = '25px',
      style = "z-index: 10000;",
      h6("Page 1 of 5"),
    )
  ),
  fluidRow(
    # box(
    #   width = 12,
    #   h3(strong("Page 1 of 5"), align = "center"),
    #   br()
    # ),
    box(
      width=12,
      radioButtons(
        inputId = "diff_reach_1",
        label = h5("Do you have any difficulty reaching or extending your arms above your shoulders?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_reach_1 == "NA"',
        radioButtons(
          inputId = "diff_reach_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_crouch_1",
        label = h5("Do you have any difficulty stooping, crouching, or kneeling down?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_crouch_1 == "NA"',
        radioButtons(
          inputId = "diff_crouch_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_push_1",
        label = h5("Do you have any difficulty pushing or pulling large objects like a living room chair?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_push_1 == "NA"',
        radioButtons(
          inputId = "diff_push_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_lift_1",
        label = h5("Do you have any difficulty lifting ten pounds (or 4.5 kg) from the floor, like a heavy bag of groceries?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_lift_1 == "NA"',
        radioButtons(
          inputId = "diff_lift_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_hndl_1",
        label = h5("Do you have any difficulty handling small objects, like picking up a coin from a table?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_hndl_1 == "NA"',
        radioButtons(
          inputId = "diff_hndl_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_stand_1",
        label = h5("Do you have any difficulty standing for a long period, around 15 minutes?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_stand_1 == "NA"',
        radioButtons(
          inputId = "diff_stand_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_stndchair_1",
        label = h5("Do you have any difficulty standing up after sitting in a chair?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_stndchair_1 == "NA"',
        radioButtons(
          inputId = "diff_stndchair_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_walkalone_1",
        label = h5("Do you have any difficulty walking alone up and down a flight of stairs?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_walkalone_1 == "NA"',
        radioButtons(
          inputId = "diff_walkalone_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_walkblock_1",
        label = h5("Do you have any difficulty walking 2 to 3 neighbourhood blocks?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_walkblock_1 == "NA"',
        radioButtons(
          inputId = "diff_walkblock_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_makebed_1",
        label = h5("Do you have any difficulty making a bed?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_makebed_1 == "NA"',
        radioButtons(
          inputId = "diff_makebed_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_washback_1",
        label = h5("Do you have any difficulty washing your back?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_washback_1 == "NA"',
        radioButtons(
          inputId = "diff_washback_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_cutfood_1",
        label = h5("Do you have any difficulty using a knife to cut food?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_cutfood_1 == "NA"',
        radioButtons(
          inputId = "diff_cutfood_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "diff_rec_1",
        label = h5("Do you have any difficulty with recreational or work activities in which you take some force or impact through your arm, shoulder, or hand (e.g., golf, hammering, tennis, typing, etc.)?"),
        choices = diff1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.diff_rec_1 == "NA"',
        radioButtons(
          inputId = "diff_rec_2",
          label = h6("Would you say that the degree of difficulty is..."),
          choices = diff2,
          selected = "NS"
        )
      )
    ),
    box(
      width = 12,
      column(
        width = 12,
        align = "center",
        actionButton('backtodemo', 'Previous Page', width = '120px'),
        actionButton('tobadl', 'Next Page', width = '120px'),
      )
    )
  )
),

# BADLs -------------------------------------------------------------------

tabItem(
  tabName = "badl",
  fluidRow(
    fixedPanel(
      bottom = '1.5px',
      right = '25px',
      style = "z-index: 10000;",
      h6("Page 2 of 5"),
    )
  ),
  fluidRow(
    box(
      width = 12,
      # h3(strong("Page 2 of 5"), align = "center"),
      # br(),
      h5("The following questions ask about some basic activities of daily living. Remember, these are activities that can be done without help, with some help, or which you are unable to do.", 
         align = "center"),
      ),
    box(
      width=12,
      radioButtons(
        inputId = "badl_dress_1",
        label = h5("Can you dress and undress yourself without help (including picking out clothes and putting on socks & shoes)?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.badl_dress_1 == "NA"',
        radioButtons(
          inputId = "badl_dress_2",
          label = h6("Can you dress and undress yourself with some help?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.badl_dress_2 == "NA"',
          radioButtons(
            inputId = "badl_dress_3",
            label = h6("Are you completely unable to dress and undress yourself?"),
            choices = yn3,
            selected = "NS"
          )
        )
      )
      ),
      box(
      width=12,
      radioButtons(
        inputId = "badl_appear_1",
        label = h5("Can you take care of your own appearance without help, for example, combing your hair, shaving (if male)?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.badl_appear_1 == "NA"',
        radioButtons(
          inputId = "badl_appear_2",
          label = h6("Can you take care of your own appearance with some help?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.badl_appear_2 == "NA"',
          radioButtons(
            inputId = "badl_appear_3",
            label = h6("Are you completely unable to take care of your own appearance?"),
            choices = yn3,
            selected = "NS"
          )
        )
      ),
      ),
    
      box(
      width=12,
      radioButtons(
        inputId = "badl_walk_1",
        label = h5("Can you walk without help?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.badl_walk_1 == "NA"',
        radioButtons(
          inputId = "badl_walk_2",
          label = h6("Can you walk with some help from a person, or with the use of a walker or crutches, etc.?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.badl_walk_2 == "NA"',
          radioButtons(
            inputId = "badl_walk_3",
            label = h6("Are you completely unable to walk?"),
            choices = yn3,
            selected = "NS"
          )
        )
      ),
      ),
      
    box(
      width=12,
      radioButtons(
        inputId = "badl_inbed_1",
        label = h5("Can you get in and out of bed without any help or aids?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.badl_inbed_1 == "NA"',
        radioButtons(
          inputId = "badl_inbed_2",
          label = h6("Can you get in and out of bed with some help (either from a person or with the aid of some device)?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.badl_inbed_2 == "NA"',
          radioButtons(
            inputId = "badl_inbed_3",
            label = h6("Are you totally dependent on someone else to lift you in and out of bed?"),
            choices = yn3,
            selected = "NS"
          )
        )
      ),
    ),
     
    box(
      width=12, 
      radioButtons(
        inputId = "badl_bath_1",
        label = h5("Can you take a bath or shower without help?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.badl_bath_1 == "NA"',
        radioButtons(
          inputId = "badl_bath_2",
          label = h6("Can you take a bath or shower with some help (i.e., you need help from someone getting in and out of the tub or you need special attachments on the tub)?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.badl_bath_2 == "NA"',
          radioButtons(
            inputId = "badl_bath_3",
            label = h6("Are you completely unable to take a bath and a shower by yourself?"),
            choices = yn3,
            selected = "NS"
          )
        )
      )
    ),
    
    box(
      width = 12,
      column(
        width = 12,
        align = "center",
      actionButton('backtomobility', 'Previous Page', width = '120px'),
      actionButton('toiadl', 'Next Page', width = '120px'),
      )
    ),
  )
),

# IADLs -------------------------------------------------------------------

tabItem(
  tabName = "iadl",
  fluidRow(
    fixedPanel(
      bottom = '1.5px',
      right = '25px',
      style = "z-index: 10000;",
      h6("Page 3 of 5"),
    )
  ),
  fluidRow(
    box(
      width = 12,
      # h3(strong("Page 3 of 5"), align = "center"),
      # br(),
      h5("The following questions ask about some other activities of daily living, activities that can be done without help, with some help or which you are unable to do. 
      You may feel that some of these questions do not apply to you, but it is important that we ask the same questions of everyone.",
         align = "center"),
    ),
    
    box(
      width=12,  
      radioButtons(
        inputId = "iadl_phone_1",
        label = h5("Can you use the telephone without help, including looking up numbers and dialling?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.iadl_phone_1 == "NA"',
        radioButtons(
          inputId = "iadl_phone_2",
          label = h6("Can you use the telephone with some help (i.e., you can answer the phone or dial the operator in an emergency, but need a special phone or help in getting the number or dialling)?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.iadl_phone_2 == "NA"',
          radioButtons(
            inputId = "iadl_phone_3",
            label = h6("Are you completely unable to use the telephone?"),
            choices = yn3,
            selected = "NS"
          )
        )
      ),
    ),
    
    box(
      width=12, 
      radioButtons(
        inputId = "iadl_getplace_1",
        label = h5("Can you get to places out of walking distance without help (i.e., you drive your own car, or travel alone on buses, or taxis)?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.iadl_getplace_1 == "NA"',
        radioButtons(
          inputId = "iadl_getplace_2",
          label = h6("Can you get to places out of walking distance with some help (i.e., you need someone to help you or go with you when travelling)?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.iadl_getplace_2 == "NA"',
          radioButtons(
            inputId = "iadl_getplace_3",
            label = h6("Are you unable to travel unless emergency arrangements are made for a specialized vehicle, like an ambulance?"),
            choices = yn3,
            selected = "NS"
          )
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "iadl_shopping_1",
        label = h5("Can you go shopping for groceries or clothes without help (taking care of all shopping needs yourself)?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.iadl_shopping_1 == "NA"',
        radioButtons(
          inputId = "iadl_shopping_2",
          label = h6("Can you go shopping for groceries or clothes with some help (i.e., you need someone to go with you on all shopping trips)?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.iadl_shopping_2 == "NA"',
          radioButtons(
            inputId = "iadl_shopping_3",
            label = h6("Are you completely unable to do any shopping?"),
            choices = yn3,
            selected = "NS"
          )
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "iadl_prepmeals_1",
        label = h5("Can you prepare your own meals without help (i.e., you plan and cook full meals yourself)?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.iadl_prepmeals_1 == "NA"',
        radioButtons(
          inputId = "iadl_prepmeals_2",
          label = h6("Can you prepare your own meals with some help (i.e., you can prepare some things but are unable to cook full meals yourself)?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.iadl_prepmeals_2 == "NA"',
          radioButtons(
            inputId = "iadl_prepmeals_3",
            label = h6("Are you completely unable to prepare any meals?"),
            choices = yn3,
            selected = "NS"
          )
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "iadl_housework_1",
        label = h5("Can you do your housework without help (i.e., you can clean floors, etc.)?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.iadl_housework_1 == "NA"',
        radioButtons(
          inputId = "iadl_housework_2",
          label = h6("Can you do your housework with some help (i.e., you can do light housework but need help with heavy work)?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.iadl_housework_2 == "NA"',
          radioButtons(
            inputId = "iadl_housework_3",
            label = h6("Are you completely unable to do any housework?"),
            choices = yn3,
            selected = "NS"
          )
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "iadl_meds_1",
        label = h5("Can you take your own medicine without help (in the right doses at the right time)?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.iadl_meds_1 == "NA"',
        radioButtons(
          inputId = "iadl_meds_2",
          label = h6("Can you take your own medicine with some help (i.e., you are able to take medicine if someone prepares it for you or reminds you to take it)?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.iadl_meds_2 == "NA"',
          radioButtons(
            inputId = "iadl_meds_3",
            label = h6("Are you completely unable to take your medicine?"),
            choices = yn3,
            selected = "NS"
          )
        )
      ),
    ),
      
    box(
      width=12,
      radioButtons(
        inputId = "iadl_money_1",
        label = h5("Can you handle your own money without help (i.e., you write cheques, pay bills, etc.)?"),
        choices = yn1,
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.iadl_money_1 == "NA"',
        radioButtons(
          inputId = "iadl_money_2",
          label = h5("Can you handle your own money with some help (i.e., you manage day-to-day buying but need help with managing your chequebook or paying your bills)?"),
          choices = yn2,
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.iadl_money_2 == "NA"',
          radioButtons(
            inputId = "iadl_money_3",
            label = h5("Are you completely unable to handle your money?"),
            choices = yn3,
            selected = "NS"
          )
        )
      )
    ),
    box(
      width = 12,
      column(
        width = 12,
        align = "center",
      actionButton('backtobadl', 'Previous Page', width = '120px'),
      actionButton('toother', 'Next Page', width = '120px'),
      )
    )
  )
),


# Others ------------------------------------------------------------------
tabItem(
  tabName = "other",
  fluidRow(
    fixedPanel(
      bottom = '1.5px',
      right = '25px',
      style = "z-index: 10000;",
      h6("Page 4 of 5"),
    )
  ),
  fluidRow(
    # box(
    #   width = 12,
    #   h3(strong("Page 4 of 5"), align = "center"),
    #   br()
    # ),
    box(
      width = 12,
      
      radioButtons(
        inputId = "health",
        label = h5("In general, would you say your health is..."),
        choices = c("None Selected" = "NS",
                    "Excellent" = 0,
                    "Very Good" = 0.25,
                    "Good" = 0.5,
                    "Fair" = 0.75,
                    "Poor" = 1),
        selected = "NS"
      ),
    ),
    
    box(
      width = 12,
      radioButtons(
        inputId = "eyesight",
        label = h5("Is your eyesight, using glasses or corrective lens if you use them..."),
        choices = c("None Selected" = "NS",
                    "Excellent" = 0,
                    "Very Good" = 0.25,
                    "Good" = 0.5,
                    "Fair" = 0.75,
                    "Poor or non-existent (blind)" = 1),
        selected = "NS"
      ),
    ),
    
    box(
      width = 12,
      radioButtons(
        inputId = "hearing",
        label = h5("Is your hearing, using a hearing aid if you use one..."),
        choices = c("None Selected" = "NS",
                    "Excellent" = 0,
                    "Very Good" = 0.25,
                    "Good" = 0.5,
                    "Fair" = 0.75,
                    "Poor" = 1),
        selected = "NS"
      ),
    ),
    
    box(
      width = 12,
      radioButtons(
        inputId = "weight",
        label = h5("Do you consider yourself..."),
        choices = c("None Selected" = "NS",
                    "Overweight" = 0,
                    "Underweight" = 1,
                    "Just about right" = 0),
        selected = "NS"
      ),
    ),
    
    box(
      width = 12,
      radioButtons(
        inputId = "falls_1",
        label = h5("In the last 12 months, have you had any injuries that were serious enough to limit some of your normal activities? For example, a broken bone, a bad cut or burn, a sprain or a poisoning."),
        choices = c("None Selected" = "NS",
                    "Yes" = "NA",
                    "No" = 0),
        selected = "NS"
      ),
      conditionalPanel(
        condition = 'input.falls_1 == "NA"',
        radioButtons(
          inputId = "falls_2",
          label = h6("Were any of these injuries caused by a fall?"),
          choices = c("None Selected" = "NS",
                      "Yes" = "NA",
                      "No" = 0),
          selected = "NS"
        ),
        conditionalPanel(
          condition = 'input.falls_2 == "NA"',
          numericInput(
            inputId = "falls_3",
            label = h6("How many times have you fallen in the past 12 months?"),
            value = NULL,
            min = 1,
            max = 30
          )
        )
      ),
    ),
    
    box(
      width = 12,
      radioButtons(
        inputId = "effort",
        label = h5("In the past week, how often did you feel that everything you did was an effort?"),
        choices = c("None Selected" = "NS",
                    "All of the time (5-7 days)" = 1,
                    "Occasionally (3-4 days)" = 0.66,
                    "Some of the time (1-2 days)" = 0.33,
                    "Rarely or never (less than 1 day)" = 0),
        selected = "NS"
      ),
    ),
    
    box(
      width = 12,
      radioButtons(
        inputId = "lonely",
        label = h5("In the past week, how often did you feel lonely?"),
        choices = c("None Selected" = "NS",
                    "All of the time (5-7 days)" = 1,
                    "Occasionally (3-4 days)" = 0.66,
                    "Some of the time (1-2 days)" = 0.33,
                    "Rarely or never (less than 1 day)" = 0),
        selected = "NS"
      ),
    ),
    
    box(
      width = 12,
      radioButtons(
        inputId = "getgoing",
        label = h5("In the past week, how often did you feel that you could not “get going”?"),
        choices = c("None Selected" = "NS",
                    "All of the time (5-7 days)" = 1,
                    "Occasionally (3-4 days)" = 0.66,
                    "Some of the time (1-2 days)" = 0.33,
                    "Rarely or never (less than 1 day)" = 0),
        selected = "NS"
      ),),
    
    box(
      width = 12,
      radioButtons(
        inputId = "pneumonia",
        label = h5("In the past year, have you seen a doctor for pneumonia?"),
        choices = c("None Selected" = "NS",
                    "Yes" = 1,
                    "No" = 0),
        selected = "NS"
      ),
    ),
    
    box(
      width = 12,
      radioButtons(
        inputId = "uti",
        label = h5("In the past year, have you seen a doctor for urinary tract infection (UTI)?"),
        choices = c("None Selected" = "NS",
                    "Yes" = 1,
                    "No" = 0),
        selected = "NS"
      )
    ),
    box(
      width = 12,
      column(
        width = 12,
        align = "center",
      actionButton('backtoiadl', 'Previous Page', width = '120px'),
      actionButton('tocc', 'Next Page', width = '120px'),
      )
    ),
  )
),


# Chronic Conditions ------------------------------------------------------

tabItem(
  tabName = "cc",
  fluidRow(
    fixedPanel(
      bottom = '1.5px',
      right = '25px',
      style = "z-index: 10000;",
      h6("Page 5 of 5"),
    )
  ),
  fluidRow(
    box(
      width = 12,
      # h3(strong("Page 5 of 5"), align = "center"),
      # br(),
      h5('The following questions ask about chronic health conditions.
        We are interested in "long-term conditions" which are expected to last, or have already lasted 6 months or more and that have been diagnosed by a health professional.',
         align = "center"),
    ),
    
    box(
      width=12,
      
      column(
        width = 12,
        align = "center",
        
        h5('Has a doctor ever told you that you...'),
        
        radioMatrixInputJ(
          inputId = "ccs",
          rowIds = q_ccs$ids,
          minLabels = q_ccs$ccs,
          # maxLabels = q_ccs$blank,
          choices = c("None Selected" = "NS",
                      "Yes" = 1,
                      "No" = 0),
          selected = q_ccs$NS,
          labelsWidth = list("0", "450px")
        ),
        radioMatrixInputJ(
          inputId = "ccs2",
          rowIds = q_ccs2$ids,
          minLabels = q_ccs2$ccs,
          # maxLabels = q_ccs$blank,
          choices = c("None Selected" = "NS",
                      "Yes" = 1,
                      "No" = 0),
          selected = q_ccs2$NS,
          labelsWidth = list("0", "450px")
        ),
      )
    ),
    box(
      width = 12,
      column(
      width = 12,
      align = "center",
      h5(em("You have reached the end of the questionnaire.")),
      
      br(actionButton('backtoother', 'Previous Page', width = '120px')),
      br(),
      
      conditionalPanel(
        condition = 'output.missing_VARS == "nogood"',
        actionButton('tovalidation', 'Continue', width = '120px'),
      ),
      conditionalPanel(
        condition = 'output.missing_VARS == "good"',
        actionButton('toresults', 'View Results', width = '120px'),
      ),
      conditionalPanel(
        condition = 'output.missing_VARS != "nogood" & output.missing_VARS != "good"',
        h5(em("You need to answer at least one question in order to continue."))
      ),
      br()
      )
    )
  )
),

# Results -----------------------------------------------------------------

tabItem(
  tabName = "results",
  fluidRow(
    box(
      width = 12,
      h3(textOutput("fitext"), align = "center"),
      br(),
      h3("To learn more about the frailty index, please visit:", align = "center"),
      h3(a(href="https://www.dal.ca/sites/gmr/our-tools/the-frailty-index.html", "Frailty Index Information"), align = "center"),
    )
  )
),


# Validation -----------------------------------------------------------------

tabItem(
  tabName = "validation",
  fluidRow(
    box(
      width = 12,
      h3("You have reached the results page. However, not enough questions were answered create a report. 
             Please review the contents of the table and navigate to previous pages to complete more questions. 
             You may use the buttons below this table to return to the desired pages.", align = "center"),
      
      column( 
        width = 12, 
        align = "center",
        br(),
        tableOutput("missingtable"),
        br(),
        
        h3("Buttons to return to previous pages."),
        actionButton('rtomobility', 'Page 1', width = 'auto'),
        actionButton('rtobadl', 'Page 2', width = 'auto'),
        actionButton('rtoiadl', 'Page 3', width = 'auto'),
        actionButton('rtoother', 'Page 4', width = 'auto'),
        actionButton('rtocc', 'Page 5', width = 'auto'),
        br(),
        br()
        # tableOutput("table_fi62_clean") # cannot put this in the results panel below for some reason, need to have it appear somewhere or else the validation won't work
      )
    )
  )
)
    )
  )
),
tags$footer("Adapted from the Canadian Longitudinal Study on Aging | R-Shiny Version 1.0 | Updated 2021-11-05", align = "center", style = "
              position:absolute;
              bottom:20;
              width:100%;
              color: white;
              background-color:#3c8dbc;
              padding: 10px;
              z-index: 1000;")
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  

  # Buttons -----------------------------------------------------------------
  
  observeEvent(input$showValue, {
    print(input$radio_click)
  })
  
  observeEvent(input$backtodemo, {
    updateTabItems(session, "tabs", "demo");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$tobadl, {
    updateTabItems(session, "tabs", "badl");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$backtobadl, {
    updateTabItems(session, "tabs", "badl");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$toiadl, {
    updateTabItems(session, "tabs", "iadl");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$backtoiadl, {
    updateTabItems(session, "tabs", "iadl");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$tomobility, {
    updateTabItems(session, "tabs", "mobility");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$backtomobility, {
    updateTabItems(session, "tabs", "mobility");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$toother, {
    updateTabItems(session, "tabs", "other");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$backtoother, {
    updateTabItems(session, "tabs", "other");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$tocc, {
    updateTabItems(session, "tabs", "cc");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$backtocc, {
    updateTabItems(session, "tabs", "cc");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$toresults, {
    updateTabItems(session, "tabs", "results");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
    observeEvent(input$tovalidation, {
    updateTabItems(session, "tabs", "validation");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  
  # validation page buttons
  
  observeEvent(input$rtobadl, {
    updateTabItems(session, "tabs", "badl");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$rtoiadl, {
    updateTabItems(session, "tabs", "iadl");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$rtomobility, {
    updateTabItems(session, "tabs", "mobility");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$rtoother, {
    updateTabItems(session, "tabs", "other");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  observeEvent(input$rtocc, {
    updateTabItems(session, "tabs", "cc");
    shinyjs::runjs("window.scrollTo(0, 0)");
  })
  
  
  # Output --------------------------------------------------------------------
  
  allData <- reactive({
    
    rawdata <- 
      tibble(
        badl_dress_1 = c(as.numeric(input$badl_dress_1))[1],
        badl_dress_2 = c(as.numeric(input$badl_dress_2))[1],
        badl_dress_3 = c(as.numeric(input$badl_dress_3))[1],
        badl_appear_1 = c(as.numeric(input$badl_appear_1))[1],
        badl_appear_2 = c(as.numeric(input$badl_appear_2))[1],
        badl_appear_3 = c(as.numeric(input$badl_appear_3))[1],
        badl_walk_1 = c(as.numeric(input$badl_walk_1))[1],
        badl_walk_2 = c(as.numeric(input$badl_walk_2))[1],
        badl_walk_3 = c(as.numeric(input$badl_walk_3))[1],
        badl_inbed_1 = c(as.numeric(input$badl_inbed_1))[1],
        badl_inbed_2 = c(as.numeric(input$badl_inbed_2))[1],
        badl_inbed_3 = c(as.numeric(input$badl_inbed_3))[1],
        badl_bath_1 = c(as.numeric(input$badl_bath_1))[1],
        badl_bath_2 = c(as.numeric(input$badl_bath_2))[1],
        badl_bath_3 = c(as.numeric(input$badl_bath_3))[1],
        
        iadl_phone_1 = c(as.numeric(input$iadl_phone_1))[1],
        iadl_phone_2 = c(as.numeric(input$iadl_phone_2))[1],
        iadl_phone_3 = c(as.numeric(input$iadl_phone_3))[1],
        iadl_getplace_1 = c(as.numeric(input$iadl_getplace_1))[1],
        iadl_getplace_2 = c(as.numeric(input$iadl_getplace_2))[1],
        iadl_getplace_3 = c(as.numeric(input$iadl_getplace_3))[1],
        iadl_shopping_1 = c(as.numeric(input$iadl_shopping_1))[1],
        iadl_shopping_2 = c(as.numeric(input$iadl_shopping_2))[1],
        iadl_shopping_3 = c(as.numeric(input$iadl_shopping_3))[1],
        iadl_prepmeals_1 = c(as.numeric(input$iadl_prepmeals_1))[1],
        iadl_prepmeals_2 = c(as.numeric(input$iadl_prepmeals_2))[1],
        iadl_prepmeals_3 = c(as.numeric(input$iadl_prepmeals_3))[1],
        iadl_housework_1 = c(as.numeric(input$iadl_housework_1))[1],
        iadl_housework_2 = c(as.numeric(input$iadl_housework_2))[1],
        iadl_housework_3 = c(as.numeric(input$iadl_housework_3))[1],
        iadl_meds_1 = c(as.numeric(input$iadl_meds_1))[1],
        iadl_meds_2 = c(as.numeric(input$iadl_meds_2))[1],
        iadl_meds_3 = c(as.numeric(input$iadl_meds_3))[1],
        iadl_money_1 = c(as.numeric(input$iadl_money_1))[1],
        iadl_money_2 = c(as.numeric(input$iadl_money_2))[1],
        iadl_money_3 = c(as.numeric(input$iadl_money_3))[1],
        
        diff_reach_1 = c(as.numeric(input$diff_reach_1))[1],
        diff_reach_2 = c(as.numeric(input$diff_reach_2))[1],
        diff_crouch_1 = c(as.numeric(input$diff_crouch_1))[1],
        diff_crouch_2 = c(as.numeric(input$diff_crouch_2))[1],
        diff_push_1 = c(as.numeric(input$diff_push_1))[1],
        diff_push_2 = c(as.numeric(input$diff_push_2))[1],
        diff_lift_1 = c(as.numeric(input$diff_lift_1))[1],
        diff_lift_2 = c(as.numeric(input$diff_lift_2))[1],
        diff_hndl_1 = c(as.numeric(input$diff_hndl_1))[1],
        diff_hndl_2 = c(as.numeric(input$diff_hndl_2))[1],
        diff_stand_1 = c(as.numeric(input$diff_stand_1))[1],
        diff_stand_2 = c(as.numeric(input$diff_stand_2))[1],
        diff_stndchair_1 = c(as.numeric(input$diff_stndchair_1))[1],
        diff_stndchair_2 = c(as.numeric(input$diff_stndchair_2))[1],
        diff_walkalone_1 = c(as.numeric(input$diff_walkalone_1))[1],
        diff_walkalone_2 = c(as.numeric(input$diff_walkalone_2))[1],
        diff_walkblock_1 = c(as.numeric(input$diff_walkblock_1))[1],
        diff_walkblock_2 = c(as.numeric(input$diff_walkblock_2))[1],
        diff_makebed_1 = c(as.numeric(input$diff_makebed_1))[1],
        diff_makebed_2 = c(as.numeric(input$diff_makebed_2))[1],
        diff_washback_1 = c(as.numeric(input$diff_washback_1))[1],
        diff_washback_2 = c(as.numeric(input$diff_washback_2))[1],
        diff_cutfood_1 = c(as.numeric(input$diff_cutfood_1))[1],
        diff_cutfood_2 = c(as.numeric(input$diff_cutfood_2))[1],
        diff_rec_1 = c(as.numeric(input$diff_rec_1))[1],
        diff_rec_2 = c(as.numeric(input$diff_rec_2))[1],
        
        health_1 = c(as.numeric(input$health))[1],
        eyesight_1 = c(as.numeric(input$eyesight))[1],
        hearing_1 = c(as.numeric(input$hearing))[1],
        weight_1 = c(as.numeric(input$weight))[1],
        falls_1 = c(as.numeric(input$falls_1))[1],
        falls_2 = c(as.numeric(input$falls_2))[1],
        falls_3 = c(as.numeric(input$falls_3))[1],
        effort_1 = c(as.numeric(input$effort))[1],
        lonely_1 = c(as.numeric(input$lonely))[1],
        getgoing_1 = c(as.numeric(input$getgoing))[1],
        pneumonia_1 = c(as.numeric(input$pneumonia))[1],
        uti_1 = c(as.numeric(input$uti))[1],
        
        ostknee_1 = c(as.numeric(input$ccs$ostknee))[1],
        osthip_1 = c(as.numeric(input$ccs$osthip))[1],
        osthand_1 = c(as.numeric(input$ccs$osthand))[1],
        rheu_1 = c(as.numeric(input$ccs$rheu))[1],
        otherarth_1 = c(as.numeric(input$ccs$otherarth))[1],
        copd_1 = c(as.numeric(input$ccs$copd))[1],
        hbp_1 = c(as.numeric(input$ccs$hbp))[1],
        diabetes_1 = c(as.numeric(input$ccs$diabetes))[1],
        chf_1 = c(as.numeric(input$ccs$chf))[1],
        angina_1 = c(as.numeric(input$ccs$angina))[1],
        heartattk_1 = c(as.numeric(input$ccs$heartattk))[1],
        pvd_1 = c(as.numeric(input$ccs$pvd))[1],
        stroke_1 = c(as.numeric(input$ccs$stroke))[1],
        ministroke_1 = c(as.numeric(input$ccs$ministroke))[1],
        memory_1 = c(as.numeric(input$ccs$memory))[1],
        alzh_1 = c(as.numeric(input$ccs2$alzh))[1],
        parkinsons_1 = c(as.numeric(input$ccs2$parkinsons))[1],
        stomach_1 = c(as.numeric(input$ccs2$stomach))[1],
        colitis_1 = c(as.numeric(input$ccs2$colitis))[1],
        bowelinc_1 = c(as.numeric(input$ccs2$bowelinc))[1],
        uriinc_1 = c(as.numeric(input$ccs2$uriinc))[1],
        cataracts_1 = c(as.numeric(input$ccs2$cataracts))[1],
        glaucoma_1 = c(as.numeric(input$ccs2$glaucoma))[1],
        md_1 = c(as.numeric(input$ccs2$md))[1],
        cancer_1 = c(as.numeric(input$ccs2$cancer))[1],
        osteoporosis_1 = c(as.numeric(input$ccs2$osteoporosis))[1],
        backproblems_1 = c(as.numeric(input$ccs2$backproblems))[1],
        hypothyroidism_1 = c(as.numeric(input$ccs2$hypothyroidism))[1],
        hyperthyroidism_1 = c(as.numeric(input$ccs2$hyperthyroidism))[1],
        kidneydis_1 = c(as.numeric(input$ccs2$kidneydis))[1]
      ) %>% 
      mutate(falls_3 = case_when(falls_3 == 1 ~ 0.5, # 1 = half a deficit, more than 1 = 1 deficit
                                 falls_3 > 1 ~ 1))
    
    # calculate missing variables here
    missingtable <-
      rawdata %>%
      mutate(
        badl_dress = ifelse(is.na(badl_dress_1) & is.na(badl_dress_2) & is.na(badl_dress_3), NA, 1),
        badl_appear = ifelse(is.na(badl_appear_1) & is.na(badl_appear_2) & is.na(badl_appear_3), NA, 1),
        badl_walk = ifelse(is.na(badl_walk_1) & is.na(badl_walk_2) & is.na(badl_walk_3), NA, 1),
        badl_inbed = ifelse(is.na(badl_inbed_1) & is.na(badl_inbed_2) & is.na(badl_inbed_3), NA, 1),
        badl_bath = ifelse(is.na(badl_bath_1) & is.na(badl_bath_2) & is.na(badl_bath_3), NA, 1),

        iadl_phone = ifelse(is.na(iadl_phone_1) & is.na(iadl_phone_2) & is.na(iadl_phone_3), NA, 1),
        iadl_getplace = ifelse(is.na(iadl_getplace_1) & is.na(iadl_getplace_2) & is.na(iadl_getplace_3), NA, 1),
        iadl_shopping = ifelse(is.na(iadl_shopping_1) & is.na(iadl_shopping_2) & is.na(iadl_shopping_3), NA, 1),
        iadl_prepmeals = ifelse(is.na(iadl_prepmeals_1) & is.na(iadl_prepmeals_2) & is.na(iadl_prepmeals_3), NA, 1),
        iadl_housework = ifelse(is.na(iadl_housework_1) & is.na(iadl_housework_2) & is.na(iadl_housework_3), NA, 1),
        iadl_meds = ifelse(is.na(iadl_meds_1) & is.na(iadl_meds_2) & is.na(iadl_meds_3), NA, 1),
        iadl_money = ifelse(is.na(iadl_money_1) & is.na(iadl_money_2) & is.na(iadl_money_3), NA, 1),

        diff_reach = ifelse(is.na(diff_reach_1) & is.na(diff_reach_2), NA, 1),
        diff_crouch = ifelse(is.na(diff_crouch_1) & is.na(diff_crouch_2), NA, 1),
        diff_push = ifelse(is.na(diff_push_1) & is.na(diff_push_2), NA, 1),
        diff_lift = ifelse(is.na(diff_lift_1) & is.na(diff_lift_2), NA, 1),
        diff_hndl = ifelse(is.na(diff_hndl_1) & is.na(diff_hndl_2), NA, 1),
        diff_stand = ifelse(is.na(diff_stand_1) & is.na(diff_stand_2), NA, 1),
        diff_stndchair = ifelse(is.na(diff_stndchair_1) & is.na(diff_stndchair_2), NA, 1),
        diff_walkalone = ifelse(is.na(diff_walkalone_1) & is.na(diff_walkalone_2), NA, 1),
        diff_walkblock = ifelse(is.na(diff_walkblock_1) & is.na(diff_walkblock_2), NA, 1),
        diff_makebed = ifelse(is.na(diff_makebed_1) & is.na(diff_makebed_2), NA, 1),
        diff_washback = ifelse(is.na(diff_washback_1) & is.na(diff_washback_2), NA, 1),
        diff_cutfood = ifelse(is.na(diff_cutfood_1) & is.na(diff_cutfood_2), NA, 1),
        diff_rec = ifelse(is.na(diff_rec_1) & is.na(diff_rec_2), NA, 1),

        falls = ifelse(is.na(falls_1) & is.na(falls_2) & is.na(falls_3), NA, 1),

        osteo = ifelse(is.na(ostknee_1) | is.na(osthip_1) | is.na(osthand_1), NA, 1),
        rheumatoid = ifelse(is.na(rheu_1) | is.na(otherarth_1), NA, 1),
      ) %>%
      select(health_1:rheumatoid) %>%
      select(-falls_1,-falls_2,-falls_3,-ostknee_1,-osthip_1,-osthand_1,-rheu_1,-otherarth_1) %>% # remove combined columns
      mutate(
        missingvars = rowSums(is.na(.)) # calculate the number of missing items, for a 62 item, a max of 12 can be missing
      )

    # calculate deficits here
    fidata <-
      rawdata %>%
      mutate( # combine some questions (coding)
        osteoarth_1 = if_else(ostknee_1 == 1| osthip_1 == 1 | osthand_1 == 1, 1, 0),
        rheuarth_1 = if_else(rheu_1 == 1 | otherarth_1 == 1, 1, 0)
      ) %>%
      select(-ostknee_1, -osthip_1, -osthand_1, -rheu_1, -otherarth_1) %>% # remove combined variables
      pivot_longer(cols = 1:ncol(.), names_to = "names", values_to = "value") %>%
      mutate(names_nn = gsub('.{2}$', '', names)) %>% # find last 2 characters and replace with blank
      group_by(names_nn) %>%
      summarise(sum = sum(value, na.rm = T)) %>%
      pivot_wider(names_from = "names_nn", values_from = "sum") %>%
      mutate(
        deficits = rowSums(.[1:62]),
        missingvars = missingtable$missingvars, # put missing in
        totalvar = 62-missingvars,
        fi = deficits/totalvar,
        age = input$age_years, # put age and sex in
        sex = c(as.numeric(input$sex_ask))[1]
      ) %>% 
      mutate(
        age = ifelse(age < 45, 45, age),
        age = ifelse(age > 85, 85, age),
      )

    # load and classify person
    normsfi62_clean <- # normative data here
      read.csv("data/normsfi62_clean.csv")

    fioutput <-
      normsfi62_clean %>%
      select(-X) %>%
      rename(Age_Norm = Age,
             Sex_Norm = Sex,
             FI_Norm = FI) %>%
      mutate_all(as.numeric) %>%
      mutate(
        deficits = fidata$deficits,
        missingvars = fidata$missingvars,
        totalvar = fidata$totalvar,
        fi = fidata$fi,
        age = fidata$age,
        sex = fidata$sex
      ) %>%
      mutate(
        fi_dist = FI_Norm - fi,
        fi_dist = if_else(fi_dist < 0, abs(fi_dist - 1), fi_dist) # add this to get fis higher than max
      ) %>%
      filter(age == Age_Norm & sex == Sex_Norm & fi_dist >=0) %>%
      slice(which.min(fi_dist)) %>% # get the minimum positive value, percentile column will show 
      mutate(categ =  case_when(between(percentile, 0, 19) ~ "poor",
                                between(percentile, 20, 39) ~ "fair",
                                between(percentile, 40, 59) ~ "average",
                                between(percentile, 60, 79) ~ "good",
                                between(percentile, 80, 100) ~ "excellent"))
      
        
    return(fioutput)
  })
  
  
  
  # return cleaned data table to UI for testing
  output$table_fi62_clean <- renderTable(
    {
      data <- allData()
      return(data)
    })
  outputOptions(output, 'table_fi62_clean', suspendWhenHidden=FALSE) # make missingvar work for submit button  
 
  # return missingvar for results page validation, right now will only view results with 12 or less missing items, if not will move to missing items page
    output$missing_VARS <- renderText({
    datamissvar <- allData()
    
    datamissvar <- 
      datamissvar %>% 
      mutate(missingvarcheck = case_when(missingvars > 12 ~ "nogood", # if more than 12 missing, not enough to calculate FI
                                         missingvars <= 12 ~ "good"))
    
    misscheck <- 
      datamissvar$missingvarcheck %>% as.character()
    
    return(misscheck)
  })
  outputOptions(output, 'missing_VARS', suspendWhenHidden=FALSE) # make missingvar work for submit button
  
  ### RESULTS UI
  
  # return percentile of fi score based on gender and age from normative data
  output$perctext <- renderText({
    data <- allData()
    perc <-
      data$percentile %>% as.character() %>% formatC(digits = 0)

    perctext <-
      paste0("Thank you for completing the online frailty index tool. Based on the responses from the questionnaire, you are doing better than ", perc, 
             "% of people your age and sex.")

    return(perctext)
  })
  
  # create a plot for results 
  output$plotperc <- renderPlot({
    data <- allData()
    dat.bar <- 
      tibble(
        bar = rep.int(20, 5),
        name = seq(1,5, by = 1)
      )
    
    dat.label <- 
      tibble(
        text = c("Poor", "Fair", "Average", "Good", "Excellent"),
        name = c(10,30,50,70,90)
      )
    
    dat.ticks <- 
      tibble(
        text = c("0%","100%"),
        name = c(0,100)
      )
    
    barplot <- 
      ggplot() +
      geom_bar(aes(x = "c", y = dat.bar$bar, fill = dat.bar$name), stat = "identity", width = 0.2) +
      geom_point(aes(x = "c", y = data$percentile), size = 10, shape = 16) +
      geom_text(aes(label = dat.label$text, x = "c", y = dat.label$name), size = 8, hjust = 0, nudge_x = 0.15) +
      geom_text(aes(label = dat.ticks$text, x = "c", y = dat.ticks$name), size = 7, hjust = 1, nudge_x = -0.15) +
      geom_text(aes(label = paste0("  ", data$percentile, "%"), x = "c", y = data$percentile), size = 7, hjust = 1, nudge_x = -0.15) +
      labs(y = NULL,
           x = NULL) +
      theme_void() +
      scale_fill_gradient2(midpoint = 3, low = "red", mid = "yellow", high = "green") +
      theme(legend.position = "none",
            axis.title=element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank())
    
    return(print(barplot))
  })
  
  # create a table for categories
  output$tableperc <- renderTable({
    perctable <- 
      tibble(
        cat = c("Excellent", "Good", "Average", "Fair", "Poor"),
        perc = c("More than 80%", "60% to 79%", "40% to 59%", "20% to 39%", "Less than 19%")
      )
    return(perctable)
  },
  bordered = TRUE,  
  spacing = 'm',
  align = 'c',
  colnames = FALSE,
  width = "80%")
  
  # frailty level commment
  output$frailtycomment <- renderText({
    data <- allData()

    full <-
      HTML(paste0("Your frailty level is ", data$categ, " compared to your peers."))

    return(full)
  })
  
  # return fi score in text
  output$fitext <- renderText({
    data <- allData()
    fiscore <-
      data$fi %>%
      round(digits = 2) %>%
      as.character() %>%
      formatC(digits = 0)
    
    fitext <- 
      paste0("Your frailty index score is: ", fiscore)
    
    return(fitext)
  })
  
  # Validating missing answers ----------------------------------------------

  missingData <- reactive({
    
     rawdata <- 
      tibble(
        diff_reach_1 = c(as.numeric(input$diff_reach_1))[1],
        diff_reach_2 = c(as.numeric(input$diff_reach_2))[1],
        diff_crouch_1 = c(as.numeric(input$diff_crouch_1))[1],
        diff_crouch_2 = c(as.numeric(input$diff_crouch_2))[1],
        diff_push_1 = c(as.numeric(input$diff_push_1))[1],
        diff_push_2 = c(as.numeric(input$diff_push_2))[1],
        diff_lift_1 = c(as.numeric(input$diff_lift_1))[1],
        diff_lift_2 = c(as.numeric(input$diff_lift_2))[1],
        diff_hndl_1 = c(as.numeric(input$diff_hndl_1))[1],
        diff_hndl_2 = c(as.numeric(input$diff_hndl_2))[1],
        diff_stand_1 = c(as.numeric(input$diff_stand_1))[1],
        diff_stand_2 = c(as.numeric(input$diff_stand_2))[1],
        diff_stndchair_1 = c(as.numeric(input$diff_stndchair_1))[1],
        diff_stndchair_2 = c(as.numeric(input$diff_stndchair_2))[1],
        diff_walkalone_1 = c(as.numeric(input$diff_walkalone_1))[1],
        diff_walkalone_2 = c(as.numeric(input$diff_walkalone_2))[1],
        diff_walkblock_1 = c(as.numeric(input$diff_walkblock_1))[1],
        diff_walkblock_2 = c(as.numeric(input$diff_walkblock_2))[1],
        diff_makebed_1 = c(as.numeric(input$diff_makebed_1))[1],
        diff_makebed_2 = c(as.numeric(input$diff_makebed_2))[1],
        diff_washback_1 = c(as.numeric(input$diff_washback_1))[1],
        diff_washback_2 = c(as.numeric(input$diff_washback_2))[1],
        diff_cutfood_1 = c(as.numeric(input$diff_cutfood_1))[1],
        diff_cutfood_2 = c(as.numeric(input$diff_cutfood_2))[1],
        diff_rec_1 = c(as.numeric(input$diff_rec_1))[1],
        diff_rec_2 = c(as.numeric(input$diff_rec_2))[1],
        
        badl_dress_1 = c(as.numeric(input$badl_dress_1))[1],
        badl_dress_2 = c(as.numeric(input$badl_dress_2))[1],
        badl_dress_3 = c(as.numeric(input$badl_dress_3))[1],
        badl_appear_1 = c(as.numeric(input$badl_appear_1))[1],
        badl_appear_2 = c(as.numeric(input$badl_appear_2))[1],
        badl_appear_3 = c(as.numeric(input$badl_appear_3))[1],
        badl_walk_1 = c(as.numeric(input$badl_walk_1))[1],
        badl_walk_2 = c(as.numeric(input$badl_walk_2))[1],
        badl_walk_3 = c(as.numeric(input$badl_walk_3))[1],
        badl_inbed_1 = c(as.numeric(input$badl_inbed_1))[1],
        badl_inbed_2 = c(as.numeric(input$badl_inbed_2))[1],
        badl_inbed_3 = c(as.numeric(input$badl_inbed_3))[1],
        badl_bath_1 = c(as.numeric(input$badl_bath_1))[1],
        badl_bath_2 = c(as.numeric(input$badl_bath_2))[1],
        badl_bath_3 = c(as.numeric(input$badl_bath_3))[1],
        
        iadl_phone_1 = c(as.numeric(input$iadl_phone_1))[1],
        iadl_phone_2 = c(as.numeric(input$iadl_phone_2))[1],
        iadl_phone_3 = c(as.numeric(input$iadl_phone_3))[1],
        iadl_getplace_1 = c(as.numeric(input$iadl_getplace_1))[1],
        iadl_getplace_2 = c(as.numeric(input$iadl_getplace_2))[1],
        iadl_getplace_3 = c(as.numeric(input$iadl_getplace_3))[1],
        iadl_shopping_1 = c(as.numeric(input$iadl_shopping_1))[1],
        iadl_shopping_2 = c(as.numeric(input$iadl_shopping_2))[1],
        iadl_shopping_3 = c(as.numeric(input$iadl_shopping_3))[1],
        iadl_prepmeals_1 = c(as.numeric(input$iadl_prepmeals_1))[1],
        iadl_prepmeals_2 = c(as.numeric(input$iadl_prepmeals_2))[1],
        iadl_prepmeals_3 = c(as.numeric(input$iadl_prepmeals_3))[1],
        iadl_housework_1 = c(as.numeric(input$iadl_housework_1))[1],
        iadl_housework_2 = c(as.numeric(input$iadl_housework_2))[1],
        iadl_housework_3 = c(as.numeric(input$iadl_housework_3))[1],
        iadl_meds_1 = c(as.numeric(input$iadl_meds_1))[1],
        iadl_meds_2 = c(as.numeric(input$iadl_meds_2))[1],
        iadl_meds_3 = c(as.numeric(input$iadl_meds_3))[1],
        iadl_money_1 = c(as.numeric(input$iadl_money_1))[1],
        iadl_money_2 = c(as.numeric(input$iadl_money_2))[1],
        iadl_money_3 = c(as.numeric(input$iadl_money_3))[1],
        
        health_1 = c(as.numeric(input$health))[1],
        eyesight_1 = c(as.numeric(input$eyesight))[1],
        hearing_1 = c(as.numeric(input$hearing))[1],
        weight_1 = c(as.numeric(input$weight))[1],
        falls_1 = c(as.numeric(input$falls_1))[1],
        falls_2 = c(as.numeric(input$falls_2))[1],
        falls_3 = c(as.numeric(input$falls_3))[1],
        effort_1 = c(as.numeric(input$effort))[1],
        lonely_1 = c(as.numeric(input$lonely))[1],
        getgoing_1 = c(as.numeric(input$getgoing))[1],
        pneumonia_1 = c(as.numeric(input$pneumonia))[1],
        uti_1 = c(as.numeric(input$uti))[1],
        
        ostknee_1 = c(as.numeric(input$ccs$ostknee))[1],
        osthip_1 = c(as.numeric(input$ccs$osthip))[1],
        osthand_1 = c(as.numeric(input$ccs$osthand))[1],
        rheu_1 = c(as.numeric(input$ccs$rheu))[1],
        otherarth_1 = c(as.numeric(input$ccs$otherarth))[1],
        copd_1 = c(as.numeric(input$ccs$copd))[1],
        hbp_1 = c(as.numeric(input$ccs$hbp))[1],
        diabetes_1 = c(as.numeric(input$ccs$diabetes))[1],
        chf_1 = c(as.numeric(input$ccs$chf))[1],
        angina_1 = c(as.numeric(input$ccs$angina))[1],
        heartattk_1 = c(as.numeric(input$ccs$heartattk))[1],
        pvd_1 = c(as.numeric(input$ccs$pvd))[1],
        stroke_1 = c(as.numeric(input$ccs$stroke))[1],
        ministroke_1 = c(as.numeric(input$ccs$ministroke))[1],
        memory_1 = c(as.numeric(input$ccs$memory))[1],
        alzh_1 = c(as.numeric(input$ccs2$alzh))[1],
        parkinsons_1 = c(as.numeric(input$ccs2$parkinsons))[1],
        stomach_1 = c(as.numeric(input$ccs2$stomach))[1],
        colitis_1 = c(as.numeric(input$ccs2$colitis))[1],
        bowelinc_1 = c(as.numeric(input$ccs2$bowelinc))[1],
        uriinc_1 = c(as.numeric(input$ccs2$uriinc))[1],
        cataracts_1 = c(as.numeric(input$ccs2$cataracts))[1],
        glaucoma_1 = c(as.numeric(input$ccs2$glaucoma))[1],
        md_1 = c(as.numeric(input$ccs2$md))[1],
        cancer_1 = c(as.numeric(input$ccs2$cancer))[1],
        osteoporosis_1 = c(as.numeric(input$ccs2$osteoporosis))[1],
        backproblems_1 = c(as.numeric(input$ccs2$backproblems))[1],
        hypothyroidism_1 = c(as.numeric(input$ccs2$hypothyroidism))[1],
        hyperthyroidism_1 = c(as.numeric(input$ccs2$hyperthyroidism))[1],
        kidneydis_1 = c(as.numeric(input$ccs2$kidneydis))[1]
      ) %>% 
      mutate(falls_3 = case_when(falls_3 == 1 ~ 0.5, # 1 = half a deficit, more than 1 = 1 deficit
                                 falls_3 > 1 ~ 1)) %>%
      mutate(
        diff_reach = ifelse(is.na(diff_reach_1) & is.na(diff_reach_2), NA, 1),
        diff_crouch = ifelse(is.na(diff_crouch_1) & is.na(diff_crouch_2), NA, 1),
        diff_push = ifelse(is.na(diff_push_1) & is.na(diff_push_2), NA, 1),
        diff_lift = ifelse(is.na(diff_lift_1) & is.na(diff_lift_2), NA, 1),
        diff_hndl = ifelse(is.na(diff_hndl_1) & is.na(diff_hndl_2), NA, 1),
        diff_stand = ifelse(is.na(diff_stand_1) & is.na(diff_stand_2), NA, 1),
        diff_stndchair = ifelse(is.na(diff_stndchair_1) & is.na(diff_stndchair_2), NA, 1),
        diff_walkalone = ifelse(is.na(diff_walkalone_1) & is.na(diff_walkalone_2), NA, 1),
        diff_walkblock = ifelse(is.na(diff_walkblock_1) & is.na(diff_walkblock_2), NA, 1),
        diff_makebed = ifelse(is.na(diff_makebed_1) & is.na(diff_makebed_2), NA, 1),
        diff_washback = ifelse(is.na(diff_washback_1) & is.na(diff_washback_2), NA, 1),
        diff_cutfood = ifelse(is.na(diff_cutfood_1) & is.na(diff_cutfood_2), NA, 1),
        diff_rec = ifelse(is.na(diff_rec_1) & is.na(diff_rec_2), NA, 1),
        
        badl_dress = ifelse(is.na(badl_dress_1) & is.na(badl_dress_2) & is.na(badl_dress_3), NA, 1),
        badl_appear = ifelse(is.na(badl_appear_1) & is.na(badl_appear_2) & is.na(badl_appear_3), NA, 1),
        badl_walk = ifelse(is.na(badl_walk_1) & is.na(badl_walk_2) & is.na(badl_walk_3), NA, 1),
        badl_inbed = ifelse(is.na(badl_inbed_1) & is.na(badl_inbed_2) & is.na(badl_inbed_3), NA, 1),
        badl_bath = ifelse(is.na(badl_bath_1) & is.na(badl_bath_2) & is.na(badl_bath_3), NA, 1),

        iadl_phone = ifelse(is.na(iadl_phone_1) & is.na(iadl_phone_2) & is.na(iadl_phone_3), NA, 1),
        iadl_getplace = ifelse(is.na(iadl_getplace_1) & is.na(iadl_getplace_2) & is.na(iadl_getplace_3), NA, 1),
        iadl_shopping = ifelse(is.na(iadl_shopping_1) & is.na(iadl_shopping_2) & is.na(iadl_shopping_3), NA, 1),
        iadl_prepmeals = ifelse(is.na(iadl_prepmeals_1) & is.na(iadl_prepmeals_2) & is.na(iadl_prepmeals_3), NA, 1),
        iadl_housework = ifelse(is.na(iadl_housework_1) & is.na(iadl_housework_2) & is.na(iadl_housework_3), NA, 1),
        iadl_meds = ifelse(is.na(iadl_meds_1) & is.na(iadl_meds_2) & is.na(iadl_meds_3), NA, 1),
        iadl_money = ifelse(is.na(iadl_money_1) & is.na(iadl_money_2) & is.na(iadl_money_3), NA, 1),

        falls = ifelse(is.na(falls_1) & is.na(falls_2) & is.na(falls_3), NA, 1)
      ) %>% 
       # select(badl_dress:diff_rec, health_1:weight_1, falls, effort_1:kidneydis_1) %>% 
       select(diff_reach:iadl_money, health_1:weight_1, falls, effort_1:kidneydis_1)
     
     rawdata %>% 
       pivot_longer(cols = 1:ncol(.), names_to = "Name", values_to = "Action") %>% 
       filter(is.na(Action)) %>%
       mutate(
         Question = case_when(
           Name == "diff_reach" ~ "Do you have any difficulty reaching or extending your arms above your shoulders?",
           Name == "diff_crouch" ~ "Do you have any difficulty stooping, crouching, or kneeling down?",
           Name == "diff_push" ~ "Do you have any difficulty pushing or pulling large objects like a living room chair?",
           Name == "diff_lift" ~ "Do you have any difficulty lifting 10 pounds (or 4.5 kg) from the floor, like a heavy bag of groceries?",
           Name == "diff_hndl" ~ "Do you have any difficulty handling small objects, like picking up a coin from a table?",
           Name == "diff_stand" ~ "Do you have any difficulty standing for a long period, around 15 minutes?",
           Name == "diff_stndchair" ~ "Do you have any difficulty standing up after sitting in a chair?",
           Name == "diff_walkalone" ~ "Do you have any difficulty walking alone up and down a flight of stairs?",
           Name == "diff_walkblock" ~ "Do you have any difficulty walking 2 to 3 neighbourhood blocks?",
           Name == "diff_makebed" ~ "Do you have any difficulty making a bed?",
           Name == "diff_washback" ~ "Do you have any difficulty washing your back?",
           Name == "diff_cutfood" ~ "Do you have any difficulty using a knife to cut food?",
           Name == "diff_rec" ~ "Do you have any difficulty with recreational or work activities in which you take some force or impact through your arm, shoulder, or hand (e.g., golf, hammering, tennis, typing, etc.)?",
           Name == "badl_dress" ~ "Can you dress and undress yourself (including picking out clothes and putting on socks & shoes)?",
           Name == "badl_appear" ~ "Can you take care of your own appearance, for example, combing your hair, shaving (if male)?",
           Name == "badl_walk" ~ "Can you walk?",
           Name == "badl_inbed" ~ "Can you get in and out of bed?",
           Name == "badl_bath" ~ "Can you take a bath or shower (including getting in or out of the tub)?",
           Name == "iadl_phone" ~ "Can you use the telephone, including looking up numbers and dialing?",
           Name == "iadl_getplace" ~ "Can you get to places out of walking distance (i.e., you drive your own car, or travel alone on buses, or taxis)?",
           Name == "iadl_shopping" ~ "Can you go shopping for groceries or clothes (taking care of all shopping needs yourself)?",
           Name == "iadl_prepmeals" ~ "Can you prepare your own meals (i.e., you plan and cook full meals yourself)?",
           Name == "iadl_housework" ~ "Can you do your housework (i.e., you can clean fl︎oors, etc.)?",
           Name == "iadl_meds" ~ "Can you take your own medicine (in the right doses at the right time)?",
           Name == "iadl_money" ~ "Can you handle your own money (i.e., you write cheques, pay bills, etc.)?",
           Name == "health_1" ~ "In general, would you say your health is...",
           Name == "eyesight_1" ~ "Is your eyesight, using glasses or corrective lens if you use them...",
           Name == "hearing_1" ~ "Is your hearing, using a hearing aid if you use one...",
           Name == "weight_1" ~ "Do you consider yourself...",
           Name == "falls" ~ "In the last 12 months, have you had any injuries that were serious enough to limit some of your normal activities? For example, a broken bone, a bad cut or burn, a sprain or a poisoning.",
           Name == "effort_1" ~ "How often did you feel that everything you did was an effort?",
           Name == "lonely_1" ~ "How often did you feel lonely?",
           Name == "getgoing_1" ~ "How often did you feel that you could not “get going”?",
           Name == "pneumonia_1" ~ "In the past year, have you seen a doctor for pneumonia?",
           Name == "uti_1" ~ "In the past year, have you seen a doctor for urinary tract infection (UTI)?",
           Name == "ostknee_1" ~ "Has a doctor ever told you that you have osteoarthritis in the knee?",
           Name == "osthip_1" ~ "Has a doctor ever told you that you have osteoarthritis in the hip?",
           Name == "osthand_1" ~ "Has a doctor ever told you that you have osteoarthritis in one or both hands?",
           Name == "rheu_1" ~ "Has a doctor ever told you that you have rheumatoid arthritis?",
           Name == "otherarth_1" ~ "Has a doctor ever told you that you have any other type of arthritis?",
           Name == "copd_1" ~ "Has a doctor ever told you that you have/had any of the following: emphysema, chronic bronchitis, chronic obstructive pulmonary disease (COPD), or chronic changes in lungs due to smoking?",
           Name == "hbp_1" ~ "Has a doctor ever told you that you have high blood pressure or hypertension?",
           Name == "diabetes_1" ~ "Has a doctor ever told you that you have diabetes, borderline diabetes or that your blood sugar is high?",
           Name == "chf_1" ~ "Has a doctor ever told you that you have heart disease (including congestive heart failure or CHF)?",
           Name == "angina_1" ~ "Has a doctor ever told you that you have angina (or chest pain due to heart disease)?",
           Name == "heartattk_1" ~ "Has a doctor ever told you that you have had a heart attack or myocardial infarction?",
           Name == "pvd_1" ~ "Has a doctor ever told you that you have peripheral vascular disease or poor circulation in your limbs?",
           Name == "stroke_1" ~ "Has a doctor ever told you that you have experienced a stroke or CVA (cerebrovascular accident)?",
           Name == "ministroke_1" ~ "Has a doctor ever told you that you have experienced a mini-stroke or TIA (transient ischemic attack)?",
           Name == "memory_1" ~ "Has a doctor ever told you that you have a memory problem?",
           Name == "alzh_1" ~ "Has a doctor ever told you that you have dementia or Alzheimer’s disease?",
           Name == "parkinsons_1" ~ "Has a doctor ever told you that you had Parkinsonism or Parkinson’s disease?",
           Name == "stomach_1" ~ "Has a doctor ever told you that you have intestinal or stomach ulcers?",
           Name == "colitis_1" ~ "Has a doctor ever told you that you have a bowel disorder such as Crohn’s Disease, ulcerative colitis, or Irritable Bowel Syndrome?",
           Name == "bowelinc_1" ~ "Has a doctor ever told you that you experience bowel incontinence?",
           Name == "uriinc_1" ~ "Has a doctor ever told you that you experience urinary incontinence?",
           Name == "cataracts_1" ~ "Has a doctor ever told you that you have cataracts?",
           Name == "glaucoma_1" ~ "Has a doctor ever told you that you have glaucoma?",
           Name == "md_1" ~ "Has a doctor ever told you that you have macular degeneration?",
           Name == "cancer_1" ~ "Has a doctor ever told you that you had cancer?",
           Name == "osteoporosis_1" ~ "Has a doctor ever told you that you have osteoporosis, sometimes called low bone mineral density, or thin, brittle or weak bones?",
           Name == "backproblems_1" ~ "Has a doctor ever told you that you have back problems, excluding fibromyalgia and arthritis?",
           Name == "hypothyroidism_1" ~ "Has a doctor ever told you that you have an UNDER-active thyroid gland (sometimes called hypothyroidism or myxedema)?",
           Name == "hyperthyroidism_1" ~ "Has a doctor ever told you that you have an OVER-active thyroid gland (sometimes called hyperthyroidism or Graves’ disease)?",
           Name == "kidneydis_1" ~ "Has a doctor ever told you that you have kidney disease or kidney failure?"
         ),
         "Page Number" = case_when(
           is.na(Action) & Name == "diff_reach" ~ "Page 1", 
           is.na(Action) & Name == "diff_crouch" ~ "Page 1",
           is.na(Action) & Name == "diff_push" ~ "Page 1", 
           is.na(Action) & Name == "diff_lift" ~ "Page 1", 
           is.na(Action) & Name == "diff_hndl" ~ "Page 1", 
           is.na(Action) & Name == "diff_stand" ~ "Page 1", 
           is.na(Action) & Name == "diff_stndchair" ~ "Page 1", 
           is.na(Action) & Name == "diff_walkalone" ~ "Page 1", 
           is.na(Action) & Name == "diff_walkblock" ~ "Page 1", 
           is.na(Action) & Name == "diff_makebed" ~ "Page 1", 
           is.na(Action) & Name == "diff_washback" ~ "Page 1", 
           is.na(Action) & Name == "diff_cutfood" ~ "Page 1", 
           is.na(Action) & Name == "diff_rec" ~ "Page 1", 
           is.na(Action) & Name == "badl_dress" ~ "Page 2", 
           is.na(Action) & Name == "badl_appear" ~ "Page 2", 
           is.na(Action) & Name == "badl_walk" ~ "Page 2",
           is.na(Action) & Name == "badl_inbed" ~ "Page 2", 
           is.na(Action) & Name == "badl_bath" ~ "Page 2", 
           is.na(Action) & Name == "iadl_phone" ~ "Page 3", 
           is.na(Action) & Name == "iadl_getplace" ~ "Page 3",
           is.na(Action) & Name == "iadl_shopping" ~ "Page 3",
           is.na(Action) & Name == "iadl_prepmeals" ~ "Page 3",
           is.na(Action) & Name == "iadl_housework" ~ "Page 3", 
           is.na(Action) & Name == "iadl_meds" ~ "Page 3", 
           is.na(Action) & Name == "iadl_money" ~ "Page 3", 
           is.na(Action) & Name == "health_1" ~ "Page 4", 
           is.na(Action) & Name == "eyesight_1" ~ "Page 4", 
           is.na(Action) & Name == "hearing_1" ~ "Page 4", 
           is.na(Action) & Name == "weight_1" ~ "Page 4", 
           is.na(Action) & Name == "falls" ~ "Page 4", 
           is.na(Action) & Name == "effort_1" ~ "Page 4", 
           is.na(Action) & Name == "lonely_1" ~ "Page 4", 
           is.na(Action) & Name == "getgoing_1" ~ "Page 4",
           is.na(Action) & Name == "pneumonia_1" ~ "Page 4",
           is.na(Action) & Name == "uti_1" ~ "Page 4", 
           is.na(Action) & Name == "ostknee_1" ~ "Page 5", 
           is.na(Action) & Name == "osthip_1" ~ "Page 5", 
           is.na(Action) & Name == "osthand_1" ~ "Page 5", 
           is.na(Action) & Name == "rheu_1" ~ "Page 5", 
           is.na(Action) & Name == "otherarth_1" ~ "Page 5", 
           is.na(Action) & Name == "copd_1" ~ "Page 5", 
           is.na(Action) & Name == "hbp_1" ~ "Page 5", 
           is.na(Action) & Name == "diabetes_1" ~ "Page 5", 
           is.na(Action) & Name == "chf_1" ~ "Page 5", 
           is.na(Action) & Name == "angina_1" ~ "Page 5",
           is.na(Action) & Name == "heartattk_1" ~ "Page 5",
           is.na(Action) & Name == "pvd_1" ~ "Page 5", 
           is.na(Action) & Name == "stroke_1" ~ "Page 5", 
           is.na(Action) & Name == "ministroke_1" ~ "Page 5", 
           is.na(Action) & Name == "memory_1" ~ "Page 5", 
           is.na(Action) & Name == "alzh_1" ~ "Page 5", 
           is.na(Action) & Name == "parkinsons_1" ~ "Page 5", 
           is.na(Action) & Name == "stomach_1" ~ "Page 5", 
           is.na(Action) & Name == "colitis_1" ~ "Page 5", 
           is.na(Action) & Name == "bowelinc_1" ~ "Page 5",
           is.na(Action) & Name == "uriinc_1" ~ "Page 5", 
           is.na(Action) & Name == "cataracts_1" ~ "Page 5",
           is.na(Action) & Name == "glaucoma_1" ~ "Page 5", 
           is.na(Action) & Name == "md_1" ~ "Page 5", 
           is.na(Action) & Name == "cancer_1" ~ "Page 5", 
           is.na(Action) & Name == "osteoporosis_1" ~ "Page 5", 
           is.na(Action) & Name == "backproblems_1" ~ "Page 5", 
           is.na(Action) & Name == "hypothyroidism_1" ~ "Page 5", 
           is.na(Action) & Name == "hyperthyroidism_1" ~ "Page 5",
           is.na(Action) & Name == "kidneydis_1" ~ "Page 5"
         )
       ) %>% 
       select(Question, "Page Number") %>% 
       rename("Unanswered Questions" = Question)
  })
  
    # return cleaned data table to UI for testing
  output$missingtable <- renderTable(
    bordered = TRUE,
    striped = TRUE,
    hover = TRUE,
    {
      data <- missingData()
      return(data)
    })
  outputOptions(output, 'missingtable', suspendWhenHidden=FALSE) # make missingvar work for submit button  

}

shinyApp(ui, server)
