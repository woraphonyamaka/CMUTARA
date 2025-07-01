ui <- dashboardPage(
  dashboardHeader(title = "CMU-TARA"),
  
  # Sidebar layout
  dashboardSidebar(width = 300,
                   tags$style(
                     HTML("
      body {
        background-color: white; 
        color: black; 
        font-family: Arial, sans-serif;
        font-size: 14px;  
      }
      .shiny-output-error {
        color: red;
      }
      .shiny-output-error:before {
        content: ' Check your inputs or API key';
      }
      label {
        font-weight: bold;
        color: black;
      }
      .alert {
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 10px;
      }
      .alert-secondary {
        background-color: #f8f9fa;
        color: #6c757d;
      }
      .alert-success {
        background-color: #d4edda;
        color: #155724;
      }
      .custom-title {
        display: flex;
        align-items: center;
        font-size: 24px;
        color: black;
      }
      .custom-title img {
        margin-right: 10px;
      }
      ")
                   ),
                   div(
                     div(
                       class = "custom-title",
                       img(src = "https://cee.econ.cmu.ac.th/wp-content/uploads/2021/01/logo_web.png", height = "50px"),
                       img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcReaafRJj99qahugDcMRycQ0wIAcHsIUDSsEg&s", height = "60px")
                     ),
                     style = "color: black; background-color: white; padding: 20px;"
                   ),
                   
                   sidebarMenu(
                     id = "menu",  
                     div(
                       h3("Welcome to the TA-RA assistant for", style = "font-size: 16px; font-weight: bold;"),
                       h3("statistics and econometrics!", style = "font-size: 16px; font-weight: bold;"),
                       p("This application allows you to chat with.", style = "font-size: 14px;"),
                       p("an OpenAI GPT model as your TA and RA.", style = "font-size: 14px;")
                     ),
                     
                     # API Key Input
                     #div(id="APIinput", 
                     #    textInput("api_key", label = "Your CHAT-GPT API key", value = "sk-proj-YRBznSfBQRmnMbG8P0ZVT3BlbkFJ6zKSSz1bDxwGweiUPrW5", width = "200px")),
                     #tags$style(type="text/css", "
                     
                     
                     # CSV/xlsx File Upload Input
                     div(id = "fileInputDiv",
                         fileInput(inputId = "file", 
                                   label = "Upload a CSV/xlsx file",
                                   buttonLabel = "Upload...", 
                                   accept = c(".csv", ".xlsx")),
                         
                         # Apply CSS to style the label and button
                         tags$style(HTML("
      #fileInputDiv label {
        color: white; /* Change the label text color to white */
        font-weight: bold; /* Optional: Make the text bold */
      }
      #fileInputDiv .btn {
        background-color: #333; /* Change button background color */
        color: white; /* Change button text color */
      }
    "))
                     ),
                     
                     #   tags$p("Find your own OpenAI API:", 
                     #          tags$a(href = "https://platform.openai.com/account/api-keys", target = "_blank", "Register here")
                     #   ), tags$hr(),
                     div(id = "modelInput",
                         selectInput("model_name", "Model Name", 
                                     choices = c("gpt-4o", "gpt-4", "gpt-3.5-turbo-0301", "gpt-3.5-turbo"), 
                                     selected = "gpt-4o"),
                         
                         # Apply CSS using tags$style to style the label or other parts
                         tags$style(HTML("
      #modelInput label { 
        color: white; /* Change the label text color to white */
        font-weight: bold; /* Optional: Make the text bold */
      }
      #modelInput select {
        background-color: #333; /* Optional: Change the dropdown background color */
        color: white; /* Change the dropdown text color */
      }
    "))
                     ),
                     
                     tags$hr(),
                     menuItem("TARA", icon = icon("info-circle"), tabName = "tara_info"),
                     # New Menu Item for Model Suggestion
                     menuItem("Model Suggestion", icon = icon("lightbulb"),
                              menuSubItem("Suggested Models", tabName = "model_suggestion")
                     ),
                     # Collapsible Statistics Menu
                     menuItem("Statistics", icon = icon("chart-bar"),
                              menuSubItem("Descriptive Statistics", tabName = "descriptive_statistics"),
                              menuItem("Hypothesis Testing", tabName = "hypothesis_testing",
                                       icon = icon("flask"),
                                       startExpanded = TRUE,  # Optional: Automatically expands this section in the sidebar
                                       menuSubItem("T-Test (mean comparison)", tabName = "t_test"),
                                       menuSubItem("ANOVA", tabName = "anova"),
                                       menuSubItem("Chi-Square Test", tabName = "chi_square"),
                                       menuSubItem("Variance comparison", tabName = "f_test")
                              )
                     ),
                     
                     # Collapsible Econometrics Menu
                     menuItem("Econometrics", icon = icon("chart-line"),
                              menuSubItem("Time series unit root test", tabName = "unitroot_ts"),
                              menuSubItem("Panel unit root test", tabName = "unitroot_panel"),
                              menuSubItem("Linear Regression", tabName = "linear_regression"),
                              menuSubItem("Binary Regression", tabName = "binary_regression"),
                              menuSubItem("ARIMA", tabName = "arima"),
                              menuSubItem("Error Correction Model", tabName = "ecm_model"),
                              menuSubItem("GARCH Model", tabName = "garch_model"),
                              menuSubItem("VAR Model", tabName = "var_model"),
                              menuSubItem("VECM Model", tabName = "vecm_model"),
                              menuSubItem("Panel Regression Model", tabName = "panel_regression_model"),
                              menuSubItem("Multinomial Choice Model", tabName = "multinomial_choice_model"),
                              menuSubItem("Ordinal Choice Model", tabName = "ordinal_choice_model")
                     ),
                     
                     # New Menu Item for plot
                     menuItem("Plot and Visualization", icon = icon("lightbulb"),
                              menuSubItem("Plot and Visualization", tabName = "plot")
                     ),
                     
                     tags$hr(),
                     tags$div(
                       style = "text-align:center; margin-top: 15px; color: black; background-color: #FFFFFF",
                       a(href = "https://cee.econ.cmu.ac.th/", target = "_blank",
                         img(src = "https://cee.econ.cmu.ac.th/wp-content/uploads/2021/01/logo_web.png", height = "30px"),
                         "Visit the developer at CEE website"
                       )
                     )
                   )
  ),
  
  
  dashboardBody(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML")
    ),
    
    fluidRow(
      column(width = 8,  
             fluidRow(
               column(12,  
                      tabItems(
                        tabItem(tabName = "tara_info",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Welcome to TARA Information Page"),
                                         p("The TARA assistant, developed by Chiang Mai University, serves as a Teaching Assistant and Research Assistant (TA-RA) 
                  for students and researchers working with econometrics, statistics, and various data analysis tasks."),
                                         p("The platform is equipped with powerful statistical and econometric modeling tools, making it easy to perform complex 
                  analyses with a user-friendly interface. TARA also integrates with OpenAI's language models to provide on-demand explanations 
                  and guidance in interpreting model outputs."),
                                         p("Features include descriptive statistics, hypothesis testing, regression analysis (linear, binary, VAR, VECM, etc.), and more. 
                  Additionally, visualizations can be generated to help better understand the results."),
                                         tags$hr(),
                                         tags$h4("Key Features"),
                                         tags$ul(
                                           tags$li("Descriptive Statistics and testing: Easily calculate common statistical measures and Hypothesis testing."),
                                           tags$li("Econometric Models: Perform various linear and system equation models."),
                                           tags$li("Interactive Chat Assistant: Ask questions and get assistance directly from the GPT model."),
                                           tags$li("Plot and Visualization: Generate and customize plots such as histograms, boxplots, and scatter plots.")
                                         ),
                                         p("📘 Download the full user manual here: ",
                                           a(href = "https://wyamaka.wordpress.com/wp-content/uploads/2025/07/user-guide-cmu-tara.pdf", 
                                             target = "_blank", "CMU-TARA User Guide (PDF)")
                                         ),
                                         tags$br(),
                                         p("📩 For technical issues or feedback, please contact Assoc. Prof. Woraphon Yamaka at ",
                                           a(href = "mailto:woraphon.econ@gmail.com", target = "_blank", "woraphon.econ@gmail.com"),
                                           ".")
                                )
                        ),
                        tabItem(tabName = "descriptive_statistics",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Descriptive Statistics"),
                                         checkboxGroupInput("checkStat", label = h3("Descriptive Statistics"),
                                                            choices = list("N" = 1, 
                                                                           "Missing" = 2, 
                                                                           "Mean" = 3,
                                                                           "Median"= 4,
                                                                           "Sum" = 5,
                                                                           "SD" = 6,
                                                                           "variance" = 7,
                                                                           "Min" = 8,
                                                                           "Max" = 9,
                                                                           "Skewness" = 10,
                                                                           "Kurtosis" = 11,
                                                                           "JB test" = 12),
                                                            selected = c(1, 2, 3),
                                                            inline = FALSE),
                                         tags$div(class = "box", style = "border: 1px solid #ccc; padding: 15px; margin-top: 15px; background-color: #f9f9f9;",
                                                  tags$h4("Table Statistics"),
                                                  div(
                                                    id = "table",
                                                    tableOutput("table1"),
                                                    class = "table",
                                                    style = "height: 340px; overflow-y: auto; border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9;"
                                                  )
                                         )
                                )
                        ),  
                        
                        tabItem(tabName = "hypothesis_testing",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Hypothesis Testing"),
                                         p("Please select a specific hypothesis test from the sidebar.")
                                )
                        ),
                        
                        tabItem(tabName = "t_test",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("T-Test"),
                                         uiOutput("t_test_ui")
                                )
                        ),
                        
                        tabItem(tabName = "anova",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("ANOVA"),
                                         uiOutput("anova_ui")
                                )
                        ),
                        
                        
                        tabItem(tabName = "chi_square",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Pearson's Chi-Square Test"),
                                         uiOutput("chi_square_ui")
                                )
                        ),
                        
                        
                        tabItem(tabName = "f_test",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Variance comparison F-Test"),
                                         uiOutput("f_test_ui")
                                )
                        ),
                        tabItem(tabName = "unitroot_ts",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Time series unit root test"),
                                         uiOutput("unitroot_ts_ui")
                                )
                        ),
                        tabItem(tabName = "unitroot_panel",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Panel unit root test"),
                                         uiOutput("unitroot_panel_ui")
                                )
                        ),
                        
                        tabItem(tabName = "linear_regression",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Linear Regression"),
                                         uiOutput("regression_ui")
                                )
                        ),
                        tabItem(tabName = "binary_regression",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Binary Regression"),
                                         uiOutput("binary_regression_ui")
                                         
                                )
                        ),
                        tabItem(tabName = "arima",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("ARIMA"),
                                         uiOutput("arima_ui")
                                )
                        ),
                        tabItem(tabName = "ecm_model",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Error Correction Model"),
                                         uiOutput("ecm_regression_ui")
                                )
                        ),
                        tabItem(tabName = "garch_model",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("GARCH(1,1) Model"),
                                         uiOutput("garch_ui")
                                )
                        ),
                        tabItem(tabName = "var_model",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("VAR Model"),
                                         uiOutput("var_ui")
                                )
                        ),
                        tabItem(tabName = "vecm_model",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("VECM Model"),
                                         uiOutput("vecm_ui")
                                         
                                )
                        ),
                        tabItem(tabName = "panel_regression_model",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Panel Regression Model"),
                                         uiOutput("panel_regression_ui")
                                         
                                )
                        ),
                        tabItem(tabName = "multinomial_choice_model",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Multinomial Choice Model"),
                                         uiOutput("multinomial_choice_ui")
                                ) 
                        ),
                        tabItem(tabName = "ordinal_choice_model",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Ordinal Choice Model"),
                                         uiOutput("ordinal_choice_ui")
                                ) 
                        ),
                        tabItem(tabName = "model_suggestion",
                                tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                                         tags$h4("Model Suggestion"),
                                         uiOutput("suggestion_ui"),   
                                         withMathJax(uiOutput("suggestion_output"))  
                                )
                        ),
                        tabItem(tabName = "plot",
                                tags$div(class = "box", style = style1,
                                         tags$h4("Select Plot Type"),
                                         
                                         fluidRow(
                                           # First column (6 units wide)
                                           column(6, 
                                                  radioButtons("plot_type", "Choose a plot type:",
                                                               choices = list(
                                                                 "Histogram" = "hist", 
                                                                 "Boxplot" = "box", 
                                                                 "Scatter Plot" = "scatter"
                                                               )
                                                  ),
                                                  
                                                  # First set of uiOutputs for the first column
                                                  uiOutput("select_var"),
                                                  uiOutput("select_var2"),
                                                  uiOutput("select_color")
                                           ),
                                           
                                           # Second column (6 units wide)
                                           column(6, 
                                                  # Second set of uiOutputs for the second column
                                                  uiOutput("plot_title"),
                                                  uiOutput("x_axis_title"),
                                                  uiOutput("y_axis_title"),
                                                  uiOutput("title_font")
                                           )
                                         )
                                )
                        )
                        
                      )
               )
             ),
             
             
             fluidRow(
               column(12,
                      tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                               tags$h4("Estimation result"),
                               tabsetPanel(
                                 tabPanel("Estimation result", verbatimTextOutput("summary_result")),
                                 tabPanel("Theory", withMathJax(uiOutput("explain_theory")))
                               )
                      )
               )
             ),
             
             fluidRow(
               column(12,
                      tags$div(class = "box", style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;",
                               tags$h4("Interpretation"),
                               withMathJax(uiOutput("interpret_result"))
                      )
               )
             )
      ),
      
      # Third column (right side)
      column(width = 4,  
             fluidRow(
               column(12, 
                      tags$div(class = "box", style = style1,
                               tags$h4("Plot and illustration"),
                               # Adding a plot output to render the selected plot
                               card(
                                 height = 275,
                                 full_screen = TRUE, plotlyOutput("plot_output")),
                               actionButton("expand_plot", "Expand Plot", icon = icon("expand"))
                      )
               )
             ),
             fluidRow(
               column(12, 
                      tags$h3("Chat History"), 
                      tags$hr(), 
                      div(
                        id = "chat_history",
                        uiOutput("chat_history"),
                        class = "chat-history",
                        style = "height: 300px; overflow-y: auto; border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9;"
                      ), 
                      tags$hr(),
                      textAreaInput(inputId = "user_message", placeholder = "Enter your message:", label = "USER PROMPT", width = "100%"),
                      actionButton("send_message", "Send", icon = icon("play"))
               )
             )
      )
    )
  )
)
