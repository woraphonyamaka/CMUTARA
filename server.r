server <- function(input, output, session) {
  
  chat_data <- reactiveVal(data.frame())
  
  # Observe when the 'Send' button is clicked or Enter is pressed in the text area
  observeEvent(input$send_message, {
    sendMessage()
  })
  
  # Function to handle sending messages
  sendMessage <- function() {
    req( input$user_message)
    
    new_data <- data.frame(source = "User", message = input$user_message, stringsAsFactors = FALSE)
    chat_data(rbind(chat_data(), new_data))
    
    gpt_res <- call_gpt_api(api_key, input$user_message, input$model_name)
    
    if (!is.null(gpt_res)) {
      gpt_data <- data.frame(source = "ChatGPT", message = gpt_res, stringsAsFactors = FALSE)
      chat_data(rbind(chat_data(), gpt_data))
    }
    
    updateTextAreaInput(session, "user_message", value = "")
  }
  
  # Create a custom scroll event listener for the chat-history div
  js_scroll <- "
  $(document).ready(function() {
    $('#user_message').on('keydown', function(e) {
      if (e.keyCode == 13 && !e.shiftKey) {
        $('#send_message').click();
        e.preventDefault();
      }
    });
    
    function scrollChatHistory() {
      var chatHistory = document.querySelector('#chat_history');
      chatHistory.scrollTop = chatHistory.scrollHeight;
    }
    
    Shiny.addCustomMessageHandler('scrollToBottom', function(message) {
      scrollChatHistory();
    });
    
    scrollChatHistory();
  });
  "
  runjs(js_scroll)
  
  # Display Chat History
  output$chat_history <- renderUI({
    if (nrow(chat_data()) > 0) {
      lapply(1:nrow(chat_data()), function(i) {
        div(
          class = ifelse(chat_data()[i, "source"] == "User", "alert alert-secondary", "alert alert-success"),
          chat_data()[i, "message"]
        )
      })
    }
  })
  
  # API Call Function
  call_gpt_api <- function(api_key, prompt, model_name) {
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type("application/json"),
      encode = "json",
      body = list(
        model = model_name,
        messages = list(
          list(role = "user", content = prompt)
        )
      )
    )
    
    if (response$status_code == 200) {
      return(str_trim(content(response)$choices[[1]]$message$content))
    } else {
      return(NULL)
    }
  }
  
  # Upload CSV and show regression UI
  
  
  # Reactive to store uploaded CSV data
  data <- reactive({
    req(input$file)
    
    ext <- file_ext(input$file$name)
    switch(ext,
           csv = read_csv(input$file$datapath),
           xlsx = read_xlsx(input$file$datapath),
           validate("Invalid file; Please upload a .csv or .xlsx file")
    )
  })
  
  
  ### ==== estimate and plot each model ========================
  
  output$t_test_ui <- renderUI({
    req(input$file)
    
    tagList(
      selectInput("t_test_type", "Select T-Test Type:", 
                  choices = c("One Sample", "Two Sample", "Paired")),
      selectInput("t_test_variable", "Select First Variable for T-Test:", choices = names(data())),
      
      # Only show this input when "Two Sample" or "Paired" is selected
      conditionalPanel(
        condition = "input.t_test_type == 'Two Sample' || input.t_test_type == 'Paired'",
        selectInput("t_test_variable_2", "Select Second Variable for T-Test:", choices = names(data())),
        selectInput("conf_level", "Select Confidence Level", choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99))
      ),
      
      # Only show mean input when "One Sample" is selected
      conditionalPanel(
        condition = "input.t_test_type == 'One Sample'",
        numericInput("t_test_mean", "Mean (for One Sample T-Test):", value = 0),
        selectInput("conf_level", "Select Confidence Level", choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99))
      ),
      
      actionButton("run_t_test", "Run T-Test")
    )
  })
  
  output$anova_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("anova_response", "Select Response Variable:", choices = names(data())),
      selectInput("anova_factor", "Select Factor Variable:", choices = names(data()), multiple = TRUE),
      actionButton("run_anova", "Run ANOVA")
    )
  })
  
  output$chi_square_ui <- renderUI({
    req(input$file)
    
    tagList(
      selectInput("chi_square_variable_1", "Select First Categorical Variable:", choices = names(data())),
      selectInput("chi_square_variable_2", "Select Second Categorical Variable:", choices = names(data())),
      actionButton("run_chi_square", "Run Chi-Square Test")
    )
  })
  
  
  output$f_test_ui <- renderUI({
    req(input$file)
    
    tagList(
      # Dropdown for selecting F-Test Type
      selectInput("f_test_type", "Select F-Test Type:", 
                  choices = c("One Sample", "Two Sample")),
      
      # Input for One-Sample F-Test
      conditionalPanel(
        condition = "input.f_test_type == 'One Sample'",
        selectInput("f_test_var1", "Select Variable for F-Test:", choices = names(data())),
        numericInput("f_test_population_variance", "Hypothesized Population Variance:", value = 1)
      ),
      
      # Input for Two-Sample F-Test
      conditionalPanel(
        condition = "input.f_test_type == 'Two Sample'",
        selectInput("f_test_var1", "Select First Variable:", choices = names(data())),
        selectInput("f_test_var2", "Select Second Variable:", choices = names(data()))
      ),
      
      actionButton("run_f_test", "Run F-Test")
    )
  })
  
  
  
  t_test_results <- eventReactive(input$run_t_test, {
    req(data(), input$t_test_variable, input$t_test_type, input$conf_level)
    conf_level <- as.numeric(input$conf_level)
    # Extract the first variable
    variable_data <- data()[[input$t_test_variable]]
    
    if (input$t_test_type == "One Sample") {
      # One Sample T-Test
      result <- t.test(variable_data, mu = input$t_test_mean, conf.level = conf_level)

    } else if (input$t_test_type == "Two Sample") {
      # Two Sample T-Test
      req(input$t_test_variable_2)
      variable_data_2 <- data()[[input$t_test_variable_2]]
      result <- t.test(variable_data, variable_data_2, conf.level = conf_level)
      
    } else if (input$t_test_type == "Paired") {
      # Paired T-Test
      req(input$t_test_variable_2)
      variable_data_2 <- data()[[input$t_test_variable_2]]
      result <- t.test(variable_data, variable_data_2, paired = TRUE, conf.level=conf_level)
    }
    list(
      summary_result=result,
      variable_name=c(input$t_test_variable),input$t_test_variable_2)
  })
  
  # Run ANOVA Analysis
  anova_results <- eventReactive(input$run_anova, {
    req(data(), input$anova_response, input$anova_factor)
    
    response <- input$anova_response
    factors <- paste(input$anova_factor, collapse = " + ")
    formula <- as.formula(paste(response, "~", factors))
    
    result <- aov(formula, data = data())
    summary(result)
  })
  
  # Run Chi-Square Test Analysis
  chi_square_results <- eventReactive(input$run_chi_square, {
    req(data(), input$chi_square_variable_1, input$chi_square_variable_2)
    
    # Create a contingency table using two categorical variables
    contingency_table <- table(data()[[input$chi_square_variable_1]], data()[[input$chi_square_variable_2]])
    
    # Perform Pearson's Chi-Square Test of Independence
    result <- chisq.test(contingency_table)
    list(
      summary_result=result,
      variable_name=c(input$chi_square_variable_1),input$chi_square_variable_2)
    
  })
  
  # Run the F-Test Analysis
  f_test_results <- eventReactive(input$run_f_test, {
    req(data(), input$f_test_type)
    
    if (input$f_test_type == "One Sample") {
      # One-Sample F-Test
      variable_data <- data()[[input$f_test_var1]]
      hypothesized_variance <- input$f_test_population_variance
      
      # Calculating the F statistic manually since there is no direct F-Test function for one sample
      sample_variance <- var(variable_data)
      f_statistic <- sample_variance / hypothesized_variance
      df <- length(variable_data) - 1
      p_value <- pf(f_statistic, df, df, lower.tail = FALSE)
      
      result <- list(
        "F Statistic" = f_statistic,
        "Degrees of Freedom" = df,
        "P-Value" = p_value
      )
      
    } else if (input$f_test_type == "Two Sample") {
      # Two-Sample F-Test
      req(input$f_test_var1, input$f_test_var2)
      var1 <- data()[[input$f_test_var1]]
      var2 <- data()[[input$f_test_var2]]
      
      # Using var.test() for Two-Sample F-Test
      result <- var.test(var1, var2)
    }
    
    list(
      summary_result=result,
      variable_name=c(input$f_test_var1),input$f_test_var_2)
    
  })
  
  
  
  
  output$unitroot_ts_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("adf_variable", "Select Variables for ADF Test", choices = names(data()), multiple = TRUE),
      radioButtons("adf_type", "Select Type of Test:",
                   choices = c("None" = "none", "Drift" = "drift", "Trend" = "trend"),
                   selected = "drift"),
      selectInput("adf_lag_selection", "Select Lag Selection Method:",
                  choices = c("Fixed", "AIC", "BIC"), selected = "AIC"),
      numericInput("adf_fixed_lag", "Fixed Lag (if selected):", value = 1, min = 0),
      actionButton("run_adf_test", "Run ADF Test"),
    )
  })
  
  # ADF Test Analysis using ur.df() from urca package
  adf_test_results <- eventReactive(input$run_adf_test, {
    req(data(), input$adf_variable)
    
    # Extract the selected variable from the data
    series <- data()[[input$adf_variable]]
    test_type <- input$adf_type
    lag_selection <- input$adf_lag_selection
    fixed_lag <- input$adf_fixed_lag
    
    # Determine the lag selection method
    if (lag_selection == "Fixed") {
      lags <- fixed_lag
    } else if (lag_selection == "AIC") {
      lags <- "AIC"
    } else if (lag_selection == "BIC") {
      lags <- "BIC"
    }
    
    # Perform the ADF test using ur.df()
    adf_result <- ur.df(series, type = test_type, lags = ifelse(is.numeric(lags), lags, 0), selectlags = lag_selection)
    list(
      Time_series_unitroot_result = summary(adf_result),
      Time_series_variable_is = input$adf_variable
    )
    
    
  })
  
  
  
  #### panel unit root test
  output$unitroot_panel_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("panel_variable", "Select Variables for Panel Unit Root Test", choices = names(data()), multiple = TRUE),
      selectInput("panel_id", "Select Panel ID Variable", choices = names(data())),
      selectInput("panel_time", "Select Time Variable", choices = names(data())),
      radioButtons("panel_test_type", "Select Type of Test:",
                   choices = c("Levin-Lin-Chu (LLC)" = "levinlin",
                               "Im-Pesaran-Shin (IPS)" = "ips",
                               "Maddala-Wu (Fisher)" = "madwu",
                               "Hadri Test" = "hadri"),
                   selected = "levinlin"),
      radioButtons("panel_exo", "Select Deterministic Component:",
                   choices = c("None" = "none", "Intercept" = "intercept", "Trend" = "trend"),
                   selected = "intercept"),
      selectInput("panel_lag_selection", "Select Lag Selection Method:",
                  choices = c("Fixed", "AIC", "BIC", "Hall"), selected = "AIC"),
      numericInput("panel_fixed_lag", "Fixed Lag (if selected):", value = 1, min = 0),
      actionButton("run_panel_unitroot_test", "Run Panel Unit Root Test"),
    )
  })
  
  
  # Panel Unit Root Test Analysis using purtest() from plm package
  panel_unitroot_test_results <- eventReactive(input$run_panel_unitroot_test, {
    req(data(), input$panel_variable, input$panel_id, input$panel_time)
    
    # Extract user inputs
    variable <- input$panel_variable
    id_var <- input$panel_id
    time_var <- input$panel_time
    test_type <- input$panel_test_type
    lag_selection <- input$panel_lag_selection
    fixed_lag <- input$panel_fixed_lag
    
    # Prepare the panel data
    panel_data <- pdata.frame(data(), index = c(id_var, time_var))
    
    # Determine the lag selection method
    lags <- if (lag_selection == "Fixed") {
      fixed_lag
    } else {
      lag_selection  # Either "AIC" or "BIC"
    }
    
    # Try to run the panel unit root test and catch any errors
    
    panel_result <- purtest(panel_data[[variable]], test = test_type, lags = ifelse(is.numeric(lags), lags, 0), exo = "intercept")
    list(
      panel_unitroot_result = panel_result,
      panel_variable_is = input$panel_variable
    )
    
    
  })
  
  # 1)  Perform linear regression and display the result
  
  output$regression_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("dependent_variable", "Select Dependent Variable (Y)", names(data())),
      selectInput("independent_variables", "Select Independent Variables (X)", names(data()), multiple = TRUE),
      checkboxInput("header", "Header", TRUE),
      actionButton("analyze_linear", "Analyze Linear Regression")
    )
  })
  
  linear_analysis_results <- eventReactive(input$analyze_linear, {
    library(lmtest)
    library(mctest)
    
    req(data(), input$dependent_variable, input$independent_variables)
    dependent_variable <- input$dependent_variable
    independent_variables <- input$independent_variables
    formula_ols <- as.formula(paste(dependent_variable, "~", paste(independent_variables, collapse = "+")))
    ols_model <- lm(formula_ols, data = data())
    list(
      regression_result =  summary(ols_model),
      Heteroscedasticity =   bptest(ols_model),
      Multicollinearity = imcdiag(ols_model)
    )
  })
  
  
  
  
  
  # 2)  Perform binary regression and display the result
  output$binary_regression_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("binary_dependent_variable", "Select Dependent Variable (Y)", names(data())),
      selectInput("binary_independent_variables", "Select Independent Variables (X)", names(data()), multiple = TRUE),
      checkboxInput("header", "Header", TRUE),
      radioButtons("binary_model_choice", "Choose Model Type:", 
                   choices = c("Logit" = "logit", "Probit" = "probit"),
                   selected = "logit"),
      checkboxInput("calculate_marginal_effects", "Calculate Marginal Effects", value = FALSE),
      actionButton("analyze_binary", "Analyze Binary Regression")
    )
  })
  
  binary_analysis_results <- eventReactive(input$analyze_binary, {
    req(data(), input$binary_dependent_variable, input$binary_independent_variables, input$binary_model_choice)
    
    # Extract user inputs
    binary_dependent_variable <- input$binary_dependent_variable
    binary_independent_variables <- input$binary_independent_variables
    
    # Define the model formula
    formula <- as.formula(paste(binary_dependent_variable, "~", paste(binary_independent_variables, collapse = "+")))
    
    # Fit the model based on user choice (logit or probit)
    if (input$binary_model_choice == "logit") {
      bi_model <- glm(formula, data = data(), family = binomial(link = "logit"))
      predictions <- ifelse(predict(bi_model, type = "response") > 0.5, 1, 0)
      actuals <- data()[[input$binary_dependent_variable]] # Replace with actual dependent variable name
      accuracy <- mean(predictions == actuals)
      
    } else {
      bi_model <- glm(formula, data = data(), family = binomial(link = "probit"))
      predictions <- ifelse(predict(bi_model, type = "response") > 0.5, 1, 0)
      actuals <- data()[[input$binary_dependent_variable]]   # Replace with actual dependent variable name
      accuracy <- mean(predictions == actuals)
    }
    
    # Calculate marginal effects if requested
      marginal_effects <- NULL
    if (input$calculate_marginal_effects) {
      library("mfx")
      marginal_effects=probitmfx(formula,data=data())
    }
    
    # Return the results
    list(
      model_summary = summary(bi_model),
      marginal_effects = marginal_effects,
      Percentage_Prediction_Accuracy =  accuracy*100
    )
  })
  
  # 3) Perform ARIMA regression and display the result with forecasting
  output$arima_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("arima_dependent_variable", "Select Time Series Variable (Y)", names(data())),
      numericInput("arima_order_p", "Lag Order for AR (p)", value = 1, min = 0),
      numericInput("arima_order_d", "Integrated Order (d)", value = 1, min = 0),
      numericInput("arima_order_q", "Lag Order for MA (q)", value = 1, min = 0),
      dateInput("start_date", "Select Start Date", value = Sys.Date(), format = "yyyy-mm-dd"),
      numericInput("frequency", "Select Frequency", value = 1, min = 1, max = 365),
      numericInput("n_forecast", "Number of Steps Ahead to Forecast", value = 10, min = 1),
      checkboxInput("header", "Header", TRUE),
      actionButton("analyze_arima", "Analyze ARIMA Model")
    )
  })
  
  
  # ARIMA Regression Analysis with Forecasting
  arima_analysis_results <- eventReactive(input$analyze_arima, {
    req(data(), input$arima_dependent_variable, input$arima_order_p, input$arima_order_d, input$arima_order_q, input$start_date, input$frequency, input$n_forecast)
    
    # Extract the time series data based on the input variable
    Y <- data()[[input$arima_dependent_variable]]
    
    # Load the necessary library
    library(forecast)
    
    # Convert data to time series object
    start_date <- as.Date(input$start_date)
    frequency <- input$frequency
    
    # Assuming frequency is yearly, monthly, etc. you need to correctly set the ts object.
    Y_ts <- ts(Y, start = c(as.numeric(format(start_date, "%Y")), as.numeric(format(start_date, "%m"))), frequency = frequency)
    
    
    # Fit the ARIMA model based on user input
    arima_order <- c(input$arima_order_p, input$arima_order_d, input$arima_order_q)
    arima_fit <- Arima(Y_ts, order = arima_order)
    
    # Perform forecasting for the specified number of steps ahead
    arima_forecast <- forecast(arima_fit, h = input$n_forecast)
    
    # Return the ARIMA summary, fitted model, and forecast
    list(
      arima_result = summary(arima_fit),
      arima_forecast = arima_forecast,
      order = arima_order,
      variable=input$arima_dependent_variable
    )
  })
  
  
  
  
  # 3)  Perform error correction regression and display the result
  output$ecm_regression_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("ecm_dependent_variable", "Select Dependent Variable (Y)", names(data())),
      selectInput("ecm_independent_variables", "Select Independent Variables (X)", names(data()), multiple = TRUE),
      checkboxInput("header", "Header", TRUE),
      actionButton("analyze_ecm", "Analyze ECM Regression")
    )
  })
  
  # ECM Regression Analysis
  ecm_analysis_results <- eventReactive(input$analyze_ecm, {
    req(data(), input$ecm_dependent_variable, input$ecm_independent_variables)
    
    # Extract the actual data based on the input names
    Y <- data()[[input$ecm_dependent_variable]]
    X<- data()[, input$ecm_independent_variables, drop = FALSE]  # Drop = FALSE to ensure it's a dataframe if selecting multiple variables
    
    # Run ECM and cointegration test
    list(
      ecm_result = ecm(Y , as.matrix(X) ,  output = FALSE ),
      coint_test_result = coint.test(Y , as.matrix(X),  output = FALSE),
      formula = as.formula(paste(input$ecm_dependent_variable, "~", paste(input$ecm_independent_variables, collapse = " + ")))
    )
    
  })
  
  
  
  # VAR model
  # VAR model UI
  output$var_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("endogenous_variable", "Select Endogenous Variables", names(data()), multiple = TRUE),
      selectInput("exogenous_variable", "Select Exogenous Variables", names(data()), multiple = TRUE),
      checkboxInput("header", "Header", TRUE),
      selectInput("lag", "Select Lag", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), multiple = FALSE),
      radioButtons("VAR_model_type", "Choose Model Type:", 
                   choices = c("none" = "none", "const" = "const", "trend" = "trend", "both" = "both"),
                   selected = "const"),
      actionButton("analyze_var", "Analyze VAR")
    )
  })
  
  # VAR Analysis
  var_analysis_results <- eventReactive(input$analyze_var, {
    req(data(), input$endogenous_variable, input$lag, input$VAR_model_type)
    
    # Extract the endogenous and exogenous variables
    Y <- data()[, input$endogenous_variable, drop = FALSE]
    
    # Only include exogenous variables if they have been selected
    if (!is.null(input$exogenous_variable) && length(input$exogenous_variable) > 0) {
      X <- data()[, input$exogenous_variable, drop = FALSE]
    } else {
      X <- NULL  # Set X to NULL if no exogenous variables are selected
    }
    
    # Convert inputs to correct formats
    P <- as.numeric(input$lag)  # Ensure lag is numeric
    Type <- input$VAR_model_type  # Model type
    
    # Run lag length selection and the VAR model
    lag_selection <- VARselect(Y, lag.max = P, exogen = X, type = Type)
    model <- VAR(Y, p = P, exogen = X, type = Type)
    
    list(
      Lag_length_selection = lag_selection,
      VAR_result = summary(model)
    )
  })
  
  # VECM Model 
  
  output$vecm_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("endogenous_variable", "Select Endogenous Variables", names(data()), multiple = TRUE),
      checkboxInput("header", "Header", TRUE),
      selectInput("lag", "Select Lag Order", choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), multiple = FALSE),
      selectInput("rank", "Number of cointegration (less than N-1)", choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), multiple = FALSE),
      radioButtons("coint_test_type", "Choose Cointegration Test Type:",
                   choices = c("Eigenvalue Test" = "eigen", "Trace Test" = "trace"),
                   selected = "eigen"),
      radioButtons("deterministic_trend", "Choose Deterministic Trend Component (ecdet):",
                   choices = c("None" = "none", "Constant" = "const", "Trend" = "trend"),
                   selected = "const"),
      actionButton("analyze_vecm", "Analyze VECM")
    )
  })
  
  
  # VECM Analysis 
  vecm_analysis_results <- eventReactive(input$analyze_vecm, {
    req(data(), input$endogenous_variable, input$rank, input$lag, input$coint_test_type, input$deterministic_trend)
    
    # Extract the endogenous and exogenous variables
    Y <- data()[, input$endogenous_variable, drop = FALSE]

    
    # Get user inputs for the test type, deterministic trend, and lag order
    test_type <- input$coint_test_type
    ecdet <- input$deterministic_trend
    K <- as.numeric(input$lag)
    rank <- as.numeric(input$rank)
    # Perform Johansen cointegration test
    coint_test <- ca.jo(Y, type = test_type, ecdet = ecdet, K = K+1)
    
    # Determine the rank (number of cointegrating relations) based on the test type
    coint_test_summary <- summary(coint_test)
    
    
    vecm_model <- VECM(Y, r = rank, lag =  K, include = ecdet, estim = "ML")
    
    list(
      Cointegration_Test = coint_test_summary,
      VECM_Result = summary(vecm_model)
    )
  })
  
  # Panel Regression
  output$panel_regression_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("dependent_variable_panel", "Select Dependent Variable (Y)", names(data())),
      selectInput("independent_variables_panel", "Select Independent Variables (X)", names(data()), multiple = TRUE),
      selectInput("panel_id", "Select Panel ID Variable", names(data())),
      selectInput("panel_time", "Select Time Variable", names(data())),
      radioButtons("panel_model_type", "Choose Estimator Type:",
                   choices = c("Fixed Effects (Within)" = "within",
                               "Random Effects" = "random",
                               "Pooled OLS" = "pooling",
                               "First Difference (FD)" = "fd"),
                   selected = "within"),
      radioButtons("panel_effect_type", "Choose Effect Type:",
                   choices = c("Individual" = "individual",
                               "Time" = "time",
                               "Two-Way" = "twoways"),
                   selected = "individual"),
      checkboxInput("perform_hausman_test", "Perform Hausman Test (FE vs RE)", value = FALSE),
      actionButton("analyze_panel", "Analyze Panel Regression")
    )
  })
  
  
  # Panel Regression Analysis
  panel_analysis_results <- eventReactive(input$analyze_panel, {
    req(data(), input$dependent_variable_panel, input$independent_variables_panel,
        input$panel_id, input$panel_time, input$panel_model_type, input$panel_effect_type)
    
    # Define the formula for the panel regression
    dependent_variable <- input$dependent_variable_panel
    independent_variables <- input$independent_variables_panel
    formula_panel <- as.formula(paste(dependent_variable, "~", paste(independent_variables, collapse = "+")))
    
    # Prepare the data for panel regression
    panel_data <- pdata.frame(data(), index = c(input$panel_id, input$panel_time))
    
    # Fit the panel regression model based on the selected estimator and effect type
    model_type <- input$panel_model_type
    effect_type <- input$panel_effect_type
    
    if (model_type == "within") {
      panel_model <- plm(formula_panel, data = panel_data, model = "within", effect = effect_type)
    } else if (model_type == "random") {
      panel_model <- plm(formula_panel, data = panel_data, model = "random", effect = effect_type)
    } else if (model_type == "pooling") {
      panel_model <- plm(formula_panel, data = panel_data, model = "pooling")
    } else if (model_type == "fd") {
      panel_model <- plm(formula_panel, data = panel_data, model = "fd")
    }
    
    # Perform Hausman test if selected
    hausman_test_result <- NULL
    if (input$perform_hausman_test) {
      fixed_model <- plm(formula_panel, data = panel_data, model = "within", effect = effect_type)
      random_model <- plm(formula_panel, data = panel_data, model = "random", effect = effect_type)
      hausman_test_result <- phtest(fixed_model, random_model)
    }
    
    # Return the model summary and Hausman test result
    list(
      formula =formula_panel,
      model_summary = summary(panel_model),
      hausman_test = hausman_test_result
    )
    
  })
  
  # Multinomial Logit Model UI
  output$multinomial_choice_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("multinomial_dependent_variable", "Select Dependent Variable (Y)", names(data())),
      selectInput("multinomial_independent_variables", "Select Independent Variables (X)", names(data()), multiple = TRUE),
      checkboxInput("header", "Header", TRUE),
      checkboxInput("calculate_marginal_effects", "Calculate Marginal Effects", value = FALSE),
      actionButton("analyze_multinomial", "Analyze Multinomial Logit Model")
    )
  })
  
  # Multinomial Logit Model Analysis with Stargazer
  multinomial_analysis_results <- eventReactive(input$analyze_multinomial, {
    req(data(), input$multinomial_dependent_variable, input$multinomial_independent_variables)
    
    # Extract user inputs
    dependent_variable <- input$multinomial_dependent_variable
    independent_variables <- input$multinomial_independent_variables
    formula_multinomial <- as.formula(paste(dependent_variable, "~", paste(independent_variables, collapse = "+")))
    
    # Fit the Multinomial Logit Model
    multinomial_model <- multinom(formula_multinomial, data = data())
    
    
    # Optionally calculate marginal effects
    marginal_effects <- NULL
    if (input$calculate_marginal_effects) {
      margins_result<- avg_slopes(multinomial_model, type = "probs")
      marginal_effects=margins_result[,c(1:6)]
    }
    
    
    
    list(
      multinomial_logit_model_result=summary(multinomial_model),
      marginal_effects_table =  marginal_effects,
      empirical_model= formula_multinomial
    )
  })
  
  
  # Ordered Model UI
  output$ordinal_choice_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("ordered_dependent_variable", "Select Dependent Variable (Y)", names(data())),
      selectInput("ordered_independent_variables", "Select Independent Variables (X)", names(data()), multiple = TRUE),
      selectInput("model_type", "Select Model Type", choices = c("Logit", "Probit"), selected = "Logit"),
      checkboxInput("header", "Header", TRUE),
      checkboxInput("calculate_marginal_effects", "Calculate Marginal Effects", value = FALSE),
      actionButton("analyze_ordered_model", "Analyze Ordered Model")
    )
  })
  
  
  
  # Ordered Logit/Probit Model Analysis with Marginal Effects
  ordered_analysis_results <- eventReactive(input$analyze_ordered_model, {
    req(data(), input$ordered_dependent_variable, input$ordered_independent_variables, input$model_type)
    
    # Extract user inputs
    dependent_variable <- input$ordered_dependent_variable
    independent_variables <- input$ordered_independent_variables
    model_type <- input$model_type
    
    # Ensure data() is evaluated and stored in a local variable
    df <- data()  # Store reactive data in a variable
    
    # Convert the dependent variable to an ordered factor
    df$choice_order <- factor(df[[dependent_variable]], ordered = TRUE)
    
    # Construct the formula
    formula_string <- paste("choice_order", "~", paste(independent_variables, collapse = "+"))
    formula_ordered_model <- as.formula(formula_string)  # Convert string to formula
    
    # Ensure formula is created before proceeding
    if (is.null(formula_ordered_model)) {
      stop("Error: formula_ordered_model is NULL")
    }
    
    # Fit the Ordered Logit or Probit Model
    ordered_model <- NULL
    tryCatch({
      if (model_type == "Logit") {
        ordered_model <- oglmx(formula_ordered_model,data = df,link= "logit",constantMEAN=FALSE,constantSD = FALSE,delta=0,threshparam = NULL)
      } else if (model_type == "Probit") {
        ordered_model <- oglmx(formula_ordered_model, data=df,link="probit",constantMEAN=FALSE,constantSD=FALSE,delta=0,threshparam = NULL)
      }
    }, error = function(e) {
      error_message(as.character(e))
    })
    
    if (is.null(ordered_model)) {
      return(NULL)
    }
    
    
    # Optionally calculate marginal effects using the marginaleffects package
  marginal_effects <- NULL
    if (input$calculate_marginal_effects) {
        margins_result <- margins.oglmx(ordered_model)
        marginal_effects <- margins_result
    } 
    
    # Return results
    list(
      ordered_model_result = summary(ordered_model),
      marginal_effects_table = marginal_effects,
      empirical_model = formula_ordered_model
    )
  })
  
  # GARCH Model UI
  output$garch_ui <- renderUI({
    req(input$file)
    tagList(
      selectInput("variable", "Select Variable (Y)", names(data())),
      selectInput("garch_type", "Select GARCH Type", choices = c("ARCH", "GARCH", "IGARCH", "TGARCH", "AGARCH", "GJRGARCH", "EGARCH", "GARCH-M"), selected = "GARCH"),
      numericInput("arma_order_p", "Select AR Order (p)", value = 0, min = 0, max = 5),
      numericInput("arma_order_q", "Select MA Order (q)", value = 0, min = 0, max = 5),
      selectInput("distribution_model", "Select Distribution Model", choices = c("norm", "std", "ged", "snorm"), selected = "norm"),
      dateInput("start_date", "Select Start Date", value = Sys.Date(), format = "yyyy-mm-dd"),
      numericInput("frequency", "Select Frequency", value = 1, min = 1, max = 365),
      actionButton("fit_garch", "Fit GARCH Model")
    )
  })
  
  # GARCH Model Analysis with User Selection
  garch_analysis_results <- eventReactive(input$fit_garch, {
    req(data(), input$variable, input$garch_type, input$arma_order_p, input$arma_order_q, input$distribution_model, input$start_date, input$frequency)
    
    # Extract user inputs
    garch_type <- input$garch_type
    arma_order_p <- input$arma_order_p
    arma_order_q <- input$arma_order_q
    distribution_model <- input$distribution_model
    start_date <- as.Date(input$start_date)
    frequency <- as.numeric(input$frequency)
    
    # Load the necessary package
    library(rugarch)
    
    # Read the dataset (get selected column)
    data <- data()
    returns <- data[[input$variable]]
    
    # Step 2: Select GARCH Type
    garch_spec <- NULL
    if (garch_type == "ARCH") {
      garch_spec <- ugarchspec(
        mean.model = list(armaOrder = c(0, 0)),
        variance.model = list(model = "fGARCH", garchOrder = c(1, 0), submodel = "GARCH"),
        distribution.model = distribution_model
      )
    } else if (garch_type == "GARCH") {
      garch_spec <- ugarchspec(
        mean.model = list(armaOrder = c(arma_order_p, arma_order_q)),
        variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "GARCH"),
        distribution.model = distribution_model
      )
    } else if (garch_type == "IGARCH") {
      garch_spec <- ugarchspec(
        mean.model = list(armaOrder = c(arma_order_p, arma_order_q)),
        variance.model = list(model = "iGARCH", garchOrder = c(1, 1)),
        distribution.model = distribution_model
      )
    } else if (garch_type == "TGARCH") {
      garch_spec <- ugarchspec(
        mean.model = list(armaOrder = c(arma_order_p, arma_order_q)),
        variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "TGARCH"),
        distribution.model = distribution_model
      )
    } else if (garch_type == "AGARCH") {
      garch_spec <- ugarchspec(
        mean.model = list(armaOrder = c(arma_order_p, arma_order_q)),
        variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "AVGARCH"),
        distribution.model = distribution_model
      )
    } else if (garch_type == "GJRGARCH") {
      garch_spec <- ugarchspec(
        mean.model = list(armaOrder = c(arma_order_p, arma_order_q)),
        variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "GJRGARCH"),
        distribution.model = distribution_model
      )
    } else if (garch_type == "EGARCH") {
      garch_spec <- ugarchspec(
        mean.model = list(armaOrder = c(arma_order_p, arma_order_q)),
        variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
        distribution.model = distribution_model
      )
    } else if (garch_type == "GARCH-M") {
      garch_spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(arma_order_p, arma_order_q), include.mean = TRUE, archm = TRUE, archpow = 2),
        distribution.model = distribution_model
      )
    }
    
    # Fit the GARCH model
    garch_fit <- ugarchfit(spec = garch_spec, data = returns)
    
    # Create a time series for volatility (`hhat`) using start date and frequency
    hhat <- ts(garch_fit@fit$sigma^2, start = as.numeric(format(start_date, "%Y")), frequency = frequency)
    
    # Return only the summary while keeping hhat available for plotting or internal use
    list(
      hhat = hhat, # Internal use only
      garch_summary = garch_fit # Return summary for display
    )
  })
  
  
  
  # Model sugesstion
  output$suggestion_ui <- renderUI({
    tagList(
      textInput("question1", "What is your topic?", value = ""),
      selectInput("question2", "What type of your data?", choices = c("Time Series data", "Panel data", "Cross Section data")),
      selectInput("question3", "What is your research focus?", choices = c("Estimating causal relationships", "Correlations", "Only one-way effect", "Prediction/forecasting")),
      textInput("question4", "How large is your dataset?", value = ""),
      textInput("question5", "Do you have any important information to tell?", value = ""),
      actionButton("suggestion", "Model Suggestion")
    )
  })
  
  # Suggestion analysis results
  suggestion_analysis_results <- eventReactive(input$suggestion, {
    req(input$question1, input$question2, input$question3, input$question4, input$question5)
    
    # Construct a message summarizing the user's study details
    user_study_info <- paste(
      "This is the information of my study:\n",
      "Topic: ", input$question1, "\n",
      "Type of Data: ", input$question2, "\n",
      "Research Focus: ", input$question3, "\n",
      "Dataset Size: ", input$question4, "\n",
      "More information: ", input$question5, "\n"
    )
    
    # Add details about available models
    available_models <- paste(
      "This application provides the following models for analysis:\n",
      "- Time series unit root test\n",
      "- Panel unit root test\n",
      "- Linear Regression\n",
      "- Binary Regression\n",
      "- Error Correction Model\n",
      "- GARCH Model\n",
      "- VAR Model\n",
      "- VECM Model\n",
      "- Panel Regression Model\n",
      "- Multinomial Choice Model\n",
      "- Ordinal Choice Model\n"
    )
    
    # Ask ChatGPT for model suggestions based on the collected study details
    suggestion <- ask_chatgpt(paste(
      user_study_info, available_models,
      "Based on the information above, could you suggest appropriate econometric or statistical models to analyze the data?",
      "Please provide a brief rationale for your suggestion.",
      "If the suggested analysis requires a model not available in this list, kindly advise the user to explore other models.",
      "Please limit your answer to only 200 words."
    ))
    
    suggestion
  })
  
  # Render suggestion output
  output$suggestion_output <- renderPrint({
    req(suggestion_analysis_results())
    suggestion_text <- suggestion_analysis_results()
    wrapped_text <- strwrap(suggestion_text, width = 80)  # Adjust 'width' as needed for your box
    formatted_text <- paste(wrapped_text, collapse = "\n")
    
    cat(formatted_text)
  })
  #  =========== Print Interpret and theory 
  
  output$summary_result <- renderPrint({
    req(input$menu) # Ensure that the input is not null
    if (input$menu == "linear_regression") {
      linear_analysis_results()
    } else if (input$menu  == "binary_regression") {
      binary_analysis_results()
    } else if (input$menu  == "arima") {
      arima_analysis_results()
    } else if (input$menu  == "ecm_model") {
      ecm_analysis_results()
    } else if (input$menu  == "var_model") {
      var_analysis_results()
    } else if (input$menu == "panel_regression_model") {
      panel_analysis_results()
    } else if (input$menu == "vecm_model") {
      vecm_analysis_results()
    } else if (input$menu == "unitroot_ts") {
      adf_test_results()
    } else if (input$menu == "unitroot_panel") {
      panel_unitroot_test_results()
    } else if (input$menu == "multinomial_choice_model") {
      multinomial_analysis_results()
    } else if (input$menu == "ordinal_choice_model") {
      ordered_analysis_results()
    } else if (input$menu == "garch_model") {
      req(garch_analysis_results())
      print(garch_analysis_results()$garch_summary)
    } else if (input$menu == "t_test") {
      t_test_results()
    } else if (input$menu == "anova") {
      anova_results()
    } else if (input$menu == "chi_square") {
      chi_square_results()
    } else if (input$menu == "f_test") {
      f_test_results()
    }
  })
  output$explain_theory <- renderPrint({
    req(input$menu)
    if (input$menu == "linear_regression") {
      theory <- Explain_theory(linear_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "binary_regression") {
      theory <- Explain_theory(binary_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "arima") {
      theory <- Explain_theory(arima_analysis_results()$arima_result)
      withMathJax(theory)
    } else if (input$menu == "ecm_model") {
      theory <- Explain_theory(ecm_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "var_model") {
      theory <- Explain_theory(var_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "vecm_model") {
      theory <- Explain_theory(vecm_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "panel_regression_model") {
      theory <- Explain_theory(panel_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "unitroot_ts") {
      theory <- Explain_theory(adf_test_results())
      withMathJax(theory)
    } else if (input$menu == "unitroot_panel") {
      theory <- Explain_theory(panel_unitroot_test_results())
      withMathJax(theory)
    } else if (input$menu == "multinomial_choice_model") {
      theory <- Explain_theory(multinomial_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "ordinal_choice_model") {
      theory <- Explain_theory(ordered_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "garch_model") {
      req(garch_analysis_results())
      theory <- Explain_theory(garch_analysis_results()$garch_summary)
      withMathJax(theory)
    } else if (input$menu == "t_test") {
      theory <- Explain_stat(t_test_results())
      withMathJax(theory)
    } else if (input$menu == "anova") {
      theory <- Explain_stat(anova_results())
      withMathJax(theory)
    } else if (input$menu == "chi_square") {
      theory <- Explain_stat(chi_square_results())
      withMathJax(theory)
    } else if (input$menu == "f_test") {
      theory <- Explain_stat(f_test_results())
      withMathJax(theory)
    }
  })
  
  
  output$interpret_result <- renderPrint({
    req(input$menu)
    if (input$menu == "linear_regression") {
      interpretation <- interpret_result(linear_analysis_results())
      withMathJax(interpretation)
    } else if (input$menu  == "binary_regression") {
      interpretation <- interpret_result(binary_analysis_results())
      withMathJax(interpretation)
    } else if (input$menu == "arima") {
      theory <- interpret_result(arima_analysis_results()$arima_result)
      withMathJax(theory)
    } else if (input$menu == "ecm_model") {
      interpretation <- interpret_result(ecm_analysis_results())
      withMathJax(interpretation)
    } else if (input$menu == "var_model") {
      interpretation <- interpret_result(var_analysis_results())
      withMathJax(interpretation)
    } else if (input$menu == "panel_regression_model") {
      theory <- interpret_result(panel_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "vecm_model") {
      theory <- interpret_result(vecm_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "unitroot_ts") {
      theory <- interpret_result( adf_test_results())
      withMathJax(theory)
    } else if (input$menu == "unitroot_panel") {
      theory <- interpret_result(panel_unitroot_test_results())
      withMathJax(theory)
    } else if (input$menu == "multinomial_choice_model") {
      theory <- interpret_result(multinomial_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "ordinal_choice_model") {
      theory <- interpret_result(ordered_analysis_results())
      withMathJax(theory)
    } else if (input$menu == "garch_model") {
      req(garch_analysis_results())
      theory <- interpret_result(garch_analysis_results()$garch_summary)
      withMathJax(theory)
    } else if (input$menu == "t_test") {
      theory <- interpret_result_stat(t_test_results())
      withMathJax(HTML(theory))
    } else if (input$menu == "anova") {
      theory <- interpret_result_stat(anova_results())
      withMathJax(HTML(theory))
    } else if (input$menu == "chi_square") {
      theory <- interpret_result_stat(chi_square_results())
      withMathJax(HTML(theory))
    } else if (input$menu == "f_test") {
      theory <- interpret_result_stat(f_test_results())
      withMathJax(HTML(theory))
      
    }
  })
  
  
  ##========================================================================================
  # Dynamically create selectInput based on the columns in the uploaded CSV file
  output$select_var <- renderUI({
    req(data())
    selectInput("var", "Select Variable for Plotting", choices = names(data()))
  })
  
  output$select_var2 <- renderUI({
    req(input$plot_type == "scatter", data())
    selectInput("var2", "Select Second Variable (for Scatter Plot)", choices = names(data()))
  })
  
  
  output$select_color <- renderUI({
    req(data())
    color_names <- colors()
    selectInput("color", "Choose a Color:", choices = color_names)
  })
  
  output$plot_title <- renderUI({
    req(data())
    textInput(inputId = "title", label = "Plot title:", 
              value = "Write your title here")})
  
  output$x_axis_title <- renderUI({
    req(data())
    textInput(inputId = "xlab", label = "xlab name:", 
              value = "Write your x-axis name here")})
  
  output$y_axis_title <- renderUI({
    req(data())
    textInput(inputId = "ylab", label = "ylab name:", 
              value = "Write your y-axis name here")})  
  
  output$title_font <- renderUI({
    req(data())
    sliderInput(inputId = "size_font", label = "Title font size",
                min = 10, max = 20, value = 11)})  
  
  
  
  output$plot_output <- renderPlotly({
    req(input$menu)
    
    if (input$menu == "garch_model") {
      # Use the stored `hhat` from `garch_analysis_results` to plot the GARCH volatility
      req(garch_analysis_results())
      hhat <- garch_analysis_results()$hhat
      start_date <- input$start_date
      frequency <- input$frequency
      
      # Generate plot with additional information about start date and frequency
      P <- ggplot() +
        geom_line(aes(x = time(hhat), y = hhat), color = "blue") +
        labs(
          title = "Volatility from GARCH Model",
          subtitle = paste("Start Date:", start_date, "| Frequency:", frequency),
          x = "Time",
          y = "Volatility"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12, face = "italic")
        )
      
      # Convert to an interactive plotly plot
      ggplotly(P) %>%
        layout(dragmode = "zoom")
      
    } else if (input$menu == "arima") {
      req(data(), input$arima_dependent_variable, input$arima_order_p, input$arima_order_d, input$arima_order_q, input$start_date, input$frequency, input$n_forecast)
      
      # Extract the time series data based on the input variable
      Y <- data()[[input$arima_dependent_variable]]
      
      # Load the necessary library
      library(forecast)
      
      # Convert data to time series object
      start_date <- as.Date(input$start_date)
      frequency <- input$frequency
      
      # Assuming frequency is yearly, monthly, etc. you need to correctly set the ts object.
      Y_ts <- ts(Y, start = c(as.numeric(format(start_date, "%Y")), as.numeric(format(start_date, "%m"))), frequency = frequency)
      
      
      # Fit the ARIMA model based on user input
      arima_order <- c(input$arima_order_p, input$arima_order_d, input$arima_order_q)
      arima_fit <- Arima(Y_ts, order = arima_order)
      
      # Perform forecasting for the specified number of steps ahead
      arima_forecast <- forecast(arima_fit, h = input$n_forecast)
      
      # Convert the time series data to a format suitable for plotting
      original_df <- data.frame(
        time = time(Y_ts),
        value = as.numeric(Y_ts)
      )
      
      # For the forecasted data, generate a sequence of time points that extends the original data
      forecast_df <- data.frame(
        time = time(arima_forecast$mean),
        value = as.numeric(arima_forecast$mean)
      )
      # Add a column to distinguish between Original and Forecasted data
      original_df$source <- "Original Data"
      forecast_df$source <- "Forecast"
      original_df$time <- as.numeric(original_df$time)
      forecast_df$time <- as.numeric(forecast_df$time)
      combined_data <- rbind(original_df, forecast_df)
      combined_data$source <- as.factor(combined_data$source)
      
      
      # Plot both the original data and forecast values with confidence intervals
      P <- ggplot() +
        geom_line(data = original_df, aes(x = time, y = value, color = "Original Data"), size = 1) +
        geom_line(data = forecast_df, aes(x = time, y = value, color = "Forecast"), size = 1, linetype = "dashed") +
        geom_ribbon(
          data = data.frame(
            time = time(arima_forecast$mean),
            ymin = arima_forecast$lower[, 2], # 95% lower bound
            ymax = arima_forecast$upper[, 2]  # 95% upper bound
          ),
          aes(x = time, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.2
        ) +
        labs(
          title = paste("ARIMA Forecast for", input$arima_dependent_variable),
          subtitle = paste("Start Date:", input$start_date, "| Frequency:", input$frequency),
          x = "Time",
          y = "Values",
          color = "Legend"
        ) +
        scale_color_manual(values = c("Original Data" = "blue", "Forecast" = "red")) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 5, face = "bold"),
          plot.subtitle = element_text(size = 5, face = "italic"),
          axis.title = element_text(size = 5),
          axis.text = element_text(size = 5),
          legend.title = element_text(size = 5),
          legend.text = element_text(size = 5)
        )
      
      # Convert to an interactive plotly plot

      ggplotly(P, tooltip = c("x", "y", "color")) %>%
        layout(
          dragmode = "zoom", # Enables zooming
          margin = list(t = 50, b = 100, l = 70, r = 30), # Adjust margins
          legend = list(orientation = "h", x = 0.3, y = -0.2) # Horizontal legend
        )
      
      
    } else if (input$menu == "plot") {
      # For other plots (histogram, boxplot, scatterplot)
      req(input$var, data())
      
      plot_data <- data()[[input$var]]
      
      if (input$plot_type == "hist") {
        Data <- data.frame(plot_data)
        P <- Data %>%
          ggplot() +
          aes(x = plot_data) +
          geom_histogram(fill = input$color, color = "black") +
          ggtitle(if_else(input$title == "Write your title here",
                          paste("The histogram of", input$var), input$title)) +
          xlab(if_else(input$xlab == "Write your x-axis name here",
                       input$var, input$xlab)) +
          ylab(if_else(input$ylab == "Write your y-axis name here",
                       "count", input$ylab)) +
          theme(axis.text.x = element_text(size = input$size_font),
                axis.text.y = element_text(size = input$size_font),
                axis.title.x = element_text(size = input$size_font),
                axis.title.y = element_text(size = input$size_font),
                text = element_text(size = input$size_font))
        ggplotly(P) %>%
          layout(dragmode = "zoom")
        
      } else if (input$plot_type == "box") {
        Data <- data.frame(plot_data)
        P <- Data %>%
          ggplot() +
          aes(x = "", y = plot_data) +
          geom_boxplot(fill = input$color, color = "black") +
          ggtitle(if_else(input$title == "Write your title here",
                          paste("The boxplot of", input$var), input$title)) +
          xlab(if_else(input$xlab == "Write your x-axis name here",
                       input$var, input$xlab)) +
          ylab(if_else(input$ylab == "Write your y-axis name here",
                       "value", input$ylab)) +
          theme(axis.text.x = element_text(size = input$size_font),
                axis.text.y = element_text(size = input$size_font),
                axis.title.x = element_text(size = input$size_font),
                axis.title.y = element_text(size = input$size_font),
                text = element_text(size = input$size_font))
        ggplotly(P) %>%
          layout(dragmode = "zoom")
        
      } else if (input$plot_type == "scatter") {
        req(input$var2)
        Data <- data.frame(x = data()[[input$var]], y = data()[[input$var2]])
        P <- Data %>%
          ggplot() +
          aes(x = x, y = y) +
          geom_point(color = input$color) +
          ggtitle(if_else(input$title == "Write your title here",
                          paste("The scatter plot of", input$var, "and", input$var2), input$title)) +
          xlab(if_else(input$xlab == "Write your x-axis name here",
                       input$var, input$xlab)) +
          ylab(if_else(input$ylab == "Write your y-axis name here",
                       input$var2, input$ylab)) +
          theme(axis.text.x = element_text(size = input$size_font),
                axis.text.y = element_text(size = input$size_font),
                axis.title.x = element_text(size = input$size_font),
                axis.title.y = element_text(size = input$size_font),
                text = element_text(size = input$size_font))
        ggplotly(P) %>%
          layout(dragmode = "zoom")
      }
    }
  })
  
  # Observe the "Expand Plot" button click to show the modal
  output$expanded_plot <- renderPlotly({
    req(input$menu)
    
    if (input$menu == "garch_model") {
      # Use the stored `hhat` from `garch_analysis_results` to plot the GARCH volatility in expanded view
      req(garch_analysis_results())
      hhat <- garch_analysis_results()$hhat
      start_date <- input$start_date
      frequency <- input$frequency
      
      # Generate plot with additional information about start date and frequency
      P <- ggplot() +
        geom_line(aes(x = time(hhat), y = hhat), color = "blue") +
        labs(
          title = "Expanded Volatility from GARCH Model",
          subtitle = paste("Start Date:", start_date, "| Frequency:", frequency),
          x = "Time",
          y = "Volatility"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 16, face = "italic")
        )
      
      # Convert to an interactive plotly plot
      ggplotly(P) %>%
        layout(dragmode = "zoom")
      
    } else if (input$menu == "arima") {

      Y <- data()[[input$arima_dependent_variable]]
      

      start_date <- as.Date(input$start_date)
      frequency <- input$frequency

      # Assuming frequency is yearly, monthly, etc. you need to correctly set the ts object.
      Y_ts <- ts(Y, start = c(as.numeric(format(start_date, "%Y")), as.numeric(format(start_date, "%m"))), frequency = frequency)
      
      # Fit ARIMA model using the time series object
      arima_fit <- auto.arima(Y_ts)
      
      # Forecast for 'n' steps ahead
      n_forecast <- input$n_forecast
      forecast_result <- forecast(arima_fit, h = n_forecast)
      P <- autoplot(forecast_result) +
        labs(
          title = "ARIMA Model Forecast",
          subtitle = paste("Forecasting", n_forecast, "Steps Ahead"),
          x = "Time",
          y = input$arima_dependent_variable
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12, face = "italic")
        )
      
      # Convert to an interactive plotly plot
      ggplotly(P) %>%
        layout(dragmode = "zoom")
      
      
    } else if (input$menu == "plot") {
      # For other expanded plots (histogram, boxplot, scatterplot)
      req(input$var, data())
      
      plot_data <- data()[[input$var]]
      
      if (input$plot_type == "hist") {
        Data <- data.frame(plot_data)
        P <- Data %>%
          ggplot() +
          aes(x = plot_data) +
          geom_histogram(fill = input$color, color = "black") +
          ggtitle(if_else(input$title == "Write your title here",
                          paste("The histogram of", input$var), input$title)) +
          xlab(if_else(input$xlab == "Write your x-axis name here",
                       input$var, input$xlab)) +
          ylab(if_else(input$ylab == "Write your y-axis name here",
                       "count", input$ylab)) +
          theme(axis.text.x = element_text(size = input$size_font + 4),
                axis.text.y = element_text(size = input$size_font + 4),
                axis.title.x = element_text(size = input$size_font + 4),
                axis.title.y = element_text(size = input$size_font + 4),
                text = element_text(size = input$size_font + 4))
        ggplotly(P) %>%
          layout(dragmode = "zoom")
        
      } else if (input$plot_type == "box") {
        Data <- data.frame(plot_data)
        P <- Data %>%
          ggplot() +
          aes(x = "", y = plot_data) +
          geom_boxplot(fill = input$color, color = "black") +
          ggtitle(if_else(input$title == "Write your title here",
                          paste("The boxplot of", input$var), input$title)) +
          xlab(if_else(input$xlab == "Write your x-axis name here",
                       input$var, input$xlab)) +
          ylab(if_else(input$ylab == "Write your y-axis name here",
                       "value", input$ylab)) +
          theme(axis.text.x = element_text(size = input$size_font + 4),
                axis.text.y = element_text(size = input$size_font + 4),
                axis.title.x = element_text(size = input$size_font + 4),
                axis.title.y = element_text(size = input$size_font + 4),
                text = element_text(size = input$size_font + 4))
        ggplotly(P) %>%
          layout(dragmode = "zoom")
        
      } else if (input$plot_type == "scatter") {
        req(input$var2)
        Data <- data.frame(x = data()[[input$var]], y = data()[[input$var2]])
        P <- Data %>%
          ggplot() +
          aes(x = x, y = y) +
          geom_point(color = input$color) +
          ggtitle(if_else(input$title == "Write your title here",
                          paste("The scatter plot of", input$var, "and", input$var2), input$title)) +
          xlab(if_else(input$xlab == "Write your x-axis name here",
                       input$var, input$xlab)) +
          ylab(if_else(input$ylab == "Write your y-axis name here",
                       input$var2, input$ylab)) +
          theme(axis.text.x = element_text(size = input$size_font + 4),
                axis.text.y = element_text(size = input$size_font + 4),
                axis.title.x = element_text(size = input$size_font + 4),
                axis.title.y = element_text(size = input$size_font + 4),
                text = element_text(size = input$size_font + 4))
        ggplotly(P) %>%
          layout(dragmode = "zoom")
      }
    }
  })
  
  # Observe the "Expand Plot" button click to show the model
  observeEvent(input$expand_plot, {
    showModal(modalDialog(
      title = "Expanded Plot",
      size = "l", # "l" for large modal size
      plotlyOutput("expanded_plot"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  output$table1 <-renderTable({
    D1 <-  stat.desc(data())
    name <- row.names(D1)
    name[1] <- "N"
    name[3] <- "Missing Value"
    name[13] <- "SD"
    
    # skewness
    skew <- apply(data(),2,skewness)
    # kurtosis
    kurt <- apply(data(),2,kurtosis)
    
    # JB.test
    JB_test <- apply(data(),2,jarque.bera.test)
    
    X_squared <- rep(0,dim(data())[2])
    df        <- X_squared
    p_value <-  X_squared
    JB <- X_squared
    
    for(i in 1:dim(data())[2]){
      X_squared[i] <- JB_test[[i]]$statistic
      df[i] <- JB_test[[i]]$parameter
      p_value[i] <- JB_test[[i]]$p.value
    }
    
    D1 <- rbind(D1, skew,kurt,X_squared,df,p_value)
    
    D1$stat <- c(name,"Skewness", "Kurtosis","JB test: X square",
                 "JB test: df","JB test: p value")
    
    D1 <-   D1[, c(dim(D1)[2],1:(dim(D1)[2]-1))]
    
    D1 <- D1[c(1,3,9,8,7,13,12,4,5,15:19), ]
    stat <- as.integer(input$checkStat)
    
    if ( 12 %in%  stat){D1[c(stat,13,14), ]}else{D1[stat, ]}
  })     
  
}
