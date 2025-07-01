Explain_stat <- function(estimation_result) {
  
  # Capture the estimation result as text
  result_text1 <- capture.output(estimation_result)
  result_text1 <- paste(result_text1, collapse = "\n")
  
  # Get the explanation from ChatGPT
  explanation <- ask_chatgpt(paste(
    result_text1,
    "I don't want you to interpret the output above.",
    "Instead, I want to understand the statistical equations without mentioning the variables.",
    "Please explain how to estimate this statistic as if you are a statistics teacher.",
    "Include all relevant equations and notations using LaTeX format.",
    "Ensure that mathematical fonts and notations within the text use LaTeX format.",
    "Start your answer with: 'The statistics of this model can be presented as follows:'",
    "Please separate each step with a line break.",
    "Keep the response concise and under 400 words."
  ))
  
  # Wrap the text with a specific width
  wrapped_text <- strwrap(explanation, width = 50)
  explain_latex <- paste(wrapped_text, collapse = "\n")
  return(explain_latex)
}
