Explain_theory <- function(estimation_result) {
  
  # Capture the estimation result as text
  result_text1 <- capture.output(print(estimation_result))
  result_text1 <- paste(result_text1, collapse = "\n")
  
  # Get the explanation from ChatGPT
  
  Explain <- ask_chatgpt(paste(result_text1, "I don't want you to interpret the output above",
                               "Instead, I want to understand the mathematical equations without mentioning the variables",
                               "and econometric theory of this model.", 
                               "Please explain how to estimate this model as if you are an econometrics teacher.",
                               "Include all relevant equations and notations using LaTeX format.",
                               "Ensure that mathematical fonts and notations within the text use LaTeX format.",
                               "Start you answer with :The theory of this model can be presented as follows:",
                               "Keep the response concise and under 400 words."))
  
  # Wrap the text with a specific width
  wrapped_text <- strwrap(Explain, width = 50)
  explain_latex <- paste(wrapped_text, collapse = "\n")
  return(explain_latex)
}
