interpret_result <- function(estimation_result) {
  
  
  # Convert estimation result to text
  result_text <- capture.output(print(estimation_result))
  result_text <- paste(result_text, collapse = "\n")
  
  # Send to ChatGPT
  interpretation <- ask_chatgpt(paste(result_text, "Can you interpret this result? 
                    Please give a mathematical equation and limit the word count to 300.",
                                      "Ensure that mathematical fonts and notations within the text and equation use LaTeX format."))
  
  wrapped_text <- strwrap(interpretation, width = 50)  # Adjust the width as needed
  formatted_interpretation <- paste(wrapped_text, collapse = "\n")
  return(formatted_interpretation)
}
