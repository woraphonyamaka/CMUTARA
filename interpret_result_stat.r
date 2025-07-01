interpret_result_stat <- function(estimation_result) {
  
  # Convert estimation result to text
  result_text <- capture.output(print(estimation_result))
  result_text <- paste(result_text, collapse = "\n")
  
  # Send to ChatGPT for interpretation
  interpretation <- ask_chatgpt(paste(
    result_text,
    "Can you interpret this result?",
    "Please provide a Four-Step Hypothesis Testing Procedure:",
    "- Write the hypotheses.",
    "- Compute the test statistic.",
    "- Make a decision.",
    "- State a real-world conclusion.",
    "Ensure that mathematical fonts and notations within the text and equation use LaTeX format.",
    "Ensure each step is well separated line by line."
  ))
