
# CMU TARA

**CMU TARA** (Teaching and Research Assistant) is an advanced AI-powered web-based Shiny application developed by Chiang Mai University. It supports teaching and research in statistics and econometrics by combining powerful statistical tools with GPT-assisted explanations and model suggestions.

🌐 **Live App**: [https://woraphon.shinyapps.io/TARA/](https://woraphon.shinyapps.io/TARA/)  
📁 **Source Code**: [https://github.com/woraphonyamaka/CMUTARA](https://github.com/woraphonyamaka/CMUTARA)
📁 **User guide**: [https://wyamaka.wordpress.com/wp-content/uploads/2025/07/user-guide-cmu-tara.pdf]

---

## ✨ Key Features

- 📊 Descriptive statistics, hypothesis testing (t-test, ANOVA, Chi-square, F-test)
- 📈 Time series and panel econometric models: ARIMA, ECM, VAR, VECM, GARCH, Unit Root Tests
- 🧠 AI-powered interpretation using OpenAI GPT (ChatGPT)
- 🔍 Suggests appropriate models based on user study info
- 📉 Visualizations: histogram, boxplot, scatter plot (with `plotly`)
- 🧪 Multinomial/ordinal/binary regressions with marginal effects
- 📚 Real-time teaching support via explainable output and statistical theory prompts

---

## 📦 Installation

1. **Clone this repository**
```bash
git clone https://github.com/woraphonyamaka/CMUTARA.git
cd CMUTARA
```

2. **Install dependencies in R**
```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs", "httr", "stringr",
  "ggplot2", "plotly", "bslib", "readr", "readxl", "pastecs", "e1071", 
  "tseries", "dplyr", "aTSA", "plm", "urca", "tsDyn", "nnet", "mlogit", 
  "stargazer", "marginaleffects", "MASS", "datasets", "vars", "mfx", 
  "dynlm", "rugarch", "oglmx"
))
```

3. **Set your OpenAI API Key**
```r
Sys.setenv(OPENAI_API_KEY = "your-key-here")
```

4. **Run the app**
```r
shiny::runApp("CMUTARA8 remove key.R")
```

---

## 🧱 File Structure

```
CMUTARA/
├── CMUTARA8 remove key.R   # Main Shiny application file
├── README.md               # This file
```

---

## 🔄 Extendability

CMU TARA is designed to be modular and easy to extend:

### ➕ Add a New Statistical or Econometric Model
1. Add a new `menuSubItem()` and `tabItem()` under `dashboardSidebar` and `dashboardBody`.
2. Define the logic for the model under `renderUI()` and `eventReactive()`.
3. Render the output using `renderPrint()` or `renderPlotly()`.

### 🤖 Customize AI-Generated Explanations
- Modify prompt templates in the functions:  
  - `interpret_result()`  
  - `Explain_stat()`  
  - `Explain_theory()`  
- These use the `ask_chatgpt()` function which sends output to OpenAI’s GPT model for interpretation or explanation.

### 🖼 Add New Visualizations
- Edit the section under `output$plot_output` in `server`.
- Add your own plot type in the `radioButtons("plot_type", ...)`.

---

## 📚 Citation

If you use CMU TARA in your work, please cite the following paper:

> Woraphon Yamaka, Paravee Maneejuk (2025). **CMU TARA: An AI-enhanced Teaching and Research Assistant for Applied Econometrics**. *SoftwareX*. [DOI pending]

---

## 📬 Contact

For questions or contributions:
- 📧 Woraphon Yamaka – [woraphon.econ@gmail.com](mailto:woraphon.econ@gmail.com)
- 📍 Faculty of Economics, Chiang Mai University

---

## 📝 License

This project is licensed under the MIT License.
