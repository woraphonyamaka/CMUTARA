# ✅ CMUTARA: Chiang Mai University Teaching and Research Assistant for Econometrics and Statistics

**CMUTARA** is a **web-based application** developed using R Shiny and integrated with ChatGPT. It is designed to support teaching and research in **econometrics and statistics** by offering model selection, estimation, AI-generated interpretation, and theoretical explanations.

> 🟢 **No installation required. Just open your browser and start using the tool.**

---

## 🌐 How to Use CMUTARA

You can use CMUTARA directly from any modern web browser.

### ✅ Steps:
1. Open one of these browsers:
   - **Google Chrome**
   - **Mozilla Firefox**
   - **Microsoft Edge**
   - **Safari**

2. Go to the application:
   👉 [https://woraphon.shinyapps.io/CMUTARA/](https://woraphon.shinyapps.io/CMUTARA/)

3. Upload your `.csv` dataset.

4. Select your dependent and independent variables.

5. Explore the features:
   - 📊 Model suggestions based on data characteristics
   - 🧮 Regression estimation (OLS, Logit, Probit, Poisson, etc.)
   - 💬 AI-generated interpretation of model results
   - 📚 Theoretical and statistical explanations
   - 📁 Exportable reports in various formats

---

## 📦 Source Code

The complete source code is available at:  
🔗 [https://github.com/woraphonyamaka/CMUTARA](https://github.com/woraphonyamaka/CMUTARA)

---

## 🛠️ For Developers: Optional Local Installation

This section is for developers who want to run CMUTARA locally for customization or offline testing.

### ✅ Supported Operating Systems:
- Linux ✅
- Windows ✅
- macOS ✅

### 🔧 Prerequisites:
- R (version ≥ 4.2.0)
- RStudio (optional)
- Internet access for OpenAI API features

### 📥 Installation Steps

**1) Clone the repository**
```bash
git clone https://github.com/woraphonyamaka/CMUTARA.git
cd CMUTARA
```
**2) Install required R packages:**
```r
install.packages(c(
  "shiny","shinydashboard","shinyWidgets","shinyjs","bslib",
  "plotly","ggplot2","readr","readxl","dplyr","stringr","httr",
  "pastecs","e1071","tseries","aTSA","plm","urca","tsDyn",
  "vars","rugarch","dynlm","MASS","nnet","mlogit","oglmx",
  "stargazer","marginaleffects","mfx","lmtest","mctest","forecast",
  "mathjaxr","chatgpt"
))
```

**3) Set your OpenAI API key (required for ChatGPT features)**

You can get your API key by signing in to your OpenAI account here:
https://platform.openai.com/account/api-keys
```r
Sys.setenv(OPENAI_API_KEY = "YOUR_OPENAI_API_KEY")

# (Optional) Save the key for future sessions
# write("OPENAI_API_KEY=YOUR_OPENAI_API_KEY", file = file.path(Sys.getenv("HOME"), ".Renviron"), append = TRUE)
```
**4) Run the Shiny application:**
```r
shiny::runApp("CMUTARA8 remove key.R")
```
