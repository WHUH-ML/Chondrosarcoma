# Deep learning models for predicting the survival of patients with chondrosarcoma based on a surveillance, epidemiology, and end results analysis

Data preprocessing is handled by R code: [DataPreprocessing.R](DataPreprocessing.R)

Model construction, hyperparameters tuning and evaluation are handled with [pysurvival](https://github.com/square/pysurvival), [scikit-learn](https://github.com/scikit-learn/scikit-learn) and [lifelines](https://github.com/CamDavidsonPilon/lifelines) packages: [ModelDevelopmentWithoutTuningOutput.ipynb](ModelDevelopmentWithoutTuningOutput.ipynb)

Web application based on [streamlit](https://github.com/streamlit/streamlit) package: [app.py](app.py)

The original data read in R code is not provided in this repository and needs to be extracted in the [SEER](https://seer.cancer.gov/) database according to inclusion criteria (AYA site recode 2020 Revision = 4.2 Chondrosarcoma)

To reproduce this study, first run the following codes to install packages:
```
git clone https://github.com/WHUH-ML/Chondrosarcoma.git
pip install -r requirements.txt
```
Then run [ModelDevelopmentWithoutTuningOutput.ipynb](ModelDevelopmentWithoutTuningOutput.ipynb) in Jupyter Notebook.

Run ```streamlit run app.py``` in terminal to open the web application locally.

[Online web application](https://share.streamlit.io/whuh-ml/chondrosarcoma/Predict/app.py)

[Paper link](https://doi.org/10.3389/fonc.2022.967758)
