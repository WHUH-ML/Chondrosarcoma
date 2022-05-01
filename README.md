#### Data preprocessing is handled by R code: [DataPreprocessing.R](DataPreprocessing.R)

#### Model construction, hyperparameters tuning and evaluation: [ModelDevelopmentWithoutTuningOutput.ipynb](ModelDevelopmentWithoutTuningOutput.ipynb)

#### Web applications based on [streamlit](https://github.com/streamlit/streamlit) packages: [app.py](app.py)

#### The original data read in R code is not provided in this repository and needs to be extracted in the [SEER](https://seer.cancer.gov/) database according to inclusion criteria (AYA site recode 2020 Revision = 4.2 Chondrosarcoma)

#### The data after data preprocessing is provided. 

#### To reproduce this study, first run the following codes to install packages:
```
git clone https://github.com/WHUH-ML/Chondrosarcoma.git
pip install -r requirement.txt
```
#### Then run [ModelDevelopmentWithoutTuningOutput.ipynb](ModelDevelopmentWithoutTuningOutput.ipynb) in Jupyter Notebook.

#### Run ```streamlit run app.py``` in terminal to open the web application locally
