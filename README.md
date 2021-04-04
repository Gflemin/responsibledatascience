---
title: "README"
output: html_document
---

![Book cover](/images/responsible_ds_cover.jpg)

# Code repository
**Responsible Data Science**:  

Transparency and Fairness in Algorithms

Grant Fleming and [Peter Bruce](https://www.amazon.com/Peter-Bruce/e/B01N3C4ACN%3Fref=dbs_a_mng_rwt_scns_share)

- Publisher: Wiley; 1st edition (May 11th, 2022)
- ISBN-10: 1119741750
- ISBN-13: 978-1119741756
- Buy on [Amazon](https://www.amazon.com/Responsible-Data-Science-Transparency-Algorithms/dp/1119741750)
- Errata: https://www.wiley.com/en-us/Responsible+Data+Science-p-9781119741756

## R
The R scripts for each chapter are located within the _chapters/_
folder of the repository. Each of these scripts "source" (load) functions 
and packages from other, relative paths within the repository. 
As such, we recommend that readers interested in running the scripts do the following.

1. Double click the .Rproj file to open an RStudio session. 
2. Open any of the scripts from within the chapters/ folder. 
3. Execute the setup code chunk within the script and enter any prompts to install packages.
4. Run the script.

Note that running these scripts without modification requires:

- renv >= 0.10.0
- R >= 3.6
- RStudio >= 1.2

## Python
The Python code for the book is exclusively for the facial recognition
example in Chapter 8. Unfortunately, the code and data for this chapter is 
large in size and does not fit into the code repository itself. To download and
run it yourself, you should do the following:

1. Navigate to the folder at https://drive.google.com/drive/folders/13OpxSM3B1ff5AHkRbcHsR4Ys3_44OCuA?usp=sharing and
download both files
2. Upload the code_and_data.zip file to the home directory of your personal Google Drive _without_ unzipping the .zip
file.
3. Navigate to https://colab.research.google.com/notebooks/ and click "Upload" to upload the 
ch8_fairness_in_computer_vision.ipynb file in Google Colab.
4. Run the first code chunk (from google.colab import drive...) and input the authentication code.

## See also
- Wiley: https://www.wiley.com/en-us/Responsible+Data+Science-p-9781119741756
- The code repository for the first edition the book: https://github.com/gflemin/responsibledatascience