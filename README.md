# emos-codinglab-2024


The 3rd edition of the EMOS Coding Lab entitled ‘Statistics Explained through literate programming’ was organised in 2024. The goal of this edition was to replicate [Statistics Explained](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Main_Page) articles and reimplement them in R, streamlining the production process for retrieving the data using the [Eurostat Application Programming Interface - API](https://github.com/eurostat/restatapi). The implemented automatise calculations, the generation of tables and visualisations and, more ambitiously, the integration of text in computational notebooks (e.g. with [R Markdown](https://rmarkdown.rstudio.com/)).  

Six students from EMOS-labelled master´s programmes worked in four groups from March to June on selected Statistics Explained articles covering the domains of culture statistics, sports statistics, and quality of life indicators.

| Article | Group |
| --- | --- |
| [Quality of life indicators - overall experience of life](https://ec.europa.eu/eurostat/statistics-explained/index.php?oldid=622907) | [Group 1](code/group1) |
| [Consumer prices of recreational and sporting goods and services](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Consumer_prices_of_recreational_and_sporting_goods_and_services) | [Group 1](code/group1)  |
| [Quality of life indicators - natural and living environment](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Quality_of_life_indicators_-_natural_and_living_environment) | [Group 2](code/group2)  |
| [Young people - digital world](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Young_people_-_digital_world) | [Group 2](code/group2)  |
| [Culture statistics - international trade in cultural goods](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Culture_statistics_-_international_trade_in_cultural_goods) | [Group 3](code/group3) |
| [Consumer prices of cultural goods and services](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Consumer_prices_of_cultural_goods_and_services) | [Group 3](code/group3) |
| [Young people - housing conditions](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Young_people_-_housing_conditions) | [Group 4](code/group4) |
| [Culture statistics - cultural employment](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Culture_statistics_-_cultural_employment) | [Group 4](code/group4) |


The students presented their work Friday, 28 June 2024. The presentation slides you can find [here](CodingLab2024.pdf).  

Here you can start Jupyter and Rstudio to see and play with implemented articles in the cloud:


Launch [`RStudio`](https://rstudio.com/):  [![badge](https://img.shields.io/badge/Codinglab%202024-binder-579ACA.svg?logo=data:image/png;base64,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)](https://mybinder.org/v2/gh/eurostat/emos-codinglab-2024/4b4879cb495086350ebe38f16fd909db2acd4619?urlpath=rstudio) <!-- [![Binder](https://mybinder.org/badge_logo.svg)](http://mybinder.org/v2/gh/eurostat/emos-codinglab-2024/master?urlpath=rstudio) -->
