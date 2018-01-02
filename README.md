# Lessons from CEO's: Analyzing the Text from the New York Times Corner Office Column in R

This repo is an extension of a previous project found at . The goal of the previous project was to provide tools to social science researchers to extract topics from a small number of interview transcripts. I used transcripts from the New York Times Corner Office Column for test data. I was curious about what an analysis of all the New York Times Corner Office Column articles would look like. Thus, the inspiration from this project. Steps included:
1. Using the NYT API to get the URL's for Corner Office articles
2. Using these URL's to obtain the content of the articles (not available directly from the API)
3. Cleaning the transcripts such that the interview questions were removed and each response was coded as an individual document
4. Cleaning the text data
5. Producing a word cloud of the most frequent words
6. Using LDA to identify common topics

## Project Steps

The following steps will allow you to replicate the analysis on your computer. Alternatively, you can change the search terms in the NYT API to use this same analysis for different keywords.

### Prerequisites

Please install the following R libraries:

```
library(XML)
library(rtimes)
library(stringr)
library(tm)
library(httr)
library(topicmodels)
library(tidytext)
library(dplyr)
library(tibble)
library(wordcloud2)
```

### Installing

A step by step series of examples that tell you have to get a development env running

Say what the step will be

```
Give the example
```

And repeat

```
until finished
```

End with an example of getting some data out of the system or using it for a little demo

## Running the tests

Explain how to run the automated tests for this system

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone who's code was used
* Inspiration
* etc
