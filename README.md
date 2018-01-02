# Lessons from CEO's: Analyzing the Text from the New York Times Corner Office Column in R

This repo is an extension of a previous project found at . The goal of the previous project was to provide tools to social science researchers to extract topics from a small number of interview transcripts. I used transcripts from the New York Times Corner Office Column for test data. I was curious about what an analysis of all the New York Times Corner Office Column articles would look like. Thus, the inspiration from this project. Steps included:
1. Using the NYT API to get the URL's for Corner Office articles (approximately 525 articles)
2. Using these URL's to obtain the content of the articles (not available directly from the API)
3. Cleaning the transcripts such that the interview questions were removed and each response was coded as an individual document
4. Cleaning the text data
5. Producing a word cloud of the most frequent words
6. Using LDA to identify common topics

## Project Steps

The following steps will allow you to replicate the analysis on your computer. Alternatively, you can change the search terms in the NYT API to use this same analysis for different keywords.

### Prerequisites

Before initiating this project, you will need to aquire a NYT API key https://developer.nytimes.com/signup and install the following R libraries:

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

Sys.setenv(NYTIMES_AS_KEY = "Insert your NYT API key here")
```

### Obtaining URL's

I used #http://brooksandrew.github.io/simpleblog/articles/new-york-times-api-to-mongodb/ to help write this block of code. Note key limitations of the NYT Search API: the API returns information (including URLs) in groups of 10, there is a maximum of 1000 requests per day, and there must be 1 second between requests. 

The key variables in this for loop:
1. (i in 0:53) tells the loop to start at Page 0 and go to Page 53. Each page produces 10 articles, so there are 540 total articles. Note that there are only 525 Corner Office interviews. However, one flaw with the API search is that you can only use it with relevant key terms (not a specific column) and some other articles were picked up in the search.
2. Sys.sleep(1) allows the API to function
3. The key terms "corner office" and "adam bryant were used to search the title, author, and text body
4. The end date is 20171025 as that is the date of the last article

```
urllist<-c()
urls <- c()
i=0
for (i in 0:53){
  Sys.sleep(1)
  cornerarticles <- as_search(q="corner office adam bryant",end_date = '20171025',page=i)
  urllist <- cornerarticles$data$web_url
  print(length(urllist))
  urls <- c(urls,urllist)
}
urls
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
