# Corpus

<br />

This folder contains the corpus used in the training stage of this application.

<br />

## Corpus file

The corpus file `cameo_select.csv` is in commma separated values.

The file contains the following fields:

* **url**: link to the original news article
* **date**: publication date
* **source**: name of the news soruce
* **sentences**: single sentence of news content
* **stems**: sentence in stem format
* **relevant_stems**: stems relevant to CAMEO categories


<br />

## Content

The corpus contains 21,000 sentences randomly selected from 35 news agencies. The sample includes 600 sentences per source.

The corpus contains a single sentence per row of headlines and leadlines of news reports.

The sources included in the corpus are:

* ABC news
* Al Jazeera
* All Africa
* AP News
* BBC
* CNA
* CNBC
* CNN
* The Conversation
* DW
* EFE
* France 24
* The Guardian
* India Times
* Japan Times
* LA Times
* NBC
* News 24
* NPR
* New York Post
* New York Times
* PBS
* Politico
* Reuter
* RFI
* SCMP
* SF Gate
* TASS
* The News PK
* UPI
* USA Today
* US News
* Washington Post
* Wall Street Journal
* Xinhua


<br />

## Sentence selection

To select these sentences, we used the CAMEO verbs and their synonyms included in "CAMEO.verbpatterns.150430.txt", which is available [here](
https://github.com/openeventdata/UniversalPetrarch/blob/master/UniversalPetrarch/data/dictionaries/CAMEO.verbpatterns.150430.txt)



<br />

## Sample to Train Annotators

File `cameo_100.csv` contains a random selection of 100 sentences derived from the corpus. This file includes the following fields:

* **No.**: unique identifier per sentence
* **sentences**: single sentence of news content
* **url**: URL of the original news story
* **date**: publication date
* **source**: name of the news soruce

To train coders in the different annotation tasks, we used the annotation platform [TagTog](https://www.tagtog.net/) to annotate this sample of 100 stories.



<br />

## Crawlers and Processes

The folder `Crawlers and Processes` contains the scripts used to generate the corpus used in this study.  The folder includes:

* **Crawlers and Processes**: example crawlers for two types of sources. We combined Newspaper3k and manually designed patterns with Beautiful Soup.
* **Preprocess**: Preprocessing pipelines for five different types of sources. Cleaning and filtering stories in conflicts domain.
* **Patterns**: statistically summarized the most frequent keywords’ regular expressions to filter conflicts domain.

The scripts in this folder run on `Python v.XXX`.



<br />
<br />
<br />
