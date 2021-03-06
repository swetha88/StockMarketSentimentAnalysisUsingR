# Stock Market Sentiment Analysis Using R
## Twitter stock market sentiment analysis 
### Basic workflow for twitter sentiment analysis
1. Collecting data from sources
2. Fetch Twitter Data
3. Text Cleaning
4. Text processing 
5. Frequent Terms
6. Word Cloud
7. Sentiment Analysis

**Collect data from sources**  *First step here starts with collecting data from different sources like Google finance, Yahoo finance, twitter, stock market news, etc. We have loads and loads of data but only the relevant data should be extracted for our purpose. For this project I used yahoo finance to choose the largest gainer stocks and largest loser stocks of my choice or choose the stocks of your choice, that has tweets for each stock for that day. (https://finance.yahoo.com/)*

**Fetch Twitter data** *To fetch the twitter data we would need to create an application using twitter API. The data Connection is authenticated using oauth and Data is fetched using the twitteR library. Using these libraries datasets are created.*

**Text Cleaning**  *Remove the exceptional characters(clean the data)*

**Text processing** *here we create a data corpus or text corpus is a large and structured set of texts. They are used to do statistical analysis and hypothesis testing
- Convert to lower case
- Remove punctuation
- Remove numbers
- Remove stopwords
- Remove domain-specific stopwords
- Stemming

**Frequent Terms(word frequency)** *Collection of words that appear frequently*

**Sentiment Analysis** *Sentiment analysis is used to analyze the given data conveys a positive, negative or a neutral sentiment.*
