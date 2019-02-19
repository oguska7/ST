# Herding in the Social Media

A good portion of stock price swings can be explained by herd behavior - a phenomenon in which traders
intentionally ignore their beliefs in order to mimic someone else's action. 
As noted by Banerjee (1992), Bikhchandani et al. (1992), Bikhchandani and Sharma (2001),
mutual mimicry among investors may temporarily move asset prices away from fundamental values, 
which in turn would increase the spread around the mean.

To model herding and contrarianism in social media, Twitter data (StockTwits, proprietary) are used. 
The repository contains two codes: 
- Newscraper. scrapes reuters news and returns news polarity for a particular stock for each day over specified 
period of time;
- execution. takes preprocessed (preprocessing code is not posted) twitter data, runs regularized (l1) maxent 
classification based on existing bullish or bearish tags (which constitutes a training set); filters obtained 
dummy through the news polarity variable received from the Newscraper. and uses reply and retweet connection between
the authors to run the (geo)spatial lag regression using this filtered variable.

The spatial lag coefficient determines herding level for a day

