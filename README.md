
# risk_analytics
Material for the 2017 Risk Analytics workshop for the University of Ljubljana, Masters in Financial Mathematics

## Comments about code

For the first day, the examples in the lectures and the code snippets
for the tutorials were produced with R. The packages used are

* copula
* xtable
* dplyr
* ggplot2

The R code is in the repository folder `Day1_data_Rcode`.

For the second day, the examples from the lectures were produced using
Python 3.6. For basic setup with Python and Jupyter Notebooks (a
lightweight IDE to code and show your results), please see:

https://github.com/amueller/ml-training-intro

In particular, he shows with screenshots how to get a copy of the
material on a repository such as this one.

## Suggested reading

### Lecture 1: Financial crises and regulatory responses

* Michael Lewis, *The Big Short*, W. W. Norton and Company (2010)
* Anat Admati and Martin Hellwig, *The Bankers' New Clothes*,
http://bankersnewclothes.com
* Marcus Christiansen and Andreas Niemayer, *Fundamental definition of
  the solvency capital requirement in Solvency II*, ASTIN Bulletin
  (44), 2014. Available at https://www.uni-ulm.de/fileadmin/website_uni_ulm/mawi2/dokumente/preprint-server/2012/1202_definitionofscr.pdf

### Lecture 2: Correlated default
* A. J. McNeil, R. Frey, P. Embrechts, *Quantitative risk management:
  concepts, techniques and tools*, Princeton University Press (2010)
* C. Bluhm, L. Overbeck, C. Wagner, *Introduction to Credit Risk
Modeling*, CRC Press (2010)
* D. Li, *On default correlation: a copula function approach*,
  Technical Report, RiskMetrics Group (2000)
* I. Onur Filiz, Xin Guo, Jason Morton, Bernd Sturmfels, *Graphical
models for correlated defaults*, https://arxiv.org/abs/0809.1393
* Michael Kalkbrener and Natalie Packham, *Correlation under stress in
  normal variance mixture models, Mathematical Finance*, 25(2):
  426-456 (2015)
* R. C. Merton, *On the pricing of corporate debt: the risk structure
of interest rates*, Journal of Finance, 29(2): 449-470 (1974)
* M. Rutkowski, S. Tarca, *Regulatory capital modeling for credit
  risk*, arxiv:1412.1183 (2014)

### Lecture 3: Natural Language Processing and Feature Learning

* Tomas Mikolov, Ilya Sutskever, Kai Chen, Greg Corrado and Jeffrey Dean,
*Distributed Representations of Words and Phrases and their Compositionality*,
http://papers.nips.cc/paper/5021-distributed-representations-of-words-and-phrases-and-their-compositionality.pdf
* Marco Baroni, Georgiana Dinu and German Kruszewski, *Don’t count,
predict! A systematic comparison of context-counting
vs. context-predicting semantic vectors*,
http://www.aclweb.org/anthology/P14-1023
* Scikit-Learn tutorial on text analytics / bag of words: http://scikit-learn.org/stable/tutorial/text_analytics/working_with_text_data.html
* https://rare-technologies.com/word2vec-tutorial/
* https://www.tensorflow.org/tutorials/word2vec
* https://www.kaggle.com/c/word2vec-nlp-tutorial#part-2-word-vectors
* https://github.com/RaRe-Technologies/gensim/blob/develop/docs/notebooks/doc2vec-lee.ipynb

### Lecture 4: Bitcoin Blockchain and Graph Learning

* Aditya Grover, Jure Leskovec, *node2vec: scalable feature learning
  for networks*, Proceedings of the 22nd ACM SIGKDD International
  Conference on Knowledge Discovery and Data Mining,
  https://arxiv.org/abs/1607.00653 (2016)
* Aditya Grover, *node2vec*, https://github.com/aditya-grover/node2vec
* Satoshi Nakamoto, *Bitcoin: a peer-to-peer electronic cash system#*, https://bitcoin.org/bitcoin.pdf
* Vitalik Buterin et. al, *Ethereum white paper*, https://github.com/ethereum/wiki/wiki/White-Paper
* Arvind Narayanan, Joseph Bonneau, Edward W. Felten, Andrew Miller, Steven Goldfeder, Jeremy Clark, *Bitcoin and cryptocurrency technologies*, https://d28rh4a8wq0iu5.cloudfront.net/bitcointech/readings/princeton_bitcoin_book.pdf
