## Information Extraction and Analysis for Biomedical Articles



This is a little project using Prolog for analyzing and extracting information across multiple articles. Here are some examples for possible queries:

**Print the N most representative words for article (computed by tf-idf)**

 ```
?- printMostRepWords('Art1', 10).

subtilis       15.612359947967775
skfa           11.709269960975831
spo0a          9.107209969647869
strain         8.387640052032227
ybco           7.8061799739838875
log            7.8061799739838875
killing        7.8061799739838875
predation      6.505149978319906
viability      5.7673611866102314
lack           5.591760034688151
 ```



**Check whether word or lemma occurs in given article**

 ``` 
?- lemma_is_in_art('Art3', 'variant').
true.

?- word_is_in_art('Art3', 'protein').
false.
 ```



 **Find relations between entities in articles**

 ```
?- What amplifies 'COX16'.
What = 'the PCR method' .

?- activates(X,Y).
X = 'Thapsigargin',
Y = 'Osteocalcin transcription'.

?- affects(X,Y,'bind').
X = 'A natively unfolded region',
Y = 'Cu2' 
 ```



**Find things that causes *growth* of any kind using syntactic phrase search** 
Subject, object and verb phrase are returned as head and full phrase.

 ```
?- full_svo(Subj,'causes','growth',Sent,Art,Fullsbj,Fullverb,Fullobj
Subj = variant,
Verb = Fullverb, Fullverb = causes,
Obj = growth,
Sent = 'S15',
Art = 'Art3',
Fullsbj = 'the B variant',
Fullobj = 'an accelerated-rate growth
 ```



**Find sentences in passive voice**

```?- passive(Theme,Verb,Agens,Sent,Art,Fulltheme,Fullverb,Fullagens).
?- passive(Theme,Verb,Agens,Sent,Art,Fulltheme,Fullverb,Fullagens).
Theme = strain,
Verb = obtained,
Agens = lab,
Sent = 'S41',
Art = 'Art1',
Fulltheme = 'The mutant strain',
Fullverb = 'was obtained',
Fullagens = 'the lab' 
```



**Retrieve study focus and results**

 ```
?- print_study_focus('Art4').
In this study , we investigate the nuclease activity of the recombinant pea protein . 
true.

?- print_results_focus('Art4').
This result is in agreement with previous studies , which have shown binding to be mainly associated with the dimeric form of NDPK . 
Conclusively , NDPK3 cleavage is independent of a hexameric stucture or a functional phosphorylation site at H117 . The ATP status regulates NDPK3 cleavage , with inhibition at concentrations similar to those shown for NDPK - H2 . 
true.
 ```



**Get *is-A* relations**

 ```
?- 'CDKN1A' is_a X.
X = 'a well-known downstream target gene'.

?- 'Spo0A' is_protein.
true.
 ```