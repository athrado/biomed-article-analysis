:- [genia_query].

writeExamples :- open('example_results.txt', write, Stream), 

	   write(Stream, 'Example Results for Final Prolog Project'), nl(Stream), write(Stream,'*****************************************'),
           nl(Stream), nl(Stream),

	   write(Stream, 'Ten Most Representative Words in First Article:'), nl(Stream),nl(Stream),
	   tfidfList('Art1',TfidfList), prefix_of(TfidfList,10,Top),
	   forall(member([Score,Lemma],Top), (sformat(S, '~w~t~15|~w', [Lemma, Score]), 
	   write(Stream, '- '),write(Stream,S), nl(Stream))), nl(Stream), nl(Stream), nl(Stream),

	   write(Stream, 'A activates B:'), nl(Stream), nl(Stream),
	   findall([A|B],activates_full(A,B,_,_),List), forall(member([C|D],List),(sformat(S,'~w~t~35|activates  ~w',[C,D]),
	   write(Stream,S),nl(Stream))),

	   nl(Stream), nl(Stream),
	   write(Stream, 'A cleaves B:'), nl(Stream), nl(Stream),
	   findall([A|B],cleaves(A,B,_,_),CleaveList), forall(member([C|D],CleaveList),(sformat(W,'~w~t~35|cleaves  ~w',[C,D]),
	   write(Stream,W),nl(Stream))),

	   nl(Stream), nl(Stream),
	   write(Stream, 'Study Focus for Article 17'), nl(Stream), nl(Stream),
	   study_focus('Art4', MainSents), forall(member(Sents, MainSents), (forall(member(L,Sents), (write(Stream,L), write(Stream,' '))),nl(Stream))),
          
	   close(Stream).

