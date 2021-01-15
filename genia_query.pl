% Final Prolog Project by Julia Suter
% in the course
% Fabio Rinaldi - Logic Programming for Knowledge Representation, Spring Semester 2015

% Get the data
:- [prolog_data].

% Access on token data

token(Position,Word, Lemma, Pos, Sent,Art) :- 	token(Position, Word, Lemma, Pos, Sent, _,_,_,Art).
lemma(Word,Lemma,Pos) :- 			token(Position,Word,Lemma,Pos,_,_).

% Print Sentence - not perfect, just for use while impelementing

print_sentence(Sentence,Art) :- 		sentence(Sentence,List,Type,Art), word_list(List,Wordlist,Sent,Art),member(Q,Wordlist), write(Q), write(' '), fail.

% Get word by position or v.v.

word(Position,Word,Sent,Art) :- 		token(Position, Word,_,_,Sent,Art).
position_list(Positionlist, Wordlist,Sent,Art) :- findall(Position, (member(Word,Wordlist), word(Position,Word,Sent,Art)), Positionlist).
word_list(Positionlist,Wordlist,Sent,Art):-	findall(Word, (member(Position,Positionlist),word(Position,Word,Sent,Art)), Wordlist).
lemma_list(Positionlist,Lemmalist,Sent,Art):-	findall(Lemma,(member(Position,Positionlist),token(Position,_,Lemma,_,Sent,Art)), Lemmalist).

% Get all sentences, words, lemmas...

all_sentences_in_art(Sentences,Art) :-		findall(Sent,sentence(Sent,_,_,Art),Sentences).
all_words_in_sent(Words,Sent,Art):-		sentence(Sent,List,_,Art), findall(Word,(member(Position,List), word(Position,Word,Sent,Art)),Words).
all_lemmas_in_sent(Lemmas,Sent,Art):-		sentence(Sent,List,_,Art), findall(Lemma,(member(Position,List), token(Position,_,Lemma,_,Sent,Art)),Lemmas).

all_words_in_art(WordsinArt,Art):-		all_sentences_in_art(Sentences,Art), findall(Words,(member(Sent,Sentences), all_words_in_sent(Words,Sent,Art)),Allwords), combine(Allwords,WordsinArt).
all_lemmas_in_art(LemmasinArt,Art):-		all_sentences_in_art(Sentences,Art), findall(Words,(member(Sent,Sentences),all_lemmas_in_sent(Words,Sent,Art)),Allwords), combine(Allwords,LemmasinArt).

number_of_arts(AllArticles,Count) :-		findall(Art,sentence(_,_,_,Art),Arts), setof(Q,Arts^member(Q,Arts),AllArticles),length(AllArticles,Count).

% Check if word/lemma is in Art/Sent

word_is_in_sent(Word, Sent, Art) :- 		word(Position, Word,Sent, Art), sentence(Sent, List, _,Art), member(Position,List).
word_is_in_art(Word,Art) :-			all_words_in_art(Allwords,Art),member(Word,Allwords),!.
lemma_is_in_art(Lemma,Art):-			all_lemmas_in_art(Alllemmas,Art), member(Lemma,Alllemmas),!.
words_are_in_sent(Words,Sent,Art) :-		all_words_in_sent(Wordlist,Sent,Art),sublist(Words,Wordlist).

part_of_sent(Sublist, Sent,Art) :- 		sentence(Sent, List,_,Art), sublist(Sublist,List).

% Occurences and Count of Lemma

occurences(Lemma,List,Count,Art) :- 		findall(Position,token(Position,_,Lemma,_,_,Art),List), length(List,Count). 

% TF-IDF

tf(Word,Count,Art):-				occurences(Word,_,Count,Art).
df(Word,Arts,Count) :-				number_of_arts(Articles,_), findall(Art, (member(Art,Articles), lemma_is_in_art(Word,Art)),Arts), length(Arts,Count).
idf(Word,Y) :-					df(Word,_,Count), number_of_arts(_,N), X is (N/Count), Y is log10(X).

tfidf(Word,Art,X) :-				lemma_is_in_art(Word,Art), tf(Word,Number,Art), idf(Word,Y), X is Number*Y.

tfidfList(Art,RepList):-			all_lemmas_in_art(Lemmas,Art), 
						setof([X,Lemma], (member(Lemma,Lemmas), tfidf(Lemma,Art,X)), List),
						msort(List,SortedList),
						reverse(SortedList,RepList).

% Get most representive words by TF-IDF score

prefix_of(L,N,P) :-   				append(P,_,L) , length(P,N).

most_rep_word(Art,H) :-				tfidfList(Art,[H|T]).

printMostRepWords(Art,N) :- 			tfidfList(Art,List), prefix_of(List,N,Top), 
						forall(member([Score,Lemma],Top), (sformat(S, '~w~t~15|~w', [Lemma, Score]),write(S),nl)),!.

% Get (longest) noun and verb groups by Sentence and Keyword


ng_in_sent(Word, Fullng, Sent, Art) :-		chunk(Position,_,NG,List, Art), word(Position,Word,Sent,Art), word_list(List,Fullng,Sent,Art), part_of_sent(List,Sent,Art).
vg_in_sent(Word, Fullvg, Sent, Art) :-		chunk(Position,_,VG,List, Art), word(Position,Word,Sent,Art), word_list(List,Fullvg,Sent,Art), part_of_sent(List,Sent,Art).

longest_ng(Word,Longestng,Sent, Art) :- 	findall(Fullng,(ng_in_sent(Word,Fullng,Sent,Art)),Listng), longest_list(Listng,Longest), string(Longest,Longestng).
						%whole_ng(Word,Longestng,NG,Sent,Art).

longest_vg(Word,Longestvg, Sent, Art) :- 	findall(Fullvg,(vg_in_sent(Word,Fullvg,Sent,Art)),Listvg), longest_list(Listvg,Longest), string(Longest,Longestvg).

% Get the full phrase for a word and the corresponding phrase according to the function

dep(Word, Depword, Function, Sent, Art) :- 	hdp(Firstword, Secword, Function,_,Sent,Art), 
				      		token(Firstword,Word,_,_,Sent,Art),
				      		token(Secword,Depword,_,_,Sent,Art).

depfull(Word, Depword,Function,Sent, Art, Fullng, Fullvg) :- dep(Word,Depword,Function,Sent,Art),
						   	longest_ng(Depword,Fullng,Sent,Art),
						   	longest_vg(Word,Fullvg,Sent,Art).

% Get full phrases (Subj, Verb, Object, Agens)


subj_verb(Subj,Verb,Sent,Art) :- 					depfull(Verb, Subj, subj,Sent,Art,_,_).
full_subj_verb(Subj,Verb,Sent,Art,Fullng,Fullvg) :-			depfull(Verb,Subj, subj, Sent,Art, Fullng, Fullvg).

full_verb_obj(Verb, Obj, Sent, Art, Fullvg, Fullng) :- 			depfull(Verb, Obj, obj, Sent, Art,Fullng, Fullvg).

full_svo(Subj, Verb, Obj, Sent, Art, Fullsbj, Fullverb, Fullobj) :- 	full_subj_verb(Subj,Verb,Sent, Art, Fullsbj,Fullverb),
							       		full_verb_obj(Verb, Obj, Sent, Art, Fullverb, Fullobj),
							       		not(token(_,Verb,_,'VBN',Sent, Art)).

full_verb_agens(Verb,Obj,Sent,Art,Fullobj,Fullverb) :- 			depfull(Verb,Obj,pobj,Sent,Art,Fullobj,Fullverb), 
									token(_,Verb,_,'VBN',Sent,Art).
							
prep_obj(Obj,Prep,Sent,Art) :- 						dep(Obj,Prep,prep,Sent,Art), 
									passive_prep(Prep), 
									token(_,Prep,_,_,Sent,Art).

% Passive phrases

passive(Theme,Verb,Agens,Sent,Art,Fulltheme, Fullverb, Fullagens) :-  full_verb_agens(Verb,Agens,Sent,Art,Fullagens,Fullverb),																								
								full_subj_verb(Theme,Verb,Sent,Art,Fulltheme,Fullverb),
								prep_obj(Agens,Prep,Sent,Art).														
passive_prep('by').
passive_prep('via').
passive_prep('through').


% A activates B

activates_full(A,B,Sent,Art) :- token(_,Verb, 'activate',_,Sent,Art), full_svo(Subj,Verb,Obj,Sent,Art,A,Fullverb,B).
activates_full(A,B,Sent,Art) :- token(_,Verb,'activate',_,Sent,Art), passive(Subj,Verb,Obj,Sent,Art,B,Fullverb,A).

act(A,B):- token(_,Verb, 'activate',_,Sent,Art), full_svo(A,Verb,B,Sent,Art,Fullsubj,Fullverb,Fullobj).
act(A,B):- token(_,Verb, 'activate',_,Sent,Art), passive(B,Verb,A,Sent,Art,Fullsubj,Fullverb,Fullobj).

activates(A,B) :- act(A,B).
activates(A,B) :- act(A,C), act(C,B).

% More A affects B

constructs(Fullobj,Fullsubj,Sent,Art) :- token(_,Verb,'construct',_,Sent,Art), passive(Subj,Verb,Obj,Sent,Art,Fullsubj,Fullverb, Fullobj).
constructs(Fullsubj,Fullobj,Sent,Art) :- token(_,Verb,'construct',_,Sent,Art), full_svo(Subj,Verb,Obj,Sent,Art,Fullsubj,Fullverb,Fullobj).
constructs(X,Y) :- constructs(X,Y,_,_).

binds(Fullobj,Fullsubj,Sent,Art) :- token(_,Verb,'bind',_,Sent,Art), passive(Subj,Verb,Obj,Sent,Art,Fullsubj,Fullverb, Fullobj).
binds(Fullsubj,Fullobj,Sent,Art) :- token(_,Verb,'bind',_,Sent,Art), full_svo(Subj,Verb,Obj,Sent,Art,Fullsubj,Fullverb,Fullobj).
binds(X,Y) :- binds(X,Y,_,_).

elevates(Fullobj,Fullsubj,Sent,Art) :- token(_,Verb,'elevate',_,Sent,Art), passive(Subj,Verb,Obj,Sent,Art,Fullsubj,Fullverb, Fullobj).
elevates(Fullsubj,Fullobj,Sent,Art) :- token(_,Verb,'elevate',_,Sent,Art), full_svo(Subj,Verb,Obj,Sent,Art,Fullsubj,Fullverb,Fullobj).
elevates(X,Y) :- elevates(X,Y,_,_).

cleaves(Fullobj,Fullsubj,Sent,Art) :- token(_,Verb,'cleave',_,Sent,Art), passive(Subj,Verb,Obj,Sent,Art,Fullsubj,Fullverb, Fullobj).
cleaves(Fullsubj,Fullobj,Sent,Art) :- token(_,Verb,'cleave',_,Sent,Art), full_svo(Subj,Verb,Obj,Sent,Art,Fullsubj,Fullverb,Fullobj).
cleaves(X,Y) :- cleaves(X,Y,_,_).

amplifies(Fullobj,Fullsubj,Sent,Art) :- token(_,Verb,'amplify',_,Sent,Art), passive(Subj,Verb,Obj,Sent,Art,Fullsubj,Fullverb, Fullobj).
amplifies(Fullsubj,Fullobj,Sent,Art) :- token(_,Verb,'amplify',_,Sent,Art), full_svo(Subj,Verb,Obj,Sent,Art,Fullsubj,Fullverb,Fullobj).
amplifies(X,Y) :- amplifies(X,Y,_,_).	
						
affects(Fullobj,Fullsubj,Sent,Art,Action) :- token(_,Verb, Action,_,Sent,Art), passive(Sbj,Verb,Obj,Sent,Art, Fullsubj,Fullverb, Fullobj).
affects(Fullsubj,Fullobj,Sent,Art,Action) :- token(_,Verb, Action,_,Sent,Art), full_svo(Subj,Verb,Obj,Sent,Art,Fullsubj,Fullverb,Fullobj).
affects(X,Y,Action) :- affects(X,Y,_,_,Action).

% Subject, Object ...

object(Verb,Fullobj) :- 	full_verb_obj(Verb,_,_,_,_,Fullobj).
subject(Fullsubj,Verb) :- 	full_subj_verb(_,Verb,_,_,Fullsubj,_).
action(Subj,Verb,Obj) :-	full_svo(S,V,O,_,_,Subj,Verb,Obj).


% Article Sections

part(Part,Words,Sent,Art):-	 	sentence(Sent,List,Part,Art), word_list(List,Words,Sent,Art).

introduction(IntroSents,Art) :- 	findall(Words, (part(section-['Introduction'],Words,_,Art)),IntroSents).
print_introduction(Art):-		introduction(IntroSents,Art), forall(member(S,IntroSents), print_sent(S)).
study_focus(Art,MainSents) :- 		introduction(IntroSents,Art), findall(S, (member(S,IntroSents),sublist(['study'],S)),MainSents).
print_study_focus(Art) :- 		introduction(IntroSents,Art), findall(S, (member(S,IntroSents),sublist(['study'],S)),MainSents), 
					forall(member(Q,MainSents),(print_sent(Q),nl)).

results(ResultSents,Art):-		findall(Words,((part(section-['Results'],Words,_,Art));
					(part(section-['Discussion'],Words,_,Art));
					(part(section-['Results and discussion'],Words,_,Art))),ResultSents).
print_results(Art):-			results(ResultSents,Art), forall(member(S,ResultSents), print_sent(S)).
result_focus(Art, MainSents):-		results(ResultSents,Art), findall(S, (member(S,ResultSents),sublist(['shown'],S)),MainSents).
print_results_focus(Art):-		results(ResultSents,Art), findall(S, (member(S,ResultSents),sublist(['shown'],S)),MainSents),
					forall(member(Q,MainSents),(print_sent(Q),nl)).


% Compounds

compound(X,Y,Compound,Sent,Art) :-  depfull(X,Y,hyph,Sent,Art,Xfull,Yfull), atomic_list_concat([Xfull,Yfull],'-', Compound).
compound(X,Compound,Sent,Art) :- compound(X,_,Compound,Sent,Art),!.
compound(X,Compound,Sent,Art) :- compound(_,X,Compound,Sent,Art),!.
compound(X,Compound) :-		 compound(X,Compound,_,_).

% Conjunctions

noun_pos('NN').
noun_pos('NNS').
noun_pos('NNP').

conj(Xfull,Yfull,Sent,Art,Pos) :- 				depfull(X,Y,conj,Sent,Art,Xfull,Yfull), token(_,X,_,Pos,Sent,Art).
nom_conj(Xfull,Yfull,Sent,Art) :-				conj(Xfull,Yfull,Sent,Art,Pos), noun_pos(Pos).

% of-constructions

of(Xfull,Yfull, Sent,Art) :- depfull(Y,X,modpp,Sent, Art, Yfull, Xfull).
is_of(Yfull,Xfull) :- depfull(_,_,modpp,_,_,Xfull,Yfull).


% What is it?

is_a(X,Y,Fullverb,Sent,Art) :- 		full_svo(X,Verb,Obj,Sent,Art,Fullsbj,Fullverb,Y), token(_,Verb,'be',_,Sent,Art), 
					not(sublist('not',Fullverb)), not(token(_,X,_,'WDT',Sent,Art)), not(token(_,Verb,_,'VBN',Sent,Art)).
is_a(X,Y) :-				is_a(X,Y,_,_,_).

% Is protein - not reliable

protein(Word) :-			term(_,_,Word,'PROT',_,_,_).
is_protein(Word) :-			protein(Word),!.


% Help Functions

print_sent(X) :-			forall(member(Q,X), (write(Q), write(' '))).
 
substring(A,B) :- 	 		atom_chars(A,X), atom_chars(B,S), append(_,T,S) , append(X,_,T) , X \= [],!.

string(List,String) :- 			atomic_list_concat(List,' ',String).

longest_list([L], L) :- !.
longest_list([H|T], H) :- 		length(H, N), longest_list(T, X), length(X, M), N > M,!.
longest_list([H|T], X) :- 		longest_list(T, X),!.

sublist( Sublist, List ) :- 		append( [_, Sublist, _], List ).

combine([],[]).
combine([X|Xs],Y) :- 			combine(X,XX), combine(Xs,XXs), append(XX,XXs,Y),!.
combine(X,[X]).

% Was not used in the end...

tags(Wordlist,Taglist,Sent,Art) :-	findall(Tag, (token(_, Word, _,Tag, Sent,Art), member(Word,Wordlist,Art)), Taglist), words_are_in_sent(Wordlist,Sent,Art).
before(A,B,Sent,Art) :- 		number(A,Sent,Art,PosA), number(B,Sent,Art,PosB), PosA < PosB.
number(Word,Sent, Art, Number) :- 	token(Position,Word,_,_,Sent,Art), atom_chars(Position,[H|T]), atomic_list_concat(T,'',Num), atom_number(Num,Number).


% Operators

:- op(500, xfy, binds).
:- op(500, xfy, constructs).
:- op(500, xfy, elevates).
:- op(500, xfy, cleaves).
:- op(500, xfy, amplifies).
:- op(500, xfy, activates).
:- op(500, xfy, object).
:- op(500, xfy, subject).
:- op(500, xfy, is_of).
:- op(500, xfy, is_a).
:- op(500, xf, is_protein).

