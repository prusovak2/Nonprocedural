%%%%%%%%%%%%%%%%%%%%%%%%%%% INPUT PARSER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%readLetter(+inputLetter, -listOfLettersTheWordConsistsof, -letterFollowingTheWord)
readLetters(46, [], 46) :- !.  %full stop .
readLetters(33, [], 33) :- !.  %Exclamation mark !
readLetters(63, [], 63) :- !.  %question mark ?
readLetters(32, [], 32) :- !.  %space
readLetters(9, [], 9) :- !.  %horizontal tab
readLetters(44, [], 44) :- !.  %comma
%end of line?
readLetters(Letter, [Letter|LetterList], AnotherLetter):-
    get_code(Code),
    readLetters(Code,LetterList, AnotherLetter).

%readRest(+Char, -WordList)
readRest(46, []) :- !.  %full stop .
readRest(33, []) :- !.  %Exclamation mark !
readRest(63, []) :- !.  %question mark ?
readRest(32, WordList) :- 
    readSentence(WordList).  %space
readRest(44, WordList) :- 
        readSentence(WordList).  %comma
readRest(9, WordList) :- 
    readSentence(WordList).  %horizontal tab
readRest(Letter, [Word | WordList]):-
    readLetters(Letter, LetterList, AnotherLetter),
    name(Word, LetterList),
    readRest(AnotherLetter,WordList).
    
%readSentence(-listOfWordsInASentence)
readSentence(WordList):-
    get(Char),  %read the first letter
    readRest(Char,WordList).

keyword(remember, 20).
keyword(computer, 10).
keyword(my, 24).
keyword(what, 24).
keyword(yes, 25).
keyword(no, 25).

findMostImportantKeyWord([],Res,Res):- nonvar(Res),!.
findMostImportantKeyWord([],_,none).
findMostImportantKeyWord([Word|List], BestSoFar, Res):-
    var(BestSoFar),
    keyword(Word, _),
    findMostImportantKeyWord(List, Word, Res), !.
findMostImportantKeyWord([Word|List], BestSoFar, Res):-
    keyword(Word, WordPriority),
    keyword(BestSoFar, BestPriority),
    WordPriority < BestPriority,
    findMostImportantKeyWord(List, Word, Res), !.
findMostImportantKeyWord([_|List], BestSoFar, Res):-
    findMostImportantKeyWord(List, BestSoFar, Res).

test(KeyW):-
    readSentence(Sen), findMostImportantKeyWord(Sen,_,KeyW ).

 
