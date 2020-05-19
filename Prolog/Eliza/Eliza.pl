% INPUT PARSER
%readLetter(+inputLetter, -listOfLettersTheWordConsistsof, -letterFollowingTheWord)
readLetters(46, [], 46) :- !.  %full stop .
readLetters(33, [], 33) :- !.  %Exclamation mark !
readLetters(63, [], 63) :- !.  %question mark ?
readLetters(32, [], 32) :- !.  %space
readLetters(9, [], 9) :- !.  %horizontal tab
%end of line?
readLetters(Letter, [Letter|LetterList], AnotherLetter):-
    get_code(Code),
    readLetters(Code,LetterList, AnotherLetter).

%readSentence(-listOfWordsInASentence)
readSentence(WordList):-
    get(Char),  %read the first letter
    readRest(Char,WordList).

%readRest(+Char, -WordList)
readRest(46, []) :- !.  %full stop .
readRest(33, []) :- !.  %Exclamation mark !
readRest(63, []) :- !.  %question mark ?
readRest(32, WordList) :- 
    readSentence(WordList).  %space
readRest(9, WordList) :- 
    readSentence(WordList).  %horizontal tab
readRest(Letter, [Word | WordList]):-
    readLetters(Letter, LetterList, AnotherLetter),
    name(Word, LetterList),
    readRest(AnotherLetter,WordList).
    


