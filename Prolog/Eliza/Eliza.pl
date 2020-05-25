%%%%%%%%%%%%%%%%%%%%%%%%%%% INPUT PARSER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%readLetter(+inputLetter, -listOfLettersTheWordConsistsof, -letterFollowingTheWord)
readLetters(46, [], 46) :- !.  %full stop .
readLetters(33, [], 33) :- !.  %Exclamation mark !
readLetters(63, [], 63) :- !.  %question mark ?
readLetters(32, [], 32) :- !.  %space
readLetters(9, [], 9) :- !.  %horizontal tab
readLetters(44, [], 44) :- !.  %comma
%end of line - what should I do about it?
readLetters(10, [], 10):-!. %Line feed
readLetters(13, [], 13):-!. %carrige return
readLetters(Letter, [Letter|LetterList], AnotherLetter):-
    get_code(Code),
    charToLowerCase(Code, LowerCode),
    readLetters(LowerCode,LetterList, AnotherLetter).

%readRest(+Char, -WordList)
readRest(46, []) :- !.  %full stop .
readRest(33, []) :- !.  %Exclamation mark !
readRest(63, []) :- !.  %question mark ?
readRest(32, WordList) :- 
    readSentence(WordList).  %space
readRest(44, WordList) :- 
        readSentence(WordList).  %comma
readRest(10, WordList) :- 
            readSentence(WordList). %Line feed
readRest(13, WordList) :- 
            readSentence(WordList). %carrige return
readRest(9, WordList) :- 
    readSentence(WordList).  %horizontal tab
readRest(Letter, [Word | WordList]):-
    readLetters(Letter, LetterList, AnotherLetter),
    name(Word, LetterList),
    readRest(AnotherLetter,WordList).
    
%readSentence(-listOfWordsInASentence)
readSentence(WordList):-
    get(Char),  %read the first letter
    charToLowerCase(Char,LowChar),
    readRest(LowChar,WordList).

charToLowerCase(In,Out):-
    In >= 65,
    In =< 90,
    Out is In + 32.
charToLowerCase(In,In).

%does not work for me as string_lower cast string into ""
/*wordListToLowerCase([H|InputList], [LowH |LowerCaseList]):-
    string_lower(H, LowH),
    wordListToLowerCase(InputList, LowerCaseList).
wordListToLowerCase([],[]).*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DATABAZE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%keyword(keyword, priority)
%database of keywords
keyword(computer, 10).
keyword(remember, 20).
keyword(are, 24).
keyword(my, 24).
keyword(what, 24).
keyword(yes, 25).
keyword(no, 25).

%pattern(keyword,question pattern, pattern Id)
%to match keyWords to corresponding question patterns
pattern(remember,[73,you,remember,1], 1).
pattern(remember,[_,do,i,remember,1], 2).
pattern(remember,[1], 3).

pattern(computer, [_|_], 1).
pattern(are, [_,are,you,1], 1).
pattern(are, [_,are,i,1], 2).
pattern(are, [_,are,1], 3).
pattern(are,[_|_], 4).

:-dynamic toUse/3.
prepareScript():-
    %delete last used phraze memory from previous run
    retractall(toUse(_,_,_)),
    %initialize memory to start with the first variant of the response pattern
    assertz(toUse(remember,1,0)),
    assertz(toUse(remember,2,0)),
    assertz(toUse(remember,3, 0)).

%DATABASE OF RESPONSE PATTERNS CORRESPONDING TO KEYWORDS 
response(remember, 1, [
    [do,you,often,think,of,1,?],
    [does,thinking,of,1,bring,anything,else,to,mind,?],
    [what,else,do,you,remember,?],
    [why,do,you,remember,1,just,now,?],
    [what,in,the,present,situation,reminds,you,of,1,?],
    [what,is,the,connection,between,me,and,1,?]
] ).
response(remember, 2 ,[
    [did,you,think,'I',would,forget,1,?],
    [why,do,you,think,i,should,recall,1,now,?],
    [what,about,1,?],
    [you,mentioned,1,'.']
] ).
response(remember, 3,
    [[it,is,deffinitely, worth,remembering,tell,me,more]
] ).

%getResponse(+KeyWord, +Input, -Response)
%creates response sentence based on Input and alredy selected KeyWord from Input
getResponse(KeyWord,Input,Response):-
    %match particular pattern for given KeyWord
    pattern(KeyWord,Pattern, PatternNum),
    %fill KeyValList with values from Pattern
    match(Pattern, KeyValList,Input),
    %get ResponsePatternList by particular KeyWord and chosen pattern
    response(KeyWord, PatternNum, ResponsePatternList),
    %get index of response pattern from ResponsePatternList, that should be used right now
    toUse(KeyWord,PatternNum,LastUsedNum),
    %forget old num of response pattern to use
    retract(toUse(KeyWord, PatternNum,LastUsedNum)),
    %pick response pattern coresponding to number    
    nth0(LastUsedNum, ResponsePatternList, ResponsePattern),
    %count new response index to be used in next iteration
    length(ResponsePatternList, Len),
    AnotherNum is ((LastUsedNum+1) mod Len),
    %remember index 
    assertz(toUse(KeyWord, PatternNum, AnotherNum)),
    %create response from response pattern and KeyValList
    match(ResponsePattern,KeyValList,Response),!.


%to use

/*rules(remember,[_,you,remember,Y],
    [
		[do,you,often,think,of,Y,?],
		[does,thinking,of,Y,bring,anything,else,to,mind,?],
		[what,else,do,you,remember,?],
		[why,do,you,remember,Y,just,now,?],
		[what,in,the,present,situation,reminds,you,of,Y,?],
        [what,is,the,connection,between,me,and,Y,?]
    ]).
rules(remember, [_,do,i,remember,Y],
    [
		[did,you,think,'I',would,forget,Y,?],
		[why,do,you,think,i,should,recall,Y,now,?],
		[what,about,Y,?],
        [you,mentioned,Y,.]
    ]).
rules(remember,[_|_],
    [[it,is,deffinitely, worth,remembering,tell,me,more]]).

rules(computer, [_|_] ,
    [
		[do,computers,worry,you,?],
		[why,do,you,mention,computers,?],
		[what,do,you,think,machines,have,to,do,with,your,problem,?],
		[do,you,not,think,computers,can,help,people,?],
		[what,about,machines,worries,you,?],
        [what,do,you,think,about,machines,?]
    ]).

rules(are, [_,are,you,Y],
    [
		[do,you,believe,you,are,Y,?],
		[would,you,want,to,be,Y,?],
		[you,wish,'I',would,tell,you,you,are,Y,?],
        [what,would,it,mean,if,you,were,Y,?]
    ]).
rules(are, [_,are,i,Y],
    [
		[why,are,you,interested,in,whether,'I',am,Y,or,not,?],
		[would,you,prefer,if,'I',were,not,Y,?],
		[perhaps,'I',am,Y,in,your,fantasies,'.'],
        [do,you,sometimes,think,'I',am,Y,?]
    ]).
rules(are, [_,are,Y],
    [
    	[did,you,think,they,might,not,be,Y,?],
		[would,you,like,it,if,they,were,not,Y,?],
		[what,if,they,were,not,Y,?],
        [possibly,they,are,Y,'.']
    ]).
rules(are,[_],
    [
		[why,do,you,say,am,?],
        ['I',do,not,understand,that,'.']
    ]).
*/

match([Word|Pattern], KeyValList, [Word|WordList]):-
    atom(Word),
    match(Pattern, KeyValList, WordList).
match([N|Pattern], KeyValList, WordList):-
    integer(N),
    findKeyValue(N,KeyValList,Left),
    append(Left, Right, WordList),
    match(Pattern, KeyValList, Right).
match([],_,[]).
    

findKeyValue(Key, [Key-Value|_],Value). %found
findKeyValue(Key, [Key1-_|KeyValList], Value):-
    Key \= Key1, %not found yet
    findKeyValue(Key, KeyValList,Value).

%findMostImportantKeyWord(+WordList, BestKeyWordFoundSoFar, -Result)
%returns KeyWord with lowest priority (consults keyWord predicate) from WordList
%or none if no KeyWord is present
findMostImportantKeyWord([],Res,Res):- nonvar(Res),!. %return keyWord found - Res has been already unified
findMostImportantKeyWord([],_,none). %no key word present in WordList
%initialize BestSoFar by the first KeyWord found
findMostImportantKeyWord([Word|List], BestSoFar, Res):-
    var(BestSoFar),
    keyword(Word, _),
    findMostImportantKeyWord(List, Word, Res), !.
%better KeyWord found
findMostImportantKeyWord([Word|List], BestSoFar, Res):-
    keyword(Word, WordPriority),
    keyword(BestSoFar, BestPriority),
    WordPriority < BestPriority,
    findMostImportantKeyWord(List, Word, Res), !.
%just skip non-key word
findMostImportantKeyWord([_|List], BestSoFar, Res):-
    findMostImportantKeyWord(List, BestSoFar, Res).

printReply([H|T]):-
    write(H),
    write(' '),
    printReply(T).
printReply([]):- nl. 




testResponse(X,Y,Z,A,B,C,D,E):-
    prepareScript(),
    getResponse(remember,[do, you, remember, abraka], X),
    getResponse(remember,[do, you, remember, abraka], Y),
    getResponse(remember,[do, you, remember, abraka], Z),
    getResponse(remember,[do, you, remember, abraka], A),
    getResponse(remember,[do, you, remember, abraka], B),
    getResponse(remember,[do, you, remember, abraka], C),
    getResponse(remember,[do, you, remember, abraka], D),
    getResponse(remember,[do, you, remember, abraka], E).

testKeyWordLookUp(KeyW):-
    readSentence(Sen), 
    %wordListToLowerCase(Sen, LowerCaseList),
    findMostImportantKeyWord(Sen,_,KeyW ).

%testListLower(LowerCaseList):-
%    readSentence(Sen), wordListToLowerCase(Sen, LowerCaseList).




 
