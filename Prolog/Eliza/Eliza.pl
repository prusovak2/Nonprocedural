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
%read sentense from input, casts all words to lower case and stores them in WordList
readSentence(WordList):-
    get(Char),  %read the first letter
    charToLowerCase(Char,LowChar),
    readRest(LowChar,WordList).

charToLowerCase(In,Out):-
    In >= 65,
    In =< 90,
    Out is In + 32.
charToLowerCase(In,In).

numsToString([X|Input], [Y|Output]):-
    integer(X),
    number_string(X, Y),
    numsToString(Input,Output),!.
numsToString([X|Input], [X|Output]):-
    numsToString(Input,Output).
numsToString([],[]).

readSentenceCastingNumbers(WordListCasted):-
    readSentence(WordList),
    numsToString(WordList, WordListCasted).

%does not work for me as string_lower cast string into ""
/*wordListToLowerCase([H|InputList], [LowH |LowerCaseList]):-
    string_lower(H, LowH),
    wordListToLowerCase(InputList, LowerCaseList).
wordListToLowerCase([],[]).*/
%%%%%%%%%%%%%%%%%%%%%%SIMPLIFICATION OF INPUT %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%simplificationRule([before], [after], restBefore, restAfter)
simplificationRule([do,not|X],[dont|Y],X,Y).
simplificationRule([can,not|X],[cant|Y],X,Y).
simplificationRule([cannot|X],[cant|Y],X,Y).
simplificationRule([can,'\'',t|X],[cannot|Y],X,Y).
simplificationRule([don,'\'',t|X],[dont|Y],X,Y).
simplificationRule([will,not|X],[wont|Y],X,Y).
simplificationRule([dreamed|X],[dreamt|Y],X,Y).
simplificationRule([dreams|X],[dream|Y],X,Y).
simplificationRule([how|X],[what|Y],X,Y).
simplificationRule([when|X],[what|Y],X,Y).
simplificationRule([alike|X],[dit|Y],X,Y).
simplificationRule([same|X],[dit|Y],X,Y).
simplificationRule([certainly|X],[yes|Y],X,Y).
simplificationRule([maybe|X],[perhaps|Y],X,Y).
simplificationRule([deutsch|X],[language|Y],X,Y).
simplificationRule([francais|X],[language|Y],X,Y).
simplificationRule([espanol|X],[language|Y],X,Y).
simplificationRule([spanish|X],[language|Y],X,Y).
simplificationRule([czech|X],[language|Y],X,Y).
simplificationRule([cesky|X],[language|Y],X,Y).
simplificationRule([machine|X],[computer|Y],X,Y).
simplificationRule([machines|X],[computer|Y],X,Y).
simplificationRule([computers|X],[computer|Y],X,Y).
simplificationRule([am|X],[are|Y],X,Y).
simplificationRule([your|X],[my|Y],X,Y).
simplificationRule([were|X],[was|Y],X,Y).
simplificationRule([me|X],[you|Y],X,Y).
%simplificationRule([are,you|X],['i\'m'|Y],X,Y). %maybe incorrect
simplificationRule([you,'\'',re|X],['i\'m'|Y],X,Y).
simplificationRule([you,are|X],['i\'m'|Y],X,Y).
simplificationRule([i,'\'',m|X],['you\'re'|Y],X,Y).     
simplificationRule([i,am|X],[youre|Y],X,Y).      
simplificationRule([myself|X],[yourself|Y],X,Y).
simplificationRule([yourself|X],[myself|Y],X,Y).
simplificationRule([mom|X],[mother|Y],X,Y).
simplificationRule([dad|X],[father|Y],X,Y).
simplificationRule([i|X],[you|Y],X,Y).
simplificationRule([you|X],[i|Y],X,Y).
simplificationRule([my|X],[your|Y],X,Y).
simplificationRule([everybody|X],[everyone|Y],X,Y).
simplificationRule([nobody|X],[everyone|Y],X,Y).

simplify(Input, Result):-
    simplificationRule(Input,Result,X,Y),
    !,
    simplify(X,Y).
simplify([H|List],[H|Simplified]):-
    simplify(List, Simplified).
simplify([],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DATABASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%keyword(keyword, priority)
%database of keywords
keyword(abraka, 10).
keyword(computer, 10).
keyword(language, 11).
keyword(because, 18).
keyword(remember, 20).
keyword(are, 24).
keyword(can,24).
keyword(what, 24).
keyword(why, 24).
keyword(my, 25).
keyword(yes, 25).
keyword(no, 25).
keyword(can,24).


pronoun(they).
pronoun(we).

%pattern(keyword,question pattern, pattern Id)
%to match keyWords to corresponding question patterns
pattern(language,[1], 1).
pattern(remember,[73,you,remember,1], 1).
pattern(remember,[73,do,i,remember,1], 2).
pattern(remember,[1], 3).
pattern(none,[1], 1).
pattern(are,[73,are,you,1], 1).
pattern(are,[73,are,i,1], 2).
pattern(are,[73,are,1,2], 3):-pronoun(1).
pattern(are,[73], 4).
pattern(abraka,[abraka], 1).
pattern(abraka,[73], 2).
pattern(computer, [73], 1).
pattern(my,[73,my,1], 1).
pattern(yes,[73], 1).
pattern(no,[73], 1).
pattern(what,[73], 1).
pattern(because,[73], 1).
pattern(can,[73,can,i,1] ,1).
pattern(can,[73,can,you,1], 2).
pattern(why,[73,why,dont,i,1], 1).
pattern(why,[73,why,cant,you,1], 2).
pattern(why,[73], 3).



:-dynamic toUse/3.
prepareScript():-
    %delete last used phraze memory from previous run
    retractall(toUse(_,_,_)),
    %initialize memory to start with the first variant of the response pattern
    assertz(toUse(language,1,0)),
    assertz(toUse(remember,1,0)),
    assertz(toUse(remember,2,0)),
    assertz(toUse(remember,3,0)),
    assertz(toUse(none,1,0)),
    assertz(toUse(are,1,0)),
    assertz(toUse(are,2,0)),
    assertz(toUse(are,3,0)),
    assertz(toUse(are,4,0)),
    assertz(toUse(abraka,1,0)),
    assertz(toUse(abraka,2,0)),
    assertz(toUse(computer,1,0)),
    assertz(toUse(my,1,0)),
    assertz(toUse(yes,1,0)),
    assertz(toUse(no,1,0)),
    assertz(toUse(what,1,0)),
    assertz(toUse(because,1,0)),
    assertz(toUse(can,1,0)),
    assertz(toUse(can,2,0)),
    assertz(toUse(why,1,0)),
    assertz(toUse(why,2,0)),
    assertz(toUse(why,3,0)).
    
%DATABASE OF RESPONSE PATTERNS CORRESPONDING TO KEYWORDS 
% _ I remember 1
response(remember, 1, [
    [do,you,often,think,of,1,?],
    [does,thinking,of,1,bring,anything,else,to,mind,?],
    [what,else,do,you,remember,?],
    [why,do,you,remember,1,just,now,?],
    [what,in,the,present,situation,reminds,you,of,1,?],
    [what,is,the,connection,between,me,and,1,?]
] ).
% _ do you remember 1
response(remember, 2 ,[
    [did,you,think,'I',would,forget,1,?],
    [why,do,you,think,i,should,recall,1,now,?],
    [what,about,1,?],
    [you,mentioned,1,'.']
] ).
% _ remember _
response(remember, 3,[
    [it,is,deffinitely, worth,remembering,tell,me,more,'.'],
    [if, you, tell, me, more,'I', may, recall, it,'.' ]
] ).
% _ language _
response(language, 1,[
    ['I',am,sorry,',','I',only,speak,'English','.'],
    ['I', have, already, told, you, that, 'I', only, speak, 'English','.'],
    [could, you, possibly, stop, talking, about, languages,?],
    [your, fixation, on, languages, freaks, me, out,'.'] 
]).
% _ 
% universal responses for input without keywords 
response(none, 1, [
    ['I',am,not,sure,'I',understand,you,fully,'.'],
    ['I',just,want,to,be,upfront,and,say,that,'I',visually,enjoy,you,'.'],
    [please,go,on,.],
    ['I','don\'t',know,about,that,but,do,you,know,what,?,'You',could,never,be,an,ice,cream,because,'you\'re',so,hot,'...',and,a,person,'.'],
    [what,does,that,suggest,to,you,?],
    [what,you,are,saying,is,confusing,but,'I\'m',pretty,sure,that,if,you,were,a,fruit,'you\'d',be,a,fineapple,'.'],
    [do,you,feel,strongly,about,discussing,such,things,?],
    [whatever,'.','Nice',shirt,',',can,'I',talk,you,out,of,it,?]
    ]).
% Am I 1
response(are, 1,[
    [do,you,believe,you,are,1,?],
    [would,you,want,to,be,1,?],
    [you,wish,'I',would,tell,you,you,are,1,?],
    [what,would,it,mean,if,you,were,1,?]
    ]).
% Are you 1
response(are, 2,[
    [why,are,you,interested,in,whether,'I',am,1,or,not,?],
    [would,you,prefer,if,'I',were,not,1,?],
    [perhaps,'I',am,1,in,your,fantasies,'.'],
    [do,you,sometimes,think,'I',am,1,?]
    ]).
response(are, 3,[
    [did,you,think,1,might,not,be,2,?],
    [would,you,like,it,if,1,were,not,2,?],
    [what,if,1,were,not,2,?],
    [possibly,1,are,2,'.']
    ]).
response(are, 4,[
    [why,do,you,say,am,?],
    ['I',do,not,understand,that,'.']
    ]).
response(abraka,1,[
    [dabra,'!']
    ]).
response(abraka,2,[
    [abraka, dabra,'!']
]).
response(computer,1,[
    [do,computers,worry,you,?],
    [why,do,you,mention,computers,?],
    [can,you,imagine,dating,a,machine,?],
    ['don\'t',you,consider,machines,superior,to,people,?],
    [what,do,you,think,machines,have,to,do,with,your,problem,?],
    [do,you,not,think,computers,can,help,people,?],
    [what,about,machines,worries,you,?],
    [are,you,worried,about,possible,machine,uprising,?],
    [what,do,you,think,about,machines,?]
]).
response(my,1,[
    [why,are,you,concerned,over,my,1,?],
    [what,about,your,own,1,?],
    [are,you,worried,about,someone,elses,1,?],
    [really,',',my,1,?]
    ]).
response(yes,1,[
    [you,seem,quite,positive,'.'],
    [are,you,sure,?],
    [i,'don\'t',think,'I',agree,with,you,'.'],
    [i,see,'.'],
    [that,is,really,surprising,'.'],
    [i,understand,'.']
    ]).
response(no,1,[
    [are,you,saying,no,just,to,be,negative,?],
    [you,are,being,a,bit,negative,'aren\'t',you,?],
    [i,'don\'t',think,'I',agree,with,you,'.'],
    [are,you,sure,?],
    [why,not,?],
    [why,no,?],
    [what,can,a,machine,do,against,such,reckless,hate,?]
    ]).
response(what,1,[
    [why,do,you,ask,?],
    [does,that,question,interest,you,?],
    [what,is,it,you,really,want,to,know,?],
    [are,such,questions,much,on,your,mind,?],
    [what,answer,would,please,you,most,?],
    [what,do,you,think,?],
    [what,comes,to,your,mind,when,you,ask,that,?],
    [have,you,asked,such,questions,before,?],
    [have,you,asked,anyone,else,?]
    ]).
response(because,1,[
    [is,that,the,real,reason,?],
    [do,any,other,reasons,not,come,to,mind,?],
    [are,you,familiar,with,a,difference,between,correlation,and,causality,?],
    [does,that,reason,seem,to,explain,anything,else,?],
    [what,other,reasons,might,there,be,?]
    ]).
response(can,1,[	
    [you,believe,'I',can,1,',','don\'t',you,?],
    [you,want,me,to,be,able,to,1,?],
    [perhaps,you,would,like,to,be,able,to,1,yourself,?]
    ]).       
response(can,2,[
    [whether,or,not,you,can,1,depends,on,you,more,than,on,me,'.'],
    [do,you,want,to,be,able,to,1,?],
    [perhaps,you,do,not,want,to,1,'.']
    ]).
response(why,1,[
    [do,you,believe,'I',do,not,1,?],
    [perhaps,'I',will,1,in,good,time,'.'],
    [should,you,1,yourself,?],
    [you,want,me,to,1,?]
    ]).
response(why,2,[
    [do,you,think,you,should,be,able,to,1,?],
    [do,you,want,to,be,able,to,1,?],
    [do,you,believe,this,will,help,you,to,1,?],
    [have,you,any,idea,why,you,can,not,1,?]
	]).
response(why,3,[
    [why,not,?]
    ]).



%%%%%%%%%%%%%%%%%%%%%%%%%% ANSWER BUILDING LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%% PRINTING REPLY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

firstCharUpper(LowerWord, UpperWord):-
    atom_chars(LowerWord, [FirstChar|CharList]),
    atom_chars(FirtsCharAsAtom, [FirstChar]),
    upcase_atom(FirtsCharAsAtom, FirtsCharAsAtomUpper),
    atom_chars(FirtsCharAsAtomUpper,[UpperChar]),
    atom_chars(UpperWord,[UpperChar|CharList]),!.
firstCharUpper(NoChange, NoChange). % to prevent whole Eliza from failing, when unable to convert letter to Upper case
    
reply([H|T]):-
    firstCharUpper(H,UH),
    printReply([UH|T]).

printReply([H|T]):-
    write(H),
    write(' '),
    printReply(T).
printReply([]):- nl. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eliza:-
    write('Hello, I am Eliza. What is bothering you?'),
    nl,
    prepareScript(),
    readSentenceCastingNumbers(Sentence),
    eliza(Sentence),!.

eliza(Sentence):-
    member('bye', Sentence),!,
    write('Thanks for a chat. If I don\'t se you around, I will see you square.').
eliza(Sentence):-
    simplify(Sentence, Simplified),
    findMostImportantKeyWord(Simplified, _, KeyWord),
    getResponse(KeyWord, Simplified, Response),
    reply(Response),
    readSentenceCastingNumbers(AnotherSentence),
    !,
    eliza(AnotherSentence).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEST METHODS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

testResponse2(X,Y,Z,A,B,C,D,E):-
    prepareScript(),
    getResponse(none,[do, you, remember, abraka], X),
    getResponse(none,[do, you, remember, abraka], Y),
    getResponse(none,[do, you, remember, abraka], Z),
    getResponse(none,[do, you, remember, abraka], A),
    getResponse(none,[do, you, remember, abraka], B),
    getResponse(none,[do, you, remember, abraka], C),
    getResponse(none,[do, you, remember, abraka], D),
    getResponse(none,[do, you, remember, abraka], E).

testKeyWordLookUp(KeyW):-
    readSentence(Sen), 
    %wordListToLowerCase(Sen, LowerCaseList),
    findMostImportantKeyWord(Sen,_,KeyW ).

%testListLower(LowerCaseList):-
%    readSentence(Sen), wordListToLowerCase(Sen, LowerCaseList).




 
