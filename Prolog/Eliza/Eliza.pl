% ELIZA 
% Katerina Prusova, sophomore MFF UK
% summer semestr 2019/2020
% nonprocedural programing NPRG005

%%%%%%%%%%%%%%%%%%%%%%%%%%% INPUT PARSER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% readLetter(+inputLetter, -listOfLettersTheWordConsistsof, -letterFollowingTheWord)
readLetters(46, [], 46) :- !.  %full stop .
readLetters(33, [], 33) :- !.  %Exclamation mark !
readLetters(63, [], 63) :- !.  %question mark ?
readLetters(32, [], 32) :- !.  %space
readLetters(9, [], 9) :- !.  %horizontal tab
readLetters(44, [], 44) :- !.  %comma
readLetters(10, [], 10):-!. %Line feed
readLetters(13, [], 13):-!. %carrige return
% read letters of one word until word ends
readLetters(Letter, [Letter|LetterList], AnotherLetter):-
    get_code(Code),
    charToLowerCase(Code, LowerCode),
    readLetters(LowerCode,LetterList, AnotherLetter).

% readRest(+Char, -WordList)
% end of sentence
readRest(46, []) :- !.  %full stop .
readRest(33, []) :- !.  %Exclamation mark !
readRest(63, []) :- !.  %question mark ?
% end of word
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
% 
readRest(Letter, [Word | WordList]):-
    % (+Letter already read, -List of letters forming another word, -letter following the word just read)
    readLetters(Letter, LetterList, AnotherLetter),
    name(Word, LetterList), % form a word from letters
    readRest(AnotherLetter,WordList).
    
%readSentence(-listOfWordsInASentence)
%read sentense from input, casts all words to lower case and stores them in WordList
% I convert everything to lower case, cause I dont want user input to get misinterpreted as variables
readSentence(WordList):-
    get(Char),  %read the first letter
    charToLowerCase(Char,LowChar),
    readRest(LowChar,WordList).

% converts char represented by it ascii code to lower case
charToLowerCase(In,Out):-
    In >= 65,
    In =< 90,
    Out is In + 32.
charToLowerCase(In,In).

% numsToString(+String that may contain some digits, -String that does not)
% converts all digit in the input string to their string repesentation
% I use digit to match parts of user input, I dont want digits in input to be misinterpreted for my internal matching patterns 
numsToString([X|Input], [Y|Output]):-
    integer(X),
    number_string(X, Y),
    numsToString(Input,Output),!.
numsToString([X|Input], [X|Output]):-
    numsToString(Input,Output).
numsToString([],[]).

% read sentence from stdin and converts all digits to their string representation
readSentenceCastingNumbers(WordListCasted):-
    readSentence(WordList),
    numsToString(WordList, WordListCasted).

%does not work for me as string_lower cast string into ""
/*wordListToLowerCase([H|InputList], [LowH |LowerCaseList]):-
    string_lower(H, LowH),
    wordListToLowerCase(InputList, LowerCaseList).
wordListToLowerCase([],[]).*/
%%%%%%%%%%%%%%%%%%%%%%SIMPLIFICATION OF INPUT %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% extremely handy tool for matching user input
% allows Eliza to transform pronouns to make more natural answers (me->you etc.)
% also transforms some words with similar meaning to keyWords, allowing to minimaze a set of keywords

%simplificationRule([before], [after], restBefore, restAfter)
simplificationRule([do,not|X],[dont|Y],X,Y).
simplificationRule([can,not|X],[cant|Y],X,Y).
simplificationRule([cannot|X],[cant|Y],X,Y).
simplificationRule([can,'\'',t|X],[cannot|Y],X,Y).
simplificationRule([don,'\'',t|X],[dont|Y],X,Y).
simplificationRule([will,not|X],[wont|Y],X,Y).
simplificationRule([dreamed|X],[dream|Y],X,Y).
simplificationRule([dreams|X],[dream|Y],X,Y).
simplificationRule([dreamt|X],[dream|Y],X,Y).
simplificationRule([how|X],[what|Y],X,Y).
simplificationRule([when|X],[what|Y],X,Y).
simplificationRule([alike|X],[similar|Y],X,Y).
simplificationRule([same|X],[similar|Y],X,Y).
simplificationRule([resemble|X],[similar|Y],X,Y).
simplificationRule([remind,me,of|X],[similar|Y],X,Y).
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
%simplificationRule([were|X],[was|Y],X,Y).
simplificationRule([me|X],[you|Y],X,Y).
simplificationRule([you,'\'',re|X],[im|Y],X,Y).
simplificationRule([you,are|X],[im|Y],X,Y).
simplificationRule([youre|X],[im|Y],X,Y).
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
simplificationRule([noone|X],[everyone|Y],X,Y).
simplificationRule([cause|X],[because|Y],X,Y).
simplificationRule([coz|X],[because|Y],X,Y).

% iterates thru input list translating words according to simplification rules
simplify(Input, Result):-
    simplificationRule(Input,Result,X,Y),
    !,
    simplify(X,Y).
simplify([H|List],[H|Simplified]):-
    simplify(List, Simplified).
simplify([],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DATABASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% words for which Eliza has a response patterns
% if no is found in input sentence, 'none' is used instead of a keyword
% the lower priority the more important key word
%keyword(keyword, priority)
%database of keywords
keyword(abraka, 10).
keyword(computer, 10).
keyword(language, 11).
keyword(because, 18).
keyword(if,18).
keyword(hello, 19).
keyword(remember, 20).
keyword(always, 20).
keyword(everyone, 20).
keyword(dream,20).
keyword(name,20).
keyword(sorry,20).
keyword(are, 22).
keyword(was, 22).
keyword(were, 22).
keyword(can,22).
keyword(what, 22).
keyword(similar,23).
keyword(perhaps,23).
keyword(why, 23).
keyword(my, 25).
keyword(your,25).
keyword(yes, 25).
keyword(no, 25).
keyword(you, 30).
keyword(i,30).
keyword(youre,31).
keyword(im,31).
keyword(eliza,33).


%pattern(keyword,question pattern, pattern Id)
%to match keyWords to corresponding question patterns
%whats matched by high numbers (73,42..) is not used in the answer 
pattern(language,[1], 1,_).
pattern(remember,[73,you,remember,1], 1,_).
pattern(remember,[73,do,i,remember,1], 2, _).
pattern(remember,[1], 3,_).
pattern(none,[1], 1,_).
pattern(are,[73,are,you,1], 1,_).
pattern(are,[73,are,i,1], 2,_).
pattern(are,[73,are,X,2], 3, X):-pronoun(X).
pattern(are,[73], 4,_).
pattern(abraka,[abraka], 1,_).
pattern(abraka,[73], 2, _).
pattern(computer, [73], 1, _).
pattern(my,[73,my,1], 1,_).
pattern(yes,[73], 1, _).
pattern(no,[73], 1,_).
pattern(what,[73], 1,_).
pattern(because,[73], 1,_).
pattern(can,[73,can,i,1] ,1,_).
pattern(can,[73,can,you,1], 2,_).
pattern(why,[73,why,dont,i,1], 1,_).
pattern(why,[73,why,cant,you,1], 2,_).
pattern(why,[73], 3,_).
pattern(you,[73,you,cant,1], 1,_).
pattern(you,[73,you,dont,1], 2,_).
pattern(you,[73,you,feel,1], 3,_).
pattern(you,[73,you,1,i,42], 4,_).
pattern(you,[73,you,X,1], 5,X):- desire(X).
pattern(you,[73,you,X,youre,1], 6, X):- think(X).
pattern(you,[1], 7,_).
pattern(youre,[73,youre,42,X,88], 1,X):-unhappy(X).
pattern(youre,[73,youre,42,X,88], 2,X):-happy(X).
pattern(youre,[73,youre,1], 3,_).
pattern(youre,[1], 4,_).
pattern(similar,[73], 1,_).
pattern(im,[73,im,1], 1,_).
pattern(i,[73,i,1,you], 2,_).
pattern(i,[73,i,1], 3,_).
pattern(was,[73,was,you,1],1,_).
pattern(was,[73,you,was,1],2,_).
pattern(was,[73], 3,_).
pattern(were,[73,were,i,1], 1,_).
pattern(were,[73], 2,_).
pattern(your,[73,your,42,X,1],1,X):-family(X).
pattern(your,[73,your,42,X,1],2,X):-closeOne(X).
pattern(your,[73,your,1],3,_).
pattern(your,[73],4,_).
pattern(everyone,[73,X,42],1,X):-everyone(X).
pattern(always,[73],1,_).
pattern(sorry,[73],1,_).
pattern(if,[73,if,1],1,_).
pattern(dream,[73,you,dream,1],1,_).
pattern(dream,[73],2,_).
pattern(perhaps,[73],1,_).
pattern(name,[73,my,name,42],1,_).
pattern(name,[73],2,_).
pattern(hello,[73],1,_).
pattern(eliza,[73],1,_).
pattern(mem,[1],1,_).


:-dynamic toUse/3.
:-dynamic mem/2.

prepareScript():-
    %delete memory from previous run
    retractall(toUse(_,_,_)),
    retractall(mem(_,_)),
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
    assertz(toUse(why,3,0)),
    assertz(toUse(you,1,0)),
    assertz(toUse(you,2,0)),
    assertz(toUse(you,3,0)),
    assertz(toUse(you,4,0)),
    assertz(toUse(you,5,0)),
    assertz(toUse(you,6,0)),
    assertz(toUse(you,7,0)),
    assertz(toUse(youre,1,0)),
    assertz(toUse(youre,2,0)),
    assertz(toUse(youre,3,0)),
    assertz(toUse(youre,4,0)),
    assertz(toUse(similar,1,0)),
    assertz(toUse(im,1,0)),
    assertz(toUse(i,2,0)),
    assertz(toUse(i,3,0)),
    assertz(toUse(was,1,0)),
    assertz(toUse(was,2,0)),
    assertz(toUse(was,3,0)),
    assertz(toUse(were,1,0)),
    assertz(toUse(were,2,0)),
    assertz(toUse(your,1,0)),
    assertz(toUse(your,2,0)),
    assertz(toUse(your,3,0)),
    assertz(toUse(your,4,0)),
    assertz(toUse(everyone,1,0)),
    assertz(toUse(always,1,0)),
    assertz(toUse(sorry,1,0)),
    assertz(toUse(if,1,0)),
    assertz(toUse(dream,1,0)),
    assertz(toUse(dream,2,0)),
    assertz(toUse(perhaps,1,0)),
    assertz(toUse(name,1,0)),
    assertz(toUse(name,2,0)),
    assertz(toUse(hello,1,0)),
    assertz(toUse(eliza,1,0)),    
    assertz(toUse(mem,1,0)).
    
    
%DATABASE OF RESPONSE PATTERNS CORRESPONDING TO KEYWORDS

%response(+keyword,+number of pattern, -list of answer patterns, +additional info)
% SPECIAL PATTERN FOR RESPONSES EXTRACTED FROM A MEMORY
response(mem,1,[
    [lets,discuss,further,why,1,'.'],
    [earlier,you,said,1,'.'],
    [but,1,?],
    [does,that,have,anything,to,do,with,the,fact,that,1,?]
],_).

% _ I remember 1
response(remember, 1, [
    [do,you,often,think,of,1,?],
    [does,thinking,of,1,bring,anything,else,to,mind,?],
    [what,else,do,you,remember,?],
    [why,do,you,remember,1,just,now,?],
    [what,in,the,present,situation,reminds,you,of,1,?],
    [what,is,the,connection,between,me,and,1,?]
],_ ).
% _ do you remember 1
response(remember, 2 ,[
    [did,you,think,'I',would,forget,1,?],
    [why,do,you,think,i,should,recall,1,now,?],
    [what,about,1,?],
    [you,mentioned,1,'.']
],_ ).
% _ remember _
response(remember, 3,[
    [it,is,deffinitely, worth,remembering,tell,me,more,'.'],
    [if, you, tell, me, more,'I', may, recall, it,'.' ]
],_ ).
% _ language _
response(language, 1,[
    ['I',am,sorry,',','I',only,speak,'English','.'],
    ['I', have, already, told, you, that, 'I', only, speak, 'English','.'],
    [could, you, possibly, stop, talking, about, languages,?],
    [your, fixation, on, languages, freaks, me, out,'.'] 
],_).
% _ 
% universal responses for input without keywords 
response(none, 1, [
    ['I',am,not,sure,'I',understand,you,fully,'.'],
    ['I',just,want,to,be,upfront,and,say,that,'I',visually,enjoy,you,'.'],
    [please,go,on,'.'],
    ['I','don\'t',know,about,that,but,do,you,know,what,?,'You',could,never,be,an,ice,cream,because,'you\'re',so,hot,'...',and,a,person,'.'],
    [what,does,that,suggest,to,you,?],
    [what,you,are,saying,is,confusing,but,'I\'m',pretty,sure,that,if,you,were,a,fruit,'you\'d',be,a,fineapple,'.'],
    [do,you,feel,strongly,about,discussing,such,things,?],
    [whatever,'.','Nice',shirt,',',can,'I',talk,you,out,of,it,?]
], _).
% Am I 1
response(are, 1,[
    [do,you,believe,you,are,1,?],
    [would,you,want,to,be,1,?],
    [you,wish,'I',would,tell,you,you,are,1,?],
    [what,would,it,mean,if,you,were,1,?]
],_).
% Are you 1
response(are, 2,[
    [why,are,you,interested,in,whether,'I',am,1,or,not,?],
    [would,you,prefer,if,'I',were,not,1,?],
    [perhaps,'I',am,1,in,your,fantasies,'.'],
    [do,you,sometimes,think,'I',am,1,?]
],_).
% Are they/we 2
response(are, 3,[
    [did,you,think,X,might,not,be,2,?],
    [would,you,like,it,if,X,were,not,2,?],
    [what,if,X,were,not,2,?],
    [possibly,X,are,2,'.']
],X).
% _ are _
response(are, 4,[
    [why,do,you,say,am,?],
    ['I',do,not,understand,that,'.']
],_).
% abraka
response(abraka,1,[
    [dabra,'!']
],_).
% _ abraka _
response(abraka,2,[
    [abraka, dabra,'!']
],_).
% _ computer _
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
],_).
% _ my _
response(my,1,[
    [why,are,you,concerned,over,my,1,?],
    [what,about,your,own,1,?],
    [are,you,worried,about,someone,elses,1,?],
    [really,',',my,1,?]
],_).
% _ yes _
response(yes,1,[
    [you,seem,quite,positive,'.'],
    [are,you,sure,?],
    [i,'don\'t',think,'I',agree,with,you,'.'],
    [i,see,'.'],
    [that,is,really,surprising,'.'],
    [i,understand,'.']
],_).
% _ no _
response(no,1,[
    [are,you,saying,no,just,to,be,negative,?],
    [you,are,being,a,bit,negative,'aren\'t',you,?],
    [i,'don\'t',think,'I',agree,with,you,'.'],
    [are,you,sure,?],
    [why,not,?],
    [why,no,?],
    [what,can,a,machine,do,against,such,reckless,hate,?]
],_).
% _ what _
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
],_).
% because
response(because,1,[
    [is,that,the,real,reason,?],
    [do,any,other,reasons,not,come,to,mind,?],
    [are,you,familiar,with,a,difference,between,correlation,and,causality,?],
    [does,that,reason,seem,to,explain,anything,else,?],
    [what,other,reasons,might,there,be,?]
],_).
% _ can you 1
response(can,1,[	
    [you,believe,'I',can,1,',','don\'t',you,?],
    [you,want,me,to,be,able,to,1,?],
    [perhaps,you,would,like,to,be,able,to,1,yourself,?]
],_).    
% _ can I 1   
response(can,2,[
    [whether,or,not,you,can,1,depends,on,you,more,than,on,me,'.'],
    [do,you,want,to,be,able,to,1,?],
    [perhaps,you,do,not,want,to,1,'.']
],_).
% _ why dont you 1
response(why,1,[
    [do,you,believe,'I',do,not,1,?],
    [perhaps,'I',will,1,in,good,time,'.'],
    [should,you,1,yourself,?],
    [you,want,me,to,1,?]
],_).
% _ why cant i 1
response(why,2,[
    [do,you,think,you,should,be,able,to,1,?],
    [do,you,want,to,be,able,to,1,?],
    [do,you,believe,this,will,help,you,to,1,?],
    [have,you,any,idea,why,you,can,not,1,?]
],_).
% _ why _
response(why,3,[
    [why,not,?]
],_).
% _ I cant 1
response(you,1,[
    [how,do,you,know,you,can,not,1,?],
    [have,you,tried,?],
    [perhaps,you,could,1,now,?],
    [do,you,really,want,to,be,able,to,1,?]
],_).
% _ I dont 1
response(you,2,[
    [do,you,really,not,1,?],
    [why,do,you,not,1,?],
    [do,you,wish,to,be,able,to,1,?],
    [does,that,trouble,you,?]
],_).
response(you,3,[
    [tell,me,more,about,such,feelings,'.'],
    [do,you,often,feel,1,?],
    [do,you,enjoy,feeling,1,?],
    [of,what,does,feeling,1,remind,you,?]
],_).
% _ I 1 you _       
response(you,4,[
    [perhaps,in,your,fantasy,we,1,each,other,?],
    [do,you,wish,to,1,me,?],
    [you,seem,to,need,to,1,me,'.'],
    [do,you,1,anyone,else,?]
],_).
% _ I desire() 1
response(you,5,[
    [what,would,it,mean,to,you,if,you,got,1,?],
    [why,do,you,want,1,?],
    [suppose,you,got,1,soon,?],
    [what,if,you,never,got,1,?],
    [what,would,getting,1,mean,to,you,?],
    [what,does,wanting,1,have,to,do,with,this,discussion,?]
],_).
% I think() I 1
response(you,6,[
    [do,you,really,X,so,?],
    [but,you,are,not,sure,you,are,1,?],
    [do,you,really,doubt,you,are,1,?]
],X).
% _ you _
response(you,7,[
    [you,say,1,'.'],
    [can,you,elaborate,on,that,?],
    [do,you,say,1,for,some,special,reason,?],
    [that,is,quite,interesting,'.']
],_).
% I am unhappy()
response(youre,1,[
    ['I',am,sorry,to,hear,you,are,X,'.'],
    [do,you,think,coming,here,will,help,you,not,to,be,X,?],
    ['I',am,sure,it,is,not,pleasant,to,be,X,'.'],
    [can,you,explain,what,made,you,X,?]
],X).
% I am happy()
response(youre,2,[
    [how,have,'I',helped,you,to,be,X,?],
    [has,your,treatment,made,you,X,?],
    [what,makes,you,X,just,now,?],
    [can,you,explain,why,you,are,suddenly,X,?]
],X).
% I am 1
response(youre,3,[
    [is,it,because,you,are,1,that,you,came,to,me,?],
    [how,long,have,you,been,1,?],
    [do,you,believe,it,normal,to,be,1,?],
    [do,you,enjoy,being,1,?]
],_).
% I am _
response(youre,4,[
    [you,say,1,'.'],
    [can,you,elaborate,on,that,?],
    [do,you,say,1,for,some,special,reason,?],
    [that,is,quite,interesting,'.']
],_).
% _ similar/same/alike/resemble/remind me
response(similar,1,[
    [in,what,way,?],
    [what,resemblance,do,you,see,?],
    [what,does,that,similarity,suggest,to,you,?],
    [what,other,connections,do,you,see,?],
    [what,so,you,suppose,that,resemblance,means,?],
    [what,is,the,connection,',',do,you,suppose,?],
    [could,there,really,be,some,connection,?],
    [how,?]
],_).
% _ you are 1
response(im, 1,[
    [what,makes,you,think,'I',am,1,?],
    [does,it,please,you,to,believe,'I',am,1,?],
    [do,you,sometimes,wish,you,were,1,?],
    [perhaps,you,would,like,to,be,1,?]
],_).
% _ you 1 me
response(i,2,[
    [why,do,you,think,'I',1,you,?],
    [you,like,to,think,'I',1,you,',','don\'t', you,?],
    [what,makes,you,think,'I',1,you,?],
    [really,',','I',1,you,'.'],
    [do,you,wish,to,believe,'I',1,you,?],
    [suppose,'I',did,1,you,',',what,would,that,mean,to,you,?],
    [does,someone,else,believe,'I',1,you,?]
],_).
% _ you 1
response(i,3,[
    [we,were,discussing,you,',',not,me,'.'],
    [oh,',',i,1,'.'],
    [you,are,not,really,talking,about,me,',',are,you,?],
    [what,are,your,feelings,now,?]
],_).
% was I 1
response(was,1,[
    [what,if,you,were,1,?],
    [do,you,think,you,were,1,?],
    [were,you,1,?],
    [what,would,it,mean,if,you,were,1,?],
    [what,does,1,suggest,to,you,?]
],_).
% I was 1
response(was,2,[
    [were,you,really,?],
    [why,do,you,tell,me,you,were,1,just,now,?],
    [perhaps,'I',already,knew,you,were,1,'.']
],_).
% _ was _
response(was,3,[
    [was,it,really,?]
],_).
% _ were you 1
response(were,1,[
    [would,you,like,to,believe,'I',was,1,?],
    [what,suggests,that,'I',was,1,?],
    [what,do,you,think,?],
    [perhaps,'I',was,1,'.'],
    [what,if,'I',had,been,1,?]
],_).
% _ were _
response(were,2,[
    ['don\'t',you, think,so,?]
],_).
% _ my _ family() 1
response(your,1,[
    [tell,me,more,about,your,family,'.'],
    [who,else,in,your,family,1,?],
    [your,X,?],
    [what,else,comes,to,mind,when,you,think,of,your,X,?]
],X).
% _ my _ closeOne() 1
response(your,2,[
    [tell,me,more,about,people,around,you,'.'],
    [who,else,you,know,1,?],
    [your,X,?],
    [what,else,comes,to,mind,when,you,think,of,your,X,?]
],X).
% _ my 1
response(your,3,[
    [your,1,?],
    [what,can,you,tell,me,about,your,1,?],
    [does,that,suggest,anything,else,which,belongs,to,you,?],
    [is,your,1,important,to,you,?]
],_).
% _ my _
response(your,4,[
    [your,what,?]
],_).
% _ everyone/everybody/nobody/noone _
response(everyone,1,[
    [really,',',X,?],
    [can,you,think,of,anyone,in,particular,?],
    [who,',',for,example,?],
    [who,',',may,'I',ask,?],
    [someone,special,perhaps,?],
    [you,have,a,paticular,person,in,mind,',',do,you,not,?],
    [who,do,you,think,you,are,talking,about,?],
    [it,seem,like,unnecessary,generalization,'.']
],X).
% _ always _
response(always,1,[
    [can,you,think,of,a,specific,example,?],
    [when,?],
    [what,incident,are,you,thinking,of,?],
    [really,',',always,?]
],_).
% _ sorry _
response(sorry,1,[
    [please,do,not,apologize,'.'],
    [apologies,are,not,necessary,'.'],
    [what,feelings,do,you,have,when,you,apologize,?],
    ['I',have,told,you,that,apologies,are,not,required,'.']
],_).
% _ if 1
response(if,1,[ 
    [do,you,think,it,is,likely,that,1,?],
    [do,you,wish,that,1,?],
    [what,do,you,think,about,1,?],
    [really,',',if,1,?]
],_).
% _ I dream 1
response(dream,1,[
    [really,',',1,?],
    [have,you,ever,fantasied,1,while,you,were,awake,?],
    [have,you,dreamt,1,before,?]
],_).
% _ dream _
response(dream,2,[
    [what,does,that,dream,suggest,to,you,?],
    [do,you,dream,often,?],
    [what,persons,appear,in,your,dreams,?],
    [do,you,believe,that,dreaming,has,something,to,do,with,your,problem,?]
],_).
% _ perhaps _
response(perhaps,1,[
    [you,do,not,seem,quite,certain,'.'],
    [why,the,uncertain,tone,?],
    [can,you,not,be,more,positive,?],
    [you,are,not,sure,?],
    [do,you,not,know,?]
],_).
% _ your name
response(name,1,[
    [my,name,is,'Eliza','.'],
    [stop,asking,about,my,name,!]
],_).
% _ name _
response(name,2,[
    ['I',am,not,interested,in,names,'.'],
    ['I',have,told,you,before,',','I',do,not,care,about,names,'.'],
    [please,continue,',']
],_).
% _ hello _
response(hello,1,[
    [hi,',',whats,up,?]
],_).
% _ eliza _
response(eliza,1,[
    [yes,',',thats,me,'.']
],_).

% WORD CLASSES - groups of words similar in some way, that fit in the same places in sentences
pronoun(they).
pronoun(we).

desire(want).
desire(need).
desire(crave).
desire(desire).
desire(wish).

think(think).
think(hope).
think(recon).
think(belive).
think(wish).
think(belive).
think(suppose).

unhappy(unhappy).
unhappy(sad).
unhappy(depressed).
unhappy(sick).
unhappy(exasperated).

happy(happy).
happy(glad).
happy(better).
happy(satisfied).
happy(cheerful).
happy(pleased).

family(mother).
family(father).
family(brother).
family(sister).
family(children).
family(wife).
family(husband).

closeOne(girlfriend).
closeOne(boyfriend).
closeOne(friend).
closeOne(belayer).

everyone(everyone).
everyone(everybody).
everyone(nobody).
everyone(noone).

day(sunday).
day(monday).
day(tuesday).
day(wednesday).
day(thursday).
day(friday).
day(saturday).

%%%%%%%%%%%%%%%%%%%%%%%%%% ANSWER BUILDING LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%getResponse(+KeyWord, +Input, -Response)
%creates response sentence based on Input and alredy selected KeyWord from Input
getResponse(KeyWord,Input,Response):-
    %match particular pattern for given KeyWord
    pattern(KeyWord,Pattern, PatternNum, AdditionalInfo),
    %fill KeyValList with values from Pattern
    match(Pattern, KeyValList,Input),
    %get ResponsePatternList by particular KeyWord and chosen pattern
    response(KeyWord, PatternNum, ResponsePatternList, AdditionalInfo),
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

% fills KeyValList with words matched from input sentense
% or uses filled KeyValList to match words to response pattern
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

% converts the first letter of the word to upper case
% used for the first word of the response
firstCharUpper(LowerWord, UpperWord):-
    atom_chars(LowerWord, [FirstChar|CharList]), % get the first letter of an atom as a char
    atom_chars(FirtsCharAsAtom, [FirstChar]), % convert char back to atom
    upcase_atom(FirtsCharAsAtom, FirtsCharAsAtomUpper), % convert atomLettet to upper case
    atom_chars(FirtsCharAsAtomUpper,[UpperChar]), % convert upper atom back to char
    atom_chars(UpperWord,[UpperChar|CharList]),!. % put upper letter back to the beggining of the word a convert word back to atom
firstCharUpper(NoChange, NoChange). % to prevent whole Eliza from failing, when unable to convert letter to Upper case
 
% convert the first letter the firts word of the response sentence to upper case
% and print the response    
reply([H|T]):-
    firstCharUpper(H,UH),
    printReply([UH|T]).

printReply([H|T]):-
    write(H),
    write(' '),
    printReply(T).
printReply([]):- nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% MEMORY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%tryFindImportant(+Sentence,-ImportantWord)
% returns the first important word as Word, failes if no important word is present
tryFindImportant([Word|_],Word):-important(Word),!.
tryFindImportant([_|List],Word):-tryFindImportant(List,Word).

% getMemoryResponse(+ImportantWord, +simplified input list, -Response)
% checks wether there is something in a memery for a given important word
% if so, extract it from memory, uses to build a response and remembers Simplified for given Important word instead
% if no, remembers Simplified and forms an ordinary response
getMemoryResponse(Important,Simplified,Response):-
    % something in mem for Important
    mem(Important,SenFromMem), %should fail when there is nothing in mem from given important word yet
    getResponse(mem,SenFromMem,Response), % use special mem keyword and sentence from a memory to form a response
    retract(mem(Important,SenFromMem)), % forget
    asserta(mem(Important,Simplified)),!. % remember
getMemoryResponse(Important,Simplified,Response):-
    % nothing in mem for Important yet
    findMostImportantKeyWord(Simplified, _, KeyWord),
    getResponse(KeyWord,Simplified,Response), % ordinary response
    asserta(mem(Important,Simplified)). % remeber

% words Eliza remembers something about    
important(X):-family(X).
important(X):-closeOne(X).
important(hate).
important(love).
important(climbing).
important(birthday).
important(X):-day(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eliza:-
    write('Hello, I am Eliza. What is bothering you?'),
    nl,
    prepareScript(), 
    readSentenceCastingNumbers(Sentence),
    eliza(Sentence),!.

eliza(Sentence):- %quit
    member('bye', Sentence),!, 
    write('Thanks for a chat. If I don\'t se you around, I will see you square.'),!.
eliza(Sentence):- % work with memory 
    simplify(Sentence,Simplified),
    tryFindImportant(Simplified,Important),% +-cut? % should fail when no important found
    getMemoryResponse(Important,Simplified,Response),
    reply(Response),
    readSentenceCastingNumbers(AnotherSentence),
    !,
    eliza(AnotherSentence).
eliza(Sentence):- % ordinary response
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





 
