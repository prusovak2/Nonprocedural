%substitute(inputList,toSubstitute, subtituteWith, outputList)

substitute([],_,_,[]).
substitute([Co|Sez],Co,Cim,[Cim|Sez]).
substitute([X|Sez],Co,Cim,[X|Vys]):-X\==Co,substitute(Sez, Co,Cim, Vys).

%substitute([],_,_,[]).
%substitute([Co,Sez],Co, Cim,[Cim|Vys]):-substitute(Sez,Co,Cim,Vys).
%substitute([X|Sez], Co, Cim, [X|Vys]):- dif(X,Co),substitute(Sez,
% Co,Cim,Vys).

%substitute([],_,_,[]):-!.
%substitute([Co|Sez],Co,Cim,[Cim|Vys]):-substitute(Sez, Co,Cim, Vys).



