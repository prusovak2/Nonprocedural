%substitute(inputList,toSubstitude, subtitudeWith, outputList)

substitute([],_,_,[]).
substitute([Co|Sez],Co,Cim,[Cim|Sez]):-!.
substitute([X|Sez],Co,Cim,[X|Vys]):-substitute(Sez, Co,Cim, Vys).

