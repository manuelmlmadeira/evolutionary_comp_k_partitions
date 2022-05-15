(* ::Package:: *)

BeginPackage["Popula\[CCedilla]\[ATilde]o`"]
Needs["Indiv\[IAcute]duos`"]

Popula\[CCedilla]\[ATilde]o::usage="Pacote que disponibiliza opera\[CCedilla]\[OTilde]es sobre popula\[CCedilla]\[OTilde]es."

novaPop::usage="novaPop denota a popula\[CCedilla]\[ATilde]o vazia."

popvaziaQ::usage="popvaziaQ[pop] recebe uma popula\[CCedilla]\[ATilde]o e devolve True se a popula\[CCedilla]\[ATilde]o estiver vazia e False caso contr\[AAcute]rio."

adInd::usage="adInd[pop,ind] recebe uma popula\[CCedilla]\[ATilde]o e um indiv\[IAcute]duo e adiciona-o \[AGrave] popula\[CCedilla]\[ATilde]o."

apagaInd::usage="apagaInd[pop,ind] recebe uma popula\[CCedilla]\[ATilde]o e um indiv\[IAcute]duo e devolve a popula\[CCedilla]\[ATilde]o resultante de o remover da mesma."

SelRandomInd::usage="SelRandomInd[pop] recebe uma popula\[CCedilla]\[ATilde]o e seleciona um elemento aleat\[OAcute]rio dessa popula\[CCedilla]\[ATilde]o."

SelDetInd::usage="SelDetInd[pop,ID] recebe uma popula\[CCedilla]\[ATilde]o e uma identifica\[CCedilla]\[ATilde]o e devolve o elemento dessa popula\[CCedilla]\[ATilde]o com identifica\[CCedilla]\[ATilde]o igual a ID"

dimPop::usage="dimPop[pop] recebe uma popula\[CCedilla]\[ATilde]o e devolve a dimens\[ATilde]o dessa popula\[CCedilla]\[ATilde]o."

geraPop::usage="geraPop[x,ID,Lista,k,T] recebe um n\[UAcute]mero natural x, um identificador inicial (ID), uma lista (Lista), um n\[UAcute]mero relativo ao n\[UAcute]mero de parti\[CCedilla]\[OTilde]es pretendido (k) e um \[IAcute]ndice de tempo (T) e devolve uma popula\[CCedilla]\[ATilde]o com x indiv\[IAcute]duos."

posi\[CCedilla]aonapop::usage="posi\[CCedilla]aonapop[pop,i] recebe uma popula\[CCedilla]\[ATilde]o e uma posi\[CCedilla]\[ATilde]o e devolve o indiv\[IAcute]duo que se encontra nessa mesma posi\[CCedilla]\[ATilde]o"

MelhorIndiv\[IAcute]duo::usage="MelhorIndiv\[IAcute]duo[pop, mincia,Lista,k] recebe uma popula\[CCedilla]\[ATilde]o de indiv\[IAcute]duos (pop), um n\[UAcute]mero (mincia), uma lista (Lista) e um n\[UAcute]mero correspondente ao n\[UAcute]mero de parti\[CCedilla]\[OTilde]es dessa lista pretendidas (k) e devolve uma lista com os indiv\[IAcute]duos da popula\[CCedilla]\[ATilde]o recebida cujo coeficiente de inadapta\[CCedilla]\[ATilde]o \[EAcute] igual mincia."


Begin["`Private`"]

novaPop={};

popvaziaQ=Function[pop,pop=={}];

adInd=Function[{pop,ind},Append[pop,ind]];

apagaInd=Function[{pop,ind},Delete[pop,First[Position[pop,ind]]]];

SelRandomInd=Function[pop,RandomChoice[pop]];

SelDetInd=Function[{pop,ID},Module[{ind,i},
i=1;
ind={};
While[i<=Length[pop]&&ind=={},
If[First[pop[[i]]]==ID,ind=pop[[i]],i=i+1;]];
ind
]];

dimPop=Function[pop,Length[pop]];

geraPop = Function[{x,ID,Lista,k,T},
Table[geraIndal[ID + i, Lista, k, T], {i, x}]];

posi\[CCedilla]aonapop = Function[{pop, i}, pop[[i]]];

MelhorIndiv\[IAcute]duo=Function[{pop, mincia,Lista,k}, Module[{i, v,ind,comp},
i = 1;
v = {};
comp=dimPop[pop];
While[i<=comp,
ind=pop[[i]];If[CIA[ind,Lista,k]==mincia,v=Append[v,pop[[i]]]];i = i + 1];
v]];




End[]

EndPackage[]



