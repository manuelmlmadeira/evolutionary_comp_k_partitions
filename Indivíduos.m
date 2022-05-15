(* ::Package:: *)

BeginPackage["Indiv\[IAcute]duos`"]
Needs["Parti\[CCedilla]\[ATilde]o`"]

Indiv\[IAcute]duos::usage="Pacote que disponibiliza opera\[CCedilla]\[OTilde]es sobre indiv\[IAcute]duos."

geraInd::usage="geraInd[ID,part,BP,TempoBP] recebe uma identifica\[CCedilla]\[ATilde]o (ID), uma parti\[CCedilla]\[ATilde]o(part), uma lista com os seus blocos perfeitos (BP) e uma lista com os respetivos tempos em que os BP se formaram (TempoBP) e devolve um indiv\[IAcute]duo com essas caracter\[IAcute]sticas."

geraIndal::usage="geraIndal[q,w,m,t] recebe uma identifica\[CCedilla]\[ATilde]o (q), uma lista (w), um n\[UAcute]mero relativo ao quantidade de parti\[CCedilla]\[OTilde]es pretendidas (m) e um tempo e gera um ind\[IAcute]viduo com um parti\[CCedilla]\[ATilde]o aleat\[OAcute]ria."

IndID::usage="IndID[ind] recebe um indiv\[IAcute]duo (ind) e devolve a sua identifica\[CCedilla]\[ATilde]o (ID)."

IndPart::usage="IndPart[ind] recebe um indiv\[IAcute]duo (ind) e devolve a parti\[CCedilla]\[ATilde]o ao qual est\[AAcute] associado."

IndBP::usage="IndBP[ind]recebe um indiv\[IAcute]duo (ind) e devolve a lista dos seus blocos perfeitos."

IndTempoBP::usage="IndTemposBP[ind] recebe um indiv\[IAcute]duo (ind) e devolve a lista dos instantes em que os seus blocos perfeitos se formaram."

NovoBP::usage="NovoBP[ind,novoB,tempoB] recebe um indiv\[IAcute]duo (ind), um novo bloco perfeito (novoB) e o instante da forma\[CCedilla]\[ATilde]o desse bloco e devolve o mesmo indiv\[IAcute]duo com o novo bloco e o instante de forma\[CCedilla]\[ATilde]o nas respetivas posi\[CCedilla]\[OTilde]es."

AlteraPart::usage="AlteraPart[ind,novaPart] recebe um indiv\[IAcute]duo (ind) e uma nova parti\[CCedilla]\[ATilde]o (novaPart) e substitui a parti\[CCedilla]\[ATilde]o que estava inicialmente associada ao indiv\[IAcute]duo por novaPart."

CIA::usage="CIA[ind, Lista, k] recebe um indiv\[IAcute]duo (ind) e calcula o seu coeficiente de inadpta\[CCedilla]\[ATilde]o, em rela\[CCedilla]\[ATilde]o a uma determinada lista [Lista] e k [k]."

Begin["`Private`"]

geraInd=Function[{ID,part,BP,TempoBP},{ID,part,BP,TempoBP}];

geraIndal = Function[{q,w,m,t}, Module[{p,c},
p=geraparticao[w,m];
c=blocosperf[w,p,m];{q,p,c,Table[t,{i,Length[c]}]}]];

IndID=Function[ind,ind[[1]]];

IndPart=Function[ind,ind[[2]]];

IndBP=Function[ind,ind[[3]]];

IndTempoBP=Function[ind,ind[[4]]];

NovoBP=Function[{ind,novoB,tempoB},Module[{a,b,c,d,n},
a=ind[[1]];
b=ind[[2]];
c=ind[[3]];
d=ind[[4]];
n=ind;
n={a,b,Append[c,novoB],Append[d,tempoB]}]];

AlteraPart=Function[{ind,novaPart},Module[{a,b,c,d,n},
a=ind[[1]];
b=novaPart;
c=ind[[3]];
d=ind[[4]];
n=ind;
n={a,b,c,d}]];

CIA = Function[{ind, Lista, k}, 
   Module[{blocoperf, somablocoscoef, blocos, i,r}, 
    blocoperf = Apply[Plus, Lista]/k;
    somablocoscoef = 0;
    blocos = agrupablocos[Lista, IndPart[ind], k];
    i = 1;
    While[i <= Length[blocos], 
     somablocoscoef = 
      somablocoscoef + Abs[Apply[Plus, blocos[[i]]] - blocoperf];
     i = i + 1];
    r=(somablocoscoef/k);
r]];


End[]

EndPackage[]






