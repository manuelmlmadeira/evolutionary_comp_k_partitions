(* ::Package:: *)

BeginPackage["CAP`"]
Needs ["Eventos`"]
Needs["Popula\[CCedilla]\[ATilde]o`"]
Needs["Indiv\[IAcute]duos`"]
Needs["ExponencialAleat\[OAcute]ria`"]

CAP::usage="Pacote que disponibiliza opera\[CCedilla]\[OTilde]es sobre a CAP."

capvazia::usage="Representa a cadeia de acontecimentos pendentes vazia."

adevent::usage="adevent[{cap,event}] adiciona o evento event \[AGrave] cadeia de acontecimentos pendentes cap de forma a manter a sua organiza\[CCedilla]\[ATilde]o por tempo."

removeevent::usage="removeevent[cap] remove o evento seguinte da cap."

proxevent::usage="proxevent[cap] devolve o primeiro evento da cap."

Mudan\[CCedilla]adeplanos::usage="Mudan\[CCedilla]adeplanos[pop, cap] recebe uma popula\[CCedilla]\[ATilde]o e uma cadeia de acontecimentos pendentes e reatualiza a informa\[CCedilla]\[ATilde]o da cadeia."

GeraCAP::usage="GeraCAP[pop,T,TMort,TRep,TMut] recebe uma popula\[CCedilla]\[ATilde]o (pop), um indice de tempo atual (T) e tr\[EHat]s n\[UAcute]meros (TMort,TRep,TMut) para as m\[EAcute]dias de frequ\[EHat]ncias dos acontecimentos respectivos e gera uma cadeia de acontecimentos pendentes  com um acontecimento morte (geral), um acontecimento reprodu\[CCedilla]\[ATilde]o (geral) e um acontecimento muta\[CCedilla]\[ATilde]o relacionado com cada indiv\[IAcute]duo da popula\[CCedilla]\[ATilde]o." 


Begin["`Private`"]

capvazia={};

adevent=Function[{cap, e}, 
          If[cap == {}, 
             {e}, 
             If[tempoev[e] < tempoev[cap[[1]]], 
                Prepend[cap, e], 
                Prepend[
                 adevent[Rest[cap], e],cap[[1]]
                 ]
              ]
           ]
        ];

removeevent=Function[cap,Rest[cap]];

proxevent=Function[cap,First[cap]];

Mudan\[CCedilla]adeplanos=Function[{pop, cap}, Module[{comuns, newcap, capquero},
    capquero = Map[idev, cap];
    comuns = Intersection[capquero, Flatten[{Null, Map[IndID, pop]}]];
    newcap = Select[cap, Function[x, MemberQ[comuns, idev[x]]]];
newcap]];

GeraCAP=Function[{pop,T,TMort,TRep,TMut},Module[{i,cap},
i=1;
cap=capvazia;
cap=adevent[cap,novoev[T+expal[TMort],"Morte",Null]];
cap=adevent[cap,novoev[T+expal[TRep],"Reprodu\[CCedilla]\[ATilde]o",Null]];
While[i<=dimPop[pop],
cap=adevent[cap,novoev[T+expal[TMut],"Muta\[CCedilla]\[ATilde]o",IndID[pop[[i]]]]];
i=i+1];
cap
]];



End[]

EndPackage[]






