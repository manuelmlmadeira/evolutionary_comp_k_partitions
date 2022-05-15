(* ::Package:: *)

BeginPackage["Parti\[CCedilla]\[ATilde]o`"]

Parti\[CCedilla]\[ATilde]o::usage="Pacote que disponibiliza opera\[CCedilla]\[OTilde]es sobre k-parti\[CCedilla]\[OTilde]es de uma lista de n\[UAcute]meros inteiros e n\[ATilde]o negativos."

geraparticao::usage="geraparti\[CCedilla]\[ATilde]o[w,k] recebe uma lista (w) e um (k) inteiro positivo e cria uma k-parti\[CCedilla]\[ATilde]o da lista w aleatoriamente."

blocosperfQ::usage="blocosperfQ[x,p,k] recebe uma lista (x), uma k-parti\[CCedilla]\[ATilde]o (p) dessa mesma lista e o k correspondente \[AGrave] k-parti\[CCedilla]\[ATilde]o em causa e devolve True caso essa parti\[CCedilla]ao possua blocos perfeitos e False caso contr\[AAcute]rio."

blocosperf::usage="blocosperf[x,p,k] recebe uma lista (x), uma k-parti\[CCedilla]\[ATilde]o (p) dessa mesma lista e o k correspondente \[AGrave] k-parti\[CCedilla]\[ATilde]o em causa e devolve uma lista com o(s) n\[UAcute]mero(s) do(s) bloco(s) perfeito (s) dessa parti\[CCedilla]\[ATilde]o."

blocosimperf::usage="blocosimperf[x,p,k] recebe uma lista (x), uma k-parti\[CCedilla]\[ATilde]o (p) dessa mesma lista e o k correspondente \[AGrave] k-parti\[CCedilla]\[ATilde]o em causa e devolve uma lista com o(s) n\[UAcute]mero(s) do(s) bloco(s) imperfeito (s) dessa parti\[CCedilla]\[ATilde]o."

bloco::usage="bloco[x,p,nbloco] recebe uma lista (x), uma k-parti\[CCedilla]\[ATilde]o (p) dessa mesma lista e o n\[UAcute]mero de um bloco (nbloco) e devolve uma lista com os elementos de x pertencentes ao bloco j\[AAcute] identificado, de acordo com a k-parti\[CCedilla]\[ATilde]o em causa."

alterabloco::usage="alterabloco[p,posi\[CCedilla]ao,blocodestino] recebe uma k-parti\[CCedilla]\[ATilde]o (p), a posi\[CCedilla]\[ATilde]o de um elemento na lista (p) e um n\[UAcute]mero que identifica um bloco (blocodestino) e devolve a parti\[CCedilla]\[ATilde]o em tudo igual \[AGrave] recebida, com excep\[CCedilla]\[ATilde]o do elemento que se encontra na posi\[CCedilla]\[ATilde]o referida, que passa para o bloco identificado."

somabloco::usage="somabloco[x,p,b] recebe uma lista (x), uma k-parti\[CCedilla]\[ATilde]o (p) dessa mesma lista e um n\[UAcute]mero indicador de um dos blocos da k-parti\[CCedilla]\[ATilde]o (b) e devolve a soma de todos os elementos de x pertencentes ao bloco indicado."

agrupablocos::usage="agrupablocos[x,p,k] recebe uma lista (x), uma k-parti\[CCedilla]\[ATilde]o (p) dessa mesma lista e o k correspondente \[AGrave] k-parti\[CCedilla]\[ATilde]o em causa e devolve uma lista onde os elementos de cada bloco se encontram agrupados por listas."

comparablocos::usage="comparablocos[x, ppai, bperfpai, pmae, bimpmae] recebe uma lista (x), duas k-parti\[CCedilla]\[OTilde]es - que podem ser iguais - (ppai e pmae), um n\[UAcute]mero que indica um bloco perfeito da ppai (bperfpai) e uma lista com os n\[UAcute]meros dos blocos imperfeitos da pmae (bimpmae). Devolve True caso seja poss\[IAcute]vel reconstruir o bloco perfeito de ppai com os elementos de x pertencencentes aos blocos imperfeitos de pmae. Caso contr\[AAcute]rio, devolve False."

escolhesorte::usage="escolhesorte[lista] recebe uma lista e escolhe aleatoriamente um elemento"

posicaon::usage="posicaon[lista,n] devolve a posi\[CCedilla]\[ATilde]o do elemento (n) na lista"

aleatoriointeiro::usage="aleatoriointeiro[{min,max}] devolve um numero inteiro aleat\[OAcute]rio de entre dois valores (min e max)"

Begin["`Private`"]

geraparticao = Function[{w, k}, Table[i = RandomInteger[{1, k}], {i, 1, Length[w]}]];

blocosperfQ = Function[{x, p, k}, Module[{nbloco, perf, b},
    perf = (Apply[Plus, x])/k;
    nbloco = 1;
    b = False;
    While[nbloco <= k && ! b,
     If[Apply[Plus, Pick[x, p, nbloco]] == perf,
      b = True];
     nbloco = nbloco + 1];
    b]];


blocosperf = Function[{x, p, k}, Module[{nbloco, perf, w},
    perf = (Apply[Plus, x])/k;
    nbloco = 1;
    w = {};
    While[nbloco <= k,
     If[Apply[Plus, Pick[x, p, nbloco]] == perf,
      w = Append[w, nbloco]];
     nbloco = nbloco + 1];
    w]];

blocosimperf = Function[{x, p, k}, Module[{nbloco, perf, w},
        perf = (Apply[Plus, x])/k;
        nbloco = 1;
        w = {};
        While[nbloco <= k,
          If[Apply[Plus, Pick[x, p, nbloco]] != perf,
            w = Append[w, nbloco]];
          nbloco = nbloco + 1];
        w]];


bloco = Function[{x, p, nbloco}, Pick[x, p, nbloco]];

alterabloco = Function[{p, posi\[CCedilla]ao, blocodestino}, Module[{w},
    w = p;
    w[[posi\[CCedilla]ao]] = blocodestino;
    w
    ]];

somabloco = Function[{x, p, b}, Apply[Plus, Pick[x, p, b]]];

agrupablocos = Function[{x, p, k}, Table[Pick[x, p, i], {i, k}]];

comparablocos = 
  Function[{x, ppai, bperfpai, pmae, bimpmae}, Module[{w1, w2},
    w1 = Pick[x, ppai, bperfpai];
    w2 = formaw2[x, pmae, bimpmae];
    comparalistas[w1, w2]
    ]];


comparalistas = 
  Function[{w1, w2}, FixedPoint[compaux, {w1, w2, True, {}}][[3]]];

compaux = Function[quadruplo, Module[{lista1, lista2, b, posi\[CCedilla]oes},
    {lista1, lista2, b, posi\[CCedilla]oes}=quadruplo;
    If[Length[lista1] > Length[lista2], {lista1, lista2, False, 
      posi\[CCedilla]oes},
     If[! b || lista1 == {}, quadruplo,
      posi\[CCedilla]oes = Position[lista1, First[lista1]];
      If[Position[lista2, First[lista1]] == {},
       {lista1, lista2, False, posi\[CCedilla]oes},
       {Delete[lista1, posi\[CCedilla]oes], lista2, b, posi\[CCedilla]oes}]]]]];


formaw2 = 
  Function[{x, pmae, bimpmae}, 
   Flatten[Nest[faux, {x, pmae, bimpmae, {}}, Length[bimpmae]][[4]]]];

faux = Function[tripla, Module[{lista, part, lbimp, prew2},
    {lista, part, lbimp, prew2} = tripla;
    {lista, part, Rest[lbimp], 
     Append[prew2, Pick[lista, part, First[lbimp]]]}]];

escolhesorte = Function[lista, RandomChoice[lista]];

posicaon = Function[{lista,n},Flatten[Position[lista,n]]];

aleatoriointeiro = Function[minmax,RandomInteger[minmax]];


End[]

EndPackage[]
