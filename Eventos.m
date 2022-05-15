(* ::Package:: *)

BeginPackage["Eventos`"]

Eventos::usage="Pacote que disponibiliza opera\[CCedilla]\[OTilde]es sobre eventos."

novoev::usage="novoev[tempo,tipo,id] cria um evento que ocorre no instante (tempo), do tipo (tipo) e associa-o ao indiv\[IAcute]duo (id)."

tempoev::usage="tempoev[e] devolve o tempo em que ocorre o evento e."

tipoev::usage="tipoev[e] devolve o tipo de evento que caracteriza e."

idev::usage="idev[e] devolve o ID ao qual est\[AAcute] associado o evento e."

Begin["`Private`"]

novoev=Function[{tempo,tipo,id},{tempo,tipo,id}];

tempoev=Function[e,e[[1]]];

tipoev=Function[e,e[[2]]];

idev=Function[e,e[[3]]];

End[]

EndPackage[]
