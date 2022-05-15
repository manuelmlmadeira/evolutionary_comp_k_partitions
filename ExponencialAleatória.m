(* ::Package:: *)

BeginPackage["ExponencialAleat\[OAcute]ria`"]

ExponencialAleat\[OAcute]ria::usage="Pacote que disponobiliza opera\[CCedilla]\[OTilde]es que envolvem vari\[AAcute]veis exponenciais aleat\[OAcute]ria."

expal::usage="expale[m] devolve uma observa\[CCedilla]\[ATilde]o da vari\[AAcute]vel exponencial aleat\[OAcute]ria com valor m\[EAcute]dio m." 

Begin["`Private`"]

expal=Function[{m},-(m*Log[Random[]])]


End[]

EndPackage[]
