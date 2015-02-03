(* ::Package:: *)

(* ::Text:: *)
(*Original code by Simon Woods using:*)
(*Code formatting function by Leonid Shifrin*)
(*Output style from Rob Collyer*)
(**)
(*See http://mathematica.stackexchange.com/a/15948*)


BeginPackage["Spelunking`"]


Spelunk::usage="Spelunk[symbol]"


Begin["`Private`"]


(* ::Subsection:: *)
(*Main function*)


(* ::Text:: *)
(*Spelunk[symbol] gets the Definition of symbol in box form using def[] and then applies the rule tidyup to tidy it up.*)
(*Tidying up consists primarily of replacing the full names of symbols (i.e. including context) with short names, with the full name and the symbol's attributes put into a tooltip.*)
(*After tidying up, the boxes are printed to a cell using output[] which does some nice code formatting.*)
(**)
(*Additional features:*)
(*Any symbol with a definition is presented as a button which runs Spelunk on that symbol.*)
(*The full symbol name may be copied to the clipboard by right-clicking on it.*)
(**)
(*Note that Spelunk also works with symbols as strings, so you can do things like Spelunk /@ Names["Image`*"] to spelunk a whole context.*)


SetAttributes[Spelunk,HoldFirst]


Spelunk[symbol_Symbol]:=Module[{outboxes},
outboxes=First[def[symbol]]/.tidyup;
output[outboxes];
If[!FreeQ[outboxes,"System`Dump`AutoLoad"]&&Hold[symbol]=!=Hold[System`Dump`AutoLoad],
output[MakeBoxes[Button["Autoload and Spelunk this function",symbol;Spelunk[symbol]]]]]]


Spelunk[s_String]:=ToExpression[s,InputForm,Spelunk]


(* ::Subsection:: *)
(*Definition extraction*)


(* ::Text:: *)
(*def[sym] gets the box form of the Definition of sym, and the attributes of sym.*)
(*The definition is set to "Null" if the symbol is Locked (symbols with no definition also return "Null")*)


SetAttributes[def,HoldFirst]


(* old version using MakeBoxes *)
(*def[sym_Symbol]:=Module[{att=Attributes[sym]},
{If[MemberQ[att, Locked], "Null",
Internal`InheritedBlock[{sym},Unprotect[sym]; ClearAttributes[sym, ReadProtected];
Quiet[MakeBoxes[Definition@sym] //. InterpretationBox[a_, ___] :> a] ]],ToString@att}]*)

def[sym_Symbol]:=Module[{att=Attributes[sym]},
{If[MemberQ[att, Locked], "Null",
Internal`InheritedBlock[{sym},Unprotect[sym]; ClearAttributes[sym, ReadProtected];
MathLink`CallFrontEnd[FrontEnd`UndocumentedTestFEParserPacket[
ToString[Definition[sym], InputForm],False]][[1,1]] ]],ToString@att}]


def[s_String]:=ToExpression[s,InputForm,def]


(* ::Subsection:: *)
(*Symbol processing*)


(* ::Text:: *)
(*tidyup is a rule to identify strings within boxes which represent symbols and pass them through processsymbol.*)
(*A string is considered to represent a symbol if it contains context marks and doesn't start with an explicit quote mark (so that string literals are left alone)*)


tidyup=s_String:>First@StringCases[s,{a:(c:Except["\""]..~~"`"~~b__)/;!StringMatchQ[c,NumberString]:>
processsymbol[a,b],other__:>other}];


(* ::Text:: *)
(*processsymbol takes the full and short names of a symbol and returns an appropriate box :*)
(*If the name contains an underscore it is probably a pattern and a simple TooltipBox is returned.*)
(*The definition of the symbol is then looked up :*)
(*If the definition is "Null", this is a symbol with no definition available and a symbolbox is returned.*)
(*If the definition is not "Null" a symbolbox is returned embedded into a button which runs Spelunk on that symbol.*)
(**)
(*processsymbol is memoized because it tends to get called multiple times for the same symbol.*)


mem:processsymbol[full_, shrt_] :=mem= Module[{db,att},
  Which[
!StringFreeQ[full, "_"],TooltipBox[shrt, full],
{db,att} = def[full];
db==="Null",symbolbox[shrt,full,att] ,
True,ButtonBox[symbolbox[shrt,full,att], ButtonFunction:>Spelunk@full,BaseStyle->{},Evaluator->Automatic]]]


(* ::Subsection:: *)
(*Output formatting*)


(* ::Text:: *)
(*symbolbox takes the short and full names and attributes of a symbol and creates a box which:*)
(*displays as the short name*)
(*has a tooltip showing the full name and attributes (if any)*)
(*can be right-clicked to copy the full name to the clipboard*)


symbolbox[shrt_,full_,att_]:=TagBox[TooltipBox[shrt,gbox[full,att]],
  EventHandlerTag[{{"MouseClicked", 2}:>CopyToClipboard[ToExpression[full,InputForm,Defer]]}]]


(* ::Text:: *)
(*spelunkcellprint[boxes] creates an output cell with syntax highlighting. (Code from rcollyer)*)
(*prettyboxes[boxes] adds line breaks and tabs to format the code nicely (Code from Leonid Shifrin)*)
(*gbox[args] creates a GridBox with args in columns.*)


spelunkcellprint[boxes_]:=CellPrint[Cell[BoxData[boxes], "Output",
Background->RGBColor[1, 0.95, 0.9],
CellGroupingRules->"GraphicsGrouping",
GeneratedCell->True,CellAutoOverwrite->True,
ShowAutoStyles->True,LanguageCategory->"Mathematica",
FontWeight->"Bold",ShowStringCharacters->True]]


prettyboxes[boxes_]:=boxes/.{" "}->{"\n-----------\n"}//.{RowBox[{left___,";",next:Except["\n"],right___}]:>
RowBox[{left,";","\n","\t",next,right}],RowBox[{sc:("Block"|"Module"|"With"),"[",RowBox[{vars_,",",body_}],"]"}]:>
RowBox[{sc,"[",RowBox[{vars,",","\n\t",body}],"]"}]}


output=spelunkcellprint @ prettyboxes @ #&;


gbox[args__,"{}"]:=gbox[args]
gbox[args__]:=GridBox[Transpose[{{args}}]]


End[]


EndPackage[]
