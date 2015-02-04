(* ::Package:: *)

(* ::Text:: *)
(*Original code by Simon Woods using:*)
(*Code formatting function by Leonid Shifrin*)
(*Output style from Rob Collyer*)
(**)
(*See http://mathematica.stackexchange.com/a/15948*)


BeginPackage["Spelunking`"]


(* ::Subsection:: *)
(*Public interface*)


Spelunk::usage="Spelunk[symbol] shows the definition of symbol (if it has one) but hides the context part of symbols for easier reading. Symbols have a tooltip showing their full name and any attributes. Any symbol (other than system symbols) with a readable definition is presented as a button which runs Spelunk on that symbol. The full symbol name may be copied to the clipboard by right-clicking on it.

Example: Spelunk[BarChart]";

LoadEverything::usage="LoadEverything[] evaluates every symbol it can find with an AutoLoad trigger, this can be used to increase the scope of SpelunkSearch. Save your work first!";

SpelunkSearch::usage="SpelunkSearch[symbolpattern, stringpattern] is used to find symbols whose names match symbolpattern (the same syntax as Names). The results are sorted by context and are clickable to run Spelunk on them. stringpattern is an optional pattern to filter the results to those containing the pattern in their definition.

Example: SpelunkSearch[\"Region*`*\"]
Example: SpelunkSearch[\"Region*`*\",\"RegionEmbeddingDimension\"]";


Begin["`Private`"]


(* ::Subsection:: *)
(*Exported functions*)


(* ::Text:: *)
(*Spelunk gets the boxes for the definition of a symbol, applies the formatting rules and pretty-prints the result*)


SetAttributes[Spelunk,HoldFirst]


Spelunk[sym_Symbol]:=Module[{outboxes},
outboxes=defboxes[sym]/.symbolrules;
If[hasAutoLoad[sym],output@autoLoadButton@sym];
output[outboxes]]


Spelunk[s_String]:=ToExpression[s,InputForm,Spelunk]


(* ::Text:: *)
(*LoadEverything autoloads everything it can find*)


LoadEverything[]:=Module[{n1,n2},
n1=Names["*`*"];
Quiet@Scan[If[hasAutoLoad[#],Symbol[#]]&,n1];
n2=Names["*`*"];
Row[{"Loaded ",Length[n2]-Length[n1]," symbols"}]]


(* ::Text:: *)
(*SpelunkSearch filters first on symbol names and then on the contents of the definition*)


SpelunkSearch[np_,s_:""]:=Module[{names,buttonfunc},
names=Select[Names[np],isDefinition[First@definitionString[#],True]&] ;
names=DeleteCases[names,"In"|"InString"|"Out"];
If[s=!="",names=Select[names,!StringFreeQ[First@definitionString[#],s]&]];
names=names~SortBy~ Context ~SplitBy~ Context;
buttonfunc=Button[StringReplace[#,__~~"`":>""],Spelunk[#],Appearance->"Frameless"]&;
Panel@Column[OpenerView[
{Context[Evaluate@First@#],Column[buttonfunc/@#]}]&/@names,
Frame->All,FrameStyle->Thin]]


(* ::Subsection:: *)
(*Definition extraction*)


(* ::Text:: *)
(*definitionString gets the string form of the definition of sym, and the attributes of sym.*)
(*The definition is set to "Null" if the symbol is Locked (symbols with no definition also return "Null")*)


SetAttributes[definitionString,HoldFirst]


definitionString[definitionString]={"Null",""};  (* prevent recursive memoisation *)


definitionString[sym_Symbol]:=Module[{att=Attributes[sym]},
{If[MemberQ[att, Locked],"Null",
Internal`InheritedBlock[{sym},Unprotect[sym]; ClearAttributes[sym, ReadProtected];
ToString[Definition[sym], InputForm]]~Quiet~Set::specset],ToString@att}]


definitionString[s_String]:=ToExpression[s,InputForm,definitionString]


(* ::Text:: *)
(*defboxes converts the definition string into box form*)


SetAttributes[defboxes,HoldFirst]


defboxes[sym_]:=
MathLink`CallFrontEnd[FrontEnd`UndocumentedTestFEParserPacket[
definitionString[sym][[1]],False]][[1,1]]


(* ::Text:: *)
(*isDefinition determines if a definition string represents a useful definition. If the definition is null or just a list of attributes, the function returns False. The second argument controls whether to consider an option list as "useful".*)


isDefinition[s_String,strict_:False]:=!StringMatchQ[s,"Attributes[*] = {*}"|"Null"|If[TrueQ@strict,"Options[*] = {*}",""]]


(* ::Text:: *)
(*hasAutoLoad determines if a symbol has an AutoLoad trigger*)


SetAttributes[hasAutoLoad,HoldFirst]


hasAutoLoad[sym_]:=StringMatchQ[First@definitionString[sym],"*:= System`Dump`AutoLoad[*"]


(* ::Text:: *)
(*autoLoadButton creates a button to AutoLoad and Spelunk a symbol*)


SetAttributes[autoLoadButton,HoldFirst]


autoLoadButton[sym_]:=MakeBoxes@Button["Autoload and Spelunk this function",sym;Spelunk[sym]]


(* ::Subsection:: *)
(*Symbol processing*)


(* ::Text:: *)
(*symbolrules is a rule to identify strings within boxes which represent symbols and pass them through processsymbol.*)
(*A string is considered to represent a symbol if it contains context marks and doesn't start with an explicit quote mark (so that string literals are left alone) and doesn't appear to be a number*)


symbolrules=s_String:>First@StringCases[s,{a:(c:Except["\""]..~~"`"~~b__)/;!StringMatchQ[c,NumberString]:>
processsymbol[a,b],other__:>other}];


(* ::Text:: *)
(*processsymbol takes the full and short names of a symbol and returns an appropriate box :*)
(*If the name contains an underscore it is probably a pattern and a simple TooltipBox is returned.*)
(*The definition of the symbol is then looked up :*)
(*If the definition is "Null", this is a symbol with no definition available and a symbolbox is returned.*)
(*If the definition is not "Null" a symbolbox is returned embedded into a button which runs Spelunk on that symbol.*)


mem:processsymbol[full_, shrt_] :=mem= Module[{def,att},
Which[
!StringFreeQ[full, "_"],TooltipBox[shrt, full],
{def,att} = definitionString[full];
isDefinition[def],ButtonBox[symbolbox[shrt,full,att],
ButtonFunction:>Spelunk@full,BaseStyle->{},Evaluator->Automatic],
True,symbolbox[shrt,full,att]]]


(* ::Subsection:: *)
(*Output formatting*)


(* ::Text:: *)
(*symbolbox takes the short and full names and attributes of a symbol and creates a box which displays as the short name but has a tooltip showing the full name and attributes (if any) and can be right-clicked to copy the full name to the clipboard*)


symbolbox[shrt_,full_,att_]:=TagBox[TooltipBox[shrt,gbox[full,att]],
  EventHandlerTag[{{"MouseClicked", 2}:>CopyToClipboard[ToExpression[full,InputForm,Defer]]}]]


(* ::Text:: *)
(*spelunkcellprint creates an output cell with syntax highlighting. (Code from rcollyer)*)


spelunkcellprint[boxes_]:=CellPrint[Cell[BoxData[boxes], "Output",
Background->RGBColor[1, 0.95, 0.9],
CellGroupingRules->"GraphicsGrouping",
GeneratedCell->True,CellAutoOverwrite->True,
ShowAutoStyles->True,LanguageCategory->"Mathematica",
FontWeight->"Bold",ShowStringCharacters->True]]


(* ::Text:: *)
(*prettyboxes adds line breaks and tabs to format the code nicely (Code from Leonid Shifrin)*)


prettyboxes[boxes_]:=boxes/.{" "}->{"\n-----------\n"}//.{RowBox[{left___,";",next:Except["\n"],right___}]:>
RowBox[{left,";","\n","\t",next,right}],RowBox[{sc:("Block"|"Module"|"With"),"[",RowBox[{vars_,",",body_}],"]"}]:>
RowBox[{sc,"[",RowBox[{vars,",","\n\t",body}],"]"}]}


(* ::Text:: *)
(*gbox creates a GridBox with args in columns*)


gbox[args__,"{}"]:=gbox[args]
gbox[args__]:=GridBox[Transpose[{{args}}]]


(* ::Text:: *)
(*output applies the formatting and prints the cell*)


output=spelunkcellprint[prettyboxes[#]]&;


End[]


EndPackage[]
