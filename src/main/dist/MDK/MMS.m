(* ::Package:: *)

(* ::Title:: *)
(*Mathematica MDK - Model Management System*)


BeginPackage["MDK`MMS`"];


(* ::Section:: *)
(*MMS*)


(* ::Subsection::Closed:: *)
(*Usages*)


MMSGetElement::usage = "MMSGetElement[server, projectId, elementID, refId (default master)]: Get an element from MMS.";
MMSGetElements::usage = "MMSGetElements[server, projectId, {elementID1, elementID2,...},refId (default master)]: Get multiple elements from MMS.";
MMSCreateElement::usage = "MMSCreateElement[server, projectId, <|property1 \[Rule] answer, property2 \[Rule] answer2,...|>, refId (default master)]: Create an element in MMS.";
MMSCreateElements::usage = "MMSCreateElements[server, projectId, {<|element1Property1 \[Rule] answer, element1Property2 \[Rule] answer2|>, <|element2Property1 \[Rule] answer3,
	element2Property2 \[Rule] answer4|>}, refId (default master)]: Create multiple elements in MMS.";
MMSUpdateElement::usage = "MMSUpdateElement[server, projectId, <|property1 \[Rule] answer, property2 \[Rule] answer2,...|>, refId (default master)]: Update an existing element in MMS.";
MMSUpdateElements::usage = "MMSUpdateElements[server, projectId, {<|element1Property1 \[Rule] answer, element1Property2 \[Rule] answer2|>, <|element2Property1 \[Rule] answer3,
	element2Property2 \[Rule] answer4|>}, refId (default master)]: Update existing elements in MMS.";
MMSDeleteElement::usage = "MMSDeleteElement[server, projectId, elementID, refId (default master)]: Delete an element in MMS.";
MMSDeleteElements::usage = "MMSDeleteElements[server, projectId, {elementID1, elementID2,...}, refId (default master)]: Delete multiple elements in MMS.";
MMSAuthorize::usage = "MMSAuthorize[username (default Null), password (default Null)]: Call this function with your MMS credentials. If omitted, you will be prompted to enter them.";
MMSDeauthorize::usage = "MMSDeauthorize[]: The user is responsible for calling this function to wipe any MMS authorization credentials from the session.";


(* ::Subsection::Closed:: *)
(*Authorization*)


Begin["`Private`"];
MMSAuthorize[username_:Null, password_:Null]:=(
If[StringQ[username],
	If[StringQ[password],
		MMSuser = username;
		MMSpass = password,
		MMSuser=username;
		MMSpass=InputString["Enter your MMS Password:",FieldMasked->True,FieldSize->{20,1},WindowTitle->"MMS Authorization",WindowSize->{290,120}];
	],
	If[StringQ[password],
		MMSuser = InputString["Enter your MMS Username:",FieldSize->{20,1},WindowTitle->"MMS Authorization",WindowSize->{290,120}];
		MMSpass = password,
		MMSuser = InputString["Enter your MMS Username:",FieldSize->{20,1},WindowTitle->"MMS Authorization",WindowSize->{290,120}];
		MMSpass = InputString["Enter your MMS Password:",FieldMasked->True,FieldSize->{20,1},WindowTitle->"MMS Authorization",WindowSize->{290,120}]
]];
MMSauth=StringJoin["Basic ",ExportString[StringRiffle[{MMSuser,MMSpass},":"],"Base64"]];
)
End[];


Begin["`Private`"];
MMSDeauthorize[]:=(
	Clear[MMSuser,MMSpass,MMSauth];
)
End[];


(* ::Subsection::Closed:: *)
(*MMSGet*)


Begin["`Private`"];
MMSGetElement[server_, projectId_, elementID_, refId_:"master"]:=(
url = StringJoin[server, "/alfresco/service/projects/", projectId, "/refs/", refId, "/elements/", elementID];
If[StringQ[MMSauth],
	out = Association[URLExecute[HTTPRequest[url,<|"Headers" -> {"Authorization" -> MMSauth} |>, Interactive->False]]];
	Return[out],
	Message[Authorization::reqMMSAuthorization]
])
End[];


Begin["`Private`"];
MMSGetElements[server_, projectId_, elementIDs_, refId_:"master"]:=(
If[StringQ[MMSauth],
	Table[MMSGetElement[server, projectId, elementIDs[[i]], refId], {i, Length[elementIDs]}],
	Message[Authorization::reqMMSAuthorization]
])
End[];


(* ::Subsection::Closed:: *)
(*MMSPost*)


Begin["`Private`"];
MMSCreateElement[server_, projectId_, element_, refId_:"master"]:=( (* element takes in an association of properties. e.g 
	<|"property1"\[Rule]"answer","property2"\[Rule]"answer2",...|>  *)
remove = KeySelect[StringStartsQ[Except @ "_"]][element];
assoc = <|"elements" -> {remove}|>;
url = StringJoin[server, "/alfresco/service/projects/", projectId, "/refs/", refId, "/elements"]; (* specify site? *)
jsonString = StringDelete[ExportString[assoc, "RawJSON"], "\n" | "\t"];
If[StringQ[MMSauth],
	out=Association[URLExecute[HTTPRequest[url,
		<|Method -> "POST",
		"Body" -> jsonString,
		"ContentType" -> "application/json;charset=UTF-8",
		"Headers" -> {"Authorization" -> MMSauth} |>, Interactive->False]]][["elements"]];
	If[KeyExistsQ[out,"message"],
		Return[Message[Authorization::Unauthorized]],
		Return[Association[out]]],
	Message[Authorization::reqMMSAuthorization]
])
End[];


Begin["`Private`"];
MMSCreateElements[server_, projectId_, elements_, refId_:"master"]:=( (* elements takes in a list of associations of the form
	{<|"element1Property1"\[Rule]"answer","element1Property2"\[Rule]"answer2"|>,<|"element2Property1"\[Rule]"answer3",
	"element2Property2"\[Rule]"answer4"|>} *)
If[StringQ[MMSauth],
	Table[single=MMSCreateElement[server, projectId, elements[[i]], refId];
		If[KeyExistsQ[single,"name"],
			single],
		{i, Length[elements]}],
	Message[Authorization::reqMMSAuthorization]
])
End[];


Begin["`Private`"]; (* updateElement(s) is only needed for completeness of the mdk. Functionally, it is the same as createElement(s) *)
MMSUpdateElement[server_, projectId_, element_, refId_:"master"]:=(
	MMSCreateElement[server, projectId, element, refId]
)
End[];


Begin["`Private`"];
MMSUpdateElements[server_, projectId_, elements_, refId_:"master"]:=(
	MMSCreateElements[server, projectId, elements, refId]
)
End[];


(* ::Subsection::Closed:: *)
(*MMSDelete*)


Begin["`Private`"];
MMSDeleteElement[server_, projectId_, elementID_, refId_:"master"]:=(
url = StringJoin[server, "/alfresco/service/projects/", projectId, "/refs/", refId, "/elements"];
assoc = <|"elements" -> { <|"id" -> elementID|>}|>;
jsonString = ExportString[assoc, "RawJSON"];
If[StringQ[MMSauth],
	out=Association[URLExecute[HTTPRequest[url,
		<|Method -> "DELETE",
		"Body" -> jsonString,
		"ContentType" -> "application/json;charset=UTF-8",
		"Headers" -> {"Authorization" -> MMSauth} |>, Interactive->False]]];
	If[KeyExistsQ[out,"message"],
		Message[Authorization::Unauthorized],
		Return[out]],
	Message[Authorization::reqMMSAuthorization]
])
End[];


Begin["`Private`"];
MMSDeleteElements[server_, projectId_, elementIDs_, refId_:"master"]:=( (* takes in a list of element IDs *)
url = StringJoin[server, "/alfresco/service/projects/", projectId, "/refs/", refId, "/elements"];
assoc = <|"elements" -> Association/@Thread["id" -> elementIDs]|>;
jsonString = ExportString[assoc, "RawJSON"];
If[StringQ[MMSauth],
	out=Association[URLExecute[HTTPRequest[url,
		<|Method -> "DELETE",
		"Body" -> jsonString,
		"ContentType" -> "application/json;charset=UTF-8",
		"Headers" -> {"Authorization" -> MMSauth} |>, Interactive->False]]];
	If[KeyExistsQ[out,"message"],
		Message[Authorization::Unauthorized],
		Return[out]],
	Message[Authorization::reqMMSAuthorization]
])
End[];


(* ::Section:: *)
(*Errors*)


Begin["`Private`"];
Authorization::reqMMSAuthorization = "Call MMSAuthorize first.";
Authorization::Unauthorized = "You are unauthorized to perform this action.";
End[];


EndPackage[] (* "MDK`MMS`" *)
