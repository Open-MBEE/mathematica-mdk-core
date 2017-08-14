(* ::Package:: *)

(* ::Title:: *)
(*Mathematica MDK - Artifactory*)


BeginPackage["MDK`ART`"];


(* ::Section:: *)
(*Artifactory*)


(* ::Subsection::Closed:: *)
(*Usages*)


ARTGetArtifact::usage = "ARTGetArtifact[server, repository, artifactDirectory, artifact, saveDirectory]: Gets the artifact from artifactory and saves it to the file and directory you specify.";
ARTGetArtifacts::usage = "ARTGetArtifacts[server, repository, artifactDirectories, artifacts, saveDirectory]: Gets the artifacts from artifactory and saves them to the local directory you specify. \"artifacts\" is a list.";
ARTCreateArtifact::usage = "ARTCreateArtifact[server, repository, artifactDirectory, artifactAbsolutePath, contentType]: The file with the absolute path you specify (e.g. /Users/<userid>/Desktop/myFile.txt) is posted to the \"artifactDireectory\" on Artifactory.";
ARTCreateArtifacts::usage = "ARTCreateArtifacts[server, repository, artifactDirectory, artifacts, fileDirectory, contentTypes]: The files located in \"fileDirectory\" (e.g. /Users/<userid>/Desktop) and listed with \"artifacts\" are posted to the specified \"artifactDirectory\".";
ARTUpdateArtifact::usage = "ARTUpdateArtifact[server, repository, artifactDirectory, artifactAbsolutePath, contentType]: The artifact you specify is updated with the file located at \"artifactAbsolutePath\" (e.g. /Users/<userid>/Desktop/myFile.txt). If the artifact does not exist, it will be created.";
ARTUpdateArtifacts::usage = "ARTUpdateArtifacts[server, repository, artifactDirectory, artifacts, fileDirectory, contentTypes]: The artifacts within \"artifactDirectory\" are updated based on the files located in \"fileDirectory\" (e.g. /Users/<userid>/Desktop). If they do not exist, they will be created."; 
ARTDeleteArtifact::usage = "ARTDeleteArtifact[server, repository, artifactDirectory, artifact]: Deletes the artifact at the location you specify.";
ARTDeleteArtifacts::usage = "ARTDeleteArtifacts[server, repository, artifactDirectories, artifacts]: Deletes the artifacts you specify with the list \"artifacts\" from \"artifactDirectory\".";
ARTAuthorize::usage = "ARTAuthorize[username (default Null), password (default Null)]: Call this function with your Artifactory credentials. If omitted, you will be prompted to enter them.";
ARTDeauthorize::usage = "ARTDeauthorize[]: The user is responsible for calling this function to wipe any Artifactory authorization credentials from the session.";


(* ::Subsection::Closed:: *)
(*Authorization*)


Begin["`Private`"];
ARTAuthorize[username_:Null, password_:Null]:=(
If[StringQ[username],
	If[StringQ[password],
		ARTuser = username;
		ARTpass = password,
		ARTuser=username;
		ARTpass=InputString["Enter your Artifactory Password:",FieldMasked->True,FieldSize->{20,1},WindowTitle->"Artifactory Authorization",WindowSize->{290,120}];
	],
	If[StringQ[password],
		ARTuser = InputString["Enter your Artifactory Username:",FieldSize->{20,1},WindowTitle->"Artifactory Authorization",WindowSize->{290,120}];
		ARTpass = password,
		ARTuser = InputString["Enter your Artifactory Username:",FieldSize->{20,1},WindowTitle->"Artifactory Authorization",WindowSize->{290,120}];
		ARTpass = InputString["Enter your Artifactory Password:",FieldMasked->True,FieldSize->{20,1},WindowTitle->"Artifactory Authorization",WindowSize->{290,120}]
]];
ARTauth=StringJoin["Basic ",ExportString[StringRiffle[{ARTuser,ARTpass},":"],"Base64"]];
)
End[];


Begin["`Private`"];
ARTDeauthorize[]:=(
	Clear[ARTuser,ARTpass,ARTauth];
)
End[];


(* ::Subsection::Closed:: *)
(*ARTGet*)


Begin["`Private`"];
ARTGetArtifact[server_, repository_, artifactDirectory_, artifact_, saveDirectory_]:=(
url = StringJoin[server, "/artifactory/", repository, "/", artifactDirectory, "/", artifact];
SetDirectory[saveDirectory];
Export[artifact, URLExecute[HTTPRequest[url]]]
)
End[];


Begin["`Private`"];
ARTGetArtifacts[server_, repository_, artifactDirectory_, artifacts_, saveDirectory_]:=(
SetDirectory[saveDirectory];
Table[url = StringJoin[server, "/artifactory/", repository, "/", artifactDirectory, "/", artifacts[[i]]];
	Export[artifacts[[i]], URLExecute[HTTPRequest[url]]], {i, Length[artifacts]} ]
)
End[];


(* ::Subsection::Closed:: *)
(*ARTPost*)


Begin["`Private`"];
ARTCreateArtifact[server_, repository_, artifactDirectory_, artifactAbsolutePath_, contentType_]:=(
url = StringJoin[server, "/artifactory/", repository, "/", artifactDirectory, "/File1.txt"];
Return@URLExecute[HTTPRequest[ url,
	<|Method -> "PUT",
	"Body" -> Import[artifactAbsolutePath],
	"ContentType" -> contentType|>]]  (* This needs to either be an argument or completely omitted *)
)
End[];


Begin["`Private`"];
ARTCreateArtifacts[server_, repository_, artifactDirectory_, artifacts_, fileDirectory_, contentTypes_]:=(      (* test if you can post a whole directory and the individual files will be created or if you have to specify the artifact *)
url = StringJoin[server, "/artifactory/", repository, "/", artifactDirectory ];
filePath = StringJoin/@Thread[ {fileDirectory, "/", artifacts} ]; (* !! test this "/" thing *)
If[Length[contentTypes]==1,
	Table[Return@URLExecute[HTTPRequest[ url,
		<|Method -> "PUT",
		"Body" -> Import[ filePath[[i]] ],
		"ContentType" -> contentTypes|> ]],
	{i, Length[artifacts]}],
	Table[Return@URLExecute[HTTPRequest[ url,
		<|Method -> "PUT",
		"Body" -> Import[ filePath[[i]] ],
		"ContentType" -> contentTypes[[i]]|> ]],
	{i, Length[artifacts]}]]
)
End[];


Begin["`Private`"];
ARTUpdateArtifact[server_, repository_, artifactDirectory_, artifactAbsolutePath_, contentType_]:=(
	ARTCreateArtifact[server, repository, artifactDirectory, artifactAbsolutePath]
)
End[];


Begin["`Private`"];
ARTUpdateArtifacts[server_, repository_, artifactDirectory_, artifacts_, fileDirectory_, contentTypes_]:=(
	ARTCreateArtifacts[server, repository, fileDirectory, artifactDirectory, artifacts, contentTypes]
)
End[];


(* ::Subsection::Closed:: *)
(*ARTDelete*)


Begin["`Private`"];
ARTDeleteArtifact[server_, repository_, artifactDirectory_, artifact_]:=(
url = StringJoin[server, "/artifactory/", repository, "/", artifactDirectory, "/", artifact];
Return@URLExecute[HTTPRequest[ url,
	<|Method -> "DELETE"|> ]]
)
End[];


Begin["`Private`"];
ARTDeleteArtifacts[server_, repository_, artifactDirectory_, artifacts_]:=(
Table[url = StringJoin[server, "/artifactory/", repository, "/", artifactDirectory, "/", artifacts[[i]]];
	Return@URLExecute[HTTPRequest[ url,
		<|Method -> "DELETE"|>]], {i, Length[artifacts]}]
)
End[];


(* ::Section:: *)
(*Errors*)


Begin["`Private`"];
Authorization::reqARTAuthorization = "Call ARTAuthorize first.";
Authorization::Unauthorized = "You are unauthorized to perform this action.";
End[];


EndPackage[] (* "MDK`ART`" *)
