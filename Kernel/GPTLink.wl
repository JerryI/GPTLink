(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["KirillBelov`GPTLink`", {"KirillBelov`Objects`"}];


GPTChatComplete::usage = 
"GPTChatComplete[chat] complete given chat. 
GPTChatCompleteAsync[prompt] complete given prompt. 
GPTChatCompleteAsync[chat, prompt] complete chat using given prompt."; 


GPTChatCompleteAsync::usage = 
"GPTChatCompleteAsync[chat, callback] complete given chat in async mode. 
GPTChatCompleteAsync[prompt, callback] complete given prompt in async mode. 
GPTChatCompleteAsync[chat, prompt, callback] complete chat using given prompt in async mode."; 


GPTChatObject::usage = 
"GPTChatObject[] symbolic chat representation in Wolfram Language.
GPTChatObject[\"system\"] symbolic chat representation with system prompt in Wolfram Language."; 


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


promptPattern = _String | _Image | {_String, _Image} | {_String, _Graphics} | {_String, Legended[_Graphics, ___]}; 


CreateType[GPTChatObject, {
	"Endpoint" -> "https://api.openai.com", 
	"Temperature" -> 0.7, 
	"User", 
	"APIToken" :> With[{x = SystemCredential["OPENAI_API_KEY"]}, If[MissingQ[x], "", x] ], 
	"Model" -> "gpt-4o-mini", 
	"MaxTokens" -> 70000, 
	"TotalTokens" -> 0, 
	"Tools" -> {}, 
	"ToolHandler" -> defaultToolHandler,
	"ToolFunction" -> defaultToolFunction,
	"ToolChoice" -> "auto", 
	"Messages" -> {}, 
	"Logger" -> None,
	"Icon" -> Uncompress["1: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"]
}]; 


GPTChatObject[system_String, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[opts]}, 
	chat["Messages"] = Append[chat["Messages"], <|
		"role" -> "system", 
		"date" -> Now,
		"content" -> system
	|>]; 
	chat
]; 


GPTChatObject /: Append[chat_GPTChatObject, message_Association?AssociationQ] := 
(chat["Messages"] = Append[chat["Messages"], Append[message, "date" -> Now]]; chat); 


GPTChatObject /: Append[chat_GPTChatObject, message_String?StringQ] := 
Append[chat, <|"role" -> "user", "content" -> message|>]; 


GPTChatObject /: Append[chat_GPTChatObject, image_Image?ImageQ] := 
With[{imageBase64 = BaseEncode[ExportByteArray[image, "JPEG"], "Base64"]}, 
	Append[chat, <|"role" -> "user", "content" -> {
		<|
			"type" -> "image_url", 
			"image_url" -> <|
				"url" -> StringTemplate["data:image/jpeg;base64,``"][imageBase64]
			|>
		|>
	}|>]
]; 


GPTChatObject /: Append[chat_GPTChatObject, {text_String?StringQ, image_Image?ImageQ}] := 
With[{imageBase64 = BaseEncode[ExportByteArray[image, "JPEG"], "Base64"]}, 
	Append[chat, <|"role" -> "user", "content" -> {
		<|"type" -> "text", "text" -> text|>, 
		<|
			"type" -> "image_url", 
			"image_url" -> <|
				"url" -> StringTemplate["data:image/jpeg;base64,``"][imageBase64]
			|>
		|>
	}|>]
]; 


GPTChatObject /: Append[chat_GPTChatObject, {text_String?StringQ, graphics: _Graphics | Legended[_Graphics, ___]}] := 
With[{image = Rasterize[graphics]}, 
	Append[chat, {text, image}]
]; 


Options[GPTChatCompleteAsync] = {
	"Endpoint" -> Automatic, 
	"Temperature" -> Automatic, 
	"User" -> Automatic, 
	"APIToken" -> Automatic, 
	"Model" -> Automatic, 
	"MaxTokens" -> Automatic, 
	"Tools" -> Automatic, 
	"ToolChoice" -> Automatic, 
	"ToolFunction" -> Automatic,
	"ToolHandler" -> Automatic,
	"Logger" -> Automatic
}; 


GPTChatCompleteAsync::err = 
"`1`"; 


GPTChatCompleteAsync[chat_GPTChatObject, callback: _Function | _Symbol, 
	secondCall: GPTChatComplete | GPTChatCompleteAsync: GPTChatCompleteAsync, opts: OptionsPattern[]] := 
Module[{ 
	endpoint = ifAuto[OptionValue["Endpoint"], chat["Endpoint"]],  
	apiToken = ifAuto[OptionValue["APIToken"], chat["APIToken"]], 
	model = ifAuto[OptionValue["Model"], chat["Model"]], 
	temperature = ifAuto[OptionValue["Temperature"], chat["Temperature"]], 
	tools = ifAuto[OptionValue["Tools"], chat["Tools"]], 
	toolFunction = ifAuto[OptionValue["ToolFunction"], chat["ToolFunction"]], 
	toolChoice = ifAuto[OptionValue["ToolChoice"], chat["ToolChoice"]], 
	maxTokens = ifAuto[OptionValue["MaxTokens"], chat["MaxTokens"]], 
	logger = ifAuto[OptionValue["Logger"], chat["Logger"]],
	toolHandler = ifAuto[OptionValue["ToolHandler"], chat["ToolHandler"]],
	url, 
	headers, 
	messages, 
	requestAssoc, 
	requestBody, 
	request
}, 
	url = URLBuild[{endpoint, "v1", "chat", "completions"}]; 
	
	headers = If[StringQ[apiToken] && TrueQ[StringLength[apiToken] > 0], 
		{
			"Authorization" -> "Bearer " <> apiToken, 
			"X-API-KEY" -> apiToken
		} 
	,
		{}
	];
	
	messages = chat["Messages"]; 
	
	requestAssoc = <|
		"model" -> model, 
		"messages" -> sanitaze[messages], 
		"temperature" -> temperature, 
		If[# === Nothing, Nothing, "tools" -> #] &@ toolFunction[tools], 
		If[Length[tools] > 0, "tool_choice" -> functionChoice[toolChoice], Nothing]
	|>; 



	requestBody = ExportString[requestAssoc, "RawJSON", CharacterEncoding -> "UTF-8"]; 
	
	request = HTTPRequest[url, <|
		Method -> "POST", 
		"ContentType" -> "application/json", 
		"Headers" -> headers, 
		"Body" -> requestBody
	|>]; 
	
	With[{$request = request, $logger = logger, $requestAssoc = requestAssoc}, 
		URLSubmit[$request, 
			HandlerFunctions -> <|
				"HeadersReceived" -> Function[$logger[<|"Body" -> $requestAssoc, "Event" -> "RequestBody"|>] ], 
				"BodyReceived" -> Function[Module[{responseBody, responseAssoc}, 
					If[#["StatusCode"] === 200, 
						(* responseBody = ExportString[#["Body"], "String"];  *)
						responseAssoc = ImportByteArray[#["BodyByteArray"], "RawJSON", CharacterEncoding -> "UTF-8"]; 

						$logger[<|"Body" -> responseAssoc, "Event" -> "ResponseBody"|>]; 

						If[AssociationQ[responseAssoc], 
							chat["ChatId"] = responseAssoc["id"]; 
							chat["TotalTokens"] = responseAssoc["usage", "total_tokens"]; 
							Append[chat, Join[responseAssoc[["choices", 1, "message"]], <|"date" -> Now|>] ]; 

							If[KeyExistsQ[chat["Messages"][[-1]], "tool_calls"], 
								Module[{
									$cbk,
									msg = chat["Messages"][[-1]]
								}, 
								
									
									$cbk = Function[$result,
									  Do[
										If[StringQ[$result[[ i]]], 
											
											Append[chat, <|
												"role" -> "tool", 
												"content" -> $result[[ i]], 
												"name" -> msg[["tool_calls", i, "function", "name"]], 
												"tool_call_id" -> msg[["tool_calls", i, "id"]],
												"date" -> Now
											|>]; 

										, 
										(*Else*)
											Message[GPTChatCompleteAsync::err, $result]; $Failed		
										];
									  , {i, Length[$result ]}];

									  (* Echo["GPTLink >> After tool calls >> Messages:"]; *)
									  $logger[<||>]; 
									  (* Echo[chat["Messages"] // Length]; *)

									  If[chat["OldMessagesLength"] =!= Length[chat["Messages"] ],
									  	chat["OldMessagesLength"] = Length[chat["Messages"] ];
									  	(* Echo["GPTLink >> Subcall"]; *)
										GPTChatCompleteAsync[chat, callback, opts];
			
									  ,
									  	(* Echo["GPTLink >> Nothing to do. No new messages"]; *)
										callback[chat];
									  ];
									];
									
									
								
									toolHandler[chat["Messages"][[-1]], $cbk];
								];
								
								,
								(*Else*)
								callback[chat];
							
							, 
							(*Else*)
								$logger[<|"Error" -> "No messages provided in the reply"|>]; 
								Message[GPTChatCompleteAsync::err, responseAssoc]; $Failed
							], 
						(*Else*)
							$logger[<|"Error" -> "Response is not valid JSON"|>]; 
							Message[GPTChatCompleteAsync::err, responseAssoc]; $Failed
						], 
						$logger[<|"Error" -> "Response code: "<>ToString[(#["StatusCode"])]|>]; 
						$Failed
					]
				] ]
			|>, 
			HandlerFunctionsKeys -> {"StatusCode", "BodyByteArray", "Headers"}
		]
	]
]; 


GPTChatCompleteAsync[chat_GPTChatObject, prompt: promptPattern, callback: _Symbol | _Function, 
	secondCall: GPTChatComplete | GPTChatCompleteAsync: GPTChatCompleteAsync, opts: OptionsPattern[]] := (
	Append[chat, prompt]; 
	GPTChatCompleteAsync[chat, callback, secondCall, opts]
); 


GPTChatCompleteAsync[prompt: promptPattern, callback: _Symbol | _Function, 
	secondCall: GPTChatComplete | GPTChatCompleteAsync: GPTChatCompleteAsync, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[]}, 
	Append[chat, prompt]; 
	GPTChatCompleteAsync[chat, callback, secondCall, opts]
]; 


Options[GPTChatComplete] = Options[GPTChatCompleteAsync]; 


GPTChatComplete[chat_GPTChatObject, opts: OptionsPattern[]] := 
(TaskWait[GPTChatCompleteAsync[chat, Identity, GPTChatComplete, opts]]; chat); 


GPTChatComplete[chat_GPTChatObject, prompt: promptPattern, opts: OptionsPattern[]] := 
(TaskWait[GPTChatCompleteAsync[chat, prompt, Identity, GPTChatComplete, opts]]; chat); 


GPTChatComplete[prompt: promptPattern, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[]}, TaskWait[GPTChatCompleteAsync[chat, prompt, Identity, GPTChatComplete, opts]]; chat]; 


(* ::Sction:: *)
(*Internal*)


ifAuto[Automatic, value_] := value; 


ifAuto[value_, _] := value; 

defaultToolHandler[message_, cbk_] := With[{},
cbk @ (Table[Module[{func = message[["tool_calls", i, "function", "name"]] // ToExpression},
	Apply[func] @ Values @ ImportByteArray[StringToByteArray @
										message[["tool_calls", i, "function", "arguments"]], "RawJSON", CharacterEncoding -> "UTF-8"
									]
], {i, Length[message[["tool_calls"]]]}])
]


defaultToolFunction[function_Symbol] := 
<|
	"type" -> "function", 
	"function" -> <|
		"name" -> SymbolName[function], 
		"description" -> function::usage, 
		"parameters" -> <|
			"type" -> "object", 
			"properties" -> Apply[Association @* List] @ (
				(
					First[First[DownValues[function]]] /. 
					Verbatim[HoldPattern][function[args___]] :> Hold[args]
				) /. 
				Verbatim[Pattern][$s_Symbol, Verbatim[Blank][$t_]] :> 
				ToString[Unevaluated[$s]] -> <|
					"type" -> ToLowerCase[ToString[$t]], 
					"description" -> ToString[Unevaluated[$s]]
				|>
			)
		|>
	|>
|>; 

defaultToolFunction[list_List] := If[Length[list] > 0, Map[defaultToolFunction] @ list, Nothing]

defaultToolFunction[assoc_Association?AssociationQ] := 
assoc; 


functionChoice[function_Symbol] := 
<|"type" -> "function", "function" -> <|"name" -> SymbolName[function]|>|>; 


functionChoice[Automatic | "auto"] := 
"auto"; 


functionChoice[assoc_Association?AssociationQ] := 
assoc; 


functionChoice[_] := 
"none"; 

sanitaze[list_List] :=  Function[message, KeyDrop[message, "date"] ] /@ list 


(* ::Section:: *)
(*Package Footer*)


End[];


EndPackage[];
