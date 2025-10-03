(* ::Package:: *)

BeginPackage["KirillBelov`GPTLink`ChatView`", {
    "KirillBelov`GPTLink`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`Language`",
    "JerryI`Misc`WLJS`Transport`",
    "CoffeeLiqueur`Extensions`Boxes`",
    "CoffeeLiqueur`Extensions`RuntimeTools`"
}];

ChatView::usage = 
"ChatView[a_GPTChatObject] provides an interactive widget for a chat object";

Begin["`Private`"];


(* ::Section:: *)
(*Internal*)


$directory = 
ParentDirectory[DirectoryName[$InputFileName]]; 


ChatView /: MakeBoxes[m: ChatView[a_GPTChatObject], StandardForm] := With[{messages = Unique["gptLink"]},
    messages = KeyTake[#, {"role", "content"}] &/@ (a["Messages"]);

    With[{channel = CreateUUID[]},
      EventHandler[channel, Function[prompt, With[{decoded = URLDecode[prompt]},
        GPTChatCompleteAsync[a, decoded, Function[data,
            messages = KeyTake[#, {"role", "content"}] &/@ a["Messages"]
        ] ] ];
      ] ];
      
      ViewBox[m, ChatView[messages // Offload, channel] ]
    ]
] /; TrueQ[Internal`Kernel`WLJSQ]

ChatView /: MakeBoxes[m: ChatView[a_GPTChatObject], StandardForm] := With[{
    msg = Style["This feature is only available in WLJS Notebook. See https://wljs.io/", Background->Yellow]
},
    MakeBoxes[msg, StandardForm]
]

(* ::Section:: *)
(*Package Footer*)


End[];


EndPackage[];