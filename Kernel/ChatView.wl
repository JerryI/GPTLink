(* ::Package:: *)

BeginPackage["KirillBelov`GPTLink`ChatView`", {
    "KirillBelov`GPTLink`",
    "CoffeeLiqueur`Misc`Events`",
    "CoffeeLiqueur`Misc`WLJS`Transport`",
    "CoffeeLiqueur`Extensions`Boxes`"
}];

ChatView::usage = 
"ChatView[a_GPTChatObject] provides an interactive widget for a chat object";

Begin["`Private`"];


chatRenderer;

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
      
      ViewBox[m, chatRenderer[messages // Offload, channel] ]
    ]
] /; TrueQ[Internal`Kernel`WLJSQ]

ChatView /: MakeBoxes[m: ChatView[a_GPTChatObject], WLXForm] := With[{messages = Unique["gptLink"]},
    messages = KeyTake[#, {"role", "content"}] &/@ (a["Messages"]);

    With[{channel = CreateUUID[]},
      EventHandler[channel, Function[prompt, With[{decoded = URLDecode[prompt]},
        GPTChatCompleteAsync[a, decoded, Function[data,
            messages = KeyTake[#, {"role", "content"}] &/@ a["Messages"]
        ] ] ];
      ] ];
      
      chatRenderer[messages // Offload, channel] // CreateFrontEndObject
    ]
]

ChatView /: MakeBoxes[m: ChatView[a_GPTChatObject], form_] := With[{},
    Echo["This feature is only available in WLJS Notebook. See https://wljs.io/"];
    MakeBoxes[$Failed, form]
]


End[];
EndPackage[];