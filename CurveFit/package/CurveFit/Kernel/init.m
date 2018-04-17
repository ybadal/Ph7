(* ::Package:: *)

(* initialization file for the CurveFit data analysis package CurveFit` *)
If[$VersionNumber >= 7.0,
	If[!MemberQ[$Packages,"CurveFit`"]||ChoiceDialog[
			Beep[];
			"Are you Sure?\nReloading the CurveFit package will "<>
			"erase all data and repair any corrupted function "<>
			"definitions. If you just need to restore the CurveFit "<>
			"palettes, then choose \"Restart CurveFit\".",
			{"Restart CurveFit":>(CurveFit;False),
			 " Cancel "->False,
			 "Reload Package"->True},
			WindowFloating->True],
		Get["CurveFit`CurveFit`"];
		Get["CurveFit`Palette`"];
		Print["CurveFit for Mathematica v7.x thru v11.x, Version 1.96, 4/4/2018"];
		Print["Caltech Sophomore Physics Labs, Pasadena, CA"];
		CurveFit`Palette`CurveFit
	],
	Block[{CurveFit},
		CurveFit::version = "You need Mathematica Version 7 or later for this package!";
		Message[CurveFit::version];
	],
	Block[{CurveFit},
		CurveFit::version = "You need Mathematica Version 7 or later for this package!";
		Message[CurveFit::version];
	]
]

