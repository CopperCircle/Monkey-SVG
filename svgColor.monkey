Import mojo

Function SetSVGColor:Void(colour:String)
	If colour.Contains("rgb")	'RGB
		Local RGB:String[]=colour[4..colour.Length-1].Split(",")
		SetColor(Int(RGB[0]), Int(RGB[1]), Int(RGB[2]))
	ElseIf colour.Contains("#")	'HEX
		SetColor(HexToDec(colour[1..3]), HexToDec(colour[3..5]), HexToDec(colour[5..7]))
	Else
		Select colour			'Named
			Case "none" SetColor(0,0,0)
			Case "aqua" SetColor(0,255,255)
			Case "black" SetColor(0,0,0)
			Case "fuchsia" SetColor(255,0,255)
			Case "gray" SetColor(128,128,128)
			Case "lime" SetColor(0,255,0)
			Case "maroon" SetColor(128,0,0)
			Case "navy" SetColor(0,0,128)
			Case "olive" SetColor(128,128,0)
			Case "purple" SetColor(128,0,128)
			Case "silver" SetColor(192,192,192)
			Case "teal" SetColor(0,128,128)
			Case "white" SetColor(255,255,255)
			Case "yellow" SetColor(255,255,0)
			Case "red" SetColor(255,0,0)
			Case "green" SetColor(0,128,0)			
			Case "blue" SetColor(0,0,255)
			'
			Case "aliceblue" SetColor(240, 248, 255) 
			Case "antiquewhite" SetColor(250, 235, 215) 
			Case "aqua" SetColor( 0, 255, 255) 
			Case "aquamarine" SetColor(127, 255, 212) 
			Case "azure" SetColor(240, 255, 255) 
			Case "beige" SetColor(245, 245, 220) 
			Case "bisque" SetColor(255, 228, 196) 
			Case "blanchedalmond" SetColor(255, 235, 205) 
			Case "blueviolet" SetColor(138, 43, 226) 
			Case "brown" SetColor(165, 42, 42) 
			Case "burlywood" SetColor(222, 184, 135) 
			Case "cadetblue" SetColor( 95, 158, 160) 
			Case "chartreuse" SetColor(127, 255, 0) 
			Case "chocolate" SetColor(210, 105, 30) 
			Case "coral" SetColor(255, 127, 80) 
			Case "cornflowerblue" SetColor(100, 149, 237) 
			Case "cornsilk" SetColor(255, 248, 220) 
			Case "crimson" SetColor(220, 20, 60) 
			Case "darkblue" SetColor( 0, 0, 139) 
			Case "darkcyan" SetColor( 0, 139, 139) 
			Case "darkgoldenrod" SetColor(184, 134, 11) 
			Case "darkgray" SetColor(169, 169, 169) 
			Case "darkgreen" SetColor( 0, 100, 0) 
			Case "darkgrey" SetColor(169, 169, 169) 
			Case "darkkhaki" SetColor(189, 183, 107) 
			Case "darkmagenta" SetColor(139, 0, 139) 
			Case "darkolivegreen" SetColor( 85, 107, 47) 
			Case "darkorange" SetColor(255, 140, 0) 
			Case "darkorchid" SetColor(153, 50, 204) 
			Case "darkred" SetColor(139, 0, 0) 
			Case "darksalmon" SetColor(233, 150, 122) 
			Case "darkseagreen" SetColor(143, 188, 143) 
			Case "darkslateblue" SetColor( 72, 61, 139) 
			Case "darkslategray" SetColor( 47, 79, 79) 
			Case "darkslategrey" SetColor( 47, 79, 79) 
			Case "darkturquoise" SetColor( 0, 206, 209) 
			Case "darkviolet" SetColor(148, 0, 211) 
			Case "deeppink" SetColor(255, 20, 147) 
			Case "deepskyblue" SetColor( 0, 191, 255) 
			Case "dimgray" SetColor(105, 105, 105) 
			Case "dimgrey" SetColor(105, 105, 105) 
			Case "dodgerblue" SetColor( 30, 144, 255) 
			Case "firebrick" SetColor(178, 34, 34) 
			Case "floralwhite" SetColor(255, 250, 240) 
			Case "forestgreen" SetColor( 34, 139, 34) 
			Case "fuchsia" SetColor(255, 0, 255) 
			Case "gainsboro" SetColor(220, 220, 220) 
			Case "ghostwhite" SetColor(248, 248, 255) 
			Case "gold" SetColor(255, 215, 0) 
			Case "goldenrod" SetColor(218, 165, 32) 
			Case "greenyellow" SetColor(173, 255, 47) 
			Case "honeydew" SetColor(240, 255, 240) 
			Case "hotpink" SetColor(255, 105, 180) 
			Case "indianred" SetColor(205, 92, 92) 
			Case "indigo" SetColor( 75, 0, 130) 
			Case "ivory" SetColor(255, 255, 240) 
			Case "khaki" SetColor(240, 230, 140) 
			Case "lavender" SetColor(230, 230, 250) 
			Case "lavenderblush" SetColor(255, 240, 245) 
			Case "lawngreen" SetColor(124, 252, 0) 
			Case "lemonchiffon" SetColor(255, 250, 205) 
			Case "lightblue" SetColor(173, 216, 230) 
			Case "lightcoral" SetColor(240, 128, 128) 
			Case "lightcyan" SetColor(224, 255, 255) 
			Case "lightgoldenrodyellow" SetColor(250, 250, 210) 
			Case "lightgray" SetColor(211, 211, 211) 
			Case "lightgreen" SetColor(144, 238, 144) 
			Case "lightgrey" SetColor(211, 211, 211) 
			Case "lightpink" SetColor(255, 182, 193) 
			Case "lightsalmon" SetColor(255, 160, 122) 
			Case "lightseagreen" SetColor( 32, 178, 170) 
			Case "lightskyblue" SetColor(135, 206, 250) 
			Case "lightslategray" SetColor(119, 136, 153) 
			Case "lightslategrey" SetColor(119, 136, 153) 
			Case "lightsteelblue" SetColor(176, 196, 222) 
			Case "lightyellow" SetColor(255, 255, 224) 
			Case "lime" SetColor( 0, 255, 0) 
			Case "limegreen" SetColor( 50, 205, 50) 
			Case "linen" SetColor(250, 240, 230) 
			Case "maroon" SetColor(128, 0, 0) 
			Case "mediumaquamarine" SetColor(102, 205, 170) 
			Case "mediumblue" SetColor( 0, 0, 205) 
			Case "mediumorchid" SetColor(186, 85, 211) 
			Case "mediumpurple" SetColor(147, 112, 219) 
			Case "mediumseagreen" SetColor( 60, 179, 113) 
			Case "mediumslateblue" SetColor(123, 104, 238) 
			Case "mediumspringgreen" SetColor( 0, 250, 154) 
			Case "mediumturquoise" SetColor( 72, 209, 204) 
			Case "mediumvioletred" SetColor(199, 21, 133) 
			Case "midnightblue" SetColor( 25, 25, 112) 
			Case "mintcream" SetColor(245, 255, 250) 
			Case "mistyrose" SetColor(255, 228, 225) 
			Case "moccasin" SetColor(255, 228, 181) 
			Case "navajowhite" SetColor(255, 222, 173) 
			Case "navy" SetColor( 0, 0, 128) 
			Case "oldlace" SetColor(253, 245, 230) 
			Case "olive" SetColor(128, 128, 0) 
			Case "olivedrab" SetColor(107, 142, 35) 
			Case "orange" SetColor(255, 165, 0) 
			Case "orangered" SetColor(255, 69, 0) 
			Case "orchid" SetColor(218, 112, 214) 
			Case "palegoldenrod" SetColor(238, 232, 170) 
			Case "palegreen" SetColor(152, 251, 152) 
			Case "paleturquoise" SetColor(175, 238, 238) 
			Case "palevioletred" SetColor(219, 112, 147) 
			Case "papayawhip" SetColor(255, 239, 213) 
			Case "peachpuff" SetColor(255, 218, 185) 
			Case "peru" SetColor(205, 133, 63) 
			Case "pink" SetColor(255, 192, 203) 
			Case "plum" SetColor(221, 160, 221) 
			Case "powderblue" SetColor(176, 224, 230) 
			Case "purple" SetColor(128, 0, 128) 
			Case "rosybrown" SetColor(188, 143, 143) 
			Case "royalblue" SetColor( 65, 105, 225) 
			Case "saddlebrown" SetColor(139, 69, 19) 
			Case "salmon" SetColor(250, 128, 114) 
			Case "sandybrown" SetColor(244, 164, 96) 
			Case "seagreen" SetColor( 46, 139, 87) 
			Case "seashell" SetColor(255, 245, 238) 
			Case "sienna" SetColor(160, 82, 45) 
			Case "silver" SetColor(192, 192, 192) 
			Case "skyblue" SetColor(135, 206, 235) 
			Case "slateblue" SetColor(106, 90, 205) 
			Case "slategray" SetColor(112, 128, 144) 
			Case "slategrey" SetColor(112, 128, 144) 
			Case "snow" SetColor(255, 250, 250) 
			Case "springgreen" SetColor( 0, 255, 127) 
			Case "steelblue" SetColor( 70, 130, 180) 
			Case "tan" SetColor(210, 180, 140) 
			Case "teal" SetColor( 0, 128, 128) 
			Case "thistle" SetColor(216, 191, 216) 
			Case "tomato" SetColor(255, 99, 71) 
			Case "turquoise" SetColor( 64, 224, 208) 
			Case "violet" SetColor(238, 130, 238) 
			Case "wheat" SetColor(245, 222, 179) 
			Case "whitesmoke" SetColor(245, 245, 245) 
			Case "yellowgreen" SetColor(154, 205, 50)	
		End Select
	EndIf
End Function

Function HexToDec:Int(Hex:String)
	Local Value:Int = 0
	Local Char:Int =0
	
	If Hex.Length() <8
		'add 0's to beginning of thing so FF becomes 000000FF
		Local TempHex:String = ""
		For Char =Hex.Length() To 7
			TempHex += "0"
		Next
		TempHex += Hex
		Hex = TempHex
	Endif
	
	Local Conversion:Int = 1		'starts at 1 then 16, 256, 4096, 
	'now the characters are 'eight bytes' now begin conversion
	For Char=7 To 0 Step -1
		If Hex[ Char ]<58
			Value += ( ( Hex[ Char ] -48 ) *Conversion )
		Else
			Value += ( ( Hex[ Char ] -55 ) *Conversion )
		Endif			
		Conversion *= 16	'multiply conversion by 16 for next byte
	Next

	Return Value
End Function