'--------------------------
' Monkey SVG Parser - v0.8
' By Lee Wade/CopperCircle
'--------------------------

'v0.8 - Fixed split to sub paths on SVG_M, make sure stroke/polylines scale in size/alpha.
'v0.7 - Refactored some code, added tesselate v0.6, added support for rgb and named colours, added stroke/polyline, added basic text support.
'v0.6 - Added tesselate v0.3, added svg T/t commands.
'v0.5 - Added Difference's Triangulator, fixed polygon bug.
'v0.4 - Fix extra point creation on SVG_M commands.
'v0.3 - Tweaked parser, updated triangulate, added split paths.
'v0.2 - Updated skid's triangulate method.
'v0.1 - Added skid's triangulate method. 

#REM
The MIT License (MIT)

Copyright (c) 2014 Lee Wade

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
#END

#TEXT_FILES += "*.svg"

Strict

Import mojo
Import xml
Import tesselate

Class SVG_Demo Extends App
	Field test:SVG = New SVG
	Field testX:Float, testY:Float, testS:Float=1, testRotate:Float
	Field font:Image

	Method OnCreate:Int()
		test.LoadSVG("graph.svg")
		'test.LoadSVG("tiger.svg")
		'test.LoadSVG("monkey.svg")
		'test.LoadSVG("text.svg")
		
		font = LoadImage("font.png", 91)
		SetFont(font)

		SetUpdateRate 60
		Return 0
	End Method

	Method OnUpdate:Int()
		testX=MouseX()-(test.width/2)
		testY=MouseY()-(test.height/2)
	
		If KeyDown(KEY_DOWN) testS-=0.05
		If KeyDown(KEY_UP) testS+=0.05
		If KeyDown(KEY_LEFT) testRotate+=1
		If KeyDown(KEY_RIGHT) testRotate-=1
		
		If KeyHit(KEY_EQUALS) test.curveSegments+=10
		If KeyHit(KEY_MINUS) test.curveSegments-=10
				
		Return 0
	End Method
	
	Method OnRender:Int()
		Cls(255,255,255)

		test.DrawSVG(testX, testY, testRotate, testS)

		DrawText("Cursor keys to rotate/zoom, +/- to change curve resolution", 10, 10)
		
		Return 0
	End Method
End Class

Function Main:Int()
	New SVG_Demo
	Return 0
End Function

'---------
'SVG Class
'---------
Class SVG
	Const SVG_m:Int = 109 ' MoveTo (Relatively Positioned)
	Const SVG_h:Int = 104 ' Horizontal LineTo (Relatively Positioned)
	Const SVG_v:Int = 118 ' Vertical LineTo (Relatively Positioned)
	Const SVG_l:Int = 108 ' LineTo (Relatively Positioned)
	Const SVG_c:Int = 99  ' CurveTo (Relatively Positioned)
	Const SVG_s:Int = 115 ' Smooth CurveTo (Relatively Positioned)
	Const SVG_q:Int = 113 ' Quadratic Bézier Curve (Relatively Positioned)
	Const SVG_t:Int = 116 ' Smooth Quadratic Bézier Curve (Relatively Positioned)
	Const SVG_z:Int = 122 ' Close Path
	Const SVG_M:Int = 77  ' MoveTo (Absolutely Positioned)
	Const SVG_H:Int = 72  ' Horizontal LineTo (Absolutely Positioned)
	Const SVG_V:Int = 86  ' Vertical LineTo (Absolutely Positioned)
	Const SVG_L:Int = 76  ' LineTo (Absolutely Positioned)
	Const SVG_C:Int = 67  ' CurveTo (Absolutely Positioned)
	Const SVG_S:Int = 83  ' Smooth CurveTo (Absolutely Positioned)
	Const SVG_Q:Int = 81  ' Quadratic Bézier Curve (Absolutely Positioned)
	Const SVG_T:Int = 84  ' Smooth Quadratic Bézier Curve (Absolutely Positioned)

	Field file:XMLDoc
	Field x:Float, y:Float, rotation:Float, scale:Float
	Field width:Int, height:Int, w:Int, h:Int
	Field sx1:Float, sy1:Float
	Field poly:Float[1][]
	Field curveSegments:Int=20
	Field groupAttributes:Bool
	
	'Drawing Atrributes
	Field fill:String, fillOpacity:Bool
	Field strokeDraw:Bool, stroke:String, strokeWidth:Float
	Field textAnchor:Float
	Field attX:Float, attY:Float, attW:Float, attH:Float
	
	Method LoadSVG:Void(_file:String)
		Local _Error:XMLError
		file = ParseXML(LoadString(_file), _Error)
		If file = Null
			Error("doh! could not read "+_file)
		Else
			If file.GetAttribute("width") w=Int(file.GetAttribute("width")) ; width=w
			If file.GetAttribute("height") h=Int(file.GetAttribute("height")) ; height=h
		EndIf
	End Method
	
	Method DrawSVG:Void(_x:Float=0, _y:Float=0, _rotation:Float=0, _scale:Float=1)
		x=_x ; y=_y ; rotation=_rotation ; scale=_scale
		width=(w*scale) ; height=(h*scale)
		PushMatrix()
		Rotate(rotation)
		Translate(x,y)
		DrawNodes(file)
		PopMatrix()
	End Method
	
	Method DrawNodes:Void(nodes:XMLNode)	
		For Local node:= EachIn nodes.GetChildren()
			GetAttributes(node)

			Select node.name
				Case "rect"
					If strokeDraw
						SetSVGColor(stroke)
						DrawRect(attX*scale, attY*scale, attW*scale, attH*scale)						
						SetSVGColor(fill)
						DrawRect((attX*scale)+(strokeWidth*scale), (attY*scale)+(strokeWidth*scale), (attW*scale)-((strokeWidth*scale)*2), (attH*scale)-((strokeWidth*scale)*2))						
					Else
						SetSVGColor(fill)
						DrawRect(attX*scale, attY*scale, attW*scale, attH*scale)
					EndIf
					
				Case "circle"
					If strokeDraw
						SetSVGColor(stroke)
						DrawEllipse(Float(node.GetAttribute("cx"))*scale, Float(node.GetAttribute("cy"))*scale, Float(node.GetAttribute("r"))*scale, Float(node.GetAttribute("r"))*scale)
						SetSVGColor(fill)
						DrawEllipse(Float(node.GetAttribute("cx"))*scale, Float(node.GetAttribute("cy"))*scale, (Float(node.GetAttribute("r"))*scale)-(strokeWidth*scale), (Float(node.GetAttribute("r"))*scale)-(strokeWidth*scale))					
					Else
						SetSVGColor(fill)	
						DrawEllipse(Float(node.GetAttribute("cx"))*scale, Float(node.GetAttribute("cy"))*scale, Float(node.GetAttribute("r"))*scale, Float(node.GetAttribute("r"))*scale)
					EndIf

				Case "ellipse"
					If strokeDraw
						SetSVGColor(stroke)
						DrawEllipse(Float(node.GetAttribute("cx"))*scale, Float(node.GetAttribute("cy"))*scale, Float(node.GetAttribute("rx"))*scale, Float(node.GetAttribute("ry"))*scale)
						SetSVGColor(fill)
						DrawEllipse(Float(node.GetAttribute("cx"))*scale, Float(node.GetAttribute("cy"))*scale, (Float(node.GetAttribute("rx"))*scale)-(strokeWidth*scale), (Float(node.GetAttribute("ry"))*scale)-(strokeWidth*scale))					
					Else
						SetSVGColor(fill)			
						DrawEllipse(Float(node.GetAttribute("cx"))*scale, Float(node.GetAttribute("cy"))*scale, Float(node.GetAttribute("rx"))*scale, Float(node.GetAttribute("ry"))*scale)
					EndIf
					
				Case "line"
					SetSVGColor(fill)
					DrawLine(Float(node.GetAttribute("x1"))*scale, Float(node.GetAttribute("y2"))*scale, Float(node.GetAttribute("x2"))*scale, Float(node.GetAttribute("y2"))*scale)
			
				Case "path"
					If fill="none" And strokeDraw=False fill="black"
					ParsePath(node.GetAttribute("d"))

				Case "polygon"
					If fill="none" And strokeDraw=False fill="black"
					DrawPolygon(node.GetAttribute("points"))
					
				Case "polyline"
					DrawPolyline(node.GetAttribute("points"))
					
				Case "text"
					DrawText(node.value, Float(node.GetAttribute("x"))*scale, Float(node.GetAttribute("y"))*scale, textAnchor, 0.8)

				Case "g"
					groupAttributes=True
					DrawNodes(node)
					groupAttributes=False
			End Select
		Next
	End Method
	
	Method GetAttributes:Void(node:XMLNode, group:Bool=False)
		If groupAttributes=False SetColor(0,0,0) ; SetAlpha(1) ; fill="none" ; strokeDraw=False ; strokeWidth=1
		If fillOpacity fillOpacity=False ; SetAlpha(1)
		
		If node.GetAttribute("x") attX=Float(node.GetAttribute("x"))

		If node.GetAttribute("y") attY=Float(node.GetAttribute("y"))

		If node.GetAttribute("width")
			If node.GetAttribute("width").Contains("%")
				attX=0
				attW=(Float(file.GetAttribute("width"))/100.0)*Int(node.GetAttribute("width")[0..node.GetAttribute("width").Length-1])
			Else
				attW=Float(node.GetAttribute("width"))
			EndIf
		EndIf

		If node.GetAttribute("height")
			If node.GetAttribute("height").Contains("%")
				attY=0
				attH=(Float(file.GetAttribute("height"))/100.0)*Int(node.GetAttribute("height")[0..node.GetAttribute("height").Length-1])
			Else
				attH=Float(node.GetAttribute("height"))
			EndIf
		EndIf

		If node.GetAttribute("style")
			Local style:String=node.GetAttribute("style")
			fill=style[style.Find(":")+1..style.Find(";")]
			SetSVGColor(fill)
			If style.Find("stroke") strokeDraw=True ; stroke="#000000"
		EndIf
		
		If node.GetAttribute("text-anchor")
			Select node.GetAttribute("text-anchor")
				Case "start"
					textAnchor=0
				Case "middle"
					textAnchor=0.5
				Case "end"
					textAnchor=1
			End Select
		EndIf

		If node.GetAttribute("fill") fill=node.GetAttribute("fill")

		If node.GetAttribute("fill-opacity") fillOpacity=True ; SetAlpha(Float(node.GetAttribute("fill-opacity")))

		If node.GetAttribute("opacity") SetAlpha(Float(node.GetAttribute("opacity")))

		If node.GetAttribute("stroke")
			strokeDraw=True ; stroke=node.GetAttribute("stroke") ; strokeWidth=1.0
			If node.GetAttribute("stroke-width") strokeWidth=Float(node.GetAttribute("stroke-width"))
		EndIf
	End Method
	
	Method ParsePath:Void(path:String)
		Local command:Int, xy:Float[4], tag:Int
	
		For Local c:Int=0 Until path.Length()
			Select path[c]
				Case SVG_M
					xy=DrawPath(command, xy, path[tag..c])
					command=path[c] ; tag=c+1
					poly=poly.Resize(poly.Length+1) ' Split Paths			
				Case SVG_z
					xy=DrawPath(command, xy, path[tag..c])
					command=path[c] ; tag=0
					poly=poly.Resize(poly.Length+1) ' Split Paths
				Default
					If path[c]>64
						xy=DrawPath(command, xy, path[tag..c])
						command=path[c] ; tag=c+1
					EndIf		
			End Select
		Next
		
		xy=DrawPath(command, xy, path[tag..path.Length])

		DrawTriangulatedPoly(poly)
		poly=New Float[1][]
	End Method
	
	Method DrawPath:Float[](command:Int, xy:Float[], value:String)
		Local px:Float=xy[0], py:Float=xy[1]
		
		Select command
			Case SVG_M
				Local points:Float[]=GetPoints(value)
				px=points[0] ; py=points[1]
				xy[2]=px ; xy[3]=py
				AddToPoly(px, py)

			Case SVG_m
				Local points:Float[]=GetPoints(value)
				px+=points[0] ; py+=points[1]
				AddToPoly(px, py)

			Case SVG_H
				px=Float(value)
				AddToPoly(px, py)
						
			Case SVG_h
				px+=Float(value)
				AddToPoly(px, py)
				
			Case SVG_V
				py=Float(value)	
				AddToPoly(px, py)

			Case SVG_v
				py+=Float(value)
				AddToPoly(px, py)
				
			Case SVG_L
				Local points:Float[]=GetPoints(value)
				px=points[0] ; py=points[1]
				AddToPoly(px, py)		
	
			Case SVG_l
				Local points:Float[]=GetPoints(value)
				px+=points[0] ; py+=points[1]
				AddToPoly(px, py)
				
			Case SVG_C
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(points[0], points[1]), P2.Create(points[2], points[3]), P2.Create(points[4], points[5]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(curve[l].X, curve[l].Y)
				Next
				sx1=((2*points[4]) - points[2]) ; sy1=((2*points[5]) - points[3])		
				px=points[4] ; py=points[5]				
	
			Case SVG_c
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(px+points[0], py+points[1]), P2.Create(px+points[2], py+points[3]), P2.Create(px+points[4], py+points[5]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(curve[l].X, curve[l].Y)
				Next
				sx1=px+((2*points[4]) - points[2]) ; sy1=py+((2*points[5]) - points[3])		
				px+=points[4] ; py+=points[5]
				
			Case SVG_S
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(sx1, sy1), P2.Create(points[0], points[1]), P2.Create(points[2], points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(curve[l].X, curve[l].Y)				
				Next
				sx1=((2*points[2]) - points[0]) ; sy1=((2*points[3]) - points[1])	
				px=points[2] ; py=points[3]			
				
			Case SVG_s
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(sx1, sy1), P2.Create(px+points[0], py+points[1]), P2.Create(px+points[2], py+points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(curve[l].X, curve[l].Y)			
				Next
				sx1=px+((2*points[2]) - points[0]) ; sy1=py+((2*points[3]) - points[1])
				px+=points[2] ; py+=points[3]
				
			Case SVG_Q
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayQuadratic(P2.Create(px, py), P2.Create(points[0], points[1]), P2.Create(points[2], points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(curve[l].X, curve[l].Y)			
				Next
				sx1=((2*points[2]) - points[0]) ; sy1=((2*points[3]) - points[1])
				px=points[2] ; py=points[3]
				
			Case SVG_q
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayQuadratic(P2.Create(px, py), P2.Create(px+points[0], py+points[1]), P2.Create(px+points[2], py+points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(curve[l].X, curve[l].Y)
				Next
				sx1=px+((2*points[2]) - points[0]) ; sy1=py+((2*points[3]) - points[1])
				px+=points[2] ; py+=points[3]
				
			Case SVG_T
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayQuadratic(P2.Create(px, py), P2.Create(sx1, sy1), P2.Create(points[0], points[1]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(curve[l].X, curve[l].Y)				
				Next
				px=points[0] ; py=points[1]
				sx1=px; sy1=py
				
			Case SVG_t
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayQuadratic(P2.Create(px, py), P2.Create(sx1, sy1), P2.Create(px+points[0], py+points[1]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(curve[l].X, curve[l].Y)			
				Next
				px+=points[0] ; py+=points[1]
				sx1=px; sy1=py
				
			Case SVG_z
				px=xy[2] ; py=xy[3]
		End Select
		
		Return [px,py,xy[2],xy[3]]
	End Method
	
	Method DrawPolygon:Void(value:String)
		Local points:Float[]=GetPoints(value)
		For Local l:Int=0 To points.Length-2 Step 2
			AddToPoly(points[l], points[l+1])
		Next
		
		DrawTriangulatedPoly(poly, 1)
		poly=New Float[1][]
	End Method
	
	Method DrawPolyline:Void(value:String)
		Local points:Float[]=GetPoints(value)
		For Local l:Int=0 To points.Length-2 Step 2
			AddToPoly(points[l], points[l+1])
		Next
		
		If fill<>"none"
			DrawTriangulatedPoly(poly, 1)
		Else
			Local alpha:Float=GetAlpha()				
			If (strokeWidth*scale)<1 SetAlpha((strokeWidth*scale))
			For Local line:= Eachin poly
				Tessellator.TriangulateAndDrawPolyline(line, (strokeWidth*scale), False)				 
			Next
			If (strokeWidth*scale)<1 SetAlpha(alpha)
		EndIf
		poly=New Float[1][]
	End Method
	
	Method DrawTriangulatedPoly:Void(poly:Float[][], clip:Int=2)
		#IF TARGET="html5"
			'Poly
			If fill<>"none"
				SetSVGColor(fill)
				For Local c:Int=0 to poly.Length-clip
					DrawPoly(poly[c])
				Next
			EndIf

			'Stroke
			If strokeDraw
				SetSVGColor(stroke)
				Local alpha:Float=GetAlpha()				
				If (strokeWidth*scale)<1 SetAlpha((strokeWidth*scale))
				For Local line:= Eachin poly
					Tessellator.TriangulateAndDrawPolyline(line, (strokeWidth*scale), False)				 
				Next
				If (strokeWidth*scale)<1 SetAlpha(alpha)
			EndIf
		#ELSE
			'Poly
			If fill<>"none"
				SetSVGColor(fill)
				Tessellator.TriangulateAndDrawPolygons poly
			EndIf

			'Stroke
			If strokeDraw
				SetSVGColor(stroke)
				Local alpha:Float=GetAlpha()				
				If (strokeWidth*scale)<1 SetAlpha((strokeWidth*scale))
				For Local line:= Eachin poly
					Tessellator.TriangulateAndDrawPolyline(line, (strokeWidth*scale), False)				 
				Next
				If (strokeWidth*scale)<1 SetAlpha(alpha)
			EndIf
		#ENDIF
	End Method
	
	Method AddToPoly:Void(px:Float, py:Float)
		Local polyCount:Int = poly.Length-1
		poly[polyCount]=poly[polyCount].Resize(poly[polyCount].Length+2)
		poly[polyCount][poly[polyCount].Length-2]=(px*scale)
		poly[polyCount][poly[polyCount].Length-1]=(py*scale)
	End Method
	
	Method GetPoints:Float[](value:String)
		Local points:Float[], tag:Int

		For Local c:Int=1 Until value.Length()
			Select value[c]
				Case 44 ',
					points=points.Resize(points.Length+1) ; points[points.Length-1]=Float(value[tag..c]) ; tag=c+1
			
				Case 32 'Space
					If c+1<value.Length And value[c+1]<>45 points=points.Resize(points.Length+1) ; points[points.Length-1]=Float(value[tag..c]) ; tag=c+1
			
				Case 45 '-
					points=points.Resize(points.Length+1) ; points[points.Length-1]=Float(value[tag..c]) ; tag=c
			End Select
		Next

		points=points.Resize(points.Length+1) ; points[points.Length-1]=Float(value[tag..value.Length])
		
		Return points
	End Method
End Class

Class P2
	Field X:Float, Y:Float
	
	Function Create:P2(_X:Float, _Y:Float)
		Local p:P2 = New P2
		p.X	= _X
		p.Y	= _Y

		Return p
	End Function	
End Class


Function BezierArrayQuadratic:P2[](p1:P2,p2:P2,p3:P2,segments:Int)
	Local i:Int
	Local result := New List<P2>()
	Local pbx:Float, pby:Float
	
	For Local i := 0 Until segments
		Local t := Float(i) / segments
		
		Local a := Pow((1.0 - t), 2.0)
		Local b := 2.0 * t * (1.0 - t)
		Local c := Pow(t, 2.0)
		
		Local bx := a * p1.X + b * p2.X + c * p3.X
		Local by := a * p1.Y + b * p2.Y + c * p3.Y
		If PointsAreIdentical(bx, by, pbx, pby)=False result.AddLast(P2.Create(bx,by)) ; pbx=bx ; pby=by
	Next
	
	Return result.ToArray()
End Function

Function BezierArrayCubic:P2[](p1:P2,p2:P2,p3:P2,p4:P2,segments:Int)
	Local i:Int
	Local result := New List<P2>()
	Local pbx:Float, pby:Float	
	
	For Local i := 0 Until segments
	    Local t := Float(i) / segments
	
	    Local a := Pow((1.0 - t), 3.0)
	    Local b := 3.0 * t * Pow((1.0 - t), 2.0)
	    Local c := 3.0 * Pow(t, 2.0) * (1.0 - t)
	    Local d := Pow(t, 3.0)
	
	    Local bx := a * p1.X + b * p2.X + c * p3.X + d * p4.X
	    Local by := a * p1.Y + b * p2.Y + c * p3.Y + d * p4.Y

		If PointsAreIdentical(bx, by, pbx, pby)=False result.AddLast(P2.Create(bx,by)) ; pbx=bx ; pby=by	
	Next
	
	Return result.ToArray()
End Function

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