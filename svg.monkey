#TEXT_FILES += "*.svg"

Strict

Import mojo
Import xml
Import triangulate

Class SVG_Demo Extends App
	Field test:SVG = New SVG
	Field testX:Float, testY:Float, testSX:Float=1, testSY:Float=1, testRotate:Float

	Method OnCreate:Int()
		test.LoadSVG("test.svg")
		'test.LoadSVG("monkey.svg")
		'test.LoadSVG("text.svg")		
		'test.LoadSVG("concave.svg")

		SetUpdateRate 60
		Return 0
	End Method

	Method OnUpdate:Int()
		testX=MouseX()-(test.width/2)
		testY=MouseY()-(test.height/2)
	
		If KeyDown(KEY_DOWN) testSX-=0.01 ; testSY-=0.01
		If KeyDown(KEY_UP) testSX+=0.01 ; testSY+=0.01
		If KeyDown(KEY_LEFT) testRotate+=1
		If KeyDown(KEY_RIGHT) testRotate-=1
		
		If KeyHit(KEY_EQUALS) test.curveSegments+=1
		If KeyHit(KEY_MINUS) test.curveSegments-=1

		Return 0
	End Method
	
	Method OnRender:Int()
		Cls(255,255,255)

		test.DrawSVG(testX, testY, testRotate, testSX, testSY)

		SetColor(255,255,255)
		DrawText("Cursor keys to rotate/zoom, +/- to change curve resolution", 10, 10)
		
		Return 0
	End Method
End Class

Function Main:Int()
	New SVG_Demo
	Return 0
End Function

'-----------

Class SVG
	Const SVG_m:Int = 109 ' MoveTo (Relatively Positioned)
	Const SVG_h:Int = 104 ' Horizontal LineTo (Relatively Positioned)
	Const SVG_v:Int = 118 ' Vertical LineTo (Relatively Positioned)
	Const SVG_l:Int = 108 ' LineTo (Relatively Positioned)
	Const SVG_c:Int = 99  ' CurveTo (Relatively Positioned)
	Const SVG_s:Int = 115 ' Smooth CurveTo (Relatively Positioned)
	Const SVG_q:Int = 113 ' Quadratic Bézier Curve (Relatively Positioned)
	Const SVG_z:Int = 122 ' Close Path
	Const SVG_M:Int = 77  ' MoveTo (Absolutely Positioned)
	Const SVG_H:Int = 72  ' Horizontal LineTo (Absolutely Positioned)
	Const SVG_V:Int = 86  ' Vertical LineTo (Absolutely Positioned)
	Const SVG_L:Int = 76  ' LineTo (Absolutely Positioned)
	Const SVG_C:Int = 67  ' CurveTo (Absolutely Positioned)
	Const SVG_S:Int = 83  ' Smooth CurveTo (Absolutely Positioned)
	Const SVG_Q:Int = 81  ' Quadratic Bézier Curve (Absolutely Positioned)

	Field file:XMLDoc
	Field x:Float, y:Float, rotation:Float, scalex:Float, scaley:Float
	Field width:Int, height:Int, w:Int, h:Int
	Field sx1:Float, sy1:Float
	Field poly:Float[1][], fill:String
	Field curveSegments:Int=10
	Field groupAttributes:Bool
	Field strokeDraw:Bool, strokeColor:String, strokeWidth:Int
	
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
	
	Method DrawSVG:Void(_x:Float=0, _y:Float=0, _rotation:Float=0, _scalex:Float=1, _scaley:Float=1)
		x=_x ; y=_y ; rotation=_rotation ; scalex=_scalex ; scaley=_scaley
		width=(w*scalex) ; height=(h*scaley)
		PushMatrix()
		Rotate(rotation)
		Translate(x,y)
		DrawNodes(file)
		PopMatrix()
	End Method
	
	Method DrawNodes:Void(nodes:XMLNode)
		For Local node:= EachIn nodes.GetChildren()
			SetAttributes(node)
			Select node.name
				Case "rect"
					If strokeDraw
						SetHexColor(strokeColor)
						DrawRect(Float(node.GetAttribute("x"))*scalex, Float(node.GetAttribute("y"))*scaley, Float(node.GetAttribute("width"))*scalex, Float(node.GetAttribute("height"))*scaley)						
						SetHexColor(fill)
						DrawRect((Float(node.GetAttribute("x"))*scalex)+strokeWidth, (Float(node.GetAttribute("y"))*scaley)+strokeWidth, (Float(node.GetAttribute("width"))*scalex)-(strokeWidth*2), (Float(node.GetAttribute("height"))*scaley)-(strokeWidth*2))						
					Else
						DrawRect(Float(node.GetAttribute("x"))*scalex, Float(node.GetAttribute("y"))*scaley, Float(node.GetAttribute("width"))*scalex, Float(node.GetAttribute("height"))*scaley)
					EndIf
					
				Case "circle"
					If strokeDraw
						SetHexColor(strokeColor)
						DrawEllipse(Float(node.GetAttribute("cx"))*scalex, Float(node.GetAttribute("cy"))*scaley, Float(node.GetAttribute("r"))*scalex, Float(node.GetAttribute("r"))*scaley)
						SetHexColor(fill)
						DrawEllipse(Float(node.GetAttribute("cx"))*scalex, Float(node.GetAttribute("cy"))*scaley, (Float(node.GetAttribute("r"))*scalex)-strokeWidth, (Float(node.GetAttribute("r"))*scaley)-strokeWidth)					
					Else					
						DrawEllipse(Float(node.GetAttribute("cx"))*scalex, Float(node.GetAttribute("cy"))*scaley, Float(node.GetAttribute("r"))*scalex, Float(node.GetAttribute("r"))*scaley)
					EndIf

				Case "ellipse"
					If strokeDraw
						SetHexColor(strokeColor)
						DrawEllipse(Float(node.GetAttribute("cx"))*scalex, Float(node.GetAttribute("cy"))*scaley, Float(node.GetAttribute("rx"))*scalex, Float(node.GetAttribute("ry"))*scaley)
						SetHexColor(fill)
						DrawEllipse(Float(node.GetAttribute("cx"))*scalex, Float(node.GetAttribute("cy"))*scaley, (Float(node.GetAttribute("rx"))*scalex)-strokeWidth, (Float(node.GetAttribute("ry"))*scaley)-strokeWidth)					
					Else					
						DrawEllipse(Float(node.GetAttribute("cx"))*scalex, Float(node.GetAttribute("cy"))*scaley, Float(node.GetAttribute("rx"))*scalex, Float(node.GetAttribute("ry"))*scaley)
					EndIf
					
				Case "line"
					DrawLine(Float(node.GetAttribute("x1"))*scalex, Float(node.GetAttribute("y2"))*scaley, Float(node.GetAttribute("x2"))*scalex, Float(node.GetAttribute("y2"))*scaley)
			
				Case "path"
					ParsePath(node.GetAttribute("d"), False)
			
				Case "polygon"
					DrawPolygon(node.GetAttribute("points"))
					
				Case "polyline"
					DrawPolygon(node.GetAttribute("points"))					

				Case "g"
					groupAttributes=False
					SetAttributes(node, True)
					DrawNodes(node)
			End Select
		Next
	End Method
	
	Method SetAttributes:Void(node:XMLNode, group:Bool=False)
		If groupAttributes=False SetColor(0,0,0) ; SetAlpha(1) ; strokeDraw=False ; strokeWidth=1

		If node.GetAttribute("fill") fill=node.GetAttribute("fill") ; SetHexColor(fill)
		If node.GetAttribute("fill-opacity") SetAlpha(Float(node.GetAttribute("fill-opacity")))
		If node.GetAttribute("opacity")
			SetAlpha(Float(node.GetAttribute("opacity")))
			If group groupAttributes=True
		EndIf
		If node.GetAttribute("stroke") strokeDraw=True ; strokeColor=node.GetAttribute("stroke")
		If node.GetAttribute("stroke-width") strokeWidth=Int(node.GetAttribute("stroke-width"))	
	End Method
	
	Method ParsePath:Void(path:String, stroke:Bool)
		Local command:Int, xy:Float[4], tag:Int
	
		For Local c:Int=0 Until path.Length()
			Select path[c]											
				Case SVG_z
					xy=DrawPath(command, xy, path[tag..c], stroke)
					command=path[c] ; tag=0
				Default
					If path[c]>64
						xy=DrawPath(command, xy, path[tag..c], stroke)
						command=path[c] ; tag=c+1
					EndIf		
			End Select
		Next
		
		xy=DrawPath(command, xy, path[tag..path.Length], stroke)

		If stroke=False	DrawTriangulatedPoly(poly)
		poly=New Float[1][]
	End Method
	
	Method DrawPath:Float[](command:Int, xy:Float[], value:String, stroke:Bool)
		Local px:Float=xy[0], py:Float=xy[1]
		Local spiltPaths:Bool=True
		
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
				If stroke DrawLine(px*scalex, py*scaley, Float(value)*scalex, py*scaley)
				px=Float(value)
				AddToPoly(px, py)
						
			Case SVG_h
				If stroke DrawLine(px*scalex, py*scaley, (px+Float(value))*scalex, py*scaley)
				px+=Float(value)
				AddToPoly(px, py)		
				
			Case SVG_V
				If stroke DrawLine(px*scalex, py*scaley, px*scalex, Float(value)*scaley)
				py=Float(value)	
				AddToPoly(px, py)
							
			Case SVG_v
				If stroke DrawLine(px*scalex, py*scaley, px*scalex, (py+Float(value))*scaley)
				py+=Float(value)
				AddToPoly(px, py)		
				
			Case SVG_L
				Local points:Float[]=GetPoints(value)
				If stroke DrawLine(px*scalex, py*scaley, points[0]*scalex, points[1]*scaley)
				px=points[0] ; py=points[1]
				AddToPoly(px, py)		
	
			Case SVG_l
				Local points:Float[]=GetPoints(value)
				If stroke DrawLine(px*scalex, py*scaley, (px+points[0])*scalex, (py+points[1])*scaley)
				px+=points[0] ; py+=points[1]
				AddToPoly(px, py)
				
			Case SVG_C
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(points[0], points[1]), P2.Create(points[2], points[3]), P2.Create(points[4], points[5]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					If stroke DrawPoint(curve[l].X*scalex, curve[l].Y*scaley)
					AddToPoly(curve[l].X, curve[l].Y)
				Next
				sx1=((2*points[4]) - points[2]) ; sy1=((2*points[5]) - points[3])		
				px=points[4] ; py=points[5]				
	
			Case SVG_c
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(px+points[0], py+points[1]), P2.Create(px+points[2], py+points[3]), P2.Create(px+points[4], py+points[5]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					If stroke DrawPoint(curve[l].X*scalex, curve[l].Y*scaley)
					AddToPoly(curve[l].X, curve[l].Y)
				Next
				sx1=px+((2*points[4]) - points[2]) ; sy1=py+((2*points[5]) - points[3])		
				px+=points[4] ; py+=points[5]
				
			Case SVG_S
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(sx1, sy1), P2.Create(points[0], points[1]), P2.Create(points[2], points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					If stroke DrawPoint(curve[l].X*scalex, curve[l].Y*scaley)
					AddToPoly(curve[l].X, curve[l].Y)				
				Next
				sx1=((2*points[2]) - points[0]) ; sy1=((2*points[3]) - points[1])	
				px=points[2] ; py=points[3]			
				
			Case SVG_s
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(sx1, sy1), P2.Create(px+points[0], py+points[1]), P2.Create(px+points[2], py+points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					If stroke DrawPoint(curve[l].X*scalex, curve[l].Y*scaley)
					AddToPoly(curve[l].X, curve[l].Y)			
				Next
				sx1=px+((2*points[2]) - points[0]) ; sy1=py+((2*points[3]) - points[1])
				px+=points[2] ; py+=points[3]
				
			Case SVG_Q
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayQuadratic(P2.Create(px, py), P2.Create(points[0], points[1]), P2.Create(points[2], points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					If stroke DrawPoint(curve[l].X*scalex, curve[l].Y*scaley)
					AddToPoly(curve[l].X, curve[l].Y)			
				Next
				px=points[2] ; py=points[3]
				
			Case SVG_q
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayQuadratic(P2.Create(px, py), P2.Create(px+points[0], py+points[1]), P2.Create(px+points[2], py+points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					If stroke DrawPoint(curve[l].X*scalex, curve[l].Y*scaley)
					AddToPoly(curve[l].X, curve[l].Y)
				Next
				px+=points[2] ; py+=points[3]
			
			Case SVG_z
				If stroke DrawLine(px*scalex, py*scaley, xy[2]*scalex, xy[3]*scaley)
				px=xy[2] ; py=xy[3]
				If spiltPaths poly=poly.Resize(poly.Length+1)
		End Select
		
		Return [px,py,xy[2],xy[3]]
	End Method
	
	Method DrawPolygon:Void(value:String, stroke:Bool=False)
		Local points:Float[]=GetPoints(value)
		For Local l:Int=0 To points.Length-2 Step 2
			AddToPoly(points[l], points[l+1])
		Next
		
		DrawTriangulatedPoly(poly)
		poly=[]
	End Method
	
	Method DrawTriangulatedPoly:Void(poly:Float[][])
		#IF TARGET="html5"
			For Local c:Int=0 to poly.Length-2
				DrawPoly(poly[c])
				SetColor(255,255,255)
			Next
		#ELSE
			For Local c:Int=0 to poly.Length-2
				For Local triangle:Float[] = EachIn Triangulate(poly[c])
					DrawPoly(triangle)
				Next
				SetColor(255,255,255)
			Next
		#ENDIF
	End Method
	
	Method AddToPoly:Void(px:Float, py:Float)
		Local polyCount:Int = poly.Length-1
		poly[polyCount]=poly[polyCount].Resize(poly[polyCount].Length+2)
		poly[polyCount][poly[polyCount].Length-2]=(px*scalex)
		poly[polyCount][poly[polyCount].Length-1]=(py*scalex)
	End Method
	
	Method GetPoints:Float[](value:String)
		Local points:Float[], tag:Int
		
		For Local c:Int=1 Until value.Length()
			Select value[c]
				Case 44 ',		
					points=points.Resize(points.Length+1) ; points[points.Length-1]=Float(value[tag..c]) ; tag=c+1
			
				Case 32 'Space		
					points=points.Resize(points.Length+1) ; points[points.Length-1]=Float(value[tag..c]) ; tag=c+1		
			
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
		
		If Distance(bx, by, pbx, pby)>1 result.AddLast(P2.Create(bx,by)) ; pbx=bx ; pby=by
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

		If Distance(bx, by, pbx, pby)>1 result.AddLast(P2.Create(bx,by)) ; pbx=bx ; pby=by		
	Next
	
	Return result.ToArray()
End Function

Function Distance:Float(x1:Float, y1:Float, x2:Float, y2:Float)
	Local dx:Float = x2 - x1
	Local dy:Float = y2 - y1
	Return Sqrt(dx*dx + dy*dy)
End Function

Function SetHexColor:Void(Hex:String)
	SetColor(HexToDec(Hex[1..3]), HexToDec(Hex[3..5]), HexToDec(Hex[5..7]))
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