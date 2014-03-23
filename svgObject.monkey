Import svg
Import svgColor
Import svgFunctions
Import tesselate

Class SVGObject
 	Field type:String
	Field x:Float, y:Float, w:Float, h:Float
	Field x1:Float, y1:Float, x2:Float, y2:Float
	Field cx:Float, cy:Float
	Field r:Float, rx:Float, ry:Float
	Field fill:String="none", fillOpacity:Float=1.0
	Field strokeDraw:Bool, stroke:String="#000000", strokeWidth:Float=1.0, strokeOpacity:Float=1.0
	Field transform:Bool, transX:Float, transY:Float, transS:Float=1.0, transR:Float
	Field textAnchor:Float, text:String
	Field poly:Float[1][], polyScale:Float=1.0
	
	Method New(_node:XMLNode, _groupAttributes:XMLNode)
		Self.GetAttributes(_groupAttributes)
		Self.GetAttributes(_node)
	End Method
	
	Method Render:Void()
		'Overloaded
	End Method

	Method GetAttributes:Void(node:XMLNode)
		type = node.name

		If node.GetAttribute("x") x=Float(node.GetAttribute("x"))

		If node.GetAttribute("y") y=Float(node.GetAttribute("y"))
		
		If node.GetAttribute("x1")
			x1=Float(node.GetAttribute("x1"))
			y1=Float(node.GetAttribute("y1"))
			x2=Float(node.GetAttribute("x2"))
			y2=Float(node.GetAttribute("y2"))
			AddToPoly(poly, x1, y1)
			AddToPoly(poly, x2, y2)			
		EndIf

		If node.GetAttribute("width")
			If node.GetAttribute("width").Contains("%")
				x=0 ; w=(SVG.width/100.0)*Int(node.GetAttribute("width")[0..node.GetAttribute("width").Length-1])
			Else
				w=Float(node.GetAttribute("width"))
			EndIf
		EndIf

		If node.GetAttribute("height")
			If node.GetAttribute("height").Contains("%")
				y=0 ; h=(SVG.height/100.0)*Int(node.GetAttribute("height")[0..node.GetAttribute("height").Length-1])
			Else
				h=Float(node.GetAttribute("height"))
			EndIf
			AddToPoly(poly, x, y)
			AddToPoly(poly, x+w, y)
			AddToPoly(poly, x+w, y+h)
			AddToPoly(poly, x, y+h)			
		EndIf
		
		If node.GetAttribute("r") r=Float(node.GetAttribute("r"))
		
		If node.GetAttribute("rx") rx=Float(node.GetAttribute("rx"))
		
		If node.GetAttribute("ry")
			ry=Float(node.GetAttribute("ry"))
			For Local a:Float = 0 Until 360
				Local vx:Float = Cos(a)*rx
				Local vy:Float = Sin(a)*ry
				AddToPoly(poly, (cx+vx), (cy+vy))
			Next
		EndIf
		
		If node.GetAttribute("cx") cx=Float(node.GetAttribute("cx"))
		
		If node.GetAttribute("cy")
			cy=Float(node.GetAttribute("cy"))
			For Local a:Float = 0 Until 360
				Local vx:Float = Cos(a)*r
				Local vy:Float = Sin(a)*r
				AddToPoly(poly, (cx+vx), (cy+vy))
			Next
		EndIf
		
		If node.GetAttribute("points")
			Local points:Float[]=GetPoints(node.GetAttribute("points"))
			For Local l:Int=0 To points.Length-2 Step 2
				AddToPoly(poly, points[l], points[l+1])
			Next
		EndIf
		
		If node.GetAttribute("transform")
			transform=True
			Local pos1:Int, pos2:Int
			' Translate
			pos1=node.GetAttribute("transform").Find("translate")
			If pos1<>-1
				pos2=node.GetAttribute("transform").Find(")", pos1)
				Local xy:String[]=node.GetAttribute("transform")[pos1+10..pos2].Split(" ")
				transX=Float(xy[0])
				transY=Float(xy[1])
			EndIf
			' Rotate
			pos1=node.GetAttribute("transform").Find("rotate")
			If pos1<>-1
				pos2=node.GetAttribute("transform").Find(")", pos1)
				transR=Float(node.GetAttribute("transform")[pos1+7..pos2])
			EndIf
			' Scale
			pos1=node.GetAttribute("transform").Find("scale")
			If pos1<>-1
				pos2=node.GetAttribute("transform").Find(")", pos1)
				transS=Float(node.GetAttribute("transform")[pos1+6..pos2])
			EndIf						
		EndIf

		If node.GetAttribute("style")
			Local style:String=node.GetAttribute("style")
			fill=style[style.Find(":")+1..style.Find(";")]
			If style.Find("stroke") strokeDraw=True
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

		If node.GetAttribute("fill-opacity") fillOpacity=Float(node.GetAttribute("fill-opacity"))

		If node.GetAttribute("opacity") fillOpacity=Float(node.GetAttribute("opacity"))

		If node.GetAttribute("stroke") strokeDraw=True ; stroke=node.GetAttribute("stroke")
		
		If node.GetAttribute("stroke-width") strokeDraw=True ; strokeWidth=Float(node.GetAttribute("stroke-width"))
		
		If node.GetAttribute("stroke-opacity") strokeOpacity=Float(node.GetAttribute("stroke-opacity"))
		
		If fill="none" And strokeDraw=False fill="black"		
	End Method
	
	Method DrawTriangulatedPoly:Void(poly:Float[][], closed:Bool=False)
		#IF TARGET="html5"
			'Poly
			If fill<>"none"
				SetSVGColor(fill)
				SetAlpha(fillOpacity)				
				For Local c:Int=0 to poly.Length-1
					Local scaledPoly:Float[poly[c].Length]
					For Local p:Int=0 Until poly[c].Length
						scaledPoly[p]=poly[c][p]*SVG.scale
					Next
					DrawPoly(scaledPoly)
				Next
			EndIf
	
			'Stroke
			If strokeDraw
				SetSVGColor(stroke)
				SetAlpha(strokeOpacity)			
				If (strokeWidth*SVG.scale)<1 SetAlpha(strokeWidth*SVG.scale)
				For Local line:= Eachin poly
					Tessellator.TriangulateAndDrawPolyline(line, (strokeWidth*SVG.scale), closed, 0, 0, 4.0, SVG.scale)				 
				Next
				If (strokeWidth*SVG.scale)<1 SetAlpha(strokeOpacity)
			EndIf
		#ELSE
			'Poly
			If fill<>"none"
				SetSVGColor(fill)
				SetAlpha(fillOpacity)
				Tessellator.TriangulateAndDrawPolygons(poly, SVG.scale)
			EndIf
	
			'Stroke
			If strokeDraw
				SetSVGColor(stroke)
				SetAlpha(strokeOpacity)
				If (strokeWidth*SVG.scale)<1 SetAlpha(strokeWidth*SVG.scale)
				For Local line:= Eachin poly
					Tessellator.TriangulateAndDrawPolyline(line, (strokeWidth*SVG.scale), closed, 0, 0, 4.0, SVG.scale)			 
				Next
				If (strokeWidth*SVG.scale)<1 SetAlpha(strokeOpacity)
			EndIf
		#ENDIF
	End Method
End Class
