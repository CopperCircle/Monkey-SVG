Import constants
Import svgObject

Class SVGPath Extends SVGObject
	Field curveSegments:Int=20
	Field sx1:Float, sy1:Float
	
	Method New(_node:XMLNode, _groupAttributes:XMLNode)
		Super.New(_node, _groupAttributes)
		ParsePath(_node.GetAttribute("d"))
	End Method

	Method Render:Void()
		If transform
			PushMatrix()
			Translate(transX*SVG.scale, transY*SVG.scale)		
			Scale(transS, transS)
			Rotate(-transR)				
		EndIf
		
		DrawTriangulatedPoly(poly, False)

		If transform PopMatrix()
	End Method
	
	Method ParsePath:Void(path:String)
		Local command:Int, xy:Float[4], tag:Int
	
		For Local c:Int=0 Until path.Length()
			Select path[c]
				Case SVG_M
					xy=CreatePath(command, xy, path[tag..c])
					command=path[c] ; tag=c+1
					poly=poly.Resize(poly.Length+1) ' Split Paths			
				Case SVG_z
					xy=CreatePath(command, xy, path[tag..c])
					command=path[c] ; tag=0
					poly=poly.Resize(poly.Length+1) ' Split Paths
				Default
					If path[c]>64
						xy=CreatePath(command, xy, path[tag..c])
						command=path[c] ; tag=c+1
					EndIf		
			End Select
		Next
		
		xy=CreatePath(command, xy, path[tag..path.Length])
	End Method
	
	Method CreatePath:Float[](command:Int, xy:Float[], value:String)
		Local px:Float=xy[0], py:Float=xy[1]
		
		Select command
			Case SVG_M
				Local points:Float[]=GetPoints(value)
				px=points[0] ; py=points[1]
				xy[2]=px ; xy[3]=py
				AddToPoly(poly, px, py)

			Case SVG_m
				Local points:Float[]=GetPoints(value)
				px+=points[0] ; py+=points[1]
				AddToPoly(poly, px, py)

			Case SVG_H
				px=Float(value)
				AddToPoly(poly, px, py)
						
			Case SVG_h
				px+=Float(value)
				AddToPoly(poly, px, py)
				
			Case SVG_V
				py=Float(value)	
				AddToPoly(poly, px, py)

			Case SVG_v
				py+=Float(value)
				AddToPoly(poly, px, py)
				
			Case SVG_L
				Local points:Float[]=GetPoints(value)
				For Local p:Int=0 To points.Length-1 Step 2
					px=points[p] ; py=points[p+1]
					AddToPoly(poly, px, py)
				Next		
	
			Case SVG_l
				Local points:Float[]=GetPoints(value)
				px+=points[0] ; py+=points[1]
				AddToPoly(poly, px, py)
				
			Case SVG_C
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(points[0], points[1]), P2.Create(points[2], points[3]), P2.Create(points[4], points[5]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(poly, curve[l].X, curve[l].Y)
				Next
				sx1=((2*points[4]) - points[2]) ; sy1=((2*points[5]) - points[3])		
				px=points[4] ; py=points[5]				
	
			Case SVG_c
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(px+points[0], py+points[1]), P2.Create(px+points[2], py+points[3]), P2.Create(px+points[4], py+points[5]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(poly, curve[l].X, curve[l].Y)
				Next
				sx1=px+((2*points[4]) - points[2]) ; sy1=py+((2*points[5]) - points[3])		
				px+=points[4] ; py+=points[5]
				
			Case SVG_S
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(sx1, sy1), P2.Create(points[0], points[1]), P2.Create(points[2], points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(poly, curve[l].X, curve[l].Y)				
				Next
				sx1=((2*points[2]) - points[0]) ; sy1=((2*points[3]) - points[1])	
				px=points[2] ; py=points[3]			
				
			Case SVG_s
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayCubic(P2.Create(px, py), P2.Create(sx1, sy1), P2.Create(px+points[0], py+points[1]), P2.Create(px+points[2], py+points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(poly, curve[l].X, curve[l].Y)			
				Next
				sx1=px+((2*points[2]) - points[0]) ; sy1=py+((2*points[3]) - points[1])
				px+=points[2] ; py+=points[3]
				
			Case SVG_Q
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayQuadratic(P2.Create(px, py), P2.Create(points[0], points[1]), P2.Create(points[2], points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(poly, curve[l].X, curve[l].Y)			
				Next
				sx1=((2*points[2]) - points[0]) ; sy1=((2*points[3]) - points[1])
				px=points[2] ; py=points[3]
				
			Case SVG_q
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayQuadratic(P2.Create(px, py), P2.Create(px+points[0], py+points[1]), P2.Create(px+points[2], py+points[3]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(poly, curve[l].X, curve[l].Y)
				Next
				sx1=px+((2*points[2]) - points[0]) ; sy1=py+((2*points[3]) - points[1])
				px+=points[2] ; py+=points[3]
				
			Case SVG_T
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayQuadratic(P2.Create(px, py), P2.Create(sx1, sy1), P2.Create(points[0], points[1]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(poly, curve[l].X, curve[l].Y)				
				Next
				px=points[0] ; py=points[1]
				sx1=px; sy1=py
				
			Case SVG_t
				Local points:Float[]=GetPoints(value)
				Local curve:P2[]=BezierArrayQuadratic(P2.Create(px, py), P2.Create(sx1, sy1), P2.Create(px+points[0], py+points[1]), curveSegments)
				For Local l:Int = 0 To curve.Length-1
					AddToPoly(poly, curve[l].X, curve[l].Y)			
				Next
				px+=points[0] ; py+=points[1]
				sx1=px; sy1=py
				
			Case SVG_z
				px=xy[2] ; py=xy[3]
		End Select
		
		Return [px,py,xy[2],xy[3]]
	End Method	
End Class