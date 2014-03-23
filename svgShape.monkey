Import svgObject

Class SVGShape Extends SVGObject
	Method New(_node:XMLNode, _groupAttributes:XMLNode)
		Super.New(_node, _groupAttributes)
	End Method

	Method Render:Void()
		If transform
			PushMatrix()
			Translate(transX*SVG.scale, transY*SVG.scale)		
			Scale(transS, transS)
			Rotate(-transR)				
		EndIf
		
		Select type
			Case "rect"
				DrawTriangulatedPoly(poly, True)

			Case "circle"
				DrawTriangulatedPoly(poly, True)			

			Case "ellipse"
				DrawTriangulatedPoly(poly, True)
				
			Case "line"
				DrawTriangulatedPoly(poly, False)

			Case "polyline"
				DrawTriangulatedPoly(poly, False)
				
			Case "polygon"
				DrawTriangulatedPoly(poly, True)
		
		End Select
		
		If transform PopMatrix()
	End Method
End Class