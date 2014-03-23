Import svgObject

Class SVGText Extends SVGObject
	Method New(_node:XMLNode, _groupAttributes:XMLNode)
		Super.New(_node, _groupAttributes)
		text=_node.value
	End Method

	Method Render:Void()
		If transform
			PushMatrix()
			Translate(transX*SVG.scale, transY*SVG.scale)		
			Scale(transS, transS)
			Rotate(-transR)				
		EndIf
		
		DrawText(text, x*SVG.scale, y*SVG.scale, textAnchor, 0.8)
		
		If transform PopMatrix()
	End Method
End Class