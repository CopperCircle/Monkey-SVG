'--------------------------
' Monkey SVG Parser - v1.0
' By Lee Wade/CopperCircle
'--------------------------

'v1.0 - Refactored code into objects. Added all SVG named colours, added W3.org basic SVG examples, added basic Transform Support
'		modified Tesselate to use scaling, changed basic shapes to use polys.
'v0.9 - Added tesselate v0.8 and multi point L commands.
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
Import svgObject
Import svgShape
Import svgPath
Import svgText

'-------
'Example
'-------
Class SVG_Demo Extends App
	Field test:SVG[]
	Field file:Int
	Field files:String[]
	Field x:Float, y:Float, scale:Float=1.0, rotation:Float
	Field font:Image

	Method OnCreate:Int()
		Local fileList:String="monkey,tiger,graph,text,circle,ellipse,line,rect,polygon,polyline,opacity"
		files=fileList.Split(",")
		For Local l:Int=0 Until files.Length
			test=test.Resize(test.Length+1)
			test[l] = New SVG
			test[l].LoadSVG(files[l]+".svg")			
		Next

		font = LoadImage("font.png", 91)
		SetFont(font)

		SetUpdateRate 60
		Return 0
	End Method

	Method OnUpdate:Int()
		x=MouseX()
		y=MouseY()
		
		If KeyHit(KEY_Z) And file>0 file-=1
		If KeyHit(KEY_X) And file<test.Length-1 file+=1
			
		If KeyDown(KEY_DOWN) scale-=0.02
		If KeyDown(KEY_UP) scale+=0.02
		If KeyDown(KEY_LEFT) rotation+=2
		If KeyDown(KEY_RIGHT) rotation-=2
				
		Return 0
	End Method
	
	Method OnRender:Int()
		Cls(255,255,255)

		test[file].DrawSVG(x, y, rotation, scale)

		SetColor(255,255,255)
		SetAlpha(1)
		DrawText("File: "+files[file]+".svg", 10, 10)				
		DrawText("Cursor keys to rotate/zoom. Z/X to change file.", 10, 25)
		
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
  Private
	Field w:Int, h:Int 
	
  Public
 	Global width:Int, height:Int, scale:Float=1.0
	
	Field x:Float, y:Float, rotation:Float
	Field elements:Stack<SVGObject> = New Stack<SVGObject>	

	Method LoadSVG:Void(_file:String)
		Local _Error:XMLError
		Local file:XMLDoc = ParseXML(LoadString(_file), _Error)
		If file = Null
			Error("doh! could not read "+_file)
		Else
			If file.GetAttribute("width") w=Int(file.GetAttribute("width")) ; width=w
			If file.GetAttribute("height") h=Int(file.GetAttribute("height")) ; height=h
			
			CreateNodes(file)
		EndIf
	End Method
	
	Method CreateNodes:Void(nodes:XMLNode, groupAttributes:XMLNode=New XMLNode)
		For Local node:= EachIn nodes.GetChildren()
			If node.name = "g"
				CreateNodes(node, node)
			Else
				Select node.name
					Case "rect", "circle", "ellipse", "line", "polygon", "polyline"
						elements.Push(New SVGShape(node, groupAttributes))
					Case "path"
						elements.Push(New SVGPath(node, groupAttributes))
					Case "text"
						elements.Push(New SVGText(node, groupAttributes))
				End Select
			EndIf
		Next
	End Method
	
	Method DrawSVG:Void(_x:Float=0, _y:Float=0, _rotation:Float=0, _scale:Float=1.0)
		x=_x ; y=_y ; rotation=_rotation ; scale=_scale
		width=(w*scale) ; height=(h*scale)
		PushMatrix()
		Translate(x,y)
		Rotate(rotation)		
		For Local element:= EachIn elements
			element.Render()
		Next
		PopMatrix()
	End Method
End Class