Function AddToPoly:Void(poly:Float[][], px:Float, py:Float)
	Local polyCount:Int = poly.Length-1
	poly[polyCount]=poly[polyCount].Resize(poly[polyCount].Length+2)
	poly[polyCount][poly[polyCount].Length-2]=px
	poly[polyCount][poly[polyCount].Length-1]=py
End Function

Function GetPoints:Float[](value:String)
	Local points:Float[], tag:Int
	
	For Local c:Int=1 Until value.Length()
		Select value[c]
			Case 44 ',
				points=points.Resize(points.Length+1) ; points[points.Length-1]=Float(value[tag..c]) ; tag=c+1
		
			Case 32 'Space
				If c+1<value.Length And value[c+1]<>45 And value[c+1]<>32 points=points.Resize(points.Length+1) ; points[points.Length-1]=Float(value[tag..c]) ; tag=c+1
		
			Case 45 '-
				points=points.Resize(points.Length+1) ; points[points.Length-1]=Float(value[tag..c]) ; tag=c
		End Select
	Next

	points=points.Resize(points.Length+1) ; points[points.Length-1]=Float(value[tag..value.Length])
	
	Return points
End Function

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

Function PointsAreIdentical:Bool(x1:Float,y1:Float,x2:Float,y2:float)
	' Manhatten distance should do
	If Abs(x1-x2) < 0.005
		If Abs(y1-y2) < 0.005	 		
			Return True
		EndIf
	EndIf

	Return False
End Function