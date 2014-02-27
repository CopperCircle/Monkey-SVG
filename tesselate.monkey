Strict

' tesselate.monkey
' Poly earclipper that deals with holes
' Chuck full of usefull routines.
' () 2014.02.24 - Peter Scheutz aka Difference

' 2014.02.26
' v0.3  - changed Tessellate() to a while loop, keeping problem points for examination 
'		- better detection of identical points in incomming poly
' 		- check for intersection with all polys in HoleMerge
'		- check that two adjacent lines connects from outer to inner poly (only use one)
'
' 2014.02.24 
' v0.2 - first release version

#rem
The MIT License (MIT)

Copyright (c)  2014   Peter Scheutz , portions by Darel Rex Finley, Nathan Mercer, Paul Bourke

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
#end

Import mojo


Global gIntersecsX:Float
Global gIntersecsY:Float

Global gIntersectionCorner:Int
Global EPS:Float = 0.001


Function PointsAreIdentical:Bool(x1:Float,y1:Float,x2:Float,y2:float)


	' Manhatten distance should do
	If Abs(x1-x2) < EPS
		If Abs(y1-y2) < EPS	 		
			Return True

		Endif
	Endif

	Return False
End Function


'Polygon with holes 
Class CompoundPolygon 
	Field points:Float[]
 	Field triangleindexes:Int[] 
	Field trianglecount:Int
	Field polyindexes:IndexedPolygon[] 
 
 
 	Field mergedpoly:IndexedPolygon
 
	Method New(newpoints:Float[][])

	 

' ****************** BEGIN Clean up input arrays
' IMO this sould be done by the parser
		
		If Not newpoints.Length() Return ' no polygons
		If Not newpoints[0].Length() Return 'no point in first poly
 
	
		' sometimes we arepassed empty polys
		If newpoints[newpoints.Length()-1].Length() = 0
			newpoints = newpoints[..(newpoints.Length()-1)] 
		Endif		 
			

 '#rem			

 		' remove endpoints that doubles firstpoint
		For Local i:Int = 0 Until newpoints.Length()

			Local skiplist:= New IntList	
			
			Repeat
				skiplist.Clear()
			
				' a list af adjacent vertices that are identical
				' we need to remove them or they will confuse the earclipper
				If newpoints[i].Length()>4
			
					Local p3:Int = newpoints[i].Length() - 4
					Local p2:Int = newpoints[i].Length() - 2
					
					For Local p1:Int = 0 Until newpoints[i].Length() Step 2
					
 					 	If PointsAreIdentical(newpoints[i][p1],newpoints[i][p1+1],newpoints[i][p2],newpoints[i][p2+1])
						'	skiplist.AddLast p2
						'Elseif Abs(PolygonArea(newpoints[i],[p1,p2,p3])) < 0.02
 
							skiplist.AddLast p2
						Endif
						
						p3 = p2
						p2 = p1
						
					Next	
				
					Local skip:Int
					
					For Local n:Int = 0 Until newpoints[i].Length() Step 2	
					
						If skiplist.Contains(n)
							skip +=2
						Endif
					
						If n+skip<newpoints[i].Length()
							newpoints[i][n] = newpoints[i][n+skip] 
							newpoints[i][n+1] = newpoints[i][n+skip+1] 
						Endif
					
					Next
					
					 If skip Then 	
					 	' truncate array to new length()
						newpoints[i] = newpoints[i][..(newpoints[i].Length()-skip)]
					Endif
				
				Endif
			Until  skiplist.IsEmpty()  	
		Next
			 
'#End 		
' ****************** END Clean up input arrays
 		

		' make a pool with all points
		' this is important when drawing the triangles later, or there can be gaps/flickering
		' if we douplicate points because of floating point inacuracy 
		points =  FloatArraysMerge(newpoints)	
		
		' make an array for Polygons 
		polyindexes = 	New IndexedPolygon[newpoints.Length()]
		
	
		'make polygons that references the points in the pool
		' each value is an offset to the x coordinate in the pool
		Local index:Int
		For Local i:Int = 0 Until newpoints.Length() 
			polyindexes[i] = New IndexedPolygon(points) 
			
			For Local n:Int = 0 Until newpoints[i].Length()-1 Step 2			
				polyindexes[i].Push(index)
				index += 2
			Next
		Next
		
	 
		' merge holes and polygon
		' OBS for now assume all polys with index> 0 are holes 
		' start by adding first poly, then merge holes	
		mergedpoly  = polyindexes[0]
		
		For Local holeindex:Int = 1 Until polyindexes.Length() 
			mergedpoly = HoleMerge(points,mergedpoly,polyindexes[holeindex],polyindexes)
		Next
			
		If mergedpoly.Length()<2 Then Return
 
 		' triangulate it
		Tessellate(points,mergedpoly)

	End Method


	
	Method Draw:Void()		
		DrawTriangles(points,triangleindexes)
		
	   Return ' below this for debug only
		
		'Draw Problem points / parts that was not clipped 		
		SetColor 255,0,255
 		
			If mergedpoly.Length()>-2
		
				Local cords:= mergedpoly.ToPoints()
			
				DrawPoly(cords)
				
				SetAlpha 1
				SetColor 255,255,0
				For Local i:Int = 0 Until cords.Length() Step 2
				
					DrawText i,cords[i]+i*.002,cords[i+1]+i*.002
				
				Next
			Endif
 		
	End Method	
	
	Method Tessellate:Void(points:Float[],indexedpoly:IndexedPolygon)
 	
		' a polygon can be triangulated with vertexcount - 2 triangles
		trianglecount = indexedpoly.Length() - 2
		
		If trianglecount<=0 Then Return 
 	
		' an arrry for triangleindexes to be passed to DrawTriangles()
		triangleindexes = New Int[trianglecount*3]	
 
		Local i:int
 
		While indexedpoly.ClipEar()
 			triangleindexes[i] = indexedpoly.ear[0]
			triangleindexes[i+1] = indexedpoly.ear[1]
			triangleindexes[i+2] = indexedpoly.ear[2]	
			i +=3
		Wend

		' did we loose some in the fire?
		If i< triangleindexes.Length()
			triangleindexes = triangleindexes[..i]
			'Print "TRIS LOST: " + (trianglecount-i/3)			
			trianglecount = i/3
		Endif
		

	End Method
	
End Class

	

Class IndexedPolygon Extends IntStack
	Field pool:Float[]
	Field area:float
	Field winding:Int
	Field prepared:Bool

	Field ear:Int[3]

	Method New(points:Float[])		
		pool = points		
	End Method 
	
	' only for debugging
	Method ToPoints:Float[]()
		Local cords:= New Float[Self.Length()*2] 
		Local i:Int
		For Local n:= Eachin Self
			cords[i] = pool[n]
			cords[i+1] = pool[n+1]		
			i +=2
		Next
	
		Return cords
	
	End Method
	
	
	 ' determine winding
	Method Prepare:Void()
	
		If   pool.Length()<3 Then
			Error "IndexedPolygon must have a pool"
		Endif		
		area = PolygonArea(pool,Self)
		winding = Sgn(area)
		
		prepared = true
	End Method
 
		
	' Return True if ear was clipped
	' clipped ear indexes is in ear array	
	Method ClipEar:Bool()
		If Self.Length() < 3 then Return False
	
		If Not prepared Then Prepare()
 
		Local minarea:Float = EPS  
 
		For Local r:Int = 0 To  Self.Length()
			Local f:Int = Self.Pop() 

				ear[0] = Self.Top()
				ear[1] = f
				ear[2] = Self.Get(0)
				
				Local a:Float = PolygonArea(pool,ear)
				
				If (Sgn(a) = winding) 'Or Sgn(a) = 0 ' check triangle winding to ignore concave triangles (they are not ears)					
					If NoPointsInTriangle(pool,Self,ear) ' check that no point fron the main poly is in the ear (then it does not intersect)
						Return True
					Endif
	 			
				Endif
				Self.Insert(0,f)  ' roll polygon
				
			Next
		
		Return false
		
	End Method	

End Class







'Bool pointInPolygon() {
'
' Int      i, j=polySides-1 ;
'  boolean  oddNodes=NO      ;
'
'  For (i=0; i<polySides; i++) {
'    If (polyY[i]<y && polyY[j]>=y
'    ||  polyY[j]<y && polyY[i]>=y) {
'      If (polyX[i]+(y-polyY[i])/(polyY[j]-polyY[i])*(polyX[j]-polyX[i])<x) {
'        oddNodes=!oddNodes; }}
'    j=i; }
'
'  Return oddNodes; }

'Translated to monkey by Difference from : Darel Rex Finley, Nathan Mercer - http://alienryderflex.com/polygon/
#rem
Function PointInPolygon:Int(polyPoints:Float[],x:Float,y:Float) 
	Local  j:Int = polyPoints.Length()-2
  	Local oddNodes:Int 
	For Local i:Int =0 Until polyPoints.Length() Step 2
		If (polyPoints[i+1]< y And polyPoints[j+1]>=y ) Or (polyPoints[j+1]< y And polyPoints[i+1]>=y) 
			If  (polyPoints[i]<=x Or polyPoints[j]<=x) 
				If (polyPoints[i]+(y-polyPoints[i+1])/(polyPoints[j+1]-polyPoints[i+1])*(polyPoints[j]-polyPoints[i])<x) 
					oddNodes = 1 ~ oddNodes 
				Endif
			Endif
		Endif
		j=i 
	Next
  Return oddNodes
End Function
#end


Function PointInPolygon:Int(points:Float[],indexes:int[],x:Float,y:Float) 

	Local  j:Int = indexes[indexes.Length()-1]
  	Local oddNodes:Int 

	For Local i:= Eachin indexes
		If (points[i+1]< y And points[j+1]>=y ) Or (points[j+1]< y And points[i+1]>=y) 
			If  (points[i]<=x Or points[j]<=x) 
				If (points[i]+(y-points[i+1])/(points[j+1]-points[i+1])*(points[j]-points[i])<x) 
					oddNodes = 1 ~ oddNodes 
				Endif
			Endif
		Endif
		
		j=i 
	Next

  Return oddNodes

End Function



Function PolygonArea:Float(pool:Float[],poly:Int[])

	Local accum:Float
	Local  j:Int = poly[poly.Length()-1]

	For Local i:= Eachin poly
		accum += pool[j] * pool[i+1] - pool[i] * pool[j+1]
    	j=i 
	Next

	Return accum / 2.0

End Function



Function PolygonArea:Float(pool:Float[],poly:IndexedPolygon)

	Local accum:Float
	Local  j:Int = poly.Top()

	For Local i:=Eachin poly
		accum += pool[j] * pool[i+1] - pool[i] * pool[j+1]
    	j=i 
	Next

	Return accum / 2.0

End Function


Function PolygonArea:Float(polyPoints:Float[])

	Local accum:Float
	Local  j:Int = polyPoints.Length()-2

	For Local i:Int =0 Until polyPoints.Length() Step 2
		accum += polyPoints[j] * polyPoints[i+1] - polyPoints[i] * polyPoints[j+1]
    	j=i 
	Next

	Return accum / 2.0

End Function



'Function params: pointpool , intarray/stack with indexes and the 3 triangle indexes to ignore
Function NoPointsInTriangle:Bool(points:Float[],poly:IntStack,tri:Int[])

	For Local i:= Eachin poly
		If (i <> tri[0]) 
			If (i <> tri[1]) 
				If (i <> tri[2])
					If PointInPolygon(points,tri,points[i],points[i+1])
						'Print "Point in " + i
						Return False
					Endif
				Endif
			Endif
		Endif
	Next 
	Return True
End Function 

#rem
Function NoPointsInTriangle:Bool(points:Float[],ignore:Int[])

	For Local i:Int = 0 Until points.Length() Step 2
		If (i <> ignore[0]) 
			If (i <> ignore[1]) 
				If (i <> ignore[2])
					If pointInPolygon(tri,points[i],points[i+1])
						'Print "Point in " + i
						Return False
					Endif
				Endif
			Endif
		Endif
	Next 
	Return True
End Function 
#end 


' this function should be a part of mojo
' drawing this way will leave seams between the trangles
' because of antialiasing
' ( one work around is to draw thj triangles twise  see DrawTriangles2)

Function DrawTriangles:Void(points:Float[],indexes:Int[])

	Local tri:Float[6]
	For Local n:Int=0 Until  indexes.Length() Step 3
	
		Local tri:Float[6]
		tri[0] = points[indexes[n]]
		tri[1] = points[indexes[n]+1]
		tri[2] = points[indexes[n+1]]
		tri[3] = points[indexes[n+1]+1]				
		tri[4] = points[indexes[n+2]]
		tri[5] = points[indexes[n+2]+1]

		DrawPoly tri
	
	Next

End Function


 
 


' find 

Function FloatArraysMerge:Float[](arr:float[][])

	Local marr:Float[]
	Local offset:int

	For Local n:Int = 0 Until arr.Length()
	'	Print "N " + n
		
		Local taillen:Int = arr[n].Length()
		
		marr = marr.Resize(offset+taillen)
		
		For Local i:Int = 0 Until taillen
			marr[offset+i] = arr[n][i]
		Next
		
		offset = marr.Length()
		
	Next	
	
	Return marr	

End Function

' find a line that goes from outer to inner poly without crossing either of them
Function HoleMerge:IndexedPolygon(points:Float[],poly:IndexedPolygon,hole:IndexedPolygon,allpolys:IndexedPolygon[])

	poly.Prepare	
	hole.Prepare	
	
	If poly.winding = hole.winding
		Print "Winding the same, deal with it!"
	
	Endif
	

	Local ip:Int	
	
	
	Local p2:Int = poly.Get(0)
	
	For Local p1:= Eachin poly
		Local ih:Int
		
		
		Local h2:Int = hole.Get(0)
		For Local h1:=  Eachin hole
		
 
	 		' look for two lines
	 		' we are only using one for now
			If Not SegmentIntersectsPolys(p1,h2,allpolys,points)
 			If Not SegmentIntersectsPolys(p2,h1,allpolys,points)

				'	Print "Success - Hole connector found"
				
					Local newpoly:= New IndexedPolygon(points) 

					' insert outer poly first:	
					For Local np:Int = ip Until poly.Length()
						newpoly.Push poly.Get(np)	
					Next 
					
					For Local np:Int = 0 Until ip
						newpoly.Push poly.Get(np)	
					Next 	
					
					'insert "break" point from outer poly again 
					newpoly.Push poly.Get(ip)
					
					
					' insert inner poly (assume it's reversed already )
					
					For Local np:Int = ih Until hole.Length()
						newpoly.Push hole.Get(np)	
					Next 
					
					For Local np:Int = 0 Until ih
						newpoly.Push hole.Get(np)	
					Next 	
					
					'insert "break" point from outer poly again 
					newpoly.Push hole.Get(ih)
		
					Return newpoly				
				
		 	Endif
			Endif
			
			h2 = h1
			
			ih +=1
		Next
		
		p2=p1
		
 		ip +=1
	Next

	 Print "No connector line found between hole andf poly, returning poly without hole"
 
	Return  poly

End Function

 
' find a line that goes from outer to inner poly without crossing either of them
Function OLDHoleMerge:IndexedPolygon(points:Float[],poly:IndexedPolygon,hole:IndexedPolygon,allpolys:IndexedPolygon[])


	Local ip:Int	
	
	For Local p:= Eachin poly
		Local ih:int
		For Local h:=  Eachin hole
		
			Local px:Float = points[p]
			Local py:Float = points[p+1]

			Local hx:Float = points[h]
			Local hy:Float = points[h+1]


			
	 
			If Not SegmentIntersectsPolys(p,h,allpolys,points)
 

				'	Print "Success - Hole connector found"
				
					Local newpoly:= New IndexedPolygon(points) 

					' insert outer poly first:	
					For Local np:Int = ip Until poly.Length()
						newpoly.Push poly.Get(np)	
					Next 
					
					For Local np:Int = 0 Until ip
						newpoly.Push poly.Get(np)	
					Next 	
					
					'insert "break" point from outer poly again 
					newpoly.Push poly.Get(ip)
					
					
					' insert inner poly (assume it's reversed already )
					
					For Local np:Int = ih Until hole.Length()
						newpoly.Push hole.Get(np)	
					Next 
					
					For Local np:Int = 0 Until ih
						newpoly.Push hole.Get(np)	
					Next 	
					
					'insert "break" point from outer poly again 
					newpoly.Push hole.Get(ih)
		
					Return newpoly				
				
		 
			Endif
			ih +=1
		Next
 		ip +=1
	Next

	 Print "No connector line found between hole andf poly, returning poly without hole"
 
	Return  poly

End Function



'/* from http://paulbourke.net/geometry/lineline2d/
'   Determine the intersection point of two line segments

	'Function returns 
	' 0 both lines are parallel and will never intersect.
	'1 lines are colinear (i.e. the same).
	'2 intersects, but outside segnments
	
	 
	'4 both segments intersect 
'*/
Function Intersects:Int(x1:Float, y1:Float, x2:Float, y2:Float, x3:Float, y3:Float, x4:Float, y4:Float )


	Local denom:Float  = (y4-y3) * (x2-x1) - (x4-x3) * (y2-y1)
	Local numera:Float = (x4-x3) * (y1-y3) - (y4-y3) * (x1-x3)
	Local numerb:Float = (x2-x1) * (y1-y3) - (y2-y1) * (x1-x3)

	'/* Are the line coincident? */
	If (Abs(numera) < EPS And Abs(numerb) < EPS And Abs(denom) < EPS) Then
		gIntersecsX = (x1 + x2) / 2
		gIntersecsY = (y1 + y2) / 2
		Return 1
	Endif

	'/* Are the line parallel */
	If (Abs(denom) < EPS) Then
		gIntersecsX = 0
		gIntersecsY = 0
		Return 0
	Endif

	'/* Is the intersection along the the segments */
	Local mua:Float = numera / denom
	Local mub:Float = numerb / denom
	gIntersecsX = x1 + mua * (x2 - x1)
   	gIntersecsY = y1 + mua * (y2 - y1)

	gIntersectionCorner = 0


	If  (mua >= 0.0) And (mua <= 1.0)
		If  (mub >= 0.0) And (mub <= 1.0)

			If Abs(mub) <= EPS Then gIntersectionCorner = 3
			If Abs(mub-1.0) <= EPS Then gIntersectionCorner = 4
	
         	Return 4
 
		Endif
	Endif

    Return 2		

End Function


' Function to check if a line segment intersects a single polygon
Function SegmentIntersectsPoly:Bool(s1:Int,s2:Int,poly:IntStack,points:Float[])
		Local i2:Int = poly.Top()
		
		For Local i1:= Eachin poly
		
			If (i1<>s1) And (i1<>s2) And (i2<>s1) And (i2<>s2) ' avoid checking line ends
				If Intersects(points[s1],points[s1+1],points[s2],points[s2+1],points[i1],points[i1+1],points[i2],points[i2+1])>2
	 				 If Not gIntersectionCorner Return True
				Endif	
			Endif	
						
			i2 = i1
		Next
		
		Return False
End Function


' wrapper to check multiple polygons 
Function SegmentIntersectsPolys:Bool(p:Int,h:Int,polys:IndexedPolygon[],points:Float[])
	For Local poly:= Eachin polys
		If SegmentIntersectsPoly(p,h,poly,points)	
			Return True
		Endif
	Next 
	Return False
End Function