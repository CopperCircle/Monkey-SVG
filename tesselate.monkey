Strict

' tesselate.monkey
' Poly earclipper that deals with holes
' Chuck full of usefull routines.
' () 2014.02.24 - Peter Scheutz aka Difference


' v0.7 - 2014.03.11
' major rework of the polyline function
' miter-limit and bevel joints are in 
' Square captype is now working


' v0.6b - 2014.03.07
' Thick line added, (miter joins only for now)
' added params to TriangulateAndDrawPolyline()
 
' v0.5 - 2014.03.01
' New method for merging holes, see comments in HolMerge for strategy
' Moved the functions to Tesselator Class to wrap the calls 

' v0.3 - 2014.02.26 
'	 - changed Tessellate() To a While loop, keeping problem points For examination 
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
Const EPS:Float = 0.005
Const EPS2:Float = 0.005

Function PointsAreIdentical:Bool(x1:Float,y1:Float,x2:Float,y2:float)


	' Manhatten distance should do
	If Abs(x1-x2) < EPS
		If Abs(y1-y2) < EPS	 		
			Return True

		Endif
	Endif

	Return False
End Function

 Function DebugDrawFloatPoints2 :Void(points:Float[], pointNumbers:Bool=False)
	SetColor 0,100,0
	For Local p:Int=0 Until points.Length()  Step 2
		DrawCircle points[p],points[p+1],5
		If pointNumbers	DrawText(p, points[p]+Rnd(-5,5),points[p+1]+Rnd(-5,5))
	Next

	SetColor 255,0,0
	For Local p:Int=0 Until points.Length()  Step 2
		DrawLine points[p], points[p+1], points[p+2], points[p+3]
	Next
End Function


Class Tessellator


	' jointype: miter = 0 , round = 1, bevel = 2	  	 
	' captypes: butt = 0 , round = 1, square = 2	 
	Function TriangulateAndDrawPolyline:Void(points:Float[],linewidth:Float,closed:Bool,jointype:Int=0,captype:Int=0,miterlimit:Float=4.0)
	
		' captype = 2
 	 	' linewidth = 20
		' miterlimit = 1
 		' closed = true
		' DebugDrawFloatPoints points 

		Local fatline:= New  ThickPolyline(points,linewidth,closed,jointype,captype,miterlimit)
		
		fatline.Draw()

	End Function


 	Function TriangulateAndDrawPolygons:Void(polys:Float[][])
 	
 		Local compoundpoly:= New CompoundPolygon(polys)
 		compoundpoly.Draw
 		
	End Function	




End Class

'Polygon with holes 
Class CompoundPolygon 

	' for debugging
	Field _pointsmerged:Int	
	Field _holesmerged:Int
	Field _holesskipped:Int
 
	Field _losttriangles:Int
	
	
	Field bridgecount:Int
	
	' end debugging

	Field points:Float[]
 	Field triangleindexes:Int[] 
	Field trianglecount:Int
	Field polyindexes:= New Deque<IndexedPolygon>
 
 
 	Field mergedpoly:IndexedPolygon
 	
  
  
 
 
	Method New(newpoints:Float[][])

	 

' ****************** BEGIN Clean up input arrays
' IMO this sould be done by the parser
		
		If Not newpoints.Length() Return ' no polygons
		If Not newpoints[0].Length() Return 'no point in first poly
 
	
		' sometimes we are passed empty polys
		If newpoints[newpoints.Length()-1].Length() = 0
			newpoints = newpoints[..(newpoints.Length()-1)] 
		Endif		 
			

 '#rem			

 		' remove endpoints that doubles firstpoint
		For Local i:Int = 0 Until newpoints.Length()		
			newpoints[i] = CleanUpPolygon( newpoints[i])
		Next
			 
'#End 		
' ****************** END Clean up input arrays
 		

		Build newpoints
	
	
	End Method

	Method Build:Void(newpoints:Float[][])

		' make a pool with all points
		' this is important when drawing the triangles later, or there can be gaps/flickering
		' if we douplicate points because of floating point inacuracy 
		points =  FloatArraysMerge(newpoints)	
		
 	
		'make polygons that references the points in the pool
		' each value is an offset to the x coordinate in the pool
		Local index:Int
		For Local i:Int = 0 Until newpoints.Length()
		
			Local ipoly:= New IndexedPolygon(points)  
		 
			polyindexes.PushLast ipoly
			
			For Local n:Int = 0 Until newpoints[i].Length()-1 Step 2			
				ipoly.Push(index)
				index += 2
			Next
		Next
		
	 
		' merge holes and polygon
		' OBS for now assume all polys with index> 0 are holes 
		' start by adding first poly, then merge holes	
		mergedpoly  = polyindexes.PopFirst()
		
		While polyindexes.Length()
			Local hole:= polyindexes.PopFirst()
			mergedpoly = HoleMerge(points,mergedpoly,hole,polyindexes)
		wend
			
		If mergedpoly.Length()<2 Then Return
 
 		' triangulate it
		Tessellate(points,mergedpoly)
 
	End Method


	
	Method Draw:Void()		
 	
		DrawTriangles(points,triangleindexes)
 		
	End Method	
	
	Method Tessellate:Void(points:Float[],indexedpoly:IndexedPolygon)
 	
		' a polygon can be triangulated with vertexcount - 2 triangles
		trianglecount = indexedpoly.Length() - 2
		
		
		
		If indexedpoly.bridges
			bridgecount = indexedpoly.bridges.Length()/3
			trianglecount +=  bridgecount
		endif
		
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
	
		If indexedpoly.bridges   
			While indexedpoly.bridges.Length() 
				triangleindexes[i+2] = indexedpoly.bridges.Pop()
				triangleindexes[i+1] = indexedpoly.bridges.Pop()		
				triangleindexes[i] = indexedpoly.bridges.Pop()	
				i +=3	
			Wend
		endif	
		

		' did we loose some in the fire?
		If i < triangleindexes.Length()
			triangleindexes = triangleindexes[..i]
			_losttriangles = (trianglecount-i/3)			
			
			trianglecount = i/3
		Endif
 		

	End Method
	
End Class

	

Class IndexedPolygon Extends IntStack

	' for debugging
	'Field _mergelines:= New IntStack
	Field status_holeslost:Int

	Field bridges:IntStack

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
	'	If Self.Length() <3 Then Return False
	
	
		If Not prepared Then Prepare()
 
		Local attempts:Int = Self.Length()
		  
 
		For Local r:Int = 0 To attempts
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
						''Print "Point in " + i
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
						''Print "Point in " + i
						Return False
					Endif
				Endif
			Endif
		Endif
	Next 
	Return True
End Function 
#end 

Function DebugDrawFloatPoints :Void(points:Float[])

	
	SetColor 0,100,0
	For Local p:Int=0 Until  points.Length()-2 Step 2
 			
		DrawCircle points[p],points[p+1],5
	Next	

	SetColor 255,0,0
	For Local p:Int=0 Until  points.Length()-2 Step 2
		DrawLine points[p],points[p+1],points[p+2]+Rnd(0),points[p+3]+Rnd(0)			
	Next	
End function	


Function DebugDrawFloatPoints :Void(points:Float[],indexes:IntStack)

	SetColor 255,0,0

	Local p1:Int = -1
	For Local p2:= Eachin indexes
		If p1 >-1
			DrawLine points[p1],points[p1+1],points[p2] ,points[p2+1] 
		Endif
		p1 = p2
			
	Next	
End function	




' This function should be a part of mojo, for speed and because
' drawing this way will often leave seams between the trangles
' because of hardware antialiasing functions
Function DrawTriangles:Void(points:Float[],indexes:Int[])


	'SetAlpha 0.5

	Local tri:Float[6]
	For Local n:Int=0 Until  indexes.Length() Step 3
	
		' for visual debugging
		'If n Mod 6
		'	SetColor 255,0,0
		'Else
		'	SetColor 0,255,0		
		'Endif
	
 
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


Function CleanUpPolygon:float[](poly:float[])

	Local skiplist:= New IntList	
			
	Repeat
		skiplist.Clear()
	
		' a list af adjacent vertices that are identical
		' we need to remove them or they will confuse the earclipper
		If poly.Length()>4
	
			Local p3:Int = poly.Length() - 4
			Local p2:Int = poly.Length() - 2
			
			For Local p1:Int = 0 Until poly.Length() Step 2
			
 				If PointsAreIdentical(poly[p1],poly[p1+1],poly[p2],poly[p2+1])
					skiplist.AddLast p2
				Endif
				
				p3 = p2
				p2 = p1
				
			Next	
		
			Local skip:Int
			
			For Local n:Int = 0 Until poly.Length() Step 2	
			
				If skiplist.Contains(n)
					skip +=2
				Endif
			
				If n+skip<poly.Length()
					poly[n] = poly[n+skip] 
					poly[n+1] = poly[n+skip+1] 
				Endif
			
			Next
			
			 If skip Then 	
			 
			 '	_pointsmerged += skip/2
			 
			 	' truncate array to new length()
				poly = poly[..(poly.Length()-skip)]
			Endif
		
		Endif
	Until  skiplist.IsEmpty()  	

	Return poly

End Function 
 


' find 

Function FloatArraysMerge:Float[](arr:float[][])

	Local marr:Float[]
	Local offset:int

	For Local n:Int = 0 Until arr.Length()
	'	'Print "N " + n
		
		Local taillen:Int = arr[n].Length()
		
		marr = marr.Resize(offset+taillen)
		
		For Local i:Int = 0 Until taillen
			marr[offset+i] = arr[n][i]
		Next
		
		offset = marr.Length()
		
	Next	
	
	Return marr	

End Function

' find two lines that goes from outer to inner poly without crossing either of them or  any other holes
' cut away this piece of the polys and merge them
' save the bridge triangeles in a special pool and add them to the triangulation in the end of tesselate

Function HoleMerge:IndexedPolygon(points:Float[],poly:IndexedPolygon,hole:IndexedPolygon,allpolys:Deque<IndexedPolygon>)

 
	Local ip2:Int	= poly.Length() -1 	
	
	For Local ip1:=0 Until poly.Length()
		
		Local ih2:Int = hole.Length() - 1
		
		For Local ih1:=0 Until hole.Length()
		
		
			Local p1:= poly.Get(ip1)
			Local p2:= poly.Get(ip2)
			Local h1:= hole.Get(ih1)
			Local h2:= hole.Get(ih2)								
		
 
	 		' look for two lines
	 		
	 		Local bridgefound:Bool = True
	 		
 
			If SegmentIntersectsPoly(p1,h2,poly,points) 
				bridgefound = False
			Elseif SegmentIntersectsPoly(h2,p1,hole,points) 
				bridgefound = False
			Elseif SegmentIntersectsPoly(p2,h1,poly,points) 
				bridgefound = False
			Elseif SegmentIntersectsPoly(h1,p2,hole,points) 
				bridgefound = False
				
			else	
				
				' check the other hole polys too, merge lines can not cross them either
				For Local apoly:= Eachin allpolys
				
					If (apoly <> poly) And (apoly <> hole)
					
						If SegmentIntersectsPoly(p1,h2,apoly,points)
							bridgefound = False
						Elseif SegmentIntersectsPoly(h1,p2,apoly,points) 
							bridgefound = False
						Endif
					endif
				Next


				
				
			Endif

 			If bridgefound

				'	'Print "Success - Hole connectors found"
				
					Local newpoly:= New IndexedPolygon(points) 
					
					newpoly.status_holeslost = poly.status_holeslost	' only for status
					
 

					' insert outer poly first:	
					For Local np:Int = ip1 Until poly.Length()
						newpoly.Push poly.Get(np)	
					Next 
					
					For Local np:Int = 0 Until ip1 
						newpoly.Push poly.Get(np)	
					Next 	
					
					'insert "break" point from outer poly again 
					'newpoly.Push poly.Get(ip1)
					
					
					' insert inner poly (assume it's reversed )
					
					For Local np:Int = ih1  Until hole.Length()
						newpoly.Push hole.Get(np)	
					Next 
					
					For Local np:Int = 0 Until ih1 
						newpoly.Push hole.Get(np)	
					Next 	
					
					'insert "break" point from outer poly again 
					'newpoly.Push hole.Get(ih1)
					If  poly.bridges
						newpoly.bridges = poly.bridges
					Else
						newpoly.bridges = New IntStack
					Endif
 


 					' try this first, if good add the other triangle
 					' otherwise  add the two triangles with the opposite diagonal
					If Not PointInPolygon(points,[p2,p1,h2],points[h1],points[h1+1]) 

 						newpoly.bridges.Push p1
 						newpoly.bridges.Push h2
 						newpoly.bridges.Push p2

 						newpoly.bridges.Push h1 						
						newpoly.bridges.Push p2
 						newpoly.bridges.Push h2
 						
 					Else
 						newpoly.bridges.Push h1 					
			 			newpoly.bridges.Push p2
 						newpoly.bridges.Push p1

			 			newpoly.bridges.Push p1
 						newpoly.bridges.Push h2
 						newpoly.bridges.Push h1

 					Endif
 

					Return newpoly				
				
		 	Endif
			 
			
			ih2 =ih1
			
		Next
		
	 
		
 		ip2 =ip1
	Next

 
 	poly.status_holeslost += 1 ' No bridge between hole and poly, returning poly without hole 
 	
	Return  poly

End Function

 
  

'Function DotProduct:Float(x0:Float,y0:Float,x1:Float,y1:Float,x2:Float,y2:Float)
'	Return (x1-x0)*(y2-y1)-(x2-x1)*(y1-y0)
'End Function


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
	If (Abs(numera) < EPS2 And Abs(numerb) < EPS2 And Abs(denom) < EPS2) Then
		gIntersecsX = (x1 + x2) / 2
		gIntersecsY = (y1 + y2) / 2
		Return 1
	Endif

	'/* Are the line parallel */
	If (Abs(denom) < EPS2) Then
		gIntersecsX = (x1 + x2) / 2
		gIntersecsY = (y1 + y2) / 2
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

			If Abs(mub) <= EPS Then gIntersectionCorner = 1
			If Abs(mub-1.0) <= EPS Then gIntersectionCorner += 2

			If Abs(mua) <= EPS Then gIntersectionCorner += 10
			If Abs(mua-1.0) <= EPS Then gIntersectionCorner += 20

	
         	Return 4
 
		Endif
	Endif

    Return 2		

End Function


' Function to check if a line segment intersects a single polygon
' ignoring the starting point of the line segmet
Function SegmentIntersectsPoly:Bool(s1:Int,s2:Int,poly:IntStack,points:Float[])
		
		Local i2:Int = poly.Top()
		
		For Local i1:= Eachin poly
 		
			If (s1 <> i1) And (s1 <> i2) 
				If Intersects(points[s1],points[s1+1],points[s2],points[s2+1],points[i1],points[i1+1],points[i2],points[i2+1])>2
 			  		'If Not gIntersectionCorner Then
 			  		 Return True
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



 

Function CrossProduct:Float(v1x:Float,v1y:Float,v2x:Float,v2y:Float)
    Return (v1x*v2y) - (v1x*v2y)
End Function
 
' determine of point is right or left of directional vector 
Function  IsLeft:Bool(ax:Float,ay:Float,bx:Float,by:Float,cx:Float,cy:Float)
	Return ((bx - ax)*(cy - ay) - (by - ay)*(cx - ax)) > 0
End Function 
 
 
Class ThickPolyline  Extends CompoundPolygon


	' jointype: miter = 0 , round = 1, bevel = 2	  	 
	' captypes: butt = 0 , round = 1, square = 2		 
	Method New(orgpoints:Float[],linewidth:Float,closed:Bool,jointype:Int,captype:Int,miterlimit:Float)
	
		orgpoints = CleanUpPolygon( orgpoints )
	
		If orgpoints.Length() < 4 Then Return 

 


		Calculate orgpoints,linewidth,closed,jointype,captype,miterlimit
	


	End Method


	Method Calculate:Void(orgpoints:Float[],linewidth:Float,closed:Bool,jointype:Int,captype:Int,miterlimit:Float )
	
		' no round joint yet, so use bevel
		If jointype = 1 Then jointype = 2
 	
	
 		Local tris:= New IntStack
  	
		Local lps:= New FloatDeque
		Local rps:= New FloatDeque

 
 
		Local halfwidth:Float = linewidth / 2.0
  
		Local segmentcount:Int  = orgpoints.Length()/2
		If closed segmentcount +=1

		Local p1:Int = orgpoints.Length() - 2
		Local p2:Int = 0
		Local p3:Int = 2

		
		For Local seg:Int = 1 To segmentcount
			
			' two line segments, p2 is the corner that we're making point for  
			Local ax:Float = orgpoints[p1]
			Local ay:Float	= orgpoints[p1+1]		
			Local bx:Float = orgpoints[p2]
			Local by:Float	= orgpoints[p2+1]
			Local cx:Float = orgpoints[p3]
			Local cy:Float	= orgpoints[p3+1]		
			

			Local segmentjointype:Int = jointype ' this is overridden on a per joint basis
 
			' create a vector perpendicular to line for offsettting   line a-b
			Local v1x:Float =  -(by-ay)
			Local v1y:Float =  (bx-ax)		
	
			' normalise it
			Local magnitude1:Float = Sqrt(v1x*v1x+v1y*v1y)
			v1x /= magnitude1
			v1y /= magnitude1
	
			'give it thickness
			v1x *= halfwidth
			v1y *= halfwidth
				
			' create a vector perpendicular to line for offsettting   line b-c
			Local v2x:Float =  -(cy-by)
			Local v2y:Float =  (cx-bx)			
	
			' normalise it
			Local magnitude2:Float    = Sqrt(v2x*v2x+v2y*v2y)
 
 			v2x /= magnitude2
			v2y /= magnitude2
	
			'give it thickness
			v2x *= halfwidth
			v2y *= halfwidth	
	
 
 
 			' check if segment b-c is turning left or right
 			Local isleft:Bool = IsLeft(ax,ay,bx,by,cx,cy)
 
  			 
			
			If isleft 'look at left side intersection
				If Not Intersects(ax+v1x,ay+v1y,bx+v1x,by+v1y,bx+v2x,by+v2y,cx+v2x,cy+v2y)>=2  
					' its a staight line or a point
			 		segmentjointype = 3 ' straight line 
				Endif
			Else	' look at right side intersection
				If Not Intersects(ax-v1x,ay-v1y,bx-v1x,by-v1y,bx-v2x,by-v2y,cx-v2x,cy-v2y)>=2  
					' its a staight line or a point
			 		segmentjointype = 3 ' straight line 
			 	endif
			endif
			
 			
 			' base for indexes to the points
 			' using two pointstacks makes for easirer visual debugging
 			' but results in this slightly convoluted offset trick
 			' negative indeses are for the right line
  			Local lindex:Int = lps.Length() - 2 			
  			Local rindex:Int = rps.Length() - 2 + 1000000
  			
  			Local beveltri:Int = 0
  			
  			
			' check miterlimit and mark it as a bevel join if limit is crossed
			If segmentjointype = 0
			
				Local mx:Float =  bx - gIntersecsX	
				Local my:Float =  by - gIntersecsY	
							
				Local dist:Float = Sqrt(mx*mx+my*my)
	
				If dist>miterlimit *linewidth 
					segmentjointype  = 2
				endif
			
			Endif
				
 
			' minimal angle before considering bevel or round
			Local angle:Float = ATan2(v2y,v2x) - ATan2(v1y,v1x)
			If Abs(angle) < 1 Then 	segmentjointype = 3


			' cap end point if not closed			
			If Not closed			
				If p2 = 0 ' first point
					segmentjointype = 4	 ' first point	  			
				Endif
			

				If seg = segmentcount   
				 	segmentjointype = 5	 ' last point
				Endif
	
			endif		
			
			Select segmentjointype
			
	
				Case 0	' miter join
					
					If isleft
		
						' mirror the point around point b
						lps.PushLast 2*bx - gIntersecsX	 	
						lps.PushLast 2*by - gIntersecsY 	
						
 						
						' panic and do something to fix protruding inner joits
						' this will result in some self intersection
						 If   IsLeft(cx,cy,cx- v2x,cy- v2y,gIntersecsX,gIntersecsY)	 
						 	rps.PushLast	cx + v2x  
						 	rps.PushLast	cy + v2y  				
 					
						Else
	 						rps.PushLast gIntersecsX      
							rps.PushLast gIntersecsY  
						
						Endif
		  
					Else

						' panic and do something to fix protruding inner joits
						' this will result in some self intersection
						If IsLeft(cx,cy,cx- v2x,cy- v2y,gIntersecsX,gIntersecsY)		
							lps.PushLast cx - v2x  
							lps.PushLast cy - v2y  
 				 		Else
 					 		lps.PushLast gIntersecsX    
							lps.PushLast gIntersecsY  
			 
				 		endif
				
				
						rps.PushLast 2*bx - gIntersecsX	 	
						rps.PushLast 2*by - gIntersecsY 					
  
					Endif		
 			
				Case 2
	 			
					If isleft
						beveltri = 1	' flag indicates we sould add a triangle later
	

						lps.PushLast bx - v1x  
						lps.PushLast by - v1y  

	
						lps.PushLast bx - v2x 	 
						lps.PushLast by - v2y 	 
						
  
						' panic and do something to fix protruding inner joits
						' this will result in some self-intersection
						 If   IsLeft(cx,cy,cx- v2x,cy- v2y,gIntersecsX,gIntersecsY)	 
						
						 	rps.PushLast	cx + v2x  
						 	rps.PushLast	cy + v2y  				
 					
						Else
	 						rps.PushLast gIntersecsX      
							rps.PushLast gIntersecsY  
						
						Endif  
  
						
					Else
						beveltri = 2 ' flag indicates we sould add a triangle later
						
						
						' panic and do something to fix protruding inner joits
						' this will result in some self-intersection
						If IsLeft(cx,cy,cx- v2x,cy- v2y,gIntersecsX,gIntersecsY)		
  	
							lps.PushLast cx - v2x  
							lps.PushLast cy - v2y  
 				 		Else
 					 		lps.PushLast gIntersecsX      
							lps.PushLast gIntersecsY  
			 
				 		endif	 

 
						rps.PushLast bx + v1x  
						rps.PushLast by + v1y   
						
						rps.PushLast bx + v2x 	 
						rps.PushLast by + v2y 						
	
	 				Endif	


				Case 3 ' straight line joint
				
					lps.PushLast bx - v2x 	 	
					lps.PushLast by - v2y 	 
					rps.PushLast bx +  v2x
					rps.PushLast by +  v2y	 
	 					
		
				Case 4 ' first point when not closed  
				
		 
				
					If captype = 2 ' squre end point, extend it by halfline width
						lps.PushLast bx - v2x - v2y ' add square off sets ' (swap and switch signs to get unit vector
						lps.PushLast by - v2y + v2x 
						rps.PushLast bx + v2x - v2y  
						rps.PushLast by + v2y + v2x 		
				
					Else
						lps.PushLast bx - v2x 	 	
						lps.PushLast by - v2y 	 
						rps.PushLast bx + v2x
						rps.PushLast by + v2y	 
 					Endif
 
 

				Case 5   ' last point when not closed  
				
		 			If captype = 2 ' squre end point, extend it by halfline width
	
						lps.PushLast bx - v1x + v1y  	
						lps.PushLast by - v1y - v1x 
						rps.PushLast bx + v1x + v1y  	
						rps.PushLast by + v1y	- v1x 	
		 			
		 			
		 			else
				
						lps.PushLast bx - v1x 	 	
						lps.PushLast by - v1y 	 
						rps.PushLast bx +  v1x
						rps.PushLast by +  v1y	 
					endif

			End select
			 
			' Add the triangles 
			' the line body
			If lindex>=0
					
				tris.Push lindex
				tris.Push lindex + 2 
				tris.Push - rindex   
	 		 '			
				tris.Push lindex  + 2
				tris.Push -rindex - 2  
				tris.Push -rindex 
 		 
 		 
 		 		' add bevel fillers
			 	If beveltri = 1
			
					tris.Push lindex + 2
					tris.Push lindex + 4 
					tris.Push -rindex -2  
 			 	
			 	Elseif  beveltri = 2
			
			 		tris.Push lindex  + 2
			 		tris.Push -rindex -4 
			 		tris.Push -rindex -2
				 	
			 	Endif
					
			Endif	 
 

			p1 = p2
			p2 = p3
			p3 += 2
	
		

			
		If p3>=	orgpoints.Length()	p3 -= orgpoints.Length()
	'	If p2>=	orgpoints.Length()	p2 -= 	orgpoints.Length()
	'	If p1>=	orgpoints.Length()	p1 -= 	orgpoints.Length()		
	
							
	Next		
	
	 		
		Local offset:Int = lps.Length()  

	'	DebugDrawFloatPoints rps.ToArray()
  	'	DebugDrawFloatPoints lps.ToArray()
  		
 
		' append right side to left
		For Local d:= Eachin rps
			lps.PushLast d
		Next
 
		points = lps.ToArray()
		
		triangleindexes = tris.ToArray()	
		
		
		
		For Local i:Int = 0 Until triangleindexes.Length()
 			If triangleindexes[i] < -900000 Then
				triangleindexes[i] += 1000000  ' correct the negative index
				triangleindexes[i] = offset - triangleindexes[i]   ' add offset and correct negative index
			endif
		
	 	Next	

	End Method
 

End Class