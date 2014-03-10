Strict

' tesselate.monkey
' Poly earclipper that deals with holes
' Chuck full of usefull routines.
' () 2014.02.24 - Peter Scheutz aka Difference


' v0.6 - 2014.03.04
' Thick line added, (miter joins only for now)

 
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

'Global gIntersectionCorner:Int
Const EPS:Float = 0.005
Const EPS2:Float = 0.05

Function PointsAreIdentical:Bool(x1:Float,y1:Float,x2:Float,y2:float)


	' Manhatten distance should do
	If Abs(x1-x2) < EPS
		If Abs(y1-y2) < EPS	 		
			Return True

		Endif
	Endif

	Return False
End Function


Class Tessellator

 	 
	Function TriangulateAndDrawPolyline:Void(points:Float[],thickness:Float,closed:Bool)
		
		Local fatline:= New  ThickPolyline(points,thickness,closed)
		
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
 	
  	 
 
 
 	Method ShowDebugInfo:Void(x:Float,y:Float)
 	
 		SetAlpha 1
 	
 	
 		SetColor 255,255,255
 
 		Local lh:Float = 30
 		
 		DrawText "Point welded: " + _pointsmerged , x,y 
 		
 		DrawText "Lost triangles: " + _losttriangles, x,y + lh*1
 
 		If mergedpoly
 			DrawText "Pointsleftover: " + (mergedpoly.Length() - 2), x,y + lh*2
 		Else
 			DrawText "No Points at all"	, x,y + lh*2
 		Endif
 		
 		SetColor 0,0,255
 			
 			
 			
 			
 			
 		If mergedpoly

			Local bridgesindex:Int = triangleindexes.Length() - bridgecount - 3	 
			
			''Print bridgecount

			 For Local n:Int = bridgesindex Until  triangleindexes.Length() Step 3
			
	 			
			
'				DrawLine mergedpoly.pool[triangleindexes[n]],mergedpoly.pool[triangleindexes[n]+1],mergedpoly.pool[triangleindexes[n]+2],mergedpoly.pool[triangleindexes[n]+3]
			
			
			next
		endif

'	 		Local ml:= mergedpoly._mergelines
	 		
	 		''Print ml.Length()
	 		
	 		
	 		
'	 		Local j:Int  
'			For Local i:Int = 0 Until ml.Length()-1 Step 2
'			
'				 If i>0 And (i Mod 6 = 0) SetColor 0,0,255
'	 		
'	 			DrawLine mergedpoly.pool[ml.Get(i)],mergedpoly.pool[ml.Get(i)+1],mergedpoly.pool[ml.Get(i+1)],mergedpoly.pool[ml.Get(i+1)+1]
'	 
'	 				SetColor 255,255,0
''	 
	' 		Next
 '
' 		endif
 
 
  		Return ' below this for debug of leftover points
		
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
		
			newpoints[i] = CleanUpPolygon( newpoints[i])
		
			
		
		Next
			 
'#End 		
' ****************** END Clean up input arrays
 		

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
 		
 		'ShowDebugInfo 30,30
 		
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


' This function should be a part of mojo, for speed and because
' drawing this way will often leave seams between the trangles
' because of hardware antialiasing functions
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


 	'		If Not SegmentIntersectsPolys(p1,h2,allpolys,points)
 	'		If Not SegmentIntersectsPolys(p2,h1,allpolys,points)
 			




 			If bridgefound

				'	'Print "Success - Hole connectors found"
				
					Local newpoly:= New IndexedPolygon(points) 
					
					newpoly.status_holeslost = poly.status_holeslost	' only for status
					
					' for debugging
'					For Local ml:= Eachin  poly._mergelines
'						newpoly._mergelines.Push ml
'					next
					
	 			'	newpoly._mergelines.Push p1
				'	newpoly._mergelines.Push h2				
				'
	 			'	newpoly._mergelines.Push p2
				'	newpoly._mergelines.Push h1	

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

 
 
' find a line that goes from outer to inner poly without crossing either of them
Function LASTHoleMerge:IndexedPolygon(points:Float[],poly:IndexedPolygon,hole:IndexedPolygon,allpolys:IndexedPolygon[])

	poly.Prepare	
	hole.Prepare	
	
	If poly.winding = hole.winding
		'Print "Winding the same, deal with it!"
	
	Endif
	

	Local ip:Int	
	
	
	Local p2:Int = poly.Top()
	
	For Local p1:= Eachin poly
		Local ih:Int
		
		
		Local h2:Int = hole.Top()
		For Local h1:=  Eachin hole
		
 
	 		' look for two lines
	 		' we are only using one for now
			If Not SegmentIntersectsPoly(p1,h2,poly,points)
			If Not SegmentIntersectsPoly(h2,p1,hole,points)

			If Not SegmentIntersectsPoly(p2,h1,poly,points)
			If Not SegmentIntersectsPoly(h1,p2,hole,points)



 			'If Not SegmentIntersectsPolys(p2,h2,allpolys,points)
 			
 			

 			

				'	'Print "Success - Hole connector found"
				
					Local newpoly:= New IndexedPolygon(points) 
					
					' for debugging
					For Local ml:= Eachin  poly._mergelines
						newpoly._mergelines.Push ml
					next
					
					
	 				newpoly._mergelines.Push p1
					newpoly._mergelines.Push h2				
					

	 				newpoly._mergelines.Push p2
					newpoly._mergelines.Push h1	

					' insert outer poly first:	
					For Local np:Int = ip+1 Until poly.Length()
						newpoly.Push poly.Get(np)	
					Next 
					
					For Local np:Int = 0 Until ip 
						newpoly.Push poly.Get(np)	
					Next 	
					
					'insert "break" point from outer poly again 
					newpoly.Push poly.Get(ip+1)
					
					
					' insert inner poly (assume it's reversed already )
					
					For Local np:Int = ih+1  Until hole.Length()
						newpoly.Push hole.Get(np)	
					Next 
					
					For Local np:Int = 0 Until ih 
						newpoly.Push hole.Get(np)	
					Next 	
					
					'insert "break" point from outer poly again 
					newpoly.Push hole.Get(ih+1)
					
 
					
	 
					
		
					Return newpoly				
				
		 	Endif
			Endif

		 	Endif
			Endif
			
			h2 = h1
			
			ih +=1
		Next
		
		p2=p1
		
 		ip +=1
	Next

	 'Print "No connector line found between hole andf poly, returning poly without hole"
 
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
 

				'	'Print "Success - Hole connector found"
				
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

	_holeslost +=1
 
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

	'gIntersectionCorner = 0


	If  (mua >= 0.0) And (mua <= 1.0)
		If  (mub >= 0.0) And (mub <= 1.0)

			'If Abs(mub) <= EPS Then gIntersectionCorner = 1
			'If Abs(mub-1.0) <= EPS Then gIntersectionCorner += 2

			'If Abs(mua) <= EPS Then gIntersectionCorner += 10
			'If Abs(mua-1.0) <= EPS Then gIntersectionCorner += 20

	
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



Class ThickPolyline  

	Field pool:Float[]
 	Field triangleindexes:Int[] 

	Method New(orgpoints:Float[],linewidth:Float,closed:Bool)
	
		orgpoints = CleanUpPolygon( orgpoints )
	
		If orgpoints.Length() < 2 Then Return 

		pool = New Float[orgpoints.Length()*2]

		Local segmentcount:Int = orgpoints.Length()/2-1
		
		If closed Then segmentcount +=1

		Local trianglecount:int =  segmentcount*2
		
 		triangleindexes = New Int[trianglecount*3] 
		
		Local poff:int

		' make indexes for the triangles
		For Local n:Int = 0 Until trianglecount*3 Step 6
		
			Local p0:Int = poff
			Local p1:Int = poff + 2
			Local p2:Int = poff	 + 4				
			Local p3:Int = poff	 + 6	
			
			' wrap around for closed lines
			If p2 >= pool.Length()  		
				p2 = 0
				p3 = 2
			Endif
	 		
			triangleindexes[n] = p0
			triangleindexes[n+1] = 	p1					
			triangleindexes[n+2] = 	p3	  

			triangleindexes[n+3] = p0
			triangleindexes[n+4] = 	p3					
			triangleindexes[n+5] = 	p2	 
		
			poff += 4
	 	Next
		


		' calculate point positions
		Local halfwidth:Float = linewidth / 2.0

		Local p1:Int = orgpoints.Length()   - 2
	
 		Local poolindex:int


		For Local p2:Int = 0 Until orgpoints.Length()  Step 2
		
			Local p3:Int = p2 + 2
			If p3 >  orgpoints.Length()   - 2 Then p3 = 0
			
			' two line segments, p2 is the middle one  
			Local p1x:Float = orgpoints[p1]
			Local p1y:Float	= orgpoints[p1+1]		
			Local p2x:Float = orgpoints[p2]
			Local p2y:Float	= orgpoints[p2+1]
			Local p3x:Float = orgpoints[p3]
			Local p3y:Float	= orgpoints[p3+1]		
			

			' create a vector perpendicular to line for offsettting the line
			Local v1x:Float =  (p2y-p1y)
			Local v1y:Float =  -(p2x-p1x)		
	
			' normalise it
			Local magnitude1:Float = Sqrt(v1x*v1x+v1y*v1y)
			v1x /= magnitude1
			v1y /= magnitude1
			

			
	
			'give it thickness
			v1x *= halfwidth
			v1y *= halfwidth
				
			' the first two points of the outer line		
			' p1x   + v1x
			' p1y   + v1y							
			' p2x   + v1x
			' p2y   + v1y	

			Local v2x:Float =  (p3y-p2y)
			Local v2y:Float =  -(p3x-p2x)			
	
			' normalise it
			Local magnitude2:Float    = Sqrt(v2x*v2x+v2y*v2y)

		

			v2x /= magnitude2
			v2y /= magnitude2
	
			'give it thickness
			v2x *= halfwidth
			v2y *= halfwidth	
	

			' the second two points of the outer line		
			' p2x+v2x
			' p2y+v2y							
			' p3x+v2x
			' p3y+v2y	

			If Not closed And p2 = 0	'non looping first point, dont use crossings just offset 
			
			
			
				pool[poolindex] =   p2x + v2x  
				pool[poolindex+1] =  p2y + v2y
					
				pool[poolindex+2] = p2x -  v2x   
				pool[poolindex+3] = p2y - v2y   		
			
			
			Elseif Not closed And p2 = orgpoints.Length() - 2	'non looping last point, dont use crossings just offset 
						
				pool[poolindex] =   p2x +  v1x  
				pool[poolindex+1] =  p2y +  v1y
					
				pool[poolindex+2] = p2x -  v1x   
				pool[poolindex+3] = p2y - v1y  						
			Else
	
				' find intersection point bewtwwen the two outer lines
				If Intersects(p1x+v1x,p1y+v1y,p2x+v1x,p2y+v1y,p2x+v2x,p2y+v2y,p3x+v2x,p3y+v2y)>=2  
			
 
				
	 				pool[poolindex] =  gIntersecsX
					pool[poolindex+1] =  gIntersecsY
					
					' mirror the point around p2
					pool[poolindex+2] = p2x + p2x - gIntersecsX 
					pool[poolindex+3] = p2y + p2y - gIntersecsY 	
					
				Else
			 		'Print "	Fallback because theres no clean intersection, maybe two points are the same"
			 		
					pool[poolindex] =  p2x + v2x  
					pool[poolindex+1] =  p2y + v2y
						
					pool[poolindex+2] = p2x - v2x   
					pool[poolindex+3] = p2y - v2y  			 		
			 			
				Endif	
			Endif
		poolindex +=4
		p1 = p2							
	Next		
	
	End Method
 
	Method Draw:Void()		
		DrawTriangles(pool,triangleindexes)		
	End Method	
 
End Class