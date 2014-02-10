Function TestClockwise2D:Bool(xy#[],a,b,c)
	Local x0#
	Local y0#
	Local x1#
	Local y1#
	
	x0=xy[b*2+0]-xy[a*2+0]
	y0=xy[b*2+1]-xy[a*2+1]

	x1=xy[b*2+0]-xy[c*2+0]
	y1=xy[b*2+1]-xy[c*2+1]	

	Return x1*y0>x0*y1
End


Function dot#(x0#,y0#,x1#,y1#,x2#,y2#)
	Return (x1-x0)*(y2-y1)-(x2-x1)*(y1-y0)
End Function

Function InsideQuad?(px#,py#,x0#,y0#,x1#,y1#,x2#,y2#,x3#,y3#)
	If dot(x0,y0,x1,y1,px,py)>0
		If dot(x1,y1,x2,y2,px,py)>0
			If dot(x2,y2,x3,y3,px,py)>0
				If dot(x3,y3,x0,y0,px,py)>0
					Return True
				Endif
			Endif
		Endif
	Endif
End Function

Function InsideTriangle?(px#,py#,x0#,y0#,x1#,y1#,x2#,y2#)
	If dot(x0,y0,x1,y1,px,py)>0
		If dot(x1,y1,x2,y2,px,py)>0
			If dot(x2,y2,x0,y0,px,py)>0
				Return True
			Endif
		Endif
	Endif
End

Function Triangulate:List<Float[]>(poly#[])

	Local xy#[]=poly.Resize(poly.Length)

	Local list:=New List<Float[]>;

	Local n=xy.Length/2
	
	Local count=n-2

	Local a=0
	Local b=1
	Local c=2
	
	Local miss=0
	
'	Local w:=IsClockwise2D(xy,a,b,c)
	
	While count>0 And miss<20
				
		If TestClockwise2D(xy,a,b,c)

			Local x0#=xy[a*2+0]
			Local y0#=xy[a*2+1]
			Local x1#=xy[b*2+0]
			Local y1#=xy[b*2+1]
			Local x2#=xy[c*2+0]
			Local y2#=xy[c*2+1]

			Local ok?=True
			
			Local t=c+1
			If t=n t=0

			While t<>a
				
				Local px#=xy[t*2+0]
				Local py#=xy[t*2+1]
				
				If InsideTriangle(px,py,x0,y0,x1,y1,x2,y2)				
					ok=False
					Exit
				Endif
				
				t+=1
				If t=n t=0		
			Wend
			
			If  ok
		
				Local d#[]=New Float[6]			
				d[0]=x0
				d[1]=y0
				d[2]=x1
				d[3]=y1
				d[4]=x2
				d[5]=y2
				list.AddLast d	
				
				miss=0
				count-=1
				n-=1
				
				For Local i=b Until n
					xy[i*2+0]=xy[i*2+2]
					xy[i*2+1]=xy[i*2+3]
				Next
				
				If a>b a-=1
				If c>b c-=1
							
				b=c
				c=c+1
	
				If c>=n c-=n
						
				Continue
			Endif
			
		Endif
	
		a=b
		b=c
		c=c+1
		If c>=n c-=n		
		miss+=1
	
	Wend
	
	Return list
End