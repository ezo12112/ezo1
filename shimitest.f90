program shimi
implicit none
integer :: n,a,i,g,c,p,j
print *, " adad ra vared konid"
read *, n
open( unit = 123 , file= "shimi.txt" , status = "unknown")
do i = 1,30
a = i * n
print *, i ,"*", n , "=" , a 
write(123,*) i,"*",n,"=" ,a 
end do
call sh(a,n)
p = a 
j = n +1
call ch(p,n,j)
end

subroutine sh(x,y)
implicit none 
integer :: x,y,p,c
intent(in) ::x,y
do p = 1,30
c = ((y**2)+ y + p )
print *, y**2 ,"+" ,y, "+" , p , "=" , c
write (123,*) y**2 ,"+" ,y, "+" , p , "=" , c
end do
end subroutine sh

subroutine ch(d,f,c)
implicit none
integer :: d,f,c,l,v
do l = 1,30 
v = ((d**2 )-( l ** 2) -(f + d))
print *, d**2  , "-" ,  l ** 2 , " -" , f + d , "=" , v
write (123, *)  d**2  , "-" ,  l ** 2 , " -" , f + d , "=" , v
end do
close (123)
end subroutine ch


