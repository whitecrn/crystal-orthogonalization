module fun
implicit none
contains
real(kind=8) function cosin(a,b)
        real,dimension(3) :: a,b
        real :: temp1,temp2
        temp1=sqrt(a(1)**2+a(2)**2+a(3)**2)
        temp2=sqrt(b(1)**2+b(2)**2+b(3)**2)
        cosin=(a(1)*b(1)+a(2)*b(2)+a(3)*b(3))/(temp1*temp2)
end function cosin

real(kind=8) function cross(a,b)
        real,dimension(3) :: a,b
        cross=a(1)*b(2)-a(2)*b(1)
end function cross
end module fun
program nonlinear
        use fun
implicit none
integer :: x1,x2,x3,x4,s1,s2,s3,s4
integer :: i,j,k,l,m,n
integer :: up,down,temp,min_val
real,dimension(3) :: a,b,a_prime,b_prime
real :: error
a(1)=11.4364004135
a(2)=0.0
a(3)=0.0
b(1)=-5.7182015256
b(2)=9.9042125245
b(3)=0.0

up=10
down=-10
error=1e-4
min_val=1000000
loop1:do i=down,up
loop2:  do j=down,up
loop3:          do k=down,up
loop4:                  do l=down,up
                                if (i==0 .and. j==0) then
                                        cycle loop4
                                end if
                                if (k==0 .and. l==0) then
                                        cycle loop4
                                end if
                                x1=i
                                x2=j
                                x3=k
                                x4=l
                                if (x2*x3-x1*x4<0) then
                                        a_prime(1)=x1*a(1)+x2*b(1)
                                        a_prime(2)=x1*a(2)+x2*b(2)
                                        a_prime(3)=x1*a(3)+x2*b(3)
                                        b_prime(1)=x3*a(1)+x4*b(1)
                                        b_prime(2)=x3*a(2)+x4*b(2)
                                        b_prime(3)=x3*a(3)+x4*b(3)
                                        !write(*,*) abs(cosin(a_prime,b_prime))
                                        if (abs(cosin(a_prime,b_prime))<=error) then
                                                temp=abs(x1)+abs(x2)+abs(x3)+abs(x4)
                                                if (temp<min_val) then
                                                        min_val=temp

                                                        s1=x1
                                                        s2=x2
                                                        s3=x3
                                                        s4=x4
                                                end if
                                        end if
                                end if
                        end do loop4
                end do loop3
        end do loop2
end do loop1
write(*,*) s1,s2,s3,s4,min_val
stop
end program
                                                       
