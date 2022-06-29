real function acclaration(x)
implicit none
real::x,a
a=-(x)                        !k=m=1 assumed
acclaration=a
end function acclaration



program ODE
implicit none
real::dt,acclaration,time,Tf
real,allocatable,dimension(:)::x,v,energy
integer::i,Tff
write(*,*)"dt"
read(*,*)dt
write(*,*)"final time"
read(*,*)Tf
Tff=Tf/dt 
time=0
allocate(x(Tff+1))
write(*,*)"initial position"
read(*,*)x(1)
open(unit=1,file="xx005.dat",status="replace")
write(1,3)time,x(1)
allocate(v(Tff+1))
write(*,*)"initial velocity"
read(*,*)v(1)
open(unit=2,file="vv005.dat",status="replace")
write(2,3)time,v(1)
allocate(energy(Tff+1))
energy(1)=0.5*(x(1)*x(1)+v(1)*v(1))        !k=m=1 assumed
open(unit=3,file="ee005.dat",status="replace")
write(3,4)time,energy(1)
3 format(f8.3,4x,f8.3)
4 format(f8.3,4x,f8.3)
do i=2,Tff+1
x(i)=x(i-1)+dt*v(i-1)
v(i)=v(i-1)+dt*acclaration(x(i-1))
energy(i)=0.5*(x(i)*x(i)+v(i)*v(i))
time=time+dt
write(1,3)time,x(i)
write(2,3)time,v(i)
write(3,4)time,energy(i)
end do
end program

