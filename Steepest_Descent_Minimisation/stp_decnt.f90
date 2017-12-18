
!-----------------------------------------------------------------------------!
! 	FORTRAN90 PROGRAM to find  local minimum of a function by using       !
!		Steepest Descent Minimization Algarithm			      !	
!									      !
!									      !
!  Authour  :  ANJI BABU KAPKAYALA					      !
!              IIT KANPUR, INDIA.					      !
!									      !
!  SHO : U(x) = (x**2 -1)**2 						      !
!        Negative Gradient of U(x) = -4*(x**3-x)  			      !
!-----------------------------------------------------------------------------!

PROGRAM stp_decnt
IMPLICIT NONE
REAL*8  :: x0, x, minimum, f, dt, k,pe
INTEGER*8 :: i,nsteps
REAL*8, PARAMETER :: cut_off=0.00001
dt = 0.0001
OPEN (11,file='DATA-new.dat',STATUS='UNKNOWN')
PRINT*, " Enter Intial Position : "
READ*, x0
!RINT*, " Enter No. Of Steps : "
!EAD*, nsteps
nsteps=1500000
DO i=1,nsteps
call force (x0,f,pe)
!print*, f
call posi_update (x0,dt,f,x)
x0=x
if (abs(f) <= cut_off) then
!print*,i,x,f
WRITE(*,100) x ,f
 100 FORMAT(3X,"Minimum=",2X,F15.5,3X,"At Force : ",2X,F15.5)
exit
endif
WRITE(11,120) i,x,pe,f
120 FORMAT(3X,I10,3X,F15.5,3X,F15.5,3X,F15.5)
END DO
CLOSE(11)
END PROGRAM stp_decnt
!
SUBROUTINE posi_update (x0,dt,f,x)
IMPLICIT NONE
REAL*8 , INTENT(IN)  :: x0, dt, f
REAL*8 , INTENT(OUT) :: x
x=x0+dt*f
END SUBROUTINE posi_update
!
SUBROUTINE force (x,f,pe)
IMPLICIT NONE
REAL*8 , INTENT(IN)  :: x
REAL*8 , INTENT(OUT) :: f, pe
!INTEGER*8  :: k=1
! u = 0.5*k*x**2
  pe=((x**2)-1)**2
  f=-4.0*(x**3-x)
END SUBROUTINE force
!----------------------------------------------!
!       Written By ANJI BABU KAPAKAYALA	       !
!----------------------------------------------!
