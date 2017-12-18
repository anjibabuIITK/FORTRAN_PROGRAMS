!-----------------------------------------------------------------------!
!          PROGRAM to find local minimum of a function by using         !
!                 Steepest Descent Minimization Algarithm	        !
!			(SIMPLE HARMONIC OSCILLATOR)			!
!                                                                       !
!  Authour  :  ANJI BABU KAPKAYALA                                      !
!              IIT KANPUR, INDIA.					!
!									!
!  SHO : U(x) = 1/2*k*x**2   						!
!        Negative Gradient of U(x) = -kx				!
!-----------------------------------------------------------------------!
PROGRAM steepest
IMPLICIT NONE
REAL*8  :: x0, x, minimum, f, dt, k,u
INTEGER*8 :: i,nsteps
REAL*8, PARAMETER :: cutoff=0.00001
dt = 0.0001
k=1
OPEN (11,file='DATA.dat',STATUS='UNKNOWN')
PRINT*, " Enter Intial Position : "
READ*, x0
PRINT*, " Enter No. Of Steps : "
READ*, nsteps
DO i=1,nsteps
call force (x0,f,u )
!print*, f
call posi_update (x0,dt,f,x)
x0=x
if (abs(f) <= cutoff) then
WRITE(*,100) x,f
100 FORMAT(3X,"Minimun:",2X,F15.5,3X,"Force:",2X,F15.5)
exit
endif
WRITE(11,120) i,x,u,f
120 FORMAT(3X,I10,2X,F15.5,2X,F15.5,2X,F15.5)
END DO
CLOSE(11)
END PROGRAM steepest
!----------------------------------------------!
SUBROUTINE posi_update (x0,dt,f,x)
IMPLICIT NONE
REAL*8 , INTENT(IN)  :: x0, dt, f
REAL*8 , INTENT(OUT) :: x
x=x0+dt*f
END SUBROUTINE posi_update
!----------------------------------------------!
SUBROUTINE force (x,f,u)
IMPLICIT NONE
REAL*8 , INTENT(IN)  :: x
REAL*8 , INTENT(OUT) :: f, u
INTEGER*8  :: k=1
 u = 0.5*k*x**2
 f = -k*x
END SUBROUTINE force
!---------------------------------------------------------------------!
!                  Written By ANJI BABU KAPAKAYALA	       	      !
!---------------------------------------------------------------------!
