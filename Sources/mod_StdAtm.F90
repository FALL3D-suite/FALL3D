!***************************************************************
!>
!> Module for standard atmosphere calculation
!> @author
!> Arnau Folch and Giovanni Macedonio
!>
!***************************************************************
MODULE StdAtm
  use KindType
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: stdatm_ta
  PUBLIC :: stdatm_pa
  PUBLIC :: stdatm_rhoa
  !
  !    LIST OF PRIVATER ROUTINES IN THE MODULE
  !
  PRIVATE :: atmosphere
  !
  !
  !    LIST OF PRIVATE VARIABLES
  !
  real(rp), PRIVATE, parameter :: pref0 = 101325.0_rp    !< Ref. pressure at sea level
  real(rp), PRIVATE, parameter :: tref0 = 288.15_rp      !< Ref. temperatrure at sea level
  real(rp), PRIVATE, parameter :: rhoa0 = 1.225002_rp    !< Ref. density at sea level
  !
CONTAINS
  !
  !-----------------------------------------
  !    function stdatm_ta
  !-----------------------------------------
  !
  !>   @brief
  !>   Return temperature of the Standard Atmosphere
  !
  real(rp) function stdatm_ta(zeta)
    implicit none
    !
    !>   @param zeta  elevation in m (a.s.l.)
    !
    real(rp), intent(in) :: zeta
    !
    real(rp)             :: sigma,delta,theta
    call atmosphere(zeta,sigma,delta,theta)
    stdatm_ta = tref0*theta
  end function stdatm_ta
  !
  !-----------------------------------------
  !    function stdatm_pa
  !-----------------------------------------
  !
  !>   @brief
  !>   Return pressure of the Standard Atmosphere
  !
  real(rp) function stdatm_pa(zeta)
    implicit none
    !
    !>   @param zeta  elevation in m (a.s.l.)
    !
    real(rp), intent(in) :: zeta
    !
    real(rp)             :: sigma,delta,theta
    call atmosphere(zeta,sigma,delta,theta)
    stdatm_pa = pref0*delta
  end function stdatm_pa
  !
  !-----------------------------------------
  !    function stdatm_rhoa
  !-----------------------------------------
  !
  !>   @brief
  !>   Return denity of the Standard Atmosphere
  !
  real(rp) function stdatm_rhoa(zeta)
    implicit none
    !
    !>   @param zeta  elevation in m (a.s.l.)
    !
    real(rp), intent(in) :: zeta
    !
    real(rp)             :: sigma,delta,theta
    call atmosphere(zeta,sigma,delta,theta)
    stdatm_rhoa = rhoa0*sigma
  end function stdatm_rhoa
  !
  !
  !    PRIVATE ROUTINES
  !
  !
  !
  SUBROUTINE Atmosphere(zeta, sigma, delta, theta)   ! Modified alt->zeta (G.M.)
    !   ------------------------------------------------------------------------
    ! PURPOSE - Compute the properties of the 1976 standard atmosphere to 86 km.
    ! AUTHOR - Ralph Carmichael, Public Domain Aeronautical Software
    ! NOTE - If alt > 86, the values returned will not be correct, but they will
    !   not be too far removed from the correct values for density.
    !   The reference document does not use the terms pressure and temperature
    !   above 86 km.
    !
    IMPLICIT NONE
    !===========================================================================
    !     A R G U M E N T S                                                    |
    !===========================================================================
    REAL(RP),INTENT(IN) :: zeta   ! geometric altitude, m. (Inserted by G.M.)
    REAL(RP)            :: alt    ! geometric altitude, km.  (Modified by G.M.)
    REAL(RP),INTENT(OUT):: sigma  ! density/sea-level standard density
    REAL(RP),INTENT(OUT):: delta  ! pressure/sea-level standard pressure
    REAL(RP),INTENT(OUT):: theta  ! temperature/sea-level standard temperature
    !===========================================================================
    !     L O C A L   C O N S T A N T S                                        |
    !===========================================================================
    REAL(RP),PARAMETER:: REARTH = 6369.0            ! radius of the Earth (km)
    REAL(RP),PARAMETER:: GMR = 34.163195                 ! hydrostatic constant
    INTEGER,PARAMETER:: NTAB=8       ! number of entries in the defining tables
    !===========================================================================
    !     L O C A L   V A R I A B L E S                                        |
    !===========================================================================
    INTEGER:: i,j,k                                                  ! counters
    REAL(RP):: h                                   ! geopotential altitude (km)
    REAL(RP):: tgrad, tbase  ! temperature gradient and base temp of this layer
    REAL(RP):: tlocal                                       ! local temperature
    REAL(RP):: deltah                         ! height above base of this layer
    !===========================================================================
    !     L O C A L   A R R A Y S   ( 1 9 7 6   S T D.  A T M O S P H E R E )  |
    !===========================================================================
    REAL(RP),DIMENSION(NTAB),PARAMETER:: htab= &
         (/0.0, 11.0, 20.0, 32.0, 47.0, 51.0, 71.0, 84.852/)
    REAL(RP),DIMENSION(NTAB),PARAMETER:: ttab= &
         (/288.15, 216.65, 216.65, 228.65, 270.65, 270.65, 214.65, 186.946/)
    REAL(RP),DIMENSION(NTAB),PARAMETER:: ptab= &
         (/1.0, 2.233611E-1, 5.403295E-2, 8.5666784E-3, 1.0945601E-3, &
         6.6063531E-4, 3.9046834E-5, 3.68501E-6/)
    REAL(RP),DIMENSION(NTAB),PARAMETER:: gtab= &
         (/-6.5, 0.0, 1.0, 2.8, 0.0, -2.8, -2.0, 0.0/)
    !---------------------------------------------------------------------------
    !
    alt = 0.001_rp*zeta             ! Convert altitude to km (Inserted by G.M.)
    !
    h=alt*REARTH/(alt+REARTH)      ! convert geometric to geopotential altitude

    i=1
    j=NTAB                                       ! setting up for binary search
    DO
       k=(i+j)/2                                              ! integer division
       IF (h < htab(k)) THEN
          j=k
       ELSE
          i=k
       END IF
       IF (j <= i+1) EXIT
    END DO

    tgrad=gtab(i)                                     ! i will be in 1...NTAB-1
    tbase=ttab(i)
    deltah=h-htab(i)
    tlocal=tbase+tgrad*deltah
    theta=tlocal/ttab(1)                                    ! temperature ratio

    IF (tgrad == 0.0) THEN                                     ! pressure ratio
       delta=ptab(i)*EXP(-GMR*deltah/tbase)
    ELSE
       delta=ptab(i)*(tbase/tlocal)**(GMR/tgrad)
    END IF

    sigma=delta/theta                                           ! density ratio
    RETURN
  END Subroutine Atmosphere
  !
END MODULE StdAtm
