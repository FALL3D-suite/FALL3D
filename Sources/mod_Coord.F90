!***************************************************************
!>
!> Module for coordinate operations
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE Coord
  use KindType, only: ip, rp, error_status
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: coord_ll2utm              ! Transform lat/lon to UTM
  PUBLIC :: coord_utm2ll              ! Transform UTM to lat/lon
  PUBLIC :: spherical_rotation        ! Rotate a point (lon/lat) about a pole
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: ll2osgb
  PRIVATE :: osgb2ll
  PRIVATE :: get_grid_zone
  PRIVATE :: get_lambda0
  PRIVATE :: isdigit
  !
  !
  !    LIST OF PRIVATE VARIABLES
  !
  !    World Geodetic System 1984 (WGS84)
  !       Ellipsoid: WGS84; a: 6378137,     b: 6356752.31425
  !       Helmert parameters (form this to WGS84, change all signs otherwise)
  !            tx:   0.0,  ty: - 0.0,  tz:  0.0,  // m
  !            rx:   0.0,  ry:   0.0,  rz:  0.0,  // sec
  !             s:   0.0                          // ppm
  !       True Origin: 0N, 0W
  !       False Origin: 500 km west, 0 km north
  !       Scale Factor: 0.9996
  !
  integer(ip), parameter :: WGS_84_DATUM = 0
  !
  !    North American Datum 1983 (NAD83)
  !       Ellipsoid: GRS80; a = 6378137.0  b = 6356752.31414
  !       Helmert parameters (form this to WGS84, change all signs otherwise)
  !            tx:    -1.004,  ty:   1.910,   tz:   0.515,  // m
  !            rx:    -0.0267 ,ry:    -0.00034, rz:    -0.011,  // sec
  !             s:    0.0015
  !       True Origin: 0N, 0W
  !       False Origin: 500 km west, 0 km north
  !       Scale Factor: 0.9996
  !
  integer(ip), parameter :: NAD83_DATUM  = 1
  !
  !    European Datum 1950 (ED50)
  !       Ellipsoid: International 1924; a = 6378388.0  b = 6356911.946
  !       Helmert parameters (form this to WGS84, change all signs otherwise)
  !            tx: -84.0,  ty: -107.0,  tz: -120.0,  // m
  !            rx:   0.0,  ry:    0.0,  rz: -0.156,  // sec
  !             s:   1.2                             // ppm
  !       True Origin: 0N, 0W
  !       False Origin: 500 km west, 0 km north
  !       Scale Factor: 0.9996
  !
  integer(ip), parameter :: ED50_DATUM = 2
  !
  !    European Terrestrial Reference System 1989 (ETRS89)
  !       Ellipsoid: GRS80; a = 6378137.0  b = 6356752.31414
  !       Helmert parameters (form this to WGS84, change all signs otherwise)
  !            tx:   0.0,  ty: - 0.0,  tz:  0.0,  // m
  !            rx:   0.0,  ry:   0.0,  rz:  0.0,  // sec
  !             s:   0.0                          // ppm
  !       True Origin: 0N, 0W
  !       False Origin: 500 km west, 0 km north
  !       Scale Factor: 0.9996
  !
  integer(ip), parameter :: ETRS89_DATUM = 3
  !
  !    Hellenic Geodetic Reference System 1987 (HGRS87)
  !       Ellipsoid: GRS80; a = 6378137.0  b = 6356752.31414
  !       Helmert parameters (form this to WGS84, change all signs otherwise)
  !            tx: -199.87,  ty: 74.79,   tz: 246.62,  // m
  !            rx:   0.0,    ry:    0.0,  rz: 0.0,     // sec
  !             s:   0.0                               // ppm
  !       True Origin: 0N, 24E
  !       False Origin: 500 km west, 0 km north
  !       Scale Factor: 0.9996
  !
  integer(ip), parameter :: HGRS87_DATUM = 4
  !
  !    Ordenance Survey National Grid (OSGB36)
  !       Ellipsoid: Airy 1830; a: 6377563.396, b: 6356256.909
  !       Helmert parameters (form this to WGS84, change all signs otherwise)
  !            tx:  446.448,  ty: -125.157,   tz: 542.060,  // m
  !            rx:   0.1502,  ry:    0.2470,  rz:   0.8421, // sec
  !             s:  -20.4894 }                               // ppm
  !       True Origin: 49N, 2W
  !       False Origin: 400 km west, 100 km north of True Origin
  !       Scale Factor: 0.9996
  !
  integer(ip), parameter :: OSGB36_DATUM = 5
  !
  !    Sistema de Referencia Geocentrico para las Americas 2000 (SIRGAS00)
  !       http://spatialreference.org/ref/epsg/sirgas-2000-utm-zone-23s/html/
  !       Ellipsoid: GRS80; a = 6378137.0  b = 6356752.31414
  !       Helmert parameters (form this to WGS84, change all signs otherwise)
  !            tx:   0.0,  ty: 0.0,  tz: 0.0,  // m
  !            rx:   0.0,  ry: 0.0,  rz: 0.0, // sec
  !             s:  0.0 }                               // ppm
  !       True Origin: 0N, 0E
  !       False Origin: 500 km west, 100000 km north
  !       Scale Factor: 0.9996
  !       Code EPSG   : 31983
  !
  integer(ip), parameter :: SIRGAS00_DATUM = 6
  !
  integer(ip), private, parameter :: GRID_ZONE_LENGTH = 3
  real(rp),    private, parameter :: LOWER_EPS_LIMIT = 1e-14_rp
  real(rp),    private, parameter :: M_PI   = 3.14159265358979323846_rp   ! pi
  real(rp),    private, parameter :: M_PI_2 = 1.57079632679489661923_rp   ! pi/2
  real(rp),    private, parameter :: RAD2DEG = 180.0_rp/M_PI              ! Convert rad to degrees
  real(rp),    private, parameter :: DEG2RAD = M_PI/180.0_rp              ! Convert degrees to radians
  !
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine coord_ll2utm
  !-----------------------------------------
  !
  !>   @brief
  !>   Converts lat/lon to UTM, using the specified datum
  !
  subroutine coord_ll2utm (rlon, rlat, grid_zone, utmx, utmy, datum, MY_ERR)
    implicit none
    !
    !>   @param  rlon      longitude
    !>   @param  rlat      latitude
    !>   @param  grid_zone output UMT zone
    !>   @param  utmx      x-coordinate
    !>   @param  utmy      y-coordinate
    !>   @param  datum     datum flag
    !>   @param  MY_ERR    error handler
    !
    real(rp),           intent(IN   ) :: rlon
    real(rp),           intent(IN   ) :: rlat
    character(len=3),   intent(  OUT) :: grid_zone
    real(rp),           intent(  OUT) :: utmx
    real(rp),           intent(  OUT) :: utmy
    integer(ip),        intent(IN   ) :: datum
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    character(len=3) :: utmgz
    integer(ip)      :: i
    real(rp)         :: a,b,f,e,e2,e4,e6,phi,phi0,t,x,y, rho, cc,cc2,dlamb0
    real(rp)         :: aa,aa2,aa3,aa4,aa5,aa6,lambda, k0, mm, mm0, nn, ep2,tt2,tt
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'coord_ll2utm'
    MY_ERR%message = ' '
    !
    !*** Parameters of the ellipsoid (in m) of each coordinate system
    !
    if(datum.eq.WGS_84_DATUM) then        ! Used by GoogleEarth
       a = 6378137.0
       b = 6356752.31425
    else if(datum.eq.NAD83_DATUM) then    ! Geodetic system that uses the GRS80 ellipsoid
       a = 6378137.0
       b = 6356752.31414
    else if(datum.eq.ED50_DATUM) then     ! Geodetic system that uses the INTERNATIONAL_1924 datum
       a = 6378388.0
       b = 6356911.946
    else if(datum.eq.ETRS89_DATUM) then   ! Geodetic system that uses the GRS80 ellipsoid
       a = 6378137.0
       b = 6356752.31414
    else if(datum.eq.HGRS87_DATUM) then   ! Geodetic system that uses the GRS80 ellipsoid
       a = 6378137.0
       b = 6356752.31414
    else if(datum.eq.OSGB36_DATUM) then   ! Geodetic system that uses the Airy 1830 ellipsoid
       !         a = 6377563.396
       !         b = 6356256.909
       !
       !  For OSGB36 do it differently
       !
       call ll2osgb(utmx,utmy,rlon,rlat,MY_ERR%flag)
       return
    else if(datum.eq.SIRGAS00_DATUM) then    ! Geodetic system that uses the GRS80 ellipsoid
       a = 6378137.0
       b = 6356752.31414
    else
       MY_ERR%flag    = 1
       MY_ERR%message = 'Datum not found'
       return
    endif
    !
    !***  Calculate flatness and eccentricity
    !
    f =1.0_rp - (b/a)
    e2 = (2.0_rp - f) * f
    e = sqrt(e2)
    e4 = e2*e2
    e6 = e4*e2
    !
    !***  Convert latitude/longitude to radians
    !
    phi = rlat * M_PI / 180.0_rp
    lambda = rlon * M_PI / 180.0_rp
    !
    !***  Figure out the UTM zone, as well as dlamb0
    !
    call get_grid_zone (rlat, rlon, utmgz, dlamb0)
    grid_zone = utmgz
    !
    !***  Force the value utm zone, i.e. get lamda0
    !***  Note that this will not give the real UTM zone
    !
    !      utmgz = grid_zone
    !      call get_lambda0 (utmgz,dlamb0,istat)


    phi0 = 0.0_rp
    !
    !***  See if this will use UTM or UPS
    !
    if (rlat.gt.84.0_rp) then
       !     use Universal Polar Stereographic Projection (north polar aspect)
       k0 = 0.994_rp

       t = sqrt(((1.0_rp-sin(phi))/(1.0_rp+sin(phi))) *  &
            ((1.0_rp+e*sin(phi))/(1.0_rp-e*sin(phi)))**e)
       rho = 2.0_rp*a*k0*t/sqrt((1.0_rp+e)**(1.0_rp+e)*(1.0_rp-e)**(1.0_rp-e))
       x = rho * sin(lambda - dlamb0)
       y = -rho * cos(lambda - dlamb0)
       !
       !***     Apply false easting/northing
       !
       x = x + 2e6_rp
       y = y + 2e6_rp

    elseif (rlat.lt.(-80.0_rp)) then

       !     use Universal Polar Stereographic Projection (south polar aspect)

       phi = -phi
       lambda = -lambda
       dlamb0 = -dlamb0

       k0 = 0.994_rp
       t = sqrt(((1.0_rp-sin(phi))/(1.0_rp+sin(phi))) *  &
            ((1.0_rp+e*sin(phi))/(1.0_rp-e*sin(phi)))**e)
       rho =2.0_rp*a*k0*t/ sqrt((1.0_rp+e)**(1.0_rp+e)*(1.0_rp-e)**(1.0_rp-e))

       x = rho * sin(lambda - dlamb0)
       y = -rho * cos(lambda - dlamb0)
       x = -x
       y = -y
       !
       !***    Apply false easting/northing
       !
       x = x + 2e6_rp
       y = y + 2e6_rp

    else

       !     Use UTM
       !     set scale on central median (0.9996 for UTM)

       k0 = 0.9996_rp
       mm = a * ((1.0_rp-e2/4.0_rp - 3.0_rp*e4/64.0_rp - 5.0_rp*e6/256.0_rp) * phi -  &
            (3.0_rp*e2/8.0_rp+3.0_rp*e4/32.0_rp+45.0_rp*e6/1024.0_rp)*sin(2*phi)+    &
            (15.0_rp*e4/256.0_rp + 45.0_rp*e6/1024.0_rp) * sin(4.0_rp*phi) -   &
            (35.0_rp*e6/3072.0_rp) * sin(6.0_rp*phi))

       mm0 = a * ((1.0_rp-e2/4._rp - 3._rp*e4/64._rp - 5._rp*e6/256._rp)*phi0 -  &
            (3._rp*e2/8._rp+3._rp*e4/32._rp+45._rp*e6/1024._rp)*sin(2._rp*phi0) +  &
            (15._rp*e4/256._rp + 45._rp*e6/1024._rp) * sin(4._rp*phi0) -        &
            (35._rp*e6/3072._rp) * sin(6._rp*phi0))

       aa = (lambda - dlamb0) * cos(phi)
       aa2 = aa*aa
       aa3 = aa2*aa
       aa4 = aa2*aa2
       aa5 = aa4*aa
       aa6 = aa3*aa3

       ep2 = e2 / (1.0_rp - e2)
       nn = a / sqrt(1.0_rp - e2*sin(phi)**2)
       tt = tan(phi)**2
       tt2 = tt*tt
       cc = ep2 * cos(phi)**2
       cc2 = cc*cc

       x = k0*nn*(aa+(1.0_rp-tt+cc)*aa3/6._rp+(5._rp-18._rp*tt+tt2+ &
            72._rp*cc-58._rp*ep2)*aa5/120._rp)
       y = k0*(mm-mm0+nn*tan(phi)*(aa2/2._rp+(5._rp-tt+9._rp*cc+ &
            4._rp*cc2)*aa4/24._rp+(61._rp-58._rp*tt+tt2+600._rp*cc-  &
            330._rp*ep2)*aa6/720._rp))
       !
       !***     Apply false easting and northing
       !
       x = x + 5e5_rp
       if(y.lt.0._rp) y = y + 1e7_rp
    endif
    !
    !***  Set entries in UTM structure
    !
    do i=1,GRID_ZONE_LENGTH
       grid_zone(i:i) = utmgz(i:i)
    enddo
    utmx = x
    utmy = y
    !
    !***  done
    !
    return
  end subroutine coord_ll2utm
  !
  !-----------------------------------------
  !    subroutine coord_utm2ll
  !-----------------------------------------
  !
  !>   @brief
  !>   Converts UTM to lat/lon, using the specified datum
  !
  subroutine coord_utm2ll (utmx,utmy,grid_zone,rlon,rlat,datum,MY_ERR)
    implicit none
    !
    !>   @param  utmx      x-coordinate
    !>   @param  utmy      y-coordinate
    !>   @param  grid_zone UMT zone
    !>   @param  rlon      longitude
    !>   @param  rlat      latitude
    !>   @param  datum     datum flag
    !>   @param  MY_ERR    error handler
    !
    real(rp),           intent(IN   ) :: utmx
    real(rp),           intent(IN   ) :: utmy
    character(len=3),   intent(IN   ) :: grid_zone
    real(rp),           intent(  OUT) :: rlon
    real(rp),           intent(  OUT) :: rlat
    integer(ip),        intent(IN   ) :: datum
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: lat_zone
    real(rp) :: a,b,f,e,e1,e2,e4,e6,e8,dlamb0,x,y,rho,t,chi,phit,phi,phi0
    real(rp) :: lambda, k0, mm, mm0, mu, nn1,e12,e13,e14,ep2,tt1
    real(rp) :: dd,dd2,dd3,dd4,dd5,dd6,cc1,rr1,phi1
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'coord_utm2ll'
    MY_ERR%message = ' '
    !
    !*** Parameters of the ellipsoid (in m) of each coordinate system
    !
    if(datum.eq.WGS_84_DATUM) then        ! Used by GoogleEarth
       a = 6378137.0
       b = 6356752.31425
    else if(datum.eq.NAD83_DATUM) then    ! Geodetic system that uses the GRS80 ellipsoid
       a = 6378137.0
       b = 6356752.31414
    else if(datum.eq.ED50_DATUM) then     ! Geodetic system that uses the INTERNATIONAL_1924 datum
       a = 6378388.0
       b = 6356911.946
    else if(datum.eq.ETRS89_DATUM) then   ! Geodetic system that uses the GRS80 ellipsoid
       a = 6378137.0
       b = 6356752.31414
    else if(datum.eq.HGRS87_DATUM) then   ! Geodetic system that uses the GRS80 ellipsoid
       a = 6378137.0
       b = 6356752.31414
    else if(datum.eq.OSGB36_DATUM) then   ! Geodetic system that uses the Airy 1830 ellipsoid
       !         a = 6377563.396
       !         b = 6356256.909
       !
       !  For OSGB36 do it differently
       !
       call osgb2ll(utmx,utmy,rlon,rlat,MY_ERR%flag)
       return
    else if(datum.eq.SIRGAS00_DATUM) then   ! Geodetic system that uses the GRS80 ellipsoid
       a = 6378137.0
       b = 6356752.31414
    else
       MY_ERR%flag    = 1
       MY_ERR%message = 'Datum not found'
       return
    endif
    !
    !***  Calculate flatness and eccentricity
    !
    f = 1.0_rp - (b/a)
    e2 = (2._rp - f) * f
    e = sqrt(e2)
    e4 = e2*e2
    e6 = e4*e2
    e8 = e4*e4
    !
    !***  Given the UTM grid zone, sets the central meridian  dlamb0
    !
    if(datum.eq.WGS_84_DATUM) then
       call get_lambda0(grid_zone, dlamb0,MY_ERR%flag)
       !
    elseif(datum.eq.NAD83_DATUM) then
       call get_lambda0(grid_zone, dlamb0,MY_ERR%flag)
       !
    else if(datum.eq.ED50_DATUM) then
       call get_lambda0(grid_zone, dlamb0,MY_ERR%flag)
       !
    else if(datum.eq.ETRS89_DATUM) then
       call get_lambda0(grid_zone, dlamb0,MY_ERR%flag)
       !
    else if(datum.eq.HGRS87_DATUM) then   ! Central meridian at 24E
       dlamb0 = 24.0*M_PI/180.
    else if(datum.eq.OSGB36_DATUM) then   ! Central meridian at 2W
       dlamb0 = -2.0*M_PI/180.
       !
    else if(datum.eq.SIRGAS00_DATUM) then
       call get_lambda0(grid_zone, dlamb0,MY_ERR%flag)
    end if
    !
    if(MY_ERR%flag.lt.0) then
       MY_ERR%flag = 1
       return
    endif
    !
    lat_zone = ichar(grid_zone(3:3))
    !
    !***  Different zones are trated differently
    !
    if(lat_zone.eq.ichar('Y').or.lat_zone.eq.ichar('Z')) then
       !
       !***    1. North polar aspect
       !
       !       Subtract the false easting/northing
       !
       x = utmx - 2e6_rp
       y = utmy - 2e6_rp
       !
       !***    Solve for inverse equations
       !
       k0 = 0.994_rp
       rho = sqrt(x*x + y*y)
       t = rho*sqrt((1.0_rp+e)**(1.0_rp+e)*(1.0_rp-e)**(1.0_rp-e))/(2._rp*a*k0)
       !
       !***     Solve for latitude and longitude
       !
       chi = M_PI_2 - 2._rp * atan(t)
       phit = chi + (e2/2._rp+5._rp*e4/24._rp+e6/12._rp+13._rp*e8/360._rp)* &
            sin(2._rp*chi)+(7._rp*e4/48._rp+29._rp*e6/240._rp+811._rp*e8/11520._rp)* &
            sin(4._rp*chi)+(7._rp*e6/120._rp+81._rp*e8/1120._rp)*sin(6._rp*chi)+ &
            (4279._rp*e8/161280._rp)*sin(8._rp*chi)
       !
10     continue
       phi = phit
       phit = M_PI_2 - 2._rp*atan(t*((1.0_rp-e*sin(phi))/(1.0_rp+e*sin(phi)))**(e/2._rp))

       if(abs(phi-phit).le.LOWER_EPS_LIMIT) goto 20
       goto 10
20     continue

       lambda = dlamb0 + atan2(x,-y)
       !
    elseif(lat_zone.eq.ichar('A').or.lat_zone.eq.ichar('B')) then
       !
       !***    2. South polar aspect
       !
       !       Subtract the false easting/northing
       !
       x = -(utmx - 2e6_rp)
       y = -(utmy - 2e6_rp)
       !
       !***     Solve for inverse equations
       !
       k0 = 0.994_rp
       rho = sqrt (x*x + y*y)
       t = rho*sqrt((1.0_rp+e)**(1.0_rp+e)*(1.0_rp-e)**(1.0_rp-e))/(2._rp*a*k0)
       !
       !***    Solve for latitude and longitude
       !
       chi = M_PI_2 - 2._rp * atan(t)
       phit = chi + (e2/2._rp + 5._rp*e4/24._rp + e6/12._rp + 13._rp*e8/360._rp)* &
            sin(2._rp*chi) + (7._rp*e4/48._rp + 29._rp*e6/240._rp + &
            811._rp*e8/11520._rp) * sin(4._rp*chi) + (7._rp*e6/120._rp + &
            81._rp*e8/1120._rp) * sin(6._rp*chi) + (4279._rp*e8/161280._rp)* &
            sin(8._rp*chi)
       !
11     continue
       phi = phit
       phit = M_PI_2 - 2._rp*atan(t*((1.0_rp-e*sin(phi))/(1.0_rp+e*sin(phi)))**(e/2._rp))
       !
       if(abs(phi-phit) .le. LOWER_EPS_LIMIT) goto 21
       goto 11
21     continue
       phi = -phi
       lambda = -(-dlamb0 + atan2(x,-y))
       !
       !
    else
       !
       !        3. Rest of the UTM locations
       !
       k0 = 0.9996_rp
       !
       !***     Remove false eastings/northings
       !
       x = utmx - 5e5_rp
       y = utmy
       !
       if(lat_zone.gt.ichar('B').and.lat_zone.lt.ichar('N')) y = y - 1e7_rp       ! southern hemisphere
       !
       !     Calculate the footpoint latitude
       !
       phi0 = 0.0_rp
       e1 = (1.0_rp - sqrt(1.0_rp-e2))/(1.0_rp + sqrt(1.0_rp-e2))
       e12 = e1*e1
       e13 = e1*e12
       e14 = e12*e12
       !
       mm0 = a * ((1.0_rp-e2/4._rp - 3._rp*e4/64._rp - 5._rp*e6/256._rp)*phi0- &
            (3._rp*e2/8._rp+3._rp*e4/32._rp+45._rp*e6/1024._rp)*sin(2._rp*phi0)+ &
            (15._rp*e4/256._rp + 45._rp*e6/1024._rp)*sin(4._rp*phi0)- &
            (35._rp*e6/3072._rp) * sin(6._rp*phi0))
       mm = mm0 + y/k0
       mu = mm/(a*(1.0_rp-e2/4._rp-3*e4/64._rp-5*e6/256._rp))
       !
       phi1 = mu + (3._rp*e1/2._rp - 27._rp*e13/32._rp) * sin(2._rp*mu)+ &
            (21._rp*e12/16._rp - 55._rp*e14/32._rp) * sin(4._rp*mu)+ &
            (151._rp*e13/96._rp) * sin(6._rp*mu)+ &
            (1097._rp*e14/512._rp) * sin(8._rp*mu)
       !
       !***    Now calculate lambda and phi
       !
       ep2 = e2/(1.0_rp-e2)
       cc1 = ep2*cos(phi1)**2
       tt1 = tan(phi1)**2
       nn1 = a / sqrt(1.0_rp-e2*sin(phi1)**2)
       rr1 = a * (1.0_rp-e2)/(1.0_rp-e2*sin(phi1)**2)**(1.5_rp)
       dd = x / (nn1 * k0)
       !
       dd2 = dd*dd
       dd3 = dd*dd2
       dd4 = dd2*dd2
       dd5 = dd3*dd2
       dd6 = dd4*dd2
       !
       phi = phi1-(nn1*tan(phi1)/rr1)*(dd2/2._rp-(5._rp+3._rp*tt1+  &
            10._rp*cc1-4._rp*cc1*cc1-9._rp*ep2)*dd4/24._rp+ &
            (61._rp+90._rp*tt1+298._rp*cc1+45._rp*tt1*tt1-252._rp*ep2- &
            3._rp*cc1*cc1)*dd6/720._rp)
       lambda = dlamb0 + (dd - (1.0_rp+2._rp*tt1+cc1)*dd3/6._rp + &
            (5._rp-2._rp*cc1+28._rp*tt1-3._rp*cc1*cc1+8._rp*ep2+24._rp*tt1*tt1)* &
            dd5/120._rp)/ cos(phi1)
    endif
    !
    !***  Convert phi/lambda to degrees
    !
    rlat = phi * 180.0_rp / M_PI
    rlon = lambda * 180.0_rp / M_PI
    !
    !***  All done
    !
    return
  end subroutine coord_utm2ll
  !
  !
  !   LIST OF PRIVATE ROUTINES
  !
  !
  subroutine ll2osgb(x,y,lon,lat,istat)
    !***********************************************************************
    !*
    !*    Converts lat/lon to OSGB36 to lat/lon using the native datum (Airy36).
    !*
    !************************************************************************
    implicit none
    integer(ip) :: istat
    real(rp)    :: x,y,lon,lat
    !
    real(rp) :: a,b,F0,lat0,lon0,N0,E0,e2,n,n2,n3
    real(rp) :: cos_lat,sin_lat,nu,eta2,rho
    real(rp) :: Ma,Mb,Mc,Md,M,cos3lat,cos5lat,tan2lat,tan4lat
    real(rp) :: I,II,III,IIIA,IV,V,VI,dL
    !
    istat = 0
    !
    !***  Define parameters
    !
    a    = 6377563.396      ! The Airy 180 semi-major and semi-minor axes used for OSGB36 (m)
    b    = 6356256.909
    F0   = 0.9996012717     ! scale factor on the central meridian
    lat0 = 49*M_PI/180.0    ! Latitude of true origin (radians)
    lon0 = -2*M_PI/180.0    ! Longtitude of true origin and central meridian (radians)
    N0   = -100000.0        ! Northing & easting of true origin (m)
    E0   =  400000.0
    !
    e2 = 1 - (b*b)/(a*a)    ! eccentricity squared
    n  = (a-b)/(a+b)
    n2 = n*n
    n3 = n*n*n
    !
    cos_lat = cos(lat*M_PI/180.0)
    sin_lat = sin(lat*M_PI/180.0)
    nu      = a*F0/sqrt(1-e2*sin_lat*sin_lat)                 ! transverse radius of curvature
    rho     = a*F0*(1-e2)/((1-e2*sin_lat*sin_lat)**(1.5_rp))  ! rho = meridional radius of curvature
    eta2    = nu/rho-1
    !
    Ma = (1 + n + (5./4.)*n2 + (5./4.)*n3) * (lat*M_PI/180.0-lat0)
    Mb = (3*n + 3*n*n + (21./8.)*n3) * sin(lat*M_PI/180.0-lat0) * cos(lat*M_PI/180.0+lat0)
    Mc = ((15./8.)*n2 + (15./8.)*n3) * sin(2*(lat*M_PI/180.0-lat0)) * cos(2*(lat*M_PI/180.0+lat0))
    Md = (35./24.)*n3 * sin(3*(lat*M_PI/180.0-lat0)) * cos(3*(lat*M_PI/180.0+lat0))
    M  = b * F0 * (Ma - Mb + Mc - Md)              ! meridional arc
    !
    cos3lat = cos_lat*cos_lat*cos_lat
    cos5lat = cos3lat*cos_lat*cos_lat
    tan2lat = tan(lat*M_PI/180.0)*tan(lat*M_PI/180.0)
    tan4lat = tan2lat*tan2lat
    !
    I    = M + N0
    II   = (nu/2.  )*sin_lat*cos_lat
    III  = (nu/24. )*sin_lat*cos3lat*(5-tan2lat+9*eta2)
    IIIA = (nu/720.)*sin_lat*cos5lat*(61-58*tan2lat+tan4lat)
    IV   = nu*cos_lat
    V    = (nu/6.  )*cos3lat*(nu/rho-tan2lat)
    VI   = (nu/120.)*cos5lat*(5 - 18*tan2lat + tan4lat + 14*eta2 - 58*tan2lat*eta2)
    !
    dL = lon*M_PI/180.0-lon0
    !
    y = I  + II*dL**2. + III*dL**4. + IIIA*dL**6.
    x = E0 + IV*dL + V*dL**3. + VI*dL**5.
    !
    return
  end subroutine ll2osgb
  !
  !
  !
  subroutine osgb2ll(x,y,lon,lat,istat)
    !***********************************************************************
    !*
    !*    Converts OSGB36 to lat/lon using the native datum (Airy36).
    !*    NOTE: to convert to lon-lat in WGS84 it is necessary to call ll2datum_WGS_84 later
    !*
    !************************************************************************
    implicit none
    integer(ip) :: istat
    real(rp) :: x,y,lon,lat
    !
    real(rp)    :: a,b,F0,lat0,lon0,N0,E0,e2,n,M,M1,M2,M3,M4
    real(rp)    :: nu,rho,eta2,secLat,VII,VIII,IX,XX,XI,XII,XIIA,dE
    !
    !***  Define parameters
    !
    a    = 6377563.396      ! The Airy 180 semi-major and semi-minor axes used for OSGB36 (m)
    b    = 6356256.909
    F0   = 0.9996012717     ! scale factor on the central meridian
    lat0 = 49*M_PI/180.0    ! Latitude of true origin (radians)
    lon0 = -2*M_PI/180.0    ! Longtitude of true origin and central meridian (radians)
    N0   = -100000.0        ! Northing & easting of true origin (m)
    E0   =  400000.0
    !
    e2 = 1 - (b*b)/(a*a)    ! eccentricity squared
    n  = (a-b)/(a+b)
    !
    !***  Initialise the iterative variables
    !
    lat = lat0
    M   = 0.0
    !
    do while( y-N0-M .ge. 0.001)  ! Accurate to 1 mm
       !
       lat = lat + (y-N0-M)/(a*F0)
       M1  = (1.0 + n + (5./4.)*n**2 + (5./4.)*n**3) * (lat-lat0)
       M2  = (3.*n + 3.*n**2 + (21./8.)*n**3) * sin(lat-lat0) * cos(lat+lat0)
       M3  = ((15./8.)*n**2 + (15./8.)*n**3) * sin(2.*(lat-lat0)) * cos(2.*(lat+lat0))
       M4  = (35./24.)*n**3 * sin(3.*(lat-lat0)) * cos(3.*(lat+lat0))
       M   = b * F0 * (M1 - M2 + M3 - M4)
    end do
    !
    nu = a*F0/sqrt(1-e2*sin(lat)**2)  ! transverse radius of curvature
    !
    rho  = a*F0*(1-e2)*(1-e2*sin(lat)**2)**(-1.5)  !  meridional radius of curvature
    eta2 = nu/rho-1.
    !
    secLat = 1./cos(lat)
    VII  = tan(lat)/(2*rho*nu)
    VIII = tan(lat)/(24*rho*nu**3)*(5+3*tan(lat)**2+eta2-9*tan(lat)**2*eta2)
    IX   = tan(lat)/(720*rho*nu**5)*(61+90*tan(lat)**2+45*tan(lat)**4)
    XX   = secLat/nu
    XI   = secLat/(6*nu**3)*(nu/rho+2*tan(lat)**2)
    XII  = secLat/(120*nu**5)*(5+28*tan(lat)**2+24*tan(lat)**4)
    XIIA = secLat/(5040*nu**7)*(61+662*tan(lat)**2+1320*tan(lat)**4+720*tan(lat)**6)
    !
    dE = x-E0
    !
    lat = lat - VII*dE**2 + VIII*dE**4 - IX*dE**6
    lon = lon0 + XX*dE - XI*dE**3 + XII*dE**5 - XIIA*dE**7
    !
    !***  Convert to degrees
    !
    lat = lat*180./M_PI
    lon = lon*180./M_PI
    !
    istat = 0
    !
    return
  end subroutine osgb2ll
  !
  !
  !
  subroutine get_grid_zone (rlat,rlon,grid_zone,dlamb0)
    !******************************************************************
    !*
    !*    Solve for the grid zone, returns the central meridian
    !*
    !*****************************************************************
    implicit none
    real(rp)         :: rlat,rlon,dlamb0
    character(len=3) :: grid_zone
    !
    integer(ip) :: long_zone,lat_zone
    !
    !*** First, let's take care of the polar regions
    !
    if(rlat.lt.(-80._rp)) then
       if (rlon.lt.0._rp) then
          grid_zone = '30A'
          dlamb0 =0.0_rp * M_PI / 180.0_rp
       else
          grid_zone = '31B'
          dlamb0 =0.0_rp * M_PI / 180.0_rp
       endif
       return
    else if(rlat.gt.84._rp) then
       if(rlon.lt.0._rp) then
          grid_zone = '30Y'
          dlamb0 =0.0_rp * M_PI / 180.0_rp
       else
          grid_zone = '31Z'
          dlamb0 =0.0_rp * M_PI / 180.0_rp
       endif
       return
    endif
    !
    !***  Now the special "X" grid
    !
    if(rlat.gt.72._rp.and.rlon.gt.0.and.rlon.lt.42._rp) then
       if(rlon.lt.9._rp) then
          dlamb0 = 4.5_rp
          grid_zone = '31X'
       else if(rlon.lt.21._rp) then
          dlamb0 = 15._rp * M_PI / 180.0_rp
          grid_zone = '33X'
       else if (rlon.lt.33._rp) then
          dlamb0 = 27._rp * M_PI / 180.0_rp
          grid_zone = '35X'
       else if (rlon.lt.42._rp) then
          dlamb0 = 37.5_rp * M_PI / 180.0_rp
          grid_zone = '37X'
       endif
       !
       return
    endif
    !
    !***  Handle the special "V" grid
    !
    if (rlat.gt.56._rp.and.rlat.lt.64._rp.and.rlon.gt.0._rp &
         .and.rlon.lt.12._rp) then
       if (rlon.lt.3._rp) then
          dlamb0 = 1.5_rp * M_PI / 180.0_rp
          grid_zone = '31V'
       else if (rlon.lt.12._rp) then
          dlamb0 = 7.5_rp * M_PI / 180.0_rp
          grid_zone = '32V'
       endif
       return
    endif
    !
    !***  The remainder of the grids follow the standard rule
    !
    long_zone = int((rlon - (-180.0_rp)) / 6._rp) + 1
    dlamb0 = ((long_zone - 1)*6._rp + (-180.0_rp)+3._rp)*M_PI/180.0_rp
    lat_zone = int((rlat-(-80._rp))/8._rp) + ichar('C')
    !
    if(lat_zone.gt.ichar('H')) lat_zone = lat_zone+1
    if(lat_zone.gt.ichar('N')) lat_zone = lat_zone+1
    !
    if(rlat.gt.80._rp) lat_zone = ichar('X')
    !
    grid_zone(1:1) = char((long_zone/10) + ichar('0'))
    grid_zone(2:2) = char(mod(long_zone,10) + ichar('0'))
    grid_zone(3:3) = char(lat_zone)
    !
    return
  end subroutine get_grid_zone
  !
  !
  !
  subroutine get_lambda0 (grid_zone,dlamb0,istat)
    !********************************************************************
    !*
    !*    Given the grid zone, sets the central meridian, dlamb0
    !*
    !********************************************************************
    implicit none
    character(len=3) :: grid_zone
    integer(ip)      :: istat
    real(rp)         :: dlamb0
    !
    integer(ip) :: long_zone,lat_zone
    !
    !***  Check the grid zone format
    !
    if (.not.isdigit(grid_zone(1:1)).or. &
         .not.isdigit(grid_zone(2:2))) then
       istat = -1
       return
    endif
    !
    long_zone = (ichar(grid_zone(1:1)) - ichar('0')) * 10 &
         + (ichar(grid_zone(2:2)) - ichar('0'))
    lat_zone  =  ichar(grid_zone(3:3))
    !
    !***  Take care of special cases
    !
    if(lat_zone.eq.ichar('A').or.lat_zone.eq.ichar('B').or. &
         lat_zone.eq.ichar('Y').or.lat_zone.eq.ichar('Z')) then
       dlamb0 =0.0_rp
       istat=0
       return
    elseif(lat_zone.eq.ichar('V')) then
       if(long_zone.eq.31) then
          dlamb0 = 1.5_rp * M_PI / 180.0_rp
          istat=0
          return
       elseif(lat_zone.eq.32) then
          dlamb0 = 7.5_rp * M_PI / 180.0_rp
          istat=0
          return
       endif
    elseif(lat_zone.eq.ichar('X')) then
       if(long_zone.eq.31) then
          dlamb0 = 4.5_rp * M_PI / 180.0_rp
          istat=0
          return
       elseif(lat_zone.eq.33) then
          dlamb0 = 15._rp * M_PI / 180.0_rp
          istat=0
          return
       elseif(lat_zone.eq.35) then
          dlamb0 = 27._rp * M_PI / 180.0_rp
          istat = 0
          return
       elseif(lat_zone.eq.37) THEN
          dlamb0 = 37.5_rp * M_PI / 180.0_rp
          istat = 0
          return
       elseif(lat_zone.eq.32.or.lat_zone.eq.34.or.lat_zone.eq.36) then
          istat = -1
          return
       endif
    endif
    !
    !***  Now handle standard cases
    !
    dlamb0 = ((long_zone-1)*6._rp+(-180.0_rp)+3._rp) * M_PI / 180.0_rp
    !
    !***  All done
    !
    istat = 0
    return
  end subroutine get_lambda0
  !
  !
  !
  logical function isdigit(a)
    implicit none
    character(len=1) :: a
    if(ichar(a).ge.ichar('0').and.ichar(a).le.ichar('9')) then
       isdigit = .true.
    else
       isdigit = .false.
    endif
    return
  end function isdigit
  !
  !
  ! SUBROUTINES FOR COORDINATE AND VECTOR ROTATIONS
  !
  !
  subroutine l2c(lon, lat, x, y, z)
    ! Transform geographical coordinates (lon, lat) to cartesian coordinates
    ! Longitiude and latitude are in degrees
    ! The vector (x,y,z) is normalized to one
    !
    ! Author: Giovanni Macedonio (Last upgrade 9-SEP-2021)
    !
    implicit none
    real(rp), intent(in)  :: lon, lat
    real(rp), intent(out) :: x, y, z
    real(rp) :: r
    r = cos(deg2rad*lat)
    z = sin(deg2rad*lat)
    x = r*cos(deg2rad*lon)
    y = r*sin(deg2rad*lon)
  end subroutine l2c

  subroutine c2l(x, y, z, lon, lat)
    ! Transform cartesian coordinates (x,y,z) to geographical coordinates (lon, lat)
    ! The vector (x,y,z) must be normalized to one
    ! Longitiude and latitude are in degrees
    !
    ! Author: Giovanni Macedonio (last upgrade 9-SEP-2021)
    !
    implicit none
    real(rp), intent(in)  :: x, y, z
    real(rp), intent(out) :: lon, lat

    lat = rad2deg*asin(z)     ! Asin returns the angle in the range [-pi/2, pi/2]
    lon = rad2deg*atan2(y,x)  ! Atan2 returns angles in the range [-pi, pi]
  end subroutine c2l

  subroutine cartesian_rotate3d(u, v, w, theta, x, y, z, xnew, ynew, znew, beta)
    ! Rotates vector (x,y,z) around the axis (u,v,w) by an angle theta
    ! Angle theta is in degrees, taken counterclockwise
    ! The output vector (xnew,ynew,xnew) can share the same memory of the input vector
    !
    ! The optional output argument (beta) contains the rotation angle of a
    ! vector tangent to the sphere initially located at (x,y,z)
    !
    ! NOTE: The input vector (x,y,z) and the rotation axis vector (u,v,w) must be
    ! normalized to one
    !
    ! Author: Giovanni Macedonio (Last upgrade 15-JUL-2022)
    !
    implicit none
    real(rp), intent(in)    :: u, v, w           ! Rotation axis (normalized to one)
    real(rp), intent(in)    :: theta             ! Rotation angle (degrees)
    real(rp), intent(inout) :: x, y, z           ! Point
    real(rp), intent(inout) :: xnew, ynew, znew  ! Rotated point
    real(rp), optional, intent(out) :: beta      ! Rotation angle of a tangent vector
    real(rp) :: mat(3,3)
    real(rp) :: angle, s, c, t, u2, v2, w2, xt, yt, zt, tlon, tlat, cosa
    !
    angle = deg2rad*theta     ! Transform angle in radians
    cosa = x*u + y*v + z*w    ! Cos of angle between (x,y,z) and (u,v,w)
    s = sin(angle)
    c = cos(angle)
    t = 1.0_rp - c
    u2 = u**2
    v2 = v**2
    w2 = w**2
    ! Set the rotation matrix
    mat(1,1) = t*u2 + c
    mat(1,2) = t*u*v - s*w
    mat(1,3) = t*u*w + s*v
    mat(2,1) = t*u*v + w*s
    mat(2,2) = t*v2 + c
    mat(2,3) = t*v*w - s*u
    mat(3,1) = t*u*w - s*v
    mat(3,2) = t*v*w + s*u
    mat(3,3) = t*w2 + c
    ! Multiply (rotate)
    xt = mat(1,1)*x + mat(1,2)*y + mat(1,3)*z
    yt = mat(2,1)*x + mat(2,2)*y + mat(2,3)*z
    zt = mat(3,1)*x + mat(3,2)*y + mat(3,3)*z
    ! Copy
    xnew = xt
    ynew = yt
    znew = zt
    ! Evaluate beta
    if(present(beta)) beta = theta*cosa
  end subroutine cartesian_rotate3d

  subroutine spherical_rotation(lonpole,latpole,angle,oldlon,oldlat,newlon,newlat,beta)
    ! Rotates a point with coordinates (oldlon,oldlat) about a pole
    ! Angles are expressed in degrees
    ! Note: oldlon/oldlat may share the same memory address of newlon/newlat
    !
    ! The optional output argument (beta) contains the rotation angle of a
    ! vector tangent to the sphere initially located at (oldlon,oldlat)
    !
    ! Author: Giovanni Macedonio (Last upgrade 12-SEP-2021)
    !
    implicit none
    real(rp), intent(in)    :: lonpole,latpole
    real(rp), intent(in)    :: angle
    real(rp), intent(inout) :: oldlon,oldlat
    real(rp), intent(inout) :: newlon,newlat
    real(rp), optional, intent(out) :: beta
    real(rp) :: xp,yp,zp,x,y,z,locbeta
    !
    ! Convert rotation axis to cartesian
    call l2c(lonpole, latpole, xp, yp, zp)
    ! Convert point to cartesian
    call l2c(oldlon, oldlat, x, y, z)
    ! Rotate
    call cartesian_rotate3d(xp, yp, zp, angle, x, y, z, x, y, z, locbeta)
    ! Convert output to geographical coordinates
    call c2l(x, y, z, newlon, newlat)
    ! Returns beta (if present)
    if(present(beta)) beta = locbeta
    !
  end subroutine spherical_rotation

  !
END MODULE Coord
