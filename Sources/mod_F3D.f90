!***********************************************************************
!>
!> Module for procedures related to Fall3d task executionCLASS NAME
!> @author
!> Arnau Folch
!>
!**********************************************************************
MODULE F3D
  use KindType
  use Parallel
  use Time
  use Grid
  use Phys
  use Src
  use nc_IO
  use ADS
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: F3D_initialize
  PUBLIC :: F3D_release
  PUBLIC :: F3D_write_data
  PUBLIC :: F3D_set_source_term
  PUBLIC :: F3D_update_meteo_term
  PUBLIC :: F3D_end_time_step
  PUBLIC :: F3D_time_step
  PUBLIC :: F3D_set_pts
  PUBLIC :: F3D_out_pts_grn
  PUBLIC :: F3D_add_radial_wind
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine F3D_initialize
  !-----------------------------------------
  !
  !>   @brief
  !>   Allocate resources and initialize variables
  !
  subroutine F3D_initialize(limiter,time_marching)
    implicit none
    integer(ip), intent(in)    :: limiter
    integer(ip), intent(in)    :: time_marching

    call ADS_initialize(limiter,time_marching)

  end subroutine F3D_initialize

  !
  !-----------------------------------------
  !    subroutine F3D_release
  !-----------------------------------------
  !
  !>   @brief
  !>   Release resources used by F3D
  !
  subroutine F3D_release()
    implicit none

    call ADS_release()

  end subroutine F3D_release

  !
  !
  !-----------------------------------------
  !    subroutine F3D_write_data
  !-----------------------------------------
  !
  !>   @brief
  !>   Writes input data to the log file
  !
  subroutine F3D_write_data(MY_FILES,MY_TIME,MY_GRID,MY_MOD,MY_TRA,MY_OUT,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TIME   run time related parameters
    !>   @param MY_GRID   grid configuration parameters
    !>   @param MY_MOD    model physics related parameters
    !>   @param MY_TRA    variables related to tracers
    !>   @param MY_OUT    model output and postprocess related parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(RUN_TIME),      intent(IN   ) :: MY_TIME
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(MODEL_PHYS),    intent(IN   ) :: MY_MOD
    type(TRACERS),       intent(IN   ) :: MY_TRA
    type(MODEL_OUTPUT),  intent(IN   ) :: MY_OUT
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    character(len=24) :: time_str,attr_mh,attr_mv,str
    integer(ip)       :: lulog
    integer(ip)       :: iyr,imo,idy,ihr,imi,ise
    integer(ip)       :: ic,nbins,ipts,k
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'F3D_write_data'
    MY_ERR%message = ' '
    !
    lulog = MY_FILES%lulog
    !
    write(lulog,10)
10  format(&
         '----------------------------------------------------',/,   &
         '                                                    ',/,   &
         '               METEO AND GRID DATA                  ',/,   &
         '                                                    ',/,   &
         '----------------------------------------------------')
    !
    call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
         iyr,imo,idy,ihr,imi,ise,MY_TIME%dbs_start,MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str, MY_ERR)
    write(lulog,20) TRIM(time_str)
20  format(/,&
         'TIME RANGE OF METEO DATA',/, &
         '  Initial time       : ',a)
    !
    call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
         iyr,imo,idy,ihr,imi,ise,MY_TIME%dbs_end,MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str, MY_ERR)
    write(lulog,21) TRIM(time_str)
21  format('  Final   time       : ',a)
    !
    write(lulog,22) (MY_TIME%dbs_end-MY_TIME%dbs_start)/36d2,INT(MY_TIME%dbs_end-MY_TIME%dbs_start)
22  format('  Meteo coverage     : ',f6.1,' h (',i9,' sec)')
    !
    select case(MY_GRID%map_h)
    case(MAP_H_CARTESIAN)
       attr_mh = 'cartesian'
    case(MAP_H_SPHERICAL)
       attr_mh = 'spherical'
    case(MAP_H_POLAR)
       attr_mh = 'polar_stereographic'
    case(MAP_H_MERCATOR)
       attr_mh = 'mercator'
    case default
       attr_mh = ''
    end select
    !
    select case(MY_GRID%map_v)
    case(MAP_V_CARTESIAN)
       attr_mv = 'cartesian'
    case(MAP_V_SIGMA_NO_DECAY)
       attr_mv = 'sigma_no_decay'
    case(MAP_V_SIGMA_LINEAR_DECAY)
       attr_mv = 'sigma_linear_decay'
    case(MAP_V_SIGMA_EXPONENTIAL_DECAY)
       attr_mv = 'sigma_exponential_decay'
    case default
       attr_mv = ''
    end select
    !
    write(lulog,31) TRIM(attr_mh), &
         MY_GRID%lonmin,MY_GRID%latmin,MY_GRID%lonmax,MY_GRID%latmax,MY_GRID%X3max, &
         np(1),np(2),np(3),MY_GRID%dlon,MY_GRID%dlat
    write(lulog,32) (MY_GRID%gl_sigma(k),k=1,gl_nbz)
31  format(/, &
         'MESH',/, &
         '  Horizontal mapping :  ',a               ,/,      &
         '  Bottom-left corner : (',f9.4,2x,f9.4,')',/,      &
         '  Top-right   corner : (',f9.4,2x,f9.4,')',/,      &
         '  z top domain       : ',f9.1             ,/,      &
         '  Number points x    : ',i9  ,/,                   &
         '  Number points y    : ',i9  ,/,                   &
         '  Number points z    : ',i9  ,/,                   &
         '  Grid incr. (deg)   : ',f9.5,/,                   &
         '  Grid incr. (deg)   : ',f9.5)
32  format(&
         '  Sigma levels       : ',100(10(f7.4,1x),/,'                       '))
    !
    write(lulog,100)
100 format(/,&
         '----------------------------------------------------',/,    &
         '                                                    ',/,    &
         '               FALL3D INPUT DATA                    ',/,    &
         '                                                    ',/,    &
         '----------------------------------------------------')
    !
    call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
         iyr,imo,idy,ihr,imi,ise,MY_TIME%run_start,MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str, MY_ERR)
    write(lulog,120) TRIM(time_str)
120 format(/,&
         'FALL3D TIME RANGE',/, &
         '  Initial time       : ',a)
    !
    call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
         iyr,imo,idy,ihr,imi,ise,MY_TIME%run_end,MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str, MY_ERR)
    write(lulog,121) TRIM(time_str)
121 format('  Final   time       : ',a)
    !
    write(lulog,122) (MY_TIME%run_end-MY_TIME%run_start)/36d2,INT(MY_TIME%run_end-MY_TIME%run_start)
122 format('  Time increment     : ',f6.1,' h (',i9,' sec)')
    !
    if(MY_TIME%restart) then
       write(lulog,123)  TRIM(MY_FILES%file_rst)
    else
       write(lulog,124)
    end if
123 format('  restart run        : yes',/,&
         '  restart file       : ',a)
124 format('  restart run        : no ')
    !
    write(lulog,260)
260 format(/,                         &
         'NUMERICAL TREATMENT')
    !
    select case(MY_MOD%modkv)
    case(MOD_CONSTANT)
       write(lulog,261) MY_MOD%kv0
    case(MOD_SIMILARITY)
       write(lulog,262)
    end select
261 format('  Vertical turbulence      : Kv constant ( ',e12.4,' )')
262 format('  Vertical turbulence      : Kv similarity theory   ')
    !
    select case(MY_MOD%modkh)
    case(MOD_CONSTANT)
       write(lulog,271) MY_MOD%kh0
    case(MOD_CMAQ)
       write(lulog,272)
    end select
271 format('  Horizontal turbulence    : Kh constant ( ',e12.4,' )')
272 format('  Horizontal turbulence    : Kh CMAQ                ')
    !
    select case(MY_MOD%modv)
    case(MOD_ARASTOPOUR)
       str = 'Arastoopour et al., 1982'
    case(MOD_GANSER)
       str = 'Ganser, 1993'
    case(MOD_WILSON)
       str = 'Wilson and Huang 1979'
    case(MOD_DELLINO)
       str = 'Dellino et al., 2005'
    case(MOD_PFEIFFER)
       str = 'Pfeiffer et al., 2005'
    case(MOD_DIOGUARDI2017)
       str = 'Dioguardi et al., 2017'
    case(MOD_DIOGUARDI2018)
       str = 'Dioguardi et al., 2018'
    end select
    write(lulog,280) str
280 format('  Settling velocity model  : ',a)
    !
    nbins = MY_TRA%MY_BIN%nbins
    write(lulog,320) nbins
    write(lulog,321) (TRIM(MY_TRA%MY_BIN%bin_name(ic)),ic=1,nbins)
    write(lulog,322) (MY_TRA%MY_BIN%bin_diam(ic)*1e3_rp,ic=1,nbins)
    write(lulog,323) (-log(1e3_rp*MY_TRA%MY_BIN%bin_diam(ic))/log(2.0_rp),ic=1,nbins)
    write(lulog,324) (MY_TRA%MY_BIN%bin_rho(ic),ic=1,nbins)
    write(lulog,326) (MY_TRA%MY_BIN%bin_sphe(ic),ic=1,nbins)
    write(lulog,327) (MY_TRA%MY_BIN%bin_fc(ic)*1d2,ic=1,nbins)
    write(lulog,328) sum(MY_TRA%MY_BIN%bin_fc(1:nbins))*1d2
    !
320 format(/,                                       &
         'GRANULOMETRIC DISTRIBUTION            ',/,     &
         '  NUMBER OF EFFECTIVE BINS          : ',i4)
321 format(                                         &
         '  CLASS NAME                        : ',50(a14,1x),/)
322 format(                                         &
         '  DIAMETER                  (mm   ) : ',50(f14.4,1x),/)
323 format(                                         &
         '  PHI                       (-    ) : ',50(f14.2,1x),/)
324 format(                                         &
         '  DENSITY                   (kg/m3) : ',50(f14.2,1x),/)
326 format(                                         &
         '  SPHERICITY                (-    ) : ',50(f14.2,1x),/)
327 format(                                         &
         '  PERCENTAGE                (in % ) : ',50(f14.4,1x),/)
328 format(                                         &
         '  SUM                       (in % ) : ',1(f14.1,1x))
    !
    write(lulog,340)
340 format( &
         'OUTPUT STRATEGY')
    !
    write(lulog,341) &
         MY_OUT%parallel_IO, &
         MY_OUT%out_rst , &
         MY_OUT%rst/36d2,MY_OUT%rst, &
         MY_OUT%out_start /36d2,MY_OUT%out_start, &
         MY_OUT%dt/36d2,MY_OUT%dt, &
         MY_OUT%out_con_total, &
         MY_OUT%out_con_bins, &
         MY_OUT%out_con_bins, &
         MY_OUT%out_grn_total, &
         MY_OUT%out_grn_bins, &
         MY_OUT%MY_CUTS%ncutx, &
         MY_OUT%MY_CUTS%ncuty, &
         MY_OUT%MY_CUTS%ncutz, &
         MY_OUT%MY_CUTS%nfl, &
         MY_OUT%MY_PTS%npts
341 format(/,&
         '  Parallel I/O       : ',l1,/,&
         '  Write restart      : ',l1,/,&
         '  Restart time step  : ',f6.1,' h (',f12.1,' s)',/, &
         '  Output  time start : ',f6.1,' h (',f12.1,' s)',/, &
         '  Output  time step  : ',f6.1,' h (',f12.1,' s)',/, &
         '  out_con_total      : ',l1,/,&
         '  out_con_bins       : ',l1,/,&
         '  out_con_bins       : ',l1,/,&
         '  out_grn_total      : ',l1,/,&
         '  out_grn_bins       : ',l1,/,&
         '  out_x-cuts         : ',i3,/,&
         '  out_y-cuts         : ',i3,/,&
         '  out_z-cuts         : ',i3,/,&
         '  out_FLS            : ',i3,/,&
         '  Tracked points     : ',i3)
    !
    if(MY_OUT%track_points) then
       write(lulog,350) (MY_OUT%MY_PTS%name_pts(ipts), &
            MY_OUT%MY_PTS%xpts    (ipts), &
            MY_OUT%MY_PTS%ypts    (ipts), &
            MY_OUT%MY_PTS%zpts    (ipts),ipts=1,MY_OUT%MY_PTS%npts)
350    format(1000(4x,a20,1x,f13.4,1x,f13.4,1x,f13.4,/))
    end if
    !
    !*** Writes time integration header
    !
    write(lulog,500)
500 format(/, &
         '----------------------------------------------------',/,   &
         '                                                    ',/,   &
         '               TIME INTEGRATION                     ',/,   &
         '                                                    ',/,   &
         '----------------------------------------------------',/)
    !
    return
  end subroutine F3D_write_data
  !
  !-----------------------------------------
  !    subroutine F3D_set_source_term
  !-----------------------------------------
  !
  !>   @brief
  !>   Sets the source term in MY_TRA%my_s for the current interval
  !
  subroutine F3D_set_source_term(MY_FILES,MY_TIME,MY_MOD,MY_OUT,MY_GRID,MY_TRA,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TIME   run time related parameters
    !>   @param MY_MOD    model physics related parameters
    !>   @param MY_OUT    model output and postprocess related parameters
    !>   @param MY_GRID   grid configuration parameters
    !>   @param MY_TRA    variables related to tracers
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(RUN_TIME),      intent(INOUT) :: MY_TIME
    type(MODEL_PHYS),    intent(INOUT) :: MY_MOD
    type(MODEL_OUTPUT),  intent(IN   ) :: MY_OUT
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(TRACERS),       intent(INOUT) :: MY_TRA
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    character(len=24)     :: time1_str,time2_str
    integer(ip)           :: lulog,nbins
    integer(ip)           :: iyr,imo,idy,ihr,imi,ise
    integer(ip)           :: ix,iy,iz,is,i,j,k,ibin
    real(rp)              :: time,lonmin,lonmax,latmin,latmax,glonmin,glatmin
    real(rp)              :: dlon,dlat,inv_dlon,inv_dlat
    real(rp)              :: xs,ys,zs,s,t,st,myshape(4),shapez,Hm1,Hm2,Hm3
    real(rp), allocatable :: my_zc(:)        ! my_z at corners
    !
    type(SRC_PARAMS)  :: GL_SRC
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'F3D_set_source_term'
    MY_ERR%message = ' '
    !
    lulog = MY_FILES%lulog
    time  = MY_TIME%time
    nbins = MY_TRA%nbins
    !
    MY_TRA%my_s(my_ips:my_ipe,my_jps:my_jpe,my_kps:my_kpe,1:nbins) = 0.0_rp
    MY_TRA%gl_mass_rate = 0.0_rp
    !
    !*** Master gets and broadcasts the source at current time instant
    !
    if(master_model) call src_read_source(MY_FILES,MY_TIME,time,GL_SRC,MY_ERR)
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    !
    call src_bcast_source(GL_SRC,MY_ERR)
    if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    !
    !*** Updates MY_TIME and checks if a source interval has been found and is
    !*** consistent
    !
    MY_TIME%source_time = GL_SRC%end_time
    !
    if(GL_SRC%np.eq.0) then   ! no source active
       return
    end if
    !
    if(GL_SRC%nbins.ne.MY_TRA%nbins) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Inconsistent number of bins in src and grn files'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    MY_TRA%gl_mass_rate = SUM(GL_SRC%M)
    !
    !*** Writes to the log files
    !
    if((MY_OUT%log_level.ge.LOG_LEVEL_NORMAL).and.master_model) then
       call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
            iyr,imo,idy,ihr,imi,ise,1.0_rp*GL_SRC%start_time,MY_ERR)
       call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time1_str, MY_ERR)
       !
       call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
            iyr,imo,idy,ihr,imi,ise,1.0_rp*GL_SRC%end_time,MY_ERR)
       call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time2_str, MY_ERR)
       !
       write(lulog,10) time1_str,time2_str,(GL_SRC%end_time-GL_SRC%start_time), &
            GL_SRC%total_MFR,SUM(GL_SRC%M), &
            100.0*SUM(GL_SRC%M)/GL_SRC%total_MFR, &
            GL_SRC%np
10     format(/, &
            'SOURCE FILE READ              '      ,/, &
            '  From time                 : ',a    ,/, &
            '  To   time                 : ',a    ,/, &
            '  Time interval (in sec)    : ',i9   ,/, &
            '  Total     Mass flow rate  : ',e14.6,/, &
            '  Effective Mass flow rate  : ',e14.6,' (',f5.1,' %)'/, &
            '  Number of sources         : ',i5   ,/)
    end if
    !
    !*** Interpolates source points at my corners
    !
    allocate(my_zc(my_kbs:my_kbe))
    !
    my_zc(:) = 0.0_rp
    !
    lonmin = MY_GRID%lon_c(my_ibs)
    lonmax = MY_GRID%lon_c(my_ibe)
    latmin = MY_GRID%lat_c(my_jbs)
    latmax = MY_GRID%lat_c(my_jbe)
    glonmin = MY_GRID%lonmin
    if(glonmin.ge.180.0_rp) glonmin = glonmin - 360.0_rp
    glatmin = MY_GRID%latmin
    !
    inv_dlon = 1.0_rp / MY_GRID%dlon
    inv_dlat = 1.0_rp / MY_GRID%dlat
    !
    !*** Loop over source points
    !
    compute_source: do is = 1,GL_SRC%np
       !
       xs = GL_SRC%x(is)
       ys = GL_SRC%y(is)
       zs = GL_SRC%z(is)
       !
       ! Note that longitudes are in the range [-180,180) and so is xs
       if(xs.ge.180.0_rp) xs = xs - 360.0_rp
       !
       s = 0.0_rp
       t = 0.0_rp
       !
       ! Check latitudes
       if(ys.lt.latmin .or. ys.ge.latmax) cycle compute_source
       !
       ! Check longitudes (all in [-180,180))
       if(lonmin.lt.lonmax) then
           if(xs.lt.lonmin .or. xs.ge.lonmax) cycle compute_source
       else
           if(xs.lt.lonmin .and. xs.ge.lonmax) cycle compute_source
       end if
       !
       !*** I am a candidate point.
       !*** Compute interpolation factors and
       !*** values of z at point coordinates
       !
       ! Compute indexes
       dlat = ys - glatmin
       dlon = xs - glonmin
       if(dlon.lt.0) dlon = dlon + 360.0_rp  ! meridian crossing
       ix = 1 + int(dlon*inv_dlon)
       iy = 1 + int(dlat*inv_dlat)
       !
       dlat = ys - MY_GRID%lat_c(iy)
       dlon = xs - MY_GRID%lon_c(ix)
       if(dlon.lt.0) dlon = dlon + 360.0_rp
       s = 2.0_rp * dlon*inv_dlon - 1.0_rp
       t = 2.0_rp * dlat*inv_dlat - 1.0_rp
       !
       st=s*t
       myshape(1)=(1.0_rp-t-s+st)*0.25_rp                           !  4         3
       myshape(2)=(1.0_rp-t+s-st)*0.25_rp                           !
       myshape(3)=(1.0_rp+t+s+st)*0.25_rp                           !
       myshape(4)=(1.0_rp+t-s-st)*0.25_rp                           !  1         2
       !
       ! compute zc(kbs:kbe)
       !
       do iz = my_kbs,my_kbe
          my_zc(iz) = myshape(1)*MY_GRID%z_c(ix  ,iy  ,iz) + &
                      myshape(2)*MY_GRID%z_c(ix+1,iy  ,iz) + &
                      myshape(3)*MY_GRID%z_c(ix+1,iy+1,iz) + &
                      myshape(4)*MY_GRID%z_c(ix  ,iy+1,iz)
       end do
       !
       if( (zs.ge.my_zc(my_kbs)).and.(zs.lt.my_zc(my_kbe)) ) then
         !
         !*** Source at mass points
         !
         call grid_get_shapez(my_kbs,my_kbe,my_zc,zs,iz,shapez)
         MY_TRA%my_s(ix,iy,iz,1:nbins) = MY_TRA%my_s(ix,iy,iz,1:nbins) + GL_SRC%M(1:nbins,is)
       elseif(my_kbs.eq.1 .and. zs.lt.my_zc(my_kbs)) then
         ! source point is assigned to first layer if it is found below terrain
         MY_TRA%my_s(ix,iy,1,1:nbins) = MY_TRA%my_s(ix,iy,1,1:nbins) + GL_SRC%M(1:nbins,is)
         !
       end if
    end do compute_source
    !
    !*** Scales the source term at mass points
    !
    do ibin = 1,nbins
       do k = my_kps,my_kpe
          do j = my_jps,my_jpe
             Hm1 = MY_GRID%Hm1_p(j)
             Hm2 = MY_GRID%Hm2_p(j)
             do i = my_ips,my_ipe
                Hm3 = MY_GRID%Hm3_p(i,j)
                MY_TRA%my_s(i,j,k,ibin) = MY_TRA%my_s(i,j,k,ibin)*(Hm1*Hm2*Hm3)
             end do
          end do
       end do
    end do
    !
    !*** Store variables eventually needed by the GC model
    !
    if(MY_MOD%gravity_current) then
       MY_MOD%MY_GC%mass_flow_rate = GL_SRC%total_MFR     ! do not use GL_SRC%M because of effective bins
       MY_MOD%MY_GC%h_tot          = maxval(GL_SRC%z)     ! maximum column height (top of source)
    end if
    !
    return
  end subroutine F3D_set_source_term
  !
  !-----------------------------------------
  !    subroutine F3D_update_meteo_term
  !-----------------------------------------
  !
  !>   @brief
  !>   Updates meteorological variables and performs related operations
  !
  subroutine F3D_update_meteo_term(MY_FILES,MY_TIME,MY_OUT,MY_GRID,MY_MOD,MY_AGR,MY_MET,MY_TRA,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TIME   run time related parameters
    !>   @param MY_OUT        model output and postprocess related parameters
    !>   @param MY_GRID   grid configuration parameters
    !>   @param MY_MOD    model physics related parameters
    !>   @param MY_AGR    list of parameters defining an aggregation model
    !>   @param MY_MET    variables related to meteorology in MY_GRID
    !>   @param MY_TRA    TRACERS structure
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(RUN_TIME),      intent(INOUT) :: MY_TIME
    type(MODEL_OUTPUT),  intent(IN   ) :: MY_OUT
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(MODEL_PHYS),    intent(INOUT) :: MY_MOD
    type(AGR_PARAMS),    intent(IN   ) :: MY_AGR
    type(METEOROLOGY),   intent(INOUT) :: MY_MET
    type(TRACERS),       intent(INOUT) :: MY_TRA
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    character(len=24) :: time1_str,time2_str
    integer(ip), save :: ipass = 0
    integer(ip)       :: lulog,it,ibin,k,i,j
    integer(ip)       :: iyr,imo,idy,ihr,imi,ise
    real(rp)          :: time1,time2,time,s_time
    real(rp)          :: my_max,gl_max_u,gl_max_v,gl_max_w,gl_max_r,gl_max_t,gl_max_k1,gl_max_k3
    real(rp)          :: my_min,gl_min_u,gl_min_v,gl_min_w,gl_min_r,gl_min_t,gl_min_k1,gl_min_k3
    real(rp)          :: my_dt,gl_dt
    !
    real(rp), allocatable :: my_max_vs(:,:),gl_max(:)
    real(rp), allocatable :: my_min_vs(:,:),gl_min(:)
    !
    real(rp), allocatable :: my_varc (:,:,:)
    real(rp), allocatable :: my_varc2(:,:  )
    real(rp), allocatable :: my_ustc (:,:  )
    real(rp), allocatable :: my_uc   (:,:,:)
    real(rp), allocatable :: my_vc   (:,:,:)
    real(rp), allocatable :: my_wc   (:,:,:)
    real(rp), allocatable :: my_tvc  (:,:,:)
    real(rp), allocatable :: my_rhoc (:,:,:)
    real(rp), allocatable :: my_tc   (:,:,:)
    real(rp), allocatable :: my_u    (:,:,:)
    real(rp), allocatable :: my_v    (:,:,:)
    real(rp), allocatable :: my_w    (:,:,:)
    real(rp), allocatable :: my_met_v(:,:,:,:)
    real(rp), allocatable :: my_met_w(:,:,:,:)
    real(rp), allocatable :: my_met_k2(:,:,:)
    real(rp), allocatable :: my_met_k3(:,:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'F3D_update_meteo_term'
    MY_ERR%message = ' '
    !
    lulog = MY_FILES%lulog
    !
    !*** Allocates memory (first time only)
    !
    if(ipass.eq.0) then
       ipass = 1
       !
       allocate(MY_MET%my_u  (my_ibs_1h:my_ibe_1h,my_jps   :my_jpe   ,my_kps   :my_kpe   ,2))
       allocate(MY_MET%my_v  (my_jbs_1h:my_jbe_1h,my_ips   :my_ipe   ,my_kps   :my_kpe   ,2))
       allocate(MY_MET%my_w  (my_kbs_1h:my_kbe_1h,my_ips   :my_ipe   ,my_jps   :my_jpe   ,2))
       !
       allocate(MY_MET%my_rho(my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps   :my_kpe   ))
       allocate(MY_MET%my_t  (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps   :my_kpe   ))
       allocate(MY_MET%my_tv (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps   :my_kpe   ))
       allocate(MY_MET%my_k1 (my_ips_2h:my_ipe_2h,my_jps   :my_jpe   ,my_kps   :my_kpe   ))
       allocate(MY_MET%my_k2 (my_jps_2h:my_jpe_2h,my_ips   :my_ipe   ,my_kps   :my_kpe   ))
       allocate(MY_MET%my_k3 (my_kps_2h:my_kpe_2h,my_ips   :my_ipe   ,my_jps   :my_jpe   ))
       !
       allocate(MY_MET%my_pblh(my_ips:my_ipe,my_jps:my_jpe))
       allocate(MY_MET%my_mon (my_ips:my_ipe,my_jps:my_jpe))
       allocate(MY_MET%my_ust (my_ips:my_ipe,my_jps:my_jpe))
       !
       if(MY_MOD%wet_deposition) then
          allocate(MY_MET%my_pre (my_ips:my_ipe,my_jps:my_jpe))
       end if
       !
    end if
    !
    !*** Allocate temporary fields. At the end of this routine, they will be transposed and stored in my meteo
    !
    allocate(my_met_v (my_ips:my_ipe , my_jbs_1h:my_jbe_1h, my_kps   :my_kpe   ,2))
    allocate(my_met_w (my_ips:my_ipe , my_jps   :my_jpe   , my_kbs_1h:my_kbe_1h,2))
    allocate(my_met_k2(my_ips:my_ipe , my_jps_2h:my_jpe_2h, my_kps   :my_kpe     ))
    allocate(my_met_k3(my_ips:my_ipe , my_jps   :my_jpe   , my_kps_2h:my_kpe_2h  ))
    !
    !*** Determine the indexes and bounds of the time interpolation interval
    !
    if((MY_TIME%time.lt.MY_MET%timesec(1)        ).or. &
         (MY_TIME%time.gt.MY_MET%timesec(MY_MET%nt))) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Time not found in meteo data'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    else
       do it = 1,MY_MET%nt-1
          if((MY_TIME%time.ge.MY_MET%timesec(it  )).and. &
               (MY_TIME%time.lt.MY_MET%timesec(it+1))) then
             MY_MET%its         = it
             MY_MET%ite         = it + 1
             MY_TIME%meteo_time = min(MY_TIME%time+MY_MET%meteo_coupling_interval, MY_MET%timesec(it+1))
          end if
       end do
    end if
    !
    !*** Put ground velocity to zero
    !
    if(my_kbs.eq.1) then
       MY_MET%my_uc(:,:,my_kbs,MY_MET%its) = 0.0_rp
       MY_MET%my_uc(:,:,my_kbs,MY_MET%ite) = 0.0_rp
       MY_MET%my_vc(:,:,my_kbs,MY_MET%its) = 0.0_rp
       MY_MET%my_vc(:,:,my_kbs,MY_MET%ite) = 0.0_rp
       MY_MET%my_wc(:,:,my_kbs,MY_MET%its) = 0.0_rp
       MY_MET%my_wc(:,:,my_kbs,MY_MET%ite) = 0.0_rp
    end if
    !
    !*** Get staggered velocities at cell boundaries. Note that for velocity field both limits of the meteo model
    !*** time interval are stored in order to interpolate in time later during each model time integration step
    !
    call grid_get_stagered_velocity(&
         MY_MET%my_uc(:,:,:,MY_MET%its),MY_MET%my_vc(:,:,:,MY_MET%its),MY_MET%my_wc(:,:,:,MY_MET%its), &
         MY_MET%my_u (:,:,:,1),         my_met_v (:,:,:,1),         my_met_w (:,:,:,1),MY_ERR)
    !
    call grid_get_stagered_velocity(&
         MY_MET%my_uc(:,:,:,MY_MET%ite),MY_MET%my_vc(:,:,:,MY_MET%ite),MY_MET%my_wc(:,:,:,MY_MET%ite), &
         MY_MET%my_u (:,:,:,2),         my_met_v (:,:,:,2),         my_met_w (:,:,:,2),MY_ERR)
    !
    !*** Update other meteo variables at mass points. Note that variables are interpolated to the mid-point
    !*** of the time interval
    !
    allocate(my_varc (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe))
    allocate(my_varc2(my_ibs:my_ibe, my_jbs:my_jbe               ))
    !
    time   = MY_TIME%time + 0.5_rp*(MY_TIME%meteo_time - MY_TIME%time)                                    ! time instant
    s_time = (time - MY_MET%timesec(MY_MET%its))/(MY_MET%timesec(MY_MET%ite)-MY_MET%timesec(MY_MET%its))  ! time interpolation factor
    !
    ! air density
    !
    my_varc(:,:,:) = (1.0_rp-s_time) * MY_MET%my_rhoc(:,:,:,MY_MET%its) + &    ! at corners
         s_time  * MY_MET%my_rhoc(:,:,:,MY_MET%ite)
    call grid_c2p( MY_MET%my_rho, my_varc )
    !
    ! air temperature
    !
    my_varc(:,:,:) = (1.0_rp-s_time) * MY_MET%my_tc(:,:,:,MY_MET%its) + &      ! at corners
         s_time  * MY_MET%my_tc(:,:,:,MY_MET%ite)
    call grid_c2p( MY_MET%my_t, my_varc )
    !
    ! air virtual temperature
    !
    my_varc(:,:,:) = (1.0_rp-s_time) * MY_MET%my_tvc(:,:,:,MY_MET%its) + &     ! at corners
         s_time  * MY_MET%my_tvc(:,:,:,MY_MET%ite)
    call grid_c2p( MY_MET%my_tv, my_varc )
    !
    ! boundary layer height
    !
    my_varc2(:,:) = (1.0_rp-s_time) * MY_MET%my_pblhc(:,:,MY_MET%its) + &
         s_time  * MY_MET%my_pblhc(:,:,MY_MET%ite)
    call grid_c2p_2D( MY_MET%my_pblh, my_varc2 )
    !
    ! friction velocity
    !
    my_varc2(:,:) = (1.0_rp-s_time) * MY_MET%my_ustc(:,:,MY_MET%its) + &
         s_time  * MY_MET%my_ustc(:,:,MY_MET%ite)
    call grid_c2p_2D( MY_MET%my_ust, my_varc2 )
    !
    ! Monin
    !
    my_varc2(:,:) = (1.0_rp-s_time) * MY_MET%my_monc(:,:,MY_MET%its) + &
         s_time  * MY_MET%my_monc(:,:,MY_MET%ite)
    call grid_c2p_2D( MY_MET%my_mon, my_varc2 )
    where(abs(MY_MET%my_mon).lt.10) MY_MET%my_mon = sign(10.0_rp,MY_MET%my_mon)
    !
    ! precipitation rate
    !
    if(MY_MOD%wet_deposition) then
       my_varc2(:,:) = (1.0_rp-s_time) * MY_MET%my_prec(:,:,MY_MET%its) + &
            s_time  * MY_MET%my_prec(:,:,MY_MET%ite)
       call grid_c2p_2D( MY_MET%my_pre, my_varc2 )
    end if
    !
    deallocate(my_varc )
    deallocate(my_varc2)
    !
    !*** Calculate the diffusion coefficients using the velocity field at current time
    !
    allocate(my_uc (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe))
    allocate(my_vc (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe))
    allocate(my_wc (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe))
    allocate(my_tvc(my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe))
    !
    my_uc(:,:,:)  = (1.0_rp-s_time) * MY_MET%my_uc(:,:,:,MY_MET%its) + &
         s_time  * MY_MET%my_uc(:,:,:,MY_MET%ite)
    my_vc(:,:,:)  = (1.0_rp-s_time) * MY_MET%my_vc(:,:,:,MY_MET%its) + &
         s_time  * MY_MET%my_vc(:,:,:,MY_MET%ite)
    my_wc(:,:,:)  = (1.0_rp-s_time) * MY_MET%my_wc(:,:,:,MY_MET%its) + &
         s_time  * MY_MET%my_wc(:,:,:,MY_MET%ite)
    my_tvc(:,:,:) = (1.0_rp-s_time) * MY_MET%my_tvc(:,:,:,MY_MET%its) + &
         s_time  * MY_MET%my_tvc(:,:,:,MY_MET%ite)
    !
    call phys_get_kdiffu(MY_MOD, MY_GRID, my_uc, my_vc, my_tvc, MY_MET%my_pblh, MY_MET%my_mon, MY_MET%my_ust, &
         MY_MET%my_k1, my_met_k2, my_met_k3, MY_ERR)
    if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    !
    !*** Calculate settling velocities at w-boundaries using air properties at current time
    !
    allocate(my_rhoc(my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe))
    allocate(my_tc  (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe))
    !
    my_rhoc(:,:,:) = (1.0_rp-s_time) * MY_MET%my_rhoc(:,:,:,MY_MET%its) + &
         s_time  * MY_MET%my_rhoc(:,:,:,MY_MET%ite)
    my_tc(:,:,:)   = (1.0_rp-s_time) * MY_MET%my_tc  (:,:,:,MY_MET%its) + &
         s_time  * MY_MET%my_tc  (:,:,:,MY_MET%ite)
    !
    call phys_get_vset(MY_MOD, MY_AGR, my_rhoc, my_tc, MY_TRA%MY_BIN, MY_TRA%my_vs, MY_ERR)
    if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    !
    !*** Calculate dry deposition velocities at w-boundaries using air properties at current time
    !*** to account for effects different from sedimentation velocity. Deposition velocity
    !*** is added to settling velocity
    !
    if(MY_MOD%dry_deposition) then
       !
       allocate(my_ustc(my_ibs:my_ibe, my_jbs:my_jbe))
       !
       my_ustc(:,:) = (1.0_rp-s_time) * MY_MET%my_ustc(:,:,MY_MET%its) + &
            s_time  * MY_MET%my_ustc(:,:,MY_MET%ite)
       !
       call phys_dry_deposition(MY_MOD, my_rhoc, my_tc, MY_GRID%z_c, my_ustc, MY_MET%my_lusec, &
            MY_TRA%MY_BIN, MY_TRA%my_vs, MY_ERR)
       if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
       !
    end if
    !
    !*** Writes meteo information to the log file
    !
    if((MY_OUT%log_level.ge.LOG_LEVEL_NORMAL).and.master_model) then
       time1 = MY_TIME%time
       call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
            iyr,imo,idy,ihr,imi,ise,time1,MY_ERR)
       call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time1_str, MY_ERR)
       !
       time2 = MY_TIME%meteo_time
       call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
            iyr,imo,idy,ihr,imi,ise,time2,MY_ERR)
       call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time2_str, MY_ERR)
       !
       write(lulog,10) time1_str,time2_str,INT(time2-time1)
10     format(/, &
            'METEO DATA UPDATED            '      ,/, &
            '  From time                 : ',a    ,/, &
            '  To   time                 : ',a    ,/, &
            '  Time interval (in sec)    : ',i9)
    end if
    !
    if(MY_OUT%log_level.eq.LOG_LEVEL_FULL) then
       my_max = maxval(my_uc)
       call parallel_max(my_max,gl_max_u,COMM_MODEL)
       my_min = minval(my_uc)
       call parallel_min(my_min,gl_min_u,COMM_MODEL)
       my_max = maxval(my_vc)
       call parallel_max(my_max,gl_max_v,COMM_MODEL)
       my_min = minval(my_vc)
       call parallel_min(my_min,gl_min_v,COMM_MODEL)
       my_max = maxval(my_wc)
       call parallel_max(my_max,gl_max_w,COMM_MODEL)
       my_min = minval(my_wc)
       call parallel_min(my_min,gl_min_w,COMM_MODEL)
       my_max = maxval(my_tc)
       call parallel_max(my_max,gl_max_t,COMM_MODEL)
       my_min = minval(my_tc)
       call parallel_min(my_min,gl_min_t,COMM_MODEL)
       my_max = maxval(my_rhoc)
       call parallel_max(my_max,gl_max_r,COMM_MODEL)
       my_min = minval(my_rhoc)
       call parallel_min(my_min,gl_min_r,COMM_MODEL)
       my_max = maxval(MY_MET%my_k1)
       call parallel_max(my_max,gl_max_k1,COMM_MODEL)
       my_min = minval(MY_MET%my_k1)
       call parallel_min(my_min,gl_min_k1,COMM_MODEL)
       my_max = maxval(my_met_k3)
       call parallel_max(my_max,gl_max_k3,COMM_MODEL)
       my_min = minval(my_met_k3)
       call parallel_min(my_min,gl_min_k3,COMM_MODEL)
       !
       if(master_model) write(lulog,20) gl_max_u ,gl_min_u,  &
            gl_max_v ,gl_min_v,  &
            gl_max_w ,gl_min_w,  &
            gl_max_t ,gl_min_t,  &
            gl_max_r ,gl_min_r,  &
            gl_max_k1,gl_min_k1, &
            gl_max_k3,gl_min_k3
20     format(&
            '  X-velocity values           ',/,       &
            '    Maximum                 : ',f12.4,/, &
            '    Minimum                 : ',f12.4,/, &
            '  Y-velocity values           ',/,       &
            '    Maximum                 : ',f12.4,/, &
            '    Minimum                 : ',f12.4,/, &
            '  Z-velocity values           ',/,       &
            '    Maximum                 : ',f12.4,/, &
            '    Minimum                 : ',f12.4,/, &
            '  Temperature  C              ',/,       &
            '    Maximum                 : ',f12.4,/, &
            '    Minimum                 : ',f12.4,/, &
            '  Air Density                 ',/,       &
            '    Maximum                 : ',f12.4,/, &
            '    Minimum                 : ',f12.4,/, &
            '  Horizontal diffusion        ',/,       &
            '    Maximum                 : ',f12.4,/, &
            '    Minimum                 : ',f12.4,/, &
            '  Vertical   diffusion        ',/,       &
            '    Maximum                 : ',f12.4,/, &
            '    Minimum                 : ',f12.4)
       !
       allocate(my_max_vs(gl_nbz,MY_TRA%MY_BIN%nbins))
       allocate(my_min_vs(gl_nbz,MY_TRA%MY_BIN%nbins))
       allocate(gl_max   (       MY_TRA%MY_BIN%nbins))
       allocate(gl_min   (       MY_TRA%MY_BIN%nbins))
       my_max_vs(:,:) = -1e9_rp
       my_min_vs(:,:) =  1e9_rp
       !
       if(master_model) write(lulog,30) (ibin,ibin=1,MY_TRA%MY_BIN%nbins)
       do k = my_kbs,my_kbe
          do ibin = 1,MY_TRA%MY_BIN%nbins
             my_max_vs(k,ibin) = maxval(MY_TRA%my_vs(:,:,k,ibin))
             my_min_vs(k,ibin) = minval(MY_TRA%my_vs(:,:,k,ibin))
          end do
       end do

       do k = 1,gl_nbz
          do ibin = 1,MY_TRA%MY_BIN%nbins
             call parallel_max(my_max_vs(k,ibin),gl_max(ibin),COMM_MODEL)
             call parallel_min(my_min_vs(k,ibin),gl_min(ibin),COMM_MODEL)
          end do
          if(master_model) write(lulog,31) k,(gl_max(ibin),ibin = 1,MY_TRA%MY_BIN%nbins)
          if(master_model) write(lulog,32)   (gl_min(ibin),ibin = 1,MY_TRA%MY_BIN%nbins)
       end do
30     format(/, &
            'TERMINAL VELOCITIES (m/s)',/,  &
            '  Level/Bin   ',1x,50(8x,i2))
31     format(/,'   ',i3,'          max ',50(f9.4,1x))
32     format(  '   ',3x,'          min ',50(f9.4,1x))
    end if
    !
    !*** Computes scaled velocities at time interval boundaries
    !
    call grid_get_scaled_velocity(&
         MY_MET%my_uc(:,:,:,MY_MET%its), MY_MET%my_vc(:,:,:,MY_MET%its), &
         MY_MET%my_u (:,:,:,1),             my_met_v (:,:,:,1),             my_met_w (:,:,:,1), MY_GRID,MY_ERR)
    call grid_get_scaled_velocity(&
         MY_MET%my_uc(:,:,:,MY_MET%ite), MY_MET%my_vc(:,:,:,MY_MET%ite), &
         MY_MET%my_u (:,:,:,2),             my_met_v (:,:,:,2),             my_met_w (:,:,:,2), MY_GRID,MY_ERR)
    !
    !*** Computes other scaled variables at mass points at current time (mid point)
    !
    call grid_get_scaled_variables(&
         MY_MET%my_k1, my_met_k2, my_met_k3, MY_MET%my_rho, &
         MY_TRA%my_vs, MY_TRA%MY_BIN%nbins, MY_GRID,MY_ERR)
    !
    !*** Calculates the critical time step at mid-time interval using already the scaled quantities
    !
    allocate(my_u(my_ibs_1h:my_ibe_1h,my_jps   :my_jpe   ,my_kps   :my_kpe   ))
    allocate(my_v(my_ips   :my_ipe   ,my_jbs_1h:my_jbe_1h,my_kps   :my_kpe   ))
    allocate(my_w(my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kbs_1h:my_kbe_1h))
    !
    my_u(:,:,:) = (1.0_rp-s_time) * MY_MET%my_u(:,:,:,1) + s_time * MY_MET%my_u(:,:,:,2)
    my_v(:,:,:) = (1.0_rp-s_time) * my_met_v(:,:,:,1)    + s_time * my_met_v(:,:,:,2)
    !
    !if(MY_MOD%gravity_current) MY_MOD%CFL_safety_factor = 0.3_rp  ! conservative option to ensure CFL
    !
    MY_TIME%my_dt = MY_TIME%run_end
    MY_TIME%gl_dt = MY_TIME%run_end
    !
    do ibin = 1,MY_TRA%MY_BIN%nbins
       my_w(:,:,:) = (1.0_rp-s_time) * my_met_w(:,:,:,1) + s_time * my_met_w(:,:,:,2)
       my_w(:,:,:) = my_w(:,:,:) - MY_TRA%my_vs(:,:,:,ibin)
       !
       call grid_get_time_step(&
            my_dt,gl_dt,my_u,my_v,my_w,MY_MET%my_k1,my_met_k2,my_met_k3,MY_GRID,MY_MOD,MY_ERR)
       !
       MY_TIME%my_dt = min(my_dt,MY_TIME%my_dt)
       MY_TIME%gl_dt = min(gl_dt,MY_TIME%gl_dt)
       !
    end do
    !
    !*** Store velocity and diffusion components
    !
    do k=my_kps,my_kpe
       do i=my_ips,my_ipe
          MY_MET%my_v(my_jbs_1h:my_jbe_1h,i,k,1) = my_met_v(i,my_jbs_1h:my_jbe_1h,k,1)
          MY_MET%my_v(my_jbs_1h:my_jbe_1h,i,k,2) = my_met_v(i,my_jbs_1h:my_jbe_1h,k,2)
          MY_MET%my_k2(my_jps_2h:my_jpe_2h,i,k) = my_met_k2(i,my_jps_2h:my_jpe_2h,k)
       end do
    end do
    do j=my_jps,my_jpe
       do i=my_ips,my_ipe
          MY_MET%my_w(my_kbs_1h:my_kbe_1h,i,j,1) = my_met_w(i,j,my_kbs_1h:my_kbe_1h,1)
          MY_MET%my_w(my_kbs_1h:my_kbe_1h,i,j,2) = my_met_w(i,j,my_kbs_1h:my_kbe_1h,2)
          MY_MET%my_k3(my_kps_2h:my_kpe_2h,i,j) = my_met_k3(i,j,my_kps_2h:my_kpe_2h)
       end do
    end do

    deallocate( my_met_v)
    deallocate( my_met_w)
    deallocate( my_met_k2)
    deallocate( my_met_k3)

    return
  end subroutine F3D_update_meteo_term
  !
  !-----------------------------------------
  !    subroutine F3D_add_radial_wind
  !-----------------------------------------
  !
  !>   @brief
  !>   Updates meteorological variables and performs related operations
  !
  subroutine F3D_add_radial_wind(MY_TIME,MY_MOD,MY_GRID,my_u,my_v,MY_ERR)
    implicit none
    !
    !>   @param MY_TIME   run time related parameters
    !>   @param MY_MOD    model physics related parameters
    !>   @param MY_GRID   grid configuration parameter
    !>   @param my_u      x-component of scaled wind velocity at cell boundaries
    !>   @param my_v      y-component of scaled wind velocity at cell boundaries.
    !>                    Be awared this component is reshaped
    !>   @param MY_ERR    error handler
    !
    type(RUN_TIME),      intent(IN   ) :: MY_TIME
    type(MODEL_PHYS),    intent(INOUT) :: MY_MOD
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    real(rp),            intent(INOUT) :: my_u(my_ibs_1h:my_ibe_1h,my_jps   :my_jpe   ,my_kps   :my_kpe)
    real(rp),            intent(INOUT) :: my_v(my_jbs_1h:my_jbe_1h,my_ips   :my_ipe   ,my_kps   :my_kpe)
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    logical           :: foundx,foundy,foundz
    integer(ip)       :: ix,iy,iz,i,j,k
    !
    real(rp)    :: c_flow_rate,k_entrain,brunt_vaisala,lambda_grav
    real(rp)    :: wind_center_x, wind_center_y, Hm1
    real(rp)    :: c_gravr,min_radius,max_radius,radius_grav,c_gravu,c_grav_factor
    real(rp)    :: th_grav,h_tot,h_umbr,hmin,hmax,deltax,deltay,radius,rvel
    real(rp)    :: mass_flow_rate, time_since_eruption, vol_flow_rate
    real(rp)    :: lonmin,lonmax,latmin,latmax,my_wind,v_wind,Tp,Tb,zp
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'F3D_add_radial_wind'
    MY_ERR%message = ' '
    !
    c_flow_rate    = MY_MOD%MY_GC%c_flow_rate
    k_entrain      = MY_MOD%MY_GC%k_entrain
    brunt_vaisala  = MY_MOD%MY_GC%brunt_vaisala
    lambda_grav    = MY_MOD%MY_GC%lambda
    mass_flow_rate = MY_MOD%MY_GC%mass_flow_rate
    h_tot          = MY_MOD%MY_GC%h_tot
    !
    !*** Coordinates of the center of the radial wind
    !
    wind_center_x = MY_MOD%MY_GC%lon
    wind_center_y = MY_MOD%MY_GC%lat
    !
    !*** Check that the GC is active at the current time
    !
    if(MY_TIME%time.lt.MY_MOD%MY_GC%start_time) return
    if(MY_TIME%time.gt.MY_MOD%MY_GC%end_time  ) return
    !
    !*** Check if the source is on
    !
    if(mass_flow_rate.le.0.0_rp) return
    !
    !*** Get the he minimum radius of the gravity current. This value is set only for
    !*** numerical reasons; too small radius results may lead to strong asymmetry
    !*** of the deposit and/or large radial velocities (small dt)
    !
    deltax = REARTH*MY_GRID%Hm1_c(my_jbs)*(MY_GRID%lon_c(my_ibs)-MY_GRID%lon_c(my_ibs+1))*PI/180.0_rp
    deltay = REARTH*                      (MY_GRID%lat_c(my_jbs)-MY_GRID%lat_c(my_jbs+1))*PI/180.0_rp
    radius = 1.5_rp*sqrt(deltax*deltax + deltay*deltay)         ! a cetain cell distance, proportional to diagonal of the cell
    !
    min_radius = 0.0_rp
    call parallel_min(radius, min_radius, COMM_MODEL )
    !
    !*** Evaluate parameters for gravity current model
    !
    time_since_eruption = MY_TIME%time - MY_MOD%MY_GC%start_time         ! time since start of eruption
    !
    vol_flow_rate = c_flow_rate*sqrt(k_entrain)*mass_flow_rate**0.75_rp/(brunt_vaisala**1.25_rp)   ! Volumetric flow rate at NBL (q). Note corrigendum Costa et al. 2019
    !
    c_gravr = 3.0_rp*lambda_grav*brunt_vaisala*vol_flow_rate/(2.0_rp*PI)   ! constant
    !
    radius_grav = (c_gravr*time_since_eruption**2)**(0.33333_rp)           ! Radius(t) of the front
    radius_grav = max(radius_grav,min_radius)                              ! Avoid division by zero
    !
    c_gravu       = sqrt(2.0_rp*lambda_grav*brunt_vaisala*vol_flow_rate/(3.0_rp*PI))
    c_grav_factor = 0.75_rp*c_gravu*sqrt(radius_grav)
    th_grav       = c_gravu/(sqrt(radius_grav)*lambda_grav*brunt_vaisala)      ! Thickness
    !
    h_umbr = h_tot/1.2_rp                      ! Height of NBL (empirical). This should be consistent with C_UMBRELLA in plume model
    !
    hmin   = max(h_umbr-0.5_rp*th_grav,0.5_rp*h_tot) ! Higher that H_tot/2 (empirical)
    hmax   = min(h_umbr+0.5_rp*th_grav,h_tot)        ! Lower than total column height
    !
    !*** Find the wind velocity modulus at the center of the umbrella (h_umbr). This is used only to estimate timescales and
    !*** therefore the nearest mass point is considered (no interpolation)
    !
    lonmin  = MY_GRID%lon_c(my_ibs)
    lonmax  = MY_GRID%lon_c(my_ibe)
    latmin  = MY_GRID%lat_c(my_jbs)
    latmax  = MY_GRID%lat_c(my_jbe)
    my_wind = 0.0_rp
    !
    if((wind_center_x.gt.lonmin).and.(wind_center_x.le.lonmax).and. &
         (wind_center_y.gt.latmin).and.(wind_center_y.le.latmax)) then
       !
       ! I am a candidate host
       !
       foundx = .false.
       ix    = my_ibs
       do while(.not.foundx)
          if((wind_center_x.gt.MY_GRID%lon_c(ix)).and.(wind_center_x.le.MY_GRID%lon_c(ix+1))) then
             foundx = .true.
          else
             ix = ix + 1
             if(ix.eq.my_ibe) foundx =.true.
          end if
       end do
       !
       foundy = .false.
       iy    = my_jbs
       do while(.not.foundy)
          if((wind_center_y.gt.MY_GRID%lat_c(iy)).and.(wind_center_y.le.MY_GRID%lat_c(iy+1))) then
             foundy = .true.
          else
             iy = iy + 1
             if(iy.eq.my_jbe) foundy = .true.
          end if
       end do
       !
       ! Search in z
       !
       foundz = .false.
       do k   = my_kbs,my_kbe-1
          if((h_umbr.gt.MY_GRID%z_c(ix,iy,k)).and.(h_umbr.le.MY_GRID%z_c(ix,iy,k+1))) then
             foundz = .true.
             iz     = k
          end if
       end do
       !
       if(foundx.and.foundy.and.foundz) then
          Hm1     = MY_GRID%Hm1_c(iy)
          my_wind = sqrt(Hm1*my_u(ix,iy,iz)*Hm1*my_u(ix,iy,iz)+my_v(iy,ix,iz)*my_v(iy,ix,iz))
       end if
       !
    end if
    !
    v_wind = 0.0_rp
    call parallel_max(my_wind, v_wind, COMM_MODEL )
    !
    !*** Radius_grav(Tp) where Tp=64/27*c_gravr/v^3. Transition at Ri=025 for passive transport transition
    !
    Tp = 64.0_rp/27.0_rp*c_gravr/max(v_wind**3,0.1_rp)
    Tb = Tp/8.0_rp
    max_radius = 1.7777_rp*c_gravr/max(v_wind**2,0.1_rp)
    !
    !*** Add radial wind for my_u
    !
    do j = my_jps,my_jpe
       do i = my_ibs,my_ibe
          !
          deltax = REARTH*MY_GRID%Hm1_p(j)*(MY_GRID%lon_c(i)-wind_center_x)*PI/180.0_rp
          deltay = REARTH*                 (MY_GRID%lat_p(j)-wind_center_y)*PI/180.0_rp
          radius = sqrt(deltax*deltax + deltay*deltay)                        ! approx distance (in m)
          !
          if(abs(radius) <= min_radius ) cycle ! Skip for R <= Rmin
          if(abs(radius) >  max_radius ) cycle ! Skip for R >  Rmin
          !
          rvel = c_grav_factor/radius*(1.0_rp+radius**2 /(3.0_rp*radius_grav**2))
          !
          do k = my_kps,my_kpe
             zp = 0.25_rp*(MY_GRID%z_c(i,j,k)+MY_GRID%z_c(i,j+1,k)+MY_GRID%z_c(i,j,k+1)+MY_GRID%z_c(i,j+1,k+1))
             if((zp.ge.hmin).and.(zp.le.hmax)) then
                my_u(i,j,k) = my_u(i,j,k) + rvel * deltax/radius/MY_GRID%Hm1_p(j)
             end if
          end do
       end do
    end do
    !
    !*** Add radial wind for my_v
    !
    do j = my_jbs,my_jbe
       do i = my_ips,my_ipe
          !
          deltax = REARTH*MY_GRID%Hm1_c(j)*(MY_GRID%lon_p(i)-wind_center_x)*PI/180.0_rp
          deltay = REARTH*                 (MY_GRID%lat_c(j)-wind_center_y)*PI/180.0_rp
          radius = sqrt(deltax*deltax + deltay*deltay)                        ! approx distance (in m)
          !
          if(abs(radius) <= min_radius ) cycle ! Skip for R <= Rmin
          if(abs(radius) >  max_radius ) cycle ! Skip for R >  Rmin
          !
          rvel = c_grav_factor/radius*(1.0_rp+radius**2 /(3.0_rp*radius_grav**2))
          !
          do k = my_kps,my_kpe
             zp = 0.25_rp*(MY_GRID%z_c(i,j,k)+MY_GRID%z_c(i+1,j,k)+MY_GRID%z_c(i,j,k+1)+MY_GRID%z_c(i+1,j,k+1))
             if((zp.ge.hmin).and.(zp.le.hmax)) then
                my_v(j,i,k) = my_v(j,i,k) + rvel * deltay/radius
             end if
          end do
       end do
    end do
    !
    !*** Exchange halos
    !
    call domain_swap_velo_points_1halo_x ( my_u )
    call domain_swap_velo_points_1halo_reshaped_y ( my_v )
    !
    !*** Finally, store some (global) values for eventually writting res file
    !
    MY_MOD%MY_GC%vol_flow_rate = vol_flow_rate
    MY_MOD%MY_GC%max_radius    = max_radius
    MY_MOD%MY_GC%radius_grav   = radius_grav
    MY_MOD%MY_GC%th_grav       = th_grav
    !
    return
  end subroutine F3D_add_radial_wind
  !
  !-----------------------------------------
  !    subroutine F3D_end_time_step
  !-----------------------------------------
  !
  !>   @brief
  !>   Ends a time integration step
  !
  subroutine F3D_end_time_step(MY_FILES,MY_TIME,MY_MOD,MY_OUT,MY_GRID,MY_SPE,MY_TRA,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TIME   run time related parameters
    !>   @param MY_MOD    model physics related parameterss
    !>   @param MY_OUT    model output and postprocess related parameters
    !>   @param MY_GRID   grid configuration parameters
    !>   @param MY_SPE    list of parameters defining species and categories
    !>   @param MY_TRA    variables related to tracers
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(INOUT) :: MY_FILES
    type(RUN_TIME),       intent(IN   ) :: MY_TIME
    type(MODEL_PHYS),     intent(IN   ) :: MY_MOD
    type(MODEL_OUTPUT),   intent(INOUT) :: MY_OUT
    type(ARAKAWA_C_GRID), intent(IN   ) :: MY_GRID
    type(SPECIES_PARAMS), intent(IN   ) :: MY_SPE
    type(TRACERS),        intent(INOUT) :: MY_TRA
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    character(len=24) :: time_str
    integer(ip), save :: istep = 0
    integer(ip), save :: irst  = 1
    integer(ip), save :: igc   = 0
    integer(ip)       :: lulog, lugc
    integer(ip)       :: iyr,imo,idy,ihr,imi,ise
    real(rp)          :: time,gl_mass,gl_mass_volume,gl_mass_ground,gl_mass_lateral,gl_mass_sink
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'F3D_end_time_step'
    MY_ERR%message = ' '
    !
    lulog = MY_FILES%lulog
    lugc  = MY_FILES%lugc_res
    !
    !*** Writes log file every 10 minutes
    !
    if( (MY_TIME%iiter.eq.1).or.(mod(MY_TIME%time,600.0_rp).le.MY_TIME%gl_dt) ) then
       !
       if((MY_OUT%log_level.ge.LOG_LEVEL_NORMAL).and.master_model) then
          !
          time = MY_TIME%time
          call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
               iyr,imo,idy,ihr,imi,ise,time,MY_ERR)
          call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str, MY_ERR)
          !
          write(lulog,10) MY_TIME%iiter,MY_TIME%gl_dt, &
               int((MY_TIME%time-MY_TIME%run_start)/60.0_rp),  &
               100.0_rp*((MY_TIME%time-MY_TIME%run_start)/(MY_TIME%run_end-MY_TIME%run_start)), &
               time_str
10        format(/, &
               '-> Iteration                 : ',i8                   ,/,  &
               '   Time step                 : ',e13.6                ,/,  &
               '   Elapsed time              : ',i7,' min   (',f7.2,'%)',/,  &
               '   Current time              : ',a)
          !
       end if
       !
       !*** Perform mass balance
       !
       if(MY_OUT%log_level.ge.LOG_LEVEL_FULL) then
          !
          call grid_get_mass_volume    (gl_mass_volume,                MY_TRA,MY_GRID,MY_ERR)
          call grid_get_mass_sink      (gl_mass_sink,                  MY_TRA,MY_GRID,MY_ERR)
          call grid_get_mass_boundaries(gl_mass_ground,gl_mass_lateral,MY_TRA,MY_ERR)
          gl_mass_sink    = gl_mass_sink    + MY_TRA%rst_mass_sink
          gl_mass_ground  = gl_mass_ground  + MY_TRA%rst_mass_ground
          gl_mass_lateral = gl_mass_lateral + MY_TRA%rst_mass_lateral
          !
          gl_mass = gl_mass_ground + gl_mass_lateral + gl_mass_sink + gl_mass_volume
          !
          if(master_model) write(lulog,20) gl_mass_volume ,100.0_rp*gl_mass_volume /gl_mass, &
                                           gl_mass_lateral,100.0_rp*gl_mass_lateral/gl_mass, &
                                           gl_mass_sink,   100.0_rp*gl_mass_sink   /gl_mass, &
                                           gl_mass_ground, 100.0_rp*gl_mass_ground /gl_mass, &
                                           gl_mass,        MY_TRA%gl_mass_in
20        format(&
               '   -------                     '                      ,/,  &
               '   (1) Mass inside  the domain     : ',e13.6,' (',f7.2,'%)' ,/,  &
               '   (2) Mass outside the domain     : ',e13.6,' (',f7.2,'%)' ,/,  &
               '   (3) Mass sink terms (wet dep.)  : ',e13.6,' (',f7.2,'%)' ,/,  &
               '   (4) Mass at ground              : ',e13.6,' (',f7.2,'%)' ,/, &
               '       ----------------------------  ' ,/,  &
               '   Summ (1)+(2)+(3)+(4)            : ',e13.6,/,  &
               '   Injected mass                   : ',e13.6)
          !
       end if
       !
    end if
    !
    !*** Writes output files (including tracking points)
    !
    if( (MY_OUT%out_start.eq.0.0_rp).or.(MY_OUT%out_start.lt.MY_TIME%run_start) ) MY_OUT%out_start = MY_TIME%run_start
    if( ((MY_OUT%out_start + istep*MY_OUT%dt).le.MY_TIME%time ).or.(.not.MY_TIME%go_on) )  then
       !
       istep = istep + 1
       if(istep.eq.1) then
          call nc_IO_out_grid(MY_FILES,MY_GRID,MY_OUT%MY_CUTS,MY_SPE,MY_TRA,MY_TIME,MY_OUT,MY_ERR)
       end if
       call nc_IO_out_res (MY_FILES,MY_GRID,MY_OUT%MY_CUTS,MY_SPE,MY_TRA,MY_TIME,MY_OUT,MY_ERR)
       !
    end if
    !
    !*** If necessary, write GC results file
    !
    if(MY_MOD%gravity_current) then
       if((MY_TIME%time.ge.MY_MOD%MY_GC%start_time).and.(MY_TIME%time.le.MY_MOD%MY_GC%end_time).and. &
            (mod(MY_TIME%time,600.0_rp).le.MY_TIME%gl_dt) ) then
          !
          if(master_model.and.igc.eq.0) then
             igc = 1
             open(lugc,FILE=TRIM(MY_FILES%file_gc),status='unknown')
             write(Lugc,200)
200          format(&
                  '----------------------------------------------------------------------------------------',/, &
                  '         time                  MFR         VFR       GC radius   GC thick   Passive rad ',/, &
                  '                              (kg/s)      (m3/s)       (km)         (km)        (km)    ',/, &
                  '----------------------------------------------------------------------------------------')
          end if
          if(master_model) then
             !
             time = MY_TIME%time
             call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
                  iyr,imo,idy,ihr,imi,ise,time,MY_ERR)
             call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str, MY_ERR)
             !
             write(lugc,201) time_str, MY_MOD%MY_GC%mass_flow_rate, MY_MOD%MY_GC%vol_flow_rate, &
                  min(MY_MOD%MY_GC%radius_grav/1e3_rp, MY_MOD%MY_GC%max_radius/1e3_rp), &
                  MY_MOD%MY_GC%th_grav/1e3_rp,MY_MOD%MY_GC%max_radius/1e3_rp
201          format(a26,2(1x,e12.5),3(2x,f8.1))
             !
          end if
       end if
    end if
    !
    !*** If necessary, writes restart file
    !
    if( (MY_OUT%out_rst).and.(MY_OUT%rst.gt.0.0_rp) ) then
       if( (MY_TIME%run_start + irst*MY_OUT%rst).le.MY_TIME%time )  then
          !
          irst = irst + 1
          call nc_IO_out_rst (MY_FILES,MY_GRID,MY_TRA,MY_OUT,MY_TIME,MY_ERR)
          !
       end if
    end if
    !
    return
  end subroutine F3D_end_time_step
  !
  !-----------------------------------------
  !    subroutine F3D_time_step
  !-----------------------------------------
  !
  !>   @brief
  !>   Performs a time integration step
  !
  subroutine F3D_time_step(MY_TIME,MY_GRID,MY_MOD,MY_MET,MY_SPE,MY_TRA,MY_ERR)
    implicit none
    !
    !>   @param MY_TIME   run time related parameters
    !>   @param MY_GRID   grid configuration parameters
    !>   @param MY_MOD    model physics related parameters
    !>   @param MY_MET    variables related to meteorology in MY_GRID
    !>   @param MY_TRA    variables related to tracers
    !>   @param MY_ERR    error handler
    !
    type(RUN_TIME),      intent(IN   ) :: MY_TIME
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(MODEL_PHYS),    intent(INOUT) :: MY_MOD
    type(METEOROLOGY),   intent(INOUT) :: MY_MET
    type(SPECIES_PARAMS),intent(IN   ) :: MY_SPE
    type(TRACERS),       intent(INOUT) :: MY_TRA
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: nbins
    integer(ip) :: i,j,k,ibin,bin_cat
    real(rp)    :: s_time
    real(rp)    :: dX,dY,dZ,vol,Hm1,Hm2,Hm3
    real(rp),pointer :: my_u(:,:,:)
    real(rp),pointer :: my_v(:,:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'F3D_time_step'
    MY_ERR%message = ' '
    !
    nbins     = MY_TRA%nbins
    !
    !*** Source term. Note that using the scaled concentration S* / dXdYdZ = S / dlon*dlat*dz
    !
    do k = my_kps,my_kpe
       dZ = MY_GRID%dX3_p(k)
       do j = my_jps,my_jpe
          dY  = MY_GRID%dX2_p(j)
          Hm1 = MY_GRID%Hm1_p(j)
          Hm2 = MY_GRID%Hm2_p(j)
          do i = my_ips,my_ipe
             dX  = MY_GRID%dX1_p(i)
             Hm3 = MY_GRID%Hm3_p(i,j)
             !
             vol = dX*dY*dZ*(Hm1*Hm2*Hm3)

             do ibin = 1,nbins
                MY_TRA%my_c(i,j,k,ibin) = MY_TRA%my_c(i,j,k,ibin) + MY_TIME%gl_dt*MY_TRA%my_s(i,j,k,ibin)/vol
             end do
          end do
       end do
    end do
    !
    s_time = (MY_TIME%time - MY_MET%timesec(MY_MET%its))/(MY_MET%timesec(MY_MET%ite)-MY_MET%timesec(MY_MET%its))  ! time interpolation factor
    !
    !*** Before proceeding with advection and diffusion, we need to update
    !    velocity with gravity current if required. For that, velocity components in current
    !    iteration are updated and then we add gravity current
    !
    call ADS_get_horizontal_vcomps( my_u, my_v )

    my_u(:,:,:) = (1.0_rp-s_time) * MY_MET%my_u(:,:,:,1) + s_time * MY_MET%my_u(:,:,:,2)
    my_v(:,:,:) = (1.0_rp-s_time) * MY_MET%my_v(:,:,:,1) + s_time * MY_MET%my_v(:,:,:,2)
    !
    !*** For gravity currents, modify the wind field adding a null-divergence wind field
    !
    if(MY_MOD%gravity_current) then
       call F3D_add_radial_wind(MY_TIME,MY_MOD,MY_GRID,my_u,my_v,MY_ERR)
    end if
    !
    !*** Advection and diffusion
    !
    if(mod(MY_TIME%iiter,2) == 0) then
       call ADS_solve_along_x(MY_TRA%my_W_flux,MY_TRA%my_E_flux,MY_TIME%gl_dt,MY_TRA%my_c(:,:,:,:), &
            MY_MET%my_k1,MY_GRID,MY_TRA%nbins)
       call ADS_solve_along_y(MY_TRA%my_S_flux,MY_TRA%my_N_flux,MY_TIME%gl_dt,MY_TRA%my_c(:,:,:,:), &
            MY_MET%my_k2,MY_GRID,MY_TRA%nbins)
    else
       call ADS_solve_along_y(MY_TRA%my_S_flux,MY_TRA%my_N_flux,MY_TIME%gl_dt,MY_TRA%my_c(:,:,:,:), &
            MY_MET%my_k2,MY_GRID,MY_TRA%nbins)
       call ADS_solve_along_x(MY_TRA%my_W_flux,MY_TRA%my_E_flux,MY_TIME%gl_dt,MY_TRA%my_c(:,:,:,:), &
            MY_MET%my_k1,MY_GRID,MY_TRA%nbins)
    end if
    !
    call ADS_solve_along_z(MY_TRA%my_D_flux,MY_TRA%my_U_flux,MY_TIME%gl_dt,MY_TRA%my_c(:,:,:,:), &
         MY_TRA%my_acum(:,:,:), MY_MET%my_k3,MY_MET%my_w(:,:,:,1),MY_MET%my_w(:,:,:,2), &
         s_time, MY_TRA%my_vs(:,:,:,:), MY_GRID,MY_TRA%nbins)
    !
    !*** Wet deposition. It is computed for particles and radionuclides only, not for gas species. Note
    !*** also that a critical cut-off size is assumed (i.e. mechanism operates only for
    !*** particle sizes below a threshold)
    !
    if(MY_MOD%wet_deposition) then
       do ibin = 1,nbins
          bin_cat = MY_TRA%MY_BIN%bin_cat(ibin)
          select case(bin_cat)
          case(CAT_PARTICLE,CAT_RADIONUCLIDE)
             !
             if(MY_TRA%MY_BIN%bin_diam(ibin).le.1d-4) then   !  100 micron cut-off
                call phys_wet_deposition(MY_MOD,MY_TIME%gl_dt,MY_MET%my_pre,MY_MET%my_pblh, MY_GRID%h_c, &
                                         MY_GRID%z_c,MY_GRID%dX3_p,MY_TRA%my_awet(:,:,ibin),&
                                         MY_TRA%my_c(:,:,:,ibin),MY_ERR)
             end if
             !
          case(CAT_AEROSOL)
             !
             continue
          end select
       end do
    end if
    !
    !*** Radionuclides decay
    !
    if(MY_SPE%exists_radionuclide) then
       call phys_radionuclides(MY_TRA,MY_SPE,MY_TIME,MY_ERR)
    end if
    !
    return
  end subroutine F3D_time_step
  !
  !-----------------------------------------
  !    subroutine F3D_set_pts
  !-----------------------------------------
  !
  !>   @brief
  !>   Gets the mesh indexes and interpolation factors for the tracking points
  !
  subroutine F3D_set_pts(MY_OUT,MY_GRID,MY_ERR)
    implicit none
    !
    !>   @param MY_GRID   grid configuration parameters
    !>   @param MY_OUT    model output and postprocess related parameters
    !>   @param MY_ERR    error handler
    !
    type(MODEL_OUTPUT),  intent(INOUT) :: MY_OUT
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: npts,ipts,gl_imax,ix,iy,iz
    real(rp)    :: xp,yp,zp,s,t,st,myshape(4),shapez
    real(rp)    :: my_lonmin,my_lonmax,my_latmin,my_latmax,glonmin,glatmin
    real(rp)    :: dlon,dlat,inv_dlon,inv_dlat
    !
    real(rp), allocatable :: my_zc(:)        ! my_z at corners
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'F3D_set_pts'
    MY_ERR%message = ' '
    !
    !*** Allocate memeory
    !
    npts = MY_OUT%MY_PTS%npts
    allocate(MY_OUT%MY_PTS%mproc(npts))
    allocate(MY_OUT%MY_PTS%ipts (npts))
    allocate(MY_OUT%MY_PTS%jpts (npts))
    allocate(MY_OUT%MY_PTS%kpts (npts))
    allocate(MY_OUT%MY_PTS%spts (npts))
    allocate(MY_OUT%MY_PTS%tpts (npts))
    allocate(MY_OUT%MY_PTS%wpts (npts))
    !
    allocate(my_zc(my_kbs:my_kbe))
    !
    !*** Initializations
    !
    MY_OUT%MY_PTS%spts(:) = 0.0_rp
    MY_OUT%MY_PTS%tpts(:) = 0.0_rp
    MY_OUT%MY_PTS%wpts(:) = 0.0_rp
    !
    my_lonmin = MY_GRID%lon_c(my_ibs)
    my_lonmax = MY_GRID%lon_c(my_ibe)
    my_latmin = MY_GRID%lat_c(my_jbs)
    my_latmax = MY_GRID%lat_c(my_jbe)
    glonmin = MY_GRID%lonmin
    if(glonmin.ge.180.0_rp) glonmin = glonmin - 360.0_rp
    glatmin = MY_GRID%latmin
    !
    inv_dlon = 1.0_rp / MY_GRID%dlon
    inv_dlat = 1.0_rp / MY_GRID%dlat
    !
    MY_OUT%MY_PTS%mproc(:) = -1
    !
    !*** Loop over tracking points
    !
    compute_points: do ipts = 1,npts
       !
       xp = MY_OUT%MY_PTS%xpts(ipts)
       yp = MY_OUT%MY_PTS%ypts(ipts)
       zp = MY_OUT%MY_PTS%zpts(ipts)
       !
       ! Note that longitudes are in the range [-180,180) and so is xp
       if(xp.ge.180.0_rp) xp = xp - 360.0_rp
       !
       s = 0.0_rp
       t = 0.0_rp
       !
       ! Check latitudes
       if(yp.lt.my_latmin .or. yp.ge.my_latmax) cycle compute_points
       !
       ! Check longitudes (all in [-180,180))
       if(my_lonmin.lt.my_lonmax) then
           if(xp.lt.my_lonmin .or. xp.ge.my_lonmax) cycle compute_points
       else
           if(xp.lt.my_lonmin .and. xp.ge.my_lonmax) cycle compute_points
       end if
       !
       !  compute indexes and interpolation factors
       !
       dlat = yp - glatmin
       dlon = xp - glonmin
       if(dlon.lt.0) dlon = dlon + 360.0_rp  ! meridian crossing
       ix = 1 + int(dlon*inv_dlon)      ! refer to cell boundaries
       iy = 1 + int(dlat*inv_dlat)
       !
       dlat = yp - MY_GRID%lat_c(iy)
       dlon = xp - MY_GRID%lon_c(ix)
       if(dlon.lt.0) dlon = dlon + 360.0_rp
       s = 2.0_rp * dlon*inv_dlon - 1.0_rp
       t = 2.0_rp * dlat*inv_dlat - 1.0_rp
       !
       !  Interpolation factor along x
       !
       MY_OUT%MY_PTS%ipts(ipts) = ix
       MY_OUT%MY_PTS%spts(ipts) = s
       !
       !  Interpolation factor along y
       !
       MY_OUT%MY_PTS%jpts(ipts) = iy
       MY_OUT%MY_PTS%tpts(ipts) = t
       !
       !  interpolation factor along z
       !
       st=s*t
       myshape(1)=(1.0_rp-t-s+st)*0.25_rp                           !  4         3
       myshape(2)=(1.0_rp-t+s-st)*0.25_rp                           !
       myshape(3)=(1.0_rp+t+s+st)*0.25_rp                           !
       myshape(4)=(1.0_rp+t-s-st)*0.25_rp                           !  1         2
       !
       ! compute zc(kbs:kbe)
       !
       do iz = my_kbs,my_kbe
          my_zc(iz) = myshape(1)*MY_GRID%z_c(ix  ,iy  ,iz) + &
                      myshape(2)*MY_GRID%z_c(ix+1,iy  ,iz) + &
                      myshape(3)*MY_GRID%z_c(ix+1,iy+1,iz) + &
                      myshape(4)*MY_GRID%z_c(ix  ,iy+1,iz)
       end do
       !
       if(zp.lt.my_zc(my_kbs)) then
         !
         if(my_kbs.eq.1) then
            MY_OUT%MY_PTS%kpts (ipts) = my_kbs
            MY_OUT%MY_PTS%wpts (ipts) = 0.0_rp  ! (0,1) in z
            MY_OUT%MY_PTS%mproc(ipts) = mype_model
         end if
         !
       elseif(zp.lt.my_zc(my_kbe)) then
         !
         !  3D points
         !
         call grid_get_shapez(my_kbs,my_kbe,my_zc,zp,iz,shapez)
         MY_OUT%MY_PTS%kpts (ipts) = iz
         MY_OUT%MY_PTS%wpts (ipts) = shapez
         MY_OUT%MY_PTS%mproc(ipts) = mype_model
       else
         !
         if(my_kbe.eq.gl_nbz) then
            MY_OUT%MY_PTS%kpts (ipts) = my_kbe-1
            MY_OUT%MY_PTS%wpts (ipts) = 1.0_rp  ! (0,1) in z
            MY_OUT%MY_PTS%mproc(ipts) = mype_model
         end if
         !
       end if
       !
    end do compute_points
    !
    do ipts = 1,npts
       !
       !  broadcast the processor hosting the point
       !
       call parallel_max(MY_OUT%MY_PTS%mproc(ipts),gl_imax,COMM_MODEL)
       MY_OUT%MY_PTS%mproc(ipts) = gl_imax
    end do
    !
    return
  end subroutine F3D_set_pts
  !
  !>   @brief
  !>   Writes the tracking point files
  !
  subroutine F3D_out_pts_grn(MY_FILES,MY_SPE,MY_OUT,MY_TRA,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_SPE    list of parameters defining species and categories
    !>   @param MY_OUT    model output and postprocess related parameters
    !>   @param MY_TRA    TRACERS structure already filled
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(SPECIES_PARAMS),intent(IN   ) :: MY_SPE
    type(MODEL_OUTPUT),  intent(IN   ) :: MY_OUT
    type(TRACERS),       intent(INOUT) :: MY_TRA
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: fname
    character(len=s_name) :: spe_name
    integer(ip)           :: npts,ipts,nbins,ibin,i,j
    integer(ip)           :: ispe, spe_code, jb
    real(rp)              :: s,t,st,val(4),total,diam,fraction
    !
    real(rp), allocatable :: my_ac   (:,:,:)   ! accumulation   at corner points
    real(rp), allocatable :: my_awetc(:,:,:)   ! wet deposition at corner points
    real(rp), allocatable :: my_ap   (:)       ! accumulation   at point
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'F3D_out_pts_grn'
    MY_ERR%message = ' '
    !
    npts  = MY_OUT%MY_PTS%npts
    nbins = MY_TRA%MY_BIN%nbins
    !
    !*** Allocates
    !
    allocate(my_ac   (my_ibs:my_ibe,my_jbs:my_jbe,1:nbins))
    allocate(my_awetc(my_ibs:my_ibe,my_jbs:my_jbe,1:nbins))
    allocate(my_ap   (1:nbins))
    !
    !*** Get ground accumulation at corner points (my_acum in kg/m2)
    !
    do ibin = 1,nbins
       call Grid_p2c_2D(MY_TRA%my_acum(:,:,ibin),my_ac(:,:,ibin))
    end do
    !
    !*** Get wet deposition at corner points (my_awetc in kg/m2)
    !
    do ibin = 1,nbins    
       call domain_swap_mass_points_2halo_2Dx ( MY_TRA%my_awet(:,:,ibin) )
       call domain_swap_mass_points_2halo_2Dy ( MY_TRA%my_awet(:,:,ibin) )
       !
       call Grid_p2c_2D(MY_TRA%my_awet(:,:,ibin),my_awetc(:,:,ibin))
    end do
    !
    !*** Add the contribution of wet deposition to ground accumulation, which includes
    !*** both dry and wet (not that wet part is 0 if MY_MOD%wet_deposition = .false.)
    !
    my_ac = my_ac + my_awetc
    !
    !*** Loop over species and points
    !
    !
    do ispe = 1,MY_SPE%nspe
       !
       spe_code = MY_SPE%code(ispe)
       spe_name = MY_SPE%name(ispe)
       !
       do ipts = 1,npts
       !
          fname = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.'// &
                  TRIM(MY_OUT%MY_PTS%name_pts(ipts))//'.'//TRIM(spe_name)//'.grnd.res'
       !
       !*** The processor hosting the 2D point interpolates. Note that 3D points are neglected
       !
       if(MY_OUT%MY_PTS%zpts(ipts).lt.0.0_rp) then
          !
          if(mype_model.eq.MY_OUT%MY_PTS%mproc(ipts)) then
             i = MY_OUT%MY_PTS%ipts(ipts)
             j = MY_OUT%MY_PTS%jpts(ipts)
             s = MY_OUT%MY_PTS%spts(ipts)
             t = MY_OUT%MY_PTS%tpts(ipts)
             st     = s*t
             val(1) = (1.0_rp-t-s+st)*0.25_rp                   !  4      3
             val(2) = (1.0_rp-t+s-st)*0.25_rp                   !
             val(3) = (1.0_rp+t+s+st)*0.25_rp                   !
             val(4) = (1.0_rp+t-s-st)*0.25_rp                   !  1      2
             !
             jb = 0
             my_ap(:) = 0.0_rp
             do ibin = 1,nbins
                if(MY_TRA%MY_BIN%bin_spe(ibin).eq.spe_code) then
                   jb = jb + 1
                   my_ap(jb) =  val(1)*my_ac(i  ,j  ,ibin) + &
                                val(2)*my_ac(i+1,j  ,ibin) + &
                                val(3)*my_ac(i+1,j+1,ibin) + &
                                val(4)*my_ac(i  ,j+1,ibin)
                end if
             end do
             !
          else
             my_ap(:) = 0.0_rp
             !
          end if
          !
          call parallel_sum(my_ap, COMM_MODEL)
          !
          !*** Master prints the file
          !
          if(master_model) then
             open(90,file=TRIM(fname),status='unknown')
             write(90,10) TRIM(MY_OUT%MY_PTS%name_pts(ipts)), &
                  MY_OUT%MY_PTS%xpts    (ipts),  &
                  MY_OUT%MY_PTS%ypts    (ipts)
10           format(&
                  'Ground deposit file for : ',a              ,/, &
                  'Coordinates             : ',f13.4,1x,f13.4 ,/, &
                  '                          '                ,/, &
                  ' Diameter    fi       load     fraction '  ,/, &
                  '  (mm)      (--)     (kg/m2)     (%)    '  ,/, &
                  '----------------------------------------')
             !
             total = sum(my_ap)
             jb = 0
             do ibin = 1,nbins
                if(MY_TRA%MY_BIN%bin_spe(ibin).eq.spe_code) then
                   jb = jb + 1
                   diam = MY_TRA%MY_BIN%bin_diam(ibin)*1e3_rp               ! in mm
                   ! Avoids NaN when total=0
                   if(total > 0.0_rp) then
                      fraction = 100.0_rp*my_ap(jb)/total
                   else
                      fraction = 0.0_rp
                   end if
                   write(90,30) diam, -log(diam)/log(2.0_rp), &
                                my_ap(jb), fraction
30                 format(f7.4,1x,f7.2,1x,e15.6E3,1x,f9.4)
                end if
             end do
             close(90)
          end if
          !
       end if   !  if(MY_OUT%MY_PTS%zpts(ipts).lt.0.0_rp) then   2D point
       !
    end do
    !
    end do
    !
    return
  end subroutine F3D_out_pts_grn
  !
  !
  !
END MODULE F3D
