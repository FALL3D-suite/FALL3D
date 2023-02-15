!***************************************************************
!>
!> Module for procedures related to SetSrc
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE Src
  use KindType
  use InpOut
  use Parallel
  use Time
  use Coord
  use PlumeBPT
  use Phys
  use Ensemble
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: src_read_inp_source_params
  PUBLIC :: src_bcast_source_params
  PUBLIC :: src_read_profiles
  PUBLIC :: src_bcast_profiles
  PUBLIC :: src_check_profiles
  PUBLIC :: src_interpolate_profile
  PUBLIC :: src_get_mer_wind
  PUBLIC :: src_solve_point
  PUBLIC :: src_solve_suzuki
  PUBLIC :: src_solve_hat
  PUBLIC :: src_solve_plume
  PUBLIC :: src_write_source
  PUBLIC :: src_read_source
  PUBLIC :: src_bcast_source
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine src_read_inp_source_params
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads the SOURCE block from the input file (source parameters)
  !
  subroutine src_read_inp_source_params(MY_FILES, MY_TIME, MY_SPE, MY_ESP, MY_PLUME, MY_ENS, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TIME   variables related to time
    !>   @param MY_SPE    list of parameters defining species and categories
    !>   @param MY_ESP    list of parameters defining a Eruption Source Parameters
    !>   @param MY_PLUME  list of parameters defining the Plume Source Parameters
    !>   @param MY_ENS    list of ensemble parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(RUN_TIME),       intent(IN   ) :: MY_TIME
    type(SPECIES_PARAMS), intent(IN   ) :: MY_SPE
    type(ESP_PARAMS),     intent(INOUT) :: MY_ESP
    type(PLUME_PARAMS),   intent(INOUT) :: MY_PLUME
    type(ENS_PARAMS),     intent(IN   ) :: MY_ENS
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    integer(ip), parameter :: MAX_RVOID=1000
    integer(ip)            :: iyr,imo,idy,ihr,imi,ise
    integer(ip)            :: ispe,cat_code,spe_code
    !
    character(len=s_file)  :: file_inp, file_ndt
    character(len=s_name)  :: cvoid
    character(len=24    )  :: time1_string,time2_string
    integer(ip)            :: ndt,idt,ndt0
    integer(ip)            :: es_duration
    real(rp)               :: rvoid(MAX_RVOID)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_read_inp_source_params'
    MY_ERR%message = ' '
    !
    file_inp = MY_FILES%file_inp
    file_ndt = ''
    !
    !*** Reference time
    !
    MY_ESP%start_year  = MY_TIME%start_year
    MY_ESP%start_month = MY_TIME%start_month
    MY_ESP%start_day   = MY_TIME%start_day
    !
    !*** First, determine the type of source
    !
    call inpout_get_cha (file_inp, 'SOURCE','SOURCE_TYPE', MY_ESP%source_type, 1, MY_ERR, .true.)
    if(MY_ERR%flag.ne.0) return
    !
    select case(MY_ESP%source_type)
    case('RESUSPENSION')
       !
       MY_ERR%flag    = 1
       MY_ERR%message = 'resuspension source type implemented yet in this version'
       !
       ! call readinp_resu
       return
    case ('POINT','SUZUKI','TOP-HAT','PLUME')
       continue
    end select
    !
    !*** Check for incompatibilities
    !
    do ispe = 1,MY_SPE%nspe
       cat_code = MY_SPE%category(ispe)
       spe_code = MY_SPE%code    (ispe)
       !
       select case(spe_code)
       case(SPE_TEPHRA)
          continue        ! admits all sources
       case(SPE_DUST)
          if(MY_ESP%source_type.ne.'RESUSPENSION') then
             MY_ERR%flag    = 1
             MY_ERR%message = 'specie dust only admits resuspension source type'
             return
          end if
       case(SPE_H2O,SPE_SO2)
          if(MY_ESP%source_type.eq.'RESUSPENSION') then
             MY_ERR%flag    = 1
             MY_ERR%message = 'aerosol species do not admit resuspension source type'
             return
          end if
       case default ! radionuclides
          if(MY_ESP%source_type.eq.'RESUSPENSION') then
             MY_ERR%flag    = 1
             MY_ERR%message = 'radionuclide species do not admit resuspension source type'
             return
          end if
          if(MY_ESP%source_type.eq.'PLUME') then
             MY_ERR%flag    = 1
             MY_ERR%message = 'radionuclide species do not admit plume source type'
             return
          end if
       end select
    end do
    !
    !*** Get the number of source time slabs ndt (e.g. number of eruption phases)
    !
    call inpout_get_cha (file_inp, 'SOURCE','SOURCE_START_(HOURS_AFTER_00)', file_ndt, 1, MY_ERR, uppercase=.false.)
    if(file_ndt(1:1) == '!') file_ndt = ''  ! consider an eventual comment
    if(TRIM(file_ndt) == '') then
       call inpout_get_npar(file_inp, 'SOURCE','SOURCE_START_(HOURS_AFTER_00)', ndt, MY_ERR)
       if(MY_ERR%flag.ne.0) return
    else
       if(file_ndt(1:1) /= '/') file_ndt = TRIM(MY_FILES%commonpath)//'/'//TRIM(file_ndt)  ! If no absolute path, then prepend the directory name
       call inpout_get_file_nrows(file_ndt,ndt,MY_ERR)  ! get ndt from file
       if(MY_ERR%flag.ne.0) return
    end if
    ndt =  min(ndt,MAX_RVOID)
    !
    !*** Allocates memory
    !
    MY_ESP%ndt = ndt
    allocate(MY_ESP%start_time(MY_ESP%ndt))
    allocate(MY_ESP%end_time  (MY_ESP%ndt))
    allocate(MY_ESP%h_dt      (MY_ESP%ndt))
    allocate(MY_ESP%M0_dt     (MY_ESP%ndt))
    allocate(MY_ESP%T0_dt     (MY_ESP%ndt))
    allocate(MY_ESP%w0_dt     (MY_ESP%ndt))
    allocate(MY_ESP%As_dt     (MY_ESP%ndt))
    allocate(MY_ESP%Ls_dt     (MY_ESP%ndt))
    allocate(MY_ESP%Th_dt     (MY_ESP%ndt))
    !
    if( MY_ESP%source_type.eq.'PLUME') then
       MY_PLUME%ndt = ndt
       allocate(MY_PLUME%u0_dt(MY_PLUME%ndt))
       allocate(MY_PLUME%Tv_dt(MY_PLUME%ndt))
       allocate(MY_PLUME%Tl_dt(MY_PLUME%ndt))
       allocate(MY_PLUME%Ts_dt(MY_PLUME%ndt))
       allocate(MY_PLUME%wv_dt(MY_PLUME%ndt))
       allocate(MY_PLUME%wl_dt(MY_PLUME%ndt))
       allocate(MY_PLUME%ws_dt(MY_PLUME%ndt))
    end if
    !
    !*** Reads source intervals starting/ending times
    !
    if(TRIM(file_ndt) == '') then
       call inpout_get_rea (file_inp, 'SOURCE','SOURCE_START_(HOURS_AFTER_00)', rvoid, MY_ESP%ndt, MY_ERR)
       if(MY_ERR%flag.ne.0) return
    else
       call inpout_get_file_col(file_ndt,1,rvoid,MY_ESP%ndt,MY_ERR) !  read file first column
       if(MY_ERR%flag.ne.0) return
    end if
    !
    do idt = 1, MY_ESP%ndt
       MY_ESP%start_time(idt) = INT(rvoid(idt)*3600.0_rp)    ! h --> s
    end do
    !
    if(TRIM(file_ndt) == '') then
       call inpout_get_rea (file_inp, 'SOURCE','SOURCE_END_(HOURS_AFTER_00)', rvoid, MY_ESP%ndt, MY_ERR)
       if(MY_ERR%flag.ne.0) return
    else
       call inpout_get_file_col(file_ndt,2,rvoid,MY_ESP%ndt,MY_ERR) !  read second first column
       if(MY_ERR%flag.ne.0) return
    end if
    !
    do idt = 1, MY_ESP%ndt
       MY_ESP%end_time(idt) = INT(rvoid(idt)*3600.0_rp)    ! h --> s
    end do
    !
    !*** If necessary, perturbate source times
    !
    if(nens.gt.1) then
       !
       !*** Perturbate start/end time
       !
       do idt = 1, MY_ESP%ndt
          !
          es_duration            = MAX(MY_ESP%end_time(idt) - MY_ESP%start_time(idt), 0)
          !
          es_duration            = INT(ensemble_perturbate_variable( ID_SOURCE_DURATION, &
                                         1.0_rp*es_duration, MY_ENS ))
          !
          MY_ESP%start_time(idt) = INT(ensemble_perturbate_variable( ID_SOURCE_START, &
                                         1.0_rp*MY_ESP%start_time(idt), MY_ENS ))
          !
          MY_ESP%end_time(idt)   = MY_ESP%start_time(idt) + es_duration
          !
       end do
       !
       !*** Check for overlapping phases
       !
       do idt = 1, MY_ESP%ndt-1
          MY_ESP%end_time(idt) = min(MY_ESP%end_time(idt),MY_ESP%start_time(idt+1))
       end do
       !
    end if
    !
    !*** Check consistency between multiple (>1) intervals
    !
    do idt = 2, MY_ESP%ndt
       if(MY_ESP%start_time(idt).lt.MY_ESP%end_time(idt-1)) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'Inconsistent time slabs at SOURCE_END_(HOURS_AFTER_00)'
          return
       end if
    end do
    !
    !*** Reads source injection height
    !
    if(TRIM(file_ndt) == '') then
       call inpout_get_npar(file_inp, 'SOURCE','HEIGHT_ABOVE_VENT_(M)', ndt0, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       ndt0 =  min(ndt0,ndt)
       call inpout_get_rea (file_inp, 'SOURCE','HEIGHT_ABOVE_VENT_(M)', MY_ESP%h_dt, ndt0, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       if(ndt0.lt.ndt) MY_ESP%h_dt(ndt0:ndt) = MY_ESP%h_dt(ndt0)
    else
       call inpout_get_file_col(file_ndt,3,MY_ESP%h_dt,MY_ESP%ndt,MY_ERR)  ! read third column
    end if
    !
    !*** If necessary, perturbate column height in ensemble runs
    !
    if(nens.gt.1) then
       MY_ESP%h_dt = ensemble_perturbate_variable( ID_COLUMN_HEIGHT, &
                                                   MY_ESP%h_dt, MY_ENS )
    end if
    !
    !*** Reads source mass flow rate
    !
    if(MY_ESP%source_type.eq.'PLUME') then
       !
       MY_ESP%meteo_coupling = .true.
       !
       call inpout_get_cha &
            (file_inp, 'IF_PLUME_SOURCE','SOLVE_PLUME_FOR', MY_PLUME%solve_plume_for, 1, MY_ERR, .true.)
       !
       if(MY_PLUME%solve_plume_for.eq.'MFR') then
          !
          call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','MFR_SEARCH_RANGE', MY_PLUME%n_MFR,2, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          !
          MY_ESP%M0_dt(:) = 0.0_rp
          !
       else   ! solve for height, MEF given
          !
          ndt0 = 0
          call inpout_get_npar(file_inp, 'SOURCE','MASS_FLOW_RATE_(KGS)', ndt0, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          if(ndt0.eq.0) then
             MY_ERR%flag    = -1
             MY_ERR%message = 'Value of mass flow rate needed when solving for plume height'
             return
          end if
          ndt0 =  min(ndt0,ndt)
          !
          call inpout_get_rea (file_inp, 'SOURCE','MASS_FLOW_RATE_(KGS)', MY_ESP%M0_dt, ndt0, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          if(ndt0.lt.ndt) MY_ESP%M0_dt(ndt0:ndt) = MY_ESP%M0_dt(ndt0)
       end if
       !
    else
       !
       cvoid =''
       call inpout_get_cha (file_inp, 'SOURCE','MASS_FLOW_RATE_(KGS)', cvoid, 1, MY_ERR, .true.)
       !
       select case(TRIM(cvoid))
       case('ESTIMATE-MASTIN')
          !
          MY_ESP%MER_vs_h        = 'ESTIMATE-MASTIN'
          MY_ESP%meteo_coupling  = .false.
          !
          !*** Estimate MFR from height using  MFR = rho*(H/2)**(1/0.241) with rho = 2500 kg/m3 or, equivalently,
          !***                                 MFR = 140.91*H**4.149  (H in km)
          do idt = 1,ndt
             MY_ESP%M0_dt(idt) = 140.91_rp*((MY_ESP%h_dt(idt)/1e3_rp)**4.149_rp)
          end do
          !
       case('ESTIMATE-WOODHOUSE')
          !
          MY_ESP%MER_vs_h        = 'ESTIMATE-WOODHOUSE'
          MY_ESP%meteo_coupling  = .true.
          !
       case('ESTIMATE-DEGRUYTER')
          !
          MY_ESP%MER_vs_h       = 'ESTIMATE-DEGRUYTER'
          MY_ESP%meteo_coupling = .true.
          !
       case default
          !
          MY_ESP%MER_vs_h       = 'NONE'
          MY_ESP%meteo_coupling = .false.
          !
          call inpout_get_npar(file_inp, 'SOURCE','MASS_FLOW_RATE_(KGS)', ndt0, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          ndt0 =  min(ndt0,ndt)
          call inpout_get_rea (file_inp, 'SOURCE','MASS_FLOW_RATE_(KGS)', MY_ESP%M0_dt, ndt0, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          if(ndt0.lt.ndt) MY_ESP%M0_dt(ndt0:ndt) = MY_ESP%M0_dt(ndt0)
          !
          !*** If necessary, perturbate mass flow rate in ensemble runs. Note that this is not activated for PLUME or ESTIMATE options
          !
          if(nens.gt.1) then
            MY_ESP%M0_dt = ensemble_perturbate_variable( ID_MASS_FLOW_RATE, &
                                                         MY_ESP%M0_dt, MY_ENS )
          end if
          !
       end select
       !
    end if
    !
    call inpout_get_rea (file_inp, 'SOURCE','ALFA_PLUME', MY_ESP%alfa_plume, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) MY_ESP%alfa_plume = 0.1  ! default vaule
    !
    call inpout_get_rea (file_inp, 'SOURCE','BETA_PLUME', MY_ESP%beta_plume, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) MY_ESP%beta_plume = 0.5  ! default vaule
    !
    !*** Other variables
    !
    call inpout_get_npar(file_inp, 'SOURCE','EXIT_TEMPERATURE_(K)', ndt0, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    ndt0 =  min(ndt0,ndt)
    call inpout_get_rea (file_inp, 'SOURCE','EXIT_TEMPERATURE_(K)', MY_ESP%T0_dt, ndt0, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    if(ndt0.lt.ndt) MY_ESP%T0_dt(ndt0:ndt) = MY_ESP%T0_dt(ndt0)
    !
    call inpout_get_npar(file_inp, 'SOURCE','EXIT_WATER_FRACTION_(%)', ndt0, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    ndt0 =  min(ndt0,ndt)
    call inpout_get_rea (file_inp, 'SOURCE','EXIT_WATER_FRACTION_(%)', MY_ESP%w0_dt, ndt0, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    if(ndt0.lt.ndt) MY_ESP%w0_dt(ndt0:ndt) = MY_ESP%w0_dt(ndt0)
    MY_ESP%w0_dt(:) = MY_ESP%w0_dt(:)/1e2_rp                        ! Convert from %
    !
    if(MY_ESP%meteo_coupling ) then
       !
       call inpout_get_rea (file_inp, 'METEO_DATA','METEO_COUPLING_INTERVAL_(MIN)', rvoid, 1, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       MY_ESP%meteo_coupling_interval = INT(rvoid(1))
       MY_ESP%meteo_coupling_interval = MY_ESP%meteo_coupling_interval*60  ! min --> s
       !
    else
       !
       MY_ESP%meteo_coupling_interval = 0
       !
    end if
    !
    !*** Reads source dependent parameters
    !
    select case(MY_ESP%source_type)
    case ('POINT')
       !
       continue
       !
    case('SUZUKI')
       !
       call inpout_get_npar(file_inp, 'IF_SUZUKI_SOURCE','A', ndt0, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       ndt0 =  min(ndt0,ndt)
       call inpout_get_rea (file_inp, 'IF_SUZUKI_SOURCE','A', MY_ESP%As_dt, ndt0, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       if(ndt0.lt.ndt) MY_ESP%As_dt(ndt0:ndt) = MY_ESP%As_dt(ndt0)
       !
       call inpout_get_npar(file_inp, 'IF_SUZUKI_SOURCE','L', ndt0, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       ndt0 =  min(ndt0,ndt)
       call inpout_get_rea (file_inp, 'IF_SUZUKI_SOURCE','L', MY_ESP%Ls_dt, ndt0, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       if(ndt0.lt.ndt) MY_ESP%Ls_dt(ndt0:ndt) = MY_ESP%Ls_dt(ndt0)
       !
       !*** If necessary, perturbate Suzuki parameters in ensemble runs
       !
       if(nens.gt.1) then
          MY_ESP%As_dt = ensemble_perturbate_variable( ID_SUZUKI_A, &
                                                       MY_ESP%As_dt, MY_ENS )
          MY_ESP%Ls_dt = ensemble_perturbate_variable( ID_SUZUKI_L, &
                                                       MY_ESP%Ls_dt, MY_ENS )
       end if
       !
    case('TOP-HAT')
       !
       call inpout_get_npar(file_inp, 'IF_TOP-HAT_SOURCE','THICKNESS_(M)', ndt0, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       ndt0 =  min(ndt0,ndt)
       call inpout_get_rea (file_inp, 'IF_TOP-HAT_SOURCE','THICKNESS_(M)', MY_ESP%Th_dt, ndt0, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       if(ndt0.lt.ndt) MY_ESP%Th_dt(ndt0:ndt) = MY_ESP%Th_dt(ndt0)
       !
       !*** If necessary, perturbate Top-hat parameters in ensemble runs
       !
       if(nens.gt.1) then
          MY_ESP%Th_dt = ensemble_perturbate_variable( ID_TOP_HAT_THICKNESS, &
                                                       MY_ESP%Th_dt, MY_ENS )
       end if
       !
    case('PLUME')
       !
       call inpout_get_npar(file_inp, 'IF_PLUME_SOURCE','EXIT_VELOCITY_(MS)', ndt0, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       ndt0 =  min(ndt0,ndt)
       call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','EXIT_VELOCITY_(MS)', MY_PLUME%u0_dt, ndt0, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       if(ndt0.lt.ndt) MY_PLUME%u0_dt(ndt0:ndt) = MY_PLUME%u0_dt(ndt0)
       !
       call inpout_get_npar(file_inp, 'IF_PLUME_SOURCE','EXIT_GAS_WATER_TEMPERATURE_(K)', ndt0, MY_ERR)
       if(MY_ERR%flag.eq.0) then
          ndt0 =  min(ndt0,ndt)
          call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','EXIT_GAS_WATER_TEMPERATURE_(K)', MY_PLUME%Tv_dt, ndt0, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          if(ndt0.lt.ndt) MY_PLUME%Tv_dt(ndt0:ndt) = MY_PLUME%Tv_dt(ndt0)
       else
          MY_PLUME%Tv_dt(:) = MY_ESP%T0_dt(:)  ! default mixture values
       end if
       !
       call inpout_get_npar(file_inp, 'IF_PLUME_SOURCE','EXIT_LIQUID_WATER_TEMPERATURE_(K)', ndt0, MY_ERR)
       if(MY_ERR%flag.eq.0) then
          ndt0 =  min(ndt0,ndt)
          call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','EXIT_LIQUID_WATER_TEMPERATURE_(K)', MY_PLUME%Tl_dt, ndt0, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          if(ndt0.lt.ndt) MY_PLUME%Tl_dt(ndt0:ndt) = MY_PLUME%Tl_dt(ndt0)
       else
          MY_PLUME%Tl_dt(:) = MY_ESP%T0_dt(:)  ! default mixture values
       end if
       !
       call inpout_get_npar(file_inp, 'IF_PLUME_SOURCE','EXIT_SOLID_WATER_TEMPERATURE_(K)', ndt0, MY_ERR)
       if(MY_ERR%flag.eq.0) then
          ndt0 =  min(ndt0,ndt)
          call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','EXIT_SOLID_WATER_TEMPERATURE_(K)', MY_PLUME%Ts_dt, ndt0, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          if(ndt0.lt.ndt) MY_PLUME%Ts_dt(ndt0:ndt) = MY_PLUME%Ts_dt(ndt0)
       else
          MY_PLUME%Ts_dt(:) = MY_ESP%T0_dt(:)  ! default mixture values
       end if
       !
       call inpout_get_npar(file_inp, 'IF_PLUME_SOURCE','EXIT_GAS_WATER_FRACTION_(%)', ndt0, MY_ERR)
       if(MY_ERR%flag.eq.0) then
          ndt0 =  min(ndt0,ndt)
          call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','EXIT_GAS_WATER_FRACTION_(%)', MY_PLUME%wv_dt, ndt0, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          if(ndt0.lt.ndt) MY_PLUME%wv_dt(ndt0:ndt) = MY_PLUME%wv_dt(ndt0)
          MY_PLUME%wv_dt(:) = MY_PLUME%wv_dt(:)/1e2_rp   ! Convert from %
       else
          MY_PLUME%wv_dt(:) = MY_ESP%w0_dt(:)  ! default values (already converted)
       end if

       !
       call inpout_get_npar(file_inp, 'IF_PLUME_SOURCE','EXIT_LIQUID_WATER_FRACTION_(%)', ndt0, MY_ERR)
       if(MY_ERR%flag.eq.0) then
          ndt0 =  min(ndt0,ndt)
          call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','EXIT_LIQUID_WATER_FRACTION_(%)', MY_PLUME%wl_dt, ndt0, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          if(ndt0.lt.ndt) MY_PLUME%wl_dt(ndt0:ndt) = MY_PLUME%wl_dt(ndt0)
       else
          MY_PLUME%wl_dt(:) = 0.0_rp  ! default values
       end if
       MY_PLUME%wl_dt(:) = MY_PLUME%wl_dt(:)/1e2_rp   ! Convert from %
       !
       call inpout_get_npar(file_inp, 'IF_PLUME_SOURCE','EXIT_SOLID_WATER_FRACTION_(%)', ndt0, MY_ERR)
       if(MY_ERR%flag.eq.0) then
          ndt0 =  min(ndt0,ndt)
          call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','EXIT_SOLID_WATER_FRACTION_(%)', MY_PLUME%ws_dt, ndt0, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          if(ndt0.lt.ndt) MY_PLUME%ws_dt(ndt0:ndt) = MY_PLUME%ws_dt(ndt0)
       else
          MY_PLUME%ws_dt(:) = 0.0_rp  ! default values
       end if
       MY_PLUME%ws_dt(:) = MY_PLUME%ws_dt(:)/1e2_rp   ! Convert from %
       !
       call inpout_get_cha (file_inp, 'IF_PLUME_SOURCE','WIND_COUPLING', cvoid, 1, MY_ERR, .true.)
       if(MY_ERR%flag.eq.0.and.TRIM(cvoid).eq.'YES') then
          MY_PLUME%wind_coupling = .true.
       else
          MY_PLUME%wind_coupling = .false.
       end if
       !
       call inpout_get_cha (file_inp, 'IF_PLUME_SOURCE','AIR_MOISTURE', cvoid, 1, MY_ERR, .true.)
       if(MY_ERR%flag.eq.0.and.TRIM(cvoid).eq.'YES') then
          MY_PLUME%moist_air = .true.
       else
          MY_PLUME%moist_air = .false.
       end if
       !
       call inpout_get_cha (file_inp, 'IF_PLUME_SOURCE','LATENT_HEAT', cvoid, 1, MY_ERR, .true.)
       if(MY_ERR%flag.eq.0.and.TRIM(cvoid).eq.'YES') then
          MY_PLUME%latent_heat = .true.
       else
          MY_PLUME%latent_heat = .false.
       end if
       !
       call inpout_get_cha (file_inp, 'IF_PLUME_SOURCE','REENTRAINMENT', cvoid, 1, MY_ERR, .true.)
       if(MY_ERR%flag.eq.0.and.TRIM(cvoid).eq.'YES') then
          MY_PLUME%reentrainment  = .true.
       else
          MY_PLUME%reentrainment  = .false.
       end if
       !
       call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','BURSIK_FACTOR', rvoid, 1, MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_PLUME%xi  = rvoid(1)
       else
          MY_PLUME%xi  = 0.1_rp
       end if
       !
       call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','Z_MIN_WIND', rvoid, 1, MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_PLUME%zmin_wind  = rvoid(1)
       else
          MY_PLUME%zmin_wind  = 100.0_rp
       end if
       !
       call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','C_UMBRELLA', rvoid, 1, MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_PLUME%c_umbrella  = rvoid(1)
       else
          MY_PLUME%c_umbrella  = 1.32_rp
       end if
       !
       call inpout_get_cha (file_inp, 'IF_PLUME_SOURCE','A_S', cvoid, 1, MY_ERR, .true.)
       if(MY_ERR%flag.ne.0) return
       if(TRIM(cvoid).eq.'CONSTANT') then
          MY_PLUME%type_as = 'CONSTANT'
          call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','A_S', rvoid, 2, MY_ERR)
          if(MY_ERR%flag.eq.0) then
             MY_PLUME%a_s_jet   = rvoid(1)
             MY_PLUME%a_s_plume = rvoid(2)
          else
             MY_PLUME%a_s_jet   = 0.075_rp
             MY_PLUME%a_s_plume = 0.12_rp
          end if
       else if(TRIM(cvoid).eq.'KAMINSKI-R') then
          MY_PLUME%type_as = cvoid
       else if(TRIM(cvoid).eq.'KAMINSKI-C') then
          MY_PLUME%type_as = cvoid
       else
          MY_ERR%flag    = 1
          MY_ERR%message = 'A_S incorrerct type of parameterization'
          return
       end if
       !
       call inpout_get_cha (file_inp, 'IF_PLUME_SOURCE','A_V', cvoid, 1, MY_ERR, .true.)
       if(MY_ERR%flag.ne.0) return
       if(TRIM(cvoid).eq.'CONSTANT') then
          MY_PLUME%type_av = 'CONSTANT'
          call inpout_get_rea (file_inp, 'IF_PLUME_SOURCE','A_V', rvoid, 1, MY_ERR)
          if(MY_ERR%flag.eq.0) then
             MY_PLUME%a_v = rvoid(1)
          else
             MY_PLUME%a_v = 0.3_rp
          end if
       else if(TRIM(cvoid).eq.'TATE') then
          MY_PLUME%type_av = cvoid
       else
          MY_ERR%flag    = 1
          MY_ERR%message = 'A_V incorrerct type of parameterization'
          return
       end if
       !
       MY_PLUME%umbrella_model = 'SPARKS1986'  ! Copy default value
       !
    end select
    !
    !*** Reads vent coordinates
    !
    call inpout_get_rea (file_inp, 'SOURCE','LON_VENT',MY_ESP%lon, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    call inpout_get_rea (file_inp, 'SOURCE','LAT_VENT',MY_ESP%lat, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    call inpout_get_rea (file_inp, 'SOURCE','VENT_HEIGHT_(M)',MY_ESP%zo, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    if(MY_ESP%source_type.eq.'PLUME') then
       !
       MY_PLUME%zv = MY_ESP%zo
       call coord_ll2utm (MY_ESP%lon,MY_ESP%lat,MY_PLUME%zone_UTM,MY_PLUME%xv_UTM,MY_PLUME%yv_UTM, WGS_84_DATUM, MY_ERR)
       !
    end if
    !
    !*** Writes to log file
    !
    call time_addtime(MY_ESP%start_year,MY_ESP%start_month,MY_ESP%start_day, 0,  &
         iyr,imo,idy,ihr,imi,ise,1.0_rp*MY_ESP%start_time(1),MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip,time1_string,MY_ERR)
    !
    call time_addtime(MY_ESP%start_year,MY_ESP%start_month,MY_ESP%start_day, 0,  &
         iyr,imo,idy,ihr,imi,ise,1.0_rp*MY_ESP%end_time(MY_ESP%ndt),MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip,time2_string,MY_ERR)
    !
    write(MY_FILES%lulog,10) time1_string,time2_string
10  format(/,&
         '  SOURCE TIME RANGE',/,&
         '  Initial time       : ',a,/,&
         '  Final   time       : ',a,/)
    !
    return
  end subroutine src_read_inp_source_params
  !
  !-----------------------------------------
  !    subroutine src_bcast_source_params
  !-----------------------------------------
  !
  !>   @brief
  !>   Broadcasts SOURCE block parameters
  !
  subroutine src_bcast_source_params(MY_ESP, MY_PLUME, MY_ERR)
    implicit none
    !
    !>   @param MY_ESP    list of parameters defining Eruption Source Parameters
    !>   @param MY_PLUME  list of parameters defining the Plume Source Parameters
    !>   @param MY_ERR    error handler
    !
    type(ESP_PARAMS),    intent(INOUT) :: MY_ESP
    type(PLUME_PARAMS),  intent(INOUT) :: MY_PLUME
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_bcast_source_params'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_ESP%meteo_coupling         ,1,0)
    call parallel_bcast(MY_ESP%source_type            ,1,0)
    call parallel_bcast(MY_ESP%MER_vs_h               ,1,0)
    call parallel_bcast(MY_ESP%ndt                    ,1,0)
    call parallel_bcast(MY_ESP%meteo_coupling_interval,1,0)
    call parallel_bcast(MY_ESP%start_year             ,1,0)
    call parallel_bcast(MY_ESP%start_month            ,1,0)
    call parallel_bcast(MY_ESP%start_day              ,1,0)
    call parallel_bcast(MY_ESP%lon                    ,1,0)
    call parallel_bcast(MY_ESP%lat                    ,1,0)
    call parallel_bcast(MY_ESP%zo                     ,1,0)
    call parallel_bcast(MY_ESP%alfa_plume             ,1,0)
    call parallel_bcast(MY_ESP%beta_plume             ,1,0)
    !
    !*** Memory allocation
    !
    if(.not.master_model) then
       allocate(MY_ESP%start_time(MY_ESP%ndt))
       allocate(MY_ESP%end_time  (MY_ESP%ndt))
       allocate(MY_ESP%h_dt      (MY_ESP%ndt))
       allocate(MY_ESP%M0_dt     (MY_ESP%ndt))
       allocate(MY_ESP%T0_dt     (MY_ESP%ndt))
       allocate(MY_ESP%w0_dt     (MY_ESP%ndt))
       allocate(MY_ESP%As_dt     (MY_ESP%ndt))
       allocate(MY_ESP%Ls_dt     (MY_ESP%ndt))
       allocate(MY_ESP%Th_dt     (MY_ESP%ndt))
    end if
    !
    call parallel_bcast(MY_ESP%start_time,MY_ESP%ndt,0)
    call parallel_bcast(MY_ESP%end_time  ,MY_ESP%ndt,0)
    call parallel_bcast(MY_ESP%h_dt      ,MY_ESP%ndt,0)
    call parallel_bcast(MY_ESP%M0_dt     ,MY_ESP%ndt,0)
    call parallel_bcast(MY_ESP%T0_dt     ,MY_ESP%ndt,0)
    call parallel_bcast(MY_ESP%w0_dt     ,MY_ESP%ndt,0)
    call parallel_bcast(MY_ESP%As_dt     ,MY_ESP%ndt,0)
    call parallel_bcast(MY_ESP%Ls_dt     ,MY_ESP%ndt,0)
    call parallel_bcast(MY_ESP%Th_dt     ,MY_ESP%ndt,0)
    !
    !*** If necessary broadcast MY_PLUME
    !
    if( MY_ESP%source_type.eq.'PLUME') then
       !
       call parallel_bcast(MY_PLUME%moist_air      ,1,0)
       call parallel_bcast(MY_PLUME%wind_coupling  ,1,0)
       call parallel_bcast(MY_PLUME%reentrainment  ,1,0)
       call parallel_bcast(MY_PLUME%latent_heat    ,1,0)
       call parallel_bcast(MY_PLUME%solve_plume_for,1,0)
       call parallel_bcast(MY_PLUME%type_as        ,1,0)
       call parallel_bcast(MY_PLUME%type_av        ,1,0)
       call parallel_bcast(MY_PLUME%umbrella_model ,1,0)
       call parallel_bcast(MY_PLUME%ndt            ,1,0)
       call parallel_bcast(MY_PLUME%ns             ,1,0)
       call parallel_bcast(MY_PLUME%np             ,1,0)
       call parallel_bcast(MY_PLUME%time1          ,1,0)
       call parallel_bcast(MY_PLUME%time2          ,1,0)
       !
       call parallel_bcast(MY_PLUME%xv_UTM         ,1,0)
       call parallel_bcast(MY_PLUME%yv_UTM         ,1,0)
       call parallel_bcast(MY_PLUME%zone_UTM       ,1,0)
       call parallel_bcast(MY_PLUME%zv             ,1,0)
       call parallel_bcast(MY_PLUME%n_MFR          ,2,0)
       call parallel_bcast(MY_PLUME%xi             ,1,0)
       call parallel_bcast(MY_PLUME%zmin_wind      ,1,0)
       call parallel_bcast(MY_PLUME%c_umbrella     ,1,0)
       call parallel_bcast(MY_PLUME%a_s_jet        ,1,0)
       call parallel_bcast(MY_PLUME%a_s_plume      ,1,0)
       call parallel_bcast(MY_PLUME%a_v            ,1,0)
       !
       if(.not.master_model) then
          allocate(MY_PLUME%u0_dt(MY_PLUME%ndt))
          allocate(MY_PLUME%Tv_dt(MY_PLUME%ndt))
          allocate(MY_PLUME%Tl_dt(MY_PLUME%ndt))
          allocate(MY_PLUME%Ts_dt(MY_PLUME%ndt))
          allocate(MY_PLUME%wv_dt(MY_PLUME%ndt))
          allocate(MY_PLUME%wl_dt(MY_PLUME%ndt))
          allocate(MY_PLUME%ws_dt(MY_PLUME%ndt))
       end if
       !
       call parallel_bcast(MY_PLUME%u0_dt,MY_PLUME%ndt,0)
       call parallel_bcast(MY_PLUME%Tv_dt,MY_PLUME%ndt,0)
       call parallel_bcast(MY_PLUME%Tl_dt,MY_PLUME%ndt,0)
       call parallel_bcast(MY_PLUME%Ts_dt,MY_PLUME%ndt,0)
       call parallel_bcast(MY_PLUME%wv_dt,MY_PLUME%ndt,0)
       call parallel_bcast(MY_PLUME%wl_dt,MY_PLUME%ndt,0)
       call parallel_bcast(MY_PLUME%ws_dt,MY_PLUME%ndt,0)
       !
    end if
    !
    return
  end subroutine src_bcast_source_params
  !
  !-----------------------------------------
  !    subroutine src_read_profiles
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads meteo profiles from a dbs file
  !
  subroutine src_read_profiles(MY_FILES, GL_METPROFILES, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES    list of files
    !>   @param GL_METPROFILES  variables related to meteorological profiles
    !>   @param MY_ERR      error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(METEO_PROFILE), intent(INOUT) :: GL_METPROFILES
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip), parameter :: nwormax = 128
    integer(ip), parameter :: s_long  = 512
    !
    character(len=s_file) :: fname
    character(len=s_long) :: card
    character(len=s_long) :: words(nwormax)
    character(len=24    ) :: time1_string,time2_string
    real(rp)              :: param(nwormax)
    integer(ip)           :: nword, npar
    integer(ip)           :: nt,nz,it,iz
    !
    !*** Initializations
    !
    MY_ERR%flag = 0
    fname       = MY_FILES%file_pro
    !
    !*** Get the number of time steps and vertical levels
    !
    open(90,FILE=TRIM(fname),status='old',err=100)
    nt = 0
    nz = 0
    do
       read(90,'(a512)',end=10,err=101) card
       call inpout_sdecode(card,words,param,nword,npar)
       !
       if(nword.gt.2.and.words(2).eq.'time') nt = nt + 1
       if(nword.eq.0                       ) nz = nz + 1
    end do
10  rewind(90)
    nz = nz/nt
    !
    !*** Allocates
    !
    GL_METPROFILES%exists = .true.
    GL_METPROFILES%nt     = nt
    GL_METPROFILES%nz     = nz
    !
    allocate(GL_METPROFILES%time   (GL_METPROFILES%nt              ))
    allocate(GL_METPROFILES%timesec(GL_METPROFILES%nt              ))
    allocate(GL_METPROFILES%zavl   (GL_METPROFILES%nz,GL_METPROFILES%nt))
    allocate(GL_METPROFILES%p      (GL_METPROFILES%nz,GL_METPROFILES%nt))
    allocate(GL_METPROFILES%t      (GL_METPROFILES%nz,GL_METPROFILES%nt))
    allocate(GL_METPROFILES%tp     (GL_METPROFILES%nz,GL_METPROFILES%nt))
    allocate(GL_METPROFILES%tv     (GL_METPROFILES%nz,GL_METPROFILES%nt))
    allocate(GL_METPROFILES%u      (GL_METPROFILES%nz,GL_METPROFILES%nt))
    allocate(GL_METPROFILES%v      (GL_METPROFILES%nz,GL_METPROFILES%nt))
    allocate(GL_METPROFILES%qv     (GL_METPROFILES%nz,GL_METPROFILES%nt))
    allocate(GL_METPROFILES%rho    (GL_METPROFILES%nz,GL_METPROFILES%nt))
    !
    !*** Reads all data
    !
    it = 0
    iz = 0
    do
       read(90,'(a512)',end=20,err=101) card
       call inpout_sdecode(card,words,param,nword,npar)
       !
       if(nword.gt.2) then
          !
          if(words(2).eq.'time') then
             it = it + 1
             GL_METPROFILES%time(it) = DATETIME( nint(param(1), kind=ip), &
                                                 nint(param(2), kind=ip), &
                                                 nint(param(3), kind=ip), &
                                                 nint(param(4), kind=ip), &
                                                 nint(param(5), kind=ip), &
                                                 nint(param(6), kind=ip)  )
          else if(words(2).eq.'timesec') then
             GL_METPROFILES%timesec(it) = param(1)
          else if(words(2).eq.'lon') then
             GL_METPROFILES%lon = param(1)
          else if(words(2).eq.'lat') then
             GL_METPROFILES%lat = param(1)
          else if(words(2).eq.'zo') then
             GL_METPROFILES%zo  = param(1)
          end if
          !
       end if
       !
       if(nword.eq.0) then
          iz = iz + 1
          GL_METPROFILES%zavl(iz,it) = param(1)*1e3_rp   !  km -->m (above terrain)
          GL_METPROFILES%rho (iz,it) = param(2)
          GL_METPROFILES%p   (iz,it) = param(3)*1e2_rp   !  hPa --> Pa
          GL_METPROFILES%t   (iz,it) = param(4)
          GL_METPROFILES%qv  (iz,it) = param(5)/1e3_rp   ! g/kg  --> kg/kg
          GL_METPROFILES%u   (iz,it) = param(6)
          GL_METPROFILES%v   (iz,it) = param(7)
          if(iz.eq.GL_METPROFILES%nz) iz = 0
       end if
    end do
20  close(90)
    !
    !*** Write to log file
    !
    call time_dateformat(GL_METPROFILES%time(1)%year,   &
                         GL_METPROFILES%time(1)%month,  &
                         GL_METPROFILES%time(1)%day,    &
                         GL_METPROFILES%time(1)%hour,   &
                         GL_METPROFILES%time(1)%minute, &
                         GL_METPROFILES%time(1)%second, &
                         3_ip,time1_string,MY_ERR)
    !
    call time_dateformat(GL_METPROFILES%time(GL_METPROFILES%nt)%year,   &
                         GL_METPROFILES%time(GL_METPROFILES%nt)%month,  &
                         GL_METPROFILES%time(GL_METPROFILES%nt)%day,    &
                         GL_METPROFILES%time(GL_METPROFILES%nt)%hour,   &
                         GL_METPROFILES%time(GL_METPROFILES%nt)%minute, &
                         GL_METPROFILES%time(GL_METPROFILES%nt)%second, &
                         3_ip,time2_string,MY_ERR)
    !
    write(MY_FILES%lulog,30) time1_string,time2_string
30  format('  METEO (PROFILES) TIME RANGE',/,&
         '  Initial time       : ',a,/,&
         '  Final   time       : ',a,/)
    !
    return
    !
100 MY_ERR%flag = -1
    MY_ERR%source  = 'src_read_profiles'
    MY_ERR%message = 'error opening file '//TRIM(fname)
    return
101 MY_ERR%flag = -1
    MY_ERR%source  = 'src_read_profiles'
    MY_ERR%message = 'error reading file '//TRIM(fname)
    !
  end subroutine src_read_profiles
  !
  !-----------------------------------------
  !    subroutine src_bcast_profiles
  !-----------------------------------------
  !
  !>   @brief
  !>   Broadcasts a set of vertical profiles
  !
  subroutine src_bcast_profiles(GL_METPROFILES, MY_ERR)
    implicit none
    !
    !>   @param GL_METPROFILES  variables related to meteorological profiles
    !>   @param MY_ERR      error handler
    !
    type(METEO_PROFILE), intent(INOUT) :: GL_METPROFILES
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_bcast_profiles'
    MY_ERR%message = ' '
    !
    call parallel_bcast(GL_METPROFILES%exists,1,0)
    call parallel_bcast(GL_METPROFILES%nz    ,1,0)
    call parallel_bcast(GL_METPROFILES%nt    ,1,0)
    call parallel_bcast(GL_METPROFILES%lon   ,1,0)
    call parallel_bcast(GL_METPROFILES%lat   ,1,0)
    call parallel_bcast(GL_METPROFILES%zo    ,1,0)
    !
    !*** Memory allocation
    !
    if(.not.master_model) then
       allocate(GL_METPROFILES%time   (GL_METPROFILES%nt              ))
       allocate(GL_METPROFILES%timesec(GL_METPROFILES%nt              ))
       allocate(GL_METPROFILES%zavl   (GL_METPROFILES%nz,GL_METPROFILES%nt))
       allocate(GL_METPROFILES%p      (GL_METPROFILES%nz,GL_METPROFILES%nt))
       allocate(GL_METPROFILES%t      (GL_METPROFILES%nz,GL_METPROFILES%nt))
       allocate(GL_METPROFILES%tp     (GL_METPROFILES%nz,GL_METPROFILES%nt))
       allocate(GL_METPROFILES%tv     (GL_METPROFILES%nz,GL_METPROFILES%nt))
       allocate(GL_METPROFILES%u      (GL_METPROFILES%nz,GL_METPROFILES%nt))
       allocate(GL_METPROFILES%v      (GL_METPROFILES%nz,GL_METPROFILES%nt))
       allocate(GL_METPROFILES%qv     (GL_METPROFILES%nz,GL_METPROFILES%nt))
       allocate(GL_METPROFILES%rho    (GL_METPROFILES%nz,GL_METPROFILES%nt))
    end if
    !
    call parallel_bcast(GL_METPROFILES%time%year,   GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%time%month,  GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%time%day,    GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%time%hour,   GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%time%minute, GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%time%second, GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%timesec,     GL_METPROFILES%nt,0)

    call parallel_bcast(GL_METPROFILES%zavl,   GL_METPROFILES%nz*GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%p,      GL_METPROFILES%nz*GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%t,      GL_METPROFILES%nz*GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%tp,     GL_METPROFILES%nz*GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%tv,     GL_METPROFILES%nz*GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%u,      GL_METPROFILES%nz*GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%v,      GL_METPROFILES%nz*GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%qv,     GL_METPROFILES%nz*GL_METPROFILES%nt,0)
    call parallel_bcast(GL_METPROFILES%rho,    GL_METPROFILES%nz*GL_METPROFILES%nt,0)
    !
    return
  end subroutine src_bcast_profiles
  !
  !-----------------------------------------
  !    subroutine src_check_profiles
  !-----------------------------------------
  !
  !>   @brief
  !>   Checks that the meteo profiles span the required time interval
  !
  subroutine src_check_profiles(MY_ESP, GL_METPROFILES, MY_ERR)
    implicit none
    !
    !>   @param MY_ESP      list of parameters defining Eruption Source Parameters
    !>   @param GL_METPROFILES  variables related to meteorological profiles
    !>   @param MY_ERR      error handler
    !
    type(ESP_PARAMS),    intent(IN   ) :: MY_ESP
    type(METEO_PROFILE), intent(IN   ) :: GL_METPROFILES
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_check_profiles'
    MY_ERR%message = ' '
    !
    !*** Check time consistency
    !
    if(GL_METPROFILES%timesec(1).gt.MY_ESP%start_time(1) .or. &
       GL_METPROFILES%timesec(GL_METPROFILES%nt).lt.MY_ESP%end_time(MY_ESP%ndt) ) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Inconsistency between time range and profiles'
       return
    end if
    !
    return
  end subroutine src_check_profiles
  !
  !-----------------------------------------
  !    subroutine src_interpolate_profile
  !-----------------------------------------
  !
  !>   @brief
  !>   Gets the wind profile at the current time by interpolating in time
  !
  subroutine src_interpolate_profile(timesec,GL_PLUMEPROF,GL_METPROFILES,MY_ERR)
    implicit none
    !
    !>   @param timesec           current time (s after 00UTC)
    !>   @param GL_PLUMEPROF      variables related to meteorological profiles at current time
    !>   @param GL_METPROFILES    variables related to meteorological profiles
    !>   @param MY_ERR            error handler
    !
    real(rp),            intent(IN   ) :: timesec
    type(METEO_PROFILE), intent(INOUT) :: GL_PLUMEPROF
    type(METEO_PROFILE), intent(IN   ) :: GL_METPROFILES
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    logical     :: found
    integer(ip) :: i,it,iz,nz
    real(rp)    :: s,ux,uy,angle,dTdz
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_interpolate_profile'
    MY_ERR%message = ' '
    !
    !*** Finds linear interpolation factor s
    !
    found = .false.
    i = 1
    do while(.not.found)
       if((timesec.ge.GL_METPROFILES%timesec(i)).and.(timesec.le.GL_METPROFILES%timesec(i+1))) then
          found = .true.
          it    = i
          s     = (GL_METPROFILES%timesec(i+1)-timesec)/ &
               (GL_METPROFILES%timesec(i+1)-GL_METPROFILES%timesec(i))
       end if
       if((.not.found).and.(i.eq.(GL_METPROFILES%nt-1))) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'Interpolation file not found'
          return
       end if
       i = i +1
    end do
    !
    !*** Interpolate profile in time
    !
    do iz = 1,GL_METPROFILES%nz
       GL_PLUMEPROF%zavl(iz,1) = s*GL_METPROFILES%zavl(iz,it) + (1.0_rp-s)*GL_METPROFILES%zavl(iz,it+1)
       GL_PLUMEPROF%u   (iz,1) = s*GL_METPROFILES%u   (iz,it) + (1.0_rp-s)*GL_METPROFILES%u   (iz,it+1)
       GL_PLUMEPROF%v   (iz,1) = s*GL_METPROFILES%v   (iz,it) + (1.0_rp-s)*GL_METPROFILES%v   (iz,it+1)
       GL_PLUMEPROF%t   (iz,1) = s*GL_METPROFILES%t   (iz,it) + (1.0_rp-s)*GL_METPROFILES%t   (iz,it+1)
       GL_PLUMEPROF%p   (iz,1) = s*GL_METPROFILES%p   (iz,it) + (1.0_rp-s)*GL_METPROFILES%p   (iz,it+1)
       GL_PLUMEPROF%qv  (iz,1) = s*GL_METPROFILES%qv  (iz,it) + (1.0_rp-s)*GL_METPROFILES%qv  (iz,it+1)
       GL_PLUMEPROF%rho (iz,1) = s*GL_METPROFILES%rho (iz,it) + (1.0_rp-s)*GL_METPROFILES%rho (iz,it+1)
       !
       GL_PLUMEPROF%zasl(iz,1) = GL_PLUMEPROF%zo + GL_PLUMEPROF%zavl(iz,1)
    end do

    !
    !*** Computes other variables not given by GL_METPROFILES
    !
    nz = GL_METPROFILES%nz
    do iz = 1,nz
       !
       ux = GL_PLUMEPROF%u(iz,1)
       uy = GL_PLUMEPROF%v(iz,1)
       !
       !*** Vair
       !
       GL_PLUMEPROF%Vair(iz,1) = sqrt(ux*ux+uy*uy)
       !
       !*** Aair
       !
       if(abs(ux).gt.1d-8) then
          angle = atan2(uy,ux)*180.0_rp/PI
       else
          if(uy.gt.1d-8) then
             angle = 90.0_rp
          else if(uy.lt.-1d-8) then
             angle = 270.0_rp
          else
             angle = 0.0_rp
          end if
       end if
       !
       GL_PLUMEPROF%Aair(iz,1) = angle
       if(GL_PLUMEPROF%Aair(iz,1).lt.0.0_rp) &
            GL_PLUMEPROF%Aair(iz,1) = 360.0_rp + GL_PLUMEPROF%Aair(iz,1)   ! Angle in deg. (0 to 360)
       GL_PLUMEPROF%Aair(iz,1) = GL_PLUMEPROF%Aair(iz,1)*PI/180.0_rp     ! Angle in Rad. (0 to 2pi)
       !
       !*** Nair; buoyancy frequency (squared)
       !
       if(iz.eq.1) then
          dTdz = (GL_PLUMEPROF%t(2,1)-GL_PLUMEPROF%t(1,1))/(GL_PLUMEPROF%zavl(2,1)-GL_PLUMEPROF%zavl(1,1))
       else if (iz.eq.nz) then
          dTdz = (GL_PLUMEPROF%t(nz,1)-GL_PLUMEPROF%t(nz-1,1))/(GL_PLUMEPROF%zavl(nz,1)-GL_PLUMEPROF%zavl(nz-1,1))
       else
          dTdz = (GL_PLUMEPROF%t(iz+1,1)-GL_PLUMEPROF%t(iz-1,1))/(GL_PLUMEPROF%zavl(iz+1,1)-GL_PLUMEPROF%zavl(iz-1,1))
       end if
       !
       GL_PLUMEPROF%Nair(iz,1) = GI*GI*(1+CA0*dTdz/GI)/(CA0*GL_PLUMEPROF%t(1,1))
       !LAM: quick fix for negative Nair. To be reviewed!
       if(GL_PLUMEPROF%Nair(iz,1).lt.0) GL_PLUMEPROF%Nair(iz,1) = 0.0
       !
    end do
    !
    return
  end subroutine src_interpolate_profile
  !
  !-----------------------------------------
  !    subroutine src_get_mer_wind
  !-----------------------------------------
  !
  !>   @brief
  !>    Obtain M0 depending on plume height, wind profile and vent conditions
  !
  subroutine src_get_mer_wind(MER_vs_h,GL_PLUMEPROF,HPlume,w0,T0,M0,alfa,beta,MY_ERR)
    implicit none
    !
    !>   @param MER_vs_h          parameterization to derive MER from h (NONE / ESTIMATE-MASTIN / ESTIMATE-WOODHOUSE)
    !>   @param GL_PLUMEPROF      variables related to meteorological profiles at current time
    !>   @param HPlume            eruption column height (a.v.l.)
    !>   @param w0                water vapor mass fraction at vent
    !>   @param T0                mixture temperature       at vent
    !>   @param M0                computed mass flow rate   at vent
    !>   @param alfa              radial entrainment coefficient
    !>   @param beta              wind entrainment coefficient
    !>   @param MY_ERR            error handler
    !
    character(s_name),   intent(IN   ) :: MER_vs_h
    type(METEO_PROFILE), intent(IN   ) :: GL_PLUMEPROF
    real(rp),            intent(IN   ) :: HPlume
    real(rp),            intent(IN   ) :: w0
    real(rp),            intent(IN   ) :: T0
    real(rp),            intent(INOUT) :: M0
    real(rp),            intent(IN   ) :: alfa
    real(rp),            intent(IN   ) :: beta
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: iz,jz
    real   (rp) :: Ta,rhoa,z1,gprime
    real   (rp) :: cte,v_mean,N_mean,H1,V1,a,b,c
    real   (rp) :: fW,N3,H4,W
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_get_mer_wind'
    MY_ERR%message = ' '
    !
    Ta      = GL_PLUMEPROF%t  (1,1)  ! air temperature at vent
    rhoa    = GL_PLUMEPROF%rho(1,1)  ! air density     at vent
    !
    gprime  = GI*((CV0*w0+CP0*(1.0_rp-w0))*T0-CA0*Ta)/(rhoa*CA0*Ta)
    !
    !*** Wind speed and buoyancy frequency averaged over the plume height
    !
    v_mean = 0.0_rp
    N_mean = 0.0_rp
    jz     = 2
    do iz = 2,GL_PLUMEPROF%nz
       if(GL_PLUMEPROF%zavl(iz-1,1).le.HPlume) then
          v_mean = v_mean + 0.5_rp*(GL_PLUMEPROF%Vair(iz-1,1)+GL_PLUMEPROF%Vair(iz,1))* &
               (GL_PLUMEPROF%zavl(iz,1)-GL_PLUMEPROF%zavl(iz-1,1))
          N_mean = N_mean + 0.5_rp*(GL_PLUMEPROF%Nair(iz-1,1)+GL_PLUMEPROF%Nair(iz,1))* &
               (GL_PLUMEPROF%zavl(iz,1)-GL_PLUMEPROF%zavl(iz-1,1))
          jz = iz
       end if
    end do
    V1     = GL_PLUMEPROF%Vair(jz,1)                         ! reference velocity
    H1     = GL_PLUMEPROF%zavl(jz,1)-GL_PLUMEPROF%zavl(1,1)  ! reference height
    v_mean = v_mean/H1
    N_mean = N_mean/H1
    N_mean = sqrt(N_mean)
    !
    !*** Definition of the function f(W) accounting for wind effect and model constants
    !
    SELECT CASE(MER_vs_h)
    case('ESTIMATE-DEGRUYTER')
       !
       z1  = 2.8_rp                            ! maximum non-dimensional height (Morton et al. 1956)
       cte = 2.0_rp**(2.5_rp)*PI/(z1**4.0_rp)
       !
       !*** Estimates fW as in Degruyter and Bonadonna (2012)
       !
       !LAM: check it
       if(N_mean.gt.0.0) then
           W  = v_mean/(N_mean*H1)
           fW = ((z1**4.0_rp)*beta*beta*W)/((2.0_rp**2.5_rp)*6.0_rp*alfa*alfa)
           fW = 1.0_rp + fW
       else
           fW = 0.0
       end if
       !
    case('ESTIMATE-WOODHOUSE')
       !
       z1  = 2.67_rp                           ! maximum non-dimensional height (Woodhouse et al. 2013)
       cte = 2.0_rp**(2.5_rp)*PI/(z1**4.0_rp)
       !
       !*** Estimates fW as in Woodhouse et al. (2016)
       !
       !a = 1.373_rp   ! old values as in Woodhouse et al. (2013)
       !b = 4.266_rp
       !c = 0.3527_rp
       !
       a  = 0.87_rp + 0.05_rp*beta/alfa
       b  = 1.09_rp + 0.32_rp*beta/alfa
       c  = 0.06_rp + 0.03_rp*beta/alfa
       !
       if(N_mean.gt.0.0) then
           W  = 1.44_rp*V1/(N_mean*H1)
           fW = (1.0_rp+b*W+c*W*W) /(1.0_rp+a*W)
           fW = fW**4.0_rp
       else
           fW = 0.0
       end if
       !
    case default
       !
       M0 = 0.0_rp
       return
    end select
    !
    !*** MER
    !
    N3 = N_mean*N_mean*N_mean
    H4 = HPlume*HPlume*HPlume*HPlume
    M0 = cte*alfa*alfa*N3*H4*fW/gprime
    !
    return
  end subroutine src_get_mer_wind
  !
  !-----------------------------------------
  !    subroutine src_solve_point
  !-----------------------------------------
  !
  !>   @brief
  !>   Point source solution
  !
  subroutine src_solve_point(M0,Havl,MY_ESP,MY_GRN,GL_SRC,MY_ERR)
    implicit none
    !
    !>   @param M0        mass flow rate at vent
    !>   @param Havl      source point heigh (in m a.v.l.)
    !>   @param MY_ESP    list of parameters defining Eruption Source Parameters
    !>   @param MY_GRN    list of parameters defining granulometry
    !>   @param GL_SRC    list of parameters defining a source term
    !>   @param MY_ERR    error handler
    !
    real(rp),            intent(IN   ) :: M0
    real(rp),            intent(IN   ) :: Havl
    type(ESP_PARAMS),    intent(IN   ) :: MY_ESP
    type(BIN_PARAMS),    intent(IN   ) :: MY_GRN
    type(SRC_PARAMS),    intent(INOUT) :: GL_SRC
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: nbins
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_solve_point'
    MY_ERR%message = ' '
    !
    !*** Source position
    !
    GL_SRC%x(:) = MY_ESP%lon
    GL_SRC%y(:) = MY_ESP%lat
    GL_SRC%z(:) = MY_ESP%zo + Havl  ! a.s.l.
    !
    !***  MFR
    !
    nbins = GL_SRC%nbins
    GL_SRC%M(1:nbins,1) = M0*MY_GRN%bin_fc(1:nbins)
    !
    return
  end subroutine src_solve_point
  !
  !-----------------------------------------
  !    subroutine src_solve_suzuki
  !-----------------------------------------
  !
  !>   @brief
  !>   Suzuki source solution
  !
  subroutine src_solve_suzuki(M0,Havl,As,Ls,MY_ESP,MY_GRN,GL_SRC,MY_ERR)
    implicit none
    !
    !>   @param M0        mass flow rate at vent
    !>   @param Havl      source point heigh (in m a.v.l.)
    !>   @param As        A-Suzuki parameter
    !>   @param Ls        L-Suzuki parameter
    !>   @param MY_ESP    list of parameters defining Eruption Source Parameters
    !>   @param MY_GRN    list of parameters defining granulometry
    !>   @param GL_SRC    list of parameters defining a source term
    !>   @param MY_ERR    error handler
    !
    real(rp),            intent(IN   ) :: M0
    real(rp),            intent(IN   ) :: Havl
    real(rp),            intent(IN   ) :: As
    real(rp),            intent(IN   ) :: Ls
    type(ESP_PARAMS),    intent(IN   ) :: MY_ESP
    type(BIN_PARAMS),    intent(IN   ) :: MY_GRN
    type(SRC_PARAMS),    intent(INOUT) :: GL_SRC
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: nbins,np,is,ic
    real(rp)              :: msum,deltaz,z,z2
    real(rp), allocatable :: S(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_solve_suzuki'
    MY_ERR%message = ' '
    !
    nbins = GL_SRC%nbins
    np    = GL_SRC%np
    !
    !*** Horizontal source position
    !
    GL_SRC%x(:) = MY_ESP%lon
    GL_SRC%y(:) = MY_ESP%lat
    !
    !*** Vertical position and total mass according to Suzuki distribution
    !
    allocate(S(np))
    msum    = 0.0_rp
    deltaz = Havl/np   ! from the ground to Havl
    do is = 1,np
       z            = is*deltaz
       z2           = max(1.0_rp-z/Havl,0.0_rp)
       S(is)        = z2*exp(-As*z2)
       S(is)        = S(is)**Ls
       GL_SRC%z(is) = MY_ESP%zo + z   ! a.s.l.
       msum         = msum + S(is)
    end do
    !
    !*** Normalization to MFR (SUMz=MFR)
    !
    if(msum>0) then
        S(1:np) = M0*S(1:np)/msum
    else
        S(1:np) = 0.0
    end if
    !
    !*** Mass distribution
    !
    do is = 1,np
       do ic = 1,nbins
          GL_SRC%M(ic,is) = MY_GRN%bin_fc(ic)*S(is)
       end do
    end do
    !
    return
  end subroutine src_solve_suzuki
  !
  !-----------------------------------------
  !    subroutine src_solve_hat
  !-----------------------------------------
  !
  !>   @brief
  !>   Top hat source solution
  !
  subroutine src_solve_hat(M0,Havl,Th,MY_ESP,MY_GRN,GL_SRC,MY_ERR)
    implicit none
    !
    !>   @param M0        mass flow rate at vent
    !>   @param Havl      source point heigh (in m a.v.l.)
    !>   @param Th        Hat thickness
    !>   @param MY_ESP    list of parameters defining Eruption Source Parameters
    !>   @param MY_GRN    list of parameters defining granulometry
    !>   @param GL_SRC    list of parameters defining a source term
    !>   @param MY_ERR    error handler
    !
    real(rp),            intent(IN   ) :: M0
    real(rp),            intent(IN   ) :: Havl
    real(rp),            intent(IN   ) :: Th
    type(ESP_PARAMS),    intent(IN   ) :: MY_ESP
    type(BIN_PARAMS),    intent(IN   ) :: MY_GRN
    type(SRC_PARAMS),    intent(INOUT) :: GL_SRC
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: nbins,np,is,ic
    real(rp)              :: deltaz
    real(rp), allocatable :: S(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_solve_hat'
    MY_ERR%message = ' '
    !
    nbins = GL_SRC%nbins
    np    = GL_SRC%np
    !
    !*** Horizontal source position
    !
    GL_SRC%x(:) = MY_ESP%lon
    GL_SRC%y(:) = MY_ESP%lat
    !
    !*** Vertical position and total mass according to hat distribution
    !
    allocate(S(np))
    deltaz = Th/(np-1)   ! from the hat bottom to Havl
    do is = 1,np
       GL_SRC%z(is) = MY_ESP%zo + Havl - Th + (is-1)*deltaz  ! a.s.l.
       S(is) = 1.0_rp
    end do
    !
    !*** Normalization to MFR (SUMz=MFR)
    !
    S(1:np) = M0*S(1:np)/np
    !
    !*** Mass distribution
    !
    do is = 1,np
       do ic = 1,nbins
          GL_SRC%M(ic,is) = MY_GRN%bin_fc(ic)*S(is)
       end do
    end do
    !
    return
  end subroutine src_solve_hat
  !
  !-----------------------------------------
  !    subroutine src_solve_plume
  !-----------------------------------------
  !
  !>   @brief
  !>   Solves the 1-D radially-averaged equations for a plume
  !
  subroutine src_solve_plume(M0,Havl,idt,MY_TIME,MY_FILES,MY_ESP,MY_PLUME,MY_GRN,MY_AGR,GL_PLUMEPROF,MY_MOD,GL_SRC,MY_ERR)
    implicit none
    !
    !>   @param M0            mass flow rate at vent
    !>   @param Havl          plume heigh (in m a.v.l.)
    !>   @param idt           phase index
    !>   @param MY_TIME       RUN_TIME structure already filled
    !>   @param MY_FILES      list of files
    !>   @param MY_ESP        list of parameters defining Eruption Source Parameters
    !>   @param MY_PLUME      list of parameters defining the Plume Source Parameters
    !>   @param MY_GRN        list of parameters defining granulometry
    !>   @param MY_AGR        list of parameters defining an aggregation model
    !>   @param GL_PLUMEPROF  variables related to meteorological profiles at current time
    !>   @param MY_MOD        model physics related parameters
    !>   @param GL_SRC        list of parameters defining a source term
    !>   @param MY_ERR        error handler
    !
    real(rp),            intent(INOUT) :: M0
    real(rp),            intent(IN   ) :: Havl
    integer(ip),         intent(IN   ) :: idt
    type(RUN_TIME),      intent(IN   ) :: MY_TIME
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(ESP_PARAMS),    intent(IN   ) :: MY_ESP
    type(PLUME_PARAMS),  intent(IN   ) :: MY_PLUME
    type(BIN_PARAMS),    intent(IN   ) :: MY_GRN
    type(AGR_PARAMS),    intent(IN   ) :: MY_AGR
    type(METEO_PROFILE), intent(IN   ) :: GL_PLUMEPROF
    type(MODEL_PHYS),    intent(IN   ) :: MY_MOD
    type(SRC_PARAMS),    intent(INOUT) :: GL_SRC
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip), save :: ipass = 0
    integer(ip)       :: plume_status,plume_ns,nc,is,ic
    !
    real(rp)              :: R0,MER,x,y
    real(rp)              :: LonPlume,LatPlume
    real(rp), allocatable :: Xplum (:)       ! (ns)
    real(rp), allocatable :: Yplum (:)
    real(rp), allocatable :: Zplum (:)
    real(rp), allocatable :: Qplum (:)
    real(rp), allocatable :: Eplum (:)
    real(rp), allocatable :: Lplum (:)
    real(rp), allocatable :: Hplum (:)
    real(rp), allocatable :: Uplum (:)
    real(rp), allocatable :: Tplum (:)
    real(rp), allocatable :: Dplum (:)
    real(rp), allocatable :: Rplum (:)
    real(rp), allocatable :: Mair  (:)
    real(rp), allocatable :: Mwplum(:)
    real(rp), allocatable :: D_rhoa_plum(:)
    real(rp), allocatable :: plume_xv  (:)    ! water vapor  mass fraction
    real(rp), allocatable :: plume_xl  (:)    ! liquid water mass fraction
    real(rp), allocatable :: plume_xs  (:)    ! ice (solid)  mass fraction
    real(rp), allocatable :: plume_as  (:)    ! a_shear
    real(rp), allocatable :: plume_av  (:)    ! a_vortex
    real(rp), allocatable :: Mplum (:,:)     ! Particle mass flow rate (nc,ns)
    real(rp), allocatable :: Mfplum(:,:)     ! Mass flow rate of particles that fall from the eruption column (nc,ns)
    real(rp), allocatable :: Maplum(:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_solve_plume'
    MY_ERR%message = ' '
    !
    !*** Allocates memory
    !
    plume_ns = MY_PLUME%ns
    nc       = MY_GRN%nbins
    !
    allocate(Xplum (plume_ns))
    allocate(Yplum (plume_ns))
    allocate(Zplum (plume_ns))
    allocate(Qplum (plume_ns))
    allocate(Eplum (plume_ns))
    allocate(Lplum (plume_ns))
    allocate(Hplum (plume_ns))
    allocate(Uplum (plume_ns))
    allocate(Tplum (plume_ns))
    allocate(Dplum (plume_ns))
    allocate(Rplum (plume_ns))
    allocate(Mwplum(plume_ns))
    allocate(Mair  (plume_ns))
    allocate(D_rhoa_plum(plume_ns))
    allocate(plume_xv   (plume_ns))
    allocate(plume_xl   (plume_ns))
    allocate(plume_xs   (plume_ns))
    allocate(plume_as   (plume_ns))
    allocate(plume_av   (plume_ns))
    allocate(Mplum (nc,plume_ns))
    allocate(Mfplum(nc,plume_ns))
    allocate(Maplum(nc,plume_ns))
    !
    Xplum  = 0.0_rp
    Yplum  = 0.0_rp
    Zplum  = 0.0_rp
    Qplum  = 0.0_rp
    Eplum  = 0.0_rp
    Lplum  = 0.0_rp
    Hplum  = 0.0_rp
    Tplum  = 0.0_rp
    Dplum  = 0.0_rp
    Rplum  = 0.0_rp
    Mwplum = 0.0_rp
    Mair   = 0.0_rp
    D_rhoa_plum = 0.0_rp
    plume_xv = 0.0_rp
    plume_xl = 0.0_rp
    plume_xs = 0.0_rp
    plume_as = 0.0_rp
    plume_av = 0.0_rp
    Mplum (:,:) = 0.0_rp
    Mfplum(:,:) = 0.0_rp
    Maplum(:,:) = 0.0_rp
    !
    !*** Initialize plume data
    !
    if (ipass == 0) then
       ipass = 1
       !
       call plumeBPT_initialize_plume( &
            MY_GRN%nbins,         &   !  integer              number of bins
            MY_GRN%bin_fc,        &   !  real  (nc)           class mass fraction
            MY_GRN%bin_diam,      &   !  real  (nc)           class diameter
            MY_GRN%bin_rho,       &   !  real  (nc)           class density
            MY_GRN%bin_psi,       &   !  real  (nc)           class shape factor (depending on velocity model)
            MY_AGR%vset_fac,      &   !  real                 Factor multiplying setling velocity of aggregates
            MY_AGR%diam_aggr,     &   !  real                 Aggregate diameter
            MY_AGR%Dfo,           &   !  real                 Fractal exponent for aggregation
                                !
            CW0,                  &   !  real                 Specific heat of water      at constant pressure
            CP0,                  &   !  real                 Specific heat of pyroclasts at constant pressure
            CA0,                  &   !  real                 Specific heat of air        at constant pressure
                                !
            MY_PLUME%ns,          &   !  integer
            MY_PLUME%np,          &   !  integer
            MY_MOD%modv,          &   !  integer              Terminal velocity model
            MY_PLUME%xv_UTM,      &   !  real                 Vent UTM-X coordinate
            MY_PLUME%yv_UTM,      &   !  real                 Vent UTM-y coordinate
            MY_PLUME%zv,          &   !  real                 Vent altitude (m a.s.l.)
            MY_PLUME%n_MFR,       &   !  real   (2)           MER search range
            MY_PLUME%xi,          &   !  real                 Factor (Bursik 2001).
            MY_PLUME%zmin_wind,   &   !  real                 Ignore wind entrainment below this zvalue (low jet region)
            MY_PLUME%c_umbrella,  &   !  real                 Thickness of umbrella relative to Hb (>1)
            MY_PLUME%a_s_jet,     &   !  real                 Default (constant) value in jet   region
            MY_PLUME%a_s_plume,   &   !  real                 Default (constant) value in plume region
            MY_PLUME%a_v,         &   !  real                 Default (constant) value
            MY_AGR%aggregation,   &   !  logical
            MY_PLUME%moist_air,   &   !  logical
            MY_PLUME%wind_coupling,   &   !  logical
            MY_PLUME%reentrainment,   &   !  logical
            MY_PLUME%latent_heat,     &   !  logical
            MY_AGR%aggregation_model, &   !  character(s_name)    'NONE'/'CORNELL'/'PERCENTAGE'/'COSTA'
            MY_PLUME%solve_plume_for, &   !  character(s_name)     Plume solving strategy
            MY_PLUME%type_as,         &   !  character(s_name)     Default parameterization type for as
            MY_PLUME%type_av,         &   !  character(s_name)     Default parameterization type for av
            MY_PLUME%umbrella_model,  &   !  character(s_name)     Default umbrella model
            MY_ERR )
    end if
    !
    !*** Initialize wind profile
    !
    call plumeBPT_initialize_wind(&
         GL_PLUMEPROF%nz,          &   !  integer                 Number of z-layers
         GL_PLUMEPROF%zasl(:,1),   &   !  real     (profile_nz)   Height of layers (a.s.l.)
         GL_PLUMEPROF%rho (:,1),   &   !  real     (profile_nz)   Density
         GL_PLUMEPROF%p   (:,1),   &   !  real     (profile_nz)   Pressure
         GL_PLUMEPROF%t   (:,1),   &   !  real     (profile_nz)   Temperature
         GL_PLUMEPROF%qv  (:,1),   &   !  real     (profile_nz)   Specific humidity
         GL_PLUMEPROF%Vair(:,1),   &   !  real     (profile_nz)   Wind speed
         GL_PLUMEPROF%Aair(:,1),   &   !  real     (profile_nz)   Wind direction in Rad
         MY_ERR )
    !
    !*** Solve plume equations
    !
    call plumeBPT_solve_plume(&
                                !                      Input variables
         Havl,                 &   ! HPlume
         M0,                   &   ! M0
         MY_PLUME%u0_dt(idt),  &   ! u0
         MY_ESP%T0_dt(idt),    &   ! T0
         MY_PLUME%Tv_dt(idt),  &   ! Tv0
         MY_PLUME%Tl_dt(idt),  &   ! Tl0
         MY_PLUME%Ts_dt(idt),  &   ! Ts0
         MY_PLUME%wv_dt(idt),  &   ! xv0
         MY_PLUME%wl_dt(idt),  &   ! xl0
         MY_PLUME%ws_dt(idt),  &   ! xs0
         plume_ns,             &
                                !                      Output variables
         plume_status,       &   ! Exit status
         R0,                 &   ! Vent radius
         MER,                &   ! MER
         Xplum,              &   ! x-coord(ns)
         Yplum,              &   ! y-coord(ns)
         Zplum,              &   ! z-coord(ns) (elevation in m a.s.l.)
         Qplum,              &   ! Bulk mass flow rate (ns)
         Eplum,              &   ! Total energy flow rate (ns)
         Mplum,              &   ! Particle mass flow rate (nc,ns)
         Mfplum,             &   ! Mass flow rate of particles that fall from the eruption column (nc,ns)
         Maplum,             &   ! Mass flow rate of particles that aggregate (nc,ns)
         Mair,               &   ! Mass flow rate of air (ns)
         Mwplum,             &   ! Mass flow rate of volatiles (vapor + liquid + ice) (ns)
         Lplum,              &   ! Coordinate s (along the plume centerline)
         Hplum,              &   ! Theta angle
         Uplum,              &   ! Bulk velocity
         Tplum,              &   ! Bulk temperature
         Dplum,              &   ! Bulk density
         D_rhoa_plum,        &   ! Bulk density/air density
         Rplum,              &   ! Plume radius
         plume_xv,           &   ! water vapor  mass fraction
         plume_xl,           &   ! liquid water mass fraction
         plume_xs,           &   ! ice (solid)  mass fraction
         plume_as,           &   ! a_shear
         plume_av,           &   ! a_vortex
         MY_ERR)
    !
    !*** Check exis status
    !
    if(plume_status == STATUS_OK) then
       continue
    else if(plume_status == STATUS_COLLAPSE) then
       call task_wriwarn(MY_ERR,'Plume collapse')
    else if(plume_status == STATUS_ERROR) then
       MY_ERR%flag    = -1
       MY_ERR%message = 'Error when solving the plume equations'
       call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
    else
       call task_wriwarn(MY_ERR,'Unidentified plume_status value; some error may occur')
    end if
    !
    !*** Convert coordinates from UTM to LON-LAT
    !
    do is = 1,plume_ns
       x = Xplum(is)
       y = Yplum(is)
       call coord_utm2ll (x,y,MY_PLUME%zone_UTM,LonPlume,LatPlume,WGS_84_DATUM,MY_ERR)
       if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
       !
       GL_SRC%x(is) = LonPlume
       GL_SRC%y(is) = LatPlume
       GL_SRC%z(is) = Zplum(is)
    end do
    !
    !*** Writes plume results for this time slab
    !
    if(master_model) call plumeBPT_write_plumeprop(&
         MY_FILES,MY_TIME,MY_PLUME,MY_ESP,MY_GRN,MY_AGR,MY_MOD,MY_ERR)
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
    !
    !*** Finally, store the mass that falls
    !*** (compatibility in write source with other types of source term)
    !
    do is = 1,plume_ns
       do ic = 1,nc
          GL_SRC%M(ic,is) = Mfplum(ic,is)
       end do
    end do
    !
    return
  end subroutine src_solve_plume
  !
  !-----------------------------------------
  !    subroutine src_write_source
  !-----------------------------------------
  !
  !>   @brief
  !>   writes the source file during a time step (ibeg,iend) for the effective bins
  !
  subroutine src_write_source(ibeg,iend,MY_GRN,GL_SRC,MY_FILES,MY_ERR)
    implicit none
    !
    !>   @param ibeg      starting time (in s)
    !>   @param end       end      time (in s)
    !>   @param MY_GRN    list of parameters defining granulometry
    !>   @param GL_SRC    list of parameters defining a source term
    !>   @param MY_FILES  list of files
    !>   @param MY_ERR    error handler
    !
    integer(ip),         intent(IN   ) :: ibeg
    integer(ip),         intent(IN   ) :: iend
    type(BIN_PARAMS),    intent(IN   ) :: MY_GRN
    type(SRC_PARAMS),    intent(IN   ) :: GL_SRC
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: lusrc
    integer(ip)           :: np,nc,ic,is,ibin
    real(rp)              :: MFR,x,y,z
    real(rp), allocatable :: work(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_write_source'
    MY_ERR%message = ' '
    !
    lusrc = MY_FILES%lusrc
    !
    !*** Gets the number of point sources and effective bins
    !
    np = GL_SRC%np
    nc = 0
    do ibin = 1,MY_GRN%nbins
       if(MY_GRN%bin_effe(ibin)) nc = nc + 1
    end do
    !
    !*** Gets the total mass flow rate
    !
    MFR = SUM(GL_SRC%M(:,:))
    !
    !*** Writes header for the current interval
    !
    write(lusrc,10) ibeg,iend
    write(lusrc,11) np,nc
    write(lusrc,12) MFR
10  format(i7,1x,i7)
11  format(i7,1x,i7)
12  format(e16.9)
    !
    !*** Writes the rest of file
    !
    allocate(work(nc))
    do is = 1,np
       x = GL_SRC%x(is)
       y = GL_SRC%y(is)
       z = GL_SRC%z(is)      ! a.s.l.

       ic = 0
       do ibin = 1,MY_GRN%nbins
          if(MY_GRN%bin_effe(ibin)) then
             ic = ic + 1
             work(ic) = GL_SRC%M(ibin,is)
          end if
       end do
       write(lusrc,20) x,y,z,(work(ic),ic=1,nc)
20     format(2(1x,f11.6),2x,f9.0,2x,100(e16.9,1x))
    end do
    !
    return
  end subroutine src_write_source
  !
  !-----------------------------------------
  !    subroutine src_read_source
  !-----------------------------------------
  !
  !>   @brief
  !>   reads the source for a given time step (ibeg,iend)
  !
  subroutine src_read_source(MY_FILES,MY_TIME,timesec,GL_SRC,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TIME   RUN_TIME structure already filled
    !>   @param timesec   current time (in sec after 00UTC). NOTE: can differ from MY_TIME%time
    !>   @param GL_SRC    list of parameters defining a source term
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(RUN_TIME),      intent(IN   ) :: MY_TIME
    real(rp),            intent(IN   ) :: timesec
    type(SRC_PARAMS),    intent(INOUT) :: GL_SRC
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: fname
    character(len=8     ) :: str
    logical     :: go_on
    integer(ip) :: time,start_time,end_time,np,nbins
    integer(ip) :: ibin,i
    real(rp)    :: MFR,rvoid
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_read_source'
    MY_ERR%message = ' '
    !
    fname = MY_FILES%file_src
    time  = INT(timesec)
    !
    GL_SRC%start_time = 0
    GL_SRC%end_time   = 0
    GL_SRC%np         = 0
    GL_SRC%nbins      = 0
    GL_SRC%total_MFR  = 0.0_rp
    !
    !*** Opens the file
    !
    open(90,FILE=fname(1:LEN_TRIM(fname)),STATUS='unknown',ERR=100)
    !
    !*** Reads until the required time
    !
    go_on = .true.
    do while(go_on)
       !
       read(90,*,ERR=101,END=10) start_time,end_time
       read(90,*,ERR=101)        np,nbins
       read(90,*,ERR=101)        MFR
       !
       if(time.lt.start_time) then !  x (----)
          go_on = .false.
          GL_SRC%end_time = start_time
          !
       else if((time.ge.start_time).and.(time.lt.end_time)) then  ! (--x--)
          go_on = .false.
          !
          GL_SRC%start_time = start_time
          GL_SRC%end_time   = end_time
          GL_SRC%np         = np
          GL_SRC%nbins      = nbins
          GL_SRC%total_MFR  = MFR
          !
          allocate(GL_SRC%x(np))
          allocate(GL_SRC%y(np))
          allocate(GL_SRC%z(np))
          allocate(GL_SRC%M(nbins,np))
          !
          do i = 1,np
             read(90,*,ERR=101) GL_SRC%x(i),GL_SRC%y(i),GL_SRC%z(i),(GL_SRC%M(ibin,i),ibin=1,nbins)
          end do
       else           ! (----) x
          !
          do i = 1,np
             read(90,*,ERR=101) rvoid
          end do
          !
       end if
       !
    end do
    !
    !*** Normal end
    !
    close(90)
    return
    !
    !*** Source time not found
    !
10  close(90)
    GL_SRC%end_time = INT(MY_TIME%run_end)
    return
    !
    !*** List of errors
    !
100 MY_ERR%flag    = 1
    MY_ERR%message ='error opening the source file '//TRIM(fname)
    return
101 MY_ERR%flag    = 1
    write(str,'(i8)') INT(time)
    MY_ERR%message ='error reading the source file at time '//TRIM(str)
    return
  end subroutine src_read_source
  !
  !-----------------------------------------
  !    subroutine src_bcast_source
  !-----------------------------------------
  !
  !>   @brief
  !>   Broadcasts all the source time intervals
  !
  subroutine src_bcast_source(GL_SRC,MY_ERR)
    implicit none
    !
    !>   @param GL_SRC    variables related to each source term interval
    !>   @param MY_ERR    error handler
    !
    type(SRC_PARAMS),    intent(INOUT) :: GL_SRC
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: np,nbins
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'src_bcast_source'
    MY_ERR%message = ' '
    !
    call parallel_bcast(GL_SRC%start_time,1,0)
    call parallel_bcast(GL_SRC%end_time  ,1,0)
    call parallel_bcast(GL_SRC%np        ,1,0)
    call parallel_bcast(GL_SRC%nbins     ,1,0)
    call parallel_bcast(GL_SRC%total_MFR ,1,0)
    !
    np    = GL_SRC%np
    nbins = GL_SRC%nbins
    !
    !*** Memory allocation
    !
    if(GL_SRC%np.gt.0) then
      if(.not.master_model) then
         allocate(GL_SRC%x(np))
         allocate(GL_SRC%y(np))
         allocate(GL_SRC%z(np))
         allocate(GL_SRC%M(nbins,np))
      end if
      !
      call parallel_bcast(GL_SRC%x,np      ,0)
      call parallel_bcast(GL_SRC%y,np      ,0)
      call parallel_bcast(GL_SRC%z,np      ,0)
      call parallel_bcast(GL_SRC%M,nbins*np,0)
    end if
    !
    return
  end subroutine src_bcast_source
  !
  !
  !
END MODULE Src
