subroutine task_SetSrc
  !
  !------------------------------------
  !    subroutine task_SetSrc
  !------------------------------------
  !
  !>   @brief
  !>   Task for generation of source terms
  !>   @author
  !>   Arnau Folch
  !
  use KindType
  use Shared
  use InpOut
  use Parallel
  use Grn
  use Src
  use Time
  implicit none
  !
  type(BIN_PARAMS)    :: MY_GRN
  type(ESP_PARAMS)    :: MY_ESP
  type(PLUME_PARAMS)  :: MY_PLUME
  type(SRC_PARAMS)    :: GL_SRC
  type(METEO_PROFILE) :: GL_PLUMEPROF
  !
  logical     :: go_on
  integer(ip) :: idt, time1, time2, ibeg, iend
  real(rp)    :: M0, T0, w0, h, As, Ls, Th, erumass
  !
  !*** Initializations
  !
  MY_ERR%nwarn = 0
  call CPU_TIME(MY_ERR%cpu_start_time)
  !
  call inpout_get_filenames(TASK_SET_SRC,MY_FILES,MY_ERR)
  !
  !*** Master opens log file
  !
  if(master_model) call inpout_open_log_file(TASK_SET_SRC, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
  !
  !*** Master opens src file
  !
  if(master_model) call inpout_open_file(MY_FILES%lusrc, MY_FILES%file_src, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
  !
  !*** Master reads and broadcasts SPECIES block from input file (MY_SPE)
  !
  if(TASK_FLAG(TASK_SET_TGSD).eq.1) then
     !
     continue   ! already stored in MY_SPE
     !
  else
     !
     if(master_model) call grn_read_inp_species(MY_FILES, MY_SPE, MY_ERR)
     call parallel_bcast(MY_ERR%flag,1_ip,0_ip)
     if(MY_ERR%flag.eq.0) then
        call grn_bcast_species(MY_SPE,MY_ERR)
     else
        call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
     end if
  end if
  !
  !*** Master reads and broadcasts TIME_UTC block from input file
  !
  if(TASK_FLAG(TASK_SET_DBS).eq.1) then
     !
     continue   ! already stored in MY_TIME
     !
  else
     !
     if(master_model) call time_read_inp_time(MY_FILES, MY_TIME, MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
     !
     call time_bcast_inp_time(MY_TIME,MY_ERR)
  end if
  !
  !*** Master reads and broadcasts MODEL_PHYSICS block from input file (MY_MOD)
  !
  if(master_model) call phys_read_inp_model(MY_FILES, MY_MOD, MY_ENS, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  call phys_bcast_inp_model(MY_MOD,MY_ERR)
  !
  !*** Master reads and broadcasts SOURCE block from input file (MY_ESP)
  !
  if(master_model) call src_read_inp_source_params(MY_FILES, MY_TIME, MY_SPE, MY_ESP, MY_PLUME, MY_ENS, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
  !
  call src_bcast_source_params(MY_ESP,MY_PLUME,MY_ERR)
  !
  !*** Master reads and broadcasts PARTICLE_AGGREGATION block from input file (MY_AGR)
  !
  if(master_model) call grn_read_inp_aggregation(MY_FILES, MY_AGR, MY_ENS, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
  !
  call grn_bcast_aggregation(MY_AGR,MY_ERR)
  !
  !*** Master computes and broadcasts granulometry for each specie (MY_GRN)
  !
  if(master_model) call grn_get_granulometry(MY_FILES, MY_SPE, MY_MOD, MY_GRN, MY_AGR, MY_ESP, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
  !
  call grn_bcast_granulometry(MY_GRN, MY_ERR)
  !
  !*** Master writes the (effective) granulometry file
  !
  if(master_model) call grn_write_granulometry(MY_FILES, MY_GRN, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
  !
  !*** If necessary, Master reads and forecasts meteo profiles
  !
  if( (MY_ESP%meteo_coupling).and.(.not.GL_METPROFILES%exists) ) then
     !
     if(master_model) call src_read_profiles(MY_FILES, GL_METPROFILES, MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
     !
     call src_bcast_profiles(GL_METPROFILES,MY_ERR)
     !
  end if
  !
  !*** If necessary, checks that the meteo profiles span the required time interval
  !*** and allocates memory
  !
  if(MY_ESP%meteo_coupling) then
     call src_check_profiles(MY_ESP, GL_METPROFILES, MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
     !
     GL_PLUMEPROF%exists = .true.
     GL_PLUMEPROF%nz     = GL_METPROFILES%nz
     GL_PLUMEPROF%nt     = 1
     GL_PLUMEPROF%lon    = GL_METPROFILES%lon
     GL_PLUMEPROF%lat    = GL_METPROFILES%lat
     GL_PLUMEPROF%zo     = GL_METPROFILES%zo
     !
     allocate(GL_PLUMEPROF%zavl(GL_PLUMEPROF%nz,GL_PLUMEPROF%nt))
     allocate(GL_PLUMEPROF%zasl(GL_PLUMEPROF%nz,GL_PLUMEPROF%nt))
     allocate(GL_PLUMEPROF%u   (GL_PLUMEPROF%nz,GL_PLUMEPROF%nt))
     allocate(GL_PLUMEPROF%v   (GL_PLUMEPROF%nz,GL_PLUMEPROF%nt))
     allocate(GL_PLUMEPROF%t   (GL_PLUMEPROF%nz,GL_PLUMEPROF%nt))
     allocate(GL_PLUMEPROF%p   (GL_PLUMEPROF%nz,GL_PLUMEPROF%nt))
     allocate(GL_PLUMEPROF%qv  (GL_PLUMEPROF%nz,GL_PLUMEPROF%nt))
     allocate(GL_PLUMEPROF%rho (GL_PLUMEPROF%nz,GL_PLUMEPROF%nt))
     allocate(GL_PLUMEPROF%Vair(GL_PLUMEPROF%nz,GL_PLUMEPROF%nt))
     allocate(GL_PLUMEPROF%Aair(GL_PLUMEPROF%nz,GL_PLUMEPROF%nt))
     allocate(GL_PLUMEPROF%Nair(GL_PLUMEPROF%nz,GL_PLUMEPROF%nt))
     !
  end if
  !
  !*** Determines the number of source points and allocates memory for GL_SRC (work structure)
  !*** NOTE: in this version it is assumed that a point source contains all the species. This allows
  !*** for a single source file
  !
  GL_SRC%source_type = MY_ESP%source_type
  GL_SRC%nbins       = MY_GRN%nbins
  !
  SELECT CASE(GL_SRC%source_type)
  case('POINT')
     GL_SRC%np = 1
  case('SUZUKI','TOP-HAT')
     GL_SRC%np = 100
  case('PLUME')
     MY_PLUME%ns = 300         ! number of plume sources (plume+umbrella)
     MY_PLUME%np = 200         ! number of plume sources (with no umbrella)
     GL_SRC%np   = MY_PLUME%ns
  case('RESUSPENSION')
     MY_ERR%message = 'source not yet implemented'
     call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
  END SELECT
  !
  allocate(GL_SRC%x(GL_SRC%np))
  allocate(GL_SRC%y(GL_SRC%np))
  allocate(GL_SRC%z(GL_SRC%np))
  allocate(GL_SRC%M(GL_SRC%nbins,GL_SRC%np))
  !
  !*** Writes log file header
  !
  if(master_model) then
     select case(MY_ESP%source_type)
     case('POINT','SUZUKI','TOP-HAT')
        write(MY_FILES%lulog,20)
20      format('  From time   To time      MER     Column height  Column height     Mass   ',/,&
               '     (s)        (s)      (kg/s)      (m a.v.l.)     (m a.s.l)       (kg)   ',/,&
               '  -------------------------------------------------------------------------')
     case('PLUME')
        write(MY_FILES%lulog,30)
30      format('  From time   To time      MER     Column height  Column height     Mass   ',/,&
               '     (s)        (s)      (kg/s)    NBL (m a.s.l.) Total (m a.s.l)   (kg)   ',/,&
               '  -------------------------------------------------------------------------')
     end select
  end if
  !
  !*** Loop over soucre time slabs
  !
  erumass = 0.0_rp
  !
  do idt = 1,MY_ESP%ndt
     !
     ibeg = MY_ESP%start_time(idt)
     iend = MY_ESP%end_time  (idt)
     !
     SELECT CASE(MY_ESP%source_type)
     case('POINT','SUZUKI','TOP-HAT')
        !
        !***  1. POINT, SUZUKI and TOP-HAT cases
        !
        if(.not.MY_ESP%meteo_coupling) then
           !
           !*** No plume-wind coupling
           !
           M0 = MY_ESP%M0_dt(idt)
           H  = MY_ESP%h_dt (idt)
           !
           SELECT CASE(MY_ESP%source_type)
           case('POINT')
              call src_solve_point(M0,H,MY_ESP,MY_GRN,GL_SRC,MY_ERR)
           case('SUZUKI')
              As = MY_ESP%As_dt(idt)
              Ls = MY_ESP%Ls_dt(idt)
              call src_solve_suzuki(M0,H,As,Ls,MY_ESP,MY_GRN,GL_SRC,MY_ERR)
           case('TOP-HAT')
              Th = MY_ESP%Th_dt(idt)
              call src_solve_hat(M0,H,Th,MY_ESP,MY_GRN,GL_SRC,MY_ERR)
           END SELECT
           !
           !*** Write log file
           !
           erumass = erumass + (iend-ibeg)*M0
           if(master_model) write(MY_FILES%lulog,1) ibeg,iend,M0,H,GL_SRC%z(GL_SRC%np),erumass
           !
           !*** Write the source file for this interval
           !
           if(master_model) call src_write_source(ibeg,iend,MY_GRN,GL_SRC,MY_FILES,MY_ERR)
           !
        else if(MY_ESP%meteo_coupling) then
           !
           !*** plume-wind coupling
           !
           go_on = .true.
           time1 = ibeg
           time2 = time1 + MY_ESP%meteo_coupling_interval
           time2 = min(time2,iend)
           !
           do while(go_on)
              !
              !*** Gets the wind profile at the current time by interpolating in time
              !
              call src_interpolate_profile(1.0_rp*time1,GL_PLUMEPROF,GL_METPROFILES,MY_ERR)
              !
              !*** Obtain M0 depending on plume height, wind profile and vent conditions
              !
              H  = MY_ESP%h_dt (idt)
              w0 = MY_ESP%w0_dt(idt)
              T0 = MY_ESP%T0_dt(idt)
              !
              call src_get_mer_wind(MY_ESP%MER_vs_h,GL_PLUMEPROF,H,w0,T0,M0,MY_ESP%alfa_plume,MY_ESP%beta_plume,MY_ERR)
              !
              SELECT CASE(MY_ESP%source_type)
              case('POINT')
                 call src_solve_point(M0,H,MY_ESP,MY_GRN,GL_SRC,MY_ERR)
              case('SUZUKI')
                 As = MY_ESP%As_dt(idt)
                 Ls = MY_ESP%Ls_dt(idt)
                 call src_solve_suzuki(M0,H,As,Ls,MY_ESP,MY_GRN,GL_SRC,MY_ERR)
              case('TOP-HAT')
                 Th = MY_ESP%Th_dt(idt)
                 call src_solve_hat(M0,H,Th,MY_ESP,MY_GRN,GL_SRC,MY_ERR)
              END SELECT
              !
              !*** Write log file
              !
              erumass = erumass + (time2-time1)*M0
              if(master_model) write(MY_FILES%lulog,1) time1,time2,M0,H,GL_SRC%z(GL_SRC%np),erumass
              !
              !*** Write the source file for this interval
              !
              if(master_model) call src_write_source(time1,time2,MY_GRN,GL_SRC,MY_FILES,MY_ERR)
              !
              !*** Update
              !
              time1 = time2
              time2 = time1 + MY_ESP%meteo_coupling_interval
              time2 = min(time2,iend)
              if(time1.eq.iend) go_on = .false.
              !
           end do
           !
        end if   ! if(MY_ESP%meteo_coupling)
        !
     case('PLUME')
        !
        !*** 2. PLUME case
        !
        go_on = .true.
        time1 = ibeg
        time2 = time1 + MY_ESP%meteo_coupling_interval
        time2 = min(time2,iend)
        !
        do while(go_on)
           !
           !*** Gets the wind profile at the current time by interpolating in time
           !
           call src_interpolate_profile(1.0_rp*time1,GL_PLUMEPROF,GL_METPROFILES,MY_ERR)
           !
           !*** Assign values
           !
           M0             = MY_ESP%M0_dt(idt)  ! can be modifyed in solveplume
           H              = MY_ESP%h_dt (idt)
           MY_PLUME%time1 = time1
           MY_PLUME%time2 = time2
           !
           call src_solve_plume(M0,H,idt,MY_TIME,MY_FILES,MY_ESP,MY_PLUME,MY_GRN,MY_AGR,GL_PLUMEPROF,MY_MOD,GL_SRC,MY_ERR)
           !
           !*** Write log file
           !
           erumass = erumass + (time2-time1)*M0
           if(master_model) write(MY_FILES%lulog,1) time1,time2,M0,GL_SRC%z(MY_PLUME%np),GL_SRC%z(MY_PLUME%ns),erumass
           !
           !*** Write the source file for this interval
           !
           if(master_model) call src_write_source(time1,time2,MY_GRN,GL_SRC,MY_FILES,MY_ERR)
           !
           !*** Update
           !
           time1 = time2
           time2 = time1 + MY_ESP%meteo_coupling_interval
           time2 = min(time2,iend)
           if(time1.eq.iend) go_on = .false.
           !
        end do
        !
     case('RESUSPENSION')
        !
        !*** 3. RESUSPENSION case
        !
        !
     END SELECT
     !
  end do  ! idt = 1,ndt
  !
  !*** Close src file
  !
  if(master_model) call inpout_close_file(MY_FILES%lusrc, MY_ERR)
  !
  !*** If necessary, saves the effective (not total) granulometry to tracers
  !
  if(TASK_FLAG(TASK_RUN_FALL3D).eq.1) call grn_save_granulometry(MY_GRN, MY_TRA, MY_ERR)
  !
  !*** Normal end
  !
  if(master_model) call inpout_close_log_file(TASK_SET_SRC, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_SRC, MY_FILES, MY_ERR)
  !
  !*** log file formats
  !
1 format(1x,i8,3x,i8,2x,e12.5,2x,f9.1,4x,f9.1,4x,e12.5)
  !
  return
end subroutine task_SetSrc
