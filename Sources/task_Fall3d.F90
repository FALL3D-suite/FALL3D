subroutine task_Fall3d
  !
  !------------------------------------
  !    subroutine task_Fall3d
  !------------------------------------
  !
  !>   @brief
  !>   Task 3-D Advection-Diffusion-Sedimentation model
  !>   @author
  !>   Arnau Folch
  !
  use KindType
  use Shared
  use InpOut
  use Grn
  use Time
  use Grid
  use Phys
  use Domain
  use Parallel
  use nc_IO
  use F3D
  use Dbs
  use Sat
  use Ensemble
  implicit none
  !
  logical  :: sourcetime,meteotime
  real(rp), allocatable :: my_hc  (:,:)
  !
  ! Class GL_METMODEL is needed by dbs_read_inp_meteo
  ! It is used by task_SetDbs, not used here
  type(METEO_MODEL) :: GL_METMODEL
  !
  !*** Initializations
  !
  MY_ERR%nwarn = 0
  call CPU_TIME(MY_ERR%cpu_start_time)
  !
  call inpout_get_filenames(TASK_RUN_FALL3D,MY_FILES,MY_ERR)
  !
  !*** Master opens log file
  !
  if(master_model) call inpout_open_log_file(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
  !
  !*** Master reads and broadcasts SPECIES block from input file (MY_SPE)
  !
  if( (TASK_FLAG(TASK_SET_TGSD).eq.1).or.(TASK_FLAG(TASK_SET_SRC).eq.1) ) then
     !
     continue   ! already stored in MY_SPE
     !
  else
     !
     if(master_model) call grn_read_inp_species(MY_FILES, MY_SPE, MY_ERR)
     call parallel_bcast(MY_ERR%flag,1_ip,0_ip)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     call grn_bcast_species(MY_SPE,MY_ERR)
  end if
  !
  !*** Master reads and broadcasts TIME_UTC block from input file
  !
  if( (TASK_FLAG(TASK_SET_DBS).eq.1).or.(TASK_FLAG(TASK_SET_SRC).eq.1) ) then
     !
     continue   ! already stored in MY_TIME
     !
  else
     !
     if(master_model) call time_read_inp_time(MY_FILES, MY_TIME, MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     call time_bcast_inp_time(MY_TIME,MY_ERR)
  end if
  !
  !*** Master reads and broadcasts METEO_DATA block from input file
  !
  if(TASK_FLAG(TASK_SET_DBS).eq.1) then
     !
     continue   ! already stored in MY_TIME and MY_MET
     !
  else
     !
     if(master_model) call dbs_read_inp_meteo(MY_FILES, MY_TIME, MY_MET, GL_METMODEL, GL_METPROFILES, MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     call dbs_bcast_inp_meteo(MY_FILES, MY_TIME, MY_MET, GL_METPROFILES, MY_ERR)
  end if
  !
  !*** Master reads and broadcasts GRID block from input file
  !
  if(TASK_FLAG(TASK_SET_DBS).eq.1) then
     !
     continue   ! already stored in MY_GRID
     !
  else
     !
     if(master_model) call grid_read_inp_grid(MY_FILES, MY_GRID, MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     call grid_bcast_inp_grid(MY_GRID,MY_ERR)
  end if
  !
  !*** Master reads and broadcasts MODEL_PHYSICS block from input file
  !
  if(TASK_FLAG(TASK_SET_SRC).eq.1) then
     !
     continue   ! already stored in MY_MOD
     !
  else
     !
     if(master_model) call phys_read_inp_model(MY_FILES, MY_MOD, MY_ENS, MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     call phys_bcast_inp_model(MY_MOD,MY_ERR)
  end if
  !
  !*** Master reads and broadcasts AGGREGATION block from input file
  !
  if(TASK_FLAG(TASK_SET_SRC).eq.1) then
     !
     continue   ! already stored in MY_AGR
     !
  else
     !
     if(master_model) call grn_read_inp_aggregation(MY_FILES, MY_AGR, MY_ENS, MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     call grn_bcast_aggregation(MY_AGR,MY_ERR)
  end if
  !
  !*** Master reads and broadcasts MODEL_OUTPUT block from input file
  !
  if(TASK_FLAG(TASK_SET_DBS).eq.1) then
     !
     continue   ! already stored in MY_OUT
     !
  else
     !
     if(master_model) call nc_IO_read_inp_output(MY_FILES, MY_OUT, MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     call nc_IO_bcast_inp_output(MY_OUT, MY_ERR)
  end if
  !
  !*** Performs domain decomposition and constructs the Arakawa grid
  !
  if(TASK_FLAG(TASK_SET_DBS).eq.1) then
     !
     continue   ! already stored in MY_GRID
     !
  else
     !
     call domain_decompose(np,mproc,periodic,MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     allocate(my_hc(my_ibs:my_ibe,my_jbs:my_jbe))
     call Grid_build_arakawa_c(0_ip,MY_GRID%map_h ,MY_GRID%map_v ,MY_GRID%lonmin,MY_GRID%lonmax, &
          MY_GRID%latmin,MY_GRID%latmax,MY_GRID%x3max ,gl_sigma, my_hc,MY_GRID,MY_ERR)  ! task=0 (no topo exists yet)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     !  Reads my_h and checks consistency betewen input file and dbs in space-time
     !
     call nc_IO_check_dbs(MY_FILES,MY_GRID,MY_TIME,MY_OUT,my_hc,MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     call Grid_build_arakawa_c(1_ip,MY_GRID%map_h ,MY_GRID%map_v ,MY_GRID%lonmin,MY_GRID%lonmax, &
          MY_GRID%latmin,MY_GRID%latmax,MY_GRID%x3max ,gl_sigma, my_hc,MY_GRID,MY_ERR)  ! task=1 (topo already exists)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
  end if
  !
  !*** Get meteorological data from the dbs file
  !
  if(TASK_FLAG(TASK_SET_DBS).eq.1) then
     !
     continue   ! already stored in MY_MET
     !
  else
     call nc_IO_read_dbs(MY_FILES,MY_MET,MY_OUT,MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
  end if
  !
  !*** If necessary, perturbate wind components in ensemble runs
  !
  if(nens.gt.1) then
      MY_MET%my_uc  =  ensemble_perturbate_variable( ID_U_WIND, MY_MET%my_uc,  MY_ENS )
      MY_MET%my_u10 =  ensemble_perturbate_variable( ID_U_WIND, MY_MET%my_u10, MY_ENS )
      !
      MY_MET%my_vc  =  ensemble_perturbate_variable( ID_V_WIND, MY_MET%my_vc,  MY_ENS )
      MY_MET%my_v10 =  ensemble_perturbate_variable( ID_V_WIND, MY_MET%my_v10, MY_ENS )
  end if
  !
  !*** Get the effective granulometry (effective bins)
  !
  if(TASK_FLAG(TASK_SET_SRC).eq.1) then
     !
     continue   ! already stored in MY_TRA
     !
  else
     !
     if(master_model) call grn_read_effective_granulometry(MY_FILES, MY_MOD, MY_TRA, MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     call grn_bcast_effective_granulometry(MY_TRA, MY_ERR)
  end if
  !
  !*** Compute interpolation factors for tracking points
  !
  if(MY_OUT%track_points) then
     call F3D_set_pts(MY_OUT,MY_GRID,MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
  end if
  !
  !*** Allocates tracer memory in MY_TRA
  !
  allocate(MY_TRA%my_c   (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h,1:MY_TRA%nbins))
  allocate(MY_TRA%my_s   (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps   :my_kpe   ,1:MY_TRA%nbins))
  allocate(MY_TRA%my_vs  (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kbs_1h:my_kbe_1h,1:MY_TRA%nbins))
  allocate(MY_TRA%my_acum(my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h                    ,1:MY_TRA%nbins))
  allocate(MY_TRA%my_awet(my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h                    ,1:MY_TRA%nbins))
  !
  !*** Set the initial condition
  !
  if(MY_TIME%restart) then
     call nc_IO_read_rst(MY_FILES,MY_GRID,MY_TRA,MY_OUT,MY_TIME,MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     MY_TRA%my_awet(:,:,:  ) = 0.0_rp
     !
  else if(MY_TIME%insertion) then
     call sat_set_initial_condition(MY_FILES,MY_TIME,MY_GRID,MY_TRA,MY_ENS,MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     MY_TRA%my_acum(:,:,:  ) = 0.0_rp
     MY_TRA%my_awet(:,:,:  ) = 0.0_rp
  else
     MY_TRA%my_c   (:,:,:,:) = 0.0_rp
     MY_TRA%my_acum(:,:,:  ) = 0.0_rp
     MY_TRA%my_awet(:,:,:  ) = 0.0_rp
  end if
  !
  !*** Master writes input data to the log file
  !
  if(MY_OUT%log_level.ge.LOG_LEVEL_NORMAL) then
     if(master_model) call F3D_write_data(MY_FILES,MY_TIME,MY_GRID,MY_MOD,MY_TRA,MY_OUT,MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
  end if
  !
  !*** Starts time integration
  !
  MY_TIME%go_on       = .true.
  MY_TIME%time        = MY_TIME%run_start
  MY_TIME%meteo_time  = MY_TIME%run_start
  MY_TIME%source_time = MY_TIME%run_start
  MY_TIME%iiter       = 0
  !
  sourcetime = .true.
  meteotime  = .true.
  !
  !*** Initialize F3D resources
  !
  call F3D_initialize(MY_MOD%limiter,MY_MOD%time_marching)
  !
  do while(MY_TIME%go_on)
     !
     !*** If necessary reads source data
     !
     if(MY_TIME%source_time <= MY_TIME%time) then
        if(sourcetime) then
           call F3D_set_source_term(MY_FILES,MY_TIME,MY_MOD,MY_OUT,MY_GRID,MY_TRA,MY_ERR)
        end if
        if(MY_TIME%source_time >= MY_TIME%run_end) sourcetime = .false.  ! switch off source
     end if
     !
     !***  If necessary load meteo data for the current time interval
     !
     if(MY_TIME%meteo_time <= MY_TIME%time) then
        if(meteotime) then
           call F3D_update_meteo_term(MY_FILES,MY_TIME,MY_OUT,MY_GRID,MY_MOD,MY_AGR,MY_MET,MY_TRA,MY_ERR)
        end if
        if(MY_TIME%meteo_time >= MY_TIME%run_end) meteotime = .false.  ! switch off meteo update
     end if
     !
     !*** Integrate in time
     !
     call F3D_time_step(MY_TIME,MY_GRID,MY_MOD,MY_MET,MY_SPE,MY_TRA,MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
     MY_TIME%time      = MY_TIME%time  + MY_TIME%gl_dt
     MY_TIME%iiter     = MY_TIME%iiter + 1
     MY_TRA%gl_mass_in = MY_TRA%gl_mass_in + MY_TIME%gl_dt*MY_TRA%gl_mass_rate    ! Total mass (including gas species)
     !
     if(MY_TIME%time.ge.MY_TIME%run_end) MY_TIME%go_on = .false.
     !
     !*** ends a time step
     !
     call F3D_end_time_step(MY_FILES,MY_TIME,MY_MOD,MY_OUT,MY_GRID,MY_SPE,MY_TRA,MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
     !
  end do
  !
  !*** Release resources used in time loop
  !
  call F3D_release()
  !
  !*** If necessary, writes final deposit at tracked_points
  !
  if(MY_OUT%track_points) call F3D_out_pts_grn(MY_FILES,MY_SPE,MY_OUT,MY_TRA,MY_ERR)
  !
  !*** If necessary, write the restart file
  !
  if(MY_OUT%out_rst) then
     call nc_IO_out_rst(MY_FILES,MY_GRID,MY_TRA,MY_OUT,MY_TIME,MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
  end if
  !
  !*** Normal end
  !
  if(master_model) call inpout_close_log_file(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
  !
  return
end subroutine task_Fall3D
