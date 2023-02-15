subroutine task_SetDbs
  !
  !------------------------------------
  !    subroutine task_SetDbs
  !------------------------------------
  !
  !>   @brief
  !>   Task for generation of DBS
  !>   @author
  !>   Arnau Folch
  !
  use KindType
  use Shared
  use Parallel
  use InpOut
  use Time
  use Grid
  use Dbs
  use Domain
  use nc_IO
  implicit none
  !
  type(METEO_MODEL) :: GL_METMODEL       ! variables related to driving meteorological model (SetDbs task private)
  !
  real(rp), allocatable :: my_hc  (:,:)
  !
  !*** Initializations
  !
  MY_ERR%nwarn = 0
  call CPU_TIME(MY_ERR%cpu_start_time)
  !
  call inpout_get_filenames(TASK_SET_DBS,MY_FILES,MY_ERR)
  !
  !*** Master opens log file
  !
  if(master_model) call inpout_open_log_file(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  !*** Master reads and broadcasts TIME_UTC block from input file
  !
  if(master_model) call time_read_inp_time(MY_FILES, MY_TIME, MY_ERR)
  !
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  call time_bcast_inp_time(MY_TIME,MY_ERR)
  !
  !*** Master reads and broadcasts GRID block from input file
  !
  if(master_model) call grid_read_inp_grid(MY_FILES, MY_GRID, MY_ERR)
  !
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  call grid_bcast_inp_grid(MY_GRID,MY_ERR)
  !
  !*** Master reads and broadcasts METEO_DATA block from input file
  !
  if(master_model) call dbs_read_inp_meteo(MY_FILES, MY_TIME, MY_MET, GL_METMODEL, GL_METPROFILES, MY_ERR)
  !
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  call dbs_bcast_inp_meteo(MY_FILES, MY_TIME, MY_MET, GL_METPROFILES, MY_ERR)
  !
  !*** Master reads and broadcasts MODEL_OUTPUT block from input file
  !
  if(master_model) call nc_IO_read_inp_output(MY_FILES, MY_OUT, MY_ERR)
  !
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  call nc_IO_bcast_inp_output(MY_OUT, MY_ERR)
  !
  !*** Performs domain decomposition
  !
  call domain_decompose(np,mproc,periodic,MY_ERR)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  !*** Constructs the Arakawa grid (no topo yet)
  !
  allocate(my_hc(my_ibs:my_ibe,my_jbs:my_jbe))
  call Grid_build_arakawa_c(0_ip,MY_GRID%map_h ,MY_GRID%map_v ,MY_GRID%lonmin,MY_GRID%lonmax, &
       MY_GRID%latmin,MY_GRID%latmax,MY_GRID%x3max ,gl_sigma, my_hc,MY_GRID,MY_ERR)  ! task=0 (no topo exists yet)
  !
  !*** Master reads and broadcasts meteo model grid and terrain data nedded to generate MY_GRID (i.e. topography)
  !
  if(master_model) call dbs_read_metmodel_grid(MY_FILES,MY_GRID,MY_MET,GL_METMODEL,MY_ERR)
  !
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  call dbs_bcast_metmodel_grid(GL_METMODEL,MY_ERR)
  !
  !*** Master reads and broadcasts meteo model time coverage
  !
  if(master_model) call dbs_read_metmodel_times(MY_FILES,MY_MET,GL_METMODEL,MY_TIME,MY_ERR)
  !
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  call dbs_bcast_metmodel_times(MY_MET,GL_METMODEL,MY_ERR)
  !
  !*** Builds meteo model 2D Q1 grid and sets 2D interpolation factors
  !
  call dbs_set_interp2d(MY_GRID,MY_MET,GL_METMODEL,MY_ERR)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  !*** Interpolates topography (my_hc)
  !
  call dbs_interpola2d(GL_METMODEL%nx, GL_METMODEL%ny, GL_METMODEL%topg, &
       MY_MET%npoin, MY_MET%el_indexes, MY_MET%interp_factor, my_hc, MY_ERR)
  !
  !*** Builds the rest of Arakawa grid structure
  !
  call Grid_build_arakawa_c(1_ip,MY_GRID%map_h ,MY_GRID%map_v ,MY_GRID%lonmin,MY_GRID%lonmax, &
       MY_GRID%latmin,MY_GRID%latmax,MY_GRID%x3max ,gl_sigma, my_hc,MY_GRID,MY_ERR)  ! task=1 (topo already exists)
  !
  !*** Sets the vertical profiles (to eventually be used by SetSrc)
  !
  call dbs_set_profile(GL_METMODEL,MY_MET,GL_METPROFILES,MY_ERR)
  !
  !*** Interpolates rest of meteo variables within the dbs time slab
  !
  call dbs_read_metmodel_data(MY_FILES,MY_MET,GL_METMODEL,GL_METPROFILES,MY_GRID,MY_ERR)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  !*** If necessary, writes the dbs.nc file
  !
  if(MY_OUT%out_dbs_file) then
     call nc_IO_out_dbs(MY_FILES,MY_GRID,MY_MET,MY_TIME,MY_OUT,MY_ERR)
     !
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  end if
  !
  !*** If necessary, writes the profiles
  !
  if(master_model) call dbs_out_profile(MY_FILES,GL_METPROFILES,MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  !*** Normal end
  !
  if(master_model) call inpout_close_log_file(TASK_SET_DBS, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_DBS, MY_FILES, MY_ERR)
  !
  return
end subroutine task_SetDbs
