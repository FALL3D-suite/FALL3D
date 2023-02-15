  subroutine task_PosVal
  !
  !------------------------------------
  !    subroutine task_PosVal
  !------------------------------------
  !
  !>   @brief
  !>   Task for model validation (including ensemble runs) in FALL3D
  !>   @note
  !>   This tasks assumes that nens = 1 and NPZ = 1
  !>   @author
  !>   A. Folch
  !
  use KindType,   only: TASK_POS_VAL
  use Shared,     only: MY_FILES, MY_ERR
  use Validation
  use Sat
  use Deposit
  use Grid
  !
  implicit none
  !
  type(OBS_PARAMS) :: MY_OBS
  type(RES_PARAMS) :: MY_RES
  type(VAL_PARAMS) :: MY_VAL
  type(RUN_TIME)   :: MY_TIME
  type(SAT_INFO)   :: GL_SAT_INFO
  type(SAT_DATA2D) :: GL_SAT_DATA2D
  type(DEP_DATA)   :: GL_DEP_DATA
  type(DEP_PTS)    :: GL_DEP_PTS
  !
  !*** Initializations
  !
  MY_ERR%flag    = 0
  MY_ERR%source  = 'task_PosVal'
  MY_ERR%message = ' '
  MY_ERR%nwarn   = 0
  !
  call CPU_TIME(MY_ERR%cpu_start_time)
  !
  call inpout_get_filenames(TASK_POS_VAL,MY_FILES,MY_ERR)
  !
  !*** Master world opens log file
  !
  if(master_world) call inpout_open_log_file(TASK_POS_VAL, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip,COMM_WORLD)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
  !
  !*** Master reads and broadcasts MODEL_VALIDATION block from input file
  !
  if(master_model) call validation_read_inp(MY_FILES,MY_OBS,MY_RES,MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
  call validation_bcast_inp_params(MY_OBS,MY_RES,MY_ERR)
  !
  !*** Master reads and broadcast the FALL3D type of results, the computational domain, and FALL3D output times
  !
  if(master_model) call validation_read_res_params(MY_FILES,MY_RES,MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
  call validation_bcast_res_params(MY_RES,MY_ERR)
  !
  !*** Master reads and broadcasts observations
  !
  select case(MY_OBS%type)
  case(OBS_TYPE_SATELLITE_DETECTION, OBS_TYPE_SATELLITE_RETRIEVAL)
     !
     !*** Read and broadcast satellite metadata (stored in GL_SAT_INFO)
     !
     MY_FILES%file_sat     = MY_OBS%file_name
     MY_FILES%file_tbl_sat = MY_OBS%file_tbl
     MY_TIME%start_year    = MY_RES%start_year
     MY_TIME%start_month   = MY_RES%start_month
     MY_TIME%start_day     = MY_RES%start_day
     !
     call sat_get_gl_info(MY_FILES,MY_TIME,GL_SAT_INFO,MY_ERR)
     !
     !*** Master reads and broadcasts gridded satellite data (stored in GL_SAT_DATA2D)
     !
     if(master_model) call sat_get_gl_data2d(MY_FILES,GL_SAT_INFO,GL_SAT_DATA2D,MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
     call sat_bcast_sat_data2d(GL_SAT_INFO,GL_SAT_DATA2D,MY_ERR)
     !
  case(OBS_TYPE_DEPOSIT_CONTOURS)
     !
     !*** Master reads and broadcasts gridded deposit contour data (stored in GL_DEP_DATA)
     !
     MY_FILES%file_dep     = MY_OBS%file_name
     MY_FILES%file_tbl_dep = MY_OBS%file_tbl
     !
     if(master_model) call deposit_get_depdata(MY_FILES,GL_DEP_DATA,MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
     call deposit_bcast_depdata(GL_DEP_DATA,MY_ERR)
     !
  case (OBS_TYPE_DEPOSIT_POINTS)
     !
     !*** Master reads and broadcasts sctatered deposit points (stored in GL_DEP_PTS)
     !
     MY_FILES%file_dep     = MY_OBS%file_name
     MY_FILES%file_tbl_dep = MY_OBS%file_tbl
     !
     if(master_model) call deposit_get_ptsdata(MY_FILES,GL_DEP_PTS,MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
     call deposit_bcast_ptsdata(GL_DEP_PTS,MY_ERR)
     !
  case (OBS_TYPE_VAA)
     !
     MY_ERR%source  = 'task_PosVal'
     MY_ERR%message = 'Type of observation not implememted '
     call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
     !
  end select
  !
  !*** Perform domain decomposition
  !
  np(1)       = MY_RES%npx
  np(2)       = MY_RES%npy
  np(3)       = 1
  periodic(1) = MY_RES%periodic
  call domain_decompose(np,mproc,periodic,MY_ERR)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
  !
  !*** Interpolate observations to model grid or vice-versa (results stored in MY_OBS)
  !
  select case(MY_OBS%type)
  case(OBS_TYPE_SATELLITE_DETECTION, OBS_TYPE_SATELLITE_RETRIEVAL)
     !
     !*** Satellite observations interpolated to the FALL3D grid
     !
     call validation_interpolate_sat_obs(MY_FILES,MY_OBS,MY_RES,GL_SAT_INFO,GL_SAT_DATA2D,MY_ERR)
     !
   case (OBS_TYPE_DEPOSIT_CONTOURS)
     !
     !*** Deposit observations interpolated to the FALL3D grid
     !
     call validation_interpolate_dep_obs(MY_FILES,MY_OBS,MY_RES,GL_DEP_DATA,MY_ERR)
     !
   case (OBS_TYPE_DEPOSIT_POINTS)
     !
     !*** FALL3D results interpolated to deposit points (only interpolation factors computed
     !*** at this stage)
     !
     call validation_interpolate_dep_pts(MY_FILES,MY_OBS,MY_RES,GL_DEP_PTS,MY_ERR)
     !
   case (OBS_TYPE_VAA)
     !
     MY_ERR%source  = 'task_PosVal'
     MY_ERR%message = 'Type of observation not implemented '
     call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
     !
  end select
  !
  !*** Get time interpolation factors
  !
  select case(MY_OBS%type)
  case(OBS_TYPE_SATELLITE_DETECTION, OBS_TYPE_SATELLITE_RETRIEVAL, &
       OBS_TYPE_DEPOSIT_CONTOURS,    OBS_TYPE_DEPOSIT_POINTS)
     !
     !*** For satellite observations time lag is already corrected in GL_SAT_INFO
     !*** For deposit observations only last model time step is considered
     !
     call validation_get_time_factors(MY_FILES,MY_OBS,MY_RES,MY_ERR)
     !
  case (OBS_TYPE_VAA)
     !
     MY_ERR%source  = 'task_PosVal'
     MY_ERR%message = 'Type of observation not implemented '
     call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
     !
  end select
  !
  !*** Perform the validation
  !
  select case(MY_OBS%type)
  case(OBS_TYPE_SATELLITE_DETECTION, OBS_TYPE_SATELLITE_RETRIEVAL,OBS_TYPE_DEPOSIT_CONTOURS)
     !
     call validation_type_grid(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR)
     !
  case (OBS_TYPE_DEPOSIT_POINTS)
     !
     call validation_type_pts(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR)
     !
  case (OBS_TYPE_VAA)
     !
     MY_ERR%source  = 'task_PosVal'
     MY_ERR%message = 'Type of observation not implememted '
     call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
     !
  end select
  !
  !*** Normal end
  !
  if(master_world) call inpout_close_log_file(TASK_POS_VAL, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip,COMM_WORLD)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_VAL, MY_FILES, MY_ERR)
  !
  return
  end subroutine task_PosVal
