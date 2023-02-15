  subroutine task_PosEns
  !
  !------------------------------------
  !    subroutine task_PosEns
  !------------------------------------
  !
  !>   @brief
  !>   Task for postprocess of ensemble runs in FALL3D
  !>   @author
  !>   A. Folch
  !
  use KindType, only: TASK_POS_ENS
  use Shared,   only: MY_FILES, MY_ERR, MY_SPE, MY_OUT, MY_TIME, MY_GRID, MY_ENS, nens
  use Domain,   only: gl_nbx,gl_nby
  use Parallel
  use InpOut
  use Grn
  use nc_IO_names
  use nc_IO
  use Postp
  implicit none
  !
  logical               :: go_on
  character(len=s_file) :: file_res,file_pos,sblock,name_nc
  integer(ip)           :: nfl,nt,nx,ny
  integer(ip)           :: ix,iy,it,ifl,iens,ishand,irhand,jt,n,ith
  integer(ip)           :: year, month, day
  integer(ip)           :: ispe, spe_code, cat_code
  integer(ip)           :: dim
  real(rp)              :: s,min_time
  !
  integer(ip), allocatable :: pp_it(:)
  integer(ip), allocatable :: err_flag(:),ens_master_rank(:),ens_index(:)
  real(rp),    allocatable :: fl(:),timesec(:)
  real(rp),    allocatable :: pp_s(:)
  real(rp),    allocatable :: ens_rank(:),ens_min_time(:)
  !
  real(rp),    allocatable :: work3    (:,:,:)
  real(rp),    allocatable :: work4    (:,:,:,:)
  real(rp),    allocatable :: res_ens_3(:,:,:)
  real(rp),    allocatable :: res_ens_4(:,:,:,:)
  real(rp),    allocatable :: res_ens_5(:,:,:,:,:)
  real(rp),    allocatable :: res_prb_4(:,:,:,:)
  real(rp),    allocatable :: res_prb_5(:,:,:,:,:)
  !
  !*** Initializations
  !
  MY_ERR%flag    = 0
  MY_ERR%source  = 'task_PosEns'
  MY_ERR%message = ' '
  MY_ERR%nwarn   = 0
  !
  call CPU_TIME(MY_ERR%cpu_start_time)
  !
  call inpout_get_filenames(TASK_POS_ENS,MY_FILES,MY_ERR)
  file_res = MY_FILES%file_res
  file_pos = MY_FILES%file_pos
  !
  allocate(ens_index(nens))
  allocate(ens_rank (nens))
  !
  allocate(ens_master_rank(nens))
  ens_master_rank(:) = 0
  if(master_model) ens_master_rank(task_id) = mype_world
  call parallel_sum(ens_master_rank, COMM_WORLD)
  !
  !*** World master opens log file
  !
  if(master_world) call inpout_open_log_file(TASK_POS_ENS, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip,COMM_WORLD)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  !
  !*** Previous checks
  !
  if(nens.lt.2) then
     MY_ERR%flag    = 1
     MY_ERR%source  = 'task_PosEns'
     MY_ERR%message = 'Insufficient ensemble members'
     call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  end if
  !
  !*** Master reads and broadcasts TIME_UTC block from input file
  !
  if(master_model) call time_read_inp_time(MY_FILES, MY_TIME, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip)
  if(MY_ERR%flag.eq.0) then
     call time_bcast_inp_time(MY_TIME,MY_ERR)
  else
     call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  end if
  !
  !*** Master reads and broadcasts SPECIES block from input file
  !
  if(master_model) call grn_read_inp_species(MY_FILES, MY_SPE, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip)
  if(MY_ERR%flag.eq.0) then
     call grn_bcast_species(MY_SPE,MY_ERR)
  else
     call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  end if
  !
  !*** Master reads and broadcasts MODEL_OUTPUT block from input file
  !
  if(master_model) call nc_IO_read_inp_output(MY_FILES, MY_OUT, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip)
  if(MY_ERR%flag.eq.0) then
     call nc_IO_bcast_inp_output(MY_OUT, MY_ERR)
  else
     call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  end if
  !
  !*** Read and check dimensions accross different ensemble members
  !
  call postp_read_dimension(file_res, lon_nc_name, nx, MY_ERR)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  gl_nbx = nx
  call postp_check_dimension(gl_nbx,MY_ERR)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  !
  call postp_read_dimension(file_res, lat_nc_name, ny, MY_ERR)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  gl_nby = ny
  call postp_check_dimension(gl_nby,MY_ERR)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  !
  if(MY_OUT%MY_CUTS%nfl.gt.0) then
     call postp_read_dimension(file_res, zflcut_nc_name, nfl, MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
     call postp_check_dimension(nfl,MY_ERR)
     if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  end if
  !
  call postp_read_dimension(file_res, tim_nc_name, nt, MY_ERR)  ! nt can be different across ensemble members
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  !
  !*** master_model reads coordinate variables (including necessary attributes)
  !
  if(master_model) then
     !
     allocate(MY_GRID%lon_c(nx))
     allocate(MY_GRID%lat_c(ny))
     allocate(timesec(nt ))
     !
     call postp_read_variable(file_res, lon_nc_name, nx, MY_GRID%lon_c, MY_ERR)
     call postp_read_variable_attribute(file_res, lon_nc_name, attr_min_name,  MY_GRID%lonmin, MY_ERR)
     call postp_read_variable_attribute(file_res, lon_nc_name, attr_max_name,  MY_GRID%lonmax, MY_ERR)
     call postp_read_variable_attribute(file_res, lon_nc_name, attr_cell_name, MY_GRID%dlon,   MY_ERR)
     call postp_read_variable_attribute(file_res, lon_nc_name, attr_map_h_name,MY_GRID%map_h,  MY_ERR)
     !
     call postp_read_variable(file_res, lat_nc_name, ny, MY_GRID%lat_c, MY_ERR)
     call postp_read_variable_attribute(file_res, lat_nc_name, attr_min_name,  MY_GRID%latmin, MY_ERR)
     call postp_read_variable_attribute(file_res, lat_nc_name, attr_max_name,  MY_GRID%latmax, MY_ERR)
     call postp_read_variable_attribute(file_res, lat_nc_name, attr_cell_name, MY_GRID%dlat,   MY_ERR)
     call postp_read_variable_attribute(file_res, lat_nc_name, attr_map_h_name,MY_GRID%map_h,  MY_ERR)
     !
     if(MY_OUT%MY_CUTS%nfl.gt.0) then
        allocate(fl(nfl))
        call postp_read_variable(file_res, zflcut_nc_name,nfl,fl, MY_ERR)
     end if
     !
     call postp_read_variable(file_res, tim_nc_name, nt, timesec,  MY_ERR)
     call postp_read_variable_attribute(file_res, tim_nc_name, attr_year_name,  year,  MY_ERR)
     call postp_read_variable_attribute(file_res, tim_nc_name, attr_month_name, month, MY_ERR)
     call postp_read_variable_attribute(file_res, tim_nc_name, attr_day_name,   day,   MY_ERR)
     !
  end if
  !
  !*** Checks consistency for FLs in inputs
  !
  if(MY_OUT%MY_CUTS%nfl.gt.0) then
     allocate(err_flag(nens))
     err_flag(:) = 0
     if(nfl.ne.MY_OUT%MY_CUTS%nfl) then
        err_flag(task_id) = 1
     else
        do ifl = 1,nfl
           if(fl(ifl).ne.MY_OUT%MY_CUTS%fl_cut(ifl)) err_flag(task_id) = 1
        end do
     end if
     !
     call parallel_sum(err_flag, COMM_WORLD)
     if(sum(err_flag).ne.0) then
        MY_ERR%flag    = 1
        MY_ERR%source  = 'task_PosEns'
        MY_ERR%message = 'One or more ensemble members have inconsistent FL values with input file'
        call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
     end if
  end if
  !
  !*** Checks consistency in time-related inputs
  !
  err_flag(:) = 0
  !
  if(year .ne.MY_TIME%start_year ) err_flag(task_id) = 1
  if(month.ne.MY_TIME%start_month) err_flag(task_id) = 1
  if(day  .ne.MY_TIME%start_day  ) err_flag(task_id) = 1
  !
  call parallel_sum(err_flag, COMM_WORLD)
  if(sum(err_flag).ne.0) then
     MY_ERR%flag    = 1
     MY_ERR%source  = 'task_PosEns'
     MY_ERR%message = 'One or more ensemble members have inconsistent dates with input file'
     call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  end if
  !
  !*** Finds the starting time. Note that it may be different from MY_OUT%out_start in case of
  !*** data insertion and/or restart
  !
  allocate(ens_min_time(nens))
  ens_min_time(:)       = 0.0_rp
  ens_min_time(task_id) = timesec(1)
  call parallel_sum(ens_min_time, COMM_WORLD)
  min_time = maxval(ens_min_time)
  !
  !*** Build the postprocess times (may be different than times in ensemble members)
  !
  MY_TIME%time = MY_OUT%out_start   ! include start
  MY_ENS%pp_nt = 1
  if(MY_TIME%time+MY_OUT%dt.ge.min_time) min_time = min(MY_TIME%time,min_time)  ! corrects initial dt shift in output
  go_on = .true.
  do while(go_on)
     MY_TIME%time = MY_TIME%time + MY_OUT%dt
     if( (MY_TIME%time          .le.MY_TIME%run_end).and. &
         (MY_TIME%time+MY_OUT%dt.ge.min_time       )) then
          MY_ENS%pp_nt = MY_ENS%pp_nt + 1
          min_time     = min(MY_TIME%time,min_time)
     else if (MY_TIME%time.gt.MY_TIME%run_end) then
          if(MY_ENS%pp_nt.eq.1) MY_ENS%pp_nt = MY_ENS%pp_nt + 1  ! include end
          go_on = .false.
     end if
  end do
  !
  allocate(MY_ENS%pp_timesec(MY_ENS%pp_nt))
  MY_ENS%pp_timesec(1) = min_time
  do it = 2,MY_ENS%pp_nt
     MY_ENS%pp_timesec(it) = MY_ENS%pp_timesec(it-1) + MY_OUT%dt
  end do
  MY_ENS%pp_timesec(MY_ENS%pp_nt) = min(MY_ENS%pp_timesec(MY_ENS%pp_nt),MY_TIME%run_end)
  !
  !*** Computes the interpolation factors (may be different for each ensemble member)
  !
  allocate(pp_it(MY_ENS%pp_nt))
  allocate(pp_s (MY_ENS%pp_nt))
  do it = 1,MY_ENS%pp_nt
     call postp_interpolate_time(nt, timesec, MY_ENS%pp_timesec(it), pp_it(it), pp_s(it), MY_ERR)
  end do
  !
  !*** Creates output file
  !
  if(master_world) call nc_IO_out_grid_ensemble(MY_FILES,MY_ENS,MY_GRID,MY_SPE,MY_TIME,MY_OUT,MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip,COMM_WORLD)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  !
  !*** Starts computations (master_model processors only)
  !
  if(master_model) then
   do ispe = 1,MY_SPE%nspe
      spe_code   = MY_SPE%code    (ispe)
      cat_code   = MY_SPE%category(ispe)
      sblock     = SPE_TAG        (spe_code)
      !
      !*** 1. Concentration at FLs
      !
      if(MY_OUT%MY_CUTS%nfl.gt.0) then
         !
         !  allocates
         !
         allocate(res_ens_4(nx,ny,nfl,MY_ENS%pp_nt))
         if(master_world) then
            allocate(res_ens_5(nx,ny,nfl,nens,MY_ENS%pp_nt))
         end if
         !
         !  each master_model reads member results
         !
         allocate(work4(nx,ny,nfl,nt))
         name_nc = TRIM(sblock)//TRIM(fl_nc_name)
         call postp_read_variable(file_res, name_nc, nx,ny,nfl,nt, work4, MY_ERR)
         !
         !  each master_model interpolates its results in time (steps can differ for each member)
         !
         do it = 1,MY_ENS%pp_nt
            jt = pp_it(it)
            s  = pp_s(it)
            res_ens_4(:,:,:,it) = s*work4(:,:,:,jt) + (1.0_rp-s)*work4(:,:,:,jt+1)
            res_ens_4(:,:,:,it) = max(res_ens_4(:,:,:,it),0.0_rp)
         end do
         deallocate(work4)
         !
         !  master_world gathers all member results at the postprocess time instants
         !
         if(master_world) then
            allocate(work4(nx,ny,nfl,MY_ENS%pp_nt))
            res_ens_5(:,:,:,1,:) = res_ens_4(:,:,:,:)
         end if
         dim = nx*ny*nfl*MY_ENS%pp_nt
         do iens = 2,nens
            if(master_world) then
               call parallel_irecv(work4(1,1,1,1), dim, ens_master_rank(iens), 0_ip, irhand, COMM_WORLD)
               call parallel_wait( irhand )
               res_ens_5(:,:,:,iens,:) = work4(:,:,:,:)
            else if(task_id.eq.iens) then
               call parallel_isend(res_ens_4(1,1,1,1), dim, 0_ip, 0_ip, ishand, COMM_WORLD)
               call parallel_wait( ishand )
            end if
         end do
         if(master_world) deallocate(work4)
         !
         !*** master_world takes the lead of all remaining work
         !
         if(master_world) then
            !
            ! outputs ensemble member results
            !
            if(MY_ENS%postprocess_members) then
               name_nc = TRIM(sblock)//TRIM(c_total_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,nfl,nens,MY_ENS%pp_nt, res_ens_5, MY_ERR)
            end if
            !
            ! computes and outputs ensemble mean
            !
            if(MY_ENS%postprocess_mean) then
               res_ens_4 = 0.0_rp
               do iens = 1,nens
                  res_ens_4(:,:,:,:) = res_ens_4(:,:,:,:) + res_ens_5(:,:,:,iens,:)
               end do
               res_ens_4(:,:,:,:) = res_ens_4(:,:,:,:)/nens
               !
               name_nc = TRIM(sblock)//TRIM(c_total_nc_name)//TRIM(ens_mean_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,nfl,MY_ENS%pp_nt, res_ens_4, MY_ERR)
            end if
            !
            ! computes and outputs ensemble log mean
            !
            if(MY_ENS%postprocess_logmean) then
               res_ens_4 = 0.0_rp
               do iens = 1,nens
                  res_ens_4(:,:,:,:) = res_ens_4(:,:,:,:) + log10(res_ens_5(:,:,:,iens,:))
               end do
               res_ens_4(:,:,:,:) = res_ens_4(:,:,:,:)/nens
               res_ens_4(:,:,:,:) = 10.0_rp**(res_ens_4(:,:,:,:))
               !
               name_nc = TRIM(sblock)//TRIM(c_total_nc_name)//TRIM(ens_logmean_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,nfl,MY_ENS%pp_nt, res_ens_4, MY_ERR)
            end if
            !
            ! computes and outputs ensemble standard deviation
            !
            if(MY_ENS%postprocess_sandard_dev) then
               res_ens_4 = 0.0_rp
               do iens = 1,nens
                  res_ens_4(:,:,:,:) = res_ens_4(:,:,:,:) + res_ens_5(:,:,:,iens,:)
               end do
               res_ens_4(:,:,:,:) = res_ens_4(:,:,:,:)/nens
               allocate(work4(nx,ny,nfl,MY_ENS%pp_nt))
               work4(:,:,:,:) = res_ens_4(:,:,:,:)     ! mean
               do iens = 1,nens
                  res_ens_4(:,:,:,:) = (res_ens_5(:,:,:,iens,:) - work4(:,:,:,:))* &
                                       (res_ens_5(:,:,:,iens,:) - work4(:,:,:,:))
               end do
               res_ens_4(:,:,:,:) = sqrt(res_ens_4(:,:,:,:)/(nens-1))
               deallocate(work4)
               !
               name_nc = TRIM(sblock)//TRIM(c_total_nc_name)//TRIM(ens_std_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,nfl,MY_ENS%pp_nt, res_ens_4, MY_ERR)
            end if
            !
            ! computes and outputs ensemble median
            !
            if(MY_ENS%postprocess_median) then
               do it  = 1,MY_ENS%pp_nt
               do ifl = 1,nfl
               do iy  = 1,ny
               do ix  = 1,nx
                  ens_rank(:) = res_ens_5(ix,iy,ifl,:,it)
                  call postp_rank_vector(nens, ens_rank, ens_index, EPSILON, MY_ERR)
                  res_ens_4(ix,iy,ifl,it) = ens_rank(int(nens/2))
               end do
               end do
               end do
               end do
               !
               name_nc = TRIM(sblock)//TRIM(c_total_nc_name)//TRIM(ens_median_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,nfl,MY_ENS%pp_nt, res_ens_4, MY_ERR)
            end if
            !
            ! computes and outputs exceedance_probability
            !
            if(MY_ENS%postprocess_probability.and.MY_ENS%nth_con.gt.0) then
               allocate(res_prb_5(nx,ny,nfl,MY_ENS%nth_con,MY_ENS%pp_nt))
               do it  = 1,MY_ENS%pp_nt
               do ifl = 1,nfl
               do iy  = 1,ny
               do ix  = 1,nx
                  ens_rank(:) = res_ens_5(ix,iy,ifl,:,it)
                  !
                  do ith = 1,MY_ENS%nth_con  ! concentration thresholds
                     n = 0
                     do iens = 1,nens
                        if(ens_rank(iens).ge.MY_ENS%th_con(ith)) n = n + 1
                     end do
                     res_prb_5(ix,iy,ifl,ith,it) = 100.0_rp*n/nens
                  end do
               end do
               end do
               end do
               end do
               !
               name_nc = TRIM(sblock)//TRIM(c_total_nc_name)//TRIM(ens_prb_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,nfl,MY_ENS%nth_con,MY_ENS%pp_nt, res_prb_5, MY_ERR)
               deallocate(res_prb_5)
            end if
            !
            ! computes and outputs intensity measure (percentiles)
            !
            if(MY_ENS%postprocess_percentiles.and.MY_ENS%nval_per.gt.0) then
               allocate(res_prb_5(nx,ny,nfl,MY_ENS%nval_per,MY_ENS%pp_nt))
               do it  = 1,MY_ENS%pp_nt
               do ifl = 1,nfl
               do iy  = 1,ny
               do ix  = 1,nx
                  ens_rank(:) = res_ens_5(ix,iy,ifl,:,it)
                  call postp_rank_vector(nens, ens_rank, ens_index, EPSILON, MY_ERR)
                  !
                  do ith = 1,MY_ENS%nval_per
                     n = int(MY_ENS%val_per(ith)/100.0_rp*nens)
                     n = max(n,1)
                     n = min(n,nens)
                     res_prb_5(ix,iy,ifl,ith,it) = ens_rank(n)
                  end do
               end do
               end do
               end do
               end do
               !
               name_nc = TRIM(sblock)//TRIM(c_total_nc_name)//TRIM(ens_per_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,nfl,MY_ENS%nval_per,MY_ENS%pp_nt, res_prb_5, MY_ERR)
               deallocate(res_prb_5)
            end if
            !
         end if !  if(master_world)
         !
         !*** deallocates
         !
         deallocate(res_ens_4)
         if(master_world) then
            deallocate(res_ens_5)
         end if
         !
      end if ! if(MY_OUT%MY_CUTS%nfl.gt.0)
      !
      !*** 2. COL_MASS
      !
      if(MY_OUT%out_col_load) then
         !
         !  allocates
         !
         allocate(res_ens_3(nx,ny,MY_ENS%pp_nt))
         if(master_world) then
            allocate(res_ens_4(nx,ny,nens,MY_ENS%pp_nt))
         end if
         !
         !  each master_model reads member results
         !
         allocate(work3(nx,ny,nt))
         name_nc = TRIM(sblock)//TRIM(col_nc_name)
         call postp_read_variable(file_res, name_nc, nx,ny,nt, work3, MY_ERR)
         !
         !  each master_model interpolates its results in time (steps can differ for each member)
         !
         do it = 1,MY_ENS%pp_nt
            jt = pp_it(it)
            s  = pp_s(it)
            res_ens_3(:,:,it) = s*work3(:,:,jt) + (1.0_rp-s)*work3(:,:,jt+1)
            res_ens_3(:,:,it) = max(res_ens_3(:,:,it),0.0_rp)
         end do
         deallocate(work3)
         !
         !  master_world gathers all member results at the postprocess time instants
         !
         if(master_world) then
           allocate(work3(nx,ny,MY_ENS%pp_nt))
           res_ens_4(:,:,1,:) = res_ens_3(:,:,:)
         end if
         dim = nx*ny*MY_ENS%pp_nt
         do iens = 2,nens
            if(master_world) then
               call parallel_irecv(work3(1,1,1), dim, ens_master_rank(iens), 0_ip, irhand, COMM_WORLD)
               call parallel_wait( irhand )
               res_ens_4(:,:,iens,:) = work3(:,:,:)
            else if(task_id.eq.iens) then
               call parallel_isend(res_ens_3(1,1,1), dim, 0_ip, 0_ip, ishand, COMM_WORLD)
               call parallel_wait( ishand )
            end if
         end do
         if(master_world) deallocate(work3)
         !
         !*** master_world takes the lead of all remaining work
         !
         if(master_world) then
            !
            ! outputs ensemble member results
            !
            if(MY_ENS%postprocess_members) then
               name_nc = TRIM(sblock)//TRIM(col_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,nens,MY_ENS%pp_nt, res_ens_4, MY_ERR)
            end if
            !
            ! computes and outputs ensemble mean
            !
            if(MY_ENS%postprocess_mean) then
               res_ens_3 = 0.0_rp
               do iens = 1,nens
                  res_ens_3(:,:,:) = res_ens_3(:,:,:) + res_ens_4(:,:,iens,:)
               end do
               res_ens_3(:,:,:) = res_ens_3(:,:,:)/nens
               !
               name_nc = TRIM(sblock)//TRIM(col_nc_name)//TRIM(ens_mean_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,MY_ENS%pp_nt, res_ens_3, MY_ERR)
            end if
            !
            ! computes and outputs ensemble log mean
            !
            if(MY_ENS%postprocess_logmean) then
               res_ens_3 = 0.0_rp
               do iens = 1,nens
                  res_ens_3(:,:,:) = res_ens_3(:,:,:) + log10(res_ens_4(:,:,iens,:))
               end do
               res_ens_3(:,:,:) = res_ens_3(:,:,:)/nens
               res_ens_3(:,:,:) = 10.0_rp**(res_ens_3(:,:,:))
               !
               name_nc = TRIM(sblock)//TRIM(col_nc_name)//TRIM(ens_logmean_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,MY_ENS%pp_nt, res_ens_3, MY_ERR)
            end if
            !
            ! computes and outputs ensemble standard deviation
            !
            if(MY_ENS%postprocess_sandard_dev) then
               res_ens_3 = 0.0_rp
               do iens = 1,nens
                  res_ens_3(:,:,:) = res_ens_3(:,:,:) + res_ens_4(:,:,iens,:)
               end do
               res_ens_3(:,:,:) = res_ens_3(:,:,:)/nens
               allocate(work3(nx,ny,MY_ENS%pp_nt))
               work3(:,:,:) = res_ens_3(:,:,:)     ! mean
               do iens = 1,nens
                  res_ens_3(:,:,:) = (res_ens_4(:,:,iens,:) - work3(:,:,:))* &
                                     (res_ens_4(:,:,iens,:) - work3(:,:,:))
               end do
               res_ens_3(:,:,:) = sqrt(res_ens_3(:,:,:)/(nens-1))
               deallocate(work3)
               !
               name_nc = TRIM(sblock)//TRIM(col_nc_name)//TRIM(ens_std_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,MY_ENS%pp_nt, res_ens_3, MY_ERR)
            end if
            !
            ! computes and outputs ensemble median
            !
            if(MY_ENS%postprocess_median) then
               do it  = 1,MY_ENS%pp_nt
               do iy  = 1,ny
               do ix  = 1,nx
                  ens_rank(:) = res_ens_4(ix,iy,:,it)
                  call postp_rank_vector(nens, ens_rank, ens_index, EPSILON, MY_ERR)
                  res_ens_3(ix,iy,it) = ens_rank(nens/2)
               end do
               end do
               end do
               !
               name_nc = TRIM(sblock)//TRIM(col_nc_name)//TRIM(ens_median_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,MY_ENS%pp_nt, res_ens_3, MY_ERR)
            end if
            !
            ! computes and outputs exceedance_probability
            !
            if(MY_ENS%postprocess_probability) then
            if( (spe_code.eq.SPE_SO2).and.(MY_ENS%nth_col_mass_DU.gt.0) ) then
               allocate(res_prb_4(nx,ny,MY_ENS%nth_col_mass_DU,MY_ENS%pp_nt))
               do it  = 1,MY_ENS%pp_nt
               do iy  = 1,ny
               do ix  = 1,nx
                  ens_rank(:) = res_ens_4(ix,iy,:,it)
                  !
                  do ith = 1,MY_ENS%nth_col_mass_DU  ! col mass thresholds (in DU)
                     n = 0
                     do iens = 1,nens
                        if(ens_rank(iens).ge.MY_ENS%th_col_mass_DU(ith)) n = n + 1
                     end do
                     res_prb_4(ix,iy,ith,it) = 100.0_rp*n/nens
                  end do
               end do
               end do
               end do
               !
               name_nc = TRIM(sblock)//TRIM(col_nc_name)//TRIM(ens_prb_nc_name)
               call nc_IO_out_variable(file_pos,name_nc,nx,ny,MY_ENS%nth_col_mass_DU,MY_ENS%pp_nt,res_prb_4,MY_ERR)
               deallocate(res_prb_4)
               !
            else if( (spe_code.ne.SPE_SO2).and.(MY_ENS%nth_col_mass.gt.0) ) then
              allocate(res_prb_4(nx,ny,MY_ENS%nth_col_mass,MY_ENS%pp_nt))
               do it  = 1,MY_ENS%pp_nt
               do iy  = 1,ny
               do ix  = 1,nx
                  ens_rank(:) = res_ens_4(ix,iy,:,it)
                  !
                  do ith = 1,MY_ENS%nth_col_mass  ! col mass thresholds (in DU)
                     n = 0
                     do iens = 1,nens
                        if(ens_rank(iens).ge.MY_ENS%th_col_mass(ith)) n = n + 1
                     end do
                     res_prb_4(ix,iy,ith,it) = 100.0_rp*n/nens
                  end do
               end do
               end do
               end do
               !
               name_nc = TRIM(sblock)//TRIM(col_nc_name)//TRIM(ens_prb_nc_name)
               call nc_IO_out_variable(file_pos,name_nc,nx,ny,MY_ENS%nth_col_mass,MY_ENS%pp_nt,res_prb_4,MY_ERR)
               deallocate(res_prb_4)
            end if
            end if
            !
            ! computes and outputs intensity measure (percentiles)
            !
            if(MY_ENS%postprocess_percentiles.and.MY_ENS%nval_per.gt.0) then
               allocate(res_prb_4(nx,ny,MY_ENS%nval_per,MY_ENS%pp_nt))
               do it  = 1,MY_ENS%pp_nt
               do iy  = 1,ny
               do ix  = 1,nx
                  ens_rank(:) = res_ens_4(ix,iy,:,it)
                  call postp_rank_vector(nens, ens_rank, ens_index, EPSILON, MY_ERR)
                  !
                  do ith = 1,MY_ENS%nval_per
                     n = int(MY_ENS%val_per(ith)/100.0_rp*nens)
                     n = max(n,1)
                     n = min(n,nens)
                     res_prb_4(ix,iy,ith,it) = ens_rank(n)
                  end do
               end do
               end do
               end do
               !
               name_nc = TRIM(sblock)//TRIM(col_nc_name)//TRIM(ens_per_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,MY_ENS%nval_per,MY_ENS%pp_nt, res_prb_4, MY_ERR)
               deallocate(res_prb_4)
            end if
           !
         end if !  if(master_world)
         !
         !*** deallocates
         !
         deallocate(res_ens_3)
         if(master_world) then
            deallocate(res_ens_4)
         end if
         !
      end if ! if(MY_OUT%out_col_load)
      !
      !*** 3. GRN_LOAD
      !
      if(MY_OUT%out_grn_total) then
         !
         !  allocates
         !
         allocate(res_ens_3(nx,ny,MY_ENS%pp_nt))
         if(master_world) then
            allocate(res_ens_4(nx,ny,nens,MY_ENS%pp_nt))
         end if
         !
         !  each master_model reads member results
         !
         allocate(work3(nx,ny,nt))
         name_nc = TRIM(sblock)//TRIM(grn_nc_name)
         call postp_read_variable(file_res, name_nc, nx,ny,nt, work3, MY_ERR)
         !
         !  each master_model interpolates its results in time (steps can differ for each member)
         !
         do it = 1,MY_ENS%pp_nt
            jt = pp_it(it)
            s  = pp_s(it)
            res_ens_3(:,:,it) = s*work3(:,:,jt) + (1.0_rp-s)*work3(:,:,jt+1)
            res_ens_3(:,:,it) = max(res_ens_3(:,:,it),0.0_rp)
         end do
         deallocate(work3)
         !
         !  master_world gathers all member results at the postprocess time instants
         !
         if(master_world) then
            allocate(work3(nx,ny,MY_ENS%pp_nt))
            res_ens_4(:,:,1,:) = res_ens_3(:,:,:)
         end if
         dim = nx*ny*MY_ENS%pp_nt
         do iens = 2,nens
            if(master_world) then
               call parallel_irecv(work3(1,1,1), dim, ens_master_rank(iens), 0_ip, irhand, COMM_WORLD)
               call parallel_wait( irhand )
               res_ens_4(:,:,iens,:) = work3(:,:,:)
            else if(task_id.eq.iens) then
               call parallel_isend(res_ens_3(1,1,1), dim, 0_ip, 0_ip, ishand, COMM_WORLD)
               call parallel_wait( ishand )
            end if
         end do
         if(master_world) deallocate(work3)
         !
         !*** master_world takes the lead of all remaining work
         !
         if(master_world) then
            !
            ! outputs ensemble member results
            !
            if(MY_ENS%postprocess_members) then
               name_nc = TRIM(sblock)//TRIM(grn_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,nens,MY_ENS%pp_nt, res_ens_4, MY_ERR)
            end if
            !
            ! computes and outputs ensemble mean
            !
            if(MY_ENS%postprocess_mean) then
               res_ens_3 = 0.0_rp
               do iens = 1,nens
                  res_ens_3(:,:,:) = res_ens_3(:,:,:) + res_ens_4(:,:,iens,:)
               end do
               res_ens_3(:,:,:) = res_ens_3(:,:,:)/nens
               !
               name_nc = TRIM(sblock)//TRIM(grn_nc_name)//TRIM(ens_mean_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,MY_ENS%pp_nt, res_ens_3, MY_ERR)
            end if
            !
            ! computes and outputs ensemble log mean
            !
            if(MY_ENS%postprocess_logmean) then
               res_ens_3 = 0.0_rp
               do iens = 1,nens
                  res_ens_3(:,:,:) = res_ens_3(:,:,:) + log10(res_ens_4(:,:,iens,:))
               end do
               res_ens_3(:,:,:) = res_ens_3(:,:,:)/nens
               res_ens_3(:,:,:) = 10.0_rp**(res_ens_3(:,:,:))
               !
               name_nc = TRIM(sblock)//TRIM(grn_nc_name)//TRIM(ens_logmean_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,MY_ENS%pp_nt, res_ens_3, MY_ERR)
            end if
            !
            ! computes and outputs ensemble standard deviation
            !
            if(MY_ENS%postprocess_sandard_dev) then
               res_ens_3 = 0.0_rp
               do iens = 1,nens
                  res_ens_3(:,:,:) = res_ens_3(:,:,:) + res_ens_4(:,:,iens,:)
               end do
               res_ens_3(:,:,:) = res_ens_3(:,:,:)/nens
               allocate(work3(nx,ny,MY_ENS%pp_nt))
               work3(:,:,:) = res_ens_3(:,:,:)     ! mean
               do iens = 1,nens
                  res_ens_3(:,:,:) = (res_ens_4(:,:,iens,:) - work3(:,:,:))* &
                                     (res_ens_4(:,:,iens,:) - work3(:,:,:))
               end do
               res_ens_3(:,:,:) = sqrt(res_ens_3(:,:,:)/(nens-1))
               deallocate(work3)
               !
               name_nc = TRIM(sblock)//TRIM(grn_nc_name)//TRIM(ens_std_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,MY_ENS%pp_nt, res_ens_3, MY_ERR)
            end if
            !
            ! computes and outputs ensemble median
            !
            if(MY_ENS%postprocess_median) then
               do it  = 1,MY_ENS%pp_nt
               do iy  = 1,ny
               do ix  = 1,nx
                  ens_rank(:) = res_ens_4(ix,iy,:,it)
                  call postp_rank_vector(nens, ens_rank, ens_index, EPSILON, MY_ERR)
                  res_ens_3(ix,iy,it) = ens_rank(nens/2)
               end do
               end do
               end do
               !
               name_nc = TRIM(sblock)//TRIM(grn_nc_name)//TRIM(ens_median_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,MY_ENS%pp_nt, res_ens_3, MY_ERR)
            end if
            !
            ! computes and outputs exceedance_probability
            !
            if(MY_ENS%postprocess_probability.and.MY_ENS%nth_grn_load.gt.0) then
              allocate(res_prb_4(nx,ny,MY_ENS%nth_grn_load,MY_ENS%pp_nt))
              do it  = 1,MY_ENS%pp_nt
              do iy  = 1,ny
              do ix  = 1,nx
                 ens_rank(:) = res_ens_4(ix,iy,:,it)
                 !
                 do ith = 1,MY_ENS%nth_grn_load   ! ground load thresholds
                    n = 0
                    do iens = 1,nens
                       if(ens_rank(iens).ge.MY_ENS%th_grn_load(ith)) n = n + 1
                    end do
                    res_prb_4(ix,iy,ith,it) = 100.0_rp*n/nens
                 end do
              end do
              end do
              end do
              !
              name_nc = TRIM(sblock)//TRIM(grn_nc_name)//TRIM(ens_prb_nc_name)
              call nc_IO_out_variable(file_pos,name_nc,nx,ny,MY_ENS%nth_grn_load,MY_ENS%pp_nt,res_prb_4,MY_ERR)
              deallocate(res_prb_4)
            end if
            !
            ! computes and outputs intensity measure (percentiles)
            !
            if(MY_ENS%postprocess_percentiles.and.MY_ENS%nval_per.gt.0) then
               allocate(res_prb_4(nx,ny,MY_ENS%nval_per,MY_ENS%pp_nt))
               do it  = 1,MY_ENS%pp_nt
               do iy  = 1,ny
               do ix  = 1,nx
                  ens_rank(:) = res_ens_4(ix,iy,:,it)
                  call postp_rank_vector(nens, ens_rank, ens_index, EPSILON, MY_ERR)
                  !
                  do ith = 1,MY_ENS%nval_per
                     n = int(MY_ENS%val_per(ith)/100.0_rp*nens)
                     n = max(n,1)
                     n = min(n,nens)
                     res_prb_4(ix,iy,ith,it) = ens_rank(n)
                  end do
               end do
               end do
               end do
               !
               name_nc = TRIM(sblock)//TRIM(grn_nc_name)//TRIM(ens_per_nc_name)
               call nc_IO_out_variable(file_pos, name_nc, nx,ny,MY_ENS%nval_per,MY_ENS%pp_nt, res_prb_4, MY_ERR)
               deallocate(res_prb_4)
            end if
            !
         end if !  if(master_world)
         !
         !*** deallocates
         !
         deallocate(res_ens_3)
         if(master_world) then
            deallocate(res_ens_4)
         end if
         !
      end if ! if(MY_OUT%out_grn_total)
      !
   end do  ! do ispe = 1,MY_SPE%nspe
   end if  ! if(master_model)
  !
  !*** Normal end
  !
  if(master_world) call inpout_close_log_file(TASK_POS_ENS, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip,COMM_WORLD)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_POS_ENS, MY_FILES, MY_ERR)
  !
  return
  end subroutine task_PosEns
