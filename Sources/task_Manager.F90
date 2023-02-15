!*****************************************************************************
!>
!> Task Manager
!> @author
!> Arnau Folch
!>
!>    This program is free software: you can redistribute it and/or modify
!>    it under the terms of the GNU General Public License (GPL) as published by
!>    the Free Software Foundation, either version 3 of the License, or
!>    any later version.
!>
!>    This program is distributed in the hope that it will be useful,
!>    but WITHOUT ANY WARRANTY; without even the implied warranty of
!>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!>
!>    The program is distributed free of charge except for commercial use, which
!>    requires of a charge fee or a explicit written permission from the authors
!>
!>    For more details: https://opensource.org/licenses/GPL-3.0
!>
!*****************************************************************************
  program Fall3d_task_manager
  use KindType
  use Shared
  use Parallel
  use InpOut
  implicit none
  !
  integer(ip)           :: narg, iargs, iproc
  character(len=s_name) :: what
  !
  !*** Initialize MPI and COMM_WORLD communicator
  !
  call parallel_startup(MY_ERR)
  !
  !*** Gets and decodes program call arguments
  !
  narg = command_argument_count()
  !
  if(narg.lt.2) then
     if(master_world) call inpout_print_args
     call parallel_hangup(MY_ERR)
  end if
  !
  !*** Decodes the first argument (task)
  !
  call get_command_argument(1,what)
  call upcase(what)
  select case(what)
  case('SETTGSD')
     TASK_FLAG(TASK_RUN_ALL   ) = 0
     TASK_FLAG(TASK_SET_ENS   ) = 0
     TASK_FLAG(TASK_SET_TGSD  ) = 1
     TASK_FLAG(TASK_SET_DBS   ) = 0
     TASK_FLAG(TASK_SET_SRC   ) = 0
     TASK_FLAG(TASK_RUN_FALL3D) = 0
     TASK_FLAG(TASK_POS_ENS   ) = 0
     TASK_FLAG(TASK_POS_VAL   ) = 0
  case('SETDBS')
     TASK_FLAG(TASK_RUN_ALL   ) = 0
     TASK_FLAG(TASK_SET_ENS   ) = 0
     TASK_FLAG(TASK_SET_TGSD  ) = 0
     TASK_FLAG(TASK_SET_DBS   ) = 1
     TASK_FLAG(TASK_SET_SRC   ) = 0
     TASK_FLAG(TASK_RUN_FALL3D) = 0
     TASK_FLAG(TASK_POS_ENS   ) = 0
     TASK_FLAG(TASK_POS_VAL   ) = 0
  case('SETSRC')
     TASK_FLAG(TASK_RUN_ALL   ) = 0
     TASK_FLAG(TASK_SET_ENS   ) = 0
     TASK_FLAG(TASK_SET_TGSD  ) = 0
     TASK_FLAG(TASK_SET_DBS   ) = 0
     TASK_FLAG(TASK_SET_SRC   ) = 1
     TASK_FLAG(TASK_RUN_FALL3D) = 0
     TASK_FLAG(TASK_POS_ENS   ) = 0
     TASK_FLAG(TASK_POS_VAL   ) = 0
  case('FALL3D')
     TASK_FLAG(TASK_RUN_ALL   ) = 0
     TASK_FLAG(TASK_SET_ENS   ) = 0
     TASK_FLAG(TASK_SET_TGSD  ) = 0
     TASK_FLAG(TASK_SET_DBS   ) = 0
     TASK_FLAG(TASK_SET_SRC   ) = 0
     TASK_FLAG(TASK_RUN_FALL3D) = 1
     TASK_FLAG(TASK_POS_ENS   ) = 0
     TASK_FLAG(TASK_POS_VAL   ) = 0
  case('ALL')
     TASK_FLAG(TASK_RUN_ALL   ) = 1
     TASK_FLAG(TASK_SET_ENS   ) = 0
     TASK_FLAG(TASK_SET_TGSD  ) = 1
     TASK_FLAG(TASK_SET_DBS   ) = 1
     TASK_FLAG(TASK_SET_SRC   ) = 1
     TASK_FLAG(TASK_RUN_FALL3D) = 1
     TASK_FLAG(TASK_POS_ENS   ) = 0
     TASK_FLAG(TASK_POS_VAL   ) = 0
  case('POSENS')
     TASK_FLAG(TASK_RUN_ALL   ) = 0
     TASK_FLAG(TASK_SET_ENS   ) = 0
     TASK_FLAG(TASK_SET_TGSD  ) = 0
     TASK_FLAG(TASK_SET_DBS   ) = 0
     TASK_FLAG(TASK_SET_SRC   ) = 0
     TASK_FLAG(TASK_RUN_FALL3D) = 0
     TASK_FLAG(TASK_POS_ENS   ) = 1
     TASK_FLAG(TASK_POS_VAL   ) = 0
  case('POSVAL')
     TASK_FLAG(TASK_RUN_ALL   ) = 0
     TASK_FLAG(TASK_SET_ENS   ) = 0
     TASK_FLAG(TASK_SET_TGSD  ) = 0
     TASK_FLAG(TASK_SET_DBS   ) = 0
     TASK_FLAG(TASK_SET_SRC   ) = 0
     TASK_FLAG(TASK_RUN_FALL3D) = 0
     TASK_FLAG(TASK_POS_ENS   ) = 0
     TASK_FLAG(TASK_POS_VAL   ) = 1
  case default
     if(master_world) call inpout_print_args
     call parallel_hangup(MY_ERR)
  end select
  !
  !*** Decodes the second argument (input file)
  !
  call get_command_argument (2,MY_FILES%file_inp)
  !
  !*** Check that the input file exists
  !
  if(master_world) call inpout_check_file(MY_FILES%file_inp,MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip,COMM_WORLD)
  if(MY_ERR%flag.ne.0) then
      if(master_world) call inpout_print_message(MY_ERR%message)
      call parallel_hangup(MY_ERR)
  end if
  !
  !*** Decodes optional arguments:
  !*** 3 positional arguments: mproc(1:3)
  !*** 1 named argument      : nens
  !
  iargs = 3
  iproc = 1
  read_arguments: do
    if(iargs.gt.narg) EXIT read_arguments
    call get_command_argument (iargs,what)
    call upcase(what)
    if(what.eq.'-NENS') then
        iargs = iargs + 1
        if(iargs.le.narg) then
            call get_command_argument (iargs,what)
            nens = INT(stof(what,LEN_TRIM(what)))
        end if
    elseif(iproc.le.3) then
        mproc(iproc) = INT(stof(what,LEN_TRIM(what)))
        iproc = iproc + 1
    end if
    iargs = iargs + 1 ! Next argument
  end do read_arguments
  !
  !*** Check arguments
  !
  if(ANY(mproc.lt.1) .or. nens.lt.1) then
      if(master_world) call inpout_print_args
      call parallel_hangup(MY_ERR)
  end if
  !
  if(TASK_FLAG(TASK_POS_VAL).eq.1) then
    if(nens.gt.1) then                           ! no ensemble allowed for TASK_POS_VAL
       if(master_world) call inpout_print_args
       call parallel_hangup(MY_ERR)
    else if(mproc(3).gt.1) then
       if(master_world) call inpout_print_args   ! no z paralellism for TASK_POS_VAL
       call parallel_hangup(MY_ERR)
    end if
  end if
  !
  !*** Create communicator for model tasks (instances)
  !
  if(mproc(1)*mproc(2)*mproc(3)*nens.ne.npes_world) then
      if(master_world) call inpout_print_args
      call parallel_hangup(MY_ERR)
  else
      call parallel_init_model(nens,MY_ERR)
      call inpout_get_problemname(MY_FILES,MY_ERR)
  end if
  !
  !*** Set if ensembles need to be run
  !
  if(nens.gt.1) then
     if(TASK_FLAG(TASK_SET_TGSD  ).eq.1) TASK_FLAG(TASK_SET_ENS) = 1
     if(TASK_FLAG(TASK_SET_DBS   ).eq.1) TASK_FLAG(TASK_SET_ENS) = 1
     if(TASK_FLAG(TASK_SET_SRC   ).eq.1) TASK_FLAG(TASK_SET_ENS) = 1
     if(TASK_FLAG(TASK_RUN_FALL3D).eq.1) TASK_FLAG(TASK_SET_ENS) = 1
     if(TASK_FLAG(TASK_POS_ENS   ).eq.1) TASK_FLAG(TASK_SET_ENS) = 1  ! unnecessary for post-process
     if(TASK_FLAG(TASK_POS_VAL   ).eq.1) TASK_FLAG(TASK_SET_ENS) = 0
  end if
  !
  !*** Display initialization information
  !
  if(master_world) call inpout_print_greeting(npes_world,MY_FILES%file_inp)
  !
  !*** Calls the different tasks
  !
  if(TASK_FLAG(TASK_SET_ENS).eq.1) then
      if(master_world) call inpout_print_message("Running SetEns task...")
      call task_SetEns
  end if
  !
  if(TASK_FLAG(TASK_SET_TGSD).eq.1) then
     if(master_world) call inpout_print_message("Running SetTgsd task...")
     call task_SetTgsd
  end if
  !
  if(TASK_FLAG(TASK_SET_DBS).eq.1) then
     if(master_world) call inpout_print_message("Running SetDbs task...")
     call task_SetDbs
  end if
  !
  if(TASK_FLAG(TASK_SET_SRC).eq.1) then
     if(master_world) call inpout_print_message("Running SetSrc task...")
     call task_SetSrc
  end if
  !
  if(TASK_FLAG(TASK_RUN_FALL3D).eq.1) then
     if(master_world) call inpout_print_message("Running FALL3D solver...")
     call task_Fall3d
  end if
  !
  if(TASK_FLAG(TASK_POS_ENS).eq.1) then
     if(master_world) call inpout_print_message("Running PosEns task...")
     call task_PosEns
  end if
  !
  if(TASK_FLAG(TASK_POS_VAL).eq.1) then
     if(master_world) call inpout_print_message("Running PosVal task...")
     call task_PosVal
  end if
  !
  !*** Ends MPI
  !
  call parallel_hangup(MY_ERR)
  !
  end program Fall3d_task_manager
