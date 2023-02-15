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
  integer(ip)           :: narg
  character(len=s_name) :: what
  !
  !*** Initialize MPI and assign grid processors
  !
  call parallel_startup(MY_ERR)
  !
  !*** Gets and decodes program call arguments
  !
  narg = command_argument_count()
  !
  if(narg.lt.2) then
     if(master) call inpout_print_args
     call parallel_hangup(MY_ERR)
  end if
  !
  ! Decodes the first argument (task)
  !
  call get_command_argument(1,what)
  call upcase(what)
  select case(what)
  case('SETTGSD')
     TASK_FLAG(TASK_RUN_ALL  )  = 0
     TASK_FLAG(TASK_SET_TGSD  ) = 1
     TASK_FLAG(TASK_SET_DBS   ) = 0
     TASK_FLAG(TASK_SET_SRC   ) = 0
     TASK_FLAG(TASK_RUN_FALL3D) = 0
  case('SETDBS')
     TASK_FLAG(TASK_RUN_ALL  )  = 0
     TASK_FLAG(TASK_SET_TGSD  ) = 0
     TASK_FLAG(TASK_SET_DBS   ) = 1
     TASK_FLAG(TASK_SET_SRC   ) = 0
     TASK_FLAG(TASK_RUN_FALL3D) = 0
  case('SETSRC')
     TASK_FLAG(TASK_RUN_ALL  )  = 0
     TASK_FLAG(TASK_SET_TGSD  ) = 0
     TASK_FLAG(TASK_SET_DBS   ) = 0
     TASK_FLAG(TASK_SET_SRC   ) = 1
     TASK_FLAG(TASK_RUN_FALL3D) = 0
  case('FALL3D')
     TASK_FLAG(TASK_RUN_ALL  )  = 0
     TASK_FLAG(TASK_SET_TGSD  ) = 0
     TASK_FLAG(TASK_SET_DBS   ) = 0
     TASK_FLAG(TASK_SET_SRC   ) = 0
     TASK_FLAG(TASK_RUN_FALL3D) = 1
  case('ALL')
     TASK_FLAG(TASK_RUN_ALL  )  = 1
     TASK_FLAG(TASK_SET_TGSD  ) = 1
     TASK_FLAG(TASK_SET_DBS   ) = 1
     TASK_FLAG(TASK_SET_SRC   ) = 1
     TASK_FLAG(TASK_RUN_FALL3D) = 1
  case default
     if(master) call inpout_print_args
     call parallel_hangup(MY_ERR)
  end select
  !
  ! Decodes the second argument (file name)
  !
  call get_command_argument (2,MY_FILES%file_inp)
  call inpout_get_problemname(MY_FILES,MY_ERR)
  !
  ! Decodes other arguments (mproc)
  !
  if(narg.ge.3) then
     call get_command_argument (3,what)
     mproc(1) = INT(stof(what,LEN_TRIM(what)))
     if(mproc(1).eq.0) then
        if(master) call inpout_print_args
        call parallel_hangup(MY_ERR)
     end if
  else
     mproc(1) = 1
     mproc(2) = 1
     mproc(3) = 1
  end if
  !
  if(narg.ge.4) then
     call get_command_argument (4,what)
     mproc(2) = INT(stof(what,LEN_TRIM(what)))
     if(mproc(2).eq.0) then
        if(master) call inpout_print_args
        call parallel_hangup(MY_ERR)
     end if
  end if
  !
  if(narg.ge.5) then
     call get_command_argument (5,what)
     mproc(3) = INT(stof(what,LEN_TRIM(what)))
     if(mproc(3).eq.0) then
        if(master) call inpout_print_args
        call parallel_hangup(MY_ERR)
     end if
  end if
  !
  !*** Calls the different tasks
  !
  if(TASK_FLAG(TASK_SET_TGSD).eq.1) then
     call task_SetTgsd
  end if
  !
  if(TASK_FLAG(TASK_SET_DBS).eq.1) then
     if(mproc(1)*mproc(2)*mproc(3).ne.nproc) then
        if(master) call inpout_print_args
        call parallel_hangup(MY_ERR)
     end if
     call task_SetDbs
  end if
  !
  if(TASK_FLAG(TASK_SET_SRC).eq.1) then
     if(mproc(1)*mproc(2)*mproc(3).ne.nproc) then
        if(master) call inpout_print_args
        call parallel_hangup(MY_ERR)
     end if
     call task_SetSrc
  end if
  !
  if(TASK_FLAG(TASK_RUN_FALL3D).eq.1) then
     if(mproc(1)*mproc(2)*mproc(3).ne.nproc) then
        if(master) call inpout_print_args
        call parallel_hangup(MY_ERR)
     end if
     call task_Fall3d
  end if
  !
  !*** Ends MPI
  !
  call parallel_hangup(MY_ERR)
  !
end program Fall3d_task_manager
