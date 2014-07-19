*
* $Id: getcom.F,v 1.1.1.1 1996/03/06 10:47:20 mclareni Exp $
*
* $Log: getcom.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:20  mclareni
* Zebra
*
*
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
#if defined(CERNLIB_VAX)
c     *****************************************************************
      logical*4 function get_command_line(command_tables, command_verb)
c     *****************************************************************
c     Purpose : A general, all-purpose routine to get the
c               command line and submit it for CLI-parsing,
c               which checks for COMPLETENESS and VALIDITY
c     Arguments :
c       command_tables= read only.  This is the name of the
c                       command table being linked with the
c                       program of interest.  It must be declared
c                       EXTERNAL in the calling program.
c       command_verb=   read only.  This is the command prefix which
c                       is pre-appended to the qualifiers/parameters
c                       so that it will be recognized in the above
c                       command table.
c     Notes: Uses the routine CLI_ERROR_HANDLER to handle any
c     errors encountered, so that the stack will not be unwound
c     and there will not be a TRACEBACK on errors.
 
      implicit none
      integer*4     command_tables  ! Tables which define the ``grammar''
      character*(*) command_verb    ! VERB which is defined
      external      lib$get_input   ! Prompt routine, used by CLI$DCL_PARSE
      integer*4     lib$get_foreign ! Get entered command line
      integer*4     cli$dcl_parse   ! Parse for COMPLETENESS and VALIDITY
      character*255 cmd_line        ! Store command line
      integer*2     len             ! Character count (Note: Integer*2)
      integer*4     status          ! Store status returns
 
c     The call to CLI$DCL_PARSE by default will unwind the stack and
c     give a TRACEBACK for any illegal qualifiers or parameters.  We
c     do NOT want this traceback, so we will establish our own error
c     handler for this code segment.  It must be declared as EXTERNAL.
      integer*4     cli_error_handler
      external      cli_error_handler
      integer*4     old_handler     ! Store old handler address
      integer*4     lib$establish   ! Establish new error handler
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Execution begins:
#if !defined(CERNLIB_QMALPH)
c     Temporarily establish the alternate error handler.  The value
c     returned by LIB$ESTABLISH is the address of the error handler
c     currently in effect.
      old_handler= lib$establish(cli_error_handler)
#endif
 
c     Get the input command line, using LIB$GET_FOREIGN.  If the status
c     is NOT good, then signal an error.
      status= lib$get_foreign(cmd_line,,len)
      if ( .not.status ) then
          call lib$signal( %val(status) )
      else
c         Preappend the appropriate command verb to the command line.
c         Make entirely certain that there is a blank separating the
c         verb from the parameters and qualifiers so that it is
c         interpreted correctly.
          cmd_line= command_verb//' '//cmd_line
 
c         Submit to CLI$DCL_PARSE to make sure it is a valid,
c         complete command line.  LIB$GET_INPUT will be used
c         to prompt for any missing required parameters or
c         qualifier values.
          status= cli$dcl_parse( cmd_line,command_tables,lib$get_input )
      end if
 
#if !defined(CERNLIB_QMALPH)
c     Re-establish the original error handler:
      call lib$establish(old_handler)
#endif
c     And exit with status determined by the CLI$DCL_PARSE call:
      get_command_line= status
      return
      end
#endif
