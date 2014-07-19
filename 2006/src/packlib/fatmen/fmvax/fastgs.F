*
* $Id: fastgs.F,v 1.1.1.1 1996/03/07 15:18:22 mclareni Exp $
*
* $Log: fastgs.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:22  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      subroutine fastgs(chfile,chgen,chcomm,jobnum,chmess,irc)
      implicit integer (s)
 
      structure /itmlst/
         union
            map
               integer*2 buffer_length
               integer*2 item_code
               integer*4 buffer_address
               integer*4 return_length_address
            endmap
            map
               integer*4 end_list /0/
            endmap
         end union
      end structure
 
      record /itmlst/ jbc_list(9)
 
      integer*4    iosb(2)
      include '($sjcdef)'
      include '($jbcmsgdef)'
      character*(*) chfile,chgen,chcomm,chmess
      character*255 chque,chdcl,chstat,chlogs
 
#include "fatmen/fatbug.inc"
 
      irc   = 0
      lfile = lenocc(chfile)
      lgen  = lenocc(chgen)
      lcomm = lenocc(chcomm)
*
*     Create temporary command file
*
      ic = lib$get_lun(luncom)
      if(.not.ic) call lib$signal(%val(ic))
 
      chdcl = chfile(1:lfile) // '.com'
      ldcl  = lfile + 4
 
      open(luncom,status='new',form='formatted',
     +     carriagecontrol='list',recl=255,
     +     file=chdcl(1:ldcl),iostat=istat)
 
      if(istat.ne.0) then
         print *,'FASTGS. cannot create temporary file ',
     +      chdcl(1:ldcl)
         irc = -1
         chmess = 'error creating temporary file'
         return
      endif
 
      write(luncom,'(A)') '$SET NOON '
      write(luncom,'(A)') '$SET VERIFY'
      write(luncom,'(A)') chcomm(1:lcomm)
 
      close(luncom)
 
      ic = lib$free_lun(luncom)
*
*     Directory for log files
*
      chlogs = 'STAGE_LOGS:'
      llogs  = 11
*
*     Queue name
*
      chque  = 'STAGE_' // chgen(1:lgen) // 'S'
      lque   = lgen + 7
*
*     jbc item codes:
*
*        sjc$_delete_file - delete command file on job completion
*        sjc$_entry_number_output - get back job entry number
*        sjc$_job_status_output - job status message
*
 
      jbc_list(1).buffer_length           = ldcl
      jbc_list(1).item_code               = sjc$_file_specification
      jbc_list(1).buffer_address          = %loc(chdcl)
      jbc_list(1).return_length_address   = %loc(lenfile)
 
      jbc_list(2).buffer_length           = llogs
      jbc_list(2).item_code               = sjc$_log_specification
      jbc_list(2).buffer_address          = %loc(chlogs)
      jbc_list(2).return_length_address   = %loc(lenl)
 
      jbc_list(3).buffer_length           = lque
      jbc_list(3).item_code               = sjc$_queue
      jbc_list(3).buffer_address          = %loc(chque)
      jbc_list(3).return_length_address   = %loc(lenq)
 
      jbc_list(4).buffer_length           = 4
      jbc_list(4).item_code               = sjc$_delete_file
      jbc_list(4).buffer_address          = %loc(0)
      jbc_list(4).return_length_address   = %loc(lendel)
 
      jbc_list(5).buffer_length           = 4
      jbc_list(5).item_code               = sjc$_no_notify
      jbc_list(5).buffer_address          = %loc(0)
      jbc_list(5).return_length_address   = %loc(lennot)
 
      jbc_list(6).buffer_length           = 4
      jbc_list(6).item_code               = sjc$_no_log_spool
      jbc_list(6).buffer_address          = %loc(0)
      jbc_list(6).return_length_address   = %loc(lenspl)
 
      jbc_list(7).buffer_length           = 4
      jbc_list(7).item_code               = sjc$_entry_number_output
      jbc_list(7).buffer_address          = %loc(jobnum)
      jbc_list(7).return_length_address   = %loc(lenjob)
 
      jbc_list(8).buffer_length           = 255
      jbc_list(8).item_code               = sjc$_job_status_output
      jbc_list(8).buffer_address          = %loc(chmess)
      jbc_list(8).return_length_address   = %loc(lenmsg)
 
      jbc_list(9).end_list                = 0
 
      status = sys$sndjbcw
     +   (,%val(sjc$_enter_file),,jbc_list,iosb,,)
      if (.not. status) call lib$signal(%val(status))
*
*     jbc$_normal
*
      if (iosb(1) .eq. jbc$_normal) then
*
*     Send back success message and job entry number
*
 
      else
 
*
*     Send back failure message and error text
*
         status = sys$getmsg(%val(iosb(1)),lenmsg,chmess,,)
         if (.not. status) call lib$signal(%val(status))
         irc    = iosb(1)
         jobnum = -1
 
      endif
 
      end
