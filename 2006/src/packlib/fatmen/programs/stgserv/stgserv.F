*
* $Id: stgserv.F,v 1.1.1.1 1996/03/07 15:17:47 mclareni Exp $
*
* $Log: stgserv.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:47  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
********************************************************************************
*       Program STAGE_SERVER                                                   *
*       --------------------                                                   *
*       J.D. Shiers     CERN/CN/AS 1992                                        *
*                                                                              *
*       Modification log:                                                      *
*       -----------------                                                      *
*                                                                              *
* 11/06/92 - add LOOK command : call FMLOOK to see if a given tape is in use   *
*                                                                              *
*                                                                              *
********************************************************************************
      implicit integer (s)
      character*80  chact,chent
      character*255 chfile,chgen,chcomm,chmess,jobnam
*
*     Get unit for network connection
*
      status = lib$get_lun(lun)
      if (.not. status) call lib$signal(%val(status))
*
*     Open network connection
*
      open(unit=lun,file='SYS$NET',status='old')
*
*     Read action: either SUBMIT, QUERY or LOOK
*     In case of QUERY, the job entry number is also supplied
*
      read(lun,'(A)',end=99,err=99) chact
 
      if(chact(1:5).eq.'QUERY') then
*
*     Get job entry number
*
         read(lun,'(A)',end=99,err=99) chent
         lchent = lenocc(chent)
         jobnum = icdeci(chent,1,lchent)
         print *,'STGSERV. stage query command received for job ',
     +      chent(1:lchent)
         call fastgq(jobnum,chmess,irc)
*
*     and send back status
*
         lmess = lenocc(chmess)
         write(lun,'(A)') chmess(1:lmess)
         write(lun,'(A)') ' '
         print *,'STGSERV. job status = ',chmess(1:lmess)
         goto 99
      elseif(chact(1:4).eq.'LOOK') then
*
*     Read tape parameters
*
         read(lun,'(A)',end=99,err=99) chfile
         lfile = lenocc(chfile)
*
*     Read generic device type
*
         read(lun,'(A)',end=99,err=99) chgen
         lgen = lenocc(chgen)
*
*     Look to see if their is a job accessing this tape
*     (Look in all STAGE batch queues as some devices, e.g.
*      8500, can read also 8200 tapes)
*
         call fmlook(chfile(1:lfile),'*STAGE*',jobnam,jobnum)
*
*     Send back the job entry number (0=no such job)
*
         call fmitoc(jobnum,chmess,lmess)
         write(lun,'(A)') chmess(1:lmess)
         write(lun,'(A)') jobnam(1:lenocc(jobnam))
         print *,'STGSERV. job number = ',chmess(1:lmess)
         goto 99
 
      elseif(chact(1:5).eq.'STAGE') then
*
*     Read tape parameters
*
         read(lun,'(A)',end=99,err=99) chfile
         lfile = lenocc(chfile)
*
*     Read generic device type
*
         read(lun,'(A)',end=99,err=99) chgen
         lgen = lenocc(chgen)
*
*     Read stage command
*
         read(lun,'(A)',end=99,err=99) chcomm
         lcomm = lenocc(chcomm)
 
         print *,'STGSERV. stage submit command received - ',
     +      chcomm(1:lcomm)
         print *,'STGSERV. generic device type = ',chgen(1:lgen),
     +      ' log file name (directory STAGE_LOGS:) = ',chfile(1:lfile)
 
         call fastgs(chfile(1:lfile),chgen(1:lgen),chcomm(1:lcomm),
     +      jobnum,chmess,irc)
      endif
*
*    Send back status information
*
      if(jobnum.ge.0) then
         call fmitoc(jobnum,chmess,lmess)
      else
         lmess = lenocc(chmess)
      endif
 
      write(lun,'(A)') chmess(1:lmess)
      write(lun,'(A)') ' '
 
      print *,'STGSERV. status from job submission = ',chmess(1:lmess)
 
99    continue
 
      call sys$exit(%val(1))
 
      end
