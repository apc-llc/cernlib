*
* $Id: fastgq.F,v 1.1.1.1 1996/03/07 15:18:22 mclareni Exp $
*
* $Log: fastgq.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:22  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      subroutine fastgq(jobnum,chmess,irc)
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
 
      record /itmlst/ qui_list(3)
 
      integer*4    iosb(2)
      include '($quidef)'
      include '($jbcmsgdef)'
      include '($sjcdef)'
 
      character*(*) chmess
 
      irc = 0
*
*     Issue batch query for specified job entry
*
      qui_list(1).buffer_length           = 4
      qui_list(1).item_code               = qui$_job_status
      qui_list(1).buffer_address          = %loc(jobsta)
      qui_list(1).return_length_address   = %loc(lenjob)
 
      qui_list(2).buffer_length           = 4
      qui_list(2).item_code               = qui$_search_number
      qui_list(2).buffer_address          = %loc(jobnum)
      qui_list(2).return_length_address   = %loc(lenjob)
 
      qui_list(3).end_list                = 0
 
      status = sys$getquiw
     +   (,%val(qui$_display_entry),,qui_list,iosb,,)
      if (.not. status) call lib$signal(%val(status))
*
*     Has job completed?
*
      if(iosb(1).eq.jbc$_nosuchent) then
         chmess = 'COMPLETED'
      elseif(iosb(1).eq.jbc$_normal) then
         if(jobsta.eq.qui$m_job_aborting) then
            chmess = 'ABORTING'
         elseif(jobsta.eq.qui$m_job_executing) then
            chmess = 'EXECUTING'
         elseif(jobsta.eq.qui$m_job_pending) then
            chmess = 'PENDING'
         else
            irc = jobsta
            WRITE(CHMESS,9001) IRC,IRC
         endif
      else
         irc = iosb(1)
         WRITE(CHMESS,9001) IRC,IRC
      endif
 
9001  format(' unexpected return code ',I10,' (hex = ',Z8,
     +       ') from SYS$GETQUIW')
 
      end
