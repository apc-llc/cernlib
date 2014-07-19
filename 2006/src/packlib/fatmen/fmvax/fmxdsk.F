*
* $Id: fmxdsk.F,v 1.1.1.1 1996/03/07 15:18:22 mclareni Exp $
*
* $Log: fmxdsk.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:22  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_VAXVMS)
      SUBROUTINE FMXDSK(CHDSN,DSN,IRC)
*
*     If DSN contains logical name that is a search list,
*     find disk with most space and return fully qualified name
*
#include "fatmen/slate.inc"
#include "fatmen/fatbug.inc"
      CHARACTER*(*) CHDSN,DSN
      CHARACTER*255 LOGNAM
 
      integer  sys$getdviw
      structure /itmlst/
         union
            map
               integer*2 buffer_length
               integer*2 item_code
               integer*4 buffer_address
               integer*4 return_length_address
            endmap
            map
               integer*4 end_list
            endmap
         end union
      end structure
 
      record /itmlst/ dvi_list(5)
 
      include '($dvidef)'
      include '($ssdef)'
 
      data     ivalid/Z0000800/
      data     mntver/Z0004000/
 
      IRC  = 0
      LDSN = LENOCC(CHDSN)
      DSN  = CHDSN(1:LDSN)
 
      IF(IDEBFA.GE.1) PRINT *,'FMXDSK. enter for DSN = ',DSN(1:LDSN)
 
      LEND = INDEX(DSN(1:LDSN),':') -1
      IF(LEND.LE.0) RETURN
*
*     Loop over all elements looking for the disk with max(freeblocks)
*
      IOFF = 0
      MAX  = 0
   10 CONTINUE
      LOGNAM = ' '
      CALL FMGTEL(DSN(1:LEND),LOGNAM,'LNM$SYSTEM',IOFF,IC)
      LLOG = IS(1)
      IF(LLOG.EQ.0.OR.IC.NE.0) GOTO 30
 
      IF(IDEBFA.GE.1) PRINT *,'FMXDSK. checking element # ',IOFF+1,
     +   ' of ',DSN(1:LEND)
 
      dvi_list(1).buffer_length           = 4
      dvi_list(1).item_code               = dvi$_mnt
      dvi_list(1).buffer_address          = %loc(ismnt)
      dvi_list(1).return_length_address   = %loc(lenmnt)
 
      dvi_list(2).buffer_length           = 4
      dvi_list(2).item_code               = dvi$_swl
      dvi_list(2).buffer_address          = %loc(islock)
      dvi_list(2).return_length_address   = %loc(lenlck)
 
      dvi_list(3).buffer_length           = 4
      dvi_list(3).item_code               = dvi$_sts
      dvi_list(3).buffer_address          = %loc(istat)
      dvi_list(3).return_length_address   = %loc(lensta)
 
      dvi_list(4).buffer_length           = 4
      dvi_list(4).item_code               = dvi$_freeblocks
      dvi_list(4).buffer_address          = %loc(nfree)
      dvi_list(4).return_length_address   = %loc(lenblk)
 
      dvi_list(5).end_list                = 0
 
      ic = sys$getdviw(,,LOGNAM(1:LLOG),dvi_list,,,,)
*
*     Does the device exist?
*
      if(ic.eq.ss$_nosuchdev) then
         if(idebfa.ge.1) print 9001,lognam(1:llog)
9001  format(' FMXDSK. device ',A,' rejected - no such device')
         goto 20
      endif
*
*     Is it mounted?
*
      if(ismnt.eq.0)          then
         if(idebfa.ge.1) print 9002,lognam(1:llog)
9002  format(' FMXDSK. device ',A,' rejected - not mounted')
         goto 20
      endif
*
*     Is it s/w write locked?
*
      if(islock.ne.0)         then
         if(idebfa.ge.1) print 9003,lognam(1:llog)
9003  format(' FMXDSK. device ',A,' rejected - write locked')
         goto 20
      endif
*
*     Is it s/w valid?
*
      if(iand(istat,ivalid).eq.0) then
         if(idebfa.ge.1) print 9004,lognam(1:llog)
9004  format(' FMXDSK. device ',A,' rejected - not valid')
         goto 20
      endif
*
*     Is it undergoing mount verification?
*
      if(iand(istat,mntver).ne.0) then
         if(idebfa.ge.1) print 9005,lognam(1:llog)
9005  format(' FMXDSK. device ',A,' rejected - in mount verification')
         goto 20
      endif
 
      IF(NFREE.GT.MAX) THEN
         MAX  = NFREE
         JMAX = IOFF
      ENDIF
 
      IF(IDEBFA.GE.1) PRINT *,'FMXDSK. disk ',LOGNAM(1:LLOG),
     +   ' has ',NFREE,' blocks'
 
   20 CONTINUE
      IOFF = IOFF + 1
      GOTO 10
 
   30 CONTINUE
*
*     Was it a search list?
*
      IF(MAX.EQ.0) RETURN
*
*     Yes, get member with most free blocks
*
      LOGNAM = ' '
      CALL FMGTEL(DSN(1:LEND),LOGNAM,'LNM$SYSTEM',JMAX,IC)
      LLOG = IS(1)
*
*     and stick it back in the DSN
*
      IF(INDEX(LOGNAM(1:LLOG),'.]').NE.0) THEN
         DSN = LOGNAM(1:LLOG-1) // DSN(LEND+3:LDSN)
         IS(1) = LLOG + LDSN - LEND - 2
      ELSEIF(INDEX(LOGNAM(1:LLOG),':').NE.0) THEN
         DSN = LOGNAM(1:LLOG) // DSN(LEND+2:LDSN)
         IS(1) = LLOG + LDSN - LEND - 1
      ELSE
         DSN = LOGNAM(1:LLOG) // DSN(LEND+1:LDSN)
         IS(1) = LLOG + LDSN - LEND
      ENDIF
 
      END
#endif
