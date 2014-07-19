*
* $Id: fmstgc.F,v 1.1.1.1 1996/03/07 15:18:22 mclareni Exp $
*
* $Log: fmstgc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:22  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSTGC(CHFILE,ISIZE,JSIZE,IRC)
*CMZ :          20/05/92  10.57.25  by  Jamie Shiers
*-- Author :    Jamie Shiers   20/05/92
*
*     IRC = 0 : file found on disk
*          -1 : file not found
*     JSIZE   : file size
*
      CHARACTER*(*) CHFILE
      CHARACTER*255 CHSTFL,CHNAME
      LOGICAL       IEXIST
      CHARACTER*255 CHGRP
#include "fatmen/fatbug.inc"
#include "fatmen/fabalq.inc"
      PARAMETER     (MEGA=1024*1024)
      EXTERNAL      FMBALQ
 
      DATA          NENTRY/0/
      SAVE          NENTRY,CHGRP,LCHGRP
 
      IRC   = 99
      JSIZE = -1
#if defined(CERNLIB_VAXTAP)
      IF(NENTRY.EQ.0) THEN
*
*     Find staging group
*
         CALL FMSTGP(CHGRP,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMSTGQ. error checking STAGE',
     +      ' IRC = ',IRC
            IRC = -2
            RETURN
         ENDIF
         LCHGRP = LENOCC(CHGRP)
         NENTRY = 1
      ENDIF
 
      LFILE  = LENOCC(CHFILE)
      CHNAME = CHFILE(1:LFILE)
      CHSTFL = CHGRP(1:LCHGRP)//CHNAME(1:LFILE)
      LCHST  = LCHGRP + LFILE
*
*     Does this file exist?
*
      INQUIRE(FILE=CHSTFL(1:LCHST),EXIST=IEXIST)
 
      IF(IEXIST) THEN
         IRC = 0
*
*     Check file size on disk
*
         ISTAT = LIB$GET_LUN(LUN)
#include "fatmen/fatvaxrc.inc"
         OPEN(LUN,FILE=CHSTFL(1:LCHST),STATUS='OLD',
     +        FORM='UNFORMATTED',READONLY,SHARED,
     +        USEROPEN=FMBALQ,IOSTAT=ISTAT)
         JSIZE = (NBLOKS*512+.9999*MEGA)/MEGA
         ISTAT = LIB$FREE_LUN(LUN)
      ELSE
         IRC = -1
      ENDIF
#endif
      END
