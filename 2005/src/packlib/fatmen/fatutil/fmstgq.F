*
* $Id: fmstgq.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fmstgq.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSTGQ(VSN,VID,IFILE,LAB,IRC)
*CMZ :          20/05/92  10.57.25  by  Jamie Shiers
*-- Author :    Jamie Shiers   20/05/92
      CHARACTER*(*) VSN,VID,LAB
      CHARACTER*255 COMAND,CHLINE
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      CHARACTER*255 CHSTFL
      LOGICAL       IEXIST
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_VAXTAP))
      CHARACTER*255 CHGRP
#include "fatmen/fatlab.inc"
#endif
#if defined(CERNLIB_PREFIX)
      CHARACTER*6   CHVSN,CHLAB,CHSEQ
      CHARACTER*15  CHVID,XVID,XVIP
#endif
#if !defined(CERNLIB_PREFIX)
      CHARACTER*6   CHVSN,CHVID,CHLAB,CHSEQ
#endif
#include "fatmen/fatbug.inc"
 
      IRC  = 99
*
*     Issue STAGE QUERY for specified volume and file
*
      LVSN = LENOCC(VSN)
      LVID = LENOCC(VID)
      LLAB = LENOCC(LAB)
 
      CHVSN = VSN(1:LVSN)
      CHVID = VID(1:LVID)
      CHLAB = LAB(1:LLAB)
 
      CALL FMITOC(IFILE,CHSEQ,LFSEQ)
 
#if defined(CERNLIB_PREFIX)
*
*     Swap vid & prefix
*
      LDOT  = INDEX(VID(1:LVID),'.')
      XVIP  = VID(LDOT+1:LVID)
      XVID  = VID(1:LDOT-1)
      CHVID = XVID(1:LDOT-1) // '.' // XVIP(1:LVID-LDOT)
#endif
 
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_HEPVM))
*
*     Build STAGE Query command
*
      COMAND = 'EXEC STAGE QUERY ' // CHVSN(1:LVSN) // '.' //
     +          CHSEQ(1:LFSEQ)  // '.' // CHLAB(1:LLAB) // '.'
     +          // CHVID(1:LVID) // ' (LIFO'
      LC = LENOCC(COMAND)
      IF(IDEBFA.GE.2) PRINT *,'FMSTGQ. running ', COMAND(1:LC)
      CALL VMCMS(COMAND(1:LC),IRC)
 
      IF(IRC.EQ.0) THEN
*
*     Get answer and extract file size
*
         CALL VMRTRM(CHLINE,LLINE)
 
         IF(IDEBFA.GE.1) PRINT *,'FMSTGQ. ',CHLINE(1:LLINE)
 
         IF(INDEX(CHLINE(1:LLINE),'unknown to VMSTAGE').NE.0) THEN
            IRC = -1
         ELSE
            IRC = 0
         ENDIF
      ENDIF
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_VAXTAP))
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
 
      JL = ICNTH(CHLAB,IBMLAB,3)
      LCHGRP = LENOCC(CHGRP)
      CHSTFL = CHGRP(1:LCHGRP)//CHVSN(1:LVSN)//'_'//CHVID(1:LVID)
     +         //'.'//CHSEQ(1:LFSEQ)//'_'//VAXLAB(JL)
      LCHST  = LENOCC(CHSTFL)
*
*     Does this file exist?
*
      INQUIRE(FILE=CHSTFL(1:LCHST),EXIST=IEXIST)
 
      IF(IEXIST) THEN
         IRC = 0
      ELSE
         IRC = -1
      ENDIF
 
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_SHIFT))
#endif
      END
