*
* $Id: fmvinf.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmvinf.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMVINF(CHVID,MB,NFILES,CHOPT,IRC)
      CHARACTER*(*) CHVID
      CHARACTER*6   VID
      CHARACTER*255 CHTAG,CHOLD
      CHARACTER*6   COMAND
      CHARACTER*7   CHTYPE
      CHARACTER*6   CFILES,CHUSED
#include "fatmen/slate.inc"
#include "fatmen/tmsrep.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatopts.inc"
*
*     Options: S - set
*              G - get
*              I - increment (JFILES=JFILES+1, NUSED=NUSED+MB)
*
      IRC    = 0
      LVID   = LENOCC(CHVID)
      VID    = CHVID(1:LVID)
      NUSED  = MB
      JFILES = NFILES
 
      IF(IOPTI.NE.0) IOPTS = 1
 
#if !defined(CERNLIB_TMS)
      IF(IDEBFA.GE.0) PRINT *,
     +                'FMVINF. TMS option not installed on this node'
      IRC = 99
#endif
#if defined(CERNLIB_TMS)
*
*     In all cases, GET VOLINFO tag
*
      I = LENREP
      DO 1 JJ=1,LENREP
    1 TMSREP(JJ) = ' '
 
#endif
#if (defined(CERNLIB_TMS))&&(defined(CERNLIB_IN2P3))
      IF(IDEBFA.GE.3) PRINT *,'FMVINF. issuing ', 'TMS TAG VID '//
     +VID(1:LVID)//' GET SYSTEM '
 
      CALL FMSREQ('TMS     ', 'TAG VID '//VID(1:LVID) //' GET SYSTEM '
     +,IRC,TMSREP,I)
#endif
#if (defined(CERNLIB_TMS))&&(!defined(CERNLIB_IN2P3))
      IF(IDEBFA.GE.3) PRINT *,'FMVINF. issuing ', 'TMS TAG VID '//
     +VID(1:LVID)//' GET VOLINFO '
 
      CALL FMSREQ('TMS     ', 'TAG VID '//VID(1:LVID) //' GET VOLINFO '
     +,IRC,TMSREP,I)
#endif
#if defined(CERNLIB_TMS)
 
      LTAG = LENOCC(TMSREP(1))
      IF(IRC.NE.0) RETURN
      IF(LTAG.GT.0) THEN
         CHTAG = TMSREP(1)(1:LTAG)
         CALL CLTOU(CHTAG(1:LTAG))
         IF(IDEBFA.GE.3) PRINT *,'FMVINF. volinfo tag: ',
     +      CHTAG(1:LTAG)
      ELSE
         CHTAG = ' '
         IF(IDEBFA.GE.3) PRINT *,'FMVINF. volinfo tag is empty'
      ENDIF
*
*     Get the current values of MB and NFILES
*
      IF(IOPTG.NE.0.OR.IOPTI.NE.0) THEN
         NFOUND = 1
         IF(CHTAG(1:3).EQ.'MB=') THEN
            IFIRST = 4
         ELSE
            IFIRST = INDEX(CHTAG(1:LTAG),' MB=')
            IF(IFIRST.EQ.0) GOTO 20
            IFIRST = IFIRST + 4
         ENDIF
 
         NUSED = ICDECI(CHTAG,IFIRST,LTAG)
 
         IF(CHTAG(1:7).EQ.'NFILES=') THEN
            IFIRST = 8
         ELSE
            IFIRST = INDEX(CHTAG(1:LTAG),' NFILES=')
            IF(IFIRST.EQ.0) GOTO 20
            IFIRST = IFIRST + 8
         ENDIF
 
         JFILES = ICDECI(CHTAG,IFIRST,LTAG)
         IF(IOPTG.NE.0) THEN
            MB     = NUSED
            NFILES = JFILES
            NFOUND = 0
         ENDIF
 
   20    CONTINUE
 
         IF(IOPTG.NE.0) IRC = NFOUND
 
         IF(IOPTI.EQ.0) RETURN
         NUSED  = NUSED  + MB
         JFILES = JFILES + 1
      ENDIF
 
      IF(IOPTS.NE.0) THEN
*
*     Empty tag?
*
         IF(LTAG.EQ.0) THEN
            CALL FMITOC(JFILES,CFILES,LFILES)
            CALL FMITOC(NUSED,CHUSED,LUSED)
            CHTAG = 'NFILES='//CFILES(1:LFILES)//' MB='//CHUSED(1:LUSED)
            LTAG  = LFILES + LUSED + 11
 
         ELSE
*
*     Update MB and NFILES fields, leaving everything else alone
*
            CHOLD = CHTAG
            JL    = 1
            JR    = LTAG
            CHTAG = ' '
            LTAG  = 1
   10       CONTINUE
            ILEFT = ICNEXT(CHOLD,JL,JR)
            IF(ILEFT.LT.JR) THEN
               JL = IS(2)
*
*     Word is now in CHOLD(ILEFT:JL)
*
               IF(INDEX(CHOLD(ILEFT:JL),'MB=').EQ.1) THEN
                  CALL FMITOC(NUSED,CHUSED,LUSED)
                  CHTAG(LTAG:LTAG+LUSED+3) = 'MB='//CHUSED(1:LUSED)
                  LTAG = LTAG + LUSED + 4
               ELSEIF(INDEX(CHOLD(ILEFT:JL),'NFILES=').EQ.1) THEN
                  CALL FMITOC(JFILES,CFILES,LFILES)
                  CHTAG(LTAG:LTAG+LFILES+7) = 'NFILES='//
     +               CFILES(1:LFILES)
                  LTAG = LTAG + LFILES + 8
               ELSE
*
*     Copy asis
*
                  CHTAG(LTAG:LTAG+IS(1)) = CHOLD(ILEFT:IS(2))
                  LTAG = LTAG + IS(1) + 1
               ENDIF
 
               GOTO 10
 
            ENDIF
         ENDIF
*
*     Now set the new tag
*
         IF(IDEBFA.GE.3) PRINT *,'FMVINF. new volinfo tag: ',
     +      CHTAG(1:LTAG)
         I = LENREP
         CALL FMSREQ('TMS     ', 'TAG VID '//VID(1:LVID) //' SET '
     +   //'VOLINFO ' //CHTAG(1:LTAG),IRC,TMSREP,I)
      ENDIF
#endif
      END
