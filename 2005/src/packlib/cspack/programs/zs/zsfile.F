*
* $Id: zsfile.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zsfile.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZSFILE(CHMAIL)
*
*         to open a RZ file remotely
*
#include "cspack/czdir.inc"
      COMMON/QUEST/IQUEST(100)
      CHARACTER*58 CFNAME
      CHARACTER*64 CHFILE
      CHARACTER*8 STAT
      CHARACTER*4 CHOPT,CHOPTT
      CHARACTER*5 CHREC
      CHARACTER*3 CHLUN
      CHARACTER*5 CHDIR
      CHARACTER*(*) CHMAIL
      CHARACTER*8   CHTAG(1)
#if defined(CERNLIB_OS9)
      CHARACTER*(*) LOSHFT
#endif
*
*          Decode message from host
*
      CHOPT=' '
      NCH=LENOCC(CHMAIL)
      IF(CHMAIL(NCH-1:NCH-1).EQ.' ')THEN
         CHOPT=CHMAIL(NCH:NCH)
         CALL CLTOU(CHOPT)
         NCH=NCH-2
      ENDIF
      CFNAME=CHMAIL(1:NCH)
#if defined(CERNLIB_UNIX)
      CALL CUTOL(CFNAME)
#endif
#if !defined(CERNLIB_UNIX)
      CALL CLTOU(CFNAME)
#endif
#if defined(CERNLIB_OS9)
      cfname = loshft(chmail(1:nch))
#endif
*
*     Hunt for a free unit
*
      IF(NCHRZ.EQ.0) THEN
         LUN = 11
         GOTO 20
      ELSE
         DO 10 I=1,MAXFIL
            IF(LUNRZ(I).EQ.0) THEN
               LUN = I + 10
               GOTO 20
            ENDIF
10       CONTINUE
      ENDIF
 
      CALL CZPUTA('3 Maximum number of remote files reached',ISTAT)
      GO TO 99
 
20    CONTINUE
 
      IOPTN=INDEX(CHOPT,'N')
      IPASS=0
*
      WRITE(CHDIR,4000)LUN
 4000 FORMAT('LUN',I2)
*
      LRECL  = 0
      CHOPTT = ' '
      IF(IOPTN.NE.0) THEN
         CHOPTT = 'N'
         LRECL  = 1024
      ENDIF
  2   CONTINUE
      CALL RZOPEN(LUN,CHDIR,CFNAME,CHOPTT,LRECL,ISTAT)
*
  90  IF(ISTAT.NE.0)THEN
         CALL CZPUTA('3 Cannot open remote file '//CFNAME,ISTAT)
         GO TO 99
      ENDIF
 
      IF(IOPTN.EQ.0)THEN
         CALL RZFILE(LUN,CHDIR,CHOPT)
      ELSE
         CHTAG(1) = 'HBOOK-ID'
         CALL RZMAKE(LUN,CHDIR,1,'I',CHTAG,1000,' ')
      ENDIF
 
      IF(IQUEST(1).NE.0)THEN
         ISTAT=1
         GO TO 90
      ENDIF
*
      NCHRZ         = NCHRZ+1
      CHRZ(NCHRZ)   = CHDIR
      FLNAME(NCHRZ) = CFNAME
      LUNRZ(NCHRZ)  = LUN
      IDIR=NCHRZ
*
      CALL CZPUTA('1 Connecting remote file '//CFNAME(1:NCH)//
     +            ' as '//CHDIR,ISTAT)
*
  99  END
 
