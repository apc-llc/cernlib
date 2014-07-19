*
* $Id: fmfget.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fmfget.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_SHIFT)
      SUBROUTINE FMFGET(CHPOOL,CHUSER,CHFILE,CHPATH,IMODE,CHOPT,IRC)
*
*     Fortran interface to sfget
*
#include "fatmen/slate.inc"
#include "fatmen/fatbug.inc"
      CHARACTER*(*) CHPOOL,CHUSER,CHFILE,CHPATH
      CHARACTER*255 SHPOOL,SHUSER,SHFILE
      CHARACTER*255 SHCOMM,SHUNAM,SHFNAM
 
      INTEGER       SYSTEMF
 
      LOGICAL       IEXIST
 
#include "fatmen/fatopts.inc"
 
      IRC    = 0
 
      LPOOL  = LENOCC(CHPOOL)
      LUSER  = LENOCC(CHUSER)
      LFILE  = LENOCC(CHFILE)
      SHPOOL = CHPOOL(1:LPOOL)
      SHUSER = CHUSER(1:LUSER)
      SHFILE = CHFILE(1:LFILE)
 
      CHPATH = ' '
*
*     Get temporary file name
*
   10    CONTINUE
         CALL FMFNME(SHUNAM)
         LUNAM = LENOCC(SHUNAM)
         INQUIRE(FILE=SHUNAM(1:LUNAM),EXIST=IEXIST)
         IF(IEXIST) THEN
            IC = SLEEPF(1)
            GO TO 10
         ENDIF
 
         IF(IDEBFA.GE.1) PRINT *,'FMFGET. using temporary file ',
     +      SHUNAM(1:LUNAM)
*
*     Issue SFGET to obtain full shift pathname
*
         IF(IDEBFA.GE.1) PRINT *,'FMFGET. shift pool = ',
     +      SHPOOL(1:LPOOL),' shift user = ',SHUSER(1:LUSER),
     +      ' shift file ',SHFILE(1:LFILE)
 
         SHCOMM    = ' '
 
         IF(IMODE.EQ.0) THEN
            SHCOMM = 'sfget -k '
            LCOMM  = 9
         ELSE
            SHCOMM = 'sfget '
            LCOMM  = 6
         ENDIF
 
         IF(IOPTD.NE.0) THEN
            IF(LPOOL.GT.0) THEN
                SHCOMM(LCOMM+1:) = ' -p '//SHPOOL(1:LPOOL)//' '
                LCOMM = LCOMM + LPOOL + 5
                IF(IDEBFA.GE.1) PRINT *,'FMFGET. shift pool = '//
     +             SHPOOL(1:LPOOL)
            ENDIF
            IF(LUSER.GT.0) THEN
                SHCOMM(LCOMM+1:) = ' -u '//SHUSER(1:LUSER) //' '
                LCOMM = LCOMM + LUSER + 5
                IF(IDEBFA.GE.1) PRINT *,'FMFGET. shift user = '//
     +             SHUSER(1:LUSER)
            ENDIF
         ENDIF
 
         SHCOMM(LCOMM+1:) = SHFILE(1:LFILE)//' > '//SHUNAM(1:LUNAM)
         LCOMM = LCOMM + LFILE + LUNAM + 3
 
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9001) SHCOMM(1:LCOMM)
 9001 FORMAT(' FMFGET. issuing ',A)
         IRC = SYSTEMF(SHCOMM(1:LCOMM))
 
         IF(IRC.NE.0) THEN
            PRINT *,'FMFGET. return code ',IRC,' from SFGET'
            RETURN
         ENDIF
*
*     Now check if sfget was successful...
*
         CALL CIOPEN(LUNPTR,'r',SHUNAM(1:LUNAM),IRC)
         SHFNAM = ' '
         CALL CIGET(LUNPTR,SHFNAM,255,NREAD,IRC)
         CALL CICLOS(LUNPTR)
         LFNAM = LENOCC(SHFNAM)
         LNEWL = INDEX(SHFNAM,CHAR(10))
         IF(LNEWL.NE.0)  THEN
            SHFNAM(LFNAM:) = ' '
            LFNAM = LNEWL - 1
         ENDIF
         IF(IDEBFA.GE.0) PRINT *,'FMFGET. shift file name is ',
     +      SHFNAM(1:LFNAM)
 
         IF(INDEX(SHFNAM,'/shift').NE.1) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMFGET. error from ', 'sfget - ',
     +      SHFNAM(1:LFNAM)
            IRC = -1
            RETURN
*
*     Delete temporary file only if sfget worked
*
         ELSE
            CHPATH = SHFNAM(1:LFNAM)
            CALL UNLINKF(SHUNAM(1:LUNAM))
         ENDIF
 
      IS(1) = LFNAM
 
      END
#endif
