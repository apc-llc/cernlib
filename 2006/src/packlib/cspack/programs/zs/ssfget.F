*
* $Id: ssfget.F,v 1.2 1996/04/11 14:54:19 cernlib Exp $
*
* $Log: ssfget.F,v $
* Revision 1.2  1996/04/11 14:54:19  cernlib
* Zserv/pawserv used to be build from two patches; these were put both into this
* directory.
* The #includes in all files copied from the other directory had to updated.
*
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_UNIX)
      SUBROUTINE SSFGET(CHPOOL,CHUSER,CHPATH,CHFILE,IMODE,ISIZE,
     +                  CHOPT,IRC)
*
*     Fortran interface to sfget
*
#include "cspack/slate.inc"
      CHARACTER*(*) CHPOOL,CHUSER,CHFILE,CHPATH,CHOPT
      CHARACTER*255 SHPOOL,SHUSER,SHFILE,SHPATH
      CHARACTER*255 SHCOMM,SHUNAM,SHFNAM
      CHARACTER*12  CHDATE
      CHARACTER*8   CHRAND
 
      INTEGER       SYSTEMF
      INTEGER       CHDIRF
 
      LOGICAL       IEXIST
 
      IRC    = 0
 
      LPOOL  = LENOCC(CHPOOL)
      LUSER  = LENOCC(CHUSER)
      LPATH  = LENOCC(CHPATH)
      LFILE  = LENOCC(CHFILE)
 
      SHPOOL = CHPOOL(1:LPOOL)
      SHUSER = CHUSER(1:LUSER)
      SHPATH = CHPATH(1:LPATH)
      SHFILE = CHFILE(1:LFILE)
 
*
*     Check if link directory exists and create if necessary
*
      CALL GETWDF(SHPOOL)
      LPOOL = IS(1)
*
*     Send back current directory
*
      CALL CZPUTA('1'//SHPOOL(1:LPOOL),ISTAT)
 
      INQUIRE(FILE=SHUSER(1:LUSER),
     +        EXIST=IEXIST)
      IF(.NOT.IEXIST) THEN
         ISTAT = SYSTEMF('mkdir '//SHUSER(1:LUSER))
         CALL CZPUTA('2Creating link directory for user '//
     +               SHUSER(1:LUSER),ISTAT)
      ENDIF
*
*     Get temporary file name
*
   10 CONTINUE
      CALL DATIME(ID,IT)
      WRITE(CHDATE,'(I6.6,I4.4,I2.2)') ID,IT,IS(6)
      CALL CZRAND(CHRAND)
      SHUNAM = SHUSER(1:LUSER) // '.' // CHDATE // CHRAND
      LUNAM =  LENOCC(SHUNAM)
      INQUIRE(FILE=SHUNAM(1:LUNAM),EXIST=IEXIST)
      IF(IEXIST) THEN
         IC = SLEEPF(1)
         GO TO 10
      ENDIF
 
*
*     Issue SFGET to obtain full shift pathname
*
      SHCOMM = ' '
*
*     In case of one pool per experiment
*
      WRITE(SHCOMM,9001) SHPOOL(1:LPOOL),SHUSER(1:LUSER),
     +                   SHFILE(1:LFILE),
     +                   SHUNAM(1:LUNAM)
 9001 FORMAT('/usr/local/bin/sfget -link',A,'/',A,1X,A,
     +       ' > ',A,' 2>&1')
      LCOMM = LENOCC(SHCOMM)
 
      ISTAT = SYSTEMF(SHCOMM(1:LCOMM))
 
*     IF(ISTAT.NE.0) CALL CZPUTA('2sfget failed',ISTAT)
*
*     Now check if sfget was successful...
*
      CALL CIOPEN(LUNPTR,'r',SHUNAM(1:LUNAM),IRC)
      SHFNAM = ' '
      CALL CIGET(LUNPTR,SHFNAM,255,NREAD,IRC)
      CALL CICLOS(LUNPTR)
      LFNAM = LENOCC(SHFNAM)
      LNEWL = INDEX(SHFNAM,CHAR(10))
      IF(LNEWL.NE.0) THEN
         SHFNAM(LFNAM:) = ' '
         LFNAM = LNEWL - 1
      ENDIF
 
      IF(SHFNAM(1:1).NE.'/') THEN
         CALL CZPUTA('2Error from sfget = '//SHFNAM(1:LFNAM),ISTAT)
         IRC = -1
      ENDIF
*
*     Delete temporary file
*
*     IRC = SYSTEMF('rm '//SHUNAM(1:LUNAM))
 
   99 END
#endif
