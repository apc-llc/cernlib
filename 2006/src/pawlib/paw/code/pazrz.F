*
* $Id: pazrz.F,v 1.3 1998/09/17 10:00:06 couet Exp $
*
* $Log: pazrz.F,v $
* Revision 1.3  1998/09/17 10:00:06  couet
* - In the RZ/MAKE command the file is now opened with the option X (exchange
* mode) to bypass the problem described below:
*
* A RZ file, which is created with
*
*          ZEBRA/RZ/MAKE 20 [fnam] 4096 ! ! H CAL-ID
*
* on a linux system, cannot be read on an AIX system after
* binary ftp. It crashes there with the error message, that
* this ile is not an rz file.
*
* ON linux:
*
* ZEBRA/RZ/MAKE 20 bla.rz 4096 ! ! H CAL-ID
* cdir //lun20
* mdir FOPI ! H Cal-id
* fortran/close 20
*
* transfer to AIX
*
* on AIX:
*
* RZ/FILE 24 bla.rz 4096
*
* Revision 1.2  1997/05/13 15:22:35  couet
* - use C/IO always
*
* Revision 1.1.1.1  1996/03/01 11:38:44  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 26/04/95  16.46.10  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAZRZ
*
*            /ZEBRA/RZ
*           Operations on a RZ directory
*
#include "hbook/hcdire.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "paw/pcslas.inc"
#include "paw/pchtag.inc"
      CHARACTER*8  CHOPT
*
      CALL KUPATL(CHPATL,NPAR)
*
      IF(CHPATL.EQ.'FILE')THEN
         CALL PAWROP('RZ')
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'MAKE')THEN
         NCHT=NCHTOP
         CALL PAWROP('RZMAKE')
         IF(NCHT.EQ.NCHTOP)GO TO 99
         LUN=ICHTOP(NCHTOP)
         CALL KUGETI(NREC)
         CALL KUGETI(NWKEY)
         CALL KUGETC(CHTEMP,NCH)
         DO 5 I=1,NWKEY
            CALL KUGETS(CHTAG(I),NCH)
   5     CONTINUE
         CHOPT = 'CX'
         CALL RZMAKE(LUN,CHTOP(NCHTOP),NWKEY,CHTEMP,CHTAG,NREC,CHOPT)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'MDIR')THEN
         CALL HCDIR(CHTITL,'R')
         IF(CHTITL(3:6).EQ.'PAWC')THEN
            IPAW=1
         ELSE
            IPAW=0
         ENDIF
         CALL KUGETC(CHTITL,NCH)
         DO 10 I=1,NCH
            IF(CHTITL(I:I).EQ.'/'.OR.CHTITL(I:I).EQ.BSLASH)THEN
               CALL HBUG('Directory name cannot contain slash','PAW',0)
               GO TO 99
            ENDIF
  10     CONTINUE
         CALL KUGETI(NWKEY)
         CALL KUGETC(CHTEMP,NCH)
         CALL KUGETS(CHTAG(1),NCH)
         IF(IPAW.NE.0.OR.CHTAG(1).EQ.'HBOOK-ID')THEN
            CALL HMDIR(CHTITL,' ')
            GO TO 99
         ENDIF
         DO 15 I=2,NWKEY
            CALL KUGETS(CHTAG(I),NCH)
  15     CONTINUE
         CALL RZMDIR(CHTITL,NWKEY,CHTEMP,CHTAG)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'DDIR')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL HDDIR(CHTITL)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'LDIR')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL KUGETC(CHPATL,NCH)
         CHOPT='R'
         IF(NCH.GT.0)CHOPT=CHPATL(1:NCH)//'R'
         CALL KUALFA
         CALL HLDIR(CHTITL,CHOPT)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'CDIR')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL KUGETC(CHOPT,NCH)
         IF(CHTITL.EQ.' '.AND.CHOPT.EQ.' ')THEN
            CHOPT='P'
         ENDIF
         CALL KUALFA
         CALL PCHNCD(CHTITL,IER)
         IF (IER .EQ. 2) GOTO 99
         CALL HCDIR(CHTITL,CHOPT)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'PURGE')THEN
         CALL KUGETI(KEEP)
         CALL RZPURG(KEEP)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'LOCK')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL RZLOCK(CHTITL)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'FREE')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL RZFREE(CHTITL)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'STAT')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL KUALFA
         CALL RZSTAT(CHTITL,99,' ')
         GO TO 99
      ENDIF
*
  99  END
