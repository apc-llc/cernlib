*
* $Id: zsfold.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zsfold.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_OLD)
      SUBROUTINE ZSFILE(CHMAIL)
*
*         to open a RZ file remotely
*
#include "cspack/czdir.inc"
      COMMON/QUEST/IQUEST(100)
      CHARACTER*58 CFNAME
      CHARACTER*64 CHFILE
      CHARACTER*8 STAT
      CHARACTER*4 CHOPT
      CHARACTER*5 CHREC
      CHARACTER*3 CHLUN
      CHARACTER*(*) CHMAIL
#if defined(CERNLIB_OS9)
      CHARACTER*(*) LOSHFT
#endif
*
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
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_VAXVMS)
      CFNAME=CHMAIL(1:NCH)
      CALL CUTOL(CFNAME)
#endif
#if defined(CERNLIB_IBM)
      CFNAME=CHMAIL(1:NCH)
      CALL CLTOU(CFNAME)
#endif
#if defined(CERNLIB_OS9)
      cfname = loshft(chmail(1:nch))
#endif
      LUN=10+NCHRZ+1
      IOPTN=INDEX(CHOPT,'N')
      IPASS=0
      IF(IOPTN.NE.0)THEN
         STAT='UNKNOWN'
      ELSE
         STAT='OLD'
      ENDIF
      LRECL=1024
  2   CONTINUE
*
#if defined(CERNLIB_OS9)
       ISTAT=1
**      OPEN(UNIT=LUN,FILE=CFNAME,FORM='UNFORMATTED',RECL=4*LRECL,
**     +     ACCESS='DIRECT',STATUS=STAT,IOSTAT=ISTAT)
      OPEN(UNIT=LUN,FILE=CFNAME,ERR=90)
      ISTAT=0
#endif
#if defined(CERNLIB_APOLLO)
      OPEN(UNIT=LUN,FILE=CFNAME,FORM='UNFORMATTED',RECL=4*LRECL,
     +     ACCESS='DIRECT',STATUS=STAT,IOSTAT=ISTAT)
#endif
#if defined(CERNLIB_IBMVM)
      WRITE(CHLUN,'(I3)')LUN
      IF(IOPTN.EQ.0)THEN
         CHFILE='STATE '//CFNAME
         DO 5 I=7,64
            IF(CHFILE(I:I).EQ.'.')CHFILE(I:I)=' '
   5     CONTINUE
         CALL VMCMS(CHFILE,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
      ENDIF
      CHFILE='/'//CFNAME
      DO 10 I=2,64
         IF(CHFILE(I:I).EQ.'.')CHFILE(I:I)=' '
  10  CONTINUE
      CALL FILEINF(ISTAT,'MAXREC',2)
      IF(ISTAT.NE.0)GO TO 90
      IF(IOPTN.NE.0)THEN
         OPEN(UNIT=LUN,FILE=CHFILE,FORM='UNFORMATTED',RECL=4*LRECL,
     +        ACCESS='DIRECT',STATUS=STAT,IOSTAT=ISTAT)
         IF(ISTAT.NE.0)GO TO 90
         CLOSE(LUTN)
      ENDIF
      CALL FILEINF(ISTAT,'MAXREC',16777215)
      IF(ISTAT.NE.0)GO TO 90
         OPEN(UNIT=LUN,FILE=CHFILE,FORM='UNFORMATTED',RECL=4*LRECL,
     +        ACCESS='DIRECT',STATUS=STAT,IOSTAT=ISTAT)
 
#endif
#if defined(CERNLIB_VAXVMS)
      IOPTU=INDEX(CHOPT,'U')
      IF(IOPTU.NE.0.OR.IOPTN.NE.0)THEN
         OPEN(UNIT=LUN,FILE=CFNAME,FORM='UNFORMATTED',RECL=LRECL,
     +        ACCESS='DIRECT',SHARED,STATUS=STAT,IOSTAT=ISTAT)
      ELSE
         OPEN(UNIT=LUN,FILE=CFNAME,FORM='UNFORMATTED',RECL=LRECL,
     +        ACCESS='DIRECT',SHARED,READONLY,STATUS=STAT,IOSTAT=ISTAT)
      ENDIF
#endif
*
  90  IF(ISTAT.NE.0)THEN
         CALL CZPUTA('3 Cannot open remote file '//CFNAME,ISTAT)
         GO TO 99
      ENDIF
*
      WRITE(CFNAME,4000)LUN
 4000 FORMAT('LUN',I2,'   ')
*
      IF(IOPTN.EQ.0)THEN
         CALL RZFILE(LUN,CFNAME,CHOPT)
      ELSE
         CALL RZMAKE(LUN,CFNAME,1,'I','HBOOK-ID',1000,' ')
      ENDIF
      IF(IQUEST(1).NE.0)THEN
         ISTAT=1
         GO TO 90
      ENDIF
*
      NCHRZ=NCHRZ+1
      CHRZ(NCHRZ)=CFNAME
      FLNAME(NCHRZ)=CHMAIL
      LUNRZ(NCHRZ)=LUN
      IDIR=NCHRZ
*
      CALL CZPUTA('1 Connecting remote file //'//CFNAME,ISTAT)
*
  99  END
 
#endif
