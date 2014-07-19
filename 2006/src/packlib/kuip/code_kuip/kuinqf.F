*
* $Id: kuinqf.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kuinqf.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.70/00 03/07/92  09.41.39  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE KUINQF(FNAME,LUN)
*
********************************************************************************
*
* If FNAME<>' ' inquire the existence and open/close status of file FNAME
* and returns : LUN = -1 if the file does not exist,
*               LUN =  0 if the file exists but is not open,
*               LUN >  0 if the file is open on the logical unit LUN
*
* If FNAME=' ' inquire the open/close status of unit LUN
* and returns : FNAME=' ' if the unit is not used,
*               FNAME<>' ' if the unit is connected to file FNAME
*
* IQUEST(11) return the format mode :
*            1 => 'FORMATTED'
*            2 => 'UNFORMATTED'
*            0 => other
*
* IQUEST(12) returns the access type :
*            1 => 'SEQUENTIAL'
*            2 => 'DIRECT'
*            0 => other
*
* IQUEST(13) return the record length of DIRECT access files.
*
*     Input/Output :
*
      CHARACTER*(*) FNAME
      INTEGER LUN
*
********************************************************************************
*
#include "kuip/kcques.inc"
      CHARACTER*10  ACC
      CHARACTER*11  FRM
      CHARACTER*80 CHFILE
      LOGICAL OPND,EXST
#if defined(CERNLIB_IBMMVS)
      CHARACTER*8   PREFIX
#endif
*
      IQUEST(1)=1
      IQUEST(11)=0
      IQUEST(12)=0
      IQUEST(13)=0
      FRM=' '
      ACC=' '
*
      IF (FNAME.NE.' ') THEN
*
* FNAME given, LUN returned
*
        LUN=-1
#if !defined(CERNLIB_IBMVM)
        CHFILE=FNAME
#endif
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)
        CALL CUTOL(CHFILE)
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_IBMMVS))
        CHFILE(:1)='/'
        CHFILE(2:)=FNAME
        NCH=LENOCC(CHFILE)
        IF (INDEX(CHFILE,'.').EQ.0) THEN
          CHFILE(NCH+1:NCH+4)='.DAT'
          NCH=NCH+4
        ENDIF
        DO 5 I=1,NCH
          IF(CHFILE(I:I).EQ.'.')CHFILE(I:I)=' '
5       CONTINUE
#endif
#if defined(CERNLIB_IBMMVS)
*--       DON'T ADD PREFIX IF THE FIRST CHARACTER OF FILE NAME IS A DOT
        CALL KPREFI ( PREFIX, NCHPRE )
        NCH    = LENOCC(FNAME) +1
        CHFILE(:1)='/'
        IF ( FNAME(1:1) .EQ. '.' )        THEN
             CHFILE(2:) = FNAME(2:NCH)
             NCH = NCH -1
        ELSE
             CHFILE(2:) = PREFIX(1:NCHPRE)//FNAME
             NCH = NCH + NCHPRE
        ENDIF
#endif
        INQUIRE(FILE=CHFILE,IOSTAT=IOS,ERR=999,
     +          EXIST=EXST,OPENED=OPND)
        IF (EXST) THEN
          LUN=0
          IF (OPND) THEN
            ACC=' '
            FRM=' '
            INQUIRE(FILE=CHFILE,IOSTAT=IOS,ERR=999,
     +              NUMBER=LUN,ACCESS=ACC,FORM=FRM,RECL=LRECL)
          ENDIF
        ENDIF
*
      ELSE
*
* LUN given, FNAME returned
*
        FNAME=' '
        INQUIRE(LUN,IOSTAT=IOS,ERR=999,
     +          EXIST=EXST,OPENED=OPND)
        IF (OPND) THEN
          ACC=' '
          FRM=' '
          INQUIRE(LUN,NAME=FNAME,IOSTAT=IOS,ERR=999,
     +            NUMBER=LUN,ACCESS=ACC,FORM=FRM,RECL=LRECL)
#if defined(CERNLIB_IBMVM)
          CHFILE=FNAME(2:)
          FNAME=CHFILE
#endif
        ENDIF
*
      ENDIF
*
      IQUEST(1)=0
*
      IF (FRM.EQ.'FORMATTED') THEN
        IQUEST(11)=1
      ELSE IF (FRM.EQ.'UNFORMATTED') THEN
        IQUEST(11)=2
      ELSE
        IQUEST(11)=0
      ENDIF
      IF (ACC.EQ.'SEQUENTIAL') THEN
        IQUEST(12)=1
      ELSE IF (ACC.EQ.'DIRECT') THEN
        IQUEST(12)=2
        IQUEST(13)=LRECL
      ELSE
        IQUEST(12)=0
      ENDIF
*
#if defined(CERNLIB_APOLLO)
      IF (LUN.GE.5.AND.LUN.LE.7) FNAME='terminal I/O'
#endif
*
999   END
