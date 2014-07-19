*
* $Id: kxwriv.F,v 1.4 2000/01/10 10:35:44 couet Exp $
*
* $Log: kxwriv.F,v $
* Revision 1.4  2000/01/10 10:35:44  couet
* - FNAME is now taken from the common block kcvrwf
*
* Revision 1.3  1997/09/25 14:02:28  cremel
* VECTOR WRITE (kxwriv.F): increase limitation of number of vectors in the
* list VLIST from 10 to 30 (same value as in kxreav.F for VECTOR/READ).
* Update HELP for VECTOR/WRITE and VECTOR/READ and mention explicitely this
* limitation. Improve error message in kxwriv.F and kxreav.F by
* adding explicitely VECTOR/WRITE and VECTOR/READ.
* + Increase KUIP version number to 2.07/16  (25/09/97).
*
* Revision 1.2  1996/04/11 09:44:19  cernlib
* Do not convert to uppercase on retrieve of format
*
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.05/15 01/08/94  10.35.04  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE KXWRIV
*
********************************************************************************
*
* Execution routine for command '/VECTOR/WRITE'
*
********************************************************************************
*
#include "kuip/kcgen.inc"
#include "kuip/kcques.inc"
#include "kuip/kcunit.inc"
#include "kuip/kcvrwf.inc"
      CHARACTER*64 VNAME
      PARAMETER    (MAXNV=30)
      DIMENSION    LLOWS(MAXNV),LHIGHS(MAXNV)
      DIMENSION    V(MAXNV),INTV(MAXNV)
      EQUIVALENCE  (V(1),INTV(1))
      CHARACTER*64 FORMAT,FORTMP
      CHARACTER*4  CHOPT
      SAVE VNAME
*
      CALL KUGETC(VNAME,NCHNAM)
*
      IF (NCHNAM.EQ.0) THEN
        CALL KUALFA
        PRINT *,'*** VECTOR/WRITE: Vector not specified'
        IQUEST(1)=1
        GO TO 999
      ENDIF
*
      NV=0
      LENMIN=1000000
5     CONTINUE
      IF (NV.EQ.MAXNV) THEN
        PRINT *,
     +   '*** VECTOR/WRITE: Sive of Vector list is limited to: ', 
     +   MAXNV
        PRINT *,'*** VECTOR/WRITE: Last vector to be processed: ', 
     +   VNAME
      ENDIF
      CALL KUGETL(VNAME,N)
      IF (N.GT.0.AND.NV.LT.MAXNV) THEN
        NV=NV+1
        CALL KUVECT(VNAME,LLOW,LHIGH)
        IF (IQUEST(1).NE.0) GO TO 999
        IF (LLOW.EQ.0) THEN
          CALL KUALFA
          PRINT *,'*** VECTOR/WRITE: Unknown vector ',VNAME
          IQUEST(1)=1
          GO TO 999
        ELSE
          L=(IQUEST(31)-IQUEST(21)+1)*
     +      (IQUEST(32)-IQUEST(22)+1)*
     +      (IQUEST(33)-IQUEST(23)+1)
          IF (L.LT.LENMIN) LENMIN=L
        ENDIF
        LLOWS(NV)=LLOW
        LHIGHS(NV)=LHIGH
        GO TO 5
      ENDIF
*
      CALL KUGETF(FNAME,NCH)
      IF (LLOW.EQ.0) GO TO 999
      LENTOT=IQUEST(11)
      ILOW  =IQUEST(12)
      IHIGH =IQUEST(13)
      ITYPE =IQUEST(14)
      CALL KUGETS(FORMAT,NCH)
      IF (FORMAT.NE.' ') THEN
        FORTMP=' '
        FORTMP(1:1)='('
        FORTMP(2:)=FORMAT
        FORTMP(NCH+2:NCH+2)=')'
        FORMAT=FORTMP
      ENDIF
      CALL KUGETC(CHOPT,NCH)
      IF (FNAME.EQ.' ') THEN
        LUNVW=6
        CALL KUALFA
      ELSE
        CALL KUINQF(FNAME,LUN)
        IF (LUN.GT.0) THEN
          IF (IQUEST(11).EQ.1.AND.IQUEST(12).EQ.1) THEN
            LUNVW=LUN
          ELSE
            CALL KUALFA
            PRINT *,'*** VECTOR/WRITE: Error: ',
     +            FNAME(1:LENOCC(FNAME)),
     +              ' is not a FORMATTED SEQUENTIAL file'
            GO TO 999
          ENDIF
        ELSE
          LUNVW=LUIVW
        ENDIF
        IF (INDEX(CHOPT,'O').GT.0) THEN
          CALL KUOPEN(LUNVW,FNAME,'VERYNEW',ISTAT)
          IF (ISTAT.NE.0) THEN
            CALL KUALFA
            PRINT *,'*** VECTOR/WRITE: Error in opening file ',
     +         FNAME(1:LENOCC(FNAME))
            GO TO 999
          ENDIF
        ENDIF
      ENDIF

      IF (NV.EQ.1) THEN
        NLIN=1
      ELSE
        NLIN=LENMIN
      ENDIF

      DO 55 ILIN=1,NLIN

      IF (NV.EQ.1) GO TO 66

      DO 90 IV=1,NV
        V(IV)=Q(LLOWS(IV)+ILIN-1)
90    CONTINUE

66    CONTINUE

      IF (FORMAT.EQ.' ') THEN
        IF (ITYPE.EQ.1) THEN
          IF (NV.EQ.1) THEN
            WRITE (LUNVW,*,ERR=950) (Q(I),I=LLOW,LHIGH)
          ELSE
            WRITE (LUNVW,*,ERR=950) (V(I),I=1,NV)
          ENDIF
        ELSE IF (ITYPE.EQ.2) THEN
          IF (NV.EQ.1) THEN
            WRITE (LUNVW,*,ERR=950) (IQ(I),I=LLOW,LHIGH)
          ELSE
            WRITE (LUNVW,*,ERR=950) (INTV(I),I=1,NV)
          ENDIF
        ENDIF
      ELSE
        IF (ITYPE.EQ.1) THEN
          IF (NV.EQ.1) THEN
            WRITE (LUNVW,FORMAT,ERR=950) (Q(I),I=LLOW,LHIGH)
          ELSE
            WRITE (LUNVW,FORMAT,ERR=950) (V(I),I=1,NV)
          ENDIF
        ELSE IF (ITYPE.EQ.2) THEN
          IF (NV.EQ.1) THEN
            WRITE (LUNVW,FORMAT,ERR=950) (IQ(I),I=LLOW,LHIGH)
          ELSE
            WRITE (LUNVW,FORMAT,ERR=950) (INTV(I),I=1,NV)
          ENDIF
        ENDIF
      ENDIF

55    CONTINUE

      GO TO 990

950   CALL KUALFA
      PRINT *,'*** VECTOR/WRITE: Error in writing to file ',
     +        FNAME(1:LENOCC(FNAME))
      GO TO 990

990   IF ((LUNVW.NE.6).AND.(INDEX(CHOPT,'C').GT.0))
     +                                 CALL KUCLOS(LUNVW,' ',ISTAT)

999   END



