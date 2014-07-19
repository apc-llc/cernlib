*
* $Id: wrbeg.F,v 1.1.1.1 1996/01/11 14:15:03 mclareni Exp $
*
* $Log: wrbeg.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:03  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE WRBEG
C     ****************
C-- ADAPTED FROM ISAJET
C          WRITE INITIAL RECORD (TYPE 200)
C          INVERSE OF RDBEG
C
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data1.inc"
#include "cojets/data3.inc"
#include "cojets/decpar.inc"
#include "cojets/dylim.inc"
#include "cojets/final.inc"
#include "cojets/fstate.inc"
#include "cojets/idrun.inc"
#include "cojets/itapes.inc"
#include "cojets/jetlim.inc"
#include "cojets/keys.inc"
#include "cojets/mb.inc"
#include "cojets/par.inc"
#include "cojets/primar.inc"
#include "cojets/qcdpar.inc"
#include "cojets/zevel.inc"
#include "cojets/zwpar.inc"
      CHARACTER*8 BLANK
      DATA BLANK/' '/
C
C-- SET ZEVEL TO 0
      DO 200 I=1,MAXLEN
200   ZEVEL(I)=0.
C-- START WRITING
      ITA=ABS(ITEVT)
      NRECS=0
C-- 1ST TWO ENTRIES
      IZEVEL(1)=200
      IZEVEL(2)=1
      IL=3
C-- /IDRUN/
      CALL MOVLEV(IDVER,IZEVEL(IL),4)
      IL=IL+4
C-- /PRIMAR/
      CALL MOVLEV(NJET,IZEVEL(IL),10)
      IL=IL+10
C-- /FSTATE/
      CALL MOVLEV(KWMODE,IZEVEL(IL),15)
      IL=IL+15
      IF(NFORCE.GT.0) THEN
        DO 11 I=1,NFORCE
        DO 11 J=1,6
        IZEVEL(IL)=KFORCE(J,I)
11      IL=IL+1
      ENDIF
      IF(IL.GT.600) THEN
        CALL BUFOUT(IL)
        IZEVEL(1)=IZEVEL(1)+1
      ENDIF
      IZEVEL(IL)=NSTOPD
      IL=IL+1
      IF(NSTOPD.GT.0) THEN
        DO 12 I=1,NSTOPD
        IZEVEL(IL)=KSTOPD(I)
12      IL=IL+1
      ENDIF
C-- /KEYS/
      CALL MOVLEV(KEYS,IZEVEL(IL),10)
      IL=IL+10
C-- /JETLIM/
      IF(KEYS(1)) THEN
        CALL MOVLEV(PTFFFM,IZEVEL(IL),2)
        IL=IL+2
      ENDIF
C-- /DYLIM/
      IF(KEYS(3)) THEN
        CALL MOVLEV(QMIN,IZEVEL(IL),3)
        IL=IL+3
      ENDIF
C-- /QCDPAR/
      CALL MOVLEV(ALAM,IZEVEL(IL),7)
      IL=IL+7
C-- /ZWPAR/
      CALL MOVLEV(GF,IZEVEL(IL),7)
      IL=IL+7
C-- /PAR/
      CALL MOVLEV(PUD,IZEVEL(IL),20)
      IL=IL+20
C-- /DECPAR/
      CALL MOVLEV(BZDMIX,IZEVEL(IL),4)
      IL=IL+4
C-- /MB/
      CALL MOVLEV(CLPRO,IZEVEL(IL),14)
      IL=IL+14
      CALL BUFOUT(IL)
C-- /DATA1/,/DATA3/ PARTICLE TABLE STUFF
      CALL MOVLEV(CMIX,IZEVEL(IL),722)
      IL=IL+722
      IZEVEL(1)=IZEVEL(1)+1
      CALL BUFOUT(IL)
      CALL MOVLEV(IDENTF,IZEVEL(IL),353)
      IL=IL+353
      IZEVEL(1)=IZEVEL(1)+1
      CALL BUFOUT(IL)
      CALL ICHCNV(REAC,IL,8)
      CALL ICHCNV(WTYPE,IL,2)
      DO 21 L=1,176
21    CALL ICHCNV(CHA2(L),IL,5)
      IZEVEL(1)=IZEVEL(1)+1
      CALL BUFOUT(IL)
      DO 22 L=177,352
22    CALL ICHCNV(CHA2(L),IL,5)
      DO 23 L=1,2
23    CALL ICHCNV(CHA3(L),IL,5)
      DO 24 L2=1,2
      DO 24 L1=1,6
24    CALL ICHCNV(CHA4(L1,L2),IL,5)
      CALL ICHCNV(CHAGL,IL,5)
      IZEVEL(1)=IZEVEL(1)+1
      CALL BUFOUT(IL)
      RETURN
      END
