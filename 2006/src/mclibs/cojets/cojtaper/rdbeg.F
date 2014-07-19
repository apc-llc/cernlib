*
* $Id: rdbeg.F,v 1.1.1.1 1996/01/11 14:15:04 mclareni Exp $
*
* $Log: rdbeg.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:04  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE RDBEG
C     ****************
C-- ADAPTED FROM ISAJET
C
C          READ FIRST RECORD (TYPE 200)
C          INVERSE OF WRBEG
C
C-- CREATED: 88/05/08
#include "cojets/data1.inc"
#include "cojets/data3.inc"
#include "cojets/decpar.inc"
#include "cojets/dylim.inc"
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
      IL=3
C-- /IDRUN/
      IDSAVE=IDVER
      CALL MOVLEV(IZEVEL(IL),IDVER,4)
      IF(IDVER.NE.IDSAVE) WRITE(ITLIS,1777) IDVER,IDSAVE
1777  FORMAT(///,
     $' WARNING:  DATA WERE GENERATED WITH VERSION',I5,/,
     $'           DATA ARE BEING READ WITH VERSION',I5,/,
     $' RESULTS CANNOT BE PREDICTED.'///)
      IL=IL+4
C-- /PRIMAR/
      CALL MOVLEV(IZEVEL(IL),NJET,10)
      IL=IL+10
C-- /FSTATE/
      CALL MOVLEV(IZEVEL(IL),KWMODE,15)
      IL=IL+15
      IF(NFORCE.GT.0) THEN
        DO 11 I=1,NFORCE
        DO 11 J=1,6
        KFORCE(J,I)=IZEVEL(IL)
11      IL=IL+1
      ENDIF
      IF(IL.GT.600) THEN
        CALL BUFIN(IL,IFLAG)
        IF(IFLAG.NE.0) RETURN
      ENDIF
      NSTOPD=IZEVEL(IL)
      IL=IL+1
      IF(NSTOPD.GT.0) THEN
        DO 12 I=1,NSTOPD
        KSTOPD(I)=IZEVEL(IL)
12      IL=IL+1
      ENDIF
C-- /KEYS/
      CALL MOVLEV(IZEVEL(IL),KEYS,10)
      IL=IL+10
C-- /JETLIM/
      IF(KEYS(1)) THEN
        CALL MOVLEV(IZEVEL(IL),PTFFFM,2)
        IL=IL+2
      ENDIF
C-- /DYLIM/
      IF(KEYS(3)) THEN
        CALL MOVLEV(IZEVEL(IL),QMIN,3)
        IL=IL+3
      ENDIF
C-- /QCDPAR/
      CALL MOVLEV(IZEVEL(IL),ALAM,7)
      IL=IL+7
C-- /ZWPAR/
      CALL MOVLEV(IZEVEL(IL),GF,7)
      IL=IL+7
C-- /PAR/
      CALL MOVLEV(IZEVEL(IL),PUD,20)
      IL=IL+20
C-- /DECPAR/
      CALL MOVLEV(IZEVEL(IL),BZDMIX,4)
      IL=IL+4
C-- /MB/
      CALL MOVLEV(IZEVEL(IL),CLPRO,14)
      IL=IL+14
C-- /DATA1/,/DATA3/ PARTICLE TABLE STUFF
      CALL BUFIN(IL,IFLAG)
      IF(IFLAG.NE.0) RETURN
      CALL MOVLEV(IZEVEL(IL),CMIX,722)
      IL=IL+722
      CALL BUFIN(IL,IFLAG)
      IF(IFLAG.NE.0) RETURN
      CALL MOVLEV(IZEVEL(IL),IDENTF,353)
      IL=IL+353
      CALL BUFIN(IL,IFLAG)
      IF(IFLAG.NE.0) RETURN
      CALL CHICNV(REAC,IL,8)
      CALL CHICNV(WTYPE,IL,2)
      DO 21 L=1,176
21    CALL CHICNV(CHA2(L),IL,5)
      CALL BUFIN(IL,IFLAG)
      IF(IFLAG.NE.0) RETURN
      DO 22 L=177,352
22    CALL CHICNV(CHA2(L),IL,5)
      DO 23 L=1,2
23    CALL CHICNV(CHA3(L),IL,5)
      DO 24 L2=1,2
      DO 24 L1=1,6
24    CALL CHICNV(CHA4(L1,L2),IL,5)
      CALL CHICNV(CHAGL,IL,5)
      RETURN
      END
