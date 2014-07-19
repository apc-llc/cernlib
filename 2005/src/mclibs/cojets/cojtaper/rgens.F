*
* $Id: rgens.F,v 1.1.1.1 1996/01/11 14:15:05 mclareni Exp $
*
* $Log: rgens.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:05  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE RGENS(IFLAG)
C     ***********************
C-- ADAPTED FROM ISAJET
C
C          INVERSE OF WGENS
C          READ A RECORD BY A CALL BUFIN
C          IF RECORD TYPE IS NOT EVENT TYPE RETURN
C          IF RGENS CALLED WITH IFLAG=10 RETURN WITHOUT UNPACKING
C          UNPACK ZEVEL INTO APPROPRIATE COMMON BLOCKS
C
C-- CREATED: 88/05/08
#include "cojets/idrun.inc"
#include "cojets/itapes.inc"
#include "cojets/jetpar.inc"
#include "cojets/jetset.inc"
#include "cojets/keys.inc"
#include "cojets/partcl.inc"
#include "cojets/pfirst.inc"
#include "cojets/pinits.inc"
#include "cojets/pjets.inc"
#include "cojets/primar.inc"
#include "cojets/rectp.inc"
#include "cojets/zevel.inc"
C
      IFL=IFLAG
      CALL BUFIN(IL,IFLAG)
      IF(IFLAG.NE.0) RETURN
      ISAV=IZEVEL(1)/100
      IRECTP=ISAV*100
      IREC=MOD(IZEVEL(1),100)
      IF(IRECTP.EQ.200) RETURN
      IF(IRECTP.EQ.300) RETURN
      IF(IFL.EQ.10) RETURN
      IL=3
C-- /IDRUN/
      CALL MOVLEV(IZEVEL(IL),IDVER,4)
      IL=IL+4
C-- /JETPAR/, ISIGS ONLY
      ISIGS=IZEVEL(IL)
      IL=IL+1
C-- /PRIMAR/, NJET ONLY
      NJET=IZEVEL(IL)
      IL=IL+1
C-- /PARTCL/
      NPTCL=IZEVEL(IL)
      IL=IL+1
      IF(ISIGS.GT.0) THEN
C-- /PFIRST/
        CALL MOVLEV(ZEVEL(IL),PFIRST,12)
        IL=IL+12
C-- /PINITS/
        CALL MOVLEV(ZEVEL(IL),PINITS,12)
        IL=IL+12
      ENDIF
      IF(NJET.GT.0) THEN
C-- /JETPAR/
        CALL MOVLEV(ZEVEL(IL),P,46)
        IL=IL+46
        CALL MOVLEV(IZEVEL(IL),INITYP,4)
        IL=IL+4
        CALL MOVLEV(ZEVEL(IL),PBEAMS,5)
        IL=IL+5
C-- /PJETS/
        IEX=NJET*5
        CALL MOVLEV(IZEVEL(IL),PJETS,IEX)
        IL=IL+IEX
        CALL MOVLEV(IZEVEL(IL),IDJETS,NJET)
        IL=IL+NJET
      ENDIF
      IF(KEYS(3)) THEN
C-- /PJETS/
        CALL MOVLEV(IZEVEL(IL),QWJET,6)
        IL=IL+6
C-- /JETPAR/
        CALL MOVLEV(IZEVEL(IL),QMW,16)
        IL=IL+16
      ELSEIF(KEYS(6)) THEN
C-- /PJETS/, W PAIRS
        NPAIR=IZEVEL(IL)
        IL=IL+1
        CALL MOVLEV(ZEVEL(IL),PPAIR(1,1),5*NPAIR)
        IL=IL+5*NPAIR
        CALL MOVLEV(IZEVEL(IL),IDPAIR(1),NPAIR)
        IL=IL+NPAIR
        CALL MOVLEV(IZEVEL(IL),JPAIR(1),NPAIR)
        IL=IL+NPAIR
      ENDIF
C-- /JETSET/
      NJSET=IZEVEL(IL)
      IL=IL+1
      IF(NJSET.LT.1) GOTO 12
      DO 50 I=1,NJSET
      CALL MOVLEV(ZEVEL(IL),PJSET(1,I),5)
      IL=IL+5
      JORIG(I)=IZEVEL(IL)
      JTYPE(I)=IZEVEL(IL+1)
      JDCAY(I)=IZEVEL(IL+2)
      IL=IL+3
      IF(IL.LE.MAXLEN-9) GO TO 50
      IF(I.EQ.NJSET.AND.NPTCL.EQ.0) GO TO 12
      CALL BUFIN(IL,IFLAG)
      IF(IFLAG.NE.0) RETURN
50    CONTINUE
C
C-- /PARTCL/
C          NPTCL.LT.0 IMPLIES ONLY STABLE PARTICLES ON THIS FILE
C          ORIGIN AND DECAY INFORMATION SUPPRESSED
12    IF(NPTCL.EQ.0) GOTO 999
      IF(NPTCL.GT.0) GOTO 997
C          ONLY STABLE PARTICLES
      NPTCL=-NPTCL
      DO 992 K=1,NPTCL
      CALL MOVLEV(ZEVEL(IL),PPTCL(1,K),5)
      IORIG(K)=IZEVEL(IL+5)
      IDENT(K)=IZEVEL(IL+6)
      IDCAY(K)=0
      IL=IL+7
      IF(IL.LE.MAXLEN-7) GOTO 992
      IF(K.EQ.NPTCL) RETURN
      CALL BUFIN(IL,IFLAG)
      IF(IFLAG.NE.0) RETURN
  992 CONTINUE
      RETURN
C          ALL PARTICLES
C          NOTE THAT IDCAY CAN EXCEED 2**24 LIMIT OF PAIRPAK
  997 CONTINUE
      DO 998 K=1,NPTCL
      CALL MOVLEV(ZEVEL(IL),PPTCL(1,K),5)
      IORIG(K)=IZEVEL(IL+5)
      IDENT(K)=IZEVEL(IL+6)
      IDCAY(K)=IZEVEL(IL+7)*IPACK+IZEVEL(IL+8)
      IL=IL+9
      IF(IL.LE.MAXLEN-9) GOTO 998
      IF(K.EQ.NPTCL) RETURN
      CALL BUFIN(IL,IFLAG)
      IF(IFLAG.NE.0) RETURN
  998 CONTINUE
  999 CONTINUE
      RETURN
      END
