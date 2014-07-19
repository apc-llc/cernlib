*
* $Id: hwhigs.F,v 1.1.1.1 1996/03/08 17:02:15 mclareni Exp $
*
* $Log: hwhigs.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:15  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHIGS.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWHIGS
C     HIGGS PRODUCTION VIA GLUON OR QUARK FUSION
C     MEAN EVWGT = HIGGS PRODN C-S * BRANCHING FRACTION IN NB
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER IDEC,I,J,ID1,ID2
      DOUBLE PRECISION BRHIGQ,EMH,CSFAC(13),EVSUM(13),EMFAC,CV,CA,BR,
     &  RWGT,E1,E2,EMQ,HWUALF,HWHIGT,HWRGEN,HWUSQR,GFACTR,HWUAEM
      SAVE CSFAC,BR,EVSUM
      IF (GENEV) THEN
        RWGT=HWRGEN(0)*EVSUM(13)
        IDN(1)=1
        DO 10 I=1,12
 10       IF (RWGT.GT.EVSUM(I)) IDN(1)=I+1
        IDN(2)=13
        IF (IDN(1).LE.12) IDN(2)=IDN(1)-6
        IF (IDN(1).LE. 6) IDN(2)=IDN(1)+6
        IDCMF=201
        CALL HWEONE
      ELSE
        EVWGT=0.
        CALL HWHIGM(EMH,EMFAC)
        IF (EMH.LE.0 .OR. EMH.GE.PHEP(5,3)) RETURN
        EMSCA=EMH
        IF (EMSCA.NE.EMLST) THEN
          EMLST=EMH
          XXMIN=(EMH/PHEP(5,3))**2
          XLMIN=LOG(XXMIN)
          GFACTR=GEV2NB*HWUAEM(EMH**2)/(288.*SWEIN*RMASS(198)**2)
          DO 20 I=1,13
            EMQ=RMASS(I)
            IF (I.EQ.13) THEN
              CSFAC(I)=-GFACTR*HWHIGT(RMASS(NFLAV)/EMH)*XLMIN
     &                         *HWUALF(1,EMH)**2 *EMFAC*ENHANC(NFLAV)**2
            ELSEIF (I.GT.6) THEN
              CSFAC(I)=CSFAC(I-6)
            ELSEIF (EMH.GT.2*EMQ) THEN
              CSFAC(I)=-GFACTR*96.*PIFAC**2 *(1-(2*EMQ/EMH)**2)
     &                *(EMQ/EMH)**2 *XLMIN *EMFAC*ENHANC(I)**2
            ELSE
              CSFAC(I)=0
            ENDIF
 20       CONTINUE
C  INCLUDE BRANCHING RATIO OF HIGGS
          IDEC=MOD(IPROC,100)
          BR=1
          IF (IDEC.EQ.0) THEN
            BRHIGQ=0
            DO 30 I=1,6
 30           BRHIGQ=BRHIGQ+BRHIG(I)
            BR=BRHIGQ
          ELSEIF (IDEC.EQ.10) THEN
            CALL HWDBOZ(198,ID1,ID2,CV,CA,BR,1)
            CALL HWDBOZ(199,ID1,ID2,CV,CA,BR,1)
            BR=BR*BRHIG(IDEC)
          ELSEIF (IDEC.EQ.11) THEN
            CALL HWDBOZ(200,ID1,ID2,CV,CA,BR,1)
            CALL HWDBOZ(200,ID1,ID2,CV,CA,BR,1)
            BR=BR*BRHIG(IDEC)
          ELSEIF (IDEC.LE.12) THEN
            BR=BRHIG(IDEC)
          ENDIF
        ENDIF
        CALL HWSGEN(.TRUE.)
        EVWGT=0
        E1=PHEP(4,MAX(1,JDAHEP(1,1)))
        E2=PHEP(4,MAX(2,JDAHEP(1,2)))
        DO 40 I=1,13
          EMQ=RMASS(I)
          IF (EMH.GT.2*EMQ) THEN
            J=13
            IF (I.LE.12) J=I-6
            IF (I.LE. 6) J=I+6
            IF (XX(1).LT.0.5*(1-EMQ/E1+HWUSQR(1-2*EMQ/E1)) .AND.
     &          XX(2).LT.0.5*(1-EMQ/E2+HWUSQR(1-2*EMQ/E2)))
     &          EVWGT=EVWGT+DISF(I,1)*DISF(J,2)*CSFAC(I)*BR
          ENDIF
          EVSUM(I)=EVWGT
 40     CONTINUE
      ENDIF
  999 END
