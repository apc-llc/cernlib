*
* $Id: pcharg.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: pcharg.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION PCHARG(IDP)
C.----------------------------------------------------------------------
C.
C.    FUNCTION DETERMINES THE CHARGE OF PARTON/PARTICLE WITH CODE = IDP
C.    LAST UPDATE: 15/04/89
C.
C.----------------------------------------------------------------------
      DIMENSION IQCH(9),ILCH(9),IWCH(9),ISCH(9)
      DATA IQCH/  2, -1, -1,  2, -1,  2, -1,  2,  0/
      DATA ILCH/ -1,  0, -1,  0, -1,  0, -1,  0,  0/
      DATA IWCH/  0,  1,  0,  0,  0,  0,  0,  1,  0/
      DATA ISCH/  2,  2,  2,  2,  0,  0,  0,  0,  0/
C--
C--   INITIALIZE AND DECODE
      PCHARG=0
      ID=IABS(IDP)
      IF (ID.GT.1000) THEN
        IF (ID.GT.1004) THEN
C--
C--   BARYONS
          IQ1=ID/1000
          IQ2=(ID-IQ1*1000)/100
          IQ3=(ID-IQ1*1000-IQ2*100)/10
          PCHARG=FLOAT(IQCH(IQ1)+IQCH(IQ2)+IQCH(IQ3))/3.
        ELSE
C--
C--   LEPTOQUARKS
          PCHARG=FLOAT(ISCH(ID-1000))/3.
        ENDIF
      ELSEIF (ID.GT.100) THEN
        IF ((ID.LT.190).OR.(ID.GT.200)) THEN
C--
C--   MESONS
          IQ1=ID/100
          IQ2=(ID-IQ1*100)/10
          PCHARG=FLOAT(IQCH(IQ1)-IQCH(IQ2))/3.
        ELSE
C--
C--   WEAK BOSONS, HIGGS
          PCHARG=FLOAT(IWCH(ID-190))
        ENDIF
      ELSEIF (ID.GT.90) THEN
C--
C--   LEPTONS, PHOTON
        PCHARG=FLOAT(ILCH(ID-90))
      ELSE
C--
C--   DIQUARKS, QUARKS AND GLUON
        IF (ID.GT.10) THEN
          IQ1=ID/10
          IQ2=ID-IQ1*10
          PCHARG=FLOAT(IQCH(IQ1)+IQCH(IQ2))/3.
        ELSE
          PCHARG=FLOAT(IQCH(ID))/3.
        ENDIF
      ENDIF
      IF ((PCHARG.NE.0.).AND.(IDP.LT.0)) PCHARG=-PCHARG
      RETURN
      END
