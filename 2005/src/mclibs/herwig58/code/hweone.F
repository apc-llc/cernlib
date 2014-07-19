*
* $Id: hweone.F,v 1.1.1.1 1996/03/08 17:02:12 mclareni Exp $
*
* $Log: hweone.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:12  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWEONE.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWEONE
C     SETS UP 2->1 (COLOUR SINGLET) HARD SUBPROCESS
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER ICMF,I,IBM,IHEP
      DOUBLE PRECISION PA
C---INCOMING LINES
      ICMF=NHEP+3
      DO 15 I=1,2
      IBM=I
C---FIND BEAM AND TARGET
      IF (JDAHEP(1,I).NE.0) IBM=JDAHEP(1,I)
      IHEP=NHEP+I
      IDHW(IHEP)=IDN(I)
      IDHEP(IHEP)=IDPDG(IDN(I))
      ISTHEP(IHEP)=110+I
      JMOHEP(1,IHEP)=ICMF
      JMOHEP(I,ICMF)=IHEP
      JDAHEP(1,IHEP)=ICMF
C---SPECIAL - IF INCOMING PARTON IS INCOMING BEAM THEN COPY IT
      IF (XX(I).EQ.ONE.AND.IDHW(IBM).EQ.IDN(I)) THEN
        CALL HWVEQU(5,PHEP(1,IBM),PHEP(1,IHEP))
        IF (I.EQ.2) PHEP(3,IHEP)=-PHEP(3,IHEP)
      ELSE
        PHEP(1,IHEP)=0.
        PHEP(2,IHEP)=0.
        PHEP(5,IHEP)=RMASS(IDN(I))
        PA=XX(I)*(PHEP(4,IBM)+ABS(PHEP(3,IBM)))
        PHEP(4,IHEP)=0.5*(PA+PHEP(5,IHEP)**2/PA)
        PHEP(3,IHEP)=PA-PHEP(4,IHEP)
      ENDIF
 15   CONTINUE
      PHEP(3,NHEP+2)=-PHEP(3,NHEP+2)
C---HARD CENTRE OF MASS
      IDHW(ICMF)=IDCMF
      IDHEP(ICMF)=IDPDG(IDCMF)
      ISTHEP(ICMF)=110
      CALL HWVSUM(4,PHEP(1,NHEP+1),PHEP(1,NHEP+2),PHEP(1,ICMF))
      CALL HWUMAS(PHEP(1,ICMF))
C---SET UP COLOUR STRUCTURE LABELS
      JMOHEP(2,NHEP+1)=NHEP+2
      JDAHEP(2,NHEP+1)=NHEP+2
      JMOHEP(2,NHEP+2)=NHEP+1
      JDAHEP(2,NHEP+2)=NHEP+1
      JDAHEP(1,NHEP+3)=NHEP+3
      JDAHEP(2,NHEP+3)=NHEP+3
      NHEP=NHEP+3
  999 END
