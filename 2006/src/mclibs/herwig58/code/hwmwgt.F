*
* $Id: hwmwgt.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwmwgt.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWMWGT.
*CMZ :-        -02/11/93  11.11.55  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWMWGT
C COMPUTES WEIGHT FOR MINIMUM-BIAS EVENT
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION S,X,Y
      INTEGER IDB,IDT,IDBT
      IF (IERROR.NE.0) RETURN
      IDB=IDHW(1)
      IF (JDAHEP(1,1).NE.0) IDB=IDHW(JDAHEP(1,1))
      IDT=IDHW(2)
      IF (JDAHEP(1,2).NE.0) IDT=IDHW(JDAHEP(1,2))
      IDBT=100*IDB+IDT
      IF (IDT.GT.IDB) IDBT=100*IDT+IDB
C---USE TOTAL CROSS SECTION FITS OF DONNACHIE & LANDSHOFF
C   CERN-TH.6635/92
      IF (IDBT.EQ.9173) THEN
        X=21.70
        Y=98.39
      ELSEIF (IDBT.EQ.7373) THEN
        X=21.70
        Y=56.08
      ELSEIF (IDBT.EQ.7330) THEN
        X=13.63
        Y=36.02
      ELSEIF (IDBT.EQ.7338) THEN
        X=13.63
        Y=27.56
      ELSEIF (IDBT.EQ.7334) THEN
        X=11.82
        Y=26.36
      ELSEIF (IDBT.EQ.7346) THEN
        X=11.82
        Y= 8.15
      ELSEIF (IDBT.EQ.7359) THEN
        X=.0677
        Y=.1290
      ELSEIF (IDBT.EQ.9175) THEN
        X=21.70
        Y=92.71
      ELSEIF (IDBT.EQ.7573) THEN
        X=21.70
        Y=54.77
      ELSEIF (IDBT.EQ.5959) THEN
C---FOR GAMMA-GAMMA ASSUME X AND Y FACTORIZE
        X=2.1E-4
        Y=3.0E-4
      ELSE
        PRINT *,' IDBT=',IDBT
        CALL HWWARN('HWMWGT',100,*999)
      ENDIF
      S=PHEP(5,3)**2
C---EVWGT IS NON-DIFFRACTIVE CROSS SECTION IN NANOBARNS
C   ASSUMING NON-DIFFRACTIVE = TOTAL*0.7
      EVWGT=.7E6*(X*S**.0808 + Y*S**(-.4525))
  999 END
