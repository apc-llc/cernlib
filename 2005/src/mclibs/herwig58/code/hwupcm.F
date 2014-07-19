*
* $Id: hwupcm.F,v 1.1.1.1 1996/03/08 17:02:18 mclareni Exp $
*
* $Log: hwupcm.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:18  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWUPCM.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWUPCM(EM0,EM1,EM2)
      DOUBLE PRECISION HWUPCM
C     C.M. MOMENTUM FOR DECAY MASSES EM0 -> EM1 + EM2
C     SET TO -1 BELOW THRESHOLD
C------------------------------------------------------------------------
      DOUBLE PRECISION EM0,EM1,EM2,EMS
      EMS=EM1+EM2
      IF (EM0.LT.EMS) THEN
        HWUPCM=-1.
      ELSEIF (EM0.EQ.EMS) THEN
        HWUPCM=0.
      ELSE
        HWUPCM=SQRT((EM0+(EM1-EM2))*(EM0-(EM1-EM2))*
     &              (EM0+EMS)*(EM0-EMS))*.5/EM0
      ENDIF
      END
