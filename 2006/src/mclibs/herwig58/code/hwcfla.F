*
* $Id: hwcfla.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: hwcfla.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWCFLA.
*CMZ :-        -26/04/91  10.18.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWCFLA(JD1,JD2,ID1,ID2)
C     SETS UP FLAVOURS FOR CLUSTER DECAY
C------------------------------------------------------------------------
      INTEGER JD1,JD2,ID1,ID2,JD,JDEC(12)
      DATA JDEC/1,2,3,10,11,12,4,5,6,7,8,9/
      JD=JD1
      IF (JD.GT.12) JD=JD-108
      ID1=JDEC(JD)
      JD=JD2
      IF (JD.GT.12) JD=JD-96
      ID2=JDEC(JD-6)
      END
