*
* $Id: hwdidp.F,v 1.1.1.1 1996/03/08 17:02:11 mclareni Exp $
*
* $Log: hwdidp.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWDIDP.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      INTEGER FUNCTION HWDIDP(ID0,FBR)
C     CHOOSES A PARTON FROM QUARK AND LEPTON DOUBLETS (FOR HWDHVY)
C------------------------------------------------------------------------
      INTEGER ID0,ID,N
      DOUBLE PRECISION HWRGEN,RN,FR,FBR(6)
      RN=HWRGEN(0)
      FR=0.
      ID=ID0
      DO 10 N=1,6
      FR=FR+FBR(N)
C---LEPTONS HAVE ID CODES 121-132
      IF (N.EQ.4) ID=ID0+120
      IF (RN.LT.FR) GO TO 30
   10 ID=ID+2
      ID=ID-2
   30 HWDIDP=ID
      END
