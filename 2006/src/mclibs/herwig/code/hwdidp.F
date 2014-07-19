CDECK  ID>, HWDIDP.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWDIDP(ID0,FBR)
C-----------------------------------------------------------------------
C     CHOOSES A PARTON FROM QUARK AND LEPTON DOUBLETS (FOR HWDHVY)
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWRGEN,RN,FR,FBR(6)
      INTEGER HWDIDP,ID0,ID,N
      EXTERNAL HWRGEN
      RN=HWRGEN(0)
      FR=0.
      ID=ID0
      DO 10 N=1,6
      FR=FR+FBR(N)
C---LEPTONS HAVE ID CODES 121-132
      IF (N.EQ.4) ID=ID0+120
      IF (RN.LT.FR) GOTO 30
   10 ID=ID+2
      ID=ID-2
   30 HWDIDP=ID
      END
