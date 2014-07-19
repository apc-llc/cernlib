*
* $Id: frvectc.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frvectc.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRSETDM **************************
 
 
C********************************* FRVECTC ******************************
 
 
      SUBROUTINE FRVECTC(I,IQ,DP)
 
C......CONVERSIONS BETWEEN PPS OR PPSY ARRAYS AND DP(2,4):
C.........TO SET PROJECTILE AND TARGET FOUR VECTORS TO A FORM:
C.........DP(L,1-4)=(PX,PY,P_,P+) ............................
C.........IQ=1, DP=PPS;
C.........IQ=2, DP=PPSY;
C.........FOR IQ < 0, THE REVERSE IS DONE.
 
      PARAMETER (KSZ2=300)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      DOUBLE PRECISION DP(2,5)
      SAVE /FRINTN1/,/FRINTN3/
 
      IF(IQ.EQ.1) THEN
      DO 20 L = 1, 2
      DO 20 J = 1, 4
20          DP(L,J) =DBLE(PPS(L,NUC(L,I),J))
      ELSEIF(IQ.EQ.2) THEN
      DO 22 L = 1, 2
      DO 22 J = 1, 4
22          DP(L,J) =DBLE(PPSY(L,NUC(L,I),J))
      ELSEIF(IQ.EQ.-1) THEN
      DO 60 L = 1, 2
      DO 60 J = 1, 4
60          PPS(L,NUC(L,I),J)=REAL(DP(L,J))
      ELSEIF(IQ.EQ.-2) THEN
      DO 66 L = 1, 2
      DO 66 J = 1, 4
66          PPSY(L,NUC(L,I),J)=REAL(DP(L,J))
      ENDIF
 
      RETURN
      END
