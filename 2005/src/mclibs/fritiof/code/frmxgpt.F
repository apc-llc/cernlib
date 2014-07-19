*
* $Id: frmxgpt.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frmxgpt.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C*********************** END FRARIAD *****************************
 
C******************************** FRMXGPT ********************************
 
      SUBROUTINE FRMXGPT(N1,N2,IMX,VRPTNX,IDROP)
 
C..   To find the gluon with maximu inv-pt among those on FRATLE1:2-NA1.
C..   IDROP=1: The gluon will have KA1(I,3)=-2001 once it has been used.
C..        =0: Gluons will not be marked (used when called from FRTESTG.
C..   The inv pt is calculated assuming the gluon is between N1 and N2.
C..   IMX=0 - no more gluons are available;
C..   IMX>=2 - index of the gluon with largest inv-pt.
C..   VRPTNX gives value of the NEXT largest pt.
C..   When N1 or N2 <= 0, previous memorized p(N1,)and p(N2,) will be used.
 
      PARAMETER (KSZJ=4000,KSZ2=300)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/FRATLE1/NA1,KA1(KSZ2,5),PA1(KSZ2,5)
      SAVE /LUJETS/,/FRATLE1/
 
      VRPTNX=0.
      IMX=0
 
      IF(NA1.EQ.2) THEN
      IMX=2
 
      ELSEIF(NA1.GT.2) THEN
 
      IQ=1
      IF(N1.LE.0.OR.N2.LE.0) IQ=-1
      VRPT2M=-1.E4
      DO 100 I=2, NA1
      IF(KA1(I,3).EQ.-2001) GOTO 100
        DO 120 LO=1,5
120     P(N+1,LO)=PA1(I,LO)
      VRPT2 = FRIPT(N+1,N1,N2,IQ)
      IQ=-1
 
      IF(VRPT2.GT.VRPT2M.OR.IMX.EQ.0) THEN
      IMX = I
      VRPTNX=SQRT(MAX(VRPT2M,0.))
      VRPT2M=VRPT2
      ENDIF
 
100   CONTINUE
 
      ENDIF
 
      IF(IDROP.GT.0.AND.IMX.GT.0) KA1(IMX,3)=-2001
 
      RETURN
      END
