*
* $Id: frinskk.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frinskk.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C****************************** END FRPPART *****************************
 
C************************************ FRINSKK ***************************
 
      SUBROUTINE FRINSKK(XF,NKK)
 
C...To insert the soft gluon kink stored at NA1=1 to LUJETS_N.
C...OUTPUT: NkK=location of the kink.
C.......   NKK=0 - gluon kink is not inserted;
 
      PARAMETER (KSZJ=4000,KSZ1=20,KSZ2=300)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
      COMMON/FRATLE1/NA1,KA1(KSZ2,5),PA1(KSZ2,5)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      SAVE /FRPARA1/,/FRINTN1/,/FRATLE1/,/LUJETS/
 
      IF(XF.LE.1E-5.OR.XF.GE.0.99999.or.KA1(1,2).NE.21) THEN
      NKK = 0
      RETURN
      ENDIF
 
      NKK = N
        DO 200 LO = 1, 5
        P(N+1,LO) = P(N,LO)
200     K(N+1,LO) = K(N,LO)
 
        DO 205 LO = 1, 5
        K(NKK,LO) = KA1(1,LO)
205     P(NKK,LO) = PA1(1,LO)
        P(NKK,4) = SQRT(P(NKK,1)**2+P(NKK,2)**2+P(NKK,3)**2)
        P(NKK,5) = 0.0
 
        N = N+1
 
      RETURN
      END
