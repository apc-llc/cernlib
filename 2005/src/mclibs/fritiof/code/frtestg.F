*
* $Id: frtestg.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frtestg.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C************************************************************************
C********************************* END FRATLEO **************************
 
C*********************** SUBROUTINE FRTESTG *****************************
 
      SUBROUTINE FRTESTG(L,IQQK,IQGL,IOK,RFA)
 
C.......Test a gluon against the bremsstrahlung background
C....... IQGL = the number of gluons
C....... IOK =1 - gluon sticks out ;  IOK=0 - drawned.
 
      PARAMETER (KSZJ=4000,KSZ1=20,KSZ2=300)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/ARDAT1/PARA(40),MSTA(40)
      SAVE /FRINTN1/,/FRPARA1/,/FRINTN0/,/LUJETS/,/ARDAT1/
      DIMENSION RFA(2,2)
 
      IOK = 0
 
      MSTA6 = MSTA(6)
      MSTA(6) = 1
      PARA11 = PARA(11)
      PARA3 = PARA(3)
 
      CALL FRSAVEN(IOP(17),0)
 
      NOG = 0
      PTGL2 = 0.
      IF(IQQK.GT.0) PTGL2=MAX(PTGL2,FRIPT(IOP(17),IOP(17)+1,
     >                        IOP(17)+2,1))
       IF(IQGL.GT.0) THEN
       CALL FRMXGPT(IOP(17),N,IMX,VRPTNX,0)
       CALL FRINSET(IMX,IOP(17),N,NOG,1)
        CALL FRINSKK(RFA(L,2),NKK)
        IF(NKK.GT.0) THEN
        PARA(13) = PARA(11)/(1.-RFA(L,2))
        PARA(11) = PARA(11)/RFA(L,2)
        ENDIF
       IF(NOG.GT.0) PTGL2 = MAX(PTGL2,FRIPT(NOG,NOG-1,NOG+1,1) )
       ENDIF
 
      IF(PTGL2.GE.PARA3**2) THEN
C        PARA(3) = SQRT(PTGL2)
C        NM = N
C        CALL FRARIAD
C        IF(N-NM.EQ.0) IOK= 1
C
      PTAR = 0.
      NM = N
      CALL FRARIAD
       IF(N -NM.GE.1) THEN
        DO 11 II=IOP(17)+1, N-1
          IF(K(II,2).EQ.21.AND.K(II,5).EQ.1) THEN
          PTAR = FRIPT(II,II-1,II+1,1)
          GOTO 100
          ENDIF
 11     CONTINUE
       ENDIF
100   IF(PTAR.LE.PTGL2) IOK= 1
 
      ENDIF
 
C.......Restore the configuration:
 
      CALL FRSAVEN(IOP(17),1)
      MSTA(6) = MSTA6
      PARA(3) = PARA3
      PARA(11) = PARA11
 
      RETURN
      END
