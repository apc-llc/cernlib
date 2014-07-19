*
* $Id: fredipy.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: fredipy.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRVECRC ***************************
 
 
C********************************* FREDIPY *******************************
 
      SUBROUTINE FREDIPY
 
C....TO PICK OUT THE SCATTERED PARTONS OUT OF PYTHIA'S EVENT RECORD.
C....NOTE ONE MUST HAVE SET MSTP(61,63,65,71,81,111) ALL TO 0 .
C....THE PARTONS PICKED ARE STORED IN BLOCK FRPICKJ.
 
C....KFR19 controls the assignment of the partons to the original nucleons:
C..  (currently in effect: KFR19=1)
C..   =1 The hardest pairs are assigned according to PYTHIA, the rest randomly.
C..   =2 Both pairs of partons are assigned randomly to one of the nucleon;
C..   =0 assignment to be made by inspecting the Feynman diagrams in FRHQSGN.
 
C...  Side-1: KP(J,3) = 1;  Side-2: KP(J,3) = 2  - for the hardest pair;
C...  Side-1: KP(J,3) = -1;  Side-2: KP(J,3) = -2 - for the softer pairs.
 
      PARAMETER (KSZJ=4000, KSZ1=20)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/FRPICKJ/NH,KP(100,5),PP(100,5)
      SAVE /FRPARA1/,/LUJETS/,/PYPARS/,/PYSUBS/,/FRPICKJ/
 
      kfr19 = 1
 
      IFEL = 0
      DO 10 L = 1, MSTI(3)
      IF(L.EQ.1) THEN
      LINE = MSTI(7)
      IF(LINE.EQ.0) LINE = MSTI(8)
      ELSEIF(L.EQ.2) THEN
      LINE = MSTI(8)
      ELSE
      IFEL = 1
      ENDIF
 
      IF(LINE.EQ.0) IFEL = 1
      IF(IFEL.EQ.1) CALL FRMGOUT(0,1,'Error in FREDITPY:',
     >      FLOAT(MSTI(3)),FLOAT(MSTI(7)),FLOAT(MSTI(8)),0.,0.)
      DO 30 J=1,5
      PP(L,J) = P(LINE,J)
30    KP(L,J) = K(LINE,J)
 
      IF(L.EQ.1.AND.KFR19.EQ.2) THEN
      KP(1,3) = INT(1.5+RLU(0))
      ELSEIF(KFR19.EQ.2) THEN
      KP(L,3) = KP(1,3)
      ENDIF
 
10    CONTINUE
 
 
            IF(KFR19.EQ.1) THEN
      THAT = (PARI(33)*PARI(11)/2.-PP(1,4))**2-
     >   (PARI(33)*PARI(11)/2.-PP(1,3))**2-PP(1,2)**2-PP(1,1)**2
      T12 = ABS( THAT - PARI(15))
      U12 = ABS( THAT - PARI(16))
 
        if(T12.LE.U12) THEN
      KP(1,3) = 1
      KP(2,3) = 2
      ELSE
      KP(1,3) = 2
      KP(2,3) = 1
        ENDIF
            ENDIF
 
 
      NH = MSTI(3)
 
      IF(MSTP(81).EQ.0.or.KFR(7).NE.2) RETURN
 
      DO 39 L = MSTI(8)+1, N
      IF(K(L,3).EQ.0) THEN
 
      NH = NH + 1
      IF(NH.GT.100)CALL FRMGOUT(0,0,'Extend blocks FRPICKJ and FRJETS',
     >                         float(NH),0.,0.,0.,0.)
      ISIDE1 = - INT(1.5+ RLU(0))
      ISIDE2 = -3 - ISIDE1
       DO 35 LI = L+1, N
       XB = ABS(P(L,1)+P(LI,1))
       YB = ABS(P(L,2)+P(LI,2))
       IF(XB.GT.0.0001.OR.YB.GT.0.0001) GOTO 35
       NH = NH + 1
       DO 40 J=1,5
       PP(NH-1,J) = P(L,J)
       KP(NH-1,J) = K(L,J)
       PP(NH,J) = P(LI,J)
40     KP(NH,J) = K(LI,J)
       KP(NH-1,3) = ISIDE1
       IF(KFR19.EQ.2) THEN
       KP(NH,3) = ISIDE1
       K(L,3) = ISIDE1
       K(LI,3) = ISIDE1
       ELSE
       KP(NH,3) = ISIDE2
       K(L,3) = ISIDE1
       K(LI,3) = ISIDE2
       ENDIF
       GOTO 39
35     CONTINUE
      ENDIF
 
39    CONTINUE
 
      RETURN
      END
