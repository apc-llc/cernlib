*
* $Id: frtocms.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frtocms.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRBETAV ************************
 
C********************************* FRTOCMS ****************************
 
        SUBROUTINE FRTOCMS(ID,IQ,DP,DBETAO)
 
C... IN DOUBLE PRECISION.
C... GIVING A PAIR OF MEMONTA DP(2,4), A CALL TO THIS ROUTINE WILL
C... TRANSFORM THE MOMENTA INTO THEIR CMS FRAME.
C... IF ID = 0, DP IS ASSUMED TO BE THE ORDINARY 4-VECTOR;
C...    ID = 1, DP IS ASSUMED TO BE THE LIGHT-CONE FORM: PX,PY,P-,P+.
C... IF IQ > 0, A BOOST IS PERFORMED TO THE CMS FRAM; DBETA is OUTPUT of
C...             the beta factor used;
C...    IQ = 0, A BOOST is done on DP with a known Dbeta
C...    IQ < 0, A BOOST is done on DP with Dbeta = -DBETA, i.e., inverse boost,
C...             here DBETA must be given as INPUT.
C... SO, FOR EXEMPLE,
C: ...  CALL FRTOCMS(ID, 1, DP, DBETA)    -- TO THE CMS FRAME;
C: ...  CALL FRTOCMS(ID, -1, DP, DBETA)   -- BACK TO THE ORGINAL FRAME.
C:
C: ...SPECIALLY FOR |IQ|=2, PJ IN FRJETS BLOCK AND PR(NR0-NR)
C: ...IN FRCNUT BLOCK ARE ALSO BOSTED.
C: ...NR-NR0<1 IS ASSUMED.
 
      IMPLICIT DOUBLE PRECISION (D)
      COMMON/FRCNUT/NR,KR(10,5),PR(10,5),NR0
      COMMON/FRJETS/NJ,KJ(100,5),PJ(100,5)
      DIMENSION DP(2,5),DBETAO(3),DBETA(3),DRV(4),DBP(100)
      SAVE /FRCNUT/,/FRJETS/
 
C..... BETA_X, BETA_Y, BETA_Z
      DBET2 = 0.
 
      IF(ID.EQ.0) THEN
      DESUM = (DP(1,4)+DP(2,4))
      DZSUM = DP(1,3)+DP(2,3)
      else
      DESUM = (DP(1,4)+DP(1,3))/2.D0 +(DP(2,4)+DP(2,3))/2.D0
      DZSUM = (DP(1,4)-DP(1,3))/2.D0 +(DP(2,4)-DP(2,3))/2.D0
      ENDIF
 
      IF(DESUM.LE.0.) STOP 'FRCMS: E LESS THAN 0'
 
      DO 5 I = 1, 3
      IF(IQ.GT.0) THEN
       IF(I.LE.2) DBETA(I) = (DP(1,I)+DP(2,I))/DESUM
       IF(I.EQ.3) DBETA(I) = DZSUM/DESUM
       DBETAO(I) = DBETA(I)
      ELSEIF(IQ.LT.0) THEN
       DBETA(I) = - DBETAO(I)
      ELSEIF(IQ.EQ.0) THEN
       DBETA(I) = DBETAO(I)
      ENDIF
      DBET2 = DBET2 + DBETA(I)**2
5     CONTINUE
 
      IF(DBET2.GE.1.D0) STOP 'FRCMS: BETA = 1'
      IF(DBET2.LT.1.D-8) RETURN
      DGAMA = 1.D0/DFRSQR(1.D0-DBET2, 'DGA678')
      DEFF = DGAMA/(1.D0+DGAMA)
 
 
      K=0
9     K=K+1
      IF(K.EQ.1) THEN
      Iup=2
      ELSEIF(K.EQ.2.OR.K.EQ.4) THEN
      Iup=NJ
      ELSEIF(K.EQ.3) THEN
         IF(NR-NR0.GE.2) CALL FRMGOUT(0,1,'Check NR,NR0:',
     >                 FLOAT(NR),FLOAT(NR0),0.,0.,0.)
      Iup=NR-NR0+1
      ENDIF
 
      DO 888 I = 1, IUP
       IF(K.EQ.1) THEN
      DRV(1) = DP(I,1)
      DRV(2) = DP(I,2)
         IF(ID.EQ.0) THEN
      DRV(3) = DP(I,3)
      DRV(4) = DP(I,4)
         ELSE
      DRV(3) = (DP(I,4)-DP(I,3))/2.D0
      DRV(4) = (DP(I,4)+DP(I,3))/2.D0
         ENDIF
      ELSEIF(K.EQ.2) THEN
       DO 11 J=1,4
11     DRV(J) = PJ(I,J)
      ELSEIF(K.EQ.3) THEN
      IR = NR0+I-1
       DO 12 J=1,4
12     DRV(J) = PR(IR,J)
      ENDIF
 
      DBP(I) = 0.
       DO 30 J = 1, 3
30     DBP(I) = DBP(I) + DBETA(J)* DRV(J)
       DO 35 J = 1, 3
35     DRV(J) = DRV(J)+ (DEFF*DBP(I)-DRV(4))*DGAMA*DBETA(J)
40    DRV(4) = DGAMA* (DRV(4)-DBP(I))
 
       IF(K.EQ.1) THEN
      DP(I,1) = DRV(1)
      DP(I,2) = DRV(2)
        IF(ID.EQ.0) THEN
      DP(I,3) = DRV(3)
      DP(I,4) = DRV(4)
        ELSE
      DP(I,4) = DRV(4) + DRV(3)
      DP(I,3) = DRV(4) - DRV(3)
        ENDIF
      ELSEIF(K.EQ.2) THEN
      DO 41 J=1,4
41    PJ(I,J) = DRV(J)
      ELSEIF(K.EQ.3) THEN
      DO 42 J=1,4
42    PR(NR0+I-1,J) = DRV(J)
       ENDIF
888   CONTINUE
 
      IF(ABS(IQ).EQ.2.AND.NJ.GT.0.AND.K.LE.1) GO TO 9
      IF(ABS(IQ).EQ.2.and.NR-NR0.GE.0.AND.K.LE.2) GO TO 9
 
      RETURN
      END
