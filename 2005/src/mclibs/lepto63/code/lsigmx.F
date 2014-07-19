C **********************************************************************

      SUBROUTINE LSIGMX(NPAR,DERIV,DIFSIG,XF,IFLAG)

C...Calculates the negative of the differential cross-section.
C...In the generation procedure the maximum of the differential cross-
C...section is needed for weighting purposes. This maximum is found by
C...minimizing the negative differential cross-section using the MINUIT
C...routines which are then calling this routine.
C...More precisly, only the part of the cross-section formula which is
C...needed for the weighting procedure is included here.
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      DIMENSION DERIV(30),XF(30)
      DATA NCALLS/0/

      DUMMY=NPAR+DERIV(1)
      IF(IFLAG.EQ.1) NCALLS=0
      IF(IFLAG.EQ.2) WRITE(6,1000)

      DIFSIG=1.E+12
      NCALLS=NCALLS+1
      X=XF(1)
      IF(X.LT.XMIN) THEN
        DIFSIG=(XMIN-X)**2*1.E+11 
        RETURN
      ELSEIF(X.GT.XMAX) THEN 
        DIFSIG=(X-XMAX)**2*1.E+11 
        RETURN
      ENDIF
      S=PARL(21)
      PM2=PSAVE(3,2,5)**2
      Q2LOW=MAX(Q2MIN,X*YMIN*S,(W2MIN-PM2)*X/(1.-X))
      Q2UPP=MIN(Q2MAX,X*YMAX*S,(W2MAX-PM2)*X/(1.-X))
      YLOW=MAX(YMIN,Q2MIN/(S*X),(W2MIN-PM2)/(S*(1.-X)))
      YUPP=MIN(YMAX,Q2MAX/(S*X),(W2MAX-PM2)/(S*(1.-X)))
      W2LOW=MAX(W2MIN,(1.-X)*YMIN*S+PM2,Q2MIN*(1.-X)/X+PM2)
      W2UPP=MIN(W2MAX,(1.-X)*YMAX*S+PM2,Q2MAX*(1.-X)/X+PM2)
      IF(LST(31).EQ.1) THEN
        Q2=XF(2)
        IF(Q2.LT.Q2LOW) THEN
          DIFSIG=(Q2LOW-Q2)**2*1.E+11 
          RETURN
        ELSEIF(Q2.GT.Q2UPP) THEN 
          DIFSIG=(Q2-Q2UPP)**2*1.E+11 
          RETURN
        ENDIF
        Y=Q2/(PARL(21)*X)
        W2=(1.-X)*Y*PARL(21)+PM2
      ELSEIF(LST(31).EQ.2) THEN
        Y=XF(2)
        IF(Y.LT.YLOW) THEN
          DIFSIG=(YLOW-Y)**2*1.E+11 
          RETURN
        ELSEIF(Y.GT.YUPP) THEN 
          DIFSIG=(Y-YUPP)**2*1.E+11 
          RETURN
        ENDIF
        Q2=Y*X*PARL(21)
        W2=(1.-X)*Y*PARL(21)+PM2
      ELSEIF(LST(31).EQ.3) THEN
        W2=XF(2)
        IF(W2.LT.W2LOW) THEN
          DIFSIG=(W2LOW-W2)**2*1.E+11 
          RETURN
        ELSEIF(W2.GT.W2UPP) THEN 
          DIFSIG=(W2-W2UPP)**2*1.E+11 
          RETURN
        ENDIF
        Y=(W2-PM2)/((1.-X)*PARL(21))
        Q2=X*Y*PARL(21)
      ENDIF
      IF(Q2.LT.Q2LOW) THEN
        DIFSIG=(Q2LOW-Q2)**2*1.E+11 
        RETURN
      ELSEIF(Q2.GT.Q2UPP) THEN 
        DIFSIG=(Q2-Q2UPP)**2*1.E+11 
        RETURN
      ENDIF
      IF(Y.LT.YLOW) THEN
        DIFSIG=(YLOW-Y)**2*1.E+11 
        RETURN
      ELSEIF(Y.GT.YUPP) THEN 
        DIFSIG=(Y-YUPP)**2*1.E+11 
        RETURN
      ENDIF
      IF(W2.LT.W2LOW) THEN
        DIFSIG=(W2LOW-W2)**2*1.E+11 
        RETURN
      ELSEIF(W2.GT.W2UPP) THEN 
        DIFSIG=(W2-W2UPP)**2*1.E+11 
        RETURN
      ENDIF
      LST2=LST(2)
      LST(2)=-1
      CALL LEPTO
      LST(2)=LST2
      DIFSIG=0. 
      IF(LST(21).NE.0) RETURN
      DIFSIG=-PQ(17)*COMFAC

      IF(LST(3).GE.4.AND.IFLAG.EQ.3)
     &WRITE(6,1100) NCALLS,DIFSIG,X,Y,Q2,W2
      RETURN

 1000 FORMAT(' Warning: IFLAG = 2 in call to LSIGMX, which does not '
     &,'calculate derivatives.')
 1100 FORMAT(/,5X,'Terminating entry in LSIGMX after ',I5,' calls.',/,
     &5X,'Best estimate of minimum found to be ',E12.4,/,
     &5X,'located at x, y, Q**2, W**2 = ',4G10.3,/)

      END
