C***********************************************************************
C...
      PROGRAM DEMO
C...Demonstration job for LEPTO 6.5

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      DATA NPRINT/10/,NEVENT/100/
C
      EXTERNAL LUDATA,PYDATA
C...Settings
      CUT(5)=10.
      CUT(7)=25.
      
C...Init
      CALL TIMEX(T1)
      CALL LINIT(0,-11,-27.5,820.0,4)
      CALL TIMEX(T2)

      DO 500 NE=1,NEVENT
  100 CALL LEPTO
      IF(LST(21).NE.0) THEN
        WRITE(6,*) ' Warning: LST(21)=',LST(21),' event skipped'
        GOTO 100
      ENDIF
C      IF(NE.LE.NPRINT) THEN
C        CALL LULIST(1)
C        WRITE(6,*) ' x,y,Q2,W= ',X,Y,Q2,SQRT(W2)
C      ENDIF
  500 CONTINUE
  600 CONTINUE
      CALL TIMEX(T3)
      WRITE(6,1000) PARL(23),PARL(24),NEVENT,T2-T1,T3-T2,(T3-T2)/NEVENT
 1000 FORMAT(/,' Cross section from numerical integration:',G12.3,' pb',
     &       /,' Cross section from MonteCarlo simulation:',G12.3,' pb',
     &       /,' Number of events generated:              ',I12,
     &       /,' Time for initialization:         ',F12.3,' s',
     &       /,' Time for event generation,     total:    ',F12.3,' s',
     &       /,'                            per event:    ',F12.3,' s')
      END
      FUNCTION DFUN(ND,X)
      DOUBLE PRECISION DFUN,X(ND)
 
       DFUN = 1.0D0

      RETURN
      END
