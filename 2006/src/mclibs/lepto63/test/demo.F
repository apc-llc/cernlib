C***********************************************************************
C...
      PROGRAM DEMO
      IMPLICIT NONE
      INTEGER NSET
      PARAMETER(NSET=20)
C...Demonstration job for LEPTO
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      REAL P,V
      INTEGER N,K
      INTEGER NEVENT
      DATA NEVENT/100/
      
      INTEGER NE,SET
      REAL T1,T2,T3
      
      INTEGER LST7(NSET),LST8(NSET),LST11(NSET),LST15(NSET),LST19(NSET),
     &        LST20(NSET),
     &        LST23(NSET),LST34(NSET),LST35(NSET)
      INTEGER LTYPE(NSET)
      
      DATA  LST7/1,0,18*1/
      DATA  LST8/2*12,1,17*12/
      DATA LST11/3*0,111,16*0/
      DATA LST15/17*9,-4,-4,9/
      DATA LST19/4*-1,10,-10,14*-1/
      DATA LST20/6*5,1,6,12*5/
      DATA LST23/8*4,1,2,3,9*4/
      DATA LST34/11*1,0,0,7*1/
      DATA LST35/12*1,0,0,3*1,0,0,1/
      DATA LTYPE/9*-11,13,10*-11/
      
      REAL PARL7(NSET),PARL8(NSET),PARL9(NSET)
      REAL PE(NSET),LE(NSET)

      DATA PARL7/20*0.5/
      DATA PARL8/6*0.04,2*0.005,6*0.04,0.001,5*0.04/
      DATA PARL9/15*4.0,10.0,100.0,3*4.0/
      DATA PE/20*820.0/
      DATA LE/20*-27.5/
C
      EXTERNAL LUDATA,PYDATA,LEPTOD
#if defined(CERNLIB_GFORTRAN)
      call dummysub(ludata)
      call dummysub(pydata)
      call dummysub(leptod)
#endif
C...Set cuts
      LST(3)=0
      CUT(7)=100.

      DO 600 SET=1,NSET
      LST(7)=LST7(SET)
      LST(8)=LST8(SET)
      LST(11)=LST11(SET)
      LST(15)=LST15(SET)
      LST(19)=LST19(SET)
      LST(20)=LST20(SET)
      LST(23)=LST23(SET)
      LST(34)=LST34(SET)
      LST(35)=LST35(SET)
      PARL(7)=PARL7(SET)
      PARL(8)=PARL8(SET)
      PARL(9)=PARL9(SET)
      
      WRITE(6,*) 'LST(7)=',LST(7),'hadronisation'
      WRITE(6,*) 'LST(8)=',LST(8),'QCD'
      WRITE(6,*) 'LST(11)=',LST(11),'LQCD+10*LTM+100*LHT'
      WRITE(6,*) 'LST(19)=',LST(19),'Grid'
      WRITE(6,*) 'LST(20)=',LST(20),'Cut-off scheme'
      WRITE(6,*) 'LST(34)=',LST(34),'SCI'
      WRITE(6,*) 'LST(35)=',LST(35),'SQT'
      WRITE(6,*) 'PARL(7)=',PARL(7),'SCI prob'
      WRITE(6,*) 'PARL(8)=',PARL(8),'ycut or zmin'
      WRITE(6,*) 'PARL(9)=',PARL(9),'smin'
      WRITE(6,*) 'LEPTON TYPE=',LTYPE(SET)
      WRITE(6,*) 'LEPTON ENERGY=',LE(SET)
      WRITE(6,*) 'PROTON ENERGY=',PE(SET)

      CALL TIMEX(T1)
      CALL LINIT(0,LTYPE(SET),LE(SET),PE(SET),LST23(SET))
      CALL TIMEX(T2)

      DO 500 NE=1,NEVENT
  100 CALL LEPTO
      IF(LST(21).NE.0) THEN
        WRITE(6,*) ' Warning: LST(21)=',LST(21),' event skipped'
        GOTO 100
      ENDIF
      IF(MOD(NE,1000).EQ.0) THEN
C        CALL LULIST(1)
C        WRITE(6,*) 'N,x,y,Q2,W= ',NE,X,Y,Q2,SQRT(W2)
      ENDIF
  500 CONTINUE
      CALL TIMEX(T3)
      WRITE(6,1000) PARL(23),PARL(24),NEVENT,
     &              T2-T1,T3-T2,(T3-T2)/MAX(1,NEVENT)
  600 CONTINUE
 1000 FORMAT(/,' Cross section from numerical integration:',G12.3,' pb',
     &       /,' Cross section from MonteCarlo simulation:',G12.3,' pb',
     &       /,' Number of events generated:              ',I12,
     &       /,' Time for initialization (LINIT):         ',F12.3,' s',
     &       /,' Time for event generation,     total:    ',F12.3,' s',
     &       /,'                            per event:    ',F12.3,' s')
      END
      FUNCTION DFUN(ND,X)
      DOUBLE PRECISION DFUN,X(ND)
 
       DFUN = 1.0D0

      RETURN
      END
#if defined(CERNLIB_GFORTRAN)
      subroutine dummysub(a)
      return
      end
#endif
