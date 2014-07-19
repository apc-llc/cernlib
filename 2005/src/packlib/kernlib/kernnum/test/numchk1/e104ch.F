*
* $Id: e104ch.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: e104ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE E104CH(NA,ENT,NT,TABLE,OK)
          DIMENSION FX(2),FXY(2,2),FXYZ(2,2,2)
          DIMENSION ARG(3),NENT(3),ENT(NA),TABLE(NT)
          DIMENSION EX(5),EY(5),EZ(5),X(11),Y(11),Z(11),LXYZ(5,11)
          DIMENSION LX(2),LY(2),LZ(2)
          LOGICAL OK, OKT
#include "kernnumt/sysdat.inc"
          DATA EX /-0.3, 0.6, 1.1, 1.7, 2.1/
          DATA EY / 0.4, 0.7, 1.2, 1.6, 1.9/
          DATA EZ / 0.1, 0.8, 1.3, 1.5, 2.2/
          DATA X  /-0.5,-0.3, 0.1, 0.6, 0.9, 1.1, 1.3, 1.7, 1.9, 2.1,
     +              2.5/
          DATA Y  / 0.1, 0.4, 0.6, 0.7, 1.1, 1.2, 1.5, 1.6, 1.7, 1.9,
     +              2.1/
          DATA Z  /-0.2, 0.1, 0.5, 0.8, 0.9, 1.3, 1.4, 1.5, 2.1, 2.2,
     +              2.5/
C
          FCT(A,B,C)=(A**2+B**3+C**4 + 10.) * 0.1
C
          OK=.TRUE.
          NENT(1)=5
          NENT(2)=5
          DO 10 I=1,5
          DO 10 J=1,3
   10        LXYZ(I,J)=1
          LXYZ(2,4)=1
          LXYZ(2,5)=1
          DO 15 I=3,5
          DO 15 J=4,5
   15         LXYZ(I,J)=2
          LXYZ(3,6)=2
          LXYZ(3,7)=2
          DO 20 J=6,9
   20        LXYZ(4,J)=3
          LXYZ(5,6)=3
          LXYZ(5,7)=3
          DO 25 J=8,11
   25        LXYZ(5,J)=4
           DO 30 K=1,5
           DO 30 J=1,5
           DO 30 I=1,5
              IJK=I+NENT(1)*(J-1+NENT(2)*(K-1))
  30          TABLE(IJK)=FCT(EX(I),EY(J),EZ(K))
          DO 700 IEX=1,5
             NENT(1)=IEX
             DO 50 I=1,IEX
   50           ENT(I)=EX(I)
             NX=2*IEX+1
             NX2=NX-2
             DO 600 IEY=1,5
                NENT(2)=IEY
                DO 60 I=1,IEY
                   IIEY=I+IEX
   60              ENT(IIEY)=EY(I)
                NY=2*IEY+1
                NY2=NY-2
                DO 500 IEZ=1,5
                   NENT(3)=IEZ
                   DO 70 I=1,IEZ
                      IIEZ=IEX+IEY+I
   70                 ENT(IIEZ)=EZ(I)
                   NZ=2*IEZ+1
                   NZ2=NZ-2
                   DO 400 KX=1,NX
                      NARG=1
                      ARG(NARG)=X(KX)
                      LX(1)=LXYZ(IEX,KX)
                      LX(2)=LX(1)+1
                      DO 110 I=1,2
                         FX(I)=TABLE(LX(I))
  110                    CONTINUE
                      TREF=E104T1(NX2,ARG(1),EX(LX(1)),FX)
                      CALL E104P(NARG,ARG,NENT,ENT,TABLE,TREF,OKT)
                      OK  =  OK .AND. OKT
                      DO 300 KY=1,NY
                         NARG=2
                         ARG(NARG)=Y(KY)
                         LY(1)=LXYZ(IEY,KY)
                         LY(2)=LY(1)+1
                         DO 120 I=1,2
                         DO 120 J=1,2
                            IJ=LX(I)+NENT(1)*(LY(J)-1)
                            FXY(I,J)=TABLE(IJ)
  120                       CONTINUE
                         TREF=E104T2(NX2,NY2,ARG(1),ARG(2),EX(LX(1)),
     +                                        EY(LY(1)),FXY)
                         CALL E104P(NARG,ARG,NENT,ENT,TABLE,TREF,OKT)
                         OK  =  OK .AND. OKT
                         DO 200 KZ=1,NZ
                            NARG=3
                            ARG(NARG)=Z(KZ)
                            LZ(1)=LXYZ(IEZ,KZ)
                            LZ(2)=LZ(1)+1
                            DO 130 I=1,2
                            DO 130 J=1,2
                            DO 130 K=1,2
                               IJK=LX(I)+NENT(1)*(LY(J)-1+
     +                                        NENT(2)*(LZ(K)-1))
                               FXYZ(I,J,K)=TABLE(IJK)
  130                          CONTINUE
                            TREF=E104T3(NX2,NY2,NZ2,ARG(1),ARG(2),ARG(3)
     +                             ,EX(LX(1)),EY(LY(1)),EZ(LZ(1)),FXYZ)
                            CALL E104P(NARG,ARG,NENT,ENT,TABLE,TREF,OKT)
                            OK  =  OK .AND. OKT
  200                       CONTINUE
  300                    CONTINUE
  400                 CONTINUE
  500              CONTINUE
  600           CONTINUE
  700        CONTINUE
          IF(      ERPRNT .AND.       ERSTOP) WRITE(*,801)
          IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,802)
          IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,803)
          NARG  =  0
          T  =  FINT(NARG,ARG,NENT,ENT,TABLE)
          IF(T .NE. 0.) THEN
             OK  =  .FALSE.
             WRITE(*,804) NARG, T, 'E104.1'
          ENDIF
          NARG  =  6
          T  =  FINT(NARG,ARG,NENT,ENT,TABLE)
          IF(T .NE. 0.) THEN
             OK  =  .FALSE.
             WRITE(*,804) NARG, T, 'E104.1'
          ENDIF
      RETURN
  801 FORMAT(/' TWO ERROR AND ABEND MESSAGES SHOULD NOW FOLLOW ...')
  802 FORMAT(/' TWO ERROR MESSAGES SHOULD NOW FOLLOW ...')
  803 FORMAT(/' TWO ABEND MESSAGES SHOULD NOW FOLLOW ...')
  804 FORMAT(/' ????? TEST OF FINT ... NARG =', I6, ' FINT =', E20.10,
     +       ' ERROR CONDITION ', A6, ' NOT DETECTED.')
          END
          SUBROUTINE E104P(NARG,ARG,NENT,ENT,TABLE,TREF,OK)
          DIMENSION ARG(NARG),NENT(NARG),ENT(2),TABLE(2)
#include "kernnumt/sysdat.inc"
           LOGICAL OK
          DATA MARGIN /100/
          IRESF(RES)=NINT(RES/RELPR)
          TEST=FINT(NARG,ARG,NENT,ENT,TABLE)
          RES=ABS(TREF-TEST)
          IREL=IRESF(RES)
          OK=IREL .LE. MARGIN
          IF(.NOT. OK) WRITE(*,10)NARG,IREL,RES,(ARG(I),I=1,NARG)
          RETURN
   10     FORMAT(/ 25H *** ARITHMETIC ERROR *** , 2I8,1X,E12.3,3X,
     +           5E12.3)
          END
          FUNCTION E104T1(NX,X,AX,FX)
          DIMENSION AX(2),FX(2)
          E104T1=FX(1)
          IF(NX .EQ. 1) RETURN
          XI=(AX(2)-X)/(AX(2)-AX(1))
          E104T1=XI*FX(1)+(1.-XI)*FX(2)
          RETURN
          END
          FUNCTION E104T2(NX,NY,X,Y,AX,AY,FXY)
          DIMENSION AX(2),AY(2),FXY(2,2),FA(2),FX(2)
          IF(NY .EQ. 1) GO TO 10
          FX(1)=FXY(1,1)
          FX(2)=FXY(2,1)
          FA(1)=E104T1(NX,X,AX,FX)
          FX(1)=FXY(1,2)
          FX(2)=FXY(2,2)
          FA(2)=E104T1(NX,X,AX,FX)
          E104T2=E104T1(NY,Y,AY,FA)
          RETURN
   10     CONTINUE
          FX(1)=FXY(1,1)
          FX(2)=FXY(2,1)
          E104T2=E104T1(NX,X,AX,FX)
          RETURN
          END
          FUNCTION E104T3(NX,NY,NZ,X,Y,Z,AX,AY,AZ,FXYZ)
          DIMENSION AX(2),AY(2),AZ(2),FXYZ(2,2,2),FXY(2,2),FZ(2)
          IF(NZ .EQ. 1) GO TO 30
          DO 10 I=1,2
          DO 10 J=1,2
   10        FXY(I,J)=FXYZ(I,J,1)
          FZ(1)=E104T2(NX,NY,X,Y,AX,AY,FXY)
          DO 20 I=1,2
          DO 20 J=1,2
   20        FXY(I,J)=FXYZ(I,J,2)
          FZ(2)=E104T2(NX,NY,X,Y,AX,AY,FXY)
          E104T3=E104T1(NZ,Z,AZ,FZ)
          RETURN
   30     CONTINUE
          DO 40 I=1,2
          DO 40 J=1,2
   40        FXY(I,J)=FXYZ(I,J,1)
          E104T3=E104T2(NX,NY,X,Y,AX,AY,FXY)
          RETURN
          END
