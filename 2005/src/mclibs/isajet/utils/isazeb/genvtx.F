*
* $Id: genvtx.F,v 1.10 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: genvtx.F,v $
* Revision 1.10  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE GENVTX(MOM,JDPAR,XYZB,XYZ)
C-------------------------------------------------------------
C-
C-   Calculate the decay length and vertices for short lived
C-   particles: D0,D0BAR,D+,D-,F+,F-,B0,B0BAR,B+,B-,BS,BSBAR,
C-              T0,T0BAR,T+,T-,TS+,TS-,TAU+,TAU-
C-              C,B and T baryons
C-
C-   INPUT:
C-   MOM= 4-momenta, mass
C-   JDPAR= particle id
C-   XYZB= x,y and z of parent vertex
C-
C-   OUTPUT:
C-   XYZ= x,y and z of decay vertex
C-        if it is not an allowed decay XYZ is set to XYZB
C-
C-    Written by SDP Dec.,1985
C-
C-------------------------------------------------------------------
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C
      REAL MOM(5),XYZ(3),XYZB(3)
      INTEGER JDPAR
      INTEGER MAP(10)       ! mapping from particle id to index
      INTEGER I,INDX,ID,IFL
      REAL ETACT,RANF,P,DCAYL
      REAL CTAU(13)
      DATA CTAU/
     1  .010,               ! tau lifetime
     2  .028,               ! D-    "
     3  .013,               ! D0    "
     4  .006,               ! F-    "
     5  .042,.042,          ! B+,B0 lifetimes
     7  .021,               ! BS  lifetime
     8  3*.0001,            ! top lifetimes
     9  .007,               ! charm baryon lifetimes
     A  .010,               ! beauty  "        "
     B  .0001/              ! top     "        "
      DATA MAP/ 16, 240, 140, 340, 150, 250, 350,
     1         160, 260, 360/
C
C  find the appropriate index for this particle
      CALL UCOPY(XYZB,XYZ,3)
      INDX=0
      ID=IABS(JDPAR)
C
      IF(MOD(ID,10).EQ.0) THEN    ! check for lowest spin hadron
C
C  find index for mesons
        IF(ID.LT.1000) THEN
          DO 1 I=2,10
          IF(MAP(I).EQ.ID) INDX=I
    1     CONTINUE
        ELSE
C  find index for baryons
          IFL=MOD(ID,100)/10       ! find heaviest flavor
          IF(IFL.GT.3) INDX=7+IFL  ! check for heavy quark
C
        ENDIF
C
      ENDIF
      IF(MAP(1).EQ.ID) INDX=1           ! check for taus
      IF(INDX.GT.0.AND.INDX.LT.13) THEN
        P=SQRT(MOM(1)**2+MOM(2)**2+MOM(3)**2)
        ETACT=P/MOM(5)*CTAU(INDX)
        DCAYL=-ETACT*ALOG(RANF())     ! generate decay length
C  calculate decay vertex position
        XYZ(1)=MOM(1)/P*DCAYL+XYZB(1)
        XYZ(2)=MOM(2)/P*DCAYL+XYZB(2)
        XYZ(3)=MOM(3)/P*DCAYL+XYZB(3)
      ENDIF
C
      RETURN
      END
