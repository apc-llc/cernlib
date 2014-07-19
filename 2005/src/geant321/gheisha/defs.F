*
* $Id: defs.F,v 1.2 1999/11/14 19:21:18 japost Exp $
*
* $Log: defs.F,v $
* Revision 1.2  1999/11/14 19:21:18  japost
*  Drastic modification of function, as by Andrea Venturi,   8-Nov-1999
*
* His extended explanation of problem's effect:
*
*  The Fermi motion is simulated by adding a random vector, transverse to
* the incoming particle, to the final state particles total momentum
* vector. Then the final state particles are rotated in such a way that
* their total momentum PLUS the Fermi kick is along the direction of the
* incoming particle. In this way the total momentum of the final state
* particles will have a random transverse kick.
*
* Unfortunately the routine DEFS (defs.F) used to generate the parameters of the
* rotation is such that despite the fact the original Fermi kick is random in
* the transverse plane, after the rotation the kick is ALWAYS in the same
* direction (Positive Y).   This cause a bias between the direction of the
* incoming   particle and the position of the hadronic shower.
*
* This is the different rotation routine which preserves the randomness and
* we has been tested in ALEPH these past days.
* ( I can provide you some plots where the effect is clearly visible.
* A brief description of the bugs and of its effect can be found here:
* URL http://www.pi.infn.it/~venturi/Gheisha.html )
*
* Revision 1.1.1.1  1995/10/24 10:21:07  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.38  by  S.Giani
*-- Author :
C
C *** VARIOUS VECTOR OPERATIONS ***
C
C COPIED FROM F14BLO.PAMLIB 23.4.82
C AUTHOR: V.BLOBEL (UNIVERSITY OF HAMBURG)
C DESYLIB
C
C *** BLANK COMMON REPLACED BY /VECUTY/ TO MATCH GEANT/GHEISHA CODE ***
C *** NOTE THAT P(10,100) HAS BECOME PV(10,200) DUE TO THIS         ***
C
C UN-USED ENTRIES REMOVED :
C     "PCOP" "PEXC" "PZER" "PWRT" "DOT4" "IMPU" "IMPULI" "ADD3"
C     "SUB3" "CROSS" "DOT" "SMUL" "NORZ" "PARPER" "PUNIT" "TRAP"
C
C *** ALL ENTRIES RE-WRITTEN AS SUBROUTINES USING ONLY NECESSARY ***
C *** "DOUBLE PRECISION" STMTS. AND ALL SPECIFIC FUNCTIONS HAVE  ***
C *** BEEN CHANGED TO THEIR GENERIC EQUIVALENCES                 ***
C *** NVE 29-MAR-1988 CERN GENEVA ***
C
C ORIGIN : H.FESEFELDT (22-JUNE-1984)
C
      SUBROUTINE DEFS(K,L,M)
C
C --- PV-ARRAY ---
#include "geant321/mxgkgh.inc"
#include "geant321/s_blankp.inc"
C
      DOUBLE PRECISION A,B,C,D
C
      MX=M
      MY=M+1
      MZ=M+2
      DO 52 I=1,3
         H=PV(I,L)
         PV(I,MZ)=H
 52   CONTINUE
      A=0.0
      DO 44 I=1,3
         A=A+PV(I,MZ)*PV(I,MZ)
 44   CONTINUE
      B=SQRT(A)
      IF (B .NE. 0.0) B=1.0/B
      DO 46 I=1,3
         PV(I,MZ)=B*PV(I,MZ)
 46   CONTINUE
C
      A=PV(3,MZ)
      IF(A.GT.1) A=1.D0
      IF(A.LT.-1) A=-1.D0
      B=SQRT(MAX(1.D0-A*A,0.D0))
      C=0.D0
      D=0.D0
      IF(B.NE.0.D0) THEN
         C=PV(1,MZ)/B
         D=PV(2,MZ)/B
      ENDIF
C
      PV(1,MX)=1+C**2*(A-1)
      PV(2,MX)=C*D*(A-1)
      PV(3,MX)=-B*C
      PV(1,MY)=C*D*(A-1)
      PV(2,MY)=1+D**2*(A-1)
      PV(3,MY)=-B*D
C
      DO 58 J=MX,MZ
         A=0.0
         DO 54 I=1,3
            A=A+PV(I,J)*PV(I,J)
 54      CONTINUE
         B=SQRT(A)
         IF (B .NE. 0.0) B=1.0/B
         if(b.eq.0) then
            write(6,*) 'Warning in defs.F: b is equal to 0 !! at',j-mx+1
         endif
         DO 56 I=1,3
            PV(I,J)=B*PV(I,J)
 56      CONTINUE
 58   CONTINUE
      RETURN
      END
