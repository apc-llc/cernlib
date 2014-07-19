*
* $Id: d704ch.F,v 1.1.1.1 1996/02/15 17:48:42 mclareni Exp $
*
* $Log: d704ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:42  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE D704CH(NDIM,Y,Z,NREP,OK)
          COMPLEX   Y(NDIM),  Z(NDIM),  CS,       C1,       C2
          LOGICAL   OK
#include "kernnumt/sysdat.inc"
          DATA      C1, C2    /  (-10.,-10.), (+10.,+10.) /
          DATA      MARG      /  2000  /
          IRESF(RES)  =  NINT(RES/RELPR)
          OK  =  .TRUE.
          DO 50     IREP  =  1, NREP
             DO 40  M  =  1, 8
                N  =  2**M
                IF(N .GT. NDIM)  GOTO 900
                CALL CVRAN(N,C1,C2,Z(1),Z(2))
                CALL CVCPY(N,Z(1),Z(2),Y(1),Y(2))
                CALL CFFT(Y,-M)
                CALL CFFT(Y,M)
                CS  =  1./FLOAT(N)
                CALL CVSCL(N,CS,Y(1),Y(2),Y(1),Y(2))
                DO 20   I  =  1, N
                   IREL  =  IRESF(CABS((Y(I)-Z(I))/Z(I)))
                   IF(IREL .LE. MARG)  GOTO 20
                      WRITE(*,1000) I, IREL, MARG
                      OK  =  .FALSE.
  20               CONTINUE
  40            CONTINUE
  50         CONTINUE
          RETURN
 
 900      WRITE(*,9000) NDIM
          OK  =  .FALSE.
          RETURN
1000      FORMAT(37H ***** SUBROUTINE CFFT (D704) ... I =,I5,3X,
     +           7HIREL  =,I8,18H EXCEEDS MARGIN  =,I6)
9000      FORMAT( / 33H ***** SUBROUTINE CFFT (D704) ...,5X,
     +           26HSPACE AVAILABLE ... NDIM =,I6,16H IS INSUFFICIENT)
      END
#if defined(CERNLIB_NUMTIME)
#endif
