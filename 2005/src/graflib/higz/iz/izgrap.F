*
* $Id: izgrap.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izgrap.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
*CMZ :  1.23/00 25/04/95  11.19.19  by  O.Couet
*-- Author :
      SUBROUTINE IZGRAP(N,X,Y,IOPT,IOPTZ)
*.===========>
*.
*.   This routine stores in the current picture the necessary data to
*. to draw a graphic .
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of points .
*.  REAL X(N or 2 or N+1) : X coordinates .
*.  REAL Y(N or 2 or N+1) : Y coordinates .
*.  INTEGER IOPT : Option .
*.
*..==========> (O.Couet)
#include "higz/hicode.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
      EQUIVALENCE (LXADR,L2NTLA),(LYADR,LDNTLA)
      DIMENSION IOPT(*)
      DIMENSION X(2),Y(2)
*.______________________________________
*
#include "higz/hibit.inc"
#if defined(CERNLIB_ZEBRA)
*
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(3,2*N,0,'IGRAPH').NE.0)RETURN
         CALL IZCFA(IBOFCO,1)
         CALL IZCFA(IBWDCO,1)
         CALL IZCFA(ILNCO,1)
         CALL IZCFA(ILWSCO,1)
         CALL IZCFA(IPLCCO,1)
         CALL IZCFA(IBSLCO,1)
         CALL IZCFA(IFAICO,1)
         CALL IZCFA(IFASCO,1)
         CALL IZCFA(IFACCO,1)
         CALL IZCFA(ITMSCO,1)
         CALL IZCFA(IALHCO,1)
         CALL IZCFA(IALDCO,1)
         CALL IZCFA(ITXCCO,1)
         CALL IZCFA(ITXFCO,1)
         CALL IZCFA(IGTSCO,1)
         CALL IZCFA(IGTWCO,1)
         CALL IZCFA(IAWLCO,1)
         CALL IZCFA(IMKCO,1)
         CALL IZCFA(IMKSCO,1)
         CALL IZCFA(IPMCCO,1)
         CALL IZCFA(IBORCO,1)
*
         CALL IZSTCC(IGRACO,INTPTR)
         IQ(LHI+INTPTR)   = IFLPTR
         IQ(LHI+INTPTR+1) = N
         DO 10 I=1,12
            IF(IOPT(I).NE.0)CALL SBIT1(IQ(LHI+INTPTR+2),I)
   10    CONTINUE
         CALL IZINCI(3)
         IF(IOPT(7).NE.0)THEN
            DO 20 I=1,N
               IF(IOPTZ.NE.0)THEN
                  Q(LHF+IFLPTR+I-1)   = Q(LYADR+I-1)
                  Q(LHF+IFLPTR+I+N-1) = Q(LXADR+I-1)
               ELSE
                  Q(LHF+IFLPTR+I-1)   = Y(I)
                  Q(LHF+IFLPTR+I+N-1) = X(I)
               ENDIF
   20       CONTINUE
         ELSE
            DO 30 I=1,N
               IF(IOPTZ.NE.0)THEN
                  Q(LHF+IFLPTR+I-1)   = Q(LXADR+I-1)
                  Q(LHF+IFLPTR+I+N-1) = Q(LYADR+I-1)
               ELSE
                  Q(LHF+IFLPTR+I-1)   = X(I)
                  Q(LHF+IFLPTR+I+N-1) = Y(I)
               ENDIF
   30       CONTINUE
         ENDIF
         CALL IZINCF(2*N)
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,A10,I5)') IGRACO,CHOPTM,N
         CALL IMWRIT(1)
         CALL IMFOUT(N,X)
         CALL IMFOUT(N,Y)
         CALL IMWRIT(5)
      ENDIF
#endif
*
      END
#endif
