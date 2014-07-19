*
* $Id: izhist.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izhist.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
*CMZ :  1.23/00 25/04/95  11.18.53  by  O.Couet
*-- Author :
      SUBROUTINE IZHIST(N,X,Y,IOPT,IOPTZ)
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
         IF(IOPT(8).NE.0)THEN
            IF(IZPUSH(3,2*N+2,0,'IGHIST').NE.0)RETURN
         ELSE
            IF(IZPUSH(3,N+2,0,'IGHIST').NE.0)RETURN
         ENDIF
         CALL IZCFA(IBOFCO,1)
         CALL IZCFA(IBWDCO,1)
         CALL IZCFA(ILNCO,1)
         CALL IZCFA(ILWSCO,1)
         CALL IZCFA(IPLCCO,1)
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
         CALL IZSTCC(IHISCO,INTPTR)
         IQ(LHI+INTPTR)=IFLPTR
         IQ(LHI+INTPTR+1)=N
         DO 10 I=1,14
            IF(IOPT(I).NE.0)CALL SBIT1(IQ(LHI+INTPTR+2),I)
   10    CONTINUE
         CALL IZINCI(3)
         NN=2
         IF(IOPT(5).NE.0)THEN
            IF(IOPTZ.NE.0)THEN
               Q(LHF+IFLPTR)=Q(LYADR)
               Q(LHF+IFLPTR+1)=Q(LYADR+1)
            ELSE
               Q(LHF+IFLPTR)=Y(1)
               Q(LHF+IFLPTR+1)=Y(2)
            ENDIF
            IF(IOPT(8).NE.0)THEN
               NN=N+1
               DO 20 I=3,NN
                  IF(IOPTZ.NE.0)THEN
                     Q(LHF+IFLPTR+I-1)=Q(LYADR+I-1)
                  ELSE
                     Q(LHF+IFLPTR+I-1)=Y(I)
                  ENDIF
   20          CONTINUE
            ENDIF
            CALL IZINCF(NN)
            DO 30 I=1,N
               IF(IOPTZ.NE.0)THEN
                  Q(LHF+IFLPTR+I-1)=Q(LXADR+I-1)
               ELSE
                  Q(LHF+IFLPTR+I-1)=X(I)
               ENDIF
   30       CONTINUE
         ELSE
            IF(IOPTZ.NE.0)THEN
               Q(LHF+IFLPTR)=Q(LXADR)
               Q(LHF+IFLPTR+1)=Q(LXADR+1)
            ELSE
               Q(LHF+IFLPTR)=X(1)
               Q(LHF+IFLPTR+1)=X(2)
            ENDIF
            IF(IOPT(8).NE.0)THEN
               NN=N+1
               DO 40 I=3,NN
                  IF(IOPTZ.NE.0)THEN
                     Q(LHF+IFLPTR+I-1)=Q(LXADR+I-1)
                  ELSE
                     Q(LHF+IFLPTR+I-1)=X(I)
                  ENDIF
   40          CONTINUE
            ENDIF
            CALL IZINCF(NN)
            DO 50 I=1,N
               IF(IOPTZ.NE.0)THEN
                  Q(LHF+IFLPTR+I-1)=Q(LYADR+I-1)
               ELSE
                  Q(LHF+IFLPTR+I-1)=Y(I)
               ENDIF
   50       CONTINUE
         ENDIF
         CALL IZINCF(N)
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,A10,I5)') IHISCO,CHOPTM,N
         CALL IMWRIT(1)
         IF(IOPT(5).NE.0)THEN
            IF(IOPT(8).NE.0)THEN
               CALL IMFOUT(N+1,Y)
            ELSE
               CALL IMFOUT(2,Y)
            ENDIF
            CALL IMFOUT(N,X)
         ELSE
            IF(IOPT(8).NE.0)THEN
               CALL IMFOUT(N+1,X)
            ELSE
               CALL IMFOUT(2,X)
            ENDIF
            CALL IMFOUT(N,Y)
         ENDIF
         CALL IMWRIT(5)
      ENDIF
#endif
*
      END
#endif
