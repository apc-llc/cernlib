*
* $Id: iselnt.F,v 1.2 1996/06/05 10:20:42 cernlib Exp $
*
* $Log: iselnt.F,v $
* Revision 1.2  1996/06/05 10:20:42  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:45  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.21/03 31/05/94  11.25.13  by  O.Couet
*-- Author :
      SUBROUTINE ISELNT(NT)
*.===========>
*.
*.   This routine selects the transformation number NT and its priority
*. becomes the highest .
*.
*. _Input parameters:
*.
*.  INTEGER NT : Normalization transformation number (0<=NT<NBNTMX).
*.
*..==========> (O.Couet)
#if !defined(CERNLIB_NTC)
#include "higz/hint.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
*.______________________________________
*
      SCALE=1.
      IF(REDIT.NE.0.)SCALE=REDIT
*
#if !defined(CERNLIB_NTC)
      CALL IZGNT(NT,IFIND,IAWV,IPRIO,IHPRIO)
*
*              Transformation No 0
*
      IF(NT.EQ.0)THEN
         NT0PRI=IHPRIO+1
         RWXMIN=0.
         RWYMIN=0.
         RWXMAX=1.
         RWYMAX=1.
         RVXMIN=0.
         RVYMIN=0.
         RVXMAX=1.
         RVYMAX=1.
         IF(GFLAG)THEN
            CALL GSWN(1,RWXMIN,RWXMAX,RWYMIN,RWYMAX)
            CALL GSVP(1,RVXMIN*SCALE-(RDWXMI*REDIT)
     +,                 RVXMAX*SCALE-(RDWXMI*REDIT)
     +,                 RVYMIN*SCALE-(RDWYMI*REDIT)
     +,                 RVYMAX*SCALE-(RDWYMI*REDIT))
            CALL GSELNT(1)
         ENDIF
         GOTO 10
      ENDIF
*
*              NT exits in the main transformation storage
*
      IF(IFIND.NE.0)THEN
         RWXMIN=WNVPST(IAWV)
         RWYMIN=WNVPST(IAWV+2)
         RWXMAX=WNVPST(IAWV+1)
         RWYMAX=WNVPST(IAWV+3)
         RVXMIN=WNVPST(IAWV+4)
         RVYMIN=WNVPST(IAWV+6)
         RVXMAX=WNVPST(IAWV+5)
         RVYMAX=WNVPST(IAWV+7)
         NTSTOR(IFIND)=IHPRIO+1
         IF(GFLAG)THEN
            CALL GSWN(1,RWXMIN,RWXMAX,RWYMIN,RWYMAX)
            CALL GSVP(1,RVXMIN*SCALE-(RDWXMI*REDIT)
     +,                 RVXMAX*SCALE-(RDWXMI*REDIT)
     +,                 RVYMIN*SCALE-(RDWYMI*REDIT)
     +,                 RVYMAX*SCALE-(RDWYMI*REDIT))
            CALL GSELNT(1)
         ENDIF
*
*              NT does not exit in the main transformation storage
*
      ELSE
         RWXMIN=0.
         RWYMIN=0.
         RWXMAX=1.
         RWYMAX=1.
         RVXMIN=0.
         RVYMIN=0.
         RVXMAX=1.
         RVYMAX=1.
         IF(GFLAG)THEN
            CALL GSWN(1,RWXMIN,RWXMAX,RWYMIN,RWYMAX)
            CALL GSVP(1,RVXMIN*SCALE-(RDWXMI*REDIT)
     +,                 RVXMAX*SCALE-(RDWXMI*REDIT)
     +,                 RVYMIN*SCALE-(RDWYMI*REDIT)
     +,                 RVYMAX*SCALE-(RDWYMI*REDIT))
            CALL GSELNT(1)
         ENDIF
         NBNT=NTSTOR(1)
         IF(NBNT+1.GT.NBNTMX)THEN
            CALL IGERR('Too many normalisation transformation','ISELNT')
            RETURN
         ENDIF
         NTSTOR(2*NBNT+2)=NT
         NTSTOR(2*NBNT+3)=IHPRIO+1
         WNVPST(8*NBNT+1)=0.
         WNVPST(8*NBNT+2)=1.
         WNVPST(8*NBNT+3)=0.
         WNVPST(8*NBNT+4)=1.
         WNVPST(8*NBNT+5)=0.
         WNVPST(8*NBNT+6)=1.
         WNVPST(8*NBNT+7)=0.
         WNVPST(8*NBNT+8)=1.
         NTSTOR(1)=NTSTOR(1)+1
      ENDIF
   10 CONTINUE
*
#endif
#if defined(CERNLIB_NTC)
      IF(IGNSCU(NT).EQ.0)CALL IGNCRE(NT)
      CALL IGNGET(I0,I1,IHPRIO
     +,           RWXMIN,RWXMAX,RWYMIN,RWYMAX
     +,           RVXMIN,RVXMAX,RVYMIN,RVYMAX,I3D)
      CALL IGNSPR(IHPRIO+1)
      IF(GFLAG)THEN
         CALL GSWN(1,RWXMIN,RWXMAX,RWYMIN,RWYMAX)
         CALL GSVP(1,RVXMIN*SCALE-(RDWXMI*REDIT)
     +,              RVXMAX*SCALE-(RDWXMI*REDIT)
     +,              RVYMIN*SCALE-(RDWYMI*REDIT)
     +,              RVYMAX*SCALE-(RDWYMI*REDIT))
         CALL GSELNT(1)
      ENDIF
      IF(I3D.NE.0)CALL IGSVIE
*
#endif
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZSELN(NT)
#endif
      INTR=NT
      CALL ISCLIP(ICLIP)
*
      END
#endif
