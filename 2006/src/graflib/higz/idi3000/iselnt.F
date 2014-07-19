*
* $Id: iselnt.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: iselnt.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.21/03 01/06/94  13.29.03  by  O.Couet
*-- Author :
      SUBROUTINE ISELNT(NT)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Select a normalization transformation as the
*.     transformation.
*.
*.   DI-3000 access:
*.   ---------------
*.     ***** ***** *****
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hint.inc"
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
#include "higz/di3seg.inc"
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
            IF(SEGOPN)THEN
               CALL JCLOSE
               SEGOPN=.FALSE.
            ENDIF
            CALL JWINDO(RWXMIN
     +,                 RWXMAX
     +,                 RWYMIN
     +,                 RWYMAX)
            CALL JVPORT(RVXMIN*SCALE-(RDWXMI*REDIT)
     +,                 RVXMAX*SCALE-(RDWXMI*REDIT)
     +,                 RVYMIN*SCALE-(RDWYMI*REDIT)
     +,                 RVYMAX*SCALE-(RDWYMI*REDIT))
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
            IF(SEGOPN)THEN
               CALL JCLOSE
               SEGOPN=.FALSE.
            ENDIF
            CALL JWINDO(RWXMIN
     +,                 RWXMAX
     +,                 RWYMIN
     +,                 RWYMAX)
            CALL JVPORT(RVXMIN*SCALE-(RDWXMI*REDIT)
     +,                 RVXMAX*SCALE-(RDWXMI*REDIT)
     +,                 RVYMIN*SCALE-(RDWYMI*REDIT)
     +,                 RVYMAX*SCALE-(RDWYMI*REDIT))
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
            IF(SEGOPN)THEN
               CALL JCLOSE
               SEGOPN=.FALSE.
            ENDIF
            CALL JWINDO(RWXMIN
     +,                 RWXMAX
     +,                 RWYMIN
     +,                 RWYMAX)
            CALL JVPORT(RVXMIN*SCALE-(RDWXMI*REDIT)
     +,                 RVXMAX*SCALE-(RDWXMI*REDIT)
     +,                 RVYMIN*SCALE-(RDWYMI*REDIT)
     +,                 RVYMAX*SCALE-(RDWYMI*REDIT))
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
*
   10 CONTINUE
#endif
#if defined(CERNLIB_NTC)
      IF(IGNSCU(NT).EQ.0)CALL IGNCRE(NT)
      CALL IGNGET(I0,I1,IHPRIO
     +,           RWXMIN,RWXMAX,RWYMIN,RWYMAX
     +,           RVXMIN,RVXMAX,RVYMIN,RVYMAX,I3D)
      CALL IGNSPR(IHPRIO+1)
      IF(GFLAG)THEN
         IF(SEGOPN)THEN
            CALL JCLOSE
            SEGOPN=.FALSE.
         ENDIF
         CALL JWINDO(RWXMIN
     +,              RWXMAX
     +,              RWYMIN
     +,              RWYMAX)
         CALL JVPORT(RVXMIN*SCALE-(RDWXMI*REDIT)
     +,              RVXMAX*SCALE-(RDWXMI*REDIT)
     +,              RVYMIN*SCALE-(RDWYMI*REDIT)
     +,              RVYMAX*SCALE-(RDWYMI*REDIT))
      ENDIF
      IF(I3D.NE.0)CALL IGSVIE
*
#endif
#if (defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL))&&(!defined(CERNLIB_3D))
      IF(GLFLAG)CALL IZSELN(NT)
#endif
      INTR=NT
      CALL ISCLIP(ICLIP)
*
      END
#endif
