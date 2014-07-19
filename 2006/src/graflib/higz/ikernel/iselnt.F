*
* $Id: iselnt.F,v 1.1.1.1 1996/02/14 13:10:57 mclareni Exp $
*
* $Log: iselnt.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:57  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/03 31/05/94  11.24.05  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISELNT(NT)
      ENTRY      GSELNT(NT)
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
#include "higz/hint.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
#if defined(CERNLIB_PHIGS)
      DIMENSION RMOR(3,3),RMAP(3,3),VWWNLM(4),PJVPLM(4)
*
#endif
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
      CALL IZGNT(NT,IFIND,IAWV,IPRIO,IHPRIO)
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
         GOTO 10
      ENDIF
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
      ELSE
         RWXMIN=0.
         RWYMIN=0.
         RWXMAX=1.
         RWYMAX=1.
         RVXMIN=0.
         RVYMIN=0.
         RVXMAX=1.
         RVYMAX=1.
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
#if defined(CERNLIB_NTC)||defined(CERNLIB_X11)
      IF(IGNSCU(NT).EQ.0)CALL IGNCRE(NT)
      CALL IGNGET(I0,I1,IHPRIO
     +,           RWXMIN,RWXMAX,RWYMIN,RWYMAX
     +,           RVXMIN,RVXMAX,RVYMIN,RVYMAX,I3D)
      CALL IGNSPR(IHPRIO+1)
      IF(I3D.NE.0)CALL IGSVIE
*
#endif
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZSELN(NT)
#endif
#if !defined(CERNLIB_PHIGS)
      XRATIO=(RVXMAX-RVXMIN)/(RWXMAX-RWXMIN)
      YRATIO=(RVYMAX-RVYMIN)/(RWYMAX-RWYMIN)
#endif
#if defined(CERNLIB_PHIGS)
      VWWNLM(1)=RWXMIN
      VWWNLM(2)=RWXMAX
      VWWNLM(3)=RWYMIN
      VWWNLM(4)=RWYMAX
      PJVPLM(1)=RVXMIN
      PJVPLM(2)=RVXMAX
      PJVPLM(3)=RVYMIN
      PJVPLM(4)=RVYMAX
      CALL PEVOM(0.,0.,0.,1.,IERR,RMOR)
      CALL PEVMM(VWWNLM,PJVPLM,IERR,RMAP)
      IF(NT.NE.0)THEN
         DO 20 I=1,INOPWK
            CALL PSVWR(ILOPWK(I),NT,RMOR,RMAP,PJVPLM,1)
            CALL PUWK(ILOPWK(I),1)
   20    CONTINUE
      ENDIF
      CALL PSVWI(NT)
#endif
      INTR=NT
      CALL ISCLIP(ICLIP)
      END
