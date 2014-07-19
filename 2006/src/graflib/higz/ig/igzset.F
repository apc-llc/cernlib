*
* $Id: igzset.F,v 1.1.1.1 1996/02/14 13:10:41 mclareni Exp $
*
* $Log: igzset.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:41  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/09 14/09/94  15.50.32  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGZSET(CHOPT)
*.===========>
*.
*.   This routine sets the HIGZ working mode.
*.
*. _Input parameters:
*.
*.  CHARACTER CHOPT : Option to be set .
*.
*.                    CHOPT='G'raphics mode ON
*.                    CHOPT='Z'ebra mode ON, 'G' becomes OFF
*.                    CHOPT='X' Zebra mode OFF and leave 'G'
*.                    CHOPT='S'ave the current state of the FLAGS
*.                    CHOPT='R'estore the last state saved
*.                    CHOPT='E'xpand mode: in this mode attributes
*.                           are stored in the pictures without checking
*.                           if the current value is the same. This
*.                           allows to have each primitive with its
*.                           attributes.
*.                    CHOPT='C'ompress (.NOT.'E')
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
      CHARACTER*(*) CHOPT
      DIMENSION IOPT(7)
      EQUIVALENCE (IOPTG,IOPT(1)) , (IOPTZ,IOPT(2))
      EQUIVALENCE (IOPTS,IOPT(3)) , (IOPTR,IOPT(4))
      EQUIVALENCE (IOPTE,IOPT(5)) , (IOPTC,IOPT(6))
      EQUIVALENCE (IOPTX,IOPT(7))
      LOGICAL ZFLAGS,GFLAGS
      SAVE ZFLAGS,GFLAGS
*.______________________________________
*
      CALL UOPTC(CHOPT,'GZSRECX',IOPT)
*
      IF(IOPTE.NE.0)THEN
         CFLAG = .FALSE.
         RETURN
      ENDIF
      IF(IOPTC.NE.0)THEN
         CFLAG = .TRUE.
         RETURN
      ENDIF
*
      IF(IOPTS.NE.0)THEN
         ZFLAGS = ZFLAG
         GFLAGS = GFLAG
         RETURN
      ENDIF
*
      IF(IOPTX.NE.0)THEN
         ZFLAG  = .FALSE.
         GOTO 10
      ENDIF
*
      IF(IOPTR.NE.0)THEN
         IF (ZFLAGS) IOPTZ = 1
         IF (GFLAGS) IOPTG = 1
      ENDIF
*
      GLFLAG = .FALSE.
      GFLAG  = .FALSE.
      ZFLAG  = .FALSE.
*
      IF ((IOPTG.NE.0).AND.(.NOT.MFLAG)) GFLAG = .TRUE.
      IF (IOPTZ.NE.0) ZFLAG = .TRUE.
   10 GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
