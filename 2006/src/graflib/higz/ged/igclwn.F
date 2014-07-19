*
* $Id: igclwn.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: igclwn.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 16/12/88  14.00.46  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGCLWN(NTCL)
*.===========>
*.
*.   This routine clears the window NTCL
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiflag.inc"
      LOGICAL ZFS,GFS
*.______________________________________
*
      IF(NTCL.EQ.0)THEN
         CALL IGCLES
         RETURN
      ELSE
         CALL IZSAV
         ZFS=ZFLAG
         GFS=GFLAG
         ZFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         GFLAG=.TRUE.
*
         CALL ISELNT(NTCL)
         IF(LHNT.EQ.0)RETURN
         ZFLAG=.FALSE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
         IWVA=IQ(LHNT+2)
         X1CL=Q(LHF+IWVA)
         X2CL=Q(LHF+IWVA+1)
         Y1CL=Q(LHF+IWVA+2)
         Y2CL=Q(LHF+IWVA+3)
         CALL IGSET('BORD',0.)
         CALL ISFACI(0)
         CALL ISFAIS(1)
         CALL IGBOX(X1CL,X2CL,Y1CL,Y2CL)
         CALL IZSET
         ZFLAG=ZFS
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         GFLAG=GFS
      ENDIF
*
      END
