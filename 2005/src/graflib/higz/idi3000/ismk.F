*
* $Id: ismk.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: ismk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.07/00 29/05/89  12.18.25  by  O.Couet
*-- Author :
      SUBROUTINE ISMK(MTYPE)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the marker symbol.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JCMARK (CVALUE)
*.
*.   Parameters:
*.   -----------
*.     CVALUE                     [ INTEGER; Input ]
*.
*.     - The new value for the marker symbol.
*.
*.       *NOTE* : CVALUE must be in the range 1...32767.
*.       *NOTE* : The marker SIZE cannot be changed in DI-3000.
*.
*.       Marker numbers 1..5 are the same on all graphics devices:
*.
*.         1 - point or dot       .
*.         2 - plus sign          +
*.         3 - asterisk           *
*.         4 - capital letter     O
*.         5 - capital letter     X
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/di3seg.inc"
*.______________________________________
*
      IMK=MTYPE
      IF (GFLAG) THEN
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         CALL JCMARK(MTYPE)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IMKCO)
#endif
*
      END
#endif
