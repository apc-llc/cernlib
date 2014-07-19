*
* $Id: iopks.F,v 1.1.1.1 1996/02/14 13:10:28 mclareni Exp $
*
* $Log: iopks.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:28  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.20/06 08/12/93  10.14.24  by  O.Couet
*-- Author :
      SUBROUTINE IOPKS(ERRFIL)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Initialization is done by IGSSE and will not be repeated.
*.     ***** ***** ***** User *MUST* call IGSSE. ***** ***** *****
*.     This routine further sets the debug log file and graphics level
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JFILES      ! Override default for logical unit
*.     CALL JSETDB      ! Set the DI3000 graphics debugging level
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/di3dev.inc"
#include "higz/di3met.inc"
#include "higz/hivers.inc"
      CHARACTER*16 TCARD
      INTEGER ERRDEB
      INTEGER ERRFIL
*.______________________________________
*
      CALL UHTOC(
     + 40H CPP_TITLE                 
#if !defined(CERNLIB_CRAY)
     +,4,TCARD,16)
#endif
#if defined(CERNLIB_CRAY)
     +,8,TCARD,16)
#endif
      CHVERS = TCARD(9:16)
*
      IF (.NOT.ENTRY_FLAG) THEN
         CALL JBEGIN
         ENTRY_FLAG = .TRUE.
      ENDIF
      CALL JFILES (1, 0, ERRFIL)
      CALL JSETER (0)

#if defined(CERNLIB_VAX)
      IF (ICHAR(METNAM(1:1)).EQ.0) METNAM = 'HIGZ.METAFILE'
#endif
#if defined(CERNLIB_IBM)
      IF (ICHAR(METNAM(1:1)).EQ.0) METNAM = 'HIGZ METAFILE'
#endif
      CALL JFSOPN (3, 0, 0, METNAM)
*
      END
#endif
#include "higz/TITLE.higz"
