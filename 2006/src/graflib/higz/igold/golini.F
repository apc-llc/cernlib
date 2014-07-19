*
* $Id: golini.F,v 1.1.1.1 1996/02/14 13:10:47 mclareni Exp $
*
* $Log: golini.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:47  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.58.00  by  Unknown
*-- Author : V.Fine
      SUBROUTINE GOLINI(LUN,FILNAM,CHOPT)
*.
*...  GOLINI(LUN,MODE) - OPEN META-FILE ACOORDING 'CHOPT'
*.06/11/91 03:28pm  by Fine V.E. from JINR LCTA fine@main2.jinr.dubna.su
*.
*.   LUN   - LOGICAL UNIT FOR META-FILE OUTPUT
*.   FILNAM  - CHARACTER STRING CONTAINS FILE NAME
*.   CHOPT   = 'ASCII' - ASCII FORMAT OF META-FILE TO EDIT IN THE FUTURE
*.       = 'BINARY'- FASTER AND COMPACT, BUT UNCHANGED.
*.       =  SOME OTHER EQUAL 'BINARY'
*.
#include "higz/golden.inc"
#include "higz/himeta.inc"
      CHARACTER*(*) CHOPT,FILNAM
      CHARACTER HOMEBK*44, HIGZTL*17
      CHARACTER CHAR
      DIMENSION PAR(2)
#include "higz/hivers.inc"

      DATA M_OPEN /.FALSE./
     +       HOMEBK /'102758  Copyright (C) Golden Software 1987. '/
     +       HIGZTL /'Produced by HIGZ '/
*
      BIMODE = .TRUE.
      IF (CHOPT .EQ. 'ASCII') BIMODE = .FALSE.
*
#if !defined(CERNLIB_CIPACK)
      LUNG(1) = LUN
#endif
      IF (BIMODE) THEN
#if (!defined(CERNLIB_XIOPAK))&&(!defined(CERNLIB_CIPACK))
         OPEN (LUNG(1),file=FILNAM(:LENOCC(FILNAM)) , ACCESS='DIRECT',
     +   RECL=LBUF)
#endif
#if defined(CERNLIB_XIOPAK)
         i = ixopen(lung,filnam(:lenocc(filnam)),'w')
#endif
#if defined(CERNLIB_CIPACK)
         call ciopen(lung,'w',filnam(:lenocc(filnam)),i)
         LUN = LUNG(1)
#endif
         NREC = 1
         CALL VFILL(BUFFER,LBUF/4,0)
         IPOINT = 1
*
         LTIT = LENOCC(HOMEBK)+LENOCC(HIGZTL)+LENOCC(CHVERS)
         CALL MPUTBF(HOMEBK//HIGZTL//CHVERS,LTIT)
         IPOINT = 257
      ELSE
         OPEN (LUNG(1),file=FILNAM(:LENOCC(FILNAM)))
      END IF
      FILOPN = .TRUE.
      CALL GOLWRI(SS,0,SYMFIL(:LENOCC(SYMFIL)))
      PAR(1) = 0.0001*SCMETA/2.54
      PAR(2) = 0.0001*SCMETA/2.54
      CALL GOLWRI(SC,PAR,' ')
      FRADEL = .TRUE.
      END
#endif
