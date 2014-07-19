*
* $Id: dzddaw.F,v 1.1.1.1 1996/03/04 16:13:10 mclareni Exp $
*
* $Log: dzddaw.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:10  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDDAW(IXSTOR, L, CHOPT, IFIRST, ILAST)
*.
*...DZDDAW   dump data words
*.
*. INPUT     :  IXSTOR  store index
*.              L       link to bank to be dumped
*.              CHOPT   character option as for DZSHOW
*.                      with the following extensions:
*.                      'W' indicates bitwise dump with qualifiers
*.                      '0' bit values 0 are prtd as 0 (blank otherwise)
*.                      '1' or '2' or '3' is the field width
*.                       i.e. '2' selects one space between the bits
*.              IFIRST   first word to dump
*.              ILAST    last word
*. OUTPUT    :
*.
*. CALLS     : DZDBIT DZSHOW UOPTC
*. CALLED    : DZDISP
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
*
      CHARACTER*(*) CHOPT
*
      IF(L .LE. 0)THEN
        GOTO 10
      ENDIF
      IF(INDEX(CHOPT,'W')  .EQ. 0)THEN
            CALL DZSHOW(' DZSHOW called from DZDISP',
     +      IXSTOR, L,CHOPT,1,0,IFIRST,ILAST)
      ELSE
            IF(INDEX(CHOPT,'1') .NE. 0)IFWID = 1
            IF(INDEX(CHOPT,'2') .NE. 0)IFWID = 2
            IF(INDEX(CHOPT,'3') .NE. 0)IFWID = 3
            I0= INDEX(CHOPT,'0')
            CALL DZDBIT(IXSTOR,L,IFIRST,ILAST,IFWID,I0)
      ENDIF
   10 CONTINUE
      END
