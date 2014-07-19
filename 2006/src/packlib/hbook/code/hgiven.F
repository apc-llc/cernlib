*
* $Id: hgiven.F,v 1.1.1.1 1996/01/16 17:07:38 mclareni Exp $
*
* $Log: hgiven.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:38  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/03 26/05/94  11.47.32  by  Rene Brun
*-- Author :
      SUBROUTINE HGIVEN( ID1, CHTITL, NVAR, TAGS, RLOW, RHIGH )
*.==========>
*.           To give information about a N-tuple. On entry, NVAR
*.           must specify the dimension of the TAGS, RLOW and RHIGH
*.           arrays. On return, NVAR is overwritten by the actual
*.           number of dimensions. If ID1 does not exist or is not
*.           an N-tuple, a value of 0 is returned
*.
*..=========> ( R. Namjoshi )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcntpar.inc"
*
      CHARACTER*(*) CHTITL, TAGS(*)
      INTEGER       ID1, NVAR
      REAL          RLOW(*), RHIGH(*)
      CHARACTER*8   BLOCK
      LOGICAL       NTOLD
*.___________________________________________
*
      NMAX = NVAR
      NVAR = 0
*
      ID     = ID1
      IDPOS  = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF( IDPOS.LE.0 ) RETURN
      IDLAST = ID1
      LCID   = LQ(LTAB-IDPOS)
      I4     = JBIT(IQ(LCID+KBITS),4)
      IF( I4.EQ.0 ) RETURN
*
      IF (IQ(LCID-2) .NE. ZLINK) THEN
         NTOLD = .TRUE.
      ELSE
         NTOLD = .FALSE.
      ENDIF
*
      IF (NTOLD) THEN
         NDIM  = IQ(LCID+2)
         LLIMS = LQ(LCID-2)
         ITAG1 = IQ(LCID+10)
         ITIT1 = IQ(LCID+9)
         NWTIT = IQ(LCID+8)
      ELSE
         NDIM  = IQ(LCID+ZNDIM)
         ITIT1 = IQ(LCID+ZITIT1)
         NWTIT = IQ(LCID+ZNWTIT)
      ENDIF
*
      NVAR = MIN(NDIM, NMAX)
*
* Copy title
*
      NCH = LEN(CHTITL)
      IF (NCH .GT. 0) CHTITL = ' '
      NCH = MIN( NCH, 4*NWTIT )
      IF (NCH .GT. 0) CALL UHTOC( IQ(LCID+ITIT1), 4, CHTITL, NCH )
*
* Copy tag names and current lower/upper range
*
      IF (NTOLD) THEN
         NCH = LEN( TAGS(1) )
         NCH = MIN( NCH, 8 )
         DO 10 I = 1, NVAR
            IF( NCH.GT.0 ) TAGS(I) = ' '
            IF( NCH.GT.0 )THEN
               TAGS(I)=' '
               CALL UHTOC( IQ(LCID+ITAG1+2*(I-1)), 4, TAGS(I), NCH )
            ENDIF
            RLOW (I) = Q(LLIMS+2*I-1)
            RHIGH(I) = Q(LLIMS+2*I)
  10     CONTINUE
      ELSE
         DO 20 I = 1, NVAR
            CALL HNTVAR(ID1, I, TAGS(I), BLOCK, NS, IT, IS, IE)
            RLOW(I)  = 0.0
            RHIGH(I) = 0.0
  20     CONTINUE
      ENDIF
*
      NVAR = NDIM
*
      END
