*
* $Id: hntget.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hntget.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/14 06/10/94  16.48.31  by  Fons Rademakers
*-- Author :    Fons Rademakers   24/06/92
      SUBROUTINE HNTGET(ID1, TVAR, INDX, ITYPE, ISIZE, IELEM, IER)
*.==========>
*.
*.           Returns for variable VAR in N-tuple ID the array index INDX
*.           in one of the arrays PAWCR8, PAWCR4, PAWC32 depending on the
*.           type ITYPE and the size ISIZE (in bytes) of the variable.
*.           IELEM returns the number of words taken by the variable.
*.           IELEM=1 for a scalar and >1 for an array.
*.           If the variable VAR does not exist then IER<>0.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcntpaw.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*(*) TVAR
      CHARACTER*8   BLOCK
      CHARACTER*32  VAR, NAME
      INTEGER       INDCR4, INDCR8, INDC32
*
      ID    = ID1
      IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF (IDPOS .LE. 0) THEN
         CALL HBUG('Unknown N-tuple','HNTGET',ID1)
         IER = 1
         RETURN
      ENDIF
      LCID  = LQ(LTAB-IDPOS)
      I4 = JBIT(IQ(LCID+KBITS),4)
      IF (I4 .EQ. 0) THEN
         CALL HBUG('Not an N-tuple','HNTGET',ID)
         IER = 1
         RETURN
      ENDIF
      IF (IQ(LCID-2) .NE. ZLINK) THEN
         CALL HBUG('Old N-tuple, cannot use HNTGET','HNTGET',ID)
         IER = 1
         RETURN
      ENDIF
*
      IER    = 0
      VAR    = TVAR
      CALL CLTOU(VAR)
*
      INDCR4 = 1
      INDCR8 = 1
      INDC32 = 1
*
      NDIM = IQ(LCID+ZNDIM)
*
*-- Loop over all variables till we find the one we want.
*
*-- Increment in the meanwhile the INDC.. counters in the same way as we
*-- do in HWPDCL (so there is a correct match between the COMMON
*-- statements in the routine produced by HUWFUN and the actual COMMON blocks
*-- in which the variables are returned
*
      DO 10 I = 1, NDIM
*
         CALL HNTVAR(ID, I, NAME, BLOCK, NSUB, ITYPE, ISIZE, IELEM)
         CALL CLTOU(NAME)
*
         IF (VAR .EQ. NAME) THEN
            IF (ITYPE .NE. 5) THEN
               IF (ISIZE .EQ. 4) THEN
                  CALL HBNAME(ID, BLOCK, RVAR4(INDCR4), '$SET:'//VAR)
                  INDX = INDCR4
               ELSEIF (ISIZE .EQ. 8) THEN
                  CALL HBNAME(ID, BLOCK, RVAR8(INDCR8), '$SET:'//VAR)
                  INDX = INDCR8
               ENDIF
            ELSE
               DO 5 J = 1, IELEM
                  CVAR32(INDC32+J-1) = ' '
   5           CONTINUE
               CALL HBNAMC(ID, BLOCK, CVAR32(INDC32), '!SET:'//VAR)
               INDX = INDC32
            ENDIF
*
            RETURN
*
         ELSE
            IF (ITYPE .NE. 5) THEN
               IF (ISIZE .EQ. 4) THEN
                  INDCR4 = INDCR4 + IELEM
               ELSEIF (ISIZE .EQ. 8) THEN
                  INDCR8 = INDCR8 + IELEM
               ENDIF
            ELSE
               INDC32 = INDC32 + IELEM
            ENDIF
         ENDIF
*
  10  CONTINUE
*
      IER = 2
*
      END
