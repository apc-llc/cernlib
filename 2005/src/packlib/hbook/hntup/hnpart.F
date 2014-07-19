*
* $Id: hnpart.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnpart.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 27/02/92  14.44.30  by  Fons Rademakers
*-- Author :    Fons Rademakers   17/12/91
      SUBROUTINE HNPART(DESC, PART, NPART, MAXPAR, IERR)
*.==========>
*.
*.           Decode a variable description in its individual parts.
*.           There can be maximal 4 parts for reals and (un)signed integers
*.           IVAR:TYPE*SIZE:BITS:[range]
*.           and 2 parts for logicals and characters:
*.           CVAR:TYPE*SIZE
*.
*..=========> ( A.A.Rademakers )
*
      CHARACTER*(*)  DESC, PART(*)
      INTEGER        NPART, MAXPAR, IERR
      CHARACTER*1    DELIM
      PARAMETER     (DELIM = ':')
      LOGICAL        PAREN
*
      IERR  = 0
      NPART = 0
      PAREN = .FALSE.
*
      LD = LENOCC(DESC)
*
      I1 = 1
      DO 10 I = 1, LD
         IF (DESC(I:I) .EQ. '(') PAREN = .TRUE.
         IF (DESC(I:I) .EQ. ')') PAREN = .FALSE.
         IF (DESC(I:I) .EQ. DELIM) THEN
            IF (PAREN) THEN
               IERR = 1
               RETURN
            ELSE
               NPART = NPART + 1
               IF (NPART .GT. MAXPAR) THEN
                  IERR = 2
                  RETURN
               ENDIF
               IF (I1 .EQ. I) THEN
                  PART(NPART) = ' '
               ELSE
                  PART(NPART) = DESC(I1:I-1)
               ENDIF
               IF (I .EQ. LD) GOTO 10
               I1 = I + 1
            ENDIF
         ENDIF
         IF (I .EQ. LD) THEN
            IF (PAREN) THEN
               IERR = 1
               RETURN
            ELSE
               NPART = NPART + 1
               IF (NPART .GT. MAXPAR) THEN
                  IERR = 2
                  RETURN
               ENDIF
               PART(NPART) = DESC(I1:LD)
            ENDIF
         ENDIF
10    CONTINUE
*
      END
