*
* $Id: hnttok.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnttok.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 26/02/92  18.17.24  by  Fons Rademakers
*-- Author :    Fons Rademakers   17/12/91
      SUBROUTINE HNTTOK(FORM, TOK, NTOK, MAXTOK, IERR)
*.==========>
*.
*.           Find the different tokens in the HBNAME format.
*.
*..=========> ( A.A.Rademakers )
*
      CHARACTER*(*) FORM, TOK(*)
      INTEGER       NTOK, IERR
      LOGICAL       BRACK, PAREN
*
      NTOK  = 0
      IERR  = 0
      BRACK = .FALSE.
      PAREN = .FALSE.
*
      LF    = LENOCC(FORM)
      IF (LF .EQ. 0) RETURN
      CALL HCOMPR(FORM, LF)
*
      I1 = 1
      DO 10 I = 1, LF
         IF (FORM(I:I) .EQ. '[') BRACK = .TRUE.
         IF (FORM(I:I) .EQ. ']') BRACK = .FALSE.
         IF (FORM(I:I) .EQ. '(') PAREN = .TRUE.
         IF (FORM(I:I) .EQ. ')') PAREN = .FALSE.
         IF (FORM(I:I).EQ.',' .AND. .NOT.BRACK .AND. .NOT.PAREN) THEN
            NTOK = NTOK + 1
            IF (NTOK .GT. MAXTOK) THEN
               IERR = 2
               RETURN
            ENDIF
            TOK(NTOK) = FORM(I1:I-1)
            I1 = I + 1
         ENDIF
         IF (I .EQ. LF) THEN
            IF (BRACK .OR. PAREN) THEN
               IERR = 1
            ELSE
               NTOK = NTOK + 1
               IF (NTOK .GT. MAXTOK) THEN
                  IERR = 2
                  RETURN
               ENDIF
               TOK(NTOK) = FORM(I1:LF)
            ENDIF
         ENDIF
10    CONTINUE
*
      END
