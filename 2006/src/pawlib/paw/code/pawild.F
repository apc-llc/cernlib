*
* $Id: pawild.F,v 1.3 1996/09/13 10:08:55 couet Exp $
*
* $Log: pawild.F,v $
* Revision 1.3  1996/09/13 10:08:55  couet
* - Old qp removal continue ...
*
* Revision 1.2  1996/09/10 13:37:28  couet
* - NT/MERGE is obsolete (use HMERGE instead)
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
       SUBROUTINE PAWILD(CFIRST,CSECON,ILFIRS,IRESUL)

*****************************************************************************
*                                                                           *
*      subroutine PAWILD called by qp_execute.                              *
*      Used to test if two strings are equal.  The fist sting may have wild *
*      characters.  ? rep any one character and * to rep any number of      *
*      characters (incl none).                                              *
*      Passed in : CFIRST  CHARACTER*32 input string (may have wild chars)  *
*                : CSECON  CHARACTER*32 input string                        *
*                : ILFIRS  INTEGER  length of first string                  *
*                : ILSECO  INTEGER  length of second string                 *
*      Passed out: IRESUL  INTEGER  1 if the strings are equal 0 if not     *
*                                                                           *
*****************************************************************************


       CHARACTER*32 CFIRST
       CHARACTER*32 CSECON
       CHARACTER    CONEWD
       CHARACTER    CMANWD
       INTEGER      ILFIRS
       INTEGER      ILSECO, IWLFLG
       INTEGER      IPOSFI
       INTEGER      IPOSSC
       INTEGER      JINONE
       INTEGER      JINAMN
       INTEGER      IRESUL
       INTEGER      IPREVF
       INTEGER      IPREVS
*
       SAVE IWLFLG,IPREVF,IPREVS
       DATA IWLFLG,IPREVF,IPREVS/0,0,0/
*
       call cltou(cfirst)
       call cltou(csecon)
*
C      define the wild characters. ? is one characters and * can be
C      any number of characters.
       CONEWD = '?'
       CMANWD = '*'

       IPOSFI = 1
       IPOSSC = 1
       IRESUL = 1
       ILSECO = LENOCC (CSECON)

C      find the true length of CSECON as it may have spaces
 5     IF (CSECON(ILSECO:ILSECO).EQ.' ') THEN
          ILSECO = ILSECO - 1
          GO TO 5
       END IF

C      find the position of wild characters (if any)
       JINONE = INDEX (CFIRST,CONEWD)
       JINAMN = INDEX (CFIRST,CMANWD)

C      if there are no wild characters in the first string then
       IF ((JINONE.EQ.0).AND.(JINAMN.EQ.0)) THEN

C         see if the two strings are equal
          IF (CFIRST(1:ILFIRS).EQ.CSECON(1:ILSECO)
     +       .OR.CFIRST(1:ILFIRS).EQ.CMANWD) THEN
             IRESUL = 1
           IF ((CFIRST(1:ILFIRS).EQ.CMANWD).AND.
     +        (CFIRST(1:ILFIRS+1).EQ.CMANWD)) THEN
             ILFIRS = ILFIRS-1
           END IF
          ELSE
             IRESUL = 0
          END IF

       ELSE
C      there is one or more wild characters

C      if the present character is not equal to a wild character then
 10       IF ((CFIRST(IPOSFI:IPOSFI).NE.CONEWD).AND.
     +      (CFIRST(IPOSFI:IPOSFI).NE.CMANWD)) THEN

C            if the present charactre of first and second string are equal
             IF ((CFIRST(IPOSFI:IPOSFI).EQ.CSECON(IPOSSC:IPOSSC))
     +          .AND.(IPOSFI.LE.ILFIRS)) THEN
C               the strings are equal so far
                IRESUL = 1
             ELSE IF (IWLFLG.EQ.1) THEN
                IPOSFI = IPREVF
                IPREVS = IPREVS + 1
                IPOSSC = IPREVS
                IF (IPOSSC.GT.ILSECO) THEN
                   IRESUL = 0
                ELSE
                   GO TO 10
                END IF
             ELSE
                IRESUL = 0
             END IF
C         if the character is a ? wild character the
          ELSE IF (CFIRST(IPOSFI:IPOSFI).EQ.CONEWD) THEN
C            string are equal so far
             IRESUL = 1
C         else we have a * wild character
          ELSE
             IWLFLG = 1
C            if the * wild character is the last character of the first string
C            then
             IF ((IPOSFI.EQ.ILFIRS).OR.
     +          (CFIRST((IPOSFI+1):(IPOSFI+1)).EQ.CMANWD)) THEN
C               both string are equal so set result to 1 and end loop
                IRESUL = 1
                IPOSSC = ILSECO
             ELSE
                IPREVF = IPOSFI + 1
                IPREVS = IPOSSC
                IPOSSC = IPOSSC - 1
 15             IF (CFIRST((IPOSFI+1):(IPOSFI+1)).EQ.CMANWD) THEN
                   IPOSFI = IPOSFI+1
                   GO TO 15

                END IF
             END IF
          END IF
          IF ((IPOSSC.NE.ILSECO).AND.(IPOSFI.NE.ILFIRS).AND.
     +       (IRESUL.NE.0)) THEN
             IPOSSC = IPOSSC + 1
             IPOSFI = IPOSFI + 1
             GO TO 10
          END IF
C      if both character pointers are not equal to the strings lengths then
C      the strings are not equal
       IF (((CFIRST(IPOSFI:IPOSFI).EQ.CMANWD).AND.
     +    (CFIRST(IPOSFI+1:IPOSFI+1).EQ.CMANWD).AND.
     +    (IPOSSC.EQ.ILSECO).AND.(IPOSFI.EQ.ILFIRS))) THEN
       IPOSFI = ILFIRS
       IPOSSC = ILSECO
       END IF
       IF (CFIRST(IPOSFI+1:IPOSFI+1).EQ.CMANWD) THEN
       IPOSFI = ILFIRS
       END IF
       IF ((IPOSSC.NE.ILSECO).OR.(IPOSFI.NE.ILFIRS)) IRESUL = 0

       END IF

 90    END
