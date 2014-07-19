*
* $Id: padvar.F,v 1.1.1.1 1996/03/01 11:38:39 mclareni Exp $
*
* $Log: padvar.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/15 21/07/94  18.26.29  by  Fons Rademakers
*-- Author :    Fons Rademakers   05/11/92
      SUBROUTINE PADVAR(NAME1, IND, IERROR)
*.==========>
*.
*.     Add an Ntuple variable to the list of variables to be retrieved.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "paw/pawcfu.inc"
#include "paw/pntold.inc"
*
      CHARACTER*(*)  NAME1
      CHARACTER*32   NAME, NAMEO, CIVAR
      CHARACTER*8    CHS, BLOCK
      LOGICAL        RECUR, DONE
*
      IERROR = 0
      RECUR  = .FALSE.
      DONE   = .FALSE.
      NAME   = NAME1
      CALL CLTOU(NAME)
*
*-- check if old ntuple
*
      IF (NTOLD) THEN
*
*-- check if variable is already in variable list
*
         DO 5 I = 1, NVART
            IF (NAME .EQ. VAR(I)) THEN
*
*-- if so, simply  return index
*
               IND = I
               RETURN
            ENDIF
   5     CONTINUE
*
         NDIM  = IQ(LCID+2)
         ITAG1 = IQ(LCID+10)
         DO 20 I = 1, NDIM
            CHS = ' '
            CALL UHTOC(IQ(LCID+ITAG1+2*(I-1)),4,CHS,8)
            CALL CLTOU(CHS)
            ISTA = 1
   10       IF(CHS(ISTA:ISTA).EQ.' ')THEN
               ISTA = ISTA+1
               GOTO 10
            ENDIF
            ILAS = LENOCC(CHS)
            IF (ILAS.LT.ISTA) THEN
               ILAS = 1
               ISTA = 1
            ENDIF
            IF (NAME .EQ. CHS(ISTA:ILAS)) THEN
               IF (NVART .LT. MAXCLL) THEN
                  NVART = NVART + 1
                  INDX(NVART)  = I
                  ITYPE(NVART) = 1
                  ISIZE(NVART) = 4
                  IELEM(NVART) = 1
                  VAR(NVART)   = NAME
                  IND = NVART
               ENDIF
               RETURN
            ENDIF
   20    CONTINUE
         IERROR = 1
         RETURN
      ENDIF
*
*-- check if variable is already in variable list
*
   30 DO 40 I = 1, NVART
         IF (NAME .EQ. VAR(I)) THEN
*
*-- if so, simply  return index
*
            IND = I
            IF (RECUR) THEN
               DONE = .TRUE.
               GOTO 50
            ENDIF
            RETURN
         ENDIF
   40 CONTINUE
*
*-- add variable to list, if there is still place
*
   50 IF (NVART .LT. MAXCLL) THEN
*
*-- call hbook routine to find the index of the variable in HCNTPAW
*
         CALL HNTGET(ID, NAME, INDD, IT, IS, IE, IERROR)
         IF (IERROR .NE. 0) RETURN
*
         IF (.NOT.RECUR) THEN
*
*-- see if the variable depends on an index variable, if so place first
*-- the index variable in the list
*
            CALL HVXIST(NAME, BLOCK, CIVAR, IT, IS, IE)
            IF (CIVAR .NE. ' ') THEN
               RECUR = .TRUE.
               NAMEO = NAME
               INDDO = INDD
               NAME  = CIVAR
               CALL CLTOU(NAME)
               GOTO 30
            ENDIF
         ELSEIF (DONE) THEN
            NAME = NAMEO
            INDD = INDDO
            CALL HVXIST(NAME, BLOCK, CIVAR, IT, IS, IE)
            RECUR = .FALSE.
         ENDIF
*
         NVART = NVART + 1
         INDX(NVART)  = INDD
         ITYPE(NVART) = IT
         ISIZE(NVART) = IS
         IELEM(NVART) = IE
         VAR(NVART)   = NAME
         IND = NVART
         IF (RECUR) THEN
            DONE = .TRUE.
            GOTO 50
         ENDIF
      ELSE
         IERROR = -1
      ENDIF
*
      END
