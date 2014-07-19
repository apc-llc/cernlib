*
* $Id: pfindc.F,v 1.1.1.1 1996/03/01 11:38:45 mclareni Exp $
*
* $Log: pfindc.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:45  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/15 05/03/94  19.32.09  by  Fons Rademakers
*-- Author :    Fons Rademakers   24/01/94
      SUBROUTINE PFINDC(CHFILE, IERROR)
*.==========>
*.
*.     Scan the C file for the occurance of Ntuple variables.
*.     Add all found variables to the VAR list.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
*
      PARAMETER    (NDECL=21)
      CHARACTER*(*) CHFILE
      CHARACTER*80  KLINE
      CHARACTER*32  NAME
      CHARACTER*8   BLOCK
      CHARACTER*23  DECL(NDECL)
      INTEGER       LDECL(NDECL)
      LOGICAL       PLPHCH, PNUMCH
*
      DATA DECL/'PROGRAM','SUBROUTINE','FUNCTION','LOGICALFUNCTION',
     +     'CHARACTERFUNCTION','INTEGERFUNCTION','REALFUNCTION',
     +     'COMPLEXFUNCTION','DOUBLEPRECISIONFUNCTION','BLOCKDATA',
     +     'DOUBLECOMPLEXFUNCTION','OPTION','COMMON','INTEGER',
     +     'REAL','LOGICAL','COMPLEX','CHARACTER','DOUBLEPRECISION',
     +     'SAVE','DATA'/
      DATA LDECL /7,10,8,15,17,15,12,15,23,9,21,6,6,7,4,7,7,9,15,4,4/
*
      IDECL  = 0
      IERROR = 0
*--                   2 = ZNDIM
      NDIM  = IQ(LCID+2)
*
*-- open the file
*
      CALL PALUNF(60,3,LUN)
      IF (LUN.EQ.0) GOTO 99
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
      CALL CUTOL(CHFILE)
#endif
*
      CALL KUOPEN(LUN,CHFILE,'OLD',ISTAT)
      IF (ISTAT.NE.0) GOTO 99
*
*-- read line from file
*
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
  10  KLINE=' '
      READ(LUN,NUM=NBYT,END=40,ERR=99) KLINE
#endif
#if !defined(CERNLIB_IBM)||defined(CERNLIB_IBMMVS)||defined(CERNLIB_NEWLIB)
 10   READ(LUN,'(A)',END=40,ERR=99) KLINE
#endif
*
*-- use Comis function MCLINE to convert the line to uppercase, to kill
*-- the tabs and to return the length of the string
*
**    CALL CLTOU(KLINE)
**    LK = LENOCC(KLINE)
      LK = MCLINE(KLINE)
*
*-- skip comments and continuation lines of declarations INTEGER, REAL,
*-- DOUBLE, COMPLEX, LOGICAL, CHARACTER, COMMON, FUNCTION, SUBROUTINE
*
      IF (KLINE .EQ. ' ')                           GOTO 10
      IF (KLINE(1:1).EQ.'*' .OR. KLINE(1:1).EQ.'C') GOTO 10
      IF (IDECL.NE.0 .AND. KLINE(6:6).NE.' ')       GOTO 10
*
*-- remove all blanks from the line
*
      CALL HCOMPR(KLINE,LK)
*
*-- skip also declaration statements
*
      IDECL = 0
      DO 20 I = 1, NDECL
         IF (KLINE(1:LDECL(I)) .EQ. DECL(I)) THEN
            IDECL = 1
            GOTO 10
         ENDIF
  20  CONTINUE
*
*-- scan line for occurance of Ntuple variables (loop over all variables)
*
      DO 30 I = 1, NDIM
*
         CALL HNTVAR(ID, I, NAME, BLOCK, NSUB, ITYPE, ISIZE, IELEM)
         CALL CLTOU(NAME)
         LN = LENOCC(NAME)
*
*-- search for variable, if found check that it is not preceded by another
*-- character and that it is not trailed by an alphanumeric character
*-- (for the time being do not check if variable is between quotes)
*
         II = 1
  24     IND = INDEX(KLINE(II:),NAME(1:LN))
         IF (IND .NE. 0) THEN
            IND = IND + II - 1
            II  = IND + 1
            IF (IND.GT.1) THEN
               IF (PLPHCH(KLINE(IND-1:IND-1))) THEN
*
*-- special cases where keywords are not separated from variables
*-- by a numeric or non-alphanumeric character: DO and TYPE
*
                  IF (IND.GT.4) THEN
                     IF (KLINE(IND-4:IND-1) .EQ. 'TYPE') GOTO 25
                  ENDIF
                  IF (IND.GT.2) THEN
                     IF (KLINE(IND-2:IND-1) .EQ. 'DO')   GOTO 25
                  ENDIF
                  IF (II .LE. LK) GOTO 24
               ENDIF
            ENDIF
  25        IF (IND+LN .LE. LK) THEN
               IF (PNUMCH(KLINE(IND+LN:IND+LN))) GOTO 24
            ENDIF
*
*-- accept variable
*
            CALL PADVAR(NAME(1:LN), IDUM, IERROR)
            IF (IERROR .NE. 0) GOTO 99
         ENDIF
*
  30  CONTINUE
*
*-- next line
*
      GOTO 10
*
*-- end of file
*
  40  CALL PACLOS(LUN)
      RETURN
*
  99  IERROR = 1
*
      END
