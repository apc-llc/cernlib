*
* $Id: hwpdcl.F,v 1.3 1999/03/23 14:39:52 couet Exp $
*
* $Log: hwpdcl.F,v $
* Revision 1.3  1999/03/23 14:39:52  couet
* - previous commit was wrong ...
*
* Revision 1.2  1999/03/23 14:38:04  couet
* - ?
*
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/04 30/05/94  22.34.21  by  Rene Brun
*-- Author :    Fons Rademakers   28/01/92
      SUBROUTINE HWPDCL(LUN, ITRUNC)
*.==========>
*.
*.           Write integer, real, logical, character declarations for
*.           the PAW user function. Truncate variable names
*.           to ITRUNC characters.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      PARAMETER   (MAXCON = 19, MAXCMN = 50)
      CHARACTER*80 DECLR(MAXCON),DECLR8(MAXCON),DECLI(MAXCON)
      CHARACTER*80 DECLL(MAXCON),DECLC(MAXCON)
      CHARACTER*80 CMNR4(MAXCMN), CMNR8(MAXCMN), CMNC32(MAXCMN), VAR
      CHARACTER*32 NAME, SUBS
      CHARACTER*8  BLKNAM
      LOGICAL      INITR, INITR8, INITI, INITL, INITC,
     +             INICM4, INICM8, INIC32, LDUM
*
      INITR  = .TRUE.
      INITR8 = .TRUE.
      INITI  = .TRUE.
      INITL  = .TRUE.
      INITC  = .TRUE.
      INICM4 = .TRUE.
      INICM8 = .TRUE.
      INIC32 = .TRUE.
      IFDR   = 0
      IFDR8  = 0
      IFDI   = 0
      IFDL   = 0
      IFDC   = 0
      IFCM4  = 0
      IFCM8  = 0
      IFCM32 = 0
*
      LBLOK = LQ(LCID-1)
*
*-- loop over all blocks
*
  5   LNAME = LQ(LBLOK-1)
*
      IOFF = 0
      NDIM = IQ(LBLOK+ZNDIM)
      CALL UHTOC(IQ(LBLOK+ZIBLOK), 4, BLKNAM, 8)
*
      DO 10 I = 1, NDIM
*
         IF (INITR) THEN
            IF (IFDR .NE. 0) WRITE(LUN,1000)
     +          (DECLR(II)(1:LENOCC(DECLR(II))), II = 1, ILDR)
            IFDR = 0
            ILDR = 1
            DECLR(ILDR) = '      REAL'
            LPDR = LENOCC(DECLR(ILDR)) + 1
            INITR = .FALSE.
         ENDIF
*
         IF (INITR8) THEN
            IF (IFDR8.NE. 0) WRITE(LUN,1000)
     +          (DECLR8(II)(1:LENOCC(DECLR8(II))), II = 1, ILDR8)
            IFDR8 = 0
            ILDR8 = 1
            DECLR8(ILDR8) = '      DOUBLE PRECISION'
            LPDR8 = LENOCC(DECLR8(ILDR8)) + 1
            INITR8 = .FALSE.
         ENDIF
*
         IF (INITI) THEN
            IF (IFDI .NE. 0) WRITE(LUN,1000)
     +          (DECLI(II)(1:LENOCC(DECLI(II))), II = 1, ILDI)
            IFDI = 0
            ILDI = 1
            DECLI(ILDI) = '      INTEGER'
            LPDI = LENOCC(DECLI(ILDI)) + 1
            INITI = .FALSE.
         ENDIF
*
         IF (INITL) THEN
            IF (IFDL .NE. 0) WRITE(LUN,1000)
     +          (DECLL(II)(1:LENOCC(DECLL(II))), II = 1, ILDL)
            IFDL = 0
            ILDL = 1
            DECLL(ILDL) = '      LOGICAL'
            LPDL = LENOCC(DECLL(ILDL)) + 1
            INITL = .FALSE.
         ENDIF
*
         IF (INITC) THEN
            IF (IFDC .NE. 0) WRITE(LUN,1000)
     +          (DECLC(II)(1:LENOCC(DECLC(II))), II = 1, ILDC)
            IFDC = 0
            ILDC = 1
            DECLC(ILDC) = '      CHARACTER*32'
            LPDC = LENOCC(DECLC(ILDC)) + 1
            INITC = .FALSE.
         ENDIF
*
         IF (INICM4) THEN
            IF (IFCM4 .NE. 0) THEN
               PRINT *,' HUWFUN: Not space to store COMMON definition'
               GOTO 30
            ENDIF
            IFCM4 = 0
            ILCM4 = 1
            CMNR4(ILCM4) = '      COMMON /PAWCR4/'
            LPCM4 = LENOCC(CMNR4(ILCM4)) + 1
            INICM4 = .FALSE.
         ENDIF
*
         IF (INICM8) THEN
            IF (IFCM8 .NE. 0) THEN
               PRINT *,' HUWFUN: Not space to store COMMON definition'
               GOTO 30
            ENDIF
            IFCM8 = 0
            ILCM8 = 1
            CMNR8(ILCM8) = '      COMMON /PAWCR8/'
            LPCM8 = LENOCC(CMNR8(ILCM8)) + 1
            INICM8 = .FALSE.
         ENDIF
*
         IF (INIC32) THEN
            IF (IFCM32 .NE. 0) THEN
               PRINT *,' HUWFUN: Not space to store COMMON definition'
               GOTO 30
            ENDIF
            IFCM32 = 0
            ILCM32 = 1
            CMNC32(ILDC) = '      COMMON /PAWC32/'
            LPCM32 = LENOCC(CMNC32(ILCM32)) + 1
            INIC32 = .FALSE.
         ENDIF
*
         CALL HNDESC(IOFF, NSUB, ITYPE, ISIZE, NBITS, LDUM)
         LL = IQ(LNAME+IOFF+ZLNAME)
         LV = IQ(LNAME+IOFF+ZNAME)
         NAME = ' '
         CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
         LL = MIN(ITRUNC,LL)
         IF (NSUB .GT. 0) THEN
            VAR = NAME(1:LL)//'('
            DO 20 J = 1, NSUB
               LP = IQ(LINT+IQ(LNAME+IOFF+ZARIND)+(J-1))
               IF (LP .LT. 0) THEN
                  IE = -LP
                  CALL HITOC(IE, SUBS, LL, IERR)
               ELSE
                  LL = IQ(LNAME+LP-1+ZRANGE)
                  IMAX = IQ(LINT+LL+1)
                  CALL HITOC(IMAX, SUBS, LL, IERR)
               ENDIF
               IF (J .EQ. 1) THEN
                  VAR = VAR(1:LENOCC(VAR))//SUBS(1:LL)
               ELSE
                  VAR = VAR(1:LENOCC(VAR))//','//SUBS(1:LL)
               ENDIF
  20        CONTINUE
            VAR = VAR(1:LENOCC(VAR))//')'
         ELSE
            VAR = NAME(1:LL)
         ENDIF
*
         LL = MIN(ITRUNC,LENOCC(NAME))
         LV = LENOCC(VAR)
*
*-- construct declaration statements
*
         IF (ITYPE .EQ. 1) THEN
            IF (ISIZE .EQ. 4) THEN
               IF (LPDR+LV+1 .GT. 72) THEN
                  ILDR = ILDR + 1
                  DECLR(ILDR) = '     + ,'//VAR(1:LV)
               ELSE
                  IF (IFDR .EQ. 0) THEN
                     DECLR(ILDR) = DECLR(ILDR)(1:LPDR)//VAR(1:LV)
                  ELSE
                     DECLR(ILDR) = DECLR(ILDR)(1:LPDR)//','//VAR(1:LV)
                  ENDIF
                  IFDR = 1
               ENDIF
               LPDR = LENOCC(DECLR(ILDR))
               IF (ILDR .EQ. MAXCON) INITR = .TRUE.
            ELSEIF (ISIZE .EQ. 8) THEN
               IF (LPDR8+LV+1 .GT. 72) THEN
                  ILDR8 = ILDR8 + 1
                  DECLR8(ILDR8) = '     + ,'//VAR(1:LV)
               ELSE
                  IF (IFDR8 .EQ. 0) THEN
                     DECLR8(ILDR8) = DECLR8(ILDR8)(1:LPDR8)//VAR(1:LV)
                  ELSE
                     DECLR8(ILDR8) = DECLR8(ILDR8)(1:LPDR8)//','//
     +                               VAR(1:LV)
                  ENDIF
                  IFDR8 = 1
               ENDIF
               LPDR8 = LENOCC(DECLR8(ILDR8))
               IF (ILDR8 .EQ. MAXCON) INITR8 = .TRUE.
            ENDIF
         ELSEIF (ITYPE .EQ. 2 .OR. ITYPE .EQ. 3) THEN
            IF (LPDI+LV+1 .GT. 72) THEN
               ILDI = ILDI + 1
               DECLI(ILDI) = '     + ,'//VAR(1:LV)
            ELSE
               IF (IFDI .EQ. 0) THEN
                  DECLI(ILDI) = DECLI(ILDI)(1:LPDI)//VAR(1:LV)
               ELSE
                  DECLI(ILDI) = DECLI(ILDI)(1:LPDI)//','//VAR(1:LV)
               ENDIF
               IFDI = 1
            ENDIF
            LPDI = LENOCC(DECLI(ILDI))
            IF (ILDI .EQ. MAXCON) INITI = .TRUE.
         ELSEIF (ITYPE .EQ. 4) THEN
            IF (LPDL+LV+1 .GT. 72) THEN
               ILDL = ILDL + 1
               DECLL(ILDL) = '     + ,'//VAR(1:LV)
            ELSE
               IF (IFDL .EQ. 0) THEN
                  DECLL(ILDL) = DECLL(ILDL)(1:LPDL)//VAR(1:LV)
               ELSE
                  DECLL(ILDL) = DECLL(ILDL)(1:LPDL)//','//VAR(1:LV)
               ENDIF
               IFDL = 1
            ENDIF
            LPDL = LENOCC(DECLL(ILDL))
            IF (ILDL .EQ. MAXCON) INITL = .TRUE.
         ELSEIF (ITYPE .EQ. 5) THEN
            IF (LPDC+LV+1 .GT. 72) THEN
               ILDC = ILDC + 1
               DECLC(ILDC) = '     + ,'//VAR(1:LV)
            ELSE
               IF (IFDC .EQ. 0) THEN
                  DECLC(ILDC) = DECLC(ILDC)(1:LPDC)//VAR(1:LV)
               ELSE
                  DECLC(ILDC) = DECLC(ILDC)(1:LPDC)//','//VAR(1:LV)
               ENDIF
               IFDC = 1
            ENDIF
            LPDC = LENOCC(DECLC(ILDC))
            IF (ILDC .EQ. MAXCON) INITC = .TRUE.
         ENDIF
*
*-- construct COMMON statements
*
         IF (ITYPE .NE. 5) THEN
            IF (ISIZE .EQ. 4) THEN
               IF (LPCM4+LL+1 .GT. 72) THEN
                  ILCM4 = ILCM4 + 1
                  CMNR4(ILCM4) = '     + ,'//NAME(1:LL)
               ELSE
                  IF (IFCM4 .EQ. 0) THEN
                     CMNR4(ILCM4) = CMNR4(ILCM4)(1:LPCM4)//NAME(1:LL)
                  ELSE
                     CMNR4(ILCM4) = CMNR4(ILCM4)(1:LPCM4)//','//
     +                              NAME(1:LL)
                  ENDIF
                  IFCM4 = 1
               ENDIF
               LPCM4 = LENOCC(CMNR4(ILCM4))
               IF (ILCM4 .EQ. MAXCMN) INICM4 = .TRUE.
            ELSEIF (ISIZE .EQ. 8) THEN
               IF (LPCM8+LL+1 .GT. 72) THEN
                  ILCM8 = ILCM8 + 1
                  CMNR8(ILCM8) = '     + ,'//NAME(1:LL)
               ELSE
                  IF (IFCM8 .EQ. 0) THEN
                     CMNR8(ILCM8) = CMNR8(ILCM8)(1:LPCM8)//NAME(1:LL)
                  ELSE
                     CMNR8(ILCM8) = CMNR8(ILCM8)(1:LPCM8)//','//
     +                              NAME(1:LL)
                  ENDIF
                  IFCM8 = 1
               ENDIF
               LPCM8 = LENOCC(CMNR8(ILCM8))
               IF (ILCM8 .EQ. MAXCMN) INICM8 = .TRUE.
            ENDIF
         ELSE
            IF (LPCM32+LL+1 .GT. 72) THEN
               ILCM32 = ILCM32 + 1
               CMNC32(ILCM32) = '     + ,'//NAME(1:LL)
            ELSE
               IF (IFCM32 .EQ. 0) THEN
                  CMNC32(ILCM32) = CMNC32(ILCM32)(1:LPCM32)//NAME(1:LL)
               ELSE
                  CMNC32(ILCM32) = CMNC32(ILCM32)(1:LPCM32)//','//
     +                           NAME(1:LL)
               ENDIF
               IFCM32 = 1
            ENDIF
            LPCM32 = LENOCC(CMNC32(ILCM32))
            IF (ILCM32 .EQ. MAXCMN) INIC32 = .TRUE.
         ENDIF
*
         IOFF = IOFF + ZNADDR
  10  CONTINUE
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) GOTO 5
*
  30  IF (IFDR .NE. 0) WRITE(LUN,1000) (DECLR(I)(1:LENOCC(DECLR(I))),
     +                 I = 1, ILDR)
      IF (IFDR8.NE. 0) WRITE(LUN,1000) (DECLR8(I)(1:LENOCC(DECLR8(I))),
     +                 I = 1, ILDR8)
      IF (IFDI .NE. 0) WRITE(LUN,1000) (DECLI(I)(1:LENOCC(DECLI(I))),
     +                 I = 1, ILDI)
      IF (IFDL .NE. 0) WRITE(LUN,1000) (DECLL(I)(1:LENOCC(DECLL(I))),
     +                 I = 1, ILDL)
      IF (IFDC .NE. 0) WRITE(LUN,1000) (DECLC(I)(1:LENOCC(DECLC(I))),
     +                 I = 1, ILDC)
      WRITE(LUN,1000) '*'
      IF (IFCM4 .NE. 0) WRITE(LUN,1000)(CMNR4(I)(1:LENOCC(CMNR4(I))),
     +                 I = 1, ILCM4)
      IF (IFCM8 .NE. 0) WRITE(LUN,1000)(CMNR8(I)(1:LENOCC(CMNR8(I))),
     +                 I = 1, ILCM8)
      IF (IFCM32 .NE. 0)WRITE(LUN,1000)(CMNC32(I)(1:LENOCC(CMNC32(I))),
     +                 I = 1, ILCM32)
*
 1000 FORMAT(A)
*
      END
 
