*
* $Id: hwdecl.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hwdecl.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/08 07/09/93  19.33.43  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/01/92
      SUBROUTINE HWDECL(LUN, ITRUNC)
*.==========>
*.
*.           Write integer/real common block definition for the user
*.           function. Truncate common block names and variable names
*.           to ITRUNC characters.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*80 DECLR(19),DECLR8(19),DECLI(19),DECLL(19),DECLC(19)
      CHARACTER*80 CMN(19), CMNC(19), VAR
      CHARACTER*32 NAME, SUBS
      CHARACTER*8  BLKNAM
      CHARACTER*2  SIZE
      LOGICAL      LDUM
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
      IFCMN      = 0
      ILCMN      = 1
      CMN(ILCMN) = '      COMMON /'//
     +             BLKNAM(1:MIN(ITRUNC,LENOCC(BLKNAM)))//'/'
      LPCMN = LENOCC(CMN(ILCMN)) + 1
*
      IFCMNC = 0
      ILCMNC = 1
      CMNC(ILCMNC) = '      COMMON /'//
     +                BLKNAM(1:MIN(ITRUNC-1,LENOCC(BLKNAM)))//'1/'
      LPCMNC = LENOCC(CMNC(ILCMNC)) + 1
*
      IFDR = 0
      ILDR = 1
      DECLR(ILDR) = '      REAL'
      LPDR = LENOCC(DECLR(ILDR)) + 1
*
      IFDR8 = 0
      ILDR8 = 1
      DECLR8(ILDR8) = '      REAL*8'
      LPDR8 = LENOCC(DECLR8(ILDR8)) + 1
*
      IFDI = 0
      ILDI = 1
      DECLI(ILDI) = '      INTEGER'
      LPDI = LENOCC(DECLI(ILDI)) + 1
*
      IFDL = 0
      ILDL = 1
      DECLL(ILDL) = '      LOGICAL'
      LPDL = LENOCC(DECLL(ILDL)) + 1
*
      IFDC = 0
      ILDC = 1
      DECLC(ILDC) = '      CHARACTER'
      LPDC = LENOCC(DECLC(ILDC)) + 1
*
      DO 10 I = 1, NDIM
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
               IF (LPDR+LL+1 .GT. 72) THEN
                  ILDR = ILDR + 1
                  DECLR(ILDR) = '     + ,'//NAME(1:LL)
               ELSE
                  IF (IFDR .EQ. 0) THEN
                     DECLR(ILDR) = DECLR(ILDR)(1:LPDR)//NAME(1:LL)
                  ELSE
                     DECLR(ILDR) = DECLR(ILDR)(1:LPDR)//','//NAME(1:LL)
                  ENDIF
                  IFDR = 1
               ENDIF
               LPDR = LENOCC(DECLR(ILDR))
            ELSEIF (ISIZE .EQ. 8) THEN
               IF (LPDR8+LL+1 .GT. 72) THEN
                  ILDR8 = ILDR8 + 1
                  DECLR8(ILDR8) = '     + ,'//NAME(1:LL)
               ELSE
                  IF (IFDR8 .EQ. 0) THEN
                     DECLR8(ILDR8) = DECLR8(ILDR8)(1:LPDR8)//NAME(1:LL)
                  ELSE
                     DECLR8(ILDR8) = DECLR8(ILDR8)(1:LPDR8)//','//
     +                               NAME(1:LL)
                  ENDIF
                  IFDR8 = 1
               ENDIF
               LPDR8 = LENOCC(DECLR8(ILDR8))
            ENDIF
         ELSEIF (ITYPE .EQ. 2 .OR. ITYPE .EQ. 3) THEN
            IF (LPDI+LL+1 .GT. 72) THEN
               ILDI = ILDI + 1
               DECLI(ILDI) = '     + ,'//NAME(1:LL)
            ELSE
               IF (IFDI .EQ. 0) THEN
                  DECLI(ILDI) = DECLI(ILDI)(1:LPDI)//NAME(1:LL)
               ELSE
                  DECLI(ILDI) = DECLI(ILDI)(1:LPDI)//','//NAME(1:LL)
               ENDIF
               IFDI = 1
            ENDIF
            LPDI = LENOCC(DECLI(ILDI))
         ELSEIF (ITYPE .EQ. 4) THEN
            IF (LPDL+LL+1 .GT. 72) THEN
               ILDL = ILDL + 1
               DECLL(ILDL) = '     + ,'//NAME(1:LL)
            ELSE
               IF (IFDL .EQ. 0) THEN
                  DECLL(ILDL) = DECLL(ILDL)(1:LPDL)//NAME(1:LL)
               ELSE
                  DECLL(ILDL) = DECLL(ILDL)(1:LPDL)//','//NAME(1:LL)
               ENDIF
               IFDL = 1
            ENDIF
            LPDL = LENOCC(DECLL(ILDL))
         ELSEIF (ITYPE .EQ. 5) THEN
            CALL HITOC(ISIZE, SIZE, LS, IERR)
            IF (LPDC+LL+LS+2 .GT. 72) THEN
               ILDC = ILDC + 1
               DECLC(ILDC) = '     + ,'//NAME(1:LL)//
     +                       '*'//SIZE(1:LS)
            ELSE
               IF (IFDC .EQ. 0) THEN
                  DECLC(ILDC) = DECLC(ILDC)(1:LPDC)//NAME(1:LL)//
     +                          '*'//SIZE(1:LS)
               ELSE
                  DECLC(ILDC) = DECLC(ILDC)(1:LPDC)//','//NAME(1:LL)//
     +                          '*'//SIZE(1:LS)
               ENDIF
               IFDC = 1
            ENDIF
            LPDC = LENOCC(DECLC(ILDC))
         ENDIF
*
*-- construct common statements
*
         IF (ITYPE .NE. 5) THEN
            IF (LPCMN+LV+1 .GT. 72) THEN
               ILCMN = ILCMN + 1
               CMN(ILCMN) = '     + ,'//VAR(1:LV)
            ELSE
               IF (IFCMN .EQ. 0) THEN
                  CMN(ILCMN) = CMN(ILCMN)(1:LPCMN)//VAR(1:LV)
               ELSE
                  CMN(ILCMN) = CMN(ILCMN)(1:LPCMN)//','//VAR(1:LV)
               ENDIF
               IFCMN = 1
            ENDIF
            LPCMN = LENOCC(CMN(ILCMN))
         ELSE
            IF (LPCMNC+LV+1 .GT. 72) THEN
               ILCMNC = ILCMNC + 1
               CMNC(ILCMNC) = '     + ,'//VAR(1:LV)
            ELSE
               IF (IFCMNC .EQ. 0) THEN
                  CMNC(ILCMNC) = CMNC(ILCMNC)(1:LPCMNC)//VAR(1:LV)
               ELSE
                  CMNC(ILCMNC) = CMNC(ILCMNC)(1:LPCMNC)//','//
     +                           VAR(1:LV)
               ENDIF
               IFCMNC = 1
            ENDIF
            LPCMNC = LENOCC(CMNC(ILCMNC))
         ENDIF
*
         IOFF = IOFF + ZNADDR
  10  CONTINUE
*
      IF (IFDR .NE. 0)  WRITE(LUN,1000) (DECLR(I)(1:LENOCC(DECLR(I))),
     +                  I = 1, ILDR)
      IF (IFDR8.NE. 0)  WRITE(LUN,1000) (DECLR8(I)(1:LENOCC(DECLR8(I))),
     +                  I = 1, ILDR8)
      IF (IFDI .NE. 0)  WRITE(LUN,1000) (DECLI(I)(1:LENOCC(DECLI(I))),
     +                  I = 1, ILDI)
      IF (IFDL .NE. 0)  WRITE(LUN,1000) (DECLL(I)(1:LENOCC(DECLL(I))),
     +                  I = 1, ILDL)
      IF (IFDC .NE. 0)  WRITE(LUN,1000) (DECLC(I)(1:LENOCC(DECLC(I))),
     +                  I = 1, ILDC)
      IF (IFCMN .NE. 0) WRITE(LUN,1000) (CMN(I)(1:LENOCC(CMN(I))),
     +                  I = 1, ILCMN)
      IF (IFCMNC .NE. 0) WRITE(LUN,1000) (CMNC(I)(1:LENOCC(CMNC(I))),
     +                   I = 1, ILCMNC)
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) THEN
         WRITE(LUN,1000) '*'
         GOTO 5
      ENDIF
*
 1000 FORMAT(A)
*
      END
