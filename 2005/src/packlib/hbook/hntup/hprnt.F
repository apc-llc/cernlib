*
* $Id: hprnt.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hprnt.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/06 27/06/94  17.42.22  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/01/92
      SUBROUTINE HPRNT(ID1)
*.==========>
*.
*.           To print statistics about a new N-tuple
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcnt.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcunit.inc"
*
      CHARACTER*80 VAR, TITLE
      CHARACTER*32 NAME, SUBS, RANGE, SMIN, SMAX
      CHARACTER*8  BLKNAM
      CHARACTER*9  SNOENT
      CHARACTER*5  SID, SCOL
      CHARACTER*4  SBLOK, SDIM
      CHARACTER*2  SIZE, BITS
      CHARACTER*1  TYPE
      LOGICAL      VTUP, LDUM
*
      ID    = ID1
      IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF (IDPOS .LE. 0) THEN
         CALL HBUG('Unknown N-tuple','HPRNT',ID1)
         RETURN
      ENDIF
      LCID  = LQ(LTAB-IDPOS)
      I4 = JBIT(IQ(LCID+KBITS),4)
      IF (I4 .EQ. 0) RETURN
      IF (IQ(LCID-2) .NE. ZLINK) THEN
         CALL HBUG('Old N-tuple, print statistics with HPRNTU',
     +             'HPRNT',ID)
         RETURN
      ENDIF
*
      VAR   = ' '
      TITLE = ' '
      ICOLS = 0
      VTUP  = .FALSE.
*
      LBLOK = LQ(LCID-1)
      LCHAR = LQ(LCID-2)
      LINT  = LQ(LCID-3)
      LREAL = LQ(LCID-4)
*
      NOENT = IQ(LCID+ZNOENT)
      ITIT1 = IQ(LCID+ZITIT1)
      NWTIT = IQ(LCID+ZNWTIT)
*
      CALL HITOC(ID1, SID, LL, IERR)
      CALL HITOC(NOENT, SNOENT, LL, IERR)
      CALL UHTOC(IQ(LCID+ITIT1), 4, TITLE, NWTIT*4)
      WRITE(LOUT,1000) SID, SNOENT, TITLE(1:LENOCC(TITLE))
      WRITE(LOUT,2000)
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
         CALL HNDESC(IOFF, NSUB, ITYPE, ISIZE, NBITS, LDUM)
         LL = IQ(LNAME+IOFF+ZLNAME)
         LV = IQ(LNAME+IOFF+ZNAME)
         CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
         IELEM = 1
         IF (NSUB .GT. 0) THEN
            VAR = NAME(1:LL)//'('
            DO 20 J = 1, NSUB
               LP = IQ(LINT+IQ(LNAME+IOFF+ZARIND)+(J-1))
               IF (LP .LT. 0) THEN
                  IE = -LP
                  CALL HITOC(IE, SUBS, LL, IERR)
               ELSE
                  LL = IQ(LNAME+LP-1+ZLNAME)
                  LV = IQ(LNAME+LP-1+ZNAME)
                  CALL UHTOC(IQ(LCHAR+LV), 4, SUBS, LL)
                  LL1 = IQ(LNAME+LP-1+ZRANGE)
                  IE  = IQ(LINT+LL1+1)
                  VTUP = .TRUE.
               ENDIF
               IELEM = IELEM*IE
*
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
         IF (IQ(LNAME+IOFF+ZRANGE) .EQ. 0) THEN
            RANGE = ' '
         ELSE
            LP = IQ(LNAME+IOFF+ZRANGE)
            IF (ITYPE .EQ. 1) THEN
               FMIN = Q(LREAL+LP)
               FMAX = Q(LREAL+LP+1)
               WRITE(SMIN, '(F15.4)') FMIN
               WRITE(SMAX, '(F15.4)') FMAX
               CALL HCLEFT(SMIN, 1, 15)
               CALL HCLEFT(SMAX, 1, 15)
               LSI = LENOCC(SMIN)
               LSA = LENOCC(SMAX)
            ELSEIF (ITYPE.EQ.2 .OR. ITYPE.EQ.3) THEN
               IMIN = IQ(LINT+LP)
               IMAX = IQ(LINT+LP+1)
               CALL HITOC(IMIN, SMIN, LSI, IERR)
               CALL HITOC(IMAX, SMAX, LSA, IERR)
            ENDIF
            RANGE = '['//SMIN(1:LSI)//','//SMAX(1:LSA)//']'
         ENDIF
*
         IF (ITYPE .EQ. 1) THEN
            TYPE = 'R'
         ELSEIF (ITYPE .EQ. 2) THEN
            TYPE = 'I'
         ELSEIF (ITYPE .EQ. 3) THEN
            TYPE = 'U'
         ELSEIF (ITYPE .EQ. 4) THEN
            TYPE = 'L'
         ELSEIF (ITYPE .EQ. 5) THEN
            TYPE = 'C'
         ENDIF
         CALL HITOC(ISIZE, SIZE, LS, IERR)
         IF (NBITS .EQ. IBIPB*ISIZE) THEN
            BITS = ' '
         ELSE
            CALL HITOC(NBITS, BITS, LS, IERR)
         ENDIF
*
         WRITE(LOUT,3000) I, TYPE//'*'//SIZE, BITS, RANGE(1:12),
     +                    BLKNAM, VAR(1:LENOCC(VAR))
*
         ICOLS = ICOLS + IELEM
         IOFF  = IOFF + ZNADDR
  10  CONTINUE
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) GOTO 5
*
      CALL HITOC(IQ(LCID+ZNBLOK), SBLOK, LL, IERR)
      CALL HITOC(IQ(LCID+ZNDIM), SDIM, LL, IERR)
      CALL HITOC(ICOLS, SCOL, LL, IERR)
      CALL HPRNTS
      IF (VTUP) THEN
         WRITE(LOUT,4000) SBLOK, SDIM, SCOL
      ELSE
         WRITE(LOUT,4010) SBLOK, SDIM, SCOL
      ENDIF
*
 1000 FORMAT(//,
     + ' *********************************************************',
     + '*********',/,
     + ' * Ntuple ID = ',A,'  Entries = ',A,1X,A)
 2000 FORMAT(
     + ' *********************************************************',
     + '*********',/,
     + ' * Var numb * Type * Packing *    Range     *  Block   *  ',
     + 'Name    *',/,
     + ' *********************************************************',
     + '*********')
 3000 FORMAT(
     + ' *',4X,I3,2X,' * ',A,' * ',3X,A,2X,' * ',A,' * ',A,' * ',A)
 4000 FORMAT(
     + ' *********************************************************',
     + '*********',/,
     + ' * Blocks = ',A,'         Variables = ',A,'    Max. Column',
     + 's = ',A,' *',/,
     + ' *********************************************************',
     + '*********',/)
 4010 FORMAT(
     + ' *********************************************************',
     + '*********',/,
     + ' * Blocks = ',A,'         Variables = ',A,'         Column',
     + 's = ',A,' *',/,
     + ' *********************************************************',
     + '*********',/)
*
  99  RETURN
      END
