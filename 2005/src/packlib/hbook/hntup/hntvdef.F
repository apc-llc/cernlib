*
* $Id: hntvdef.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hntvdef.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/06 27/06/94  17.42.49  by  Fons Rademakers
*-- Author :    Fons Rademakers   14/08/92
      SUBROUTINE HNTVDEF(ID1,IVAR,CHTAG,BLOCK,ITYPE)
*.==========>
*.
*.           Returns the variable definition as given in HBNAME for
*.           variable with index IVAR in N-tuple ID1.
*.           N-tuple must already be in memory.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcnt.inc"
*
      CHARACTER*(*)  CHTAG, BLOCK
      CHARACTER*80   VAR
      CHARACTER*32   NAME, SUBS, RANGE,SMIN, SMAX
      CHARACTER*2    SIZE, BITS
      CHARACTER*1    TYPE
      LOGICAL        NEWTUP, LDUM
*
      ID    = ID1
      IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF (IDPOS .LE. 0) THEN
         CALL HBUG('Unknown N-tuple','HNTVAR',ID1)
         RETURN
      ENDIF
      IDLAST = ID
      LCID   = LQ(LTAB-IDPOS)
      I4 = JBIT(IQ(LCID+KBITS),4)
      IF (I4 .EQ. 0) RETURN
      NEWTUP = .TRUE.
      IF (IQ(LCID-2) .NE. ZLINK) NEWTUP = .FALSE.
*
      CHTAG = ' '
      NAME  = ' '
      BLOCK = ' '
      NSUB  = 0
      ITYPE = 0
      ISIZE = 0
*
      ICNT  = 0
*
      IF (NEWTUP) THEN
*
         IF (IVAR .GT. IQ(LCID+ZNDIM)) RETURN
*
         LBLOK = LQ(LCID-1)
         LCHAR = LQ(LCID-2)
         LINT  = LQ(LCID-3)
         LREAL = LQ(LCID-4)
*
*-- loop over all blocks
*
  5      LNAME = LQ(LBLOK-1)
*
         IOFF = 0
         NDIM = IQ(LBLOK+ZNDIM)
*
         DO 10 I = 1, NDIM
            ICNT = ICNT + 1
            IF (ICNT .EQ. IVAR) THEN
*
               CALL HNDESC(IOFF, NSUB, ITYPE, ISIZE, NBITS, LDUM)
*
               LL = IQ(LNAME+IOFF+ZLNAME)
               LV = IQ(LNAME+IOFF+ZNAME)
               CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
               CALL UHTOC(IQ(LBLOK+ZIBLOK), 4, BLOCK, 8)
*
               IF (NSUB .GT. 0) THEN
                  VAR = NAME(1:LL)//'('
                  DO 25 J = 1, NSUB
                     LP = IQ(LINT+IQ(LNAME+IOFF+ZARIND)+(J-1))
                     IF (LP .LT. 0) THEN
                        IE = -LP
                        CALL HITOC(IE, SUBS, LL, IERR)
                     ELSE
                        LL = IQ(LNAME+LP-1+ZLNAME)
                        LV = IQ(LNAME+LP-1+ZNAME)
                        CALL UHTOC(IQ(LCHAR+LV), 4, SUBS, LL)
                     ENDIF
*
                     IF (J .EQ. 1) THEN
                        VAR = VAR(1:LENOCC(VAR))//SUBS(1:LL)
                     ELSE
                        VAR = VAR(1:LENOCC(VAR))//','//SUBS(1:LL)
                     ENDIF
   25             CONTINUE
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
               VAR = VAR(1:LENOCC(VAR))//RANGE
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
               VAR = VAR(1:LENOCC(VAR))//':'//TYPE//'*'//SIZE
               IF (BITS.NE.' ' .AND. ITYPE.NE.4 .AND. ITYPE.NE.5) THEN
                  VAR = VAR(1:LENOCC(VAR))//':'//BITS
               ENDIF
*
               CHTAG = VAR
               RETURN
*
            ENDIF
*
            IOFF = IOFF + ZNADDR
  10     CONTINUE
*
         LBLOK = LQ(LBLOK)
         IF (LBLOK .NE. 0) GOTO 5
*
      ELSE
*
         IF (IVAR .GT. IQ(LCID+2)) RETURN
         ITAG1 = IQ(LCID+10)
         CALL UHTOC(IQ(LCID+ITAG1+2*(IVAR-1)), 4, NAME, 8)
         ITYPE = 1
         CHTAG = NAME(1:LENOCC(NAME))//':R*4'
*
      ENDIF
*
      END
