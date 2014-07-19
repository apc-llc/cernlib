*
* $Id: hntnam.F,v 1.2 1999/02/18 09:52:25 couet Exp $
*
* $Log: hntnam.F,v $
* Revision 1.2  1999/02/18 09:52:25  couet
* - the PART character array and related variables were too small (32).
*   They are now 80 characters. The error message generated was:
*
*   ***** ERROR in HBNAME : Error in token
*
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          05/09/95  19.04.50  by  Julian Bunn
*-- Author :    Fons Rademakers   17/12/91
      SUBROUTINE HNTNAM(TOK, IADD)
*.==========>
*.
*.           Decode a format token and store the information
*.           in the LNAME bank (and also LCHAR, LINT and LREAL banks).
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcnt.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcntpaw.inc"
*
      COMMON /QUEST/ IQUEST(100)
*
      CHARACTER*(*)  TOK
      INTEGER        IADD, ICOL, LCOL, CCOL
      CHARACTER*80   INDVAR
      PARAMETER     (MAXPAR = 10)
      CHARACTER*80   PART(MAXPAR), NAME, RANGE, SUBS(MAXPAR), CDUM
      CHARACTER*8    BLOCK
      CHARACTER*12   STRINT
      PARAMETER     (STRINT = 'IJKLMNijklmn')
      INTEGER        NDIM, IOFF, LT, LP, IP, IB, IP2, IB2, ITYPE, ISIZE
      INTEGER        NBITS, IMIN, IMAX, IDUM, IELEM, IS, I, LL, NSUB
      INTEGER        L, J, IE, HNMPTR
      REAL           FMIN, FMAX
*
#include "hbook/jbyt.inc"
*
      IERR = 0
*
      NDIM = IQ(LBLOK+ZNDIM)
      IOFF = IQ(LBLOK+ZIFNAM) - 1
*
      LT = LENOCC(TOK)
      IF (LT .EQ. 0) GOTO 901
*
      FMIN   = 0.0
      FMAX   = 0.0
      IMIN   = 0
      IMAX   = 0
      NSUB   = 0
      IELEM  = 1
      INDVAR = ' '
      RANGE  = ' '
      DO 5 I = 1, 10
         PART(I) = ' '
         SUBS(I) = ' '
5     CONTINUE
*
*-- get current size of Ntuple, print warning when more than MAXC..
*-- columns are defined for any of the three sizes. This is a limit
*-- imposed by PAW not HBOOK.
*
      CALL HNSIZE(ICOL, LCOL, CCOL)
*
*-- find the different parts of the format token
*
      CALL HNPART(TOK, PART, NPART, MAXPAR, IERR)
      IF (IERR .EQ. 1) GOTO 902
      IF (IERR .EQ. 2) GOTO 903
*
*-- too many parts
*
      IF (NPART .GT. 4) GOTO 903
*
*-- part 1: contains the variable name and, possibly, an array
*--         specification (either a number or a index variable) or
*--         in case of an integer a range
*
      LP = LENOCC(PART(1))
      IF (LP .EQ. 0) GOTO 904
*
      IP  = INDEX(PART(1), '(')
      IP2 = INDEX(PART(1), ')')
      IB  = INDEX(PART(1), '[')
      IB2 = INDEX(PART(1), ']')
      IF (IP.EQ.0 .AND. IB.EQ.0) THEN
         NAME = PART(1)
      ENDIF
      IF (IP .NE. 0) THEN
         NAME = PART(1)(1:IP-1)
         IF (IP2.EQ.0 .OR. IP2.EQ.IP+1) GOTO 904
         INDVAR = PART(1)(IP+1:IP2-1)
      ENDIF
      IF (IB .NE. 0) THEN
         IF (IP2 .GT. IB) GOTO 904
         IF (IP .EQ. 0) NAME = PART(1)(1:IB-1)
         IF (IB2.EQ.0 .OR. IB2.EQ.IB+1) GOTO 904
         RANGE = PART(1)(IB+1:IB2-1)
      ENDIF
*
*-- has name already been defined in this Ntuple?
*
      CALL HVXIST(NAME,BLOCK,CDUM,IDUM1,IDUM2,IDUM3)
      IF (IDUM3.NE.0) GOTO 913
*
*-- part 2: contains the type and size of the variable
*
      LP = LENOCC(PART(2))
      IF (LP .EQ. 0) THEN
*
*-- take defaults depending on NAME
*
         IF (INDEX(STRINT, NAME(1:1)) .NE. 0) THEN
            ITYPE = 2
            ISIZE = 4
         ELSE
            ITYPE = 1
            ISIZE = 4
         ENDIF
      ELSE
         CALL CLTOU(PART(2))
         IS = INDEX(PART(2), '*')
         IF ((IS.NE.0 .AND. IS.NE.2) .OR. IS.EQ.LP) GOTO 904
         IF (PART(2)(1:1)     .EQ. 'R') THEN
            ITYPE = 1
         ELSEIF (PART(2)(1:1) .EQ. 'I') THEN
            ITYPE = 2
         ELSEIF (PART(2)(1:1) .EQ. 'U') THEN
            ITYPE = 3
         ELSEIF (PART(2)(1:1) .EQ. 'L') THEN
            ITYPE = 4
         ELSEIF (PART(2)(1:1) .EQ. 'C') THEN
            ITYPE = 5
         ELSE
            GOTO 905
         ENDIF
*
         IF (IS .EQ. 0) THEN
            IF (LENOCC(PART(2)) .GT. 1) GOTO 925
            IF (ITYPE .EQ. 1) ISIZE = 4
            IF (ITYPE .EQ. 2) ISIZE = 4
            IF (ITYPE .EQ. 3) ISIZE = 4
            IF (ITYPE .EQ. 4) ISIZE = 4
            IF (ITYPE .EQ. 5) ISIZE = 4
         ELSE
            CALL HICTOI(PART(2)(IS+1:LP), ISIZE)
         ENDIF
         IF (ITYPE.EQ.1 .AND. (ISIZE.NE.4 .AND. ISIZE.NE.8)) GOTO 906
         IF (ITYPE.EQ.2 .AND. (ISIZE.NE.4 .AND. ISIZE.NE.8)) GOTO 906
         IF (ITYPE.EQ.3 .AND. (ISIZE.NE.4 .AND. ISIZE.NE.8)) GOTO 906
         IF (ITYPE.EQ.4 .AND. (ISIZE.NE.4))                  GOTO 906
         IF (ITYPE.EQ.5 .AND. (ISIZE.LT.1 .OR. ISIZE.GT.32 .OR.
     +                         IAND(ISIZE, 3).NE.0))         GOTO 906
      ENDIF
*
*-- part 3: contains the packing bits (not for logicals or characters,
*--         logicals are always packed in one bit)
*
      LP = LENOCC(PART(3))
      IF (LP .EQ. 0) THEN
         NBITS = IBIPB*ISIZE
         IF (ITYPE .EQ. 4) NBITS = 1
      ELSE
         IF (ITYPE.EQ.4 .OR. ITYPE.EQ.5) GOTO 907
         CALL HICTOI(PART(3), NBITS)
      ENDIF
      IF (NBITS .GT. IBIPB*ISIZE) GOTO 919
*
*-- following features currently not implemented in HFNT2 and HGNT2:
*--    Real, integer and unsigned integer packed double precision
*
      IF (ITYPE.EQ.1 .AND. ISIZE.EQ.8 .AND. NBITS.LT.64) GOTO 918
      IF (ITYPE.EQ.2 .AND. ISIZE.EQ.8 .AND. NBITS.LT.64) GOTO 918
      IF (ITYPE.EQ.3 .AND. ISIZE.EQ.8 .AND. NBITS.LT.64) GOTO 918
*
*-- part 4: in case of an integer or real this part can contain the range
*           in case of a real the range may only be set when NBITS is set
*           for logical and characters this part may not be set
*
      LP = LENOCC(PART(4))
      IF (LP .NE. 0) THEN
         IF (ITYPE.EQ.4 .OR. ITYPE.EQ.5) GOTO 908
         IF (ITYPE.EQ.1 .AND. NBITS.EQ.IBIPB*ISIZE) GOTO 910
         IB = INDEX(PART(4), '[')
         IF (IB .EQ. 0) GOTO 904
         IB2 = INDEX(PART(4),']')
         IF (IB2.EQ.0 .OR. IB2.EQ.IB-1) GOTO 904
         RANGE = PART(4)(IB+1:IB2-1)
      ENDIF
*
*-- check the range
*
      IF (RANGE .NE. ' ') THEN
         IF (ITYPE.EQ.4 .OR. ITYPE.EQ.5) GOTO 908
         LP = LENOCC(RANGE)
         IS = INDEX(RANGE, ',')
         IF (IS.EQ.0 .OR. IS.EQ.1 .OR. IS.EQ.LP) GOTO 904
         IF (ITYPE .EQ. 1) THEN
            IF (NBITS .EQ. IBIPB*ISIZE) GOTO 910
            CALL HICTON(RANGE(1:IS-1),  IDUM, FMIN)
            IF (IQUEST(1) .NE. 0) GOTO 904
            CALL HICTON(RANGE(IS+1:LP), IDUM, FMAX)
            IF (IQUEST(1) .NE. 0) GOTO 904
            IF (FMIN .GT. FMAX) GOTO 917
         ELSE
            CALL HICTOI(RANGE(1:IS-1),  IMIN)
            CALL HICTOI(RANGE(IS+1:LP), IMAX)
*
            IF (IABS(IMAX) .GT. IABS(IMIN)) THEN
               IMX = IABS(IMAX)
            ELSE
               IMX = IABS(IMIN)
            ENDIF
*
            IF (IMIN .GT. IMAX) GOTO 917
*
*-- if manifest positive make it an unsigned integer
*
            IF (IMIN.GE.0 .AND. IMAX.GE.0) ITYPE = 3
*
            IF (ITYPE .EQ. 2) THEN
               ISIGN = 1
            ELSE
               ISIGN = 0
               IF (IMIN.LT.0 .OR. IMAX.LT.0) GOTO 916
            ENDIF
*
            DO 10 I = 1, 32
               IF (IMX .LE. 2**I-1) GOTO 20
10          CONTINUE
20          IF (NBITS .EQ. IBIPB*ISIZE) NBITS = I + ISIGN
            IF (NBITS .LT. I+ISIGN) GOTO 911
         ENDIF
      ELSE
         IF (ITYPE.EQ.1 .AND. NBITS.NE.IBIPB*ISIZE) GOTO 929
      ENDIF
*
*-- check INDVAR (either a number or an index variable)
*-- in case an index variable it must already be defined: find it
*
      IF (INDVAR .NE. ' ') THEN
         CALL HNTTOK(INDVAR, SUBS, NSUB, MAXPAR, IERR)
         IF (IERR .NE. 0) GOTO 904
         IF (NSUB .GT. 7) GOTO 920
*
*-- check space in LINT bank
*
         JMAX = IQ(LCID+ZIFINT)-1 + NSUB
         IF (JMAX .GT. IQ(LINT-1)) THEN
            CALL MZPUSH(IHDIV, LINT, 0, NSUB, 'I')
         ENDIF
*
         J = IQ(LCID+ZIFINT)
         IQ(LNAME+IOFF+ZARIND) = J
         DO 25 I = 1, NSUB
            LP = LENOCC(SUBS(I))
            IF (ICHAR(SUBS(I)(1:1)) .GE. ICHAR('0') .AND.
     +          ICHAR(SUBS(I)(1:1)) .LE. ICHAR('9')) THEN
               CALL HICTOI(SUBS(I), IE)
               IF (IQUEST(1) .NE. 0) GOTO 904
               IQ(LINT+J+(I-1)) = -IE
               IELEM = IELEM*IE
            ELSE
*
*-- only last dimension may be an index variable
*
               IF (I .NE. NSUB) GOTO 921
*
*-- find the index variable
*
               L = HNMPTR(SUBS(I)(1:LP))
               IF (L .LT. 0) GOTO 909
*
*-- check validity of index variable definition
*--    1)  must be of type int (if type was unsigned over-rule)
*--    2)  must be at least 4 bytes
*--    3)  no array
*--    4)  no packing allowed (if packing was specified over-rule)
*
               ITIND = JBYT(IQ(LNAME+L+ZDESC), 14, 4)
               IF (ITIND .NE. 2 .AND. ITIND .NE. 3) GOTO 914
               IF (ITIND .EQ. 3) CALL SBYT(2, IQ(LNAME+L+ZDESC), 14, 4)
               ISIND = JBYT(IQ(LNAME+L+ZDESC), 8, 6)
               IF (ISIND .LT. 4) GOTO 915
               ISUIND = JBYT(IQ(LNAME+L+ZDESC), 18, 3)
               IF (ISUIND .GT. 0) GOTO 924
               CALL SBYT(IBIPB*ISIND, IQ(LNAME+L+ZDESC), 1, 7)
*
*-- set index variable bit
*
               CALL SBIT1(IQ(LNAME+L+ZDESC), 28)
               IQ(LINT+J+(I-1)) = L+ZDESC
*
*-- get the the range of the index variable and add to IELEM
*
               LL = IQ(LNAME+L+ZRANGE)
               IF (LL .EQ. 0) GOTO 922
*
*-- old version required lower limit to be 1, to be backward compatible
*-- print a warning and change 1 to 0 (the new required lower limit)
*
               IF (IQ(LINT+LL) .EQ. 1) THEN
                  IQ(LINT+LL) = 0
                  PRINT *, 'Variable ', SUBS(I)(1:LP)
                  CALL HBUG('+Lower limit of range of index variable'//
     +                      ' changed from 1 to 0', 'HBNAME',ID)
               ENDIF
               IF (IQ(LINT+LL) .NE. 0) GOTO 923
               JMAX  = IQ(LINT+LL+1)
               IELEM = IELEM*JMAX
            ENDIF
25       CONTINUE
         IQ(LCID+ZIFINT) = IQ(LCID+ZIFINT) + NSUB
      ELSE
         IQ(LNAME+IOFF+ZARIND) = 0
      ENDIF
*
*-- print error when Ntuple gets too large to be analyzed by PAW
*
      IF (ITYPE .NE. 5) THEN
         IF (ISIZE.EQ.4 .AND. ICOL+IELEM.GT.MAXCR4) GOTO 926
         IF (ISIZE.EQ.8 .AND. LCOL+IELEM.GT.MAXCR8) GOTO 927
      ELSE
         IF (CCOL+IELEM.GT.MAXC32) GOTO 928
      ENDIF
*
*-- make sure there is enough space in the LCHAR, LREAL and LINT banks
*
      INEED = LENOCC(NAME)
      JMAX  = IQ(LCID+ZIFCHA)-1 + INEED
      IF (JMAX .GT. IQ(LCHAR-1)) THEN
         CALL MZPUSH(IHDIV, LCHAR, 0, INEED, 'I')
      ENDIF
      INEED = 2
      JMAX  = IQ(LCID+ZIFREA)-1 + INEED
      IF (JMAX .GT. IQ(LREAL-1)) THEN
         CALL MZPUSH(IHDIV, LREAL, 0, INEED, 'I')
      ENDIF
      JMAX  = IQ(LCID+ZIFINT)-1 + INEED
      IF (JMAX .GT. IQ(LINT-1)) THEN
         CALL MZPUSH(IHDIV, LINT, 0, INEED, 'I')
      ENDIF
*
*-- fill the LNAME data structure for this variable
*
*-- in ZDESC: the ITYPE, ISIZE and NBITS
*
40    CALL SBYT(NSUB,  IQ(LNAME+IOFF+ZDESC), 18, 3)
      CALL SBYT(ITYPE, IQ(LNAME+IOFF+ZDESC), 14, 4)
      CALL SBYT(ISIZE, IQ(LNAME+IOFF+ZDESC), 8,  6)
      CALL SBYT(NBITS, IQ(LNAME+IOFF+ZDESC), 1,  7)
*
*-- in ZLNAME: the length of the variable name
*
      LL = LENOCC(NAME)
      IQ(LNAME+IOFF+ZLNAME) = LL
*
*-- in ZNAME: the pointer to the name in LCHAR
*
      I = IQ(LCID+ZIFCHA)
      IQ(LNAME+IOFF+ZNAME) = I
      CALL UCTOH(NAME, IQ(LCHAR+I), 4, LL)
      IQ(LCID+ZIFCHA) = IQ(LCID+ZIFCHA) + (LL+3)/4
*
*-- in ZRANGE: the pointer to the range in the LREAL bank for reals
*--            and in the LINT bank for integers
*
      IF (RANGE.NE.' ' .AND. ITYPE.EQ.1) THEN
         I = IQ(LCID+ZIFREA)
         IQ(LNAME+IOFF+ZRANGE) = I
         Q(LREAL+I)      = FMIN
         Q(LREAL+I+1)    = FMAX
         IQ(LCID+ZIFREA) = IQ(LCID+ZIFREA) + 2
      ELSEIF (RANGE.NE.' ' .AND. (ITYPE.EQ.2 .OR. ITYPE.EQ.3)) THEN
         I = IQ(LCID+ZIFINT)
         IQ(LNAME+IOFF+ZRANGE) = I
         IQ(LINT+I)     = IMIN
         IQ(LINT+I+1)   = IMAX
         IQ(LCID+ZIFINT) = IQ(LCID+ZIFINT) + 2
      ELSE
         IQ(LNAME+IOFF+ZRANGE) = 0
      ENDIF
*
*-- number of RZ records or memory blocks used
*
      IQ(LNAME+IOFF+ZNRZB) = 1
*
*-- contents buffer currently in memory
*
      IQ(LNAME+IOFF+ZIBANK) = 1
*
*-- link position of contents bank in LBUF
*
      IQ(LNAME+IOFF+ZLCONT) = IQ(LCID+ZNDIM) + 1
*
*-- pointer to first free word in contents bank
*
      IQ(LNAME+IOFF+ZIFCON) = 2
*
*-- pointer to first free bit in first free word in contents bank
*
      IQ(LNAME+IOFF+ZIFBIT) = 1
*
*-- in ZNADDR: the address of the first word of the variable
*
      IF (ISIZE.EQ.8 .AND. ITYPE.NE.5) THEN
         IBYOF = IAND(IADD, (2*IBYPW)-1)
         IF (IBYOF .NE. 0) GOTO 930
      ELSE
         IBYOF = IAND(IADD, IBYPW-1)
         IF (IBYOF .NE. 0) GOTO 912
      ENDIF
*
      IADDW = ISHFT(IADD, -2)
      IQ(LNAME+IOFF+ZNADDR) = IADDW - LOCF(IQ(1))
      IADD = IADD + IELEM*ISIZE
*
      IQ(LBLOK+ZIFNAM) = IQ(LBLOK+ZIFNAM) + ZNADDR
      IQ(LBLOK+ZNDIM)  = IQ(LBLOK+ZNDIM) + 1
      IQ(LCID+ZNDIM)   = IQ(LCID+ZNDIM) + 1
*
      RETURN
*
901   IERR = 1
      RETURN
*
902   IERR = 2
      PRINT *, 'Token ', TOK(1:LENOCC(TOK))
      CALL HBUG('Error in token','HBNAME',ID)
      RETURN
*
903   IERR = 3
      PRINT *, 'Token ', TOK(1:LENOCC(TOK))
      CALL HBUG('Error in token','HBNAME',ID)
      RETURN
*
904   IERR = 4
      PRINT *, 'Token ', TOK(1:LENOCC(TOK))
      CALL HBUG('Error in token','HBNAME',ID)
      RETURN
*
905   IERR = 5
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Illegal variable type','HBNAME',ID)
      RETURN
*
906   IERR = 6
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Illegal variable size','HBNAME',ID)
      RETURN
*
907   IERR = 7
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Packing bits are not allowed for this type',
     +          'HBNAME',ID)
      RETURN
*
908   IERR = 8
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Range description not allowed for this type',
     +          'HBNAME',ID)
      RETURN
*
909   IERR = 9
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Index variable not yet defined',
     +          'HBNAME',ID)
      RETURN
*
910   IERR = 10
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('No packing bits have been specified',
     +          'HBNAME',ID)
      RETURN
*
911   IERR = 11
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('No. of packing bits incompatible with range '//
     +          'limits','HBNAME',ID)
      RETURN
*
912   IERR = 12
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Address not word aligned','HBNAME',ID)
      RETURN
*
913   IERR = 13
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Variable name already used in this Ntuple',
     +          'HBNAME',ID)
      RETURN
*
914   IERR = 14
      PRINT *, 'Variable ', SUBS(I)(1:LP)
      CALL HBUG('Index variable must be an integer','HBNAME',ID)
      RETURN
*
915   IERR = 15
      PRINT *, 'Variable ', SUBS(I)(1:LP)
      CALL HBUG('Index variable must be, at least, 4 bytes',
     +          'HBNAME',ID)
      RETURN
*
916   IERR = 16
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Negative range limit not allowed for unsigned '//
     +          'integer','HBNAME',ID)
      RETURN
*
917   IERR = 17
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Lower limit of range bigger than upper limit',
     +          'HBNAME',ID)
      RETURN
*
918   IERR = 18
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Packed double precision not yet implemented',
     +          'HBNAME',ID)
      RETURN
*
919   IERR = 19
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Packing bit specification incompatible with size',
     +          'HBNAME',ID)
      RETURN
*
920   IERR = 20
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Variable may not have more than 7 dimensions',
     +          'HBNAME',ID)
      RETURN
*
921   IERR = 21
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Only last dimension may be an index variable',
     +          'HBNAME',ID)
      RETURN
*
922   IERR = 22
      PRINT *, 'Variable ', SUBS(I)(1:LP)
      CALL HBUG('Index variable must have a range specification',
     +          'HBNAME',ID)
      RETURN
*
923   IERR = 23
      PRINT *, 'Variable ', SUBS(I)(1:LP)
      CALL HBUG('Lower limit of range of index variable must be 0',
     +          'HBNAME',ID)
      RETURN
*
924   IERR = 24
      PRINT *, 'Variable ', SUBS(I)(1:LP)
      CALL HBUG('Index variable may not be an array',
     +          'HBNAME',ID)
      RETURN
*
925   IERR = 25
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Illegal variable size specification, use * separator',
     +          'HBNAME',ID)
      RETURN
*
926   IERR = 26
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Ntuple can not have more than 50000 real*4, integer'//
     +          ' or logical columns', 'HBNAME',ID)
      RETURN
*
927   IERR = 27
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Ntuple can not have more than 1000 real*8 '//
     +          'columns', 'HBNAME',ID)
      RETURN
*
928   IERR = 28
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Ntuple can not have more than 1000 character*32 '//
     +          'columns', 'HBNAME',ID)
      RETURN
*
929   IERR = 29
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Packing bits specified without range',
     +          'HBNAME',ID)
      RETURN
*
930   IERR = 30
      PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Address not double-word aligned','HBNAME',ID)
      RETURN
*
      END
 
 
