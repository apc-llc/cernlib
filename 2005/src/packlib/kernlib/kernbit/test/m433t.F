*
* $Id: m433t.F,v 1.1.1.1 1996/02/15 17:47:55 mclareni Exp $
*
* $Log: m433t.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE M433T
*
*     Test of M433 Package
*
      CHARACTER*80 STRIP,WORD,SUBWORD
      CHARACTER*80 STRING,STROUT
      INTEGER      WORDS
      DIMENSION IW(4),NW(4)
#if defined(CERNLIB_NEXT)
C**   The Absoft compiler has an intrinsic function called WORD
C     It needs the following declaration to take the KERNLIB one
      EXTERNAL WORD
#endif
      DATA IW/1,4,7,9/, NW/3,5,4,2/
*
      CALL KBHDR('M433 CLTOU')
      NT=0
      STRING='This  is  a  test1    of M433 CLTOU'
1     CONTINUE
      NT=NT+1
      NWS=WORDS(STRING)
      PRINT 1000,NT,NWS,STRING
C
      DO 10 N=0,9
        STROUT=WORD(STRING,N)
        LSTR  =LENOCC(STROUT)
        PRINT 1001,N,LSTR,STROUT
10    CONTINUE
C
      DO 20 N=1,4
        STROUT=SUBWORD(STRING,IW(N),NW(N))
        LSTR  =LENOCC(STROUT)
        PRINT 1002,IW(N),NW(N),LSTR,STROUT
20    CONTINUE
      IF(NT.EQ.1) THEN
        STRING=' /// This/is/ test2//  of/M433/CLTOU/// '
        CALL WORDSEP('/')
      ENDIF
      IF(NT.LT.2) GOTO 1
99    CONTINUE
      STROUT=STRING
      LSTR=LENOCC(STROUT)
      PRINT *, 'Stripping Leading  " " of: >>>',STROUT(1:LSTR),'<<<'
      STROUT=STRIP(STROUT,'Leading',' ')
      LSTR=LENOCC(STROUT)
      PRINT *, 'Stripping Trailing " " of: >>>',STROUT(1:LSTR),'<<<'
      STROUT=STRIP(STROUT,'trail',' ')
      LSTR=LENOCC(STROUT)
      PRINT *, 'Stripping Both     "/" of: >>>',STROUT(1:LSTR),'<<<'
      STROUT=STRIP(STROUT,'both','/')
      LSTR=LENOCC(STROUT)
      PRINT *, 'Stripped string          : >>>',STROUT(1:LSTR),'<<<'
      RETURN
1000  FORMAT(' Test#',I2,' string(nw=',I2,'):',A)
1001  FORMAT(' Word   (',I5,       ',L=',I2,')=',A)
1002  FORMAT(' Subword(',I2,',',I2,',L=',I2,')=',A)
      END
