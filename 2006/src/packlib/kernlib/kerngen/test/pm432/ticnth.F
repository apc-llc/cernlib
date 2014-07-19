*
* $Id: ticnth.F,v 1.1.1.1 1996/02/15 17:54:55 mclareni Exp $
*
* $Log: ticnth.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:55  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TICNTH

#include "kerngent/mkch.inc"
#include "kerngent/mkcde.inc"

      PARAMETER   (NPMOR=6, NAMOR=12)
      CHARACTER    CHPMOR(NPMOR)*8, CHAMOR(NAMOR)*8
      DIMENSION    MUMORG(NAMOR)

      PARAMETER   (NHAVE=7, NLOOK=14)
      CHARACTER    CHHAVE(NHAVE)*8, CHLOOK(NLOOK)*8
      DIMENSION    MUSTNE(NLOOK)

      PARAMETER   (NPXOR=4, NAXOR=20)
      CHARACTER    CHPXOR(NPXOR)*6, CHAXOR(NAXOR)*8
      DIMENSION    MUST14(NAXOR), MUST15(NAXOR)

      DATA CHPMOR  / 'del*ete ', 'add     ', 'adb*efor',
     +               'rep*lace', 'ch*ange ', 'c*ol*   '/
      DATA CHAMOR  / 'add!    ', 'delete! ', 'del!    ', 'del  !  ',
     +               'delphi! ', 'deleted!', 'replaced', 'chan!   ',
     +               'channel!', 'c!      ', 'columns!', 'cols!   ' /
      DATA MUMORG  / 2, 1, 1, 1, 0, 0, 4, 5, 0, 6, 6, 6 /

      DATA CHHAVE  / 'apo     ', 'apol    ', 'apollo  ', 'irs6000 ',
     +               'decra1  ', 'decra2  ', 'decra3  '/
      DATA CHLOOK  / 'apo     ', 'apo*    ', 'ap*ollo ', 'ap*     ',
     +               'ap      ', 'apol    ', 'apoll   ', 'apoll*  ',
     +               'ir*s60  ', 'ir*s70  ',
     +               'dec*    ', 'dec*ra  ', 'dec*ra* ', 'dec*ra3 ' /
      DATA MUSTNE  / 1, 1, 1, 1, 0, 2, 0, 3, 4, 0, 5, 5, 5, 7 /

      DATA  CHPXOR /   'change',   'cle*ar',   'mov*e ',  'c*ol* ' /
      DATA  CHAXOR /
     +    'changed ', 'change  ', 'cleared ', 'clear   ', 'clea    ',
     +    'cle     ', 'move    ', 'mov     ', 'collect ', 'column  ',
     +    'col     ', 'co      ', 'c       ', 'chang   ', 'cl      ',
     +    'moved   ', 'movi    ', 'com     ', 'cilan   ', 'MOVE    ' /

      DATA MUST14 / 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 4, 4, 4, 7*0 /
      DATA MUST15 / 0, 0, 2, 2, 2, 2, 3, 3, 4, 4, 4, 4, 4, 7*0 /

C-----------       ICNTH

      CALL NEWGUY ('ICNTH - ICINQ.', 'TICNTH  ')

      DO  24  JA=1,NAMOR
      N = ICFIND ('!', CHAMOR(JA),1,8) - 1
   24 IA(JA) = ICNTH (CHAMOR(JA)(1:N), CHPMOR, NPMOR)
      CALL MVERII (11,IA,MUMORG,NAMOR)

      DO  26  JA=1,NLOOK
      N = ICFIND ('!', CHLOOK(JA),1,8) - 1
   26 IA(JA) = ICINQ (CHLOOK(JA)(1:N), CHHAVE, NHAVE)
      CALL MVERII (12,IA,MUSTNE,NLOOK)


      DO  34  JA=1,NAXOR
   34 IA(JA) = ICNTH (CHAXOR(JA), CHPXOR, NPXOR)
      CALL MVERII (14,IA,MUST14,NAXOR)

      DO  35  JA=1,NAXOR
   35 IA(JA) = ICNTH (CHAXOR(JA)(1:5), CHPXOR, NPXOR)
      CALL MVERII (15,IA,MUST15,NAXOR)

C-----------       ICNTHL

      CALL NEWGUY ('ICNTHL - ICINQL.', 'TICNTH  ')

      DO 41  J=1,NAMOR,2
   41 CALL CLTOU (CHAMOR(J))

      DO 42  J=1,NLOOK,2
   42 CALL CLTOU (CHLOOK(J))

      DO  44  JA=1,NAMOR
      N = ICFIND ('!', CHAMOR(JA),1,8) - 1
   44 IA(JA) = ICNTHL (CHAMOR(JA)(1:N), CHPMOR, NPMOR)
      CALL MVERII (41,IA,MUMORG,NAMOR)

      DO  46  JA=1,NLOOK
      N = ICFIND ('!', CHLOOK(JA),1,8) - 1
   46 IA(JA) = ICINQL (CHLOOK(JA)(1:N), CHHAVE, NHAVE)
      CALL MVERII (43,IA,MUSTNE,NLOOK)

C-----------       ICNTHU

      CALL NEWGUY ('ICNTHU - ICINQU.', 'TICNTH  ')

      DO 51  J=1,NPMOR
   51 CALL CLTOU (CHPMOR(J))

      DO 52  J=1,NHAVE
   52 CALL CLTOU (CHHAVE(J))

      DO  54  JA=1,NAMOR
      N = ICFIND ('!', CHAMOR(JA),1,8) - 1
   54 IA(JA) = ICNTHU (CHAMOR(JA)(1:N), CHPMOR, NPMOR)
      CALL MVERII (51,IA,MUMORG,NAMOR)

      DO  56  JA=1,NLOOK
      N = ICFIND ('!', CHLOOK(JA),1,8) - 1
   56 IA(JA) = ICINQU (CHLOOK(JA)(1:N), CHHAVE, NHAVE)
      CALL MVERII (53,IA,MUSTNE,NLOOK)

C--      TIMING
      IF (ITIMES.EQ.0)       RETURN

      DO 72  JA=1,NAMOR
      CALL CTRANS ('!', ' ', CHAMOR(JA), 1,8)
   72 CALL CUTOL (CHAMOR(J))

      DO 73  J=1,NPMOR
   73 CALL CUTOL (CHPMOR(J))

      NTIMES = ITIMES*4000
      CALL TIMED (TIMERD)

      DO 76  J= 1,NTIMES
      DO 75  JA=1,NAMOR
   75 IA(JA) = ICNTH (CHAMOR(JA), CHPMOR, NPMOR)
   76 CONTINUE
      CALL TIME77 (NTIMES*NAMOR,'ICNTHU',NPMOR,'entries')
      RETURN
      END
