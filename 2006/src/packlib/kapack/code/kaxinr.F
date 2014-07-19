*
* $Id: kaxinr.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxinr.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXINR
*
*.....PERFORM RUN-DEPENDENT INITIALIZATION
*
#include "kapack/kax000.inc"
#include "kapack/kax010.inc"
#include "kapack/kax020.inc"
#include "kapack/kax030.inc"
#include "kapack/kax040.inc"
#include "kapack/kax060.inc"
#include "kapack/kax070.inc"
*
      CHARACTER C(1:MAXC)*1
*
*.....DEFINE THE INTERNAL CHARACTER SET
      DATA
     +    C(01) /' '/,
     +    C(02) /'0'/,    C(03) /'1'/,    C(04) /'2'/,    C(05) /'3'/,
     +    C(06) /'4'/,    C(07) /'5'/,    C(08) /'6'/,    C(09) /'7'/,
     +    C(10) /'8'/,    C(11) /'9'/,    C(12) /'A'/,    C(13) /'B'/,
     +    C(14) /'C'/,    C(15) /'D'/,    C(16) /'E'/,    C(17) /'F'/,
     +    C(18) /'G'/,    C(19) /'H'/,    C(20) /'I'/,    C(21) /'J'/,
     +    C(22) /'K'/,    C(23) /'L'/,    C(24) /'M'/,    C(25) /'N'/,
     +    C(26) /'O'/,    C(27) /'P'/,    C(28) /'Q'/,    C(29) /'R'/,
     +    C(30) /'S'/,    C(31) /'T'/,    C(32) /'U'/,    C(33) /'V'/,
     +    C(34) /'W'/,    C(35) /'X'/,    C(36) /'Y'/,    C(37) /'Z'/,
     +    C(38) /'$'/,    C(39) /''''/,   C(40) /'('/,    C(41) /')'/,
     +    C(42) /'*'/,    C(43) /'+'/,    C(44) /','/,    C(45) /'-'/,
     +    C(46) /'.'/,    C(47) /'/'/,    C(48) /':'/,    C(49) /'='/
*
*-----------------------------------------------------------------------
*
*.....CHECK THE VALIDITY OF THE INSTALLATION-DEPENDENT PARAMETERS
      IF ( ISYSWR .LT. 0   .OR.  ISYSWR .GT. 99   )  GO TO 90
      IF ( IWAIT  .LT. 0   .OR.  IWAIT  .GT. 6000 )  GO TO 91
      IF ( LBUF   .LT. 80                         )  GO TO 92
      IF ( LLINE  .LT. 70  .OR.  LLINE  .GT. 150  )  GO TO 93
      IF ( LWORD  .LT. 1   .OR.  LWORD  .GT. 10   )  GO TO 94
      IF ( MAXINT .LT. 99                         )  GO TO 95
      IF ( MAXLUN .LT. 1   .OR.  MAXLUN .GT. 99   )  GO TO 96
      IF ( MAXNAM .LT. 1                          )  GO TO 97
      IF ( MAXTRY .LT. 1   .OR.  MAXTRY .GT. 10   )  GO TO 98
*
*.....GET THE JOBID FOR THIS RUN
      JOBID = KAXJID()
*
*.....INITIALIZE THE INTERNAL CHARACTER SET
      DO  1  I = 1, MAXC
         CSET(I) = C(I)
 1    CONTINUE
*
*.....INITIALIZE FILE AND RUN-DEPENDENT PARAMETERS AND DEFAULT OPTIONS
      DO  2  I = 1, MAXLUN
         LBKS(I)   = 0
         NBKS(I)   = 0
         MAXK(I)   = 0
         MAXP(I)   = 0
         TITLED(I) = .FALSE.
         IOPTNS(I) = ISYSWR*10000 + ISYSWR*100
 2    CONTINUE
*
*.....INITIALIZE THE LOCK QUEUE
      DO  3  I = 1, LLOCKQ
         LOCKI(I) = 0
         LOCKU(I) = 0
         LOCKN(I) = 0
 3    CONTINUE
*
*.....INITIALIZE THE MAJOR STACK, (I.E. INVALIDATE ALL ENTRIES)
      DO  4  I = 1, LMQ
         KUNIT(I) = -1
 4    CONTINUE
*
*.....SUCCESSFUL COMPLETION
      RETURN
*
*.....ERROR PROCESSING
 90   WRITE(*,190) ISYSWR, 'ISYSWR'
      GO TO 999
*
 91   WRITE(ISYSWR,190) IWAIT, 'IWAIT'
      GO TO 999
*
 92   WRITE(ISYSWR,190) LBUF, 'LBUF'
      GO TO 999
*
 93   WRITE(ISYSWR,190) LLINE, 'LLINE'
      GO TO 999
*
 94   WRITE(ISYSWR,190) LWORD, 'LWORD'
      GO TO 999
*
 95   WRITE(ISYSWR,190) MAXINT, 'MAXINT'
      GO TO 999
*
 96   WRITE(ISYSWR,190) MAXLUN, 'MAXLUN'
      GO TO 999
*
 97   WRITE(ISYSWR,190) MAXNAM, 'MAXNAM'
      GO TO 999
*
 98   WRITE(ISYSWR,190) MAXTRY, 'MAXTRY'
      GO TO 999
*
 999  STOP 999
*
 190  FORMAT(' CRNKA300 KAXINR: KAPACK HAS BEEN INSTALLED WITH THE UNACC
     +EPTABLE VALUE OF ',I11,' FOR THE PARAMETER ''',A,'''')
*
      END
