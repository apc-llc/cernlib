*
* $Id: main.F,v 1.6 1996/09/17 08:53:22 cernlib Exp $
*
* $Log: main.F,v $
* Revision 1.6  1996/09/17 08:53:22  cernlib
* Add new routines funlux.F and its test v152m.F
*
* Revision 1.5  1996/04/22 14:49:57  mclareni
* Remove obsolete routine ADIGAM/DDIGAM (C317) from c/Imakefile,
* also from the test program and Imakefile.
*
* Revision 1.4  1996/04/17 12:32:11  mclareni
* Add d/rfstft.F (D705) and to Imakefile. cfstft.F becomes D706.
* In tests, add d705m.F for rfstft and d706m.F for cfstft and the corresponding
* additions to main.F and Imakefile.
*
* Revision 1.3  1996/04/10 16:37:41  mclareni
* Add test for cfstst (D705), d705m.F, also to main and Imakefile
*
* Revision 1.2  1996/04/02 16:23:50  mclareni
* More precise dclaus64 (C326), test added and C344 removed from TESTALL
*
* Revision 1.1.1.1  1996/04/01 15:01:12  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      PROGRAM GENTST
#include "iorc.inc"
C     Open output streams
      CALL GTOPEN
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_B100)
      CALL B100M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_B101)
      CALL B101M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_B102)
      CALL B102M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_B300)
      CALL B300M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C200)
      CALL C200M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C201)
      CALL C201M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C202)
      CALL C202M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C206)
C      OBSOLETE   CALL C206M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C207)
      CALL C207M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C208)
      CALL C208M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C209)
      CALL C209M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C210)
      CALL C210M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C300)
      CALL C300M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C301)
      CALL C301M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C302)
      CALL C302M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C303)
      CALL C303M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C304)
      CALL C304M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C306)
      CALL C306M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C307)
      CALL C307M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C310)
      CALL C310M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C312)
      CALL C312M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C313)
      CALL C313M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C315)
      CALL C315M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C316)
      CALL C316M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C318)
      CALL C318M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C320)
      CALL C320M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C321)
      CALL C321M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C322)
      CALL C322M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C323)
      CALL C323M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C324)
      CALL C324M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C324)
      CALL C326M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C327)
      CALL C327M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C328)
      CALL C328M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C330)
      CALL C330M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C331)
      CALL C331M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C333)
      CALL C333M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C334)
      CALL C334M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C335)
      CALL C335M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C336)
      CALL C336M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C337)
      CALL C337M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C338)
      CALL C338M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C339)
      CALL C339M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C340)
      CALL C340M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C342)
      CALL C342M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C343)
      CALL C343M
#endif
#if (defined(CERNLIB_C344))&&(defined(CERNLIB_IBMVM)||!defined(CERNLIB_DOUBLE))
      CALL C344M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C345)
      CALL C345M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C346)
      CALL C346M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C347)
      CALL C347M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C348)
      CALL C348M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_C349)
      CALL C349M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D101)
      CALL D101M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D103)
      CALL D103M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D104)
      CALL D104M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D105)
      CALL D105M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D107)
      CALL D107M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D108)
      CALL D108M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D110)
      CALL D110M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D113)
      CALL D113M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D115)
C         OBSOLETE     CALL D115M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D200)
      CALL D200M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D201)
      CALL D201M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D202)
      CALL D202M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D203)
      CALL D203M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D401)
      CALL D401M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D503)
      CALL D503M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D601)
      CALL D601M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D705)
      CALL D705M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_D706)
      CALL D706M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_E211)
      CALL E211M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_E406)
      CALL E406M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_E407)
      CALL E407M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_E408)
      CALL E408M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_F500)
      CALL F500M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_G100)
      CALL G100M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_G101)
      CALL G101M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_G102)
      CALL G102M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_G104)
      CALL G104M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_G105)
      CALL G105M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_G106)
      CALL G106M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_G110)
      CALL G110M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_G115)
      CALL G115M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_G116)
      CALL G116M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_H101)
      CALL H101M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_H301)
      CALL H301M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_U501)
      CALL U501M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_V100)
      CALL V100M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_V101)
      CALL V101M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_V106)
      CALL V106M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_V110)
      CALL V110M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_V135)
      CALL V135M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_V136)
      CALL V136M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_V137)
      CALL V137M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_V138)
      CALL V138M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_V152)
      CALL V152M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_V202)
      CALL V202M
#endif
#if defined(CERNLIB_TESTALL)||defined(CERNLIB_V700)
      CALL V700M
#endif
C
      PRINT 1000,NTEST,NFAIL
1000  FORMAT(///1X,30('*')
     1       /' GENLIB Test statistics:'
     2       /I7,' tests executed'
     3       /I7,' tests failed  '/1X,30('*'))

#if !defined(CERNLIB_RGENT)
      IRC=ITEST('GENT',NFAIL.EQ.0)
#endif
C     Close output streams
      IF (LOUT .NE. 6) CLOSE(UNIT=LOUT)
      STOP
      END
