*
* $Id: hcx.F,v 1.7 1998/12/02 09:05:17 couet Exp $
*
* $Log: hcx.F,v $
* Revision 1.7  1998/12/02 09:05:17  couet
* - clean up
*
* Revision 1.6  1998/10/05 14:39:00  couet
* - New option P for profile histograms. Implemented by:  Nello Nappi
*   <Nello.Nappi@cern.ch>
*
* Revision 1.5  1998/06/11 13:27:14  couet
* - mod from Kees.van.der.Poel@nikhef.nl
*
* Revision 1.4  1998/06/10 15:38:24  couet
* - bug fixed for option I (Kees van der Poel <keesp@nikhef.nl>)
*
* Revision 1.3  1997/01/10 17:49:04  couet
* - CONT and ERR2 need to be in double precision
*
* Revision 1.2  1996/11/18 13:50:09  couet
* - Bug fixed in the computation of the errors for profile histograms.
*   the bug has been found by Stephane Coutu (coutu@pooh.physics.lsa.umich.edu)
*   and fixed by Olivier Couet. The following macro shows the problem:
*
* v/create xv(3) r 0.2 0.4 1.5
* v/create yv(3) r 0.8 1.2 1.1
* v/write xv,yv test.dat '2(1x,f5.2)'
* nt/create 200 'Test ntuple' 2 ! ! x y
* nt/read 200 test.dat '2(1x,f5.2)'
* profile 201 'Test profile histogram 1' 2 0. 2. 0. 2.
* nt/pro 201 200.y%x
* v/create con1(2) r
* v/create err1(2) r
* h/get_vect/con 201 con1
* h/get_vect/err 201 err1
* v/print con1
* v/print err1
*
*   The output should be:
*
*  CON1(1) = 1
*  CON1(2) = 1.1
*  ERR1(1) = 0.141421
*  ERR1(2) = 1.04881 <=====
*
*   and not:
*
*  CON1(1) = 1
*  CON1(2) = 1.1
*  ERR1(1) = 0.141421
*  ERR1(2) = 0.000119604 <======
*
* Revision 1.1.1.1  1996/01/16 17:07:34  mclareni
* First import
*
*
#include "hbook/pilot.h"
      FUNCTION HCX(ICX,IOPT)
*.==========>
*.           IOPT  =1 HCX= CONTENT OF CHANNEL ICX
*.           IOPT  =2 HCX= ERROR CORRESPONDING TO CHANNEL ICX
*.           IOPT  =3 HCX= FUNCTION CORRESPONDING TO CHANNEL ICX
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
      DOUBLE PRECISION CONT,ERR2,SUM,EPRIM
*.___________________________________________
#include "hbook/jbyt.inc"
C
C RETURN THE CONTENTS FOR IOPT=1, OR PRE-CALCULATE THE CONTENTS
C IF IOPT=2 AND THERE ARE NO PRE-PACKED ERRORS
C
      HCX = 0.0
      LW = LQ(LCONT)
C
      IF(IOPT.EQ.1.OR.(IOPT.EQ.2.AND.LW.EQ.0)) THEN
         IF(NB.GE.32)THEN
            HCX = Q(LCONT+KCON1+ICX)
            IF(LW.NE.0)THEN
               IF(LQ(LW).NE.0)THEN
                  LN=LQ(LW)
                  IF(ICX.LE.0.OR.ICX.GT.IQ(LN-1)) THEN
                     HCX = 0.0
                     GOTO 1
                  ENDIF
                  SUM=Q(LN+ICX)
                  IF(SUM.NE.0.) HCX = HCX/SUM
               ENDIF
            ENDIF
         ELSE
            L1=ICX*NB
            NBITH=32-MOD(32,NB)
            L2=MOD(L1,NBITH)+1
            L1=LCONT+KCON1+L1/NBITH
            HCX = JBYT(IQ(L1),L2,NB)
         ENDIF
   1     IF(IOPT.EQ.1) RETURN
      ENDIF
*
      IF(IOPT.EQ.2) THEN
C
C IF NO PRE-PACKED ERRORS, THEN ERROR IS SQRT CONTENTS
C
         IF(LW.EQ.0) THEN
            HCX = SQRT(ABS(HCX))
            RETURN
         ENDIF
C
         IF(LQ(LW).EQ.0)THEN
C
C ERRORS ARE AVAILABLE IN THE BANK
C
            HCX=SQRT(Q(LW+ICX))
            RETURN
         ELSE
C
C  Profile histogram. See HBPROF for explanation of options
C
            IOPTS=JBYT(IQ(LW),1,2)
            LN=LQ(LW)
            CONT=Q(LCONT+KCON1+ICX)
            ERR2=Q(LW+ICX)
            SUMP=ABS(Q(LN+ICX))
            IF(SUMP.NE.0.)THEN
               IF(JBIT(IQ(LW),3).EQ.0)THEN
                  EPRIM=SQRT(ABS(ERR2/SUMP - (CONT/SUMP)**2))
               ELSE
                  EPRIM=SQRT(ABS(ERR2/SUMP))
               ENDIF                         
               IF(EPRIM.LE.0..AND.SUMP.GE.1.)THEN
                  IF(IOPTS.EQ.2)THEN
*
*   Mod from Kees.van.der.Poel@nikhef.nl
*   The previous version was:
*                       EPRIM=1./SQRT(12.*SUMP)
*
                     EPRIM=1./SQRT(12.)
*
                  ELSE
                     EPRIM=SQRT(ABS(CONT))
                  ENDIF
               ENDIF
               IF(IOPTS.EQ.0)THEN
                  HCX=EPRIM/SQRT(SUMP)
               ELSEIF(IOPTS.EQ.1)THEN
                  HCX=EPRIM
               ELSE
                  HCX=EPRIM/SQRT(SUMP)
               ENDIF
            ENDIF
            RETURN
         ENDIF
      ELSE IF(IOPT.EQ.3) THEN
C
C GET THE VALUE OF THE FUNCTION ASSOCIATED WITH THE HISTOGRAM
C
         LFUNC=LQ(LCONT-1)
         IC1=IQ(LFUNC+1)
         IF(ICX.GE.IC1.AND.ICX.LE.IQ(LFUNC+2))THEN
            HCX=Q(LFUNC+ICX-IC1+3)
         ENDIF
      ELSE
C
C AN INVALID OPTION VALUE WAS GIVEN
C
         CALL HBUG('+Error in option value','HCX',IOPT)
      ENDIF
      END
