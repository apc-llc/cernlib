*
* $Id: mninit.F,v 1.4 1997/09/02 15:16:08 mclareni Exp $
*
* $Log: mninit.F,v $
* Revision 1.4  1997/09/02 15:16:08  mclareni
* WINNT corrections
*
* Revision 1.3  1997/03/14 17:18:00  mclareni
* WNT mods
*
* Revision 1.2.2.1  1997/01/21 11:33:28  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.2  1996/03/15 18:02:47  james
*     Modified Files:
* mnderi.F eliminate possible division by zero
* mnexcm.F suppress print on STOP when print flag=-1
*          set FVAL3 to flag if FCN already called with IFLAG=3
* mninit.F set version 96.03
* mnlims.F remove arguments, not needed
* mnmigr.F VLEN -> LENV in debug print statement
* mnparm.F move call to MNRSET to after NPAR redefined, to zero all
* mnpsdf.F eliminate possible division by zero
* mnscan.F suppress printout when print flag =-1
* mnset.F  remove arguments in call to MNLIMS
* mnsimp.F fix CSTATU so status is PROGRESS only if new minimum
* mnvert.F eliminate possible division by zero
*
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNINIT (I1,I2,I3)
#include "minuit/d506dp.inc"
CC        This is the main initialization subroutine for MINUIT
CC     It initializes some constants in common
CC                (including the logical I/O unit nos.),
CC
#include "minuit/d506cm.inc"
C
      EXTERNAL INTRAC
      LOGICAL  INTRAC
C            I/O unit numbers
      ISYSRD = I1
      ISYSWR = I2
        ISTKWR(1) = ISYSWR
        NSTKWR = 1
      ISYSSA = I3
      NSTKRD = 0
C               version identifier
      CVRSN = '96.03 '
C               some CONSTANT constants in COMMON
      MAXINT=MNI
      MAXEXT=MNE
      UNDEFI = -54321.
      BIGEDM = 123456.
      CUNDEF = ')UNDEFINED'
      COVMES(0) = 'NO ERROR MATRIX       '
      COVMES(1) = 'ERR MATRIX APPROXIMATE'
      COVMES(2) = 'ERR MATRIX NOT POS-DEF'
      COVMES(3) = 'ERROR MATRIX ACCURATE '
C                some starting values in COMMON
      NBLOCK = 0
      ICOMND = 0
      CTITL = CUNDEF
      CFROM = 'INPUT   '
      NFCNFR = NFCN
      CSTATU= 'INITIALIZE'
      ISW(3) = 0
      ISW(4) = 0
      ISW(5) = 1
C         ISW(6)=0 for batch jobs,  =1 for interactive jobs
C                      =-1 for originally interactive temporarily batch
      ISW(6) = 0
#ifndef CERNLIB_MSSTDCALL
      IF (INTRAC(DUMMY))  ISW(6) = 1
#else
      IF (INTRAC())  ISW(6) = 1
#endif
C        DEBUG options set to default values
      DO 10 IDB= 0, MAXDBG
   10 IDBG(IDB) = 0
      LREPOR = .FALSE.
      LWARN  = .TRUE.
      LIMSET = .FALSE.
      LNEWMN = .FALSE.
      ISTRAT = 1
      ITAUR = 0
C        default page dimensions and 'new page' carriage control integer
      NPAGWD = 120
      NPAGLN = 56
      NEWPAG = 1
      IF (ISW(6) .GT. 0) THEN
         NPAGWD = 80
         NPAGLN = 30
         NEWPAG = 0
      ENDIF
      UP = 1.0
      UPDFLT = UP
C                   determine machine accuracy epsmac
      EPSTRY = 0.5
      DO 33 I= 1, 100
      EPSTRY = EPSTRY * 0.5
      EPSP1 = ONE + EPSTRY
      CALL MNTINY(EPSP1, EPSBAK)
      IF (EPSBAK .LT. EPSTRY)  GO TO 35
   33 CONTINUE
      EPSTRY = 1.0E-7
      EPSMAC = 4.0*EPSTRY
      WRITE (ISYSWR,'(A,A,E10.2)') ' MNINIT UNABLE TO DETERMINE',
     + ' ARITHMETIC PRECISION. WILL ASSUME:',EPSMAC
   35 EPSMAC = 8.0 * EPSTRY
      EPSMA2 = 2.0 * SQRT(EPSMAC)
C                 the vlims are a non-negligible distance from pi/2
C         used by MNPINT to set variables "near" the physical limits
      PIBY2 = 2.0*ATAN(1.0)
      DISTNN = 8.0*SQRT(EPSMA2)
      VLIMHI =  PIBY2 - DISTNN
      VLIMLO = -PIBY2 + DISTNN
      CALL MNCLER
      WRITE (ISYSWR,'(3A,I3,A,I3,A,E10.2)')  '  MINUIT RELEASE ',CVRSN,
     +' INITIALIZED.   DIMENSIONS ',MNE,'/',MNI,'  EPSMAC=',EPSMAC
      RETURN
      END
