*
* $Id: bookh.F,v 1.1.1.1 1996/01/11 14:15:02 mclareni Exp $
*
* $Log: bookh.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:02  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BOOKH
C     ****************
C-- INTERFACES COJETS/COJTAPEW TO WRITE BEGINNING RECORD
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/bopar.inc"
#include "cojets/cutoff.inc"
#include "cojets/data1.inc"
#include "cojets/decpar.inc"
#include "cojets/dylim.inc"
#include "cojets/eicht.inc"
#include "cojets/entrev.inc"
#include "cojets/evint.inc"
#include "cojets/evtype.inc"
#include "cojets/forgen.inc"
#include "cojets/fstate.inc"
#include "cojets/idrun.inc"
#include "cojets/itapes.inc"
#include "cojets/jetlim.inc"
#include "cojets/keybre.inc"
#include "cojets/keys.inc"
#include "cojets/kfact.inc"
#include "cojets/mb.inc"
#include "cojets/nflav.inc"
#include "cojets/nleave.inc"
#include "cojets/par.inc"
#include "cojets/primar.inc"
#include "cojets/qcdpar.inc"
#include "cojets/radlep.inc"
#include "cojets/total.inc"
#include "cojets/weakbo.inc"
C
C-- /PRIMAR/
      NJET=2
      SCM=S
      ECM=SQRT(SCM)
      HALFE=.5*ECM
      IF(IPBAR.EQ.1) THEN
        IDIN(1)=IDENTF(102)
      ELSE
        IDIN(1)=IDENTF(101)
      ENDIF
      IDIN(2)=IDENTF(101)
      NEVENT=NLEAVE
      SIGTIN=SIGINL
      FACTRK=FACTK
      NFLAVR=NFLAV
C-- /FSTATE/
      KWMODE=IDECBO
      KYWOPT=KOPTWZ
      KYWQED=IRADLP
      KYHEAV=KPRHEV
      KYFLAV=KFRFLA
      KYEVOL=KEYBRE
C-- /KEYS/
      IF(IEVTYP.EQ.1) THEN
        IF(PTMGE.LE.PTMIN.AND.KPRHEV.EQ.0) THEN
          IK=4
        ELSE
          IK=1
        ENDIF
      ENDIF
      IF(IEVTYP.GE.2.AND.IEVTYP.LE.4) IK=3
      KEYS(IK)=.TRUE.
      IF(IEVTYP.EQ.2) WTYPE='Z '
      IF(IEVTYP.EQ.3) WTYPE='W '
      IF(IEVTYP.EQ.4) WTYPE='GM'
C-- /JETLIM/
      PTFFFM=PTMGE
      PTFFFX=PTMAX
C-- /DYLIM/
      QMIN=BOMSMN
      QMAX=BOMSMX
      WZMINM=WZTHRS
C-- /QCDPAR/
      ALAM=ALAMB
      ALAM2=ALAM**2
      QZEVOL=QZEV
      PTCUTF=PTMIN
      PTSQIN=PT2INT
      CUTJET=QZ
      ISTRUC=IEICHT
C
      CALL WRBEG
      RETURN
      END
