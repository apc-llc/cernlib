*
* $Id: pawint2.F,v 1.4 1999/05/20 07:37:17 couet Exp $
*
* $Log: pawint2.F,v $
* Revision 1.4  1999/05/20 07:37:17  couet
* - The flag CERNLIB_TKTCL is now removed. It was not used and just made
*   the code confusing.
*
* Revision 1.3  1996/04/10 12:20:28  cernlib
* include the pcmatw common block.
*
* Revision 1.2  1996/04/04 09:09:12  couet
* The calls to KUTERM and KUGRFL have been moved into PAWINT3, after
* the IGINIT call (HIGZ initialisation).
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/19 22/11/95  18.55.12  by  Julian Bunn
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWINT2(NWPAWS,CHOPTT,PGEXI,IWTYP)
*
*  Second level PAW initialization routine. It initializes:
*
*     1) The running type of PAW: PAW, PAW++, PAW/PAW++ hybrid or PIAFS
*        Set by a CHOPT value of ' ', '+', or 'P', respectively.
*
*     2) The HBOOK (and KUIP, if used) packages.
*
*     3) The command structure.
*
*     4) In case of PAW or PAW/PAW++ hybrid, request workstation type of
*        user.
*
*  It returns the workstation type, IWTYP.
*
#include "hbook/hcbook.inc"
#include "hbook/hcmail.inc"
#include "paw/pawcom.inc"
#include "paw/pawlun.inc"
#include "paw/pcmask.inc"
#include "paw/pccsel.inc"
#include "hbook/czsock.inc"
#include "paw/pcslas.inc"
#include "paw/pcaddr.inc"
#include "paw/pacomis.inc"
#include "hbook/hcpiaf.inc"
#include "hbook/hcminpu.inc"
#include "paw/pcwk.inc"
#include "paw/pcmode.inc"
#include "paw/pcargs.inc"
* although not needed here, it should be loaded to avoid picking up a random 
*  object file in qp_execute to resolve a reference to pcmatw_ on Solaris
#include "paw/pcmatw.inc"
*
      CHARACTER*(*) CHOPTT
      CHARACTER*80 ARGS
      CHARACTER*8  CHOPT
      CHARACTER*6  CHXXX
*
      EXTERNAL PGEXI, PAWBRK
*
      CHOPT = CHOPTT
      CALL CLTOU(CHOPT)
*
      IPIAF  = 0
      IPAWPP = 0
      IQMARK = INDEX(CHOPT,'?')
      IF (INDEX(CHOPT,'P') .NE. 0) IPIAF  = 1
      IF (INDEX(CHOPT,'+') .NE. 0) IPAWPP = 1
*
*-- if batch option specified do not start Motif mode
*
      IF (IPAWPP.NE.0 .AND. BATCH) IPAWPP = 0
*
#if !defined(CERNLIB_BSLASH)
      BSLASH='\'
#endif
#if defined(CERNLIB_BSLASH)
      BSLASH='\\'
#endif
*
      NWPAWC = IABS(NWPAWS)
      CHMAIL = ' '
      LMASK=0
      IDOLD=0
      JOFSET=0
      CALL VZERO(LUNIT,128)
      LUNIT(5)=6
      LUNIT(6)=6
      LUNIT(7)=6
      LUNIT(10)=8
      DO 5 I=11,18
         LUNIT(I)=6
   5  CONTINUE
      LUNIT(19)=8
      LUNIT(81)=7
      LUNIT(82)=7
      LUNIT(83)=7
      LUNIT(84)=7
      LUNIT(89)=7
      LUNIT(91)=8
      LUNIT(97)=7
      CALL VZERO(IOPTCS,5)
      CSIZE=0.28
      CSOPT=' '
*
*           Initialize jump addresses for non-Motif version of routines
*
      JMPANT  = 0
      JMLAB   = 0
      JMROW   = 0
      JMCHAIN = 0
      JNTWINI = 0
      JNTWCHK = 0
      JMHIST  = 0
*
*           Initialize HBOOK
*
      CALL HLIMIT(NWPAWS)
*
*           Reserve 5000 words for working space (KUIP TEMP vectors)
*
      CALL MZWORK(IXPAWC,LQ(50),LQ(5000),0)
*
*           Initialize KUIP
*
      NWKUIP = 5000
      CALL KUINIT(NWKUIP)
*
      IF (IPIAF.EQ.0) THEN
         CALL KUEXIT(PGEXI)
         CALL KUBREK(PAWBRK)
*
*           Create command structure from definition file
*
         CALL PAWDEF
*
         CALL PFINIT
         IADTCP = 0
         ISKIN  = 0
         IPROT  = 0
      ELSE
*
*           Create limited command structure for Piaf
*
         CALL VECDEF
         CALL HISDEF
         CALL NTUDEF
         CALL ZEBDEF
         CALL FORDEF
*
         SERVPF = .TRUE.
         CLINPF = ISKIN
         CLUTPF = ISKOUT
      ENDIF
*
*           Request workstation type
*
      IF(IPIAF.NE.0)GO TO 20
      ARGS=CHARGS
      CALL CLTOU(ARGS)
      NCH=LENOCC(ARGS)
      IW=INDEX(ARGS,'-W')
*-*  Get workstation type from program line parameters -W
*-*  if -W xxx  users wants workstation type xxx: No prompt
*-*  if -W      users wants default workstation type: No prompt
      CALL IGDWK(IWK)
      IF(IW.NE.0)THEN
         IW1=IW+3
         IXXX=0
         IF(IW1.LE.NCH)THEN
            DO 10 I=IW1,NCH
               IF(ARGS(I:I).EQ.' ')GO TO 10
               IF(INDEX('0123456789',ARGS(I:I)).EQ.0)GO TO 15
               IW2=I
               DO 6 J=I+1,NCH
                  IF(ARGS(J:J).EQ.' ')THEN
                     IW2=J-1
                     GO TO 7
                  ENDIF
                  IF(INDEX('0123456789',ARGS(J:J)).EQ.0)GO TO 15
   6           CONTINUE
               IW2=NCH
   7           IF(IW2-I.GT.5)GO TO 15
               CHXXX=' '
               CHXXX(6+I-IW2:)=ARGS(I:IW2)
               READ(CHXXX,'(I6)')IWK
               GO TO 15
  10        CONTINUE
         ENDIF
*
  15     IF(IPAWPP.NE.0.AND.IWK.NE.999)IPAWPP=0
         IF(IPAWPP.EQ.0)CALL PALOGO
      ELSE
         IF (IPAWPP.NE.0.AND.IQMARK.EQ.0) THEN
            IWK = 999
         ELSEIF (BATCH) THEN
            IWK = 0
         ELSE
            CALL PALOGO
            CALL IGWKTY(IWK)
         ENDIF
      ENDIF
*
      IF (IPAWPP.EQ.0 .AND. IWK.EQ.999) THEN
         PRINT *, '*** Illegal workstation type 999, using default'
         IWK=1
         CALL IGDWK(IWK)
      ENDIF
  20  IWTYP = IWK
*
      END
