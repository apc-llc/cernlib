*
* $Id: cs2036.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: cs2036.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          25/09/95  16.04.32  by  Julian Bunn
*-- Author : V.Berezhnoi
      SUBROUTINE CS2036(LSTCL,IPCE,IPCB,NPARAM)
*****************************************
*   this routine  formed the interpreter
*   command call
*****************************************
      PARAMETER ( KCALLS=87, KCALL=92, KINP=107, KTYP=108, KPUSH=114)
***************************************************************
      PARAMETER ( KFARGL=104,KFCALL=105)
****************************************************************
      PARAMETER (   KTINT=1,  KLINT=1,  KTREAL=2, KLREAL=1,
     +   KTCHAR=3,  KTLOG=4,  KLLOG=1,  KTDOU=5,  KLCMLX=2 )
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/csdpvs.inc"
#include "comis/cskucs.inc"
#include "comis/cstvrs.inc"
      COMMON/CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
 2036 NPAR=ISEM(ITS-3)
      IB1 =ISEM(ITS-1)
      L=NPAR*3+3
      I=ITS-L-1
      IPOPT=1
      NOCHAR=0
      J=I+2
      DO 3 K=1,NPAR
        IF(ISEM(J).GT.0)THEN
          IPOPT=0
        ELSE
          ISEM(J)=-ISEM(J)
          IF(MOD(ISEM(J),100).EQ.KTCHAR)NOCHAR=NOCHAR+1
        ENDIF
 3    J=J+3
      IF(IB1.EQ.10 .OR. IB1.EQ.11) THEN
        IF(IPCE+L.GE.LAST)GO TO 727
        KOD(IPCE)=KTYP
        IF(IB1.EQ.10)  KOD(IPCE)=KINP
        KOD(IPCE+1)=ISEM(ITS-4)
        KOD(IPCE+2)=NPAR
        IPCE=IPCE+3
        J=I+1
        DO 3037 K=1,NPAR
        CALL CCOPYA(ISEM(J),KOD(IPCE),2)
        J=J+3
 3037   IPCE=IPCE+2
        GO TO 3039
      ENDIF
      IPTGP=ISEM(ITS-4)
      IF(IPTGP.LT.0 .OR. NPARAM.EQ.0)THEN
*     if formal routine or full treatment of parameters
        IPOPT=0
      ELSE
        IF(IQ(IPTGP+KSIFCS).GE.0)IPOPT=0
*       if missing or comis_routine
        CALL CSADCL(LSTCL,IPTGP)
      ENDIF
      IF(IPOPT.EQ.0)THEN
        IF(IPCE+L.GE.LAST)GO TO 727
*                          --------
        KOD(IPCE)=KCALL
        IPCC=IPCE
        KOD(IPCE+1)=IPTGP
        KOD(IPCE+2)=NPAR
        IF(NPARAM.EQ.1)THEN
          J=I+2
          IPCE=IPCE+3
          KOD(IPCC)=KCALLS
          DO 3038 K=1,NPAR
            KOD(IPCE)=ISEM(J)
            J=J+3
 3038     IPCE=IPCE+1
        ELSE
          CALL CCOPYA(ISEM(I),KOD(IPCE+3),L-3)
          IPCE=IPCE+L
        ENDIF
      ELSE
* try form optimal call to f77-routine
*          print *,' optimal call '
        IF(IPCE+6.GE.LAST)GO TO 727
        IF(NPAR.EQ.0)THEN
*+SELF,IF=-IBM.
          KOD(IPCE)=KFCALL
          KOD(IPCE+1)=IPTGP
          KOD(IPCE+2)=NPAR
          KOD(IPCE+3)=LSFT
          IPCE=IPCE+4
*+SELF.
        ELSE
          I1=ISEM(I)+IPCB
          LC=IPCE-I1
          CALL CCOPYA(KOD(I1),KOD(I1+2),LC)
          IPCE=IPCE+2
          KOD(I1)=KFARGL
          KOD(I1+1)=-(LC+2)
          CALL CSCHCD(KOD(I1+2),LC)
          IF(LC.NE.0)GO TO 709
#if defined(CERNLIB_VAX)
          LARGL=NPAR
#endif
#if defined(CERNLIB_APOLLO)
          LARGL=NPAR+NOCHAR*2
#endif
#if defined(CERNLIB_IBM)
          LARGL=NPAR*2+NOCHAR
          LSFT=LSFT+2
*+SELF,IF=SGI,IBMRT,DECS,HPUX,SUN,MSDOS.
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_STDUNIX))
          LARGL=NPAR+NOCHAR
*+SELF,IF=UNIX,IF=-SGI,IF=-IBMRT,IF=-DECS,IF=-HPUX,IF=-SUN,IF=-MSDOS.
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_STDUNIX))
          LARGL=NPAR
#endif
          KOD(IPCE)=KFCALL
          KOD(IPCE+1)=IPTGP
          KOD(IPCE+2)=NPAR*100+NOCHAR
          KOD(IPCE+3)=LSFT
          IPCE=IPCE+4
          LSFT=LSFT+LARGL
        ENDIF
      ENDIF
 3039 ISEM(I)=ISEM(ITS-2)
      ISEM(I+1)=ISEM(ITS-1)
******* CALL CCOPYA(ISEM(ITS-2),ISEM(I),2) ********
      ITS=I+2
      IF(IPCE+2.GE.LAST)GO TO 727
      KOD(IPCE)=KPUSH
      KOD(IPCE+1)=1
      IF(ISEM(I).EQ.5)KOD(IPCE+1)=KDLEN
      IF(ISEM(I).EQ.7)KOD(IPCE+1)=KLCMLX
      IPCE=IPCE+2
      RETURN
 709  IGSST=-9
      RETURN
 727  IGSST=-27
      END
