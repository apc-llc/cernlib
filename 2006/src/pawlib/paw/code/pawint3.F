*
* $Id: pawint3.F,v 1.5 1999/05/20 07:37:18 couet Exp $
*
* $Log: pawint3.F,v $
* Revision 1.5  1999/05/20 07:37:18  couet
* - The flag CERNLIB_TKTCL is now removed. It was not used and just made
*   the code confusing.
*
* Revision 1.4  1996/09/12 16:01:22  couet
* - more cleaning of the old qp
*
* Revision 1.3  1996/09/09 09:28:21  couet
* - pawcuts.inc not used
*
* Revision 1.2  1996/04/04 09:09:14  couet
* The calls to KUTERM and KUGRFL have been moved into PAWINT3, after
* the IGINIT call (HIGZ initialisation).
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/19 23/11/95  10.06.08  by  Julian Bunn
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWINT3
*
*  Third level initialization routine. It initializes:
*
*     1) The HIGZ and HPLOT packages (not for PIAF).
*
*     2) The COMIS and SIGMA packages.
*
*     3) A number of different COMMON blocks.
*
#include "hbook/hcbook.inc"
#include "paw/pawnpu.inc"
#include "paw/pawbgn.inc"
#include "paw/pawchn.inc"
#include "paw/pcmask.inc"
#include "paw/pcchar.inc"
#include "paw/pachar.inc"
#include "paw/pacomis.inc"
#include "paw/pntold.inc"
#include "hbook/hcpiaf.inc"
#include "hbook/czcbuf.inc"
#include "hbook/hcminpu.inc"
#include "paw/pcwk.inc"
#include "paw/pcmode.inc"
*
      EXTERNAL CZGETC,PMNCOMD,IGTERM
#if defined(CERNLIB_SIGMA)
      EXTERNAL SIGMAE
#endif
*
*           In case of PAW++ print logo
*
      IF (IPIAF.EQ.0 .AND. IWK.EQ.999) CALL PALOGO
*
*           Initialize HIGZ
*
      IF (IPIAF.EQ.0) THEN
         NWHIGZ=10000
         CALL IGINIT(NWHIGZ)
         CALL KUTERM(IGTERM)
         CALL KUGRFL(IGTERM)
      ENDIF
*
*           Set the No-Reference bit in ZEBRA matrix
*
      CALL MZXREF(IXKU  ,IXPAWC+21,'C')
      CALL MZXREF(IXKU  ,IXPAWC+22,'C')
      CALL MZXREF(IXKU  ,IXPAWC+23,'C')
      IF (IPIAF.EQ.0) THEN
         CALL MZXREF(IXHIGZ,IXPAWC+21,'C')
         CALL MZXREF(IXHIGZ,IXPAWC+22,'C')
         CALL MZXREF(IXHIGZ,IXPAWC+23,'C')
      ENDIF
*
*          Initialize COMIS
*
      ICOMIS=0
*
*          Initialize SIGMA
*
#if defined(CERNLIB_SIGMA)
      ISIGMA=1
      CALL SIGINI1
      CALL KUSIGM(SIGMAE)
#endif
#if !defined(CERNLIB_SIGMA)
      ISIGMA=0
#endif
*
*           Initialize /PAWNPU/
*
      NPFUNC=100
      NPFUNX=20
      NPFUNY=20
      NPFUNZ=20
      RANGX1=-1.
      RANGX2=1.
      RANGY1=-1.
      RANGY2=1.
      RANGZ1=-1.
      RANGZ2=1.
      ANGLE1=30.
      ANGLE2=30.
*
*           Initialize /PACHAR/
*
      JZERO  = ICHAR('0')
      JNEUF  = ICHAR('9')
      JAMAX  = ICHAR('A')
      JZMAX  = ICHAR('Z')
      JAMIN  = ICHAR('a')
      JZMIN  = ICHAR('z')
      JDOLL  = ICHAR('$')
      JSCOR  = ICHAR('_')
*
*           Initialize /PNTOLD/
*
      NTOLD=.TRUE.
*
*           Initialize /CZCBUF/
*
      IZCBUF  = JUMPAD(CZGETC)
*
*           Initialize /HCMINPU/
*
      IADINP  = JUMPAD(PMNCOMD)
*
*           Initialize /PAWCHN/
*
      CHAIN  = .FALSE.
      CURCHN = ' '
*
*           Initialize /PCCHAR/
*
      CHUTIT = ' '
*
      IF (IPIAF.EQ.0) THEN
         CALL KUOPEN(10,'PAW.METAFILE','UNKNOWN',ISTAT)
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
         CALL HERMES(19)
         CALL VMCMS('FILEDEF 19 DISK GKSERROR OUTPUT A',IRET)
         IF(IRET.EQ.0)THEN
            OPEN(UNIT=19,STATUS='UNKNOWN')
         ENDIF
         CALL VMCMS('FILEDEF FT05F001 TERMINAL (LOWCASE',IRET)
         CALL VMCMS('FILEDEF FT06F001 TERMINAL',IRET)
#endif
      ENDIF
*
*           Initialize HPLOT
*
      IF (IPIAF.EQ.0) THEN
         CALL HPLINT(IWK)
         CALL HERMES(6)
         IF(.NOT.BATCH)CALL IGSA(0)
         CALL ISCHH(0.28)
      ENDIF
*
*           PAW statistics file
*
      CALL PALOGS(1)
*
*            Create user additional commands
*
      IF (IPIAF.EQ.0) CALL PAWUSR(1)
*
*            For Piaf server set Ntuple Memory Buffer size to 54 MB
*            (if more is desired the maxtsiz kernel parameter (HPUX)
*            should be increased)
*
      IF (IPIAF.EQ.1) CALL KUEXEC('VMEM 54')
*
      END
