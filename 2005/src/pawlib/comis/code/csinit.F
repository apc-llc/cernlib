*
* $Id: csinit.F,v 1.11 2005/04/18 15:23:11 mclareni Exp $
*
* $Log: csinit.F,v $
* Revision 1.11  2005/04/18 15:23:11  mclareni
* Mods submitted by Kevin B. McCarty to fix insecure Cernlib uses of /tmp.
*
* Revision 1.10  2002/06/19 14:27:59  couet
* - _-PIC option removed from the list of default c compiler options on LINUX.
*
* Revision 1.9  1998/05/13 08:14:14  couet
* - Default compilation options for HPUX have been changed. With HPUX 10.20
* the option -O produce a crash when COMIS routines containing math funtionc
* are called with the f77 extension.
*
* Revision 1.8  1998/04/08 14:00:59  couet
* - VIDQQ was not used, and was initialise as a wrong "what" string.
*
* Revision 1.7  1998/01/27 08:51:23  couet
* - option -fPIC removed for LINUX g77
*
* Revision 1.6  1998/01/15 14:41:52  mclareni
* Mods for shared libraries for Linux
*
* Revision 1.5  1998/01/12 09:55:47  couet
* - Mods for LINUX (shared libraries)
*
* Revision 1.4  1997/02/14 16:22:12  dinofm
* ONLY for PIAF: if CERNLIB_SOLARIS is defined and IPIAF=1 Fortran modules
* are compiled with the -PIC option instead the -pic (larger global symbol
* table).
*
* Revision 1.3  1996/05/13 08:04:58  berejnoi
* Mods for SHL version on IBMRT
*
* Revision 1.2  1996/04/02 22:11:23  thakulin
* Add support for shared lib compilation with Apogee and EPC C and
* Fortran compilers.
*
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.19/00 07/04/95  14.21.05  by  Unknown
*-- Author : V.Berezhnoi
         SUBROUTINE CSINIT(NW)
***---------------------------
#include "comis/cslun.inc"
         COMMON/CSCONT/LCONT
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/csbuf.inc"
#include "comis/csrec.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cssysd.inc"
#include "comis/csdpvs.inc"
#include "comis/cskeys.inc"
#include "comis/cscbwl.inc"
#include "comis/csichv.inc"
#include "comis/cskucs.inc"
#if defined(CERNLIB_SHL)
#include "comis/cshlnm.inc"
#endif
#if defined(CERNLIB_PAW)
#include "paw/pcmode.inc"
#include "comis/cshfill.inc"
#endif
      COMMON /MPLIST/ INDP
         EXTERNAL MDEXP, CSMBIT
         EXTERNAL CSOFIL,CSOLOG,CSOMAP,CSLIB
         EXTERNAL CGCONT,CSCOMX,CSEXTX,CSPTFS
         EXTERNAL CSNPAR,CSKPAR,CSIPAR,CSRPAR,CSLPAR,
     +            CSTPAR,CSCPAR,CSSPAR
         CHARACTER KWORDS*22
         DATA KWORDS/'ENDIFITHENELSEIFENDDOD'/
      INDP=0
      CALL MDINIT(LHP,MDEXP)
      CALL MHDEF (LHP-6,MDEXP)
      LAST=MAX0(NW,LASTK)-2
      IBASE=0
      IPC=0
      ITA=0
      IDP=0
      IFORS=1
      ITMPLEN=0
      NCBARR=0
      NTRACE=1
      NPARAM=1
      IBSEM=MHLOC(LSSTK)
      ITS=IBSEM
      ILSEM=LSSTK+IBSEM
      LIMPL=17
      IBIMPL=MHLOC(LIMPL)
      IQ(IBIMPL)=0
      LBB=10
      NBB=0
      NBG=0
      IBB=MHLOC(LBB)-1
      IPMCAD=0
      IPGB=0
      IPGP=0
      IPGI=0
      IPLI=0
      IPLL=0
      LDATA=0
      IEXTGB=0
      LCONT=0
      NUMST=0
      JTCH=MJCHAR(IBUF(1))
      NCHS=LCSTK*NBYTPW
      JID=MJCHAR(IDEN(1))
      JTOPA=LOCF(IA(1))-1
      JPMB=0
      JPMC=0
      JMB=JTCH
      JMC=JMB
      NMC=0
      NMB=NCHS
      JFI1=MJSCHA(KWORDS)
      JFI2=JFI1+4
      JTHEN=JFI2+2
      JELSE=JTHEN+4
      JOD1=JELSE+6
      JOD2=JOD1+4
      JSR=MJSCHA(REC)
      JSR1=MJSCHA(REC1)
      JSMAIN=MJSCHA(MAINPR)
      JB4=MJCHAR(IB(4))
      PROMPT(1)='CS>'
      PROMPT(2)='FSD>'
      PROMPT(3)='MND>'
      PROMPT(4)='PAU>'
      PROMPT(5)='???>'
      KEYRD=0
      LIBRD=0
      KEYC=0
      IRECU=1-LRECU
      LUNINP=5
      ISTPM=0
      ICBWL=0
      LISTCL=0
      ICHLA=ICHAR('A')
      ICHLZ=ICHAR('Z')
      ICHD0=ICHAR('0')
      ICHD9=ICHAR('9')
      ICHSUSC=ICHAR('_')
      ICHSX=ICHAR('#')
      ICHSE=ICHAR('!')
      ICHBLN=ICHAR(' ')
      CALL UCTOH('    ',KBLN,4,4)
      ICHQUO=ICHAR('''')
      ICHSSC=ICHAR(';')
      ICHSEQ=ICHAR('=')
      ICHBRA=ICHAR('(')
      ICHKET=ICHAR(')')
      ICHMINU=ICHAR('-')
      ICHPLUS=ICHAR('+')
      ICHCOMM=ICHAR(',')
#if defined(CERNLIB_PAW)
      CALL CSPAWI
      MODHFI=0
#endif
#if !defined(CERNLIB_PAW)
      JKUVBS=-1
#endif
#if (defined(CERNLIB_HPUX))&&(defined(CERNLIB_SHL))
      ITMPLEN=CSTMPD(CHPATH, 256)
      CHF77 ='f77 -c +z +ppu'
      CHCC  ='cc -c +z'
#endif
#if (defined(CERNLIB_IBMRT))&&(defined(CERNLIB_SHL))
      ITMPLEN=CSTMPD(CHPATH, 256)
      CHF77 ='xlf -qextname -qrndsngl -qcharlen=32767 -c'
      CHCC  ='cc -c'
#endif
#if (defined(CERNLIB_SUN))&&(defined(CERNLIB_SOLARIS))&&(defined(CERNLIB_SHL))
      ITMPLEN=CSTMPD(CHPATH, 256)
# if defined(CERNLIB_QFAPOGEE)
      CHF77 ='apf77 -c -pic'
      CHCC  ='apcc -c -pic'
# else
#  if defined(CERNLIB_QFEPC)
      CHF77 ='epcf90 -c -K pic'
      CHCC  ='ecc -c -K pic'
#  else
* increase some internal tables: up to 100 nested control statements,
* 200 continuation lines, 10000 identifier names per module
#if defined(CERNLIB_PAW)
      IF (IPIAF.EQ.1) THEN
*-- PIAF requires -PIC to cope with larger selection functions
         CHF77 ='/opt/SUNWspro/bin/f77 -Nc100 -Nl200 -Nn10000 -c -PIC'
         CHCC  ='/opt/SUNWspro/bin/cc -c -K PIC'
      ELSE
         CHF77 ='/opt/SUNWspro/bin/f77 -Nc100 -Nl200 -Nn10000 -c -pic'
         CHCC  ='/opt/SUNWspro/bin/cc -c -K pic'
      ENDIF
# else
      CHF77 ='/opt/SUNWspro/bin/f77 -Nc100 -Nl200 -Nn10000 -c -pic'
      CHCC  ='/opt/SUNWspro/bin/cc -c -K pic'
#endif
#  endif
# endif
#endif
#if (defined(CERNLIB_SUN))&&(!defined(CERNLIB_SOLARIS))&&(defined(CERNLIB_SHL))
      ITMPLEN=CSTMPD(CHPATH, 256)
      CHF77 ='f77 -c -pic'
      CHCC  ='cc -c -pic'
#endif
#if (defined(CERNLIB_LINUX))
      ITMPLEN=CSTMPD(CHPATH, 256)
      CHF77 ='g77 -c'
      CHCC  ='cc -c'
#endif
#if (defined(CERNLIB_SGI))&&(defined(CERNLIB_SHL))
      ITMPLEN=CSTMPD(CHPATH, 256)
      CHF77 ='f77 -c'
      CHCC  ='cc -cckr -c'
#endif
#if (defined(CERNLIB_ALPHA_OSF))&&(defined(CERNLIB_SHL))
      ITMPLEN=CSTMPD(CHPATH, 256)
      CHF77 ='f77 -c'
      CHCC  ='cc -c'
#endif
      RETURN
      END
