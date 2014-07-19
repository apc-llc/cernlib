*
* $Id: pawcs.F,v 1.36 2003/01/31 09:30:29 couet Exp $
*
* $Log: pawcs.F,v $
* Revision 1.36  2003/01/31 09:30:29  couet
* - New routine added: RFACT RINV FINT RFERD
*
* Revision 1.35  2002/09/19 15:44:32  couet
* - change to force the loading of c_div (complex divide) for RH6.1
*
* Revision 1.34  2002/09/03 15:26:25  couet
* - added: DBESJ0,DBESJ1,DBESY0,DBESY1,DBESI0,DBESI1,DBESK0,DBESK1
*          DEBSI0,DEBSI1,DEBSK0,DEBSK1
*
* Revision 1.33  2000/12/19 16:44:40  couet
* - HF1N added
*
* Revision 1.32  2000/10/23 16:27:41  couet
* - DIVDIF added
*
* Revision 1.31  2000/07/24 14:53:32  couet
* - New dummy function PALDCF using DOUBLE COMPLEX functions to force their
*   loading in the PAW module to make them available in COMIS functions via
*   dynamic linking.
*
* Revision 1.30  2000/06/15 13:52:06  couet
* - added COMMON HCDIRN, HCDIRC, and HCFILE to be able to check which HBOOK
*   files are curently attached.
*
* Revision 1.29  2000/05/03 13:49:14  couet
* - REXPIN added
*
* Revision 1.28  2000/03/03 13:54:06  couet
* - RLUXGO added
*
* Revision 1.27  2000/02/24 17:03:28  couet
* - common HCFLAG added
*
* Revision 1.26  2000/01/28 15:01:09  couet
* - RRTEQ3 change to DRTEQ3
*
* Revision 1.25  2000/01/27 16:23:47  couet
* - RRTEQ3 added
*
* Revision 1.24  1999/10/29 15:09:49  couet
* - ALGAMA added
*
* Revision 1.23  1999/10/04 09:37:35  couet
* - new routines
*
* Revision 1.22  1999/09/15 09:55:26  couet
* - DLGAMA added
*
* Revision 1.21  1999/08/31 08:12:44  couet
* - added VAVDEN and VAVSET
*
* Revision 1.20  1998/12/08 09:12:51  couet
* - Some COMMON blocks were not listed in the help of the command CALL
*
* Revision 1.19  1998/11/30 09:28:31  couet
* - MNSTAT declared callable
*
* Revision 1.18  1998/04/07 07:57:02  couet
* - ITX3 added
*
* Revision 1.17  1997/11/18 13:40:15  couet
* - New routine DCAUCH
*
* Revision 1.16  1997/10/23 13:00:20  mclareni
* NT mods
*
* Revision 1.15  1997/09/02 15:50:57  mclareni
* WINNT corrections
*
* Revision 1.13  1997/07/11 13:40:17  couet
* - IGIWTY added
*
* Revision 1.12  1997/05/29 14:17:21  couet
* - IFA3 added
*
* Revision 1.11  1997/05/13 16:03:05  couet
* - CSEXT called with wrong number of parameters
*
* Revision 1.10  1997/05/13 16:00:00  couet
* - new routines:
*   VMOD,VDIST,VDOTN2,VSUB,VUNIT,CHISIN,VDOT,VADD,VSCALE,CROSS
*
* Revision 1.9  1997/05/13 15:52:49  couet
* *** empty log message ***
*
* Revision 1.8  1997/04/21 07:17:45  couet
*  Nes routine callbale frm comis: RANMAR, RANLUX, RNORML, RNORMX
*
* Summary:                       Obsolete      Faster       Safer
*                                --------      ------       -----
*              Uniform numbers:    RNDM        RANMAR       RANLUX
*              Gaussian numbers:  RANNOR       RNORML       RNORMX
*
* Revision 1.7  1997/03/17 13:24:49  couet
* - HXE was not declared in the HELP
* - HXE was declared as a subroutine but it is a function
* - IGQ and IGQWK were missing
* - HIJE was declared as a subroutine but it is a function
* - Reformating to make the reading easier
*
* Revision 1.6  1997/03/14 14:05:24  mclareni
* WNT mods
*
* Revision 1.5  1997/02/05 08:12:15  couet
* - HXE added
*
* Revision 1.4  1996/10/23 14:15:53  couet
* - igqwk and igq added
*
* Revision 1.3  1996/03/22 16:22:00  couet
* add HF1E
*
* Revision 1.2  1996/03/04 16:42:15  couet
* Character pointer used in CSOMC
*
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_COMIS)
      SUBROUTINE PAWCS

******************************************************************
*                                                                *
*        To initialize the COMIS package                         *
*        To declare addresses of FORTRAN routines and COMMONs    *
*        which may be invoked from COMIS routines                *
*        (one can call CSOMAP instead)                           *
*                                                                *
******************************************************************

#include "paw/pawbgn.inc"
#include "paw/pawidn.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcntpaw.inc"
#include "paw/pawchn.inc"
#include "hbook/hcfits.inc"
#include "hbook/hcfitd.inc"
#include "hbook/hcdire.inc"
#include "hbook/hcpiaf.inc"
#include "paw/pacomis.inc"
#include "paw/quest.inc"
      common/rzcount/rzcount(2)
      common/rzclun/lunrz,rzcl(10)
      COMMON/PAWC/PAW(1)
      COMMON/PAWPAR/DPAR(100)
      COMMON/KCWORK/WORKKC(100)
      COMMON/HCBOOK/HCB(26)
      COMMON/HIGRAF/NGRAF,XGRAF(500),YGRAF(500)
      COMMON/HCFLAG/IIDD(11)
      DOUBLE PRECISION DPAR
      DIMENSION P(1)
      CHARACTER*1 CP(1)
*

#ifdef CERNLIB_MSSTDCALL

	INTERFACE
	SUBROUTINE TIMED(int1)
        !DEC$ ATTRIBUTES STDCALL,ALIAS:"_TIMED@4"::TIMED
	INTEGER int1
	END

	SUBROUTINE KUGUID(ch1,ch2,int1,ch3)
        !DEC$ ATTRIBUTES STDCALL,ALIAS:"_KUGUID@28"::KUGUID
	CHARACTER*(*) ch1,ch2,ch3
     	INTEGER int1
	END

	SUBROUTINE KUPAR(ch1,ch2,ch3,ch4,ch5)
        !DEC$ ATTRIBUTES STDCALL,ALIAS:"_KUPAR@40"::KUPAR
	CHARACTER*(*) ch1,ch2,ch3,ch4,ch5
	END

	SUBROUTINE KUPVAL(ch1,ch2,int1,flt1,ch3,ch4)
        !DEC$ ATTRIBUTES STDCALL,ALIAS:"_KUPVAL@40"::KUPVAL
	CHARACTER*(*) ch1,ch2,ch3,ch4
	REAL flt1
	INTEGER int1
	END
 
	SUBROUTINE KUACT(ch1,flt1)
        !DEC$ ATTRIBUTES STDCALL,ALIAS:"_KUACT@12"::KUACT
	REAL flt1
	CHARACTER*(*) ch1
	END

	SUBROUTINE KUTIME()
        !DEC$ ATTRIBUTES STDCALL,ALIAS:"_KUTIME@0"::KUTIME
	END

	SUBROUTINE KUEXEL(ch1)
        !DEC$ ATTRIBUTES STDCALL,ALIAS:"_KUEXEL@8"::KUEXEL
	CHARACTER*(*) ch1
	END

	SUBROUTINE KUPROS(ch1,ch2,int1)
        !DEC$ ATTRIBUTES STDCALL,ALIAS:"_KUPROS@20"::KUPROS
	CHARACTER*(*) ch1,ch2
	INTEGER int1
	END

	SUBROUTINE  KUNWG(int1)
        !DEC$ ATTRIBUTES STDCALL,ALIAS:"_KUNWG@4"::KUNWG
	INTEGER int1
	END

	SUBROUTINE  KUCMD(ch1,ch2,ch3)
        !DEC$ ATTRIBUTES STDCALL,ALIAS:"_KUCMD@24"::KUCMD
	CHARACTER*(*) ch1,ch2,ch3
	END
	END INTERFACE
#endif

      EXTERNAL HBOOK1,HBOOK2,HBOOKN,HFILL,HF1,HF1E,HPRINT,HDELET,HRESET
      EXTERNAL HFITHN,HFITGA,HFITPO,HFITEX,HPROJ1,HPROJ2,HFN,HGFIT,HXE
      EXTERNAL HROPEN,PAOPEN,PACLOS,PAREAD,PAWRIT,HCDIR,HGIVEN,HF1N
      EXTERNAL HBFUN1,HBFUN2,HRNDM1,HRNDM2,HBARX,HBARY,HBAR2
      EXTERNAL HPAK,HPAKE,HUNPAK,HGIVE,HGN,HGNF,HGNPAR,HF2,HFF1,HFF2
      EXTERNAL HRIN,HROUT,HI,HIE,HIX,HIJ,HIF,HIDALL,HRDIR,HNOENT,HX,HXY
      EXTERNAL HTITLE,HCOPY,HSTATI,HBPROF,HOPERA,HIDOPT,HDERIV
      EXTERNAL HMAXIM,HMINIM,HMAX,HMIN,HSUM,HNORMA,HREND,HRENID
      EXTERNAL HEXIST,HRGET,HRPUT,HSCR,HFIND,HCX,HCXY,HLABEL
      EXTERNAL HBPROX,HBPROY,HBANDX,HBANDY,HBSLIX,HBSLIY
      EXTERNAL HBOOKB,HBSTAT,HDIFF,HUNPKE,HREBIN,HERROR,HPROF2
      EXTERNAL HOUTPU,HERMES,HISTDO,HFUNC,HXI,HIJXY,HXYIJ
      EXTERNAL HSPLI1,HSPLI2,HMDIR,HLDIR,HLOCAT,HFITH,HFITV,HFINAM
      EXTERNAL HBNT,HBNAME,HBNAMC,HFNT,HFNTB,HGNT,HGNTF,HGNTV,HBSET
      EXTERNAL HGNTB,HNBENT,HVXIST,HLPOS,HFC1,HSTAF,HKIND
      EXTERNAL HMINUT,HDIFFB,HRENAME,HNTDUP,HIJE
      EXTERNAL HMCINI,HMCMLL
      EXTERNAL MZSTOR,MZDIV,MZLINK,MZWORK,MZBOOK,MZDROP,MZPUSH,MZWIPE
      EXTERNAL MZGARB,MZFORM,LZFIND,LZFID
      EXTERNAL DZSHOW,DZVERI
      EXTERNAL FZIN,FZOUT,FZFILE,FZENDI,FZENDO,MZLOGL
      EXTERNAL HPLOT,HPLSYM,HPLERR,HPLEGO,HPLNT,HPLSUR,HPLSOF,HPLFRA
      EXTERNAL HPLABL,HPLSET,HPLGIV,HPLOC,HPLTOC,HPLNEW,HPLOPT
      EXTERNAL HPLAX,HPLFR3
      EXTERNAL MNEMAT,MNERRS,MNSTAT
      EXTERNAL KUGETV,KUDPAR,KUVECT,KILEXP,KUNDPV
#ifndef CERNLIB_MSSTDCALL
      EXTERNAL KUEXEL,KUPROS,KUPVAL,KUNWG,KUGUID,KUTIME
      EXTERNAL KUCMD,KUPAR,KUACT
#endif
      EXTERNAL IPL,IPM,IFA,IGTEXT,IGBOX,IGAXIS,IGPIE,IGRAPH,IGHIST
      EXTERNAL IGARC,IGLBL,IGRNG,IGMETA,IGSA,IGSET,IRQLC,IRQST,ISCR
      EXTERNAL ISELNT,ISFAIS,ISFASI,ISLN,ISMK,ISVP,ISWN,ITX,ITX3,ICLRWK
      EXTERNAL IGPAVE,IGTERM,IPL3,IPM3,ISMKSC,ISPMCI,ISFACI,IGCOLM
      EXTERNAL IGHTOR,IGQ,IGQWK,IFA3,IGIWTY,ISWN3,ISVP3
      EXTERNAL RZCDIR,RZLDIR,RZFILE,RZEND,RZIN,RZOUT,RZVIN,RZVOUT
      EXTERNAL RZOPEN,RZIODO,RZCLOS,RZQUOT
      EXTERNAL VZERO,UCOPY,RANNOR,LENOCC,CLTOU,CUTOL,RLUXGO,REXPIN
      EXTERNAL RANMAR,RNORML,RANLUX,RNORMX,VAVDEN,VAVSET
#ifndef CERNLIB_MSSTDCALL
      EXTERNAL TIMED
#endif
      EXTERNAL SBIT0,SBIT1,SBYT,JBIT,JBYT,UCTOH,UHTOC,PROB,FREQ
      EXTERNAL DENLAN,DSTLAN,DIFLAN,XM1LAN,XM2LAN,RANLAN
      EXTERNAL RNDM,RDMIN,RDMOUT,SORTZV
      EXTERNAL CHISIN,VDOT,VADD,VSCALE,CROSS
      EXTERNAL VMOD,VDIST,VDOTN2,VSUB,VUNIT
      EXTERNAL DBESJ0,DBESJ1,DBESY0,DBESY1,DBESI0,DBESI1,DBESK0,DBESK1
      EXTERNAL DEBSI0,DEBSI1,DEBSK0,DEBSK1
      EXTERNAL BESJ0,BESJ1,BESY0,BESY1,BESI0,BESI1,BESK0,BESK1
      EXTERNAL EBESI0,EBESI1,EBESK0,EBESK1,DCAUCH,DRTEQ3,DIVDIF
      EXTERNAL RFACT,RINV,FINT,RFERDR
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_WINNT))
      EXTERNAL CSF77,CSHEXT,CSHFAST,HKFILL,HKF1Q
      EXTERNAL JUMPT0,JUMPT1,JUMPT2,JUMPT3,JUMPT4,JUMPC5
#endif
      EXTERNAL DLGAMA,ALGAMA
#if !defined(CERNLIB_IBM)
      EXTERNAL ERF,ERFC,GAMMA
#endif
#if defined(CERNLIB_IBM)
      INTRINSIC ERF,ERFC,GAMMA
#endif
*
*------------------------------------------------------------------
*
      IF(ICOMIS.NE.0)RETURN
      ICOMIS=1
#if !defined(CERNLIB_NEWLIB)
      LUNPM =81
      LUNFIL=82
      LUNLOG=83
      LUNMAP=84
      LUNEDT=85
#endif
#if defined(CERNLIB_NEWLIB)
      LUNPM =61
      LUNFIL=62
      LUNLOG=63
      LUNMAP=64
      LUNEDT=65
#endif
*
      P(1) = 0.
      CP(1) = ' '
*
      CALL CSSETL(LUNPM,LUNFIL,LUNLOG,LUNMAP,LUNEDT)
      CALL CSINIT(NCOMIS)
      IF (SERVPF) CALL CSHLOPT('.', 'path')
*
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_WINNT))
      CALL CSEXT(
     +  'JUMPT0,JUMPT1,JUMPT2,JUMPT3,JUMPT4,JUMPC5#'
     +,  JUMPT0,JUMPT1,JUMPT2,JUMPT3,JUMPT4,JUMPC5
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'CSF77,CSHEXT,CSHFAST,HKFILL,HKF1Q#'
     +,  CSF77,CSHEXT,CSHFAST,HKFILL,HKF1Q
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
#endif
*
*  Common blocks declarations
*
      CALL CSCOM(
     +  'PAWC,PAWPAR,PAWIDN.W,KCWORK,QUEST ,HCBOOK,HIGRAF,HCFLAG#'
     +,  PAW ,DPAR  ,IDNEVT  ,WORKKC,IQUEST,HCB   ,NGRAF ,IIDD
     +,  P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSCOM(
     +  'PAWCHN,PAWCR8.W,PAWCR4.W#'
     +,  CHAIN ,RVAR8   ,RVAR4
     +,  P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSCOM(
     +  'HCFITS,HCFITD,RZCOUNT,RZCLUN,HCDIRN,HCDIRC,HCFILE#'
     +,  NCFITS,FITPAD,RZCOUNT,LUNRZ ,NLCDIR,CHCDIR,HFNAME
     +,  P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSCOMC(
     +  'PAWCHC,PAWC32.W#'
     +,  CFILE ,CVAR32
     +,  CP,CP,CP,CP,CP,CP,CP,CP)
*
*  Subroutines and functions declarations
*
*-------  1  2  3  4  5  6  7  8  9
      CALL CSEXT(
     +  'HBOOK1.S,HBOOK2.S,HBOOKN.S,HFILL.S,HF1.S,HF1E.S,HPRINT.S#'
     +,  HBOOK1,  HBOOK2,  HBOOKN,  HFILL,  HF1,  HF1E,  HPRINT
     +,  P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HDELET.S,HRESET.S,HF1N.S#'
     +,  HDELET,  HRESET,  HF1N
     +,  P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HFITGA.S,HFITPO.S,HFITEX.S,HPROJ1.S,HPROJ2.S,HFN.S,HGNPAR.S#'
     +,  HFITGA,  HFITPO,  HFITEX,  HPROJ1,  HPROJ2,  HFN,  HGNPAR
     +,  P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HBAR2.S,HFITHN.S#'
     +,  HBAR2,  HFITHN
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HROPEN.S,PAOPEN.S,PACLOS.S,PAREAD.S,PAWRIT.S,HCDIR.S#'
     +,  HROPEN,  PAOPEN,  PACLOS,  PAREAD,  PAWRIT,  HCDIR  
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HGIVEN.S#'
     +,  HGIVEN
     +,  P,P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HPAK.S,HPAKE.S,HUNPAK.S,HGIVE.S,HGN.S,HGNF.S,HF2.S#'
     +,  HPAK,  HPAKE,  HUNPAK,  HGIVE,  HGN,  HGNF,  HF2 
     +,  P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HFF1.S,HFF2.S#'
     +,  HFF1,  HFF2
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HMINUT.S,HDIFFB#.S'
     +,  HMINUT  ,HDIFFB
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HMAXIM.S,HMINIM.S,HMAX.R,HMIN.R,HSUM.R,HNORMA.S#'
     +,  HMAXIM,  HMINIM,  HMAX,  HMIN,  HSUM, HNORMA
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HREND.S#'
     +,  HREND
     +,  P,P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HI.R,HIE.R,HIX.S,HIJ.R,HIF.R,HIDALL.S,HNOENT.S,HX.R,HXY.R#'
     +,  HI,  HIE,  HIX,  HIJ,  HIF,  HIDALL,  HNOENT,  HX,  HXY
     +,  P )
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HRDIR.S#'
     +,  HRDIR
     +,  P,P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HRIN.S,HROUT.S,HCOPY.S,HBPROF.S,HOPERA.S,HIDOPT.S#'
     +,  HRIN,  HROUT,  HCOPY,  HBPROF,  HOPERA,  HIDOPT  
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HDERIV.S,HGFIT.S,HXE.R#'
     +,  HDERIV,  HGFIT,  HXE
     +,  P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HEXIST.L,HRGET.S,HRPUT.S,HSCR.S,HFIND.S,HCX.R#'
     +,  HEXIST,  HRGET,  HRPUT,  HSCR,  HFIND,  HCX
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HCXY.R,HLABEL.S#'
     +,  HCXY,  HLABEL
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HBPROX.S,HBPROY.S,HBANDX.S,HBANDY.S,HBSLIX.S#'
     +,  HBPROX,  HBPROY,  HBANDX,  HBANDY,  HBSLIX
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HBSLIY.S,HRENID.S#'
     +,  HBSLIY,  HRENID
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HBOOKB.S,HBSTAT.S,HDIFF.S,HUNPKE.S,HREBIN.S#'
     +,  HBOOKB,  HBSTAT,  HDIFF,  HUNPKE,  HREBIN  
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HERROR.S,HPROF2.S#'
     +,  HERROR,  HPROF2
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HRENAME.S,HNTDUP.S,HIJE.R#'
     +,  HRENAME,  HNTDUP,  HIJE
     +,  P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HOUTPU.S,HERMES.S,HISTDO.S,HFUNC.S,HXI.S,HIJXY.S#'
     +,  HOUTPU,  HERMES,  HISTDO,  HFUNC,  HXI,  HIJXY
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HXYIJ.S,HFINAM.S#'
     +,  HXYIJ,  HFINAM
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HSTATI.R,HLPOS.S,HFC1.S#'
     +,  HSTATI,  HLPOS,  HFC1
     +,  P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HSPLI1.S,HSPLI2.S,HMDIR.S,HLDIR.S,HLOCAT.S,HFITH.S#'
     +,  HSPLI1,  HSPLI2,  HMDIR,  HLDIR,  HLOCAT,  HFITH
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HFITV.S,HKIND.S#'
     +,  HFITV,  HKIND
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'MZSTOR.S,MZDIV.S,MZLINK.S,MZWORK.S,MZBOOK.S#'
     +,  MZSTOR,  MZDIV,  MZLINK,  MZWORK,  MZBOOK
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'MZDROP.S,MZPUSH.S#'
     +,  MZDROP,  MZPUSH
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'MZWIPE.S,MZGARB.S,MZFORM.S,LZFIND.S,LZFID.S#'
     +,  MZWIPE  ,MZGARB  ,MZFORM  ,LZFIND  ,LZFID
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'DZSHOW.S,DZVERI.S#'
     +,  DZSHOW  ,DZVERI
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'FZIN.S,FZOUT.S,FZFILE.S,FZENDI.S,FZENDO.S,MZLOGL.S#'
     +,  FZIN,  FZOUT,  FZFILE,  FZENDI,  FZENDO,  MZLOGL
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HTITLE.S,HBFUN1.S,HBFUN2.S,HRNDM1.R,HRNDM2.S#'
     +,  HTITLE,  HBFUN1,  HBFUN2,  HRNDM1,  HRNDM2
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HBARX.S,HBARY.S#'
     +,  HBARX,  HBARY
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HPLOT.S,HPLSYM.S,HPLERR.S,HPLEGO.S#'
     +,  HPLOT  ,HPLSYM  ,HPLERR  ,HPLEGO
     +,  P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HPLNT.S,HPLSUR.S,HPLSOF.S,HPLFRA.S#'
     +,  HPLNT  ,HPLSUR  ,HPLSOF  ,HPLFRA
     +,  P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HPLAX.S,HPLFR3.S#'
     +,  HPLAX  ,HPLFR3
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HPLABL.S,HPLSET.S,HPLGIV.S,HPLOC.S,HPLTOC.S#'
     +,  HPLABL  ,HPLSET  ,HPLGIV  ,HPLOC  ,HPLTOC
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HPLNEW.S,HPLOPT.S#'
     +,  HPLNEW  ,HPLOPT
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HBNT.S,HBNAME.S,HBNAMC.S,HFNT.S,HFNTB.S,HGNT.S#'
     +,  HBNT,  HBNAME,  HBNAMC,  HFNT,  HFNTB,  HGNT
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HGNTF.S,HGNTV.S,HBSET.S#'
     +,  HGNTF,  HGNTV,  HBSET
     +,  P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HGNTB.S,HNBENT.S,HVXIST.S,HSTAF.S#'
     +,  HGNTB,  HNBENT,  HVXIST,  HSTAF
     +,  P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'HMCINI.S,HMCMLL.S#'
     +,  HMCINI  ,HMCMLL
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'MNEMAT.S,MNERRS.S,MNSTAT.S#'
     +,  MNEMAT  ,MNERRS  ,MNSTAT
     +,  P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'KUGETV.S,KUDPAR.S,KUVECT.S,KILEXP.S,KUTIME.S#'
     +,  KUGETV,  KUDPAR,  KUVECT,  KILEXP,  KUTIME
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'KUEXEL.S,KUPROS.S#'
     +,  KUEXEL,  KUPROS
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'KUNWG.S,KUCMD.S,KUGUID.S,KUNDPV.S,KUPAR.S#'
     +,  KUNWG,  KUCMD,  KUGUID,  KUNDPV,  KUPAR
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'KUPVAL.S,KUACT.S#'
     +,  KUPVAL,  KUACT
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'IPL.S,IPM.S,IFA.S,IGTEXT.S,IGBOX.S,IGAXIS.S,IGPIE.S#'
     +,  IPL,  IPM,  IFA,  IGTEXT,  IGBOX,  IGAXIS,  IGPIE
     +,  P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'IGRAPH.S,IGHIST.S#'
     +,  IGRAPH,  IGHIST
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'IGARC.S,IGLBL.S,IGRNG.S,IGMETA.S,IGSA.S,IGSET.S#'
     +,  IGARC,  IGLBL,  IGRNG,  IGMETA,  IGSA,  IGSET
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'IRQLC.S,IRQST.S,ISCR.S#'
     +,  IRQLC,  IRQST,  ISCR
     +,  P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'ISELNT.S,ISFAIS.S,ISFASI.S,ISLN.S,ISMK.S,ISVP.S#'
     +,  ISELNT,  ISFAIS,  ISFASI,  ISLN,  ISMK,  ISVP
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'ISWN.S,ITX.S,ITX3.S,ICLRWK.S,ISWN3.S,ISVP3.S#'
     +,  ISWN,  ITX,  ITX3,  ICLRWK,  ISWN3,  ISVP3
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'IGPAVE.S,IGTERM.S,IPL3.S,IPM3.S,IFA3.S,ISMKSC.S#'
     +,  IGPAVE,  IGTERM,  IPL3,  IPM3,  IFA3,  ISMKSC
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'ISPMCI.S,ISFACI.S,IGCOLM.S#'
     +,  ISPMCI,  ISFACI,  IGCOLM
     +,  P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'IGHTOR.S,IGQ.S,IGQWK.S,IGIWTY.I#'
     +,  IGHTOR  ,IGQ  ,IGQWK  ,IGIWTY
     +,  P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'RZCDIR.S,RZLDIR.S,RZFILE.S,RZEND.S,RZIN.S,RZOUT.S#'
     +,  RZCDIR,  RZLDIR,  RZFILE,  RZEND,  RZIN,  RZOUT
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'RZVIN.S,RZVOUT.S,RLUXGO.S,REXPIN.R#'
     +,  RZVIN,  RZVOUT  ,RLUXGO  ,REXPIN
     +,  P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'RZIODO.S,RZOPEN.S,RZCLOS.S,RZQUOT.S#'
     +,  RZIODO  ,RZOPEN  ,RZCLOS  ,RZQUOT
     +,  P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'VZERO.S,UCOPY.S,RNDM.R,RANNOR.S,LENOCC.I,CLTOU.S#'
     +,  VZERO  ,UCOPY  ,RNDM  ,RANNOR  ,LENOCC  ,CLTOU
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'CUTOL.S,RANMAR.S,RNORML.S,RANLUX.S,RNORMX.S,VAVDEN.R,VAVSET.S#'
     +,  CUTOL  ,RANMAR  ,RNORML  ,RANLUX  ,RNORMX  ,VAVDEN  ,VAVSET
     +,  P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'SORTZV.S,CHISIN.R,VDOT.R,VADD.S,VSCALE.S,CROSS.S#'
     +,  SORTZV  ,CHISIN  ,VDOT  ,VADD  ,VSCALE  ,CROSS
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'VMOD.R,VDIST.R,VDOTN2.R,VSUB.S,VUNIT.S#'
     +,  VMOD  ,VDIST  ,VDOTN2  ,VSUB  ,VUNIT
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'RDMIN.R,RDMOUT.R#'
     +,  RDMIN  ,RDMOUT
     +,  P,P,P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'SBIT0.S,SBIT1.S,SBYT.S,JBIT.I,JBYT.I,UCTOH.S#'
     +,  SBIT0  ,SBIT1  ,SBYT  ,JBIT  ,JBYT  ,UCTOH
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'UHTOC.S,TIMED.S,RFACT.S,RINV.S,FINT.S,RFERDR.S#'
     +,  UHTOC  ,TIMED  ,RFACT  ,RINV  ,FINT  ,RFERDR
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'ERF.R,ERFC.R,FREQ.R,PROB.R,RANLAN.R,GAMMA.R,DLGAMA.R,ALGAMA.R#'
     +,  ERF  ,ERFC  ,FREQ  ,PROB  ,RANLAN  ,GAMMA  ,DLGAMA  ,ALGAMA
     +,  P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'DENLAN.R,DSTLAN.R,DIFLAN.R,XM1LAN.R,XM2LAN.R#'
     +,  DENLAN  ,DSTLAN  ,DIFLAN  ,XM1LAN  ,XM2LAN
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'BESJ0.R,BESJ1.R,BESY0.R,BESY1.R,BESI0.R,BESI1.R#'
     +,  BESJ0  ,BESJ1  ,BESY0  ,BESY1  ,BESI0  ,BESI1
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'BESK0.R,BESK1.R,EBESI0.R,EBESI1.R#'
     +,  BESK0  ,BESK1  ,EBESI0  ,EBESI1
     +,  P,P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'EBESK0.R,EBESK1.R,DCAUCH.D,DRTEQ3.R,DIVDIF.R#'
     +,  EBESK0  ,EBESK1  ,DCAUCH  ,DRTEQ3  ,DIVDIF
     +,  P,P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'DBESJ0.R,DBESJ1.R,DBESY0.R,DBESY1.R,DBESI0.R,DBESI1.R#'
     +,  DBESJ0  ,DBESJ1  ,DBESY0  ,DBESY1  ,DBESI0  ,DBESI1
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      CALL CSEXT(
     +  'DBESK0.R,DBESK1.R,DEBSI0.R,DEBSI1.R,DEBSK0.R,DEBSK1.R#'
     +,  DBESK0  ,DBESK1  ,DEBSI0  ,DEBSI1  ,DEBSK0  ,DEBSK1
     +,  P,P,P,P)
*------- 1 2 3 4 5 6 7 8 9
      RETURN
*
      ENTRY PAWCSH
      WRITE(LERR,1000)
      WRITE(LERR,1005)
      WRITE(LERR,1010)
      WRITE(LERR,2000)
      WRITE(LERR,2010)
      WRITE(LERR,2020)
      WRITE(LERR,3000)
      RETURN
 1000 FORMAT(
     + '  From HBOOK:',/
     +,'    HBOOK1,HBOOK2,HBOOKN,HFILL,HF1,HF1E,HPRINT,HDELET,HRESET',/
     +,'    HFITGA,HFITPO,HFITEX,HPROJ1,HPROJ2,HFN,HGFIT,HRENID,HF1N',/
     +,'    HROPEN,PAOPEN,PACLOS,PAREAD,PAWRIT,HCDIR,HGIVEN,HKIND',/
     +,'    HTITLE,HBFUN1,HBFUN2,HRNDM1,HRNDM2,HBARX,HBARY,HDIFFB',/
     +,'    HPAK,HPAKE,HUNPAK,HGIVE,HGN,HGNF,HGNPAR,HF2,HFF1,HFF2',/
     +,'    HRIN,HROUT,HI,HIE,HIX,HXE,HIJ,HIF,HIDALL,HNOENT,HX,HXY',/
     +,'    HTITLE,HCOPY,HSTATI,HBPROF,HOPERA,HIDOPT,HDERIV,HBAR2',/
     +,'    HMAXIM,HMINIM,HMAX,HMIN,HSUM,HNORMA,HMCINI,HMCMLL',/
     +,'    HEXIST,HREND,HRGET,HRPUT,HSCR,HFIND,HCX,HCXY,HLABEL',/
     +,'    HBPROX,HBPROY,HBANDX,HBANDY,HBSLIX,HBSLIY,HPROF2,HRDIR',/
     +,'    HBOOKB,HBSTAT,HDIFF,HUNPKE,HREBIN,HERROR,HGNTB,HSTAF',/
     +,'    HOUTPU,HERMES,HISTDO,HFUNC,HXI,HIJXY,HXYIJ,HLPOS,HFC1',/
     +,'    HSPLI1,HSPLI2,HMDIR,HLDIR,HLOCAT,HFITH,HFITV,HFINAM',/
     +,'    HBNT,HBNAME,HBNAMC,HFNT,HFNTB,HGNT,HGNTF,HGNTV,HBSET',/
     +,'    HRENAME,HNTDUP,HFITHN,HIJE')
 1005 FORMAT(
     + '  From MINUIT:',/
     +,'    MNEMAT,MNERRS,MNSTAT')
 1010 FORMAT(
     + '  From HPLOT:',/
     +,'    HPLOT,HPLSYM,HPLERR,HPLEGO,HPLNT,HPLSUR,HPLSOF,HPLFRA',/
     +,'    HPLABL,HPLSET,HPLGIV,HPLOC,HPLTOC,HPLNEW,HPLOPT')

 2000 FORMAT(
     + '  From ZEBRA:',/
     +,'    MZSTOR,MZDIV,MZLINK,MZWORK,MZBOOK,MZDROP,MZPUSH',/
     +,'    MZWIPE,MZGARB,MZFORM,LZFIND,LZFID,DZSHOW,DZVERI',/
     +,'    FZIN,FZOUT,FZFILE,FZENDI,FZENDO',/
     +,'    RZCDIR,RZLDIR,RZFILE,RZEND,RZIN,RZOUT,RZVIN,RZVOUT',/
     +,'    RZOPEN,RZIODO,RZCLOS,RZQUOT',/
     +,'  From KUIP:',/
     +,'    KUGETV,KUDPAR,KUVECT,KILEXP,KUTIME,KUEXEL,KUPROS',/
     +,'    KUNWG,KUCMD,KUGUID,KUNDPV,KUPAR,KUPVAL,KUACT')
 2010 FORMAT(
     + '  From HIGZ:',/
     +,'    IPL,IPM,IFA,IGTEXT,IGBOX,IGAXIS,IGPIE,IGRAPH,IGHIST',/
     +,'    IGARC,IGLBL,IGRNG,IGMETA,IGSA,IGSET,IRQLC,IRQST,ISCR',/
     +,'    ISELNT,ISFAIS,ISFASI,ISLN,ISMK,ISVP,ISWN,ITX,ITX3,ICLRWK',/
     +,'    IGPAVE,IGTERM,IPL3,IPM3,IFA3,ISMKSC,ISPMCI,ISFACI',/
     +,'    IGCOLM,IGHTOR,IGQ,IGQWK,IGIWTY,ISWN3,ISVP3')
 2020 FORMAT(
     + '  From MATHLIB:',/
     +,'    RANMAR,RNORML,RLUXGO,RANLUX,RNORMX,VAVDEN,VAVSET,ALGAMA',/
     +,'    DLGAMA,PROB,DENLAN,DSTLAN,DIFLAN,XM1LAN,XM2LAN,RANLAN',/
     +,'    RNDM,RDMIN,RDMOUT,SORTZV,CSF77,VMOD,VDIST,VDOTN2',/
     +,'    VSUB,VUNIT,CHISIN,VDOT,VADD,VSCALE,CROSS,REXPIN',/
     +,'    BESJ0,BESJ1,BESY0,BESY1,BESI0,BESI1,BESK0,BESK1',/
     +,'    EBESI0,EBESI1,EBESK0,EBESK1,DCAUCH,ERF,ERFC,FREQ',/
     +,'    DBESJ0,DBESJ1,DBESY0,DBESY1,DBESI0,DBESI1,DBESK0,DBESK1',/
     +,'    DEBSI0,DEBSI1,DEBSK0,DEBSK1,GAMMA,DRTEQ3,DIVDIF,RFACT',/
     +,'    RINV,FINT,RFERDR',/
     +,'  From KERNLIB:',/
     +,'    VZERO,UCOPY,RANNOR,LENOCC,SBIT0,SBIT1,SBYT',/
     +,'    JBIT,JBYT,UCTOH,UHTOC,CLTOU,CUTOL')

 3000 FORMAT(/'  The following common blocks may be referenced:',/
     +,       '    /PAWC/  , /QUEST/ , /KCWORK/, /PAWPAR/ , /PAWIDN/',/
     +,       '    /HCFITS/, /HCFITD/, /RZCLUN/, /HCBOOK/ , /HIGRAF/',/
     +,       '    /PAWCHN/, /PAWCR8/, /PAWCR4/, /RZCOUNT/, /RZCLUN/',/
     +,       '    /PAWCHC/, /PAWC32/, /HCFLAG/, /HCDIRN/ , /HCDIRC/',/
     +,       '    /HCFILE/')
      END

      SUBROUTINE PALDCF
*
* Load DOUBLE COMPLEX functions
*
      COMPLEX C1,C2
      DOUBLE COMPLEX DC1,DC2
      C1  = CMPLX(1.,1.)
      C2  = CMPLX(1.,1.)
      C2  = C1/C2
      DC1 = DCMPLX(0.1D0,0.3D0)
      DC2 = DCMPLX(0.2D0,0.3D0)
      DC2 = DC1/DC2
      DC2 = CDSQRT(DC1)
      DC2 = CDLOG(DC1)
      DC2 = CDEXP(DC1)
      END
#endif
