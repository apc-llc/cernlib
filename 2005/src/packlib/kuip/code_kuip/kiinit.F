*
* $Id: kiinit.F,v 1.2 1997/10/23 16:53:58 mclareni Exp $
*
* Disable searching kuesrv under Windows NT
*
* $Log: kiinit.F,v $
* Revision 1.2  1997/10/23 16:53:58  mclareni
* NT mods
*
* Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.05/02 27/04/94  12.42.30  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   25/08/92
      SUBROUTINE KIINIT(NWORDS)
*
********************************************************************************
*
* Initialize the command processor
*
*     Input :
*
      INTEGER NWORDS
*
********************************************************************************
*
#include "kuip/kcmcmd.inc"
#include "kuip/kcgen.inc"
#include "kuip/kcgen1.inc"
#include "kuip/kcgen2.inc"
#include "kuip/kcunit.inc"
#include "kuip/kcsigm.inc"
#include "kuip/kcpar.inc"
#include "kuip/kcvect.inc"
#include "kuip/kcmac.inc"
#include "kuip/kcesvr.inc"
*
      CHARACTER*8  RTIME
      INTEGER      LENGTH(3)
#if defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)
      LOGICAL      KUQEXE
#endif
*
#if defined(CERNLIB_APOLLO)
%INCLUDE '/sys/ins/base.ins.ftn'
#endif
#if defined(CERNLIB_IBMALL)
      EXTERNAL    ERRRUN
      CHARACTER*8 ENT108, ENT151, ENT218
*
      IERNO=0
*
      CALL ERRSET(108,999,-1)
      CALL ERRSET(151,999,-1)
      CALL ERRSET(180,999,-1)
      CALL ERRSET(203,999,-1,1,ERRRUN,204)
      CALL ERRSET(206,999,-1,1,ERRRUN,207)
      CALL ERRSET(208,999,-1)
      CALL ERRSET(209,999,-1,1,ERRRUN,217)
      CALL ERRSET(219,999,-1,1,ERRRUN,229)
      CALL ERRSET(231,999,-1,1,ERRRUN,239)
      CALL ERRSET(241,999,-1,1,ERRRUN,301)
      CALL ERRSET(218,999,-1)
*
      CALL ERRSAV(108,ENT108)
      CALL ERRSAV(151,ENT151)
      CALL ERRSAV(218,ENT218)
*
#endif
*
* Initialize backslash character
*
#if !defined(CERNLIB_BSLASH)
      BSLASH='\'
#endif
#if defined(CERNLIB_BSLASH)
      BSLASH='\\'
#endif
*
* Initialize NO user redefinable logical units :
*
* ILUIN  = Internal input
* ILUOUT = Internal output (command and parameter prompts)
*
      ILUIN=5
      ILUOUT=6
*
* Initialize user redefinable logical units :
*
* LUOUT  = External output (normal output)
* LUNLOG = External log (errors + macro trace)
*
      LUOUT=6
      LUNLOG=6
      CALL KULUN(11)
*
* Create division and link area
*
      MINDIV=NWORDS
      MAXDIV=NWPAW
      CALL MZDIV(IXPAWC,IXKUIP,'KUIP Div',MINDIV,MAXDIV,'R')
      CALL MZLINK(IXPAWC,'/KCLINK/',LTOP,LCEL,LLAST)
      LLAST=0
*
* Clear the temporary vector bank addresses
*
      DO 10 I=1,MAXLVA
        LVADDR(I)=0
10    CONTINUE
      NVADD=0
      TVECFL=.TRUE.
      CALL MZLINK(IXPAWC,'/KCVADD/',LVADDR(1),LVADDR(MAXLVA),LVADDR(1))
*
* Define bank format
*
      CALL MZFORM('TOP','-I',IOCEL)
      CALL MZFORM('NAME','9H *H',IONAME)
      CALL MZFORM('PAR','*H 2H *H',IOPAR)
      CALL MZFORM('PARV','-I',IOPARV)
      CALL MZFORM('VEC','-I',IOVEC)
      CALL MZFORM('VECN','9H 1I -F',IOVECN)
*
* Create and fill Top CEL Bank
*
      NPAR=0
      NS=3
      NL=NS+1+NPAR
      ND=5
      CALL MZBOOK(IXKUIP,LTOP,LTOP,1,'TOP',NL,NS,ND,IOCEL,0)
      NDOWN=0
      NMAPAR=0
      IENBL=1
      IQ(LTOP+1)=NDOWN
      IQ(LTOP+2)=NPAR
      IQ(LTOP+3)=NMAPAR
      IQ(LTOP+4)=IENBL
      IQ(LTOP+5)=0
*
* Create and fill top LIST bank
*
      NS = 1
      NL = 1
      ND = 1
      CALL MZBOOK(IXKUIP,LLIST,LLIST,1,'TLST',NL,NS,ND,2,0)
      IQ(LLIST+1) = 0
*
* Create the root address for Vectors and the '?' vector
*
      NS=1
      NL=NS
      ND=0
      CALL MZBOOK(IXKUIP,LVEC,LVEC,1,'VEC',NL,NS,ND,IOVEC,0)
      NUMVEC=0
      LENGTH(1)=100
      LENGTH(2)=1
      LENGTH(3)=1
      CALL KUVCRE('?','R',LENGTH,LLOW,LHIGH)
*
* Clear the directory array
*
      IWD=0
      DO 20 I=1,MAXLEV
        WDNAM(I)=' '
20    CONTINUE
*
* Glossary initializations
*
      LCELGF=0
      CHGLOS=' '
*
* Initialize name of extensions for macro files
*
#if (defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_WINNT))
      MACEXT='kum'
      MACBAK='kuo'
#endif
#if !defined(CERNLIB_MSDOS)||defined(CERNLIB_WINNT)
      MACEXT='kumac'
      MACBAK='kumacold'
#endif
*
* To NOT load SIGMA:
*
      ISIADD=0
*
* Initialisations of the pointers to the list data structures.
*
      LCLIST = 0
*
* Other initializations
*
      DEFLAG=.FALSE.
      HISTOK=.TRUE.
      NOHIST=.FALSE.
      CHMEXE=' '
      MEXEFL=.FALSE.
      MULTFL=.FALSE.
      CHMULT=' '
      CHMULM=' '
      LENMUL=-1
      LENMUM=-1
      UNIQUE=.FALSE.
      FILCAS=.FALSE.
      IONERF=1
      COMAND=' '
      IUSADD=0
      IEXADD=0
      IQUADD=0
      IUTADD=0
      NEXADD=0
      IBRADD=0
      LICALL=.TRUE.
      LICAL2=.TRUE.
      LICAL3=.TRUE.
      NVSIGM=0
      LTOPW1=LTOP
      LTOPW2=LTOP
      CHROOT='/'
      LENTER=80
      NCMD=0
      CPRMPT='KUIP >'
      LPRMPT=LENOCC(CPRMPT)
      SPRMPT=CPRMPT
      IBRAK=0
      APPLIC=' '
      LAPPLI=0
      LUNAPP=5
      TRACE=.FALSE.
      TRAPRO=' '
      IREPET=0
      IREFAC=0
      CELLST=' '
      PNALST=' '
      IIIMAC=0
      MACTAB=.FALSE.
      MACPAT=' '
CCCM
      MACLIS=' '
      AUTOEX='C'
      DEBTAB=.FALSE.
      NSTLEV=0
      QUITFL=.FALSE.
      SKIPFL=.FALSE.
      WAITFL=.FALSE.
      WAITFF=.FALSE.
*
* STYLEs initialization
*
      HELPAT=CHAR(0)
*
* Used in case of calling KUPROx not together a command line
*
      GETPAR=0
      TOTPAR=0
      PARLST=' '
      NOALIN=' '
      CHLAST=' '
      GETSPY=0
      SPYFLG=.FALSE.
      SPYING=.TRUE.
      PROFLG=.FALSE.
*
* Edit server initialization
*
#if (!defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CRAY))
      SERVER = .FALSE.
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)
      LE = LENOCC(EDITOR)
      IF (EDITOR(LE-1:LE) .EQ. 'vi') THEN
         IF (EDITOR(1:1) .EQ. '/' .OR. EDITOR(1:2) .EQ. 'vi') THEN
            SERVER = .FALSE.
         ELSEIF (KUQEXE('kuesvr')) THEN
            SERVER = .TRUE.
         ELSE
            SERVER = .FALSE.
         ENDIF
#ifndef CERNLIB_WINNT
      ELSEIF (KUQEXE('kuesvr')) THEN
         SERVER = .TRUE.
#endif
      ELSE
         SERVER = .FALSE.
      ENDIF
#endif
      NSVFIL = 0
      IESADD = 0
      DO 46 I = 1, MAXSVR
         EDTFIL(I) = ' '
         EDTCMD(I) = ' '
46    CONTINUE
*
* Initialize parameter pointers
*
      CALL VZERO(PARPTR,4*MAXPAR)
*
* Build basic command structure from definition routine (excepted VECTOR)
*
***      CALL KUIDF1
***      CALL KUIDF2
***      CALL MACDEF
*
* Time initialization
*
      CALL TIMEST(9999.)
      CALL TIMED (TIME)
      CALL KIRTIM(RTIME)
      TIMING=.FALSE.
      TIMALL=.FALSE.
*
* Initialize break handling
*
      CALL KUTRUE(.TRUE.,.FALSE.)
*
* Pass the starting address of PAWC to C part
*
      CALL KIPAWC(NWPAW)
*
* Initialize the working mode of KUIP
*
      CALMOD=0
      INPMOD='C'
*
999   END


