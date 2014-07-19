*
* $Id: iginit.F,v 1.1.1.1 1996/02/14 13:10:35 mclareni Exp $
*
* $Log: iginit.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:35  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/10 03/10/94  09.19.42  by  O.Couet
*-- Author :
      SUBROUTINE IGINIT(NWORDS)
*.===========>
*.
*.   This routine initializes HIGZ .
*.
*. _Input parameters:
*.
*.  INTEGER NWORDS : Minimal space in the ZEBRA HIGZ division .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hilun.inc"
#include "higz/hiatt.inc"
#include "higz/himet2.inc"
#include "higz/himeta.inc"
#include "higz/himenu.inc"
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
#include "higz/hint.inc"
#endif
#include "higz/hiloc.inc"
#include "higz/hilut.inc"
#include "higz/hihid.inc"
#include "higz/hilabs.inc"
#if defined(CERNLIB_X11)
#include "higz/hikern.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_DI3000)
      INTEGER*4 MAXDEV
      PARAMETER (MAXDEV = 9)
      INTEGER*4 DEVICES(0:MAXDEV)
      EQUIVALENCE (ILOPWK,DEVICES)
#endif
*.______________________________________
*
      LUNIN  = 5
      LUNOUT = 6
      LUNERR = 6
*
#if defined(CERNLIB_ZEBRA)
      IF(NWORDS.GE.0)CALL IZINIT(NWORDS)
#endif
*
*              Flags initialisation
*
* ZFLAG  = If .TRUE. the ZEBRA mode is on
* GFLAG  = If .TRUE. the graphic is on
* CFLAG  = If .TRUE. in the pictures, the attributes are stored
*          only if necessary
* MFLAG  = Mail flag (telnetg)
* PFLAG  = If .TRUE. PostScript or LaTeX metafile is active
* TFLAG  = If .TRUE. LaTeX metafile, if .FALSE. PostScript metafile
* GLFLAG = GLobal flag. If .TRUE. the IZ routines must be called
* GRFLAG = Graphic flag (used by IGSG and IGSA)
* ASFLAG = Automatic picture saving flag
* AXFLAG = Axis flag (usefull for IGHIST and IGRAPH)
* REDIT  = Editing factor
*
#if defined(CERNLIB_BATCH)
      ZFLAG  = .TRUE.
      GFLAG  = .FALSE.
#endif
#if !defined(CERNLIB_BATCH)
      ZFLAG  = .FALSE.
      GFLAG  = .TRUE.
#endif
      CFLAG  = .TRUE.
      PFLAG  = .FALSE.
      TFLAG  = .FALSE.
      MFLAG  = .FALSE.
      GRFLAG = .FALSE.
      ASFLAG = .FALSE.
      AXFLAG = .FALSE.
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
      REDIT  = 0.
#if defined(CERNLIB_MAIL)
*
*            MAIL option initialisation
*
      CHCTRL = '``'
      CHEOR  = '@'
      CHMAIL = ' '
#endif
*
* METDEF = Default metafile type
* IDMETA = Metafile identifier used by IGMETA
* CHMETA = Current metafile name
* ITGKS  = Identifier of the underlaying Graphical Kernel System used
*
      IFUSER = 0
#if (defined(CERNLIB_GKSGRAL))&&(!defined(CERNLIB_SUNGKS)||!defined(CERNLIB_ATCGKS)||!defined(CERNLIB_GKSIBM))
      ITGKS  = 1
      METDEF = 4
      IDMETA = 2
#endif
#if defined(CERNLIB_PLOT10GKS)
      ITGKS  = 2
      METDEF = 300018
      IDMETA = 2
#endif
#if defined(CERNLIB_MGKS)
      ITGKS  = 3
      METDEF = 300018
      IDMETA = 2
#endif
#if defined(CERNLIB_UNIGKS)
      ITGKS  = 5
      METDEF = 1
      IDMETA = 2
#endif
#if defined(CERNLIB_SUNGKS)
      ITGKS  = 6
      METDEF = 3
      IDMETA = 2
#endif
#if defined(CERNLIB_GKSIBM)
      ITGKS  = 7
      METDEF = 3
      IDMETA = 2
#endif
#if defined(CERNLIB_DI3000)
      ITGKS  = 11
      METDEF = 0
      IDMETA = 0
      CALL VZERO(DEVICES,MAXDEV+1)
#endif
#if (defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)||defined(CERNLIB_BATCH))&&(!defined(CERNLIB_MSDOS))
      ITGKS  = 12
      METDEF = -111
      IDMETA = 2
#endif
#if defined(CERNLIB_DECGKS)
      ITGKS  = 13
      METDEF = 2
      IDMETA = 2
#endif
#if defined(CERNLIB_ATCGKS)
      ITGKS  = 14
      METDEF = 10120
      IDMETA = 2
#endif
#if (defined(CERNLIB_PHIGS))&&(!defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_FALCO))
      ITGKS  = 15
      METDEF = 80795
      IDMETA = 2
#endif
#if defined(CERNLIB_GKSBX)
      ITGKS  = 16
      METDEF = -111
      IDMETA = 2
#endif
#if defined(CERNLIB_MSDOS)
      IPICNM = 0
      ISURF  = 18
      METDEF = ISURF
      ITGKS  = 16
      IDMETA = 2
      SCMETA = 20.0
#endif
      CHMETA = ' '
      TEKACT = .TRUE.
      METACT = .FALSE.
      FILOPN = .FALSE.
      XMFACT = 0.01
      YMFACT = 0.01
*
*              Attributes initialisation
*
      IDID   = 1
      ICLIP  = 1
      IFAISI = 0
#if !defined(CERNLIB_DI3000)
      IFASI  = 1
#endif
#if defined(CERNLIB_DI3000)
      IFASI  = 0
#endif
      ILN    = 1
      RLWSC  = 1.0
      RBSL   = 0.01
      IMK    = 1
      RMKSC  = 1.0
      IPLCI  = 1
      IPMCI  = 1
      IFACI  = 1
      ITXALH = 0
      ITXALV = 0
      ITXALG = 0
      RCHH   = 0.28
      RCHUX  = 0.0
      RCHUY  = 1.0
      RANGLE = 0.0
      ITXCI  = 1
      IFONT  = 0
      IPREC  = 2
      IFTPR  = 2
      IBORD  = 0
      INPASS = 1
      RCSHIF = 0.02
      RALH   = 0.018
      RALD   = 0.013
      RTMS   = 0.019
      RBOF   = 0.25
      RBWD   = 0.5
      RAWL   = 0.0
      INLINE = 40
      INBCOL = 8
      IDIM   = 2
*
*              Axis Alphanumeric Labels initialisation
*
      DO 10 I=1,MAXLAB
         HILABS(I) = ' '
   10 CONTINUE
*
*              Initialise workstations parameters
*
* INOPWK = Number of opened workstations
* ILOPWK = List of the workstation IDs of the opened workstations
* IWTYL  = List of the workstation types of the opened workstations
* ACWKFL = List of flags. If TRUE the workstation is active
* XWKSIZ = X size of the workstation
* YWKSIZ = Y size of the workstation
* XWKR   = Workstation ratio on X
* YWKR   = Workstation ratio on Y
* XWKWn  = X worksation window value
* YWKWn  = Y worksation window value
* XWKVn  = X worksation viewport value
* YWKVn  = Y worksation viewport value
* IWINID = Workstation window ID
*
      INOPWK = 0
      DO 20 I=1,10
         ILOPWK(I) = 0
         IWTYL(I)  = 0
         ACWKFL(I) = .FALSE.
         XWKSIZ(I) = 0.
         YWKSIZ(I) = 0.
         IWINID(I) = 0
         XWKR(I)   = 1.
         YWKR(I)   = 1.
         XWKW1(I)  = 0.
         YWKW1(I)  = 0.
         XWKV1(I)  = 0.
         YWKV1(I)  = 0.
         XWKW2(I)  = 1.
         YWKW2(I)  = 1.
         XWKV2(I)  = 1.
         YWKV2(I)  = 1.
   20 CONTINUE
*
      IPICT  = 1
      NT0PRI = 1
*
*              Default Normalisation Transformation
*
      INTR   = 0
      RWXMIN = 0.
      RWXMAX = 1.
      RWYMIN = 0.
      RWYMAX = 1.
      RVXMIN = 0.
      RVXMAX = 1.
      RVYMIN = 0.
      RVYMAX = 1.
      RDWXMI = 0.
      RDWYMI = 0.
      RDWXMA = 1.
      RDWYMA = 1.
*
*              Initialisation for IRQLC
*
      OLDXP = 0.
      OLDYP = 0.
*
*              Initialisation for IGMENU
*
      XPOS(1)=RDWXMA-0.21
      XPOS(2)=RDWXMA-0.01
      YPOS(1)=RDWYMA-0.71
      YPOS(2)=RDWYMA-0.01
#if (defined(CERNLIB_GKS))&&(!defined(CERNLIB_MGKS))
      DO 30 I=1,100
         ISEGM(I)=0
   30 CONTINUE
#endif
#if defined(CERNLIB_DI3000)
      DO 30 I=1,100
         ISEGM(I)=0
   30 CONTINUE
#endif
*
*              Initialisation of the NT transformation storage
*
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
      NTSTOR(1)=0
#endif
#if defined(CERNLIB_NTC)||defined(CERNLIB_X11)
      CALL IGNINT
#endif
*
*              Initialisation of the number of stacked legos plots
*
      NIDS=0
*
*              Initialise the color table
*
      REDLUT(1)=1.
      REDLUT(2)=0.
      REDLUT(3)=1.
      REDLUT(4)=0.
      REDLUT(5)=0.
      REDLUT(6)=1.
      REDLUT(7)=1.
      REDLUT(8)=0.
      GRNLUT(1)=1.
      GRNLUT(2)=0.
      GRNLUT(3)=0.
      GRNLUT(4)=1.
      GRNLUT(5)=0.
      GRNLUT(6)=1.
      GRNLUT(7)=0.
      GRNLUT(8)=1.
      BLULUT(1)=1.
      BLULUT(2)=0.
      BLULUT(3)=0.
      BLULUT(4)=0.
      BLULUT(5)=1.
      BLULUT(6)=0.
      BLULUT(7)=1.
      BLULUT(8)=1.
      DCOL=1./(NBCLUT-9)
      DO 60 I=9,NBCLUT
         GREY=1.-DCOL*(I-9)
         REDLUT(I)=GREY
         GRNLUT(I)=GREY
         BLULUT(I)=GREY
   60 CONTINUE
*
      END
