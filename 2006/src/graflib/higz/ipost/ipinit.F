*
* $Id: ipinit.F,v 1.3 2005/10/28 12:38:14 couet Exp $
*
* $Log: ipinit.F,v $
* Revision 1.3  2005/10/28 12:38:14  couet
* - SVG driver, workstation type -779
*
* Revision 1.2  1999/07/20 14:09:06  couet
* - New version of the pattern polygon filling. It now uses the Pattern
*   functionality provided by PostScript level 2. This allows to have
*   colored pattern and a better drawing of the pattern.
*
* Revision 1.1.1.1  1996/02/14 13:11:06  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/01 23/05/95  15.56.21  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IPINIT(IWID,LUN,IPSWK)
*.===========>
*.
*.  Initialise the PostScript interface
*.
*. _Input parameters:
*.
*. INTEGER IWID : Workstation identifier.
*. INTEGER LUN : Logical Unit Number for the PostScript file.
*. INTEGER IPSWK : Workstation type.
*.
*..==========> (O.Couet)
#include "higz/hipost.inc"
#include "higz/hiflag.inc"
*.______________________________________
*
      IPSLT  = 0
      IPSLW  = 0
      IPSCI  = 0
      IPSMSC = -1
      PSRED  = -1.
      PSGRN  = -1.
      PSBLU  = -1.
      LUNPS  = LUN
      IPSWID = IWID
      STRBUF = ' '
      LENBUF = 0
      LENBMX = 80
      IPCLIP = 0
      IPDXC  = 0
      IPDYC  = 0
      IPXC   = 0
      IPYC   = 0
      NBSAVE = 0
      IPATD  = 0
#if !defined(CERNLIB_BSLASH)
      BSLASH = '\'
#endif
#if defined(CERNLIB_BSLASH)
      BSLASH = '\\'
#endif
      TFLAG = .FALSE.
      IF(IPSWK.EQ.-777.OR.IPSWK.EQ.-778)TFLAG = .TRUE.
      VFLAG = .FALSE.
      IF(IPSWK.EQ.-779)VFLAG = .TRUE.
      IF(TFLAG) THEN
         IPSWTY = -113
         IPTWTY = IPSWK
         LOBB   = .FALSE.
         LOCLR  = .FALSE.
         LOZONE = .TRUE.
#if defined(CERNLIB_IBM)
         LENBMX = 80
#endif
#if !defined(CERNLIB_IBM)
         LENBMX = 132
#endif
         IF(IPTWTY.EQ.-778)THEN
            CALL IPPSTR(BSLASH//'documentstyle{article}')
            CALL IPPSTR(BSLASH//'pagestyle{empty}')
            CALL IPPSTR(BSLASH//'evensidemargin-.5in')
            CALL IPPSTR(BSLASH//'oddsidemargin-.5in')
            CALL IPPSTR(BSLASH//'topmargin-.5in')
            CALL IPPSTR(BSLASH//'multiply'//BSLASH//'textheight14')
            CALL IPPSTR(BSLASH//'divide'//BSLASH//'textheight10')
            CALL IPPSTR(BSLASH//'multiply'//BSLASH//'textwidth14')
            CALL IPPSTR(BSLASH//'divide'//BSLASH//'textwidth10')
            CALL IPPSTR(BSLASH//'begin{document}@')
         ENDIF
      ELSEIF (VFLAG) THEN
         IPSWTY = -113
         IPTWTY = IPSWK
         LOBB   = .FALSE.
         LOCLR  = .FALSE.
         LOZONE = .TRUE.
      ELSE
         IPSWTY = IPSWK
         LOCLR  = .TRUE.
         LOZONE = .FALSE.
         IF(IPSWK.EQ.-113)THEN
            LOBB = .FALSE.
            CALL IPPSTR('%!PS-Adobe-2.0 EPSF-2.0@')
         ELSE
            LOBB = .TRUE.
            CALL IPPSTR('%!PS-Adobe-2.0@')
            CALL IPDEF
         ENDIF
      ENDIF
      LOPRT  = .FALSE.
      LOPRNG = .FALSE.
      LOSCLI = .FALSE.
*
      END
