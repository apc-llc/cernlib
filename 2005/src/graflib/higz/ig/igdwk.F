*
* $Id: igdwk.F,v 1.4 1998/01/30 15:22:17 couet Exp $
*
* $Log: igdwk.F,v $
* Revision 1.4  1998/01/30 15:22:17  couet
* - APOLLO version removed
*
* Revision 1.3  1998/01/28 14:34:21  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:57:19  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/01 08/05/95  11.34.18  by  O.Couet
*-- Author :    O.Couet   26/11/93
      SUBROUTINE IGDWK(IWKTY)
*.===========>
*.
*.   This routine returns the default workstation type.
*.
*..==========> (O.Couet)
#if defined(CERNLIB_MSDOS)
      CHARACTER*16 CHTMP
#endif
#if (defined(CERNLIB_DECGKS))&&(defined(CERNLIB_VAX))
      CHARACTER*12 STRING
      INCLUDE '($SSDEF)'
#endif
#if (defined(CERNLIB_DECGKS))&&(defined(CERNLIB_DECS))
      CHARACTER*12 STRING
#endif
*.______________________________________
*
#if (defined(CERNLIB_SGI)||defined(CERNLIB_HPUX)||defined(CERNLIB_IBMRT)||defined(CERNLIB_SUN)||defined(CERNLIB_DECS))&&(defined(CERNLIB_GKSGRAL))&&(!defined(CERNLIB_GKSIBM))
      IWKTY = 32120
#endif
#if (defined(CERNLIB_IBMRT))&&(defined(CERNLIB_GKSIBM))
      IWKTY = 6
#endif
#if defined(CERNLIB_BATCH)
      IWKTY = 1
#endif
#if (defined(CERNLIB_GL)||defined(CERNLIB_X11)||defined(CERNLIB_MACMPW))&&(!defined(CERNLIB_MSDOS))
      IWKTY = 1
#endif
#if (defined(CERNLIB_MSDOS))&&(defined(CERNLIB_NDP))
      IWKTY = VIDEO_CONFIGURATION(VIDEO_SYSTEM)
      IF(IWKTY .GE. 18) THEN
        CALL GETENVF('SVGA_MODE',CHTMP)
        IF(CHTMP .NE. ' ')THEN
            READ(CHTMP,*,ERR = 5) I
            IWKTY = I
        ENDIF
      ENDIF
    5 CONTINUE
#endif
#if (defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_NDP))
      IWKTY = 18
#endif
#if (defined(CERNLIB_FALCO))&&(!defined(CERNLIB_KERNEL))
      IWKTY = 7878
#endif
#if (defined(CERNLIB_SUN))&&(defined(CERNLIB_SUNGKS))
      IWKTY = 4
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB_GKSGRAL))&&(!defined(CERNLIB_ATCGKS))
      IWKTY = 7878
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB_ATCGKS))
      IWKTY = 2602
#endif
#if (defined(CERNLIB_SUN))&&(defined(CERNLIB_ATCGKS))
      IWKTY = 5350
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB_DECGKS))&&(!defined(CERNLIB_WINDOWS))
      IF (SS$_NORMAL.EQ.LIB$SYS_TRNLOG('GKS$WSTYPE',LENGTH,STRING)) THEN
        IF(STRING(1:2).EQ.'%X')  THEN
          READ(STRING(3:LENGTH),'(Z8)') IWKTY
        ELSE
          READ(STRING(1:LENGTH),*) IWKTY
        ENDIF
      ELSE
        IWKTY = 14
      ENDIF
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB_DECGKS))&&(defined(CERNLIB_WINDOWS))
      IWKTY = 211
#endif
#if (defined(CERNLIB_DECGKS))&&(defined(CERNLIB_DECS))
      CALL GETENV('GKS3Dwstype',STRING)
      LENGTH = LENOCC(STRING)
      IF(LENGTH.GT.0)THEN
        READ(STRING(1:LENGTH),*)IWKTY
      ELSE
        IWKTY=211
      ENDIF
      IWKTY = IWKTY
#endif
#if (defined(CERNLIB_VAX))&&(!defined(CERNLIB_GKSGRAL))&&(!defined(CERNLIB_DECGKS))&&(!defined(CERNLIB_ATCGKS))&&(defined(CERNLIB_GKS))
      IWKTY = 301400
#endif
#if (defined(CERNLIB_IBM)||defined(CERNLIB_AIX370)||defined(CERNLIB_ALLIANT))&&(defined(CERNLIB_GKSGRAL))&&(!defined(CERNLIB_NEWLIB))
      IWKTY = 7878
#endif
#if (defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_NEWLIB))
      IWKTY = 5003
#endif
#if (defined(CERNLIB_IBM)||defined(CERNLIB_AIX370))&&(!defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_GKS))
      IWKTY = 301400
#endif
#if (defined(CERNLIB_DI3000))&&(defined(CERNLIB_IBM))
      IWKTY = 1
#endif
#if (defined(CERNLIB_DI3000))&&(!defined(CERNLIB_IBM))
      IWKTY = 1
#endif
#if defined(CERNLIB_GKSBX)
      IWKTY = 8811
#endif
#if defined(CERNLIB_PHIGS)
      IWKTY = 8887
#endif
*
      END
