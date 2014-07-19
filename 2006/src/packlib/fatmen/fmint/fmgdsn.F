*
* $Id: fmgdsn.F,v 1.1.1.1 1996/03/07 15:18:12 mclareni Exp $
*
* $Log: fmgdsn.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:12  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGDSN(LBANK,CHDSN,LDSN,IRC)
 
      CHARACTER*(*) CHDSN
      CHARACTER*255 CHNFS
 
#include "fatmen/fatmon.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/slate.inc"
 
      IRC = 0
 
      CALL UHTOC(IQ(LBANK+KOFUFA+MFQNFA),4,CHDSN,NFQNFA)
      LDSN   = LENOCC(CHDSN)
      LBLANK = INDEX(CHDSN,' ')
      IF(LBLANK.NE.0) LDSN = LBLANK - 1
#if defined(CERNLIB_UNIX)
*
*     If CHDSN starts with a $, assume that it is an environmental
*     variable.
*
      IF(CHDSN(1:1).EQ.'$') THEN
         LENV = INDEX(CHDSN(1:LDSN),'/')
         CALL GETENVF(CHDSN(2:LENV-1),CHNFS)
         IF(IS(1).GT.0) THEN
            CHDSN = CHNFS(1:IS(1)) // CHDSN(LENV:LDSN)
            LDSN  = LENOCC(CHDSN)
            CALL SBIT1(IHOWFA,JNFSFA)
         ELSE
            IF(IDEBFA.GE.-3) PRINT *,'FMGDSN. cannot translate ',
     +         ' environmental variable ',CHDSN(1:LENV-1)
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
*
*     If CHDSN starts with a $, assume that it is an environmental
*     variable.
*
      IF(CHDSN(1:1).EQ.'$') THEN
         LENV = INDEX(CHDSN(1:LDSN),'/')
         CALL FMGTLG(CHDSN(2:LENV-1),CHNFS,'LNM$SYSTEM',IRC)
         LCHNFS = IS(1)
         IF(LCHNFS.GT.0) THEN
            CALL SBIT1(IHOWFA,JNFSFA)
*
*     If there is more than one slash in file name
*     assume that the intervening elements are directory names
*
            JSLASH              = INDEXB(CHDSN(1:LDSN),'/')
            IF(JSLASH.EQ.LENV) THEN
               CHNFS(LCHNFS+1:) = CHDSN(LENV+1:LDSN)
               LCHNFS           = LCHNFS + LDSN - LENV
            ELSE
               CHNFS(LCHNFS+1:) = '[' // CHDSN(LENV+1:JSLASH-1)
     +                            // ']' // CHDSN(JSLASH+1:LDSN)
               LCHNFS           = LCHNFS + LDSN - LENV + 1
               CALL CTRANS('/','.',CHNFS,1,LCHNFS)
            ENDIF
         CHDSN  = CHNFS(1:LCHNFS)
         LDSN = LCHNFS
 
         ENDIF
      ENDIF
#endif
 
      END
