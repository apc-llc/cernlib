*
* $Id: hbont.F,v 1.1.1.1 1996/01/16 17:08:11 mclareni Exp $
*
* $Log: hbont.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:11  mclareni
* First import
*
*
#include "sys/CERNLIB_machine.h"
#include "_hbook/pilot.h"
*CMZ :  4.21/08 17/01/94  12.31.07  by  Rene Brun
*-- Author :
      SUBROUTINE HBONT(ID,CHTITL,FORMP,CHOPT)
*.==========>
*.           Creates an Omnipurpose Ntuple
*.
*.      An Omnipurpose Ntuple is a Column-Wise-Ntuple. Each row of an ONT
*.      may contain up to 4 arrays depending on FORMP.
*.      Ex:
*.         CALL HBONT(ID,'Example','Z',' ')
*.      will create an ONT with the 3 arrays XONT,YONT,YONT in one ROW.
*.
*.      Via FORMP the following, predefined, columns can be activated:
*.        X       uses array  X only
*.        Y       uses arrays X and Y
*.        Z       uses arrays X, Y and Z
*.        W       uses arrays X, Y, Z and W
*.        C       uses array CHONT (char*4)
*.
*.      NB. Options X,Y,Z,W are mutual exclusive
*.      The (scalar) column KONTYP is always present.
*.
*.      CHOPT has the same meaning as for HBNT. CHOPT='M' defines a
*.      memory resident Ntuple.
*.
*.      To avoid users declaring the above structure, a set of service
*.      routines is provided to fill the internal HBOOK commons /HCBONT/.
*.        HFONTR(NONT,XONT,YONT,ZONT,WONT)
*.        HFONTC(NCHONT,CHONT)
*.
*.      Once /HCBONT/ is filled via the above routines, the routine HFONT
*       must be used to fill the ONT.
*.        CALL HFONT(ID,KONTYP)
*.
*.      In a symetric way, the ONT may be read with the following sequence:
*.        CALL HGONT(ID,KONTYP)
*.        CALL HGONTR(NONT,XONT,YONT,ZONT,WONT)
*.        HGONTC(NCHONT,CHONT)
*.
*.        Description of the HCBONT common blocks
*.        ======================================
*.      KONTYP type of record
*.      XONT array of NONT elements (real)
*.      YONT array of NONT elements (real)
*.      ZONT array of NONT elements (real)
*.      WONT array of NONT elements (real)
*.      CHONT array of NCHONT elements (character*4)
*.
*.    NB. The arrays XONT, etc have a maximum dimension of 1000 words.
*.
*.    Bits 5,6,7 and 8 are used by the ONT.
*.
*..=========> ( R. Brun )
*
#include "hcbook.inc"
#include "hcntpar.inc"
#include "hcbont.inc"
*
      CHARACTER*(*) CHTITL, FORMP, CHOPT
      CHARACTER*32 FORM
      CHARACTER*80 CHFORM
*___________________________________________________________________
*
      CALL HBNT(ID,CHTITL,CHOPT)
*
*-- TODO: handle packing and the creation of the range of the index
*-- variables depending on MAXNP
*
      FORM=FORMP
      CALL CLTOU(FORM)
      NAR=0
      IF(INDEX(FORM,'X').NE.0)NAR=1
      IF(INDEX(FORM,'Y').NE.0)NAR=2
      IF(INDEX(FORM,'Z').NE.0)NAR=3
      IF(INDEX(FORM,'W').NE.0)NAR=4
      CHFORM='KONTYP,NONT[0,1000]'
      IC=20
      IF(NAR.GE.0)THEN
         CALL SBYT(NAR,IQ(LCID+ZBITS),5,3)
      ENDIF
      IF(NAR.GE.1)THEN
         CHFORM(IC:)=',XONT(NONT)'
         IC=IC+11
      ENDIF
      IF(NAR.GE.2)THEN
         CHFORM(IC:)=',YONT(NONT)'
         IC=IC+11
      ENDIF
      IF(NAR.GE.3)THEN
         CHFORM(IC:)=',ZONT(NONT)'
         IC=IC+11
      ENDIF
      IF(NAR.GE.4)THEN
         CHFORM(IC:)=',WONT(NONT)'
         IC=IC+11
      ENDIF
*
      CALL HBNAME(ID,'HCBONTR',KONTYP,CHFORM)
*
      IF(INDEX(FORM,'C').NE.0)THEN
         CALL HBNAME(ID,'HCBONTC',NCHONT,'NCHONT[0,100]')
         CALL HBNAMC(ID,'HCBONTC',CHONT ,'CHONT(NCHONT):C')
         CALL SBIT1(IQ(LCID+ZBITS),8)
      ENDIF
*
      KONTYP = 0
      NCHONT = 0
      NONT   = 0
*
      END
