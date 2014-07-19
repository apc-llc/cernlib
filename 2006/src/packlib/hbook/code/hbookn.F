*
* $Id: hbookn.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbookn.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/00 30/03/94  09.33.33  by  Rene Brun
*-- Author :
      SUBROUTINE HBOOKN(IDD,CHTITL,NVAR,CHRZPA,NPRIME,CHTAGS)
*.==========>
*.           BOOKING OF A N-TUPLE
*.
*.         ============> N-TUPLE  DATA STRUCTURE <============
*.
*.                   I
*.                   I
*.                   I LCID
*.    -3   -2   -1   V
*.  ************************************************************************
*.  *    *    *    ****** Bits,NDIM,NOENT,etc  (See A)                     *
*.  ************************************************************************
*.    .     I     I   LNLAST  pointer to last linear bank
*.    ......I.....I.......................................
*.          I     I                                      .
*.          I     I LCONT                                .
*.          I     I                                      V
*.          I     I       ****************************   ********************
*.          I     I------>* (x1,y1,z1,,),(x2,y2,z2,,)*-->*    next if no RZ *
*.          I             ****************************   ********************
*.          I LLIMS
*.          I
*.          I        *****************************************
*.          I------->*  xlow,xup,ylow,yup,zlow,zup,etc       *
*.                   *****************************************
*.
*.
*.    **********************************************************************
*.    * word *  tag    *               content                             *
*.    **********************************************************************
*.    *      *         *                                                   *
*.    *   1  *  BITS   *     status word                                   *
*.    *   2  *  NDIM   *     Number of variables in the N-Tuple            *
*.    *   3  *  NOENT  *     Total number of entries                       *
*.    *   4  *  NPRIME *     Number of words for primary allocation        *
*.    *   5  *  NMEM   *     Number of memory blocks (only 1 with RZ)      *
*.    *   6  *  NRZB   *     Number of RZ records (If RZ is used)          *
*.    *   7  *  IFIRST *     pointer to 1st free word in LCONT             *
*.    *   8  *  NWTITL *     Number of words in the title                  *
*. (A)*   9  *  ITIT1  *     Internal pointer to the first word of title   *
*.    *  10  *  ITAG1  *     pointer to the first tag                      *
*.    *  11  *  NCHRZ  *     Number of characters in RZ dir (may be 0)     *
*.    *  12  *  CHRZ1  *     1st word of RZ directory                      *
*.    *  13  *  CHRZ2  *     2nd word,etc                                  *
*.    *      *   ...   *                                                   *
*.    * ITAG1*  TAG1   *     1st tag (8 characters on 2 words)             *
*.    *      *   ...   *                                                   *
*.    * ITIT1*  TITLE1 *     1st word of title                             *
*.    *      *   ...   *                                                   *
*.    **********************************************************************
*.
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcform.inc"
#include "hbook/hcprin.inc"
      CHARACTER*(*) CHTAGS(1)
      CHARACTER*(*) CHRZPA,CHTITL
      CHARACTER*80 CHRZ
      CHARACTER*8 CHT1
*.___________________________________________
      IERR=0
      IF(IDD.LE.0)THEN
         CALL HBUG('ID<1 is an illegal identifier','HBOOKN',IDD)
         RETURN
      ENDIF
      IF(IDD.GE.10000)THEN
         CALL HBUG('ID>9999 is an illegal identifier','HBOOKN',IDD)
         RETURN
      ENDIF
*
      NOENT=0
*
*             Check if ID already in the table
*
      ID=IDD
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,ID)
      IF(IDPOS.GT.0)THEN
         CALL HBUG('+Already existing N-tuple replaced','HBOOKN',IDD)
         CALL HDELET(IDD)
         NRHIST=IQ(LCDIR+KNRH)
         IDPOS=-IDPOS+1
      ENDIF
*
*             Check NVAR<513
*
      NDIM=NVAR
      IF(NDIM.LE.0.OR.NDIM.GT.512)THEN
         CALL HBUG('NVAR must be 0<NVAR<513','HBOOKN',IDD)
         IF(NVAR.LE.0)NDIM=1
         IF(NVAR.GT.512)NDIM=512
      ENDIF
*
*             Title transform from various accepted formats
*
      CALL HBTIT(CHTITL,NWTITL,NCHT)
*
      NCHRZ=LENOCC(CHRZPA)
      IF(NCHRZ.GT.78)NCHRZ=78
      IF(NCHRZ.GT.0)THEN
         CHRZ=CHRZPA
         IF(CHRZPA(1:2).NE.'//')THEN
            CHRZ='//'//CHRZPA(1:NCHRZ)
            NCHRZ=NCHRZ+2
         ENDIF
      ENDIF
      NCHR=20
      ITAG1=12+NCHR
      ITIT1=ITAG1+2*NDIM
      IF(NPRIME.GT.10*NDIM)THEN
         NWP=NPRIME
      ELSE
         NWP=10*NDIM
      ENDIF
      NWP=NWP-MOD(NWP,NDIM)
      NWH=NWP
      NWID=NWTITL+ITIT1-1
      NTOT=NWH+NWID+2*NDIM+33
*
*             Enough space left ?
*
      CALL HSPACE(NTOT+1000,'HBOOKN',IDD)
      IF(IERR.NE.0)                    GO TO 99
*
*             Enter ID in the list of ordered IDs
*
      IDPOS=-IDPOS+1
      IF(NRHIST.GE.IQ(LTAB-1))THEN
         CALL MZPUSH(IHDIV,LTAB,500,500,' ')
      ENDIF
      DO 10 I=NRHIST,IDPOS,-1
         IQ(LTAB+I+1)=IQ(LTAB+I)
         LQ(LTAB-I-1)=LQ(LTAB-I)
  10  CONTINUE
*
*             Build histogram data structure
*
      IF(LIDS.EQ.0)THEN
         CALL MZBOOK(IHDIV,LIDS,LCDIR,-2,'HIDN',3,2,NWID,IOHN,0)
         LCID=LIDS
      ELSE
         LLID=LQ(LCDIR-9)
         CALL MZBOOK(IHDIV,LCID,LLID,  0,'HIDN',3,2,NWID,IOHN,0)
      ENDIF
      LQ(LCDIR-9)=LCID
      CALL MZBOOK(IHDIV,LCONT,LCID,-1,'HCON',0,0,NWH,3,0)
      CALL MZBOOK(IHDIV,LLIMS,LCID,-2,'HLIM',0,0,2*NDIM,3,0)
      LQ(LCID-3)=LCONT
*
      IQ(LCID-5)=ID
      IQ(LTAB+IDPOS)=ID
      LQ(LTAB-IDPOS)=LCID
      CALL SBIT1(IQ(LCID+KBITS),4)
*
      IQ(LCID+2)=NDIM
      IQ(LCID+4)=NWP
      IQ(LCID+5)=1
      IQ(LCID+7)=1
      IQ(LCID+8)=NWTITL
      IQ(LCID+9)=ITIT1
      IQ(LCID+10)=ITAG1
      IQ(LCID+11)=NCHRZ
*
*           Copy RZ path, TAGS and title
*
      IF(NCHRZ.NE.0)THEN
         CALL UCTOH(CHRZ,IQ(LCID+12),4,80)
         CALL SBIT1(IQ(LCONT),1)
         IQ(LCID+5)=ID
      ENDIF
      IF(NWTITL.NE.0)THEN
         CALL UCTOH(CHTITL,IQ(LCID+ITIT1),4,NCHT)
      ENDIF
*
*           Initialize boundaries
*
      DO 20 I=1,NDIM
         CHT1=CHTAGS(I)
         CALL UCTOH(CHT1,IQ(LCID+ITAG1+2*I-2),4,8)
         Q(LLIMS+2*I-1)=1.E31
         Q(LLIMS+2*I  )=-1.E31
  20  CONTINUE
*
      NRHIST=NRHIST+1
      IQ(LCDIR+KNRH)=NRHIST
*
  99  RETURN
      END
