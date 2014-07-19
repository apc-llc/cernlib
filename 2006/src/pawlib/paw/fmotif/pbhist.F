*
* $Id: pbhist.F,v 1.1.1.1 1996/03/01 11:39:05 mclareni Exp $
*
* $Log: pbhist.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:05  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.06/11 19/01/95  16.26.46  by  O.Couet
*-- Author :    Alfred Nathaniel   07/08/92
      SUBROUTINE PBHIST(BRNAME,BRCLAS,BRPATH,OBNAME,OBCLAS,STEXT,LTEXT)
      CHARACTER*(*) BRNAME,BRCLAS,BRPATH,OBNAME,OBCLAS,STEXT,LTEXT
*
*            Browser interface to return next histogram
*
      CHARACTER UPPATH*128,CHPATH*128,CHTITL*80,CHTYPE*1,CHPICT*32
      LOGICAL LASTP
      SAVE ID,LASTP,CHPICT
*-------------------------------------------------
*
*            save current directory
*
      IF(BRCLAS.EQ.'HIGZ')RETURN
      CALL HCDIR(CHPATH,'R')
      UPPATH=BRPATH
      CALL CLTOU(UPPATH)
      CALL HCDIR(UPPATH,' ')
*
      IF(OBNAME.EQ.' ') THEN
         ID=0
         CALL IZPICT(' ','QF')
         CALL IZPICT(CHPICT,'R')
         IF(CHPICT.NE.' ')THEN
            LASTP=.FALSE.
         ELSE
            LASTP=.TRUE.
         ENDIF
      ENDIF
*
*            Retrieve the pictures in memory
*
      IF(UPPATH.EQ.'//PAWC'.AND..NOT.LASTP)THEN
         CALL IZPICT(OBNAME,'R')
         OBCLAS='PICT'
         STEXT='(Picture)'
         LTEXT=' '
         CALL IZPICT(OBNAME,'QNR')
         IF(CHPICT.EQ.OBNAME)LASTP=.TRUE.
         GOTO 999
      ENDIF
*
*            Retrieve the histograms in memory
*
 1    CONTINUE
      CALL HLNEXT(ID,CHTYPE,CHTITL,'12ND')
      IF(ID.EQ.0) THEN
         OBNAME=' '
      ELSEIF(CHTYPE.EQ.'D') THEN
         OBCLAS='DIR'
         OBNAME=CHTITL
         STEXT=' '
         LTEXT='directory'
      ELSE
         WRITE(OBNAME,'(I12)') ID
         IF(CHTYPE.EQ.'1') THEN
            OBCLAS='1D'
            STEXT='(1d)'
         ELSEIF(CHTYPE.EQ.'2') THEN
            OBCLAS='2D'
            STEXT='(2d)'
         ELSEIF(CHTYPE.EQ.'N') THEN
            OBCLAS='NTUPLE'
            STEXT='(Ntuple)'
         ELSE
            GOTO 1
         ENDIF
         ILENT=INDEX(CHTITL,';')
         IF(ILENT.EQ.0)THEN
            LTEXT=CHTITL
         ELSE
            LTEXT=CHTITL(1:ILENT-1)
         ENDIF
      ENDIF
 999  CALL HCDIR(CHPATH,' ')
      END
