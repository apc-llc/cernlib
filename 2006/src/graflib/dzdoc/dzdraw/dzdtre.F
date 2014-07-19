*
* $Id: dzdtre.F,v 1.1.1.1 1996/03/04 16:13:09 mclareni Exp $
*
* $Log: dzdtre.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:09  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDTRE(LB,IB,LD,ILINK,XPOSB,YPOSB,Y0DB,
     &   REQINP,L,XCUR,YCUR,IFLCUR, ILLFLG)
      INTEGER IB(*),LB(*),LD, L, IFLCUR, DZDGLK
      REAL XPOSB,YPOSB,XCUR,YCUR
      LOGICAL REQINP
#include "dzdprm.inc"
      CHARACTER*4  HID, HIDUXX, HIDNXX
*---
      CALL DZDGNZ(LB,IB,LD,1,NLDON1,NBLEV, NULEV,NN)
      NSD=IB(LD-2)
      XPOSDB = XPOSB + 4.*DXLK
*        draw up to the highest non zero link
         LDD = DZDGLK(LB,IB,LD,ILINK)
         X01 = XPOSB - FLOAT(ILINK)*DXLK
         IF(LDD .EQ. 0)THEN
            IF(.NOT. REQINP)THEN
*              draw the little box for the link
            CALL DZDSBX(X01,YPOSB,ILINK,NSD)
            ENDIF
            GOTO 90
         ENDIF
         Y0DB = Y0DB - 2.*GRIDY
         IF(REQINP)THEN
            CALL DZDCKC(XCUR,YCUR,XPOSDB,Y0DB,LB,IB,LDD,IFLCUR)
            IF(IFLCUR .GT. 0)L = LDD
            GOTO 90
         ELSE
            CALL DZDPAR(LB,IB,LDD,
     +      HID, NID, HIDUXX, HIDNXX, JBIASX, NLDD, NSDD, NDDD)
            CALL DZDBNK(LB,IB,LDD,
     +      XPOSDB,Y0DB,DXTB,DYB,HID,HIDNXX,HIDUXX,
     +      NID, NDDD)
*           restore link
            LDD = LB(LD-ILINK)
            CALL DZDLNK(X01,YPOSB,XPOSDB,Y0DB,ILINK, NSD)
         ENDIF
 
*        is it lowest bank then indicate links if not 0
 
         IF(ILLFLG.NE.0)THEN
            CALL DZDGNZ(LB,IB,LDD,1,NLDON1,NBLEV, NULEV,NN)
            IF(NULEV.GT.0)THEN
               CALL DZDLBX(XPOSDB,Y0DB,1,NULEV,NULEV)
            ENDIF
         ENDIF
 
*    if only 1 link given draw a box indicating # of links
         IF(ILINK.EQ. 1 .AND. NBLEV.EQ.1 .AND. .NOT. REQINP)THEN
            IF(IFRCAR .NE. 0 .AND. IB(LD+2) .LT. 0)THEN
               NS1 = -1
            ELSE
               NS1 = NSD
            ENDIF
            IF(NS1.NE.1 .AND. IFRCAR .NE. 0)THEN
               CALL DZDLBX(XPOSB-DXLK, YPOSB, 2,NS1,NSD)
            ENDIF
*           GOTO 70
         ENDIF
   90 END
******************************************************************
