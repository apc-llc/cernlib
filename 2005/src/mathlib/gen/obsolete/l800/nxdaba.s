*
* $Id: nxdaba.s,v 1.1.1.1 1996/04/01 15:03:19 mclareni Exp $
*
* $Log: nxdaba.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:19  mclareni
* Mathlib gen
*
*
#if defined(CERNLIB_NOSBE)||!defined(CERNLIB_F4)
          IDENT  NXDABA
*
*         NDABA = NXDABA (DUM)
*
*         NXDABA EXPECTS THAT CMM IS ACTIVE
*         IF CMM IS NOT ACTIVE OR IF THE CMM STRUCTURE IS INVALID IT RETURNS -1
*         OTHERWISE IT RETURNS THE LAST SCM LOC THAT MAY BE USED BY THE PROGRAM
*
          ENTRY  NXDABA
 IFL      BSSZ   1
 IDABA1   BSSZ   1
 IDABA2   BSSZ   1
 FNAME    VFD    42/0LNXDABA,18/NXDABA
 NXDABA   DATA   0
          SA5    65B
          MX0    18
          LX0    18          X0=MASK
          BX5    X5*X0
          SB7    X5
          PL     B7,ERROR    JUMP, CMM NOT ACTIVE
          SB7    -B7         DABA VALUE
          SX7    B7
          SA7    IDABA1      CURRENT DABA VALUE
          SA5    B7
          BX6    X5*X0
          SA7    IDABA2      NEXT DABA VALUE
          SA6    IFL         FL
          BX7    -X0*X5
          LX7    3
          SX5    6
          IX5    X7-X5
          NZ     X5,ERROR    JUMP, INVALID CMM STRUCTURE
          SB1    1           B1=1
          SB6    B7+B1
*
*         FIND THE FIRST CMM BLOCK IN USE
*
 LOOP     BSS    0
          SA5    B6
          BX6    X0*X5       ADDRESS OF NEXT CMM BLOCK
          BX5    -X0*X5
          LX5    60-18
          BX7    X0*X5       ADDRESS OF PREVIOUS BLOCK
          SB5    X7
          NE     B5,B7,ERROR JUMP,INVALID CMM BLOCK
          SB7    B6          PREVIOUS CMM BLOCK
          SB6    X6          NEXT CMM BLOCK
          AX5    18
          ZR     X5,LOOP     JUMP, THIS CMM BLOCK IS FREE
          MX5    59
          SX6    B7          ADDRESS OF LAST FREE CMM BLOCK
          IX6    X6+X5
          SA6    IDABA2      ADDRESS OF FIRST CMM FREE BLOCK
          BX1    X6
          RJ     =XCMM.SDA
 RETURN   SA1    IDABA2      RESTORE NEW DABA VALUE
          MX5    59
          IX6    X5+X1
          EQ     NXDABA
*
*         ERROR PROCESSING
 ERROR    BSS    0
          MX6    59          RETURN ERROR CODE
          EQ     NXDABA      RETURN
*
          END
#endif
