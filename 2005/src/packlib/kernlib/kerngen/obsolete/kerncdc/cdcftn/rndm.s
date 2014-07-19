*
* $Id: rndm.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: rndm.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT RNDM
*
* CERN PROGLIB# V104    RNDM            .VERSION KERNCDC  1.02  771015
*
          ENTRY RNDM,IRNDM,RDMIN,RDMOUT
*
*     A =  RNDM (DUMMY)
*     I = IRNDM (DUMMY)
*     CALL  RDMIN  (OCT)
*     CALL  RDMOUT (OCT)
*
*         PROGRAM TAKEN FROM CERN PROGLIB  15-OCT-77
*
 NAME1    VFD   24/4HRNDM,36/RNDM
 RNDM     DATA  0
          SA2   R
          SA3   FIVE
          SA4   RNDM
          DX0   X3*X2
          MX1   13
          BX5   -X1*X0
          LX4   30
          SB5   -47
          PX7   X5,B0
          SB7   X4
          NO
          PX1   X5,B5
          SA7   A2
          NX6   X1,B2
          JP    B7
 NAME2    VFD   30/5HIRNDM,30/IRNDM
 IRNDM    DATA  0
          SA2   R
          SA3   FIVE
          MX1   13
          SA4   IRNDM
          DX0   X3*X2
          FX7   X3*X2
          BX2   -X1*X0
          LX4   30
          SB7   X4
          MX5   49
          BX3   -X5*X7
          PX7   X2,B0
          BX5   X5*X2
          IX6   X5+X3
          SA7   A2
          JP    B7
 NAME3    VFD   30/5HRDMIN,30/RDMIN
 RDMIN    DATA  0
          SA4   RDMIN
          SA3   X1                      ******
          MX1   13
          BX6   -X1*X3
          LX4   30
          SB7   X4
          PX7   X6,B0
          SA7   R
          JP    B7
 NAME4    VFD   36/6HRDMOUT,24/RDMOUT
 RDMOUT   DATA  0
          SA4   RDMOUT
          SA5   R
          LX4   30
          SB7   X4
          NO
          UX6   B2,X5
          SA6   X1                     ****
          JP    B7
 R        DATA  20000000011060471625B
 FIVE     DATA  20000000343277244615B
          END
