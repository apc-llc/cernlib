*
* $Id: fatpara%.inc,v 1.1.1.1 1996/03/07 15:17:35 mclareni Exp $
*
* $Log: fatpara%.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:35  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATPARA%_INC
#define CERNLIB_FATMEN_FATPARA%_INC
*
*
* fatpara%.inc
*
#if defined(CERNLIB__DOC)
#include "fatmen/fatpara.inc"
 
             K E Y S
 
    MKSRFA      Key serial number
    MKFNFA      File name
    MKCLFA      Copy level
    MKMTFA      Medium type
    MKLCFA      Location type
    MKNBFA      Number of bank in the data set linear chain
 
    NKDSFA      Number of keys
 
 
              B A N K
 
words    field          type   length     description
                              (in words)
 
         ---------     generic description  -------------
 
(* = NOT NULL in ORACLE or SQL/DS tables - filled in by FMLIFT)
 
1-64  * fully qual. name H        64      in case of instance the generi
                                          name of the corresponding data
                                          set
                                          dataset name has format
                                          <user.addr>fn.ft on VM,
                                          where '.addr' may be omitted.
65-66 * host name        H         2      in case of tape the nearest
                                          host to its physical
                                          location
                                          (maybe more words to clearly
                                          state the location (lab.
                                          bld. etc) would be needed)
67    * copy level       I         1      0 master 1 duplicate
                                        2 duplicate from a duplicate etc
68    * location code    I         1   to find the nearest  (FATMEN code
69    * medium type      I         1      3=tape, 2=cartridge,
                                          4=exabyte, 1=disk
                                          ...................
                                          0=instance
 
 
 
 
         ---------     disk description     -------------
 
70-73   host type        H         4         only for disk files
74-76   operating syst.  H         3           "
 
         ---------     tape description     -------------
 
77-78 * VSN              H         2         only for tapes
79-80 * VID              H         2           "
81      VID prefix       I         1           "
82      Density          I         1           "
83      vol. seq. numb.  I         1           "
84    * file seq. numb.  I         1           "
 
         ---------     file description     -------------
 
85      start record #   I         1
86      end   record #   I         1
87      start block  #   I         1
88      end   block  #   I         1
 
         ---------  physical description    -------------
 
89      record format    H         1
90      record length    I         1        (in words!)
91      block  length    I         1            "
106     file   size      I         1        (in Megabytes, rounded up
        = SYSWD0                             to next integer)
 
         ---------  logical description     -------------
 
92    * file format      H         1     FATMEN oriented
                                         FZ=zebra native
                                         FA=zebra ascii
                                         FX=zebra exchange
                                         RZ=zebra RZ
                                         EP=EPIO
                                         AS=normal Ascii
                                         UN=unknown to FATMEN
                                         ...............
93      file user type   H         1     user oriented and user defined
 
         ---------    dates and times       -------------
 
94      creation date    I         1
95      catalog. date    I         1
96      last acc.date    I         1
 
         ---------   creator identier       -------------
 
97-98   user name creat. H         2     "creator" user name
99-100  user id.    "    H         2               user id. UUUGG/UUU$GG
                                                            or NFSID
101-102 node id.    "    H         2               node id.
103-104 job id.     "    H         2               job  id.
                                         (user defined)
 
         ---------      protections         -------------
 
105      file protect.   I         1     (maybe just a pointer in a
                                          ACL definition bank)
 
         ---------       user area          -------------
 
106-115  system words    I        10     (reserved)
116-125  user words      I        10     (user defined)
126-145  comment         H        20
 
#endif

#endif
