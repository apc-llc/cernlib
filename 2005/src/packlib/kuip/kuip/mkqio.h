/*
 * $Id: mkqio.h,v 1.1.1.1 1996/03/08 15:32:59 mclareni Exp $
 *
 * $Log: mkqio.h,v $
 * Revision 1.1.1.1  1996/03/08 15:32:59  mclareni
 * Kuip
 *
 */
#if defined(CERNLIB_VMS)
/***  Routines for VMS QIO to Xtoolkit Input Event Processing ***/
#ifndef _mkqio_
#define _mkqio_

/* Constants */

#define MBX_EVENT_FLAG    23            /* event flag for add input synch  */
#define MBX_MAX_MSGSIZE 1500
#define MISC_EVENT_FLAG    1            /* event flag for misc synch  */

/* Structure declarations */

typedef struct _ItemList {
   unsigned short     BufLen;
   short              ItemCode;
   void               *BufferP;
   unsigned short     *RetLenP;
} ItemList;

typedef struct _MbxMessageRec {
   unsigned short     ioStatus;       /* Note: I/O status block is 8 bytes.  */
   unsigned short     ioLength;       /*       For mailbox reads, it is      */
   unsigned int       ioSenderPID;    /*       stat/leng/PID - don't split   */
   int                msgcnt;
   int                bytcnt;
   int                maxread;
   int                chan;
   char               buffer[MBX_MAX_MSGSIZE];
} MbxMessageRec;

extern MbxMessageRec  MbxMessage;


extern C_PROTO_3(int           MbxRead,
                                  char *buf,
                                  int bufmax,
                                  Widget text_widget);
extern C_PROTO_0(void          MbxSetup);
extern C_PROTO_2(void          VMScrelnm,
                                  char *lognam,
                                  char *devnam);
extern C_PROTO_1(void          VMSsystem,
                                  char *buf);

#endif /* _mkqio_ */
#endif
