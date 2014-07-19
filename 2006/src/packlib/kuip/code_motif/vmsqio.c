/*
 * $Id: vmsqio.c,v 1.1.1.1 1996/03/08 15:33:09 mclareni Exp $
 *
 * $Log: vmsqio.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:09  mclareni
 * Kuip
 *
 */
#if defined(CERNLIB_VAXVMS)
/*CMZ :  2.04/00 04/10/93  13.44.58  by  Alfred Nathaniel*/
/*-- Author :    Fons Rademakers   01/09/93*/

/* Special thanks to C.W. Hobbs for providing these routines */

/***********************************************************************
 ***********************************************************************
 *                                                                     *
 *   Routines for VMS QIO to Xtoolkit Input Event Processing           *
 *                                                                     *
 ***********************************************************************
 ***********************************************************************/


/* OpenVMS System Identification Macros
 *
 * Normally, both VAXC (on OpenVMS VAX only) and DEC C (on both OpenVMS VAX
 * and OpenVMS AXP) define the macros "vms" and "VMS".  DEC C also defines
 * the ANSI compatible "__vms" and "__VMS".
 *
 * If we are running on an Alpha AXP system, the DEC C for OpenVMS AXP
 * compiler will define "__alpha" and "__ALPHA".  Unless a check is made
 * against "__alpha", the VMS code is OK for both VAX and AXP.  Note that
 * the various VAX id macros might be defined on AXP as well.
 *
 * If, however, DEC C is running in strict ANSI89 mode (/STANDARD=ANSI89),
 * only the "__vms" and "__VMS" macros are defined -- "vms" and "VMS" are
 * not defined.  Since not much (including header files) is strictly
 * ANSI89 compliant, this shouldn't be an issue.
 */

/* Include files */

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kmenu.h"

#include <Xm/Xm.h>
#include <Xm/Text.h>

#include "mkutfm.h"
#include "kuip/mkqio.h"

/* Forward declarations */

static void MbxCancelQIO(void);
static void MbxErrMsg(char *buf, int status, Widget text_widget);
static void MbxErrTxt(char *buf, Widget text_widget);
static void MbxReadAST(int astprm);
static void MbxSendQIO(void);


/* Static storage declarations */

MbxMessageRec  MbxMessage;
static int            MbxCancelled=0;


/***********************************************************************
 *                                                                     *
 *   MbxCancelQIO - cancel a read request from the "stdin" mailbox     *
 *                                                                     *
 ***********************************************************************/
static void MbxCancelQIO()

{
   int   sts;

   if (MbxCancelled != 0) return;        /* already cancelled, ignore */

   sts = sys$cancel (MbxMessage.chan);

   MbxCancelled = 1;

   if (!(sts & SS$_NORMAL)) {
      printf("* unable to cancel read, VMS status %%x%X *\n", sts);
   }
}


/***********************************************************************
 *                                                                     *
 *   MbxErrMsg - display a VMS system mailbox error                    *
 *                                                                     *
 ***********************************************************************/
static void MbxErrMsg(buf, status, text_widget)
   char *buf;
   int status;
   Widget text_widget;
{
   char   msgbuf[264];
   char   msgtxt[512];
   int    sts;
   struct dsc$descriptor_s msgdsc =
       {sizeof(msgbuf)-1, DSC$K_DTYPE_T, DSC$K_CLASS_S, NULL};
   msgdsc.dsc$a_pointer = msgbuf;

   /* Print the caller's message */

   sprintf(msgtxt, "* %s, VMS status 0x%08X *\n", buf, status);
   MbxErrTxt(msgtxt, text_widget);

   /* Get the VMS system message text associated with the code */

   sts = sys$getmsg(status, &msgdsc.dsc$w_length, &msgdsc, 15, 0);
   if (!(sts & SS$_NORMAL)) {
      fprintf(stderr,
          "* unable to get message, VMS status %%x%X *\n", sts);
   }

   /* Add terminator to VMS text string, and send */

   msgbuf[msgdsc.dsc$w_length] = '\0';
   sprintf(msgtxt, "* VMS error text '%s' *\n", msgbuf);
   MbxErrTxt(msgtxt, text_widget);
}


/***********************************************************************
 *                                                                     *
 *   MbxErrTxt - display an message in the text widget                 *
 *                                                                     *
 ***********************************************************************/
static void MbxErrTxt(buf, text_widget)
     char *buf;
     Widget text_widget;
{
   fprintf(stderr, "%s", buf);
   if (text_widget) {
     int pos = XmTextGetLastPosition(text_widget);
     XmTextInsert(text_widget, pos, buf);

     pos = XmTextGetLastPosition(text_widget);
     XmTextShowPosition(text_widget, pos);
   }
}


/***********************************************************************
 *                                                                     *
 *   MbxRead - replace "read" call in "get_from_application" callback  *
 *                                                                     *
 ***********************************************************************/
int MbxRead(buf, bufmax, text_widget)
     char *buf;
     int bufmax;
     Widget text_widget;
{
   int    cpylen;
   char   msgtxt[256];

   cpylen = 0;           /* Assume no data read */

   /* ioStatus = 0 can only mean that an I/O is pending, exit now */

   if (MbxMessage.ioStatus == 0) return cpylen;

   /* Check status from read, but first turn ENDOFFILE into normal.
    * Various actions like closing the channel can cause an "EOF Record"
    * to be written to the mail box.
    */

   if (MbxMessage.ioStatus == SS$_ENDOFFILE) MbxMessage.ioStatus = SS$_NORMAL;

   /* If an error, we'll fetch the message associated with the status code
    * and write to STDERR and the output text widget, then continue.  Some
    * conditions do not effect whether or not data was read -- we'll use the
    * MbxMessage.ioLength as the indicator of whether or not data arrived.
    */

   if (MbxMessage.ioStatus != SS$_NORMAL)
      MbxErrMsg("error on read from mailbox", MbxMessage.ioStatus, text_widget);


   if (MbxMessage.ioLength > 0) {

      /* Minimize data length against output buffer */

      cpylen = bufmax;
      if (bufmax > MbxMessage.ioLength) cpylen = MbxMessage.ioLength;

      /* Force a '\n' at the end (mailboxes are records, not stream) */

      if (MbxMessage.buffer[MbxMessage.ioLength-1] != '\n'
          && MbxMessage.buffer[MbxMessage.ioLength-1] != '\2') {
         MbxMessage.buffer[cpylen] = '\n';
         cpylen = cpylen + 1;
      }

      /* Check that 'strlen' matches the data length -- if not, then
       * something is confused and we should warn (data will be lost)
       */

      MbxMessage.buffer[cpylen] = '\0';
      memcpy(buf,&MbxMessage.buffer[0],cpylen);
      if (strlen(MbxMessage.buffer) != cpylen) {
         sprintf(msgtxt,
            "* input mismatch, read %d bytes, strlen = %d bytes *\n",
            cpylen, strlen(MbxMessage.buffer));
         MbxErrTxt(msgtxt, text_widget);
      }
   }

   /* Queue the next I/O.  Note that this clears the event flag and the
    * entire IOSB (ioStatus/ioLength/ioSenderPID), lowering the X Input
    * event condition.
    */

   MbxSendQIO();

   return cpylen;
}


/***********************************************************************
 *                                                                     *
 *   MbxReadAST - Asynchronous System Trap for Mailbox QIO Completion  *
 *                                                                     *
 *        Note: Note that this routine is not needed for correct       *
 *              processing, but can be used as "debug" checking to     *
 *              make sure that the X callback logic is correct.        *
 *                                                                     *
 *     NOTE:  This routine is called asynchronously, and interrupts    *
 *            normal program flow.  Note that X Windows and all of     *
 *            the Motif and toolkit routines do not allow AST threads  *
 *            to call X routines or modify X data structures.  Calling *
 *            any window routine can cause (perhaps delayed) failure.  *
 *                                                                     *
 ***********************************************************************/
static void MbxReadAST(astprm)
   int astprm;
{
   fprintf(stderr,"PID %X/MbxReadAST: status %%x%X,  length %d\n",
        getpid(), MbxMessage.ioStatus, MbxMessage.ioLength);
}


/***********************************************************************
 *                                                                     *
 *   MbxSendQIO - issue a read request from the "stdin" mailbox        *
 *                                                                     *
 ***********************************************************************/
static void MbxSendQIO()

{
   int   sts;

   /* If we have cancelled the mailbox I/O, ignore the request */

   if (MbxCancelled != 0) return;

   /* Send the request to the I/O system (without waiting) */

   sts = sys$qio (
                MBX_EVENT_FLAG,              /* synch flag                   */
                MbxMessage.chan,             /* I/O channel number           */
                IO$_READVBLK,                /* function code (virtual read) */
                &MbxMessage.ioStatus,        /* I/O Status Block             */
                0, /* MbxReadAST,            /* Completion callback routine  */
                &MbxMessage,                 /* Callback parameter           */
                &MbxMessage.buffer[0],       /* Buffer address               */
                MbxMessage.maxread,          /* Buffer length                */
                0, 0, 0, 0);                 /* (not used in this call)      */

   /* Check status.  If an error, we will abort the program.  Note that there
    * are two status values for a SYS$QIO call (and SYS$GETDVI):
    *
    *      function value returned from call to SYS$QIO:
    *
    *             This status tells whether or not the read request was sent
    *             off to be handled (asynchronously) by the I/O system.  If
    *             this reflects failure, then there is a serious problem with
    *             the format of the call or the state of the I/O system.
    *             Since it is unlikely that a $QIO error will clear itself
    *             up, exit the entire program if we have an error.
    *
    *      status written to MbxMessage.ioStatus:
    *
    *             This status tells what the I/O system did with the request.
    *             This field is set to 0 by the SYS$QIO call, and is written
    *             asynchronously when the I/O system completes the request.
    */

   if (!(sts & SS$_NORMAL)) {
      fprintf(stderr,
          "* unable to queue read to mailbox, VMS status %%x%X *\n", sts);
      exit(sts);
   }
}


/***********************************************************************
 *                                                                     *
 *   MbxSetup  - initialize the mailbox interface and start the first  *
 *               I/O operation                                         *
 *                                                                     *
 ***********************************************************************/
void MbxSetup()

{
   char  mbxname[256];
   int   mbxbuflen;
   int   fn, sts;
   struct dsc$descriptor_s devnam = {
     (sizeof mbxname)-1, DSC$K_DTYPE_T, DSC$K_CLASS_S, NULL};
   ItemList  GetdviItems[2] = {
             {sizeof(mbxbuflen), DVI$_DEVBUFSIZ, NULL, NULL},
             {0,0,NULL,NULL} };

   devnam.dsc$a_pointer = mbxname;
   GetdviItems[0].BufferP = &mbxbuflen;

   /* Get name of mailbox into descriptor, and assign an I/O channel */

   getname(fileno(stdin),mbxname,1);
   devnam.dsc$w_length = strlen(mbxname);

   sts = sys$assign (&devnam, &MbxMessage.chan, PSL$C_USER, NULL);

   if (!(sts & SS$_NORMAL)) {
      fprintf(stderr,
          "* unable to assign channel to mailbox, VMS status %%x%X *\n",
          sts);
      exit(sts);
   }

   /* Get the buffer size of the mailbox, so we know how much to read */

   sts = sys$getdviw (
            MBX_EVENT_FLAG,         /* use for synch                    */
            MbxMessage.chan,        /* Mailbox I/O channel              */
            0,                      /* device name (not used with chan) */
            &GetdviItems,           /* item list                        */
            &MbxMessage.ioStatus,   /* I/O status block                 */
            0,0,0);                 /* not used in this call            */

   /* Like QIO, GETDVI returns function status and IOSB status.  If the
    * function status is OK copy the IOSB status and recheck.
    */
   if (sts & SS$_NORMAL) sts = MbxMessage.ioStatus;
   if (!(sts & SS$_NORMAL)) {
      fprintf(stderr,
          "* unable to get mailbox info, VMS status %%x%X *\n", sts);
      exit(sts);
   }

   /* Choose the smaller of the mailbox size and the buffer size for I/O,
    * leaving two extra bytes for a '\n\0'
    */

   MbxMessage.maxread = mbxbuflen;
   if (mbxbuflen > (MBX_MAX_MSGSIZE-2)) MbxMessage.maxread = MBX_MAX_MSGSIZE-2;

   /* Start the first maibox QIO */

   MbxCancelled = 0;         /* Reset the cancelled flag */
   MbxSendQIO ();

}


/***********************************************************************
 *                                                                     *
 *   VMScrelnm - create logical name for device                        *
 *                                                                     *
 ***********************************************************************/
void VMScrelnm(lognam, devnam)
     char *lognam;
     char *devnam;
{
   int     sts;
   short  iosb[4];
   char   dvibuf[256];
   struct dsc$descriptor_s devdsc;
   struct dsc$descriptor_s logdsc;
   $DESCRIPTOR(dsc_prctbl,"LNM$PROCESS_TABLE");
   ItemList  CrelnmItems[2] = {
             {sizeof(dvibuf), LNM$_STRING, NULL, NULL},
             {0,0,NULL,NULL} };
   ItemList  GetdviItems[2] = {
             {sizeof(dvibuf), DVI$_ALLDEVNAM, NULL, NULL},
             {0,0,NULL,NULL} };

   CrelnmItems[0].BufferP = dvibuf;
   GetdviItems[0].BufferP = dvibuf;
   GetdviItems[0].RetLenP = &CrelnmItems[0].BufLen;

   /* Get canonical device name */

   devdsc.dsc$b_dtype   = DSC$K_DTYPE_T;
   devdsc.dsc$b_class   = DSC$K_CLASS_S;
   devdsc.dsc$a_pointer = devnam;
   devdsc.dsc$w_length  = strlen( devdsc.dsc$a_pointer );

   sts = sys$getdviw (
            MISC_EVENT_FLAG,        /* use for synch                    */
            0,                      /* channel (not used with name)     */
            &devdsc,                /* device name                      */
            &GetdviItems,           /* item list                        */
            &iosb[0],               /* I/O status block                 */
            0,0,0);                 /* not used in this call            */

   if (sts & SS$_NORMAL) sts = iosb[0];
   if (!(sts & SS$_NORMAL)) {
      fprintf(stderr,
              "* unable to get device name, VMS status %%x%X *\n", sts);
   }

   /* Create user mode logical name */

   logdsc.dsc$b_dtype   = DSC$K_DTYPE_T;
   logdsc.dsc$b_class   = DSC$K_CLASS_S;
   logdsc.dsc$a_pointer = lognam;
   logdsc.dsc$w_length  = strlen( logdsc.dsc$a_pointer );

   sts = sys$crelnm (0,&dsc_prctbl,&logdsc,0,&CrelnmItems);

   if (sts != SS$_NORMAL) {
      if (sts != SS$_SUPERSEDE) {
         printf("* unable to crelnm, VMS status %%x%X *\n", sts);
      }
   }
}

/***********************************************************************
 *                                                                     *
 *   VMSsystem - replace 'system' call with LIB$SPAWN so that we can   *
 *               connect stdin/stdout from subprocess                  *
 *                                                                     *
 ***********************************************************************/
void VMSsystem(buf)
   char *buf;
{
   int     sts;
   char    outname[256];

   struct dsc$descriptor_s cmd_string;
   struct dsc$descriptor_s mbx_name;

   cmd_string.dsc$b_dtype   = DSC$K_DTYPE_T;
   cmd_string.dsc$b_class   = DSC$K_CLASS_S;
   cmd_string.dsc$a_pointer = buf;
   cmd_string.dsc$w_length  = strlen( cmd_string.dsc$a_pointer );

   /* Get names of STDOUT */

   getname(fileno(stdout),outname,1);

   mbx_name.dsc$b_dtype   = DSC$K_DTYPE_T;
   mbx_name.dsc$b_class   = DSC$K_CLASS_S;
   mbx_name.dsc$a_pointer = outname;
   mbx_name.dsc$w_length  = strlen( mbx_name.dsc$a_pointer );

   /* Spawn process to execute command, using our input and output */

   sts = lib$spawn (&cmd_string, NULL, &mbx_name);

   if (!(sts & SS$_NORMAL)) {
      printf("* unable to spawn command, VMS status %%x%X *\n", sts);
   }

}


#ifndef __DECC
/*
 * VAXC/RTL does not support wchar --- but they are not used anyway
 */
size_t mbstowcs(wchar_t *pwcs, const char *s, size_t n)
{
  printf( " *** Dummy mbstowcs() called !!!\n" );
  return -1;
}
#endif
#endif
