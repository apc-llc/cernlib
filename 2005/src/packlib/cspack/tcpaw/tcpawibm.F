*
* $Id: tcpawibm.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: tcpawibm.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_TCPSOCK))
* For systems without SAA C compiler or TCP/IP V2.0
#endif
/**********************************************************************/
/*                                                                    */
/*              PASCAL TCP/IP PACKAGE FOR PAW                         */
/*                                                                    */
/*              C.Magnin  / CERN DD SW VX /                           */
/*                                                                    */
/* Modified 13-AUG-90 J.D. Shiers to call ReadNE (Read No Echo) to    */
/* get password. READNE uses LINERD macro with TYPE=INVISIBLE.        */
/*                                                                    */
/*    Designed to be called from FORTRAN, once for SETUP and          */
/*    once for each data transfer (send/receive).                     */
/*    At exit time, the CLOSE  routine should be run.                 */
/*                                                                    */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
 
segment IBMPAW;
 
%include CMALLCL
%include CMINTER
%include CMRESGLB
%include CMEBCASC
%include CMS
 
const
   BUFFERClength = 20480;
   BUFFERBlength = 2048;
   OPENtimeout   = 120;
/* OPENtimeout   = 60;*/
 
type
   String30  = Packed Array [1..30] Of Char;
   String80  = Packed Array [1..80] Of Char;
   Buffertc = Packed array [1..BUFFERClength] of char;
   Buffertb = Array [1..BUFFERBlength] of integer;
 
#if defined(CERNLIB_IBMVM)
   Procedure ReadNE (var Password: String30;
                     var lenp    : integer);
             external;
#endif
 
   procedure Restore ( const Message: string;
                       const ReturnCode: integer );
   begin
      Write (Message);
      if ReturnCode <> OK then
         Write (SayCalRe(ReturnCode));
      WriteLn ('');
 
      EndTcpIp;
      Close (Input);
      Close (Output);
   end;
 
   procedure SASCIITOE
                     (var Bufascii: buffertc;
                      var Lbufasci: integer);
   VAR
   I: integer;
   C: char;
 
   begin
   for I := 1 to  Lbufasci DO
       begin
         c := bufascii[I];
         bufascii[I] := Asciitoebcdic[c];
       end;
   end;
 
 
   procedure SEBCDICTOA
                     (var Bufebcdic: buffertc;
                      var Lbufebcdi: integer);
   VAR
   I: integer;
   c: char;
 
   begin
   for I := 1 to  Lbufebcdi DO
       begin
         c := bufebcdic[I];
         bufebcdic[I] := chr(Ebcdictoascii[c]);
       end;
   end;
 
   /*******************************************/
   /*                                         */
   /*    CLIENT   SET UP    FUNCTION          */
   /*                                         */
   /*******************************************/
 
 
   Procedure CSETUP( var Host: String80;
                     var Sock: integer;
                     var Serv: String80);
 
     Main;
   Procedure CSETUP;
 
   const
/* Userpassl = 65; */
   Userpassl = 85;
   Replylmax =120;
 
 
   type
 
   Byt = packed 0..255;
 
 
   Janus = record
           case integer of
           0:(Byte: Byt);
           1:(Ch:char);
           end;
 
   var
   ConnectionInfo : StatusInfoType;
   HostAddress    : InternetAddressType;
   HostName       : String(80);
   Username       : Packed array [1..30] of char;
   Username2      : Packed array [1..30] of char;
   Usernames      : String(30);
   Password       : Packed Array [1..30] of char;
   ReturnCode     : integer;
   Ret            : integer;
   PORTnumber     : integer;
   Replyl         : integer;
   I              : integer;
   J              : integer;
   K              : integer;
   L              : integer;
   Lenp           : integer;
   Temp           : Janus;
   Mask           : Byt;
   Userpass       : Packed Array [1..Userpassl] of char;
   Reply          : Packed Array [1..Replylmax] of char;
   ReplyA         : Packed Array [1..20480] of char;
#if !defined(CERNLIB_TCPIPV2)
   Userpassad     : AddressType;
   Replyad        : AddressType;
#endif
#if (defined(CERNLIB_TCPIPV2))&&(!defined(CERNLIB_QMIBMXA))
   Userpassad     : Address24Type;
   Replyad        : Address24Type;
#endif
#if (defined(CERNLIB_TCPIPV2))&&(defined(CERNLIB_QMIBMXA))
   Userpassad     : Address31Type;
   Replyad        : Address31Type;
#endif
#if defined(CERNLIB_IBMMVS)
 
   cuid           : useridType;
   cprompt : packed array [1..80] of char;
   lin, lout, nn : integer;
 
   /* return jobname, first 4 characters are uid at GSI */
   procedure jobnam(var cuid : useridType);
          external;
   /* read password with no echo   */
   Procedure getinh( var  cbufin: string80;
                     var  lbufin: integer;
                     var cbufout: string30;
                     var lbufout: integer;
                     var irc: integer );
   external;
#endif
 
   begin
 
   TermOut (Output);
   TermIn (Input);
 
   /* Establish access to TCP/IP services    */
 
 
   BeginTcpIp (ReturnCode);
   if ReturnCode <> OK then begin
      Sock := -1;
      WriteLn ('BeginTcpip: ', SayCalRe(ReturnCode));
      return;
   end;
 
 
   /* Inform TCPIP which notifications will be handled by the program */
 
   Handle ((.DATAdelivered, BUFFERspaceAVAILABLE,
             CONNECTIONstateCHANGED.), ReturnCode);
   if ReturnCode <> OK then begin
      Sock := -1;
      Restore ('Handle: ', ReturnCode);
      return;
   end;
 
   /* Convert Host name Packed Array into string */
 
   Hostname :=  STR(Host);
 
   /* Convert the Host name into an Internet address    */
 
   GetHostResol (Hostname, HostAddress);
   if HostAddress = NOhost then begin
      Sock := -1;
      Restore ('GetHostResol failed', OK);
      return;
   end;
 
   /* Assign value to Portnumber depending on the input Serv */
 
   If Serv='example' then PORTnumber:=2345
                     else
   If Serv='pawserv' then PORTnumber:=345
                     else
   If Serv='zserv'   then PORTnumber:=346
                     else
 
   /* If serv is not one of the 3 previous values    */
   /* it is assumed that Serv is the port number */
 
                     READSTR(STR(Serv),PORTnumber);
 
   /* Open an active TCP connection in synchronous mode */
 
   with ConnectionInfo do begin
 
      Connection            := UNSPECIFIEDconnection;
      OpenAttemptTimeout    := OPENtimeout;
      Security              := DEFAULTsecurity;
      Compartment           := DEFAULTcompartment;
      Precedence            := DEFAULTprecedence;
      ConnectionState       := TRYINGtoOPEN;
      LocalSocket.Address   := UNSPECIFIEDaddress;
      LocalSocket.Port      := UNSPECIFIEDport;
      ForeignSocket.Address := HostAddress;
      ForeignSocket.Port    := PORTnumber;
 
   end;
 
   TcpWaitOpen (ConnectionInfo, ReturnCode);
 
   if ReturnCode <> OK then begin
      Sock := -1;
      Restore ('TcpWaitOpen: ', ReturnCode);
      return;
   end;
 
   /* Open successful TCP has updated the Connection field */
 
   Sock   := ConnectionInfo.Connection;
 
   /* Following code performs client user authentication */
 
   /* Get username and password to be sent to the server */
 
   For I := 1 to 30 DO Username[I]:=' ';
   For I := 1 to 30 DO Username2[I]:=' ';
   For I := 1 to 30 DO Password[I]:=' ';
 
#if defined(CERNLIB_IBMVM)
   /* Get VM userid issuing the CMS command, to fill the prompt */
 
   CMS ('Q USERID(LIFO',Ret);  /* Change to LIFO 22/02/91 JDS */
   ReadLn (Username);
   I:=1;
   repeat
     I:=I+1;
   until Username[I]=' ';
   repeat
     I:=I+1;
     Username[I]:=' ';
   until I=30;
   Usernames := STR(Username);
   LowCase(Usernames);
   For I:=1 TO 30 DO Username[I]:=Usernames[I];
#endif
#if defined(CERNLIB_IBMMVS)
   /* Get MVS userid by calling JBNAME Assembler Routine */
   jobnam(cuid);
   For I:=5 To 8 DO cuid[I] := ' ';    /* only userid */
   Usernames := str(cuid);
   LowCase(Usernames);
   For I:=1 TO 8 DO Username[I]:=Usernames[I];
#endif
   WriteLn('Username: ','(',MTRIM(MCOMPRESS(Hostname)),':',
   MTRIM(MCOMPRESS(SUBSTR(Usernames,1,8))),'):');
 
   ReadLn (Username2);
   If username2[1]<>' ' Then
         For I:=1 TO 30 DO Username[I]:=Username2[I];
 
   Usernames := STR(Username);
   LowCase(Usernames);
   WriteLn ('Password: ','(',MTRIM(MCOMPRESS(Hostname)),':',
   MTRIM(MCOMPRESS(SUBSTR(Usernames,1,15))),'):');
#if defined(CERNLIB_IBMVM)
/* ReadLn (Password);*/
   ReadNE (Password,lenp);   /* Use modified version of VMRTNE */
#endif
#if defined(CERNLIB_IBMMVS)
   cprompt := ' ';
   lin := 1;
   lout := 30;
   nn := 0;
   getinh( cprompt, lin, Password, lout, nn );
#endif
   If Password[1]=' ' Then
        For I:=1 To 30 DO Password[I]:=Username[I];
 
   /* writeln('Before encoding, we have: ',Username,password); */
 
   /* Encode user name and password by applying a bitwise complement */
   /* Tricky Janus type used since the NOT operator cannot directly  */
   /* apply to a CHAR variable */
 
   Mask:=255;
   I:=0;
   repeat
     I:=I+1;
     Temp.Ch:=Chr(Ebcdictoascii[Username[I]]);
     Temp.Byte:= NOT(Temp.Byte) AND Mask;
     Userpass[I]:=Temp.Ch;
   until (Username[I]=' ') OR (I=30);
 
     K:=0;
   repeat
     I:=I+1;
     K:=K+1;
     Temp.Ch:=Chr(Ebcdictoascii[Password[K]]);
     Temp.Byte:= NOT(Temp.Byte) AND Mask;
     Userpass[I]:=Temp.Ch;
   until (Password[K]=' ') OR (K=30);
 
   /* Put a zero at the end of Userpass to keep C happy */
   Temp.Byte:=0;
   Userpass[I]:=Temp.Ch;
 
   /* The encoded user name and password are ready to be sent */
 
   Userpassad:=Addr(Userpass[1]);
 
   TcpWaitSend (Sock,Userpassad,I,True,False,ReturnCode);
 
   /* If sending authorization fails, then exit with error */
 
   If ReturnCode <> OK then
   begin
        WriteLn('Error sending authorization');
        TcpClose(Sock,Returncode);
        Sock:=-1;
        Return;
   end ;
 
   /* Read back an acknowledgement from the SERVER */
 
   Replyad:=Addr(Reply[1]);
   TcpWaitReceive(Sock,Replyad,Replylmax,Replyl);
 
   /* If no message is got, then exit with error */
 
   If replyl<=0 then
   begin
     Writeln('Error receiving authorization message from SERVER');
     TcpClose(Sock,Returncode);
     Sock:=-1;
     return;
   end
          else
   begin
     L:=Replyl-1;
     For I:=1 to Replyl DO ReplyA[I]:=Reply[I];
     SASCIITOE(ReplyA,L);
     ReplyA[Replyl]:=' ';
     For I:=1 to Replyl DO Reply[I]:=ReplyA[I];
     For I:=Replyl+1 to Replylmax DO Reply[I]:=' ';
     Writeln(Reply);
   end;
 
   /* The CLIENT is ready to send data to the SERVER     */
 
   close (Input);
   close (Output);
   end;
 
   /*******************************************/
   /*                                         */
   /*    SERVER   SET UP    FUNCTION          */
   /*                                         */
   /*******************************************/
 
   Procedure SSETUP (var Sock: integer; var Port:integer);
     Main;
   Procedure SSETUP;
   var
   ConnectionInfo : StatusInfoType;
   HostAddress    : InternetAddressType;
   HostName       : String(80);
   ReturnCode     : integer;
 
   begin
   if Port = 0 then Port := 2345;
 
   TermOut (Output);
   TermIn (Input);
 
   /* Establish access to TCP/IP services    */
 
   BeginTcpIp (ReturnCode);
   if ReturnCode <> OK then begin
      WriteLn ('BeginTcpip: ', SayCalRe(ReturnCode));
      return;
   end;
 
   /* Inform TCPIP which notifications will be handled by the program */
 
   Handle ((.DATAdelivered, BUFFERspaceAVAILABLE,
             CONNECTIONstateCHANGED.), ReturnCode);
   if ReturnCode <> OK then begin
      Restore ('Handle: ', ReturnCode);
      return;
   end;
 
 
   /* Open a  passive TCP connection in synchronous mode */
 
   with ConnectionInfo do begin
 
      Connection            := UNSPECIFIEDconnection;
      OpenAttemptTimeout    := OPENtimeout;
      Security              := DEFAULTsecurity;
      Compartment           := DEFAULTcompartment;
      Precedence            := DEFAULTprecedence;
      ConnectionState       := LISTENING;
      LocalSocket.Address   := UNSPECIFIEDaddress;
      LocalSocket.Port      := Port;
      ForeignSocket.Address := UNSPECIFIEDaddress;
      ForeignSocket.Port    := UNSPECIFIEDport;
 
   end;
 
   TcpWaitOpen (ConnectionInfo, ReturnCode);
 
   if ReturnCode <> OK then begin
      Restore ('TcpWaitOpen: ', ReturnCode);
      return;
   end;
 
   /* Open successful TCP has updated the Connection field */
 
   Sock   := ConnectionInfo.Connection;
 
   /* The SERVER is ready to receive data from the CLIENT  */
 
   Close (Input);
   Close  (Output);
   end;
 
   /*******************************************/
   /*                                         */
   /*           SRECV   Procedure             */
   /*                                         */
   /*           String receive                */
   /*                                         */
   /*******************************************/
 
   Procedure  SRECV ( var sock1: integer;
                      var buf1: Buffertc;
                      var m1: integer;
                      var numrec1: integer );
   Main;
 
   Procedure SRECV;
 
   VAR
#if !defined(CERNLIB_TCPIPV2)
   Buffadd1: AddressType;
#endif
#if (defined(CERNLIB_TCPIPV2))&&(!defined(CERNLIB_QMIBMXA))
   Buffadd1: Address24Type;
#endif
#if (defined(CERNLIB_TCPIPV2))&&(defined(CERNLIB_QMIBMXA))
   Buffadd1: Address31Type;
#endif
   RetCod1 : integer;
   Numbytes1 : integer;
   N1: integer;
   K: integer;
 
   begin
   Buffadd1  := Addr(buf1[1]);
   Numbytes1 := BUFFERClength;
 
   If m1<0 then
   /* m1<0 Routine gets what it can */
 
   Begin
      n1 := -m1;
      TcpWaitReceive (sock1, Buffadd1, n1, Numbytes1);
      /* Convert the received string from ASCII to EBCDIC */
      SASCIITOE(buf1,Numbytes1);
      Numrec1 := Numbytes1;
   End
   Else
 
   /* m1>0 Routine attempts to get this length */
 
   Begin
      k :=0;
   repeat
      TcpWaitReceive (sock1, Buffadd1+k, m1-k, Numbytes1);
      k:=k+Numbytes1;
      /* reissue TcpwaitReceive until expected number is reached */
      /* and exit if Numbytes1 <1 or >m1 */
   until (k>=m1) or (Numbytes1<1) or (Numbytes1>m1);
   if k>=m1 then
                  begin
                  SASCIITOE(buf1,k);
                  Numrec1:=k;
                  end
             else Numrec1:=0
   end
   end;
 
   /*******************************************/
   /*                                         */
   /*           BRECV   Procedure             */
   /*                                         */
   /*           Binary receive                */
   /*                                         */
   /*******************************************/
 
   Procedure  BRECV ( var sock2: integer;
                      var buf2: Buffertb;
                      var m2: integer;
                      var numrec2: integer );
   Main;
 
   Procedure BRECV;
 
   VAR
#if !defined(CERNLIB_TCPIPV2)
   Buffadd2: AddressType;
#endif
#if (defined(CERNLIB_TCPIPV2))&&(!defined(CERNLIB_QMIBMXA))
   Buffadd2: Address24Type;
#endif
#if (defined(CERNLIB_TCPIPV2))&&(defined(CERNLIB_QMIBMXA))
   Buffadd2: Address31Type;
#endif
   RetCod2 : integer;
   Numbytes2 : integer;
   N2: integer;
   K: integer;
 
   begin
   Numbytes2 := BUFFERClength;
   Buffadd2  := Addr(buf2[1]);
 
   If m2<0 then
   /* m2<0 Routine gets what it can */
 
   Begin
      n2 := -m2;
      TcpWaitReceive (sock2, Buffadd2, n2, Numbytes2);
      Numrec2 := Numbytes2;
   End
   Else
 
   /* m2>0 Routine attempts to get this length */
 
   Begin
      k :=0;
   repeat
      TcpWaitReceive (sock2, Buffadd2+k, m2-k, Numbytes2);
      k:=k+Numbytes2;
      /* reissue TcpwaitReceive until expected number is reached */
      /* and exit if Numbytes2 is zero */
    until (k>=m2) or (Numbytes2<1) or (Numbytes2>m2);
    if (k>=m2) then Numrec2:=k else Numrec2 :=0
      end
  end;
 
   /*******************************************/
   /*                                         */
   /*           SSEND   PROCEDURE             */
   /*                                         */
   /*           Sends a character string      */
   /*                                         */
   /*******************************************/
 
   Procedure SSEND ( var sock3: integer;
                     var buf3: Buffertc;
                     var m3: integer;
                     var retcode3: integer);
 
   Main;
 
   Procedure SSEND;
   VAR
#if !defined(CERNLIB_TCPIPV2)
   Buffadd3: AddressType;
#endif
#if (defined(CERNLIB_TCPIPV2))&&(!defined(CERNLIB_QMIBMXA))
   Buffadd3: Address24Type;
#endif
#if (defined(CERNLIB_TCPIPV2))&&(defined(CERNLIB_QMIBMXA))
   Buffadd3: Address31Type;
#endif
   I3 : Integer;
   J3 : Integer;
   K3 : Integer;
 
 
   begin
  /*   Convert string from EBCDIC to ASCII  */
 
   SEBCDICTOA(buf3,m3);
 
   Buffadd3 := Addr(buf3[1]);
   /* The maximum length data buffer that can be sent to TCP is 8K */
 
   If m3 > 8192 Then
 
 
   Begin
 
        K3 := 8192;
        I3 := m3  DIV 8192 ;
        For J3 := 1 To I3 DO
        begin
         TcpWaitSend (sock3,Buffadd3,K3,TRUE,FALSE,RetCode3);
         Buffadd3 := Buffadd3 + 8192;
        end;
 
        K3 := m3 MOD 8192;
        TcpWaitSend (sock3,Buffadd3,K3,TRUE,FALSE,RetCode3);
        If Retcode3 <> OK Then Retcode3 := -1
                       Else Retcode3 := m3;
   End
   ElSE
   TcpWaitSend (sock3,Buffadd3,m3,TRUE,FALSE,RetCode3);
 
   If Retcode3 <> OK Then Retcode3 := -1
                     Else Retcode3 := m3;
   end;
 
 
   /*******************************************/
   /*                                         */
   /*           BSEND   PROCEDURE             */
   /*                                         */
   /*           Sends an array of integers    */
   /*                                         */
   /*******************************************/
 
   Procedure BSEND ( var sock4: integer;
                     var buf4: Buffertb;
                     var m4: integer;
                     var retcode4: integer);
 
   Main;
 
   Procedure BSEND;
   VAR
#if !defined(CERNLIB_TCPIPV2)
   Buffadd4: AddressType;
#endif
#if (defined(CERNLIB_TCPIPV2))&&(!defined(CERNLIB_QMIBMXA))
   Buffadd4: Address24Type;
#endif
#if (defined(CERNLIB_TCPIPV2))&&(defined(CERNLIB_QMIBMXA))
   Buffadd4: Address31Type;
#endif
   I4 : Integer;
   J4 : Integer;
   K4 : Integer;
 
 
 
   begin
   Buffadd4 := Addr(buf4[1]);
 
 
   /* The maximum length data buffer that can be sent to TCP is 8K */
 
   If m4 > 8192 Then
 
 
   Begin
 
        K4 := 8192;
        I4 := m4 DIV 8192 ;
        For J4 := 1 To I4 DO
        begin
         TcpWaitSend (sock4,Buffadd4,K4,TRUE,FALSE,RetCode4);
         Buffadd4 := Buffadd4 + 8192;
        end;
 
        K4 := m4 MOD 8192;
        TcpWaitSend (sock4,Buffadd4,K4,TRUE,FALSE,RetCode4);
        If Retcode4 <> OK Then Retcode4 := -1
                          Else Retcode4 := m4;
   End
   ElSE
   TcpWaitSend (sock4,Buffadd4,m4,TRUE,FALSE,RetCode4);
   If Retcode4 <> OK Then Retcode4 := -1
                     Else Retcode4 := m4;
 
   end;
 
   /*******************************************/
   /*                                         */
   /*           SCLOSE  PROCEDURE             */
   /*                                         */
   /*******************************************/
 
   procedure SCLOSE  (var sock5: integer);
   Main;
 
   procedure SCLOSE;
 
   var
   ReturnCode5: integer;
   ReturnCode6: integer;
   Note       : NotificationInfoType;
 
   begin
 
   /* Close TCP connection and wait till other end drops too */
 
   TcpClose (sock5, ReturnCode5);
 
/* repeat
      GetNextNote (Note, True, ReturnCode6);
      if Returncode6 <> OK then begin
         Restore('GetNextNote: ', Returncode6);
         return;
      end;
 
   until (Note.NotificationTag = CONNECTIONstateCHANGED) &
         (Note.NewState = NONEXISTENT);     */
 
   EndTcpIp
 
   end; .
