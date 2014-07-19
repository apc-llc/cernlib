/*
 * $Id: attatch.c,v 1.1.1.1 1996/03/01 11:38:54 mclareni Exp $
 *
 * $Log: attatch.c,v $
 * Revision 1.1.1.1  1996/03/01 11:38:54  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.04/15 05/03/94  14.53.29  by  Fons Rademakers*/
/*-- Author :*/
/*****************************************************************************
 *                                                                           *
 *  Manage the Hatch Style Dialog Box                                        *
 *                                                                           *
 *****************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Scale.h>
#include <Xm/Text.h>

#include "hmotif/uxxt.h"
#include "hmotif/pawm.h"
#include "hmotif/attstate.h"

/* extern functions */
Widget  create_hatchStyleDialog();

/* extern widgets */
extern Widget ApplyHatchBut,ResetHatchBut,CloseHatchBut;
extern Widget IndexText,DistanceScale,Angle1Scale,Angle2Scale;
extern Widget ObjectRow;

/* global data */
Widget widget_hatchStyle=(Widget) NULL;

/* global function */
void show_hatchStyle (Widget w);
void AddZero(char *string);

/* local function */
static void ValueChange_cb(Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data);
static void ApplyHatchDialog_cb(Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data);

static void ResetHatchDialog_cb (Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data);

/* local data */
static int Change;

/*****************************************************************************
 *                                                                           *
 *      Add Zero to Index Text example 0 become 000                          *
 *                                                                           *
 *****************************************************************************/
void AddZero(char *string)
{
   static char Zero[3]="0";

   while (strlen(string)<3) {
            strcat(Zero,string);
            strcpy(string,Zero);
            strcpy(Zero,"0");
         }
}

/*****************************************************************************
 *                                                                           *
 *     Manage the link between the scales and Index Text                     *
 *                                                                           *
 *****************************************************************************/
static void ValueChange_cb(Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data)
{
   int Ivalue,Distance,Angle1,Angle2;
   char *Svalue;
   TStateObject *data;
   char *ComStr;
   Widget CurrentObject;
   char string[5];

   XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentObject,NULL);
   XtVaGetValues(CurrentObject,XmNuserData,&data,NULL);
   switch ((int) client_data ) {
      case 1 : /**** value change in text */
         Svalue=(char *)XmTextGetString(IndexText);

         Ivalue=atoi(Svalue);
         if (Ivalue<0) {
            Ivalue=abs(Ivalue);
            Svalue++; /* to jump over the minus signe */
            XmTextSetString(IndexText,Svalue);
         }
         Distance=(int) (Ivalue/100);
         Angle1=(int) (Ivalue-(Distance*100))/10;
         Angle2=(int) (Ivalue-(Distance*100)-(Angle1*10));

         XmScaleSetValue(DistanceScale,Distance);
         XmScaleSetValue(Angle2Scale,Angle2);
         XmScaleSetValue(Angle1Scale,Angle1);
         if (((call_data)->reason)) {
/* nothing more to do if call by att_main:ObjectChange_cb */
/* if user enter garbagge in text to be sure to have a correct value in text */
            sprintf(string,"%d",Ivalue);
            AddZero(string);
            XmTextSetString(IndexText,string);
            if ( data->IndexHatch!=Ivalue) {
               data->IndexHatch=Ivalue;
               Change=True;
            } else
               Change=False;
         } /* reason */
         break;
      case 2:
      case 3:
      case 4:
       /* value change in scale */
         XmScaleGetValue(w,&Ivalue);
         Svalue=(char *)XmTextGetString(IndexText);
         Svalue[((int) client_data )-2]='0' + Ivalue;
         XmTextSetString(IndexText,Svalue);
         if (data->IndexHatch!=atoi(Svalue)) {
            data->IndexHatch=atoi(Svalue);
            Change=True;
         } else
            Change=False;
         break;
      default :
      fprintf(stderr,"Internal error in procedure ValueChange_cb Hatch");
   }
   if (((call_data)->reason) && Change) {
      /* don't do execute kuip if the call is make by   */

      ComStr=XtCalloc(MAX_CMD_LENGTH,1);
      sprintf(ComStr,"SET %sTYP %d",data->FirstLetter,data->IndexHatch);
      execute_kuip_cmd(ComStr);
      if (appres.auto_refresh) {
         plot_current_histo();
      }
      XtFree(ComStr);
   }
}

/*****************************************************************************
 *                                                                           *
 *    Manage the Apply Button                                                *
 *                                                                           *
 *****************************************************************************/
static  void ApplyHatchDialog_cb(Widget w,caddr_t client_data,
                                 XmAnyCallbackStruct *call_data)
{
   plot_current_histo();
}

/*****************************************************************************
 *                                                                           *
 *            Manage the reset button  : Global REset                        *
 *                                                                           *
 *****************************************************************************/
static  void ResetHatchDialog_cb (Widget w,caddr_t client_data,
                                  XmAnyCallbackStruct *call_data)
{
   Widget CurrentObject;
   TStateObject *data;
   char *ComStr;
   XmAnyCallbackStruct cb;

   XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentObject,NULL);
   XtVaGetValues(CurrentObject,XmNuserData,&data,NULL);
   data->IndexHatch=0;
   if ( (*data->FirstLetter!='X') && (*data->FirstLetter!='Y') )
   {
      ComStr=XtCalloc(MAX_CMD_LENGTH,1);
      sprintf(ComStr,"SET %sTYP %d",data->FirstLetter,data->IndexHatch);
      execute_kuip_cmd(ComStr);
      if (appres.auto_refresh) {
         plot_current_histo();
      }
      XtFree(ComStr);
      XmTextSetString(IndexText,"000");
      cb.reason=0;
      cb.event=(XEvent*)NULL;
      XtCallCallbacks(IndexText,XmNactivateCallback,&cb);
   }
}

/*****************************************************************************
 *                                                                           *
 *            Init the Geometry Dialog box and Popup the interface           *
 *                                                                           *
 *****************************************************************************/
void show_hatchStyle (Widget w)
{
   static MenuCbStruct *menu_a_settings;
   char temp[4];
   Widget CurrentObject;
   TStateObject *data;
   XmAnyCallbackStruct cb;
   XmString Label;
   char *LabelBis;
   char *LabelTer;
    Boolean ret;

   if (!widget_hatchStyle) {
      widget_hatchStyle= create_hatchStyleDialog();

      menu_a_settings = make_menuStruct(w, widget_hatchStyle);
      InstallMwmCloseCallback(XtParent(widget_hatchStyle),
                              (XtCallbackProc)cancel_cb,
                              (XtPointer)menu_a_settings);
/****
 * Callback
 ******/

      XtAddCallback(IndexText,XmNlosingFocusCallback,
                    (XtCallbackProc) ValueChange_cb,(int *) 1);
      XtAddCallback(IndexText,XmNactivateCallback,
                    (XtCallbackProc) ValueChange_cb,(int *) 1);
      XtAddCallback(DistanceScale,XmNvalueChangedCallback,
                    (XtCallbackProc) ValueChange_cb,(int *) 2);
      XtAddCallback(DistanceScale,XmNdragCallback,
                    (XtCallbackProc) ValueChange_cb,(int *) 2);
      XtAddCallback(Angle1Scale,XmNvalueChangedCallback,
                    (XtCallbackProc) ValueChange_cb,(int *) 3);
      XtAddCallback(Angle1Scale,XmNdragCallback,
                    (XtCallbackProc) ValueChange_cb,(int *) 3);
      XtAddCallback(Angle2Scale,XmNvalueChangedCallback,
                    (XtCallbackProc) ValueChange_cb,(int *) 4);
      XtAddCallback(Angle2Scale,XmNdragCallback,
                    (XtCallbackProc) ValueChange_cb,(int *) 4);

      XtAddCallback(CloseHatchBut,XmNactivateCallback,
                    (XtCallbackProc) cancel_cb,menu_a_settings);
      XtAddCallback(ApplyHatchBut,XmNactivateCallback,
                    (XtCallbackProc) ApplyHatchDialog_cb,NULL);
      XtAddCallback(ResetHatchBut,XmNactivateCallback,
                    (XtCallbackProc) ResetHatchDialog_cb,NULL);

      if (appres.auto_refresh) XtSetSensitive(ApplyHatchBut,False);
      else XtSetSensitive(ApplyHatchBut,True);

      XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentObject,NULL);
      XtVaGetValues(CurrentObject,XmNuserData,&data,NULL);
      sprintf(temp,"%d",data->IndexHatch);
      AddZero(temp);
      XmTextSetString(IndexText,temp);
       cb.reason=0;
      cb.event=(XEvent*)NULL;
      XtCallCallbacks(IndexText,XmNactivateCallback,&cb);

      /* Update Title */
      XtVaGetValues(CurrentObject,XmNlabelString,&Label,NULL);
      LabelTer=XtCalloc(30,1);
      ret=XmStringGetLtoR(Label,XmSTRING_DEFAULT_CHARSET,&LabelBis);
      if (!ret)
         fprintf(stderr,"Unable to extract string in show_hatchStyle\n");
      strcpy(LabelTer,LabelBis);
      XtVaSetValues(XtParent(widget_hatchStyle),XmNtitle,
                    strcat(LabelTer," Hatch Style") ,NULL);
      XtFree(LabelBis);
      XtFree(LabelTer);
   }

   XtSetSensitive(w,False);
   XtManageChild(widget_hatchStyle);
}

